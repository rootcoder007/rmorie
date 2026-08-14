# 3D Gaussian splatting: an explicit primitive that rasterises.
# Sources: Kerbl, B., Kopanas, G., Leimkuhler, T. and Drettakis, G.
# (2023), 3D Gaussian Splatting for Real-Time Radiance Field
# Rendering, ACM Transactions on Graphics 42(4), Article 139
# (arXiv:2308.04079) -- the explicit 3D Gaussian scene
# representation, the scale-rotation factorisation of the covariance,
# the EWA splat, the same alpha compositing as volume rendering, and
# the adaptive density control; Zwicker, M. et al. (2001), EWA volume
# splatting, Proceedings Visualization 2001 -- the projected Gaussian
# splat; Mildenhall, B. et al. (2020), NeRF, ECCV 2020 -- the
# implicit alternative.
#
# Native implementation mirroring Python morie.fn.gsplat exactly: the
# same R*S*S'*R' factorisation that keeps covariance PSD by
# construction, the same JW*Sigma*W'*J' EWA projection, the same
# front-to-back alpha compositing in depth order, and the same clone-
# split-prune rules for adaptive density control.

.GSPLAT_EPS <- 1e-12

#' @keywords internal
#' @noRd
.gs_quat_to_rot <- function(q) {
  v <- as.numeric(q)
  n <- sqrt(sum(v * v))
  if (n <= .GSPLAT_EPS)
    stop("gsplat: the rotation quaternion is zero")
  w <- v[1] / n; x <- v[2] / n; y <- v[3] / n; z <- v[4] / n
  matrix(c(1 - 2 * (y * y + z * z), 2 * (x * y - w * z), 2 * (x * z + w * y),
           2 * (x * y + w * z), 1 - 2 * (x * x + z * z), 2 * (y * z - w * x),
           2 * (x * z - w * y), 2 * (y * z + w * x), 1 - 2 * (x * x + y * y)),
         nrow = 3, byrow = TRUE)
}

#' Build a covariance matrix from a scale vector and a quaternion
#'
#' @param scale Positive length-3 scale vector.
#' @param quaternion Length-4 rotation quaternion.
#' @return List with covariance, rotation, scale, note.
#' @export
covariance_from_scale_rotation <- function(scale, quaternion) {
  s <- as.numeric(scale)
  if (length(s) != 3L || any(s <= 0))
    stop("gsplat: scales must be positive")
  R <- .gs_quat_to_rot(quaternion)
  M <- R * matrix(s, nrow = 3, ncol = 3, byrow = TRUE)
  S <- tcrossprod(M)
  list(covariance = S, rotation = R, scale = s,
       note = "PSD by construction, which raw entries would not be")
}

#' Test whether a matrix is positive semi-definite
#'
#' @param S Square symmetric matrix.
#' @param tol Numerical tolerance.
#' @return List with eigenvalues, min_eigenvalue, psd.
#' @export
is_positive_semidefinite <- function(S, tol = -1e-9) {
  M <- as.matrix(S); storage.mode(M) <- "double"
  ee <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
  list(eigenvalues = as.numeric(ee), min_eigenvalue = min(ee),
       psd = min(ee) >= as.numeric(tol))
}

#' Project a 3D covariance to a 2D one: J W Sigma W' J'
#'
#' @param S 3D covariance matrix.
#' @param W Viewing (or world) matrix.
#' @param J Jacobian of the perspective projection.
#' @return List with projected, dim, note.
#' @export
project_covariance <- function(S, W, J) {
  C <- as.matrix(S); storage.mode(C) <- "double"
  Wm <- as.matrix(W); storage.mode(Wm) <- "double"
  Jm <- as.matrix(J); storage.mode(Jm) <- "double"
  T <- Jm %*% Wm
  TC <- T %*% C
  out <- TC %*% t(T)
  list(projected = out, dim = nrow(out),
       note = "an affine approximation to perspective, hence closed form and fast")
}

#' Front-to-back alpha compositing
#'
#' @param colours Numeric matrix of per-Gaussian RGB (or more channels).
#' @param alphas Numeric vector of per-Gaussian opacities in [0, 1].
#' @param depths Optional depth vector; absent means input order.
#' @return List with colour, transmittance, coverage, note.
#' @export
alpha_composite <- function(colours, alphas, depths = NULL) {
  C <- as.matrix(colours); storage.mode(C) <- "double"
  a <- as.numeric(alphas)
  if (nrow(C) != length(a))
    stop(paste0("gsplat: ", nrow(C), " colours but ", length(a),
                " alphas"))
  if (any(a < 0 || a > 1))
    stop("gsplat: alphas must lie in [0,1]")
  order <- if (is.null(depths)) seq_along(a) - 1L
           else order(as.numeric(depths), decreasing = FALSE) - 1L
  T <- 1.0; acc <- rep(0, ncol(C))
  for (ii in order) {
    acc <- acc + T * a[ii + 1L] * C[ii + 1L, ]
    T <- T * (1.0 - a[ii + 1L])
  }
  list(colour = as.numeric(acc), transmittance = T,
       coverage = 1.0 - T,
       note = "identical compositing to volume rendering; only the primitive and traversal differ")
}

#' Adaptive density control: clone, split or prune each Gaussian
#'
#' @param gradients Per-Gaussian positional gradient.
#' @param scales Per-Gaussian scale (max component).
#' @param opacities Per-Gaussian opacity.
#' @param grad_threshold Gradient above which a Gaussian is updated.
#' @param scale_threshold Scale above which large-gradient Gaussians
#'   are split instead of cloned.
#' @param opacity_threshold Below which a Gaussian is pruned.
#' @return List with clone, split, prune, n_before, n_after, method,
#'   note.
#' @export
adaptive_density_control <- function(gradients, scales, opacities,
                                     grad_threshold = 0.0002,
                                     scale_threshold = 0.01,
                                     opacity_threshold = 0.005) {
  g <- as.numeric(gradients); s <- as.numeric(scales)
  o <- as.numeric(opacities)
  if (!(length(g) == length(s) && length(s) == length(o)))
    stop("gsplat: the inputs differ in length")
  clone <- integer(0); split <- integer(0); prune <- integer(0)
  for (i in seq_along(g)) {
    if (o[i] < as.numeric(opacity_threshold)) {
      prune <- c(prune, i - 1L)
    } else if (g[i] > as.numeric(grad_threshold)) {
      if (s[i] > as.numeric(scale_threshold))
        split <- c(split, i - 1L)
      else clone <- c(clone, i - 1L)
    }
  }
  n_after <- length(g) + length(clone) + length(split) - length(prune)
  list(estimate = list(clone = clone, split = split, prune = prune),
       clone = clone, split = split, prune = prune,
       n_before = length(g), n_after = n_after,
       method = "adaptive density control; Kerbl et al. (2023)",
       note = "under-reconstruction clones, over-reconstruction splits, transparent prunes")
}

# Compact aliases
#' @export
gaussiansplatting <- alpha_composite
#' @export
gaussian_splatting <- alpha_composite

# house entry point: the package exports one morie_<module>
morie_gsplat <- alpha_composite
