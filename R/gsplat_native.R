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
  w <- v[1] / n
  x <- v[2] / n
  y <- v[3] / n
  z <- v[4] / n
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
#' @examples
#' covariance_from_scale_rotation(c(1, 2, 0.5), c(1, 0, 0, 0))
#' @keywords internal
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
#' @examples
#' is_positive_semidefinite(S = 5L)
#' @keywords internal
is_positive_semidefinite <- function(S, tol = -1e-9) {
  M <- as.matrix(S)
  storage.mode(M) <- "double"
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
#' @examples
#' project_covariance(S = 5L, W = 5L, J = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
project_covariance <- function(S, W, J) {
  C <- as.matrix(S)
  storage.mode(C) <- "double"
  Wm <- as.matrix(W)
  storage.mode(Wm) <- "double"
  Jm <- as.matrix(J)
  storage.mode(Jm) <- "double"
  T <- Jm %*% Wm
  TC <- T %*% C
  out <- TC %*% t(T)
  list(projected = out, dim = nrow(out),
       note = "an affine approximation to perspective, hence closed form and fast")
}

#' Front-to-back alpha compositing
#'
#' @param colours Numeric matrix of per-Gaussian RGB (or more channels).
#' @param alphas Numeric vector of per-Gaussian opacities in \[0, 1\].
#' @param depths Optional depth vector; absent means input order.
#' @return List with colour, transmittance, coverage, note.
#' @export
#' @examples
#' set.seed(1)
#' alpha_composite(matrix(runif(6), 2, 3), c(0.5, 0.7))
#' @keywords internal
alpha_composite <- function(colours, alphas, depths = NULL) {
  C <- as.matrix(colours)
  storage.mode(C) <- "double"
  a <- as.numeric(alphas)
  if (nrow(C) != length(a))
    stop(paste0("gsplat: ", nrow(C), " colours but ", length(a),
                " alphas"))
  if (any(a < 0 | a > 1))
    stop("gsplat: alphas must lie in [0,1]")
  order <- if (is.null(depths)) seq_along(a) - 1L
           else order(as.numeric(depths), decreasing = FALSE) - 1L
  T <- 1.0
  acc <- rep(0, ncol(C))
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
#' @examples
#' adaptive_density_control(gradients = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   scales = c(1, 2, 3, 4, 5, 6, 7, 8), opacities = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
adaptive_density_control <- function(gradients, scales, opacities,
                                     grad_threshold = 0.0002,
                                     scale_threshold = 0.01,
                                     opacity_threshold = 0.005) {
  g <- as.numeric(gradients)
  s <- as.numeric(scales)
  o <- as.numeric(opacities)
  if (!(length(g) == length(s) && length(s) == length(o)))
    stop("gsplat: the inputs differ in length")
  clone <- integer(0)
  split <- integer(0)
  prune <- integer(0)
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
#' @rdname alpha_composite
#' @export
gaussiansplatting <- alpha_composite
#' @rdname alpha_composite
#' @export
gaussian_splatting <- alpha_composite

# house entry point: the package exports one morie_<module>
morie_gsplat <- alpha_composite

# -- restored: morie-only definition kept through the rmorie sync --
#' Front-to-back alpha compositing
#'
#' Same image formation as volume rendering, which is why the two
#' representations are interchangeable at the pixel.
#'
#' @param colours Matrix of per-Gaussian colours (n x 3 or n x d).
#' @param alphas Numeric vector of opacities in \[0, 1\].
#' @param depths Optional depth per Gaussian; sorts back-to-front.
#' @return A list with \code{colour}, \code{transmittance},
#'   \code{coverage} and \code{note}.
#' @references Kerbl, B. et al. (2023); Mildenhall, B. et al. (2020).
#' @export
morie_gsplat_composite <- function(colours, alphas, depths = NULL) {
  C <- apply(colours, c(1L, 2L), as.numeric)
  a <- as.numeric(alphas)
  if (nrow(C) != length(a))
    stop(paste0("gsplat: ", nrow(C), " colours but ", length(a),
                " alphas"))
  if (any(a < 0) || any(a > 1))
    stop("gsplat: alphas must lie in [0,1]")
  if (is.null(depths)) {
    order <- seq_len(nrow(C))
  } else {
    order <- order(as.numeric(depths))
  }
  T_ <- 1.0
  acc <- rep(0.0, ncol(C))
  for (i in order) {
    acc <- acc + T_ * a[i] * C[i, ]
    T_ <- T_ * (1.0 - a[i])
  }
  list(colour = acc, transmittance = T_, coverage = 1.0 - T_,
       note = paste0("identical compositing to volume rendering; ",
                     "only the primitive and traversal differ"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Build the 3D covariance as Sigma = R S S' R'
#'
#' Every reachable parameter value is positive semi-definite by
#' construction; gradient descent on the six raw entries would not
#' stay there.
#'
#' @param scale Length-3 positive scale vector.
#' @param quaternion Length-4 (unnormalised) rotation quaternion.
#' @return A list with \code{covariance}, \code{rotation},
#'   \code{scale} and \code{note}.
#' @references Kerbl, B. et al. (2023).
#' @export
morie_gsplat_covariance <- function(scale, quaternion) {
  s <- as.numeric(scale)
  if (length(s) != 3L || any(s <= 0))
    stop("gsplat: scales must be three positive numbers")
  v <- as.numeric(quaternion)
  n <- sqrt(sum(v * v))
  if (n <= .GSPLAT_EPS)
    stop("gsplat: the rotation quaternion is zero")
  w <- v[1L] / n
  x <- v[2L] / n
  y <- v[3L] / n
  z <- v[4L] / n
  R <- matrix(c(1 - 2 * (y * y + z * z), 2 * (x * y - w * z),
                2 * (x * z + w * y),
                2 * (x * y + w * z), 1 - 2 * (x * x + z * z),
                2 * (y * z - w * x),
                2 * (x * z - w * y), 2 * (y * z + w * x),
                1 - 2 * (x * x + y * y)),
              nrow = 3L, ncol = 3L, byrow = TRUE)
  M <- R * matrix(s, nrow = 3L, ncol = 3L, byrow = TRUE)
  S <- tcrossprod(M)
  list(covariance = S, rotation = R, scale = s,
       note = paste0("PSD by construction, which raw entries would ",
                     "not be"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Adaptive density control
#'
#' Large positional gradient with a small Gaussian means
#' under-reconstruction (clone); with a large Gaussian it means
#' over-reconstruction (split). Near-transparent Gaussians are
#' pruned. The Gaussian count is not fixed in advance.
#'
#' @param gradients Per-Gaussian positional gradient magnitudes.
#' @param scales Per-Gaussian scale (e.g. max eigenvalue).
#' @param opacities Per-Gaussian opacity.
#' @param grad_threshold Clone / split threshold on the gradient.
#' @param scale_threshold Split threshold on the scale.
#' @param opacity_threshold Prune threshold on the opacity.
#' @return A list with \code{clone}, \code{split}, \code{prune},
#'   \code{n_before} and \code{n_after}.
#' @references Kerbl, B. et al. (2023).
#' @export
morie_gsplat_density <- function(gradients, scales, opacities,
                                 grad_threshold = 2e-4,
                                 scale_threshold = 0.01,
                                 opacity_threshold = 0.005) {
  g <- as.numeric(gradients)
  s <- as.numeric(scales)
  o <- as.numeric(opacities)
  if (length(g) != length(s) || length(g) != length(o))
    stop("gsplat: the inputs differ in length")
  clone <- integer(0)
  split <- integer(0)
  prune <- integer(0)
  for (i in seq_along(g)) {
    if (o[i] < as.numeric(opacity_threshold)) {
      prune <- c(prune, i)
    } else if (g[i] > as.numeric(grad_threshold)) {
      if (s[i] > as.numeric(scale_threshold)) split <- c(split, i)
      else clone <- c(clone, i)
    }
  }
  list(estimate = list(clone = clone, split = split, prune = prune),
       clone = clone, split = split, prune = prune,
       n_before = length(g),
       n_after = length(g) + length(clone) + length(split) - length(prune),
       method = "adaptive density control; Kerbl et al. (2023)",
       note = paste0("under-reconstruction clones, over-reconstruction ",
                     "splits, transparent prunes"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' EWA projection of the 3D covariance
#'
#' \eqn{\Sigma' = J W \Sigma W^\top J^\top}, the affine approximation
#' to perspective projection that keeps the splat closed-form and
#' therefore fast.
#'
#' @param S 3x3 covariance.
#' @param W 3x3 world-to-camera transform.
#' @param J 2x3 Jacobian of the perspective projection.
#' @return A list with \code{projected} (2x2) and \code{dim}.
#' @references Zwicker, M. et al. (2001); Kerbl, B. et al. (2023).
#' @export
morie_gsplat_project <- function(S, W, J) {
  C <- apply(S, c(1L, 2L), as.numeric)
  Wm <- apply(W, c(1L, 2L), as.numeric)
  Jm <- apply(J, c(1L, 2L), as.numeric)
  T <- Jm %*% Wm
  out <- T %*% C %*% t(T)
  list(projected = out, dim = nrow(out),
       note = paste0("an affine approximation to perspective, hence ",
                     "closed form and fast"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Positive semi-definiteness check
#'
#' Computes the eigenvalues of a symmetric matrix and tests the
#' smallest against a tolerance.
#'
#' @param S 3x3 symmetric matrix.
#' @param tol Tolerance for \code{min_eigenvalue >= tol}.
#' @return A list with \code{eigenvalues}, \code{min_eigenvalue} and
#'   \code{psd}.
#' @references Kerbl, B. et al. (2023).
#' @export
morie_gsplat_psd <- function(S, tol = -1e-9) {
  M <- apply(S, c(1L, 2L), as.numeric)
  ev <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
  list(eigenvalues = ev, min_eigenvalue = min(ev),
       psd = min(ev) >= as.numeric(tol))
}
