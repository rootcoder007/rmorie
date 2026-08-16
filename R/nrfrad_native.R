# NeRF: a scene as a continuous 5D function.
# Sources: Mildenhall, B., Srinivasan, P. P., Tancik, M., Barron, J. T.,
# Ramamoorthi, R. & Ng, R. (2020) "NeRF: Representing Scenes as
# Neural Radiance Fields for View Synthesis", ECCV 2020, LNCS 12346,
# 405-421, doi:10.1007/978-3-030-58452-8_24, arXiv:2003.08934. The
# abstract and Secs. 3-5: a scene as a fully-connected (non-
# convolutional) network whose input is a single continuous 5D
# coordinate -- spatial location and viewing direction -- and whose
# output is volume density and view-dependent emitted radiance;
# differentiability of volume rendering letting posed images suffice;
# positional encoding; hierarchical volume sampling. Max, N. (1995)
# "Optical models for direct volume rendering", IEEE TVCG 1(2),
# 99-108, doi:10.1109/2945.468400, for the volume rendering integral.
# Kerbl, B., Kopanas, G., Leimkuhler, T. & Drettakis, G. (2023) "3D
# Gaussian Splatting for Real-Time Radiance Field Rendering", ACM
# TOG 42(4), doi:10.1145/3592433, the explicit alternative.

# Base R only, faithful translation of nrfrad_python_reference.py.

.NRFRAD_EPS <- 1e-12

# S3 helper: ensure a numeric vector (k.vec).
#' S3 helper: ensure a numeric vector (k.vec)
#'
#' A step of the nrfrad_native implementation. Called by \code{density_is_view_independent}, \code{positional_encoding}, \code{ray_points} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.nrfrad_vec <- function(p) {
  if (is.numeric(p) && !is.list(p)) {
    as.numeric(p)
  } else if (is.list(p)) {
    as.numeric(unlist(p))
  } else {
    as.numeric(p)
  }
}

# S3 helper: ensure a numeric matrix of row vectors (k.mat).
#' S3 helper: ensure a numeric matrix of row vectors (k.mat)
#'
#' A step of the nrfrad_native implementation. Called by \code{volume_render}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param colour A matrix; passed to \code{as.matrix}.
#' @return One of two values, depending on the branch taken.
#' @export
.nrfrad_mat <- function(colour) {
  if (is.matrix(colour)) {
    storage.mode(colour) <- "double"
    colour
  } else if (is.list(colour)) {
    mat <- do.call(rbind, lapply(colour, function(r) as.numeric(r)))
    mat
  } else {
    as.matrix(colour)
  }
}

#' positional_encoding
#'
#' A step of the nrfrad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Passed to \code{.nrfrad_vec}.
#' @param L Defaults to \code{10}.
#' @param include_input A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{out}, as built in the body.
#' @export
positional_encoding <- function(p, L = 10, include_input = TRUE) {
  v <- .nrfrad_vec(p)
  Li <- as.integer(L)
  if (Li < 1L)
    stop("nrfrad: L must be at least 1")
  out <- if (isTRUE(include_input)) as.numeric(v) else numeric(0)
  for (j in seq_len(Li) - 1L) {
    f <- (2.0 ^ j) * pi
    for (q in seq_along(v)) {
      out <- c(out, sin(f * v[q]), cos(f * v[q]))
    }
  }
  out
}

#' ray_points
#'
#' A step of the nrfrad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param origin Passed to \code{.nrfrad_vec}.
#' @param direction Passed to \code{.nrfrad_vec}.
#' @param t_near See Usage.
#' @param t_far See Usage.
#' @param n_samples See Usage.
#' @param seed Defaults to \code{0}.
#' @param stratified A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{t}, \code{points}, \code{direction}.
#' @export
ray_points <- function(origin, direction, t_near, t_far, n_samples,
                       seed = 0, stratified = TRUE) {
  o <- .nrfrad_vec(origin)
  d_raw <- .nrfrad_vec(direction)
  nrm <- sqrt(sum(d_raw * d_raw))
  if (nrm <= .NRFRAD_EPS)
    stop("nrfrad: the ray direction is zero")
  d <- d_raw / nrm
  n <- as.integer(n_samples)
  if (n < 1L || !(as.numeric(t_far) > as.numeric(t_near)))
    stop("nrfrad: need n >= 1 and t_far > t_near")
  step <- (as.numeric(t_far) - as.numeric(t_near)) / n
  e <- .ghc_rng(as.numeric(seed))
  ts <- numeric(n)
  for (i in seq_len(n) - 1L) {
    lo <- as.numeric(t_near) + i * step
    u <- if (isTRUE(stratified)) .ghc_unif(e, 1L) else 0.5
    ts[i + 1L] <- lo + u * step
  }
  pts <- t(apply(matrix(ts, ncol = 1L), 1L,
                 function(tv) o + tv * d))
  if (n == 1L) pts <- matrix(pts, nrow = 1L)
  colnames(pts) <- NULL
  list(t = ts, points = pts, direction = as.numeric(d))
}

#' volume_render
#'
#' A step of the nrfrad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sigma Passed to \code{.nrfrad_vec}.
#' @param colour Passed to \code{.nrfrad_mat}.
#' @param t Passed to \code{.nrfrad_vec}.
#' @return A list with \code{colour}, \code{weights}, \code{accumulated_alpha}, \code{transmittance_final}, \code{note}.
#' @export
volume_render <- function(sigma, colour, t) {
  s <- .nrfrad_vec(sigma)
  C <- .nrfrad_mat(colour)
  ts <- .nrfrad_vec(t)
  n <- length(s)
  if (!(nrow(C) == length(ts) && n == nrow(C)))
    stop("nrfrad: sigma, colour and t differ in length")
  if (any(s < 0.0))
    stop("nrfrad: density cannot be negative")
  if (n <= 0L)
    stop("nrfrad: need at least one sample")
  if (n == 1L) {
    deltas <- c(1e10)
  } else {
    deltas <- c(ts[2:n] - ts[1:(n - 1L)], 1e10)
  }
  Tcur <- 1.0
  ncolC <- ncol(C)
  acc <- rep(0.0, ncolC)
  weights <- numeric(n)
  for (i in seq_len(n)) {
    a <- 1.0 - exp(-s[i] * deltas[i])
    w <- Tcur * a
    weights[i] <- w
    acc <- acc + as.numeric(C[i, ]) * w
    Tcur <- Tcur * (1.0 - a)
  }
  list(
    colour = as.numeric(acc),
    weights = as.numeric(weights),
    accumulated_alpha = sum(weights),
    transmittance_final = Tcur,
    note = "differentiable, which is why only posed IMAGES are needed -- no 3D supervision"
  )
}

#' sample_pdf
#'
#' A step of the nrfrad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param bins Passed to \code{.nrfrad_vec}.
#' @param weights Passed to \code{.nrfrad_vec}.
#' @param n_samples See Usage.
#' @param seed Defaults to \code{0}.
#' @param eps Defaults to \code{1e-05}.
#' @return A vector, from \code{sort}.
#' @export
sample_pdf <- function(bins, weights, n_samples, seed = 0, eps = 1e-5) {
  b <- .nrfrad_vec(bins)
  w <- .nrfrad_vec(weights) + as.numeric(eps)
  if (!(length(w) == length(b) - 1L || length(w) == length(b)))
    stop("nrfrad: ", length(w), " weights do not match ",
         length(b), " bins")
  tot <- sum(w)
  pdf <- w / tot
  cdf <- cumsum(pdf)
  e <- .ghc_rng(as.numeric(seed))
  out <- numeric(as.integer(n_samples))
  for (k in seq_along(out)) {
    u <- .ghc_unif(e, 1L)
    i <- 1L
    while (i < length(cdf) && u > cdf[i]) {
      i <- i + 1L
    }
    lo <- b[i]
    hi <- b[min(i + 1L, length(b))]
    out[k] <- lo + (hi - lo) * .ghc_unif(e, 1L)
  }
  sort(out)
}

#' density_is_view_independent
#'
#' A step of the nrfrad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param model See Usage.
#' @param point Passed to \code{.nrfrad_vec}.
#' @param directions See Usage.
#' @param tol Defaults to \code{1e-09}.
#' @return A list with \code{sigmas}, \code{max_deviation}, \code{view_independent}, \code{note}.
#' @export
density_is_view_independent <- function(model, point, directions,
                                        tol = 1e-9) {
  p <- .nrfrad_vec(point)
  ss <- vapply(directions, function(d) {
    dd <- .nrfrad_vec(d)
    as.numeric(model(p, dd)$sigma)
  }, numeric(1L))
  dev <- max(ss) - min(ss)
  list(
    sigmas = as.numeric(ss),
    max_deviation = dev,
    view_independent = dev < as.numeric(tol),
    note = "sigma from position alone; direction enters only for colour"
  )
}

#' .nrfrad_cheatsheet
#'
#' A step of the nrfrad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.nrfrad_cheatsheet <- function() {
  paste("nrfrad: a scene IS a continuous 5D function -- position plus ",
        "viewing direction to density and radiance -- stored in a ",
        "plain MLP; the weights are the scene. DENSITY must come from ",
        "position ALONE (direction only affects colour), or the ",
        "network fakes specularity by making geometry appear and ",
        "vanish with the camera. Classic volume rendering, and ",
        "because it is DIFFERENTIABLE the only input is posed images ",
        "-- no 3D supervision. POSITIONAL ENCODING is not optional: ",
        "a raw-coordinate MLP is low-frequency biased and renders ",
        "blurry. Hierarchical sampling reuses the coarse weights as ",
        "a PDF.", sep = "")
}

# compact alias per ledger/NAMING.md
neuralradiancefield <- volume_render

# public names resolved by fn/_lazy_map.json
nerf_radiance <- volume_render
nerfradiance <- volume_render

morie_nrfrad <- volume_render
