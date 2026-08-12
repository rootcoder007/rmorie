# Curve-level functional bootstrap bands.
# Source: Cuevas, Febrero & Fraiman (2006), CSDA 51(2), 1063-1074,
# Sec. 3(e) (fetched-wave3/On_the_use_of_the_bootstrap_for_
# estimating_functions_with_functional_data.pdf).  Mirrors Python
# morie.fn.funBoot exactly (same SplitMix64 stream: per curve one
# index uniform, then m normals when smoothing).

#' Functional bootstrap tolerance band (Cuevas et al. 2006)
#'
#' Curve-level bootstrap of the functional estimator T; the band is
#' the ball of radius D around the bootstrap average of T, with D
#' the (1-alpha) quantile of the distances of the replicates to
#' their average (L2 or sup metric).  smooth > 0 gives the smoothed
#' bootstrap (Gaussian perturbation of resampled curves).
#'
#' @param curves Matrix (n x m) of discretized curves.
#' @param statistic Function(list of curves) -> curve (default
#'   pointwise mean).
#' @param alpha 1 - band level.
#' @param B Bootstrap samples.
#' @param metric "l2" or "sup".
#' @param smooth Smoothed-bootstrap standard deviation (0 = naive).
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @return A list with elements \code{center}, \code{radius},
#'   \code{estimate}, \code{distances}, \code{n_within},
#'   \code{metric}, \code{alpha}, \code{B}, \code{seed},
#'   \code{method}.
#' @references Cuevas, A., Febrero, M. and Fraiman, R. (2006). On
#'   the use of the bootstrap for estimating functions with
#'   functional data. Computational Statistics & Data Analysis,
#'   51(2), 1063-1074.
#' @export
morie_funboot <- function(curves, statistic = NULL, alpha = 0.05,
                          B = 500, metric = "l2", smooth = 0,
                          seed = 0) {
  X <- as.matrix(curves)
  n <- nrow(X)
  m <- ncol(X)
  if (n < 3) stop("curves must be rectangular with n >= 3")
  if (is.null(statistic)) statistic <- function(cs) colMeans(cs)
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0, 1)")
  met <- tolower(metric)
  if (!met %in% c("l2", "sup")) stop("metric must be 'l2' or 'sup'")
  e <- .ghc_rng(seed)
  t_obs <- as.numeric(statistic(X))
  reps <- matrix(0, B, m)
  for (b in seq_len(B)) {
    sample_m <- matrix(0, n, m)
    for (i in seq_len(n)) {
      pick <- X[min(floor(.ghc_unif(e, 1) * n), n - 1) + 1, ]
      if (smooth > 0) {
        pick <- pick + smooth * .ghc_norm(e, m)
      }
      sample_m[i, ] <- pick
    }
    reps[b, ] <- as.numeric(statistic(sample_m))
  }
  center <- colMeans(reps)
  dists <- apply(reps, 1, function(r) {
    if (met == "sup") max(abs(r - center)) else
      sqrt(mean((r - center)^2))
  })
  sd_ <- sort(dists)
  idx <- max(min(ceiling((1 - alpha) * B), B), 1)
  D <- sd_[idx]
  list(center = center, radius = D, estimate = t_obs,
       distances = dists, n_within = sum(dists <= D),
       metric = met, alpha = alpha, B = as.integer(B), seed = seed,
       method = "functional bootstrap band (Cuevas et al. 2006, Sec. 3e)")
}
