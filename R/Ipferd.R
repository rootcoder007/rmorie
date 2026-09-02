# SPDX-License-Identifier: AGPL-3.0-or-later
#' IPW estimate with a replicate-weight (jackknife) variance
#'
#' The point estimate is the Hajek weighted difference in means between
#' treated and control units; its variance comes from the replicate
#' weights supplied with the survey, which is the design-based route
#' when the sampling design is not recoverable from the microdata.  The
#' scale factor is exposed so JKn and BRR schemes (factor 1/B) can be
#' used in place of the JK1 default.
#'
#' Formula: v(theta) = ((B - 1)/B) sum_b (theta_b - theta)^2.
#'
#' @param y Outcome vector.
#' @param D Treatment indicator, 0 or 1.
#' @param w Full-sample analysis weights.
#' @param replicate_weights n by B matrix of replicate weights.
#' @param scale Multiplier on the sum of squared deviations;
#'   \code{NULL} uses the JK1 value (B - 1)/B.
#' @return List with \code{estimate}, \code{se}, \code{variance},
#'   \code{scale}, \code{replicate_estimates}, \code{rep_mean},
#'   \code{n_replicates}, \code{n}, \code{method}.
#' @references Rust and Rao (1996), Variance estimation for complex
#'   surveys using replication techniques, Statistical Methods in
#'   Medical Research 5(3):283-310. \doi{10.1177/096228029600500305}
#'   Lumley (2010), Complex Surveys, Wiley, \doi{10.1002/9780470580066};
#'   Lumley and Scott (2017), Statistical Science 32(2):265-278.
#'   \doi{10.1214/16-STS605}
#' @export
#' @examples
#' set.seed(1)
#' Ipferd(y = rnorm(20), D = rbinom(20, 1, 0.5), w = runif(20, 1, 2),
#'        replicate_weights = matrix(runif(200, 0.5, 1.5), 20, 10))
Ipferd <- function(y, D, w, replicate_weights, scale = NULL) {
  yv <- .s03vec(y); n <- length(yv)
  if (n == 0L) stop("ipw_with_replicate: y is empty")
  d <- .s03vec(D)
  wv <- if (!is.null(w)) .s03vec(w) else rep(1, n)
  if (length(d) != n || length(wv) != n) stop("ipw_with_replicate: y, D and w have different lengths")
  if (any(d != 0 & d != 1)) stop("ipw_with_replicate: D must be 0 or 1")
  R <- .s03mat(replicate_weights)
  if (nrow(R) != n) stop("ipw_with_replicate: replicate_weights must have one row per observation")
  B <- ncol(R)
  if (B < 2L) stop("ipw_with_replicate: need at least two replicates")
  hj <- function(ww) {
    s1 <- sum(ww[d == 1]); s0 <- sum(ww[d == 0])
    if (s1 <= 0 || s0 <= 0) stop("ipw_with_replicate: a replicate leaves one arm with no weight")
    sum(ww[d == 1] * yv[d == 1]) / s1 - sum(ww[d == 0] * yv[d == 0]) / s0
  }
  theta <- hj(wv)
  reps <- vapply(seq_len(B), function(b) hj(R[, b]), 0)
  fac <- if (is.null(scale)) (B - 1) / B else as.numeric(scale)
  vr <- fac * sum((reps - theta)^2)
  .t1_result(estimate = theta, se = sqrt(vr), variance = vr, scale = fac,
             replicate_estimates = reps, rep_mean = mean(reps),
             n_replicates = B, n = n,
             method = "Hajek weighted difference; v = ((B-1)/B) sum_b (theta_b - theta)^2, Rust & Rao (1996) JK1")
}
