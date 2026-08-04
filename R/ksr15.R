# SPDX-License-Identifier: AGPL-3.0-or-later
#' A single Newton step on the estimating equation from theta0.
#'
#' If theta0 is root-n consistent then one step is asymptotically
#' equivalent to the full Z-estimator, so the step is taken exactly once.
#' \code{derivative} is returned because a near-zero denominator makes the
#' step unstable.
#'
#' Formula: theta_1 = theta_0 + (P_n psi_\{theta_0\}) / (-P_n psi'_\{theta_0\}),
#'   psi'(u) = -1 (mean), -1\{|u| <= k\} (Huber)
#'
#' @param x The sample.
#' @param theta0 Starting value, assumed root-n consistent.
#' @param kind Either "mean" or "huber".
#' @param k Huber tuning constant.
#' @return List with \code{estimate}, \code{theta0}, \code{step},
#'   \code{psi_mean}, \code{derivative}, \code{n_used}, \code{n}.
#' @references Kosorok (2008), Introduction to Empirical Processes and
#'   Semiparametric Inference, Section 2.2.5, for the Z-estimator
#'   framework. The full text of the book was fetched and searched, and
#'   the phrase "one-step estimator" does NOT appear in it, so the
#'   one-step construction is cited to Le Cam (1956), Proceedings of the
#'   Third Berkeley Symposium 1, 129-156, and Bickel, Klaassen, Ritov &
#'   Wellner (1993), Efficient and Adaptive Estimation for Semiparametric
#'   Models, Section 2.5.
#' @export
Onestep <- function(x, theta0, kind = "huber", k = 1.345) {
  x <- .t1_vec(x); n <- length(x)
  if (n < 1L) stop("the sample must be non-empty")
  theta0 <- as.numeric(theta0); kind <- tolower(kind); k <- as.numeric(k)
  if (kind == "mean") {
    psi <- x - theta0; dpsi <- rep(-1, n)
  } else if (kind == "huber") {
    if (k <= 0) stop("the Huber constant k must be positive")
    psi <- pmax(-k, pmin(k, x - theta0))
    dpsi <- ifelse(abs(x - theta0) <= k, -1, 0)
  } else stop("kind must be 'mean' or 'huber'")
  pm <- mean(psi); dm <- mean(dpsi)
  if (dm == 0)
    stop("the mean derivative is zero; the one-step update is undefined")
  step <- pm / (-dm)
  .t1_result(estimate = theta0 + step, theta0 = theta0, step = step,
             psi_mean = pm, derivative = dm,
             n_used = sum(dpsi != 0), n = as.numeric(n),
             method = "One-step estimator on the Z-estimating equation")
}
