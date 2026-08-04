# SPDX-License-Identifier: AGPL-3.0-or-later
#' Z-estimator: the theta solving the estimating equation Psi_n(theta) = 0.
#'
#' The root is found by bisection with a FIXED iteration count rather than
#' a tolerance, so the two language arms land on bit-identical iterates.
#'
#' Formula: Psi_n(theta) = n^-1 sum_i psi(x_i - theta) = 0, with
#'   psi(u) = u (mean), sign(u) (median), max(-k, min(k, u)) (Huber)
#'
#' @param x The sample.
#' @param kind One of "mean", "median", "huber".
#' @param k Huber tuning constant, k > 0.
#' @param iters Bisection steps (fixed budget).
#' @return List with \code{estimate}, \code{psi_at_estimate},
#'   \code{lower}, \code{upper}, \code{iters}, \code{n}.
#' @references Kosorok (2008), Introduction to Empirical Processes and
#'   Semiparametric Inference, Section 2.2.5 and Theorem 10.16. Fetched as
#'   the full text of the book. The Huber psi is Huber (1964), Annals of
#'   Mathematical Statistics 35(1), 73-101.
#' @export
Zestim <- function(x, kind = "huber", k = 1.345, iters = 200) {
  x <- .t1_vec(x); n <- length(x)
  if (n < 1L) stop("the sample must be non-empty")
  kind <- tolower(kind); k <- as.numeric(k)
  if (kind == "huber" && k <= 0)
    stop("the Huber constant k must be positive")
  psi <- if (kind == "mean") function(u) u
         else if (kind == "median") function(u) sign(u)
         else if (kind == "huber") function(u) pmax(-k, pmin(k, u))
         else stop("kind must be 'mean', 'median' or 'huber'")
  Psi <- function(th) sum(psi(x - th)) / n
  lo <- min(x); hi <- max(x)
  if (lo == hi)
    return(.t1_result(estimate = lo, psi_at_estimate = 0, lower = lo,
                      upper = hi, iters = 0, n = as.numeric(n),
                      method = "Z-estimator, Kosorok Section 2.2.5"))
  a <- lo; b <- hi; it <- as.integer(iters)
  for (i in seq_len(it)) {
    m <- 0.5 * (a + b)
    if (Psi(a) * Psi(m) <= 0) b <- m else a <- m
  }
  th <- 0.5 * (a + b)
  .t1_result(estimate = th, psi_at_estimate = Psi(th), lower = a, upper = b,
             iters = as.numeric(it), n = as.numeric(n),
             method = "Z-estimator, Kosorok Section 2.2.5")
}
