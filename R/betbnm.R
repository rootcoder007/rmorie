# SPDX-License-Identifier: AGPL-3.0-or-later
#' Beta-binomial conjugate updating.
#'
#' p | y ~ Beta(alpha + y, beta + n - y); the marginal likelihood is
#' C(n,y) B(alpha+y, beta+n-y)/B(alpha, beta); the posterior predictive
#' over m trials has mean m p and variance m p (1-p)(m+s)/(s+1) with
#' s the posterior shape total.
#'
#' @param y Observed successes.
#' @param n Trials, y <= n.
#' @param alpha,beta Prior shapes, strictly positive.
#' @param m Future trials for the posterior predictive; NULL uses n.
#'
#' @return List with postalpha, postbeta, postmean, postvar, postmode,
#'   priormean, logmarglik, predmean, predvar, m.
#' @references Gelman et al. (2013), Bayesian Data Analysis, 3rd edn,
#'   Sects. 2.4-2.5 and Appendix A.  Standard published form; the book is
#'   not in the local corpus and was not read.
#' @export
#' @examples
#' Betabinom(y = 5L, n = 5L)
Betabinom <- function(y, n, alpha = 1, beta = 1, m = NULL) {
  y <- as.integer(y); n <- as.integer(n)
  if (n < 0L || y < 0L || y > n) stop("need 0 <= y <= n")
  a <- as.numeric(alpha); b <- as.numeric(beta)
  if (a <= 0 || b <= 0) stop("prior shapes must be strictly positive")
  pa <- a + y; pb <- b + n - y; s <- pa + pb
  pm <- pa / s
  pv <- pa * pb / (s * s * (s + 1))
  mode <- if (pa > 1 && pb > 1) (pa - 1) / (s - 2) else NA_real_
  lbet <- function(x, z) lgamma(x) + lgamma(z) - lgamma(x + z)
  lml <- lgamma(n + 1) - lgamma(y + 1) - lgamma(n - y + 1) +
    lbet(pa, pb) - lbet(a, b)
  mm <- if (is.null(m)) n else as.integer(m)
  if (mm < 0L) stop("m must be non-negative")
  .t1_result(postalpha = pa, postbeta = pb, postmean = pm, postvar = pv,
             postmode = mode, priormean = a / (a + b), logmarglik = lml,
             predmean = mm * pm,
             predvar = mm * pm * (1 - pm) * (mm + s) / (s + 1), m = mm,
             method = "Beta-Binomial conjugate updating (Gelman et al. BDA3 Sect. 2.4)")
}
