# SPDX-License-Identifier: AGPL-3.0-or-later
#' Highest posterior density credible interval
#'
#' Chen and Shao (1999), Monte Carlo estimation of Bayesian credible and
#' HPD intervals, JCGS 8(1), 69-92: with n sorted draws and target
#' coverage 1 - alpha, let j = floor((1 - alpha) n); the HPD interval is
#' the pair (theta_(i), theta_(i+j)) minimising theta_(i+j) - theta_(i).
#' The paper is paywalled; the estimator is quoted in its standard
#' published form.  This is the true HPD region only when the posterior is
#' unimodal -- for a multimodal posterior the region is a union of
#' intervals and this scan returns the shortest single one.  That
#' limitation is stated rather than hidden, and the equal-tailed interval
#' is returned so the two can be compared.
#'
#' @param samples posterior draws.
#' @param alpha 1 - coverage.
#' @return list: estimate, width, lo, hi, eq_lo, eq_hi, n, method.
#' @keywords internal
#' @examples
#' Hpdint(c(1, 2, 2.1, 2.2, 9), 0.2)$lo
#' @export
Hpdint <- function(samples, alpha = 0.05) {
  v <- sort(.s03vec(samples))
  n <- length(v)
  a <- as.numeric(alpha)
  # the window must contain at least (1 - alpha) n of the draws, so it
  # spans ceil((1 - alpha) n) order statistics -- ceil((1 - alpha) n) - 1
  # index steps.  Taking floor((1 - alpha) n) STEPS instead makes the
  # window cover one point too many, and it then comes out wider than the
  # equal-tailed interval at the same alpha, which is the wrong way round
  # for an HPD region.
  want <- as.integer(ceiling((1 - a) * n - 1e-12))
  if (want < 2L) want <- 2L
  if (want > n) want <- n
  j <- want - 1L
  best <- 0L
  width <- v[j + 1L] - v[1]
  if (n - j > 1L) for (i in seq_len(n - j - 1L)) {
    w <- v[i + j + 1L] - v[i + 1L]
    if (w < width) { width <- w
    best <- i }
  }
  list(estimate = width, width = width, lo = v[best + 1L],
       hi = v[best + j + 1L], eq_lo = .s03quantile7(v, a / 2),
       eq_hi = .s03quantile7(v, 1 - a / 2), n = n,
       method = "Chen and Shao (1999) HPD interval scan; single-interval, valid for a unimodal posterior")
}
