# SPDX-License-Identifier: AGPL-3.0-or-later
#' Certify that a test sequence has exponentially small error probabilities.
#'
#' The rate is read off a single n, so it is a certificate at that n and
#' not a proof of a rate.
#'
#' Formula: C_0 = -log(P_0^n phi_n)/n, C_1 = -log(sup P^n(1 - phi_n))/n,
#'   C = min(C_0, C_1)
#'
#' @param err_null Type I error, in (0, 1].
#' @param err_alt Worst-case type II error, in (0, 1].
#' @param n Sample size at which the errors were observed.
#' @return List with \code{rate}, \code{rate_null}, \code{rate_alt},
#'   \code{bound}, \code{exponential}, \code{n}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, Theorem 6.16 (Schwartz) and its
#'   proof, which invokes Lemma D.11. Read from the copy of the book held
#'   in the corpus.
#' @export
Exptest <- function(err_null, err_alt, n) {
  e0 <- as.numeric(err_null); e1 <- as.numeric(err_alt); n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1")
  if (e0 <= 0 || e0 > 1 || e1 <= 0 || e1 > 1)
    stop("error probabilities must lie in (0, 1]")
  c0 <- -log(e0) / n; c1 <- -log(e1) / n; cc <- min(c0, c1)
  .t1_result(rate = cc, rate_null = c0, rate_alt = c1,
             bound = exp(-cc * n), exponential = as.numeric(cc > 0),
             n = as.numeric(n),
             method = "Exponential test-consistency rate, Ghosal Theorem 6.16")
}
