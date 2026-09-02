# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hellinger-affinity bound from strong delta-separation at stage k
#'
#' The stage k divides n, so separating only at a large k gives a
#' correspondingly slower exponential rate.
#'
#' Formula: rho_\{1/2\}(p_0^n, int p^n dmu(p)) < e^\{-(n/k) log_- delta\}
#'   = delta^\{n/k\}
#'
#' @param delta Separation level, 0 < delta < 1.
#' @param k Stage at which separation holds, k >= 1.
#' @param n Sample size.
#' @return List with \code{bound}, \code{rate}, \code{exponent},
#'   \code{delta}, \code{k}, \code{n}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, Section 6.8.1: Definition 6.43,
#'   Lemma 6.44 and Theorem 6.45. Read from the copy of the book held in
#'   the corpus.
#' @export
#' @examples
#' Sepcons(delta = 0.5, k = 5L, n = 5L)
Sepcons <- function(delta, k, n) {
  d <- as.numeric(delta); k <- as.integer(k); n <- as.integer(n)
  if (d <= 0 || d >= 1)
    stop("delta must lie strictly between 0 and 1")
  if (k < 1L) stop("the stage k must be at least 1")
  if (n < 1L) stop("n must be at least 1")
  rate <- -log(d) / k
  .t1_result(bound = exp(-rate * n), rate = rate, exponent = -rate * n,
             delta = d, k = as.numeric(k), n = as.numeric(n),
             method = "Strong separation bound, Ghosal Lemma 6.44")
}
