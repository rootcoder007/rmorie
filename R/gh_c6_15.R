# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cesaro-average Hellinger discrepancy of the predictive densities
#'
#' The martingale approach needs no tests: what is checked is the
#' conclusion n^-1 sum d_H^2(phat_i, p_0) -> 0, plus the Lemma 6.52
#' summability condition.
#'
#' Formula: n^-1 sum_\{i=1\}^\{n\} d_H^2(phat_i, p_0) -> 0 a.s.;
#'   Lemma 6.52 needs sum_\{n>=1\} n^-2 var_0(...) < Inf
#'
#' @param dh2 Squared Hellinger distances, each in \[0, 1\].
#' @param variances Optional variances of the martingale differences.
#' @return List with \code{cesaro}, \code{final}, \code{tail_mean},
#'   \code{lemma652_sum}, \code{summable}, \code{n}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, Section 6.8.4, equations (6.17)
#'   and (6.18), and Lemma 6.52. The approach is due to Walker (2003,
#'   2004) as the book's historical notes record. Read from the copy of
#'   the book held in the corpus.
#' @export
#' @examples
#' Martcons(dh2 = c(0.1, 0.2, 0.15))
Martcons <- function(dh2, variances = NULL) {
  d <- .t1_vec(dh2); n <- length(d)
  if (n < 1L) stop("at least one discrepancy is required")
  if (any(d < 0 | d > 1))
    stop("squared Hellinger distances must lie in [0, 1]")
  ces <- cumsum(d) / seq_len(n)
  half <- n %/% 2L
  tail <- sum(d[(half + 1L):n]) / (n - half)
  if (is.null(variances)) { ls <- NaN; sm <- NaN } else {
    v <- .t1_vec(variances)
    if (length(v) != n) stop("variances must have the same length as dh2")
    if (any(v < 0)) stop("variances must be non-negative")
    ls <- sum(v / seq_len(n)^2)
    sm <- as.numeric(is.finite(ls))
  }
  .t1_result(cesaro = ces, final = ces[n], tail_mean = tail,
             lemma652_sum = ls, summable = sm, n = as.numeric(n),
             method = "Martingale consistency check, Ghosal Section 6.8.4")
}
