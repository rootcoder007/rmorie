# SPDX-License-Identifier: AGPL-3.0-or-later
#' Exact sign test of H0: median = md
#'
#' Ties at md carry no information and are discarded, so the effective n
#' is returned rather than the input length.
#'
#' Formula: S = #\{x_i > md\}, m = #\{x_i != md\};
#'   S ~ Binomial(m, 1/2) under H0;
#'   two-sided p = 2 P(Bin(m, 1/2) >= max(S, m - S)), capped at 1
#'
#' @param x The sample.
#' @param md Null median.
#' @return List with \code{statistic}, \code{p_value},
#'   \code{n_effective}, \code{n_ties}, \code{estimate}, \code{n}.
#' @references Dixon & Mood (1946), Journal of the American Statistical
#'   Association 41(236), 557-566 -- the primary source. Wasserman (2004),
#'   All of Statistics, does NOT contain the sign test; the full text of
#'   the book was fetched and searched to establish that.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Sgntest(V)
Sgntest <- function(x, md = 0) {
  x <- .t1_vec(x); n <- length(x)
  if (n < 1L) stop("the sample must be non-empty")
  md <- as.numeric(md)
  pos <- sum(x > md); neg <- sum(x < md); m <- pos + neg
  if (m == 0L) stop("every observation equals md; the test is vacuous")
  k <- max(pos, neg)
  j <- k:m
  tail <- sum(exp(lgamma(m + 1) - lgamma(j + 1) - lgamma(m - j + 1) -
                  m * log(2)))
  .t1_result(statistic = as.numeric(pos), p_value = min(1, 2 * tail),
             n_effective = as.numeric(m), n_ties = as.numeric(n - m),
             estimate = stats::median(x), n = as.numeric(n),
             method = "Exact sign test, Binomial(m, 1/2)")
}
