# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sufficient statistics for the Bernoulli and Normal models
#'
#' Sufficient statistics are far from unique; this returns the standard
#' minimal choice.
#'
#' Formula: Bernoulli T = sum_i x_i; Normal T = (xbar, s)
#'
#' @param x The sample.
#' @param family Either "bernoulli" or "normal".
#' @return List with \code{T1}, \code{T2}, \code{n}, \code{dim},
#'   \code{mle_mu}, \code{mle_sigma2}.
#' @references Wasserman (2004), All of Statistics, Section 9.13.2,
#'   Definition 9.32 and Examples 9.33 and 9.34. Fetched as the full text
#'   of the book.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Suffstat(V)
Suffstat <- function(x, family = "normal") {
  x <- .t1_vec(x); n <- length(x)
  if (n < 1L) stop("the sample must be non-empty")
  fam <- tolower(family)
  if (fam == "bernoulli") {
    if (any(!(x %in% c(0, 1)))) stop("Bernoulli data must be 0/1")
    S <- sum(x)
    return(.t1_result(T1 = S, T2 = NaN, n = n, dim = 1, mle_mu = S / n,
                      mle_sigma2 = NaN,
                      method = "Bernoulli sufficient statistic, Wasserman Ex 9.33"))
  }
  if (fam == "normal") {
    if (n < 2L) stop("the Normal statistic needs at least two points")
    m <- mean(x)
    .t1_result(T1 = m, T2 = stats::sd(x), n = n, dim = 2, mle_mu = m,
               mle_sigma2 = sum((x - m)^2) / n,
               method = "Normal sufficient statistic, Wasserman Ex 9.34")
  } else stop("family must be 'bernoulli' or 'normal'")
}
