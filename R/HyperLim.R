# SPDX-License-Identifier: AGPL-3.0-or-later

#' Hypergeometric pmf against its binomial limit
#'
#' Draws n from a population of N of which K = round(pN) are
#' successes; as N grows at fixed p the pmf tends to Binomial(n, p).
#'
#' @param k successes drawn, >= 0.
#' @param n draws, >= 0.
#' @param p success fraction in the population, in \[0, 1\].
#' @param N population size, >= n.
#' @return list(hypergeometric, binomial, abs_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (4.73), (4.75).
#' @examples
#' HyperLim(2, 5, 0.25, 1000)$abs_error
#' @export
HyperLim <- function(k, n, p, N) {
  N <- as.integer(N)
  n <- as.integer(n)
  k <- as.integer(k)
  p <- as.numeric(p)
  if (any(is.na(c(N, n, k))) || is.na(p) || N < 0L || n < 0L || k < 0L ||
        p < 0 || p > 1) {
    stop("need k, n, N >= 0 integers and p in [0, 1].", call. = FALSE)
  }
  K <- as.integer(round(p * N))
  if (K > N || n > N) stop("need K <= N and n <= N.", call. = FALSE)
  hyper <- if (k > min(K, n) || (n - k) > (N - K)) {
    0
  } else {
    choose(K, k) * choose(N - K, n - k) / choose(N, n)
  }
  binom_p <- BinomPmf(k, n, K / N)$probability
  list(hypergeometric = hyper, binomial = binom_p,
       abs_error = abs(hyper - binom_p))
}
