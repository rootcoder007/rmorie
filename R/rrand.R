# SPDX-License-Identifier: AGPL-3.0-or-later
#' The same mechanism, read as a differential-privacy guarantee
#'
#' Warner mechanism is exactly the randomised response giving local
#' differential privacy, with flip probability \code{1/(1 + e^eps)}.
#' Reading it that way makes the budget explicit: a small epsilon means a
#' flip probability near one half, so the variance grows as the guarantee
#' tightens.
#'
#' Determinism: flips come from the shared Lehmer minstd stream.
#'
#' Formula: release b unchanged with probability
#' \code{e^eps/(1 + e^eps)}; debiased rate
#' \code{(mean(released) - q)/(1 - 2q)}.
#'
#' @param bit True bits.
#' @param epsilon Privacy budget.
#' @return List with \code{estimate}, \code{released}, \code{q},
#'   \code{raw_rate}, \code{true_rate}, \code{n}.
#' @references Warner, S. L. (1965). JASA 60:63-69; Dwork & Roth (2014),
#'   Found Trends Theor Comput Sci 9:211-407, section 3.2.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Rrand(V)
Rrand <- function(bit, epsilon = 1) {
  b <- as.numeric(unlist(bit))
  n <- length(b)
  eps <- as.numeric(epsilon)
  q <- 1 / (1 + exp(eps))
  g <- .t1_lcg(1)
  rel <- numeric(n)
  for (i in seq_len(n)) rel[i] <- if (g$unif() < q) 1 - b[i] else b[i]
  raw <- sum(rel) / n
  .t1_result(estimate = (raw - q) / (1 - 2 * q), released = rel, q = q,
             raw_rate = raw, true_rate = sum(b) / n, n = n,
             method = "Randomized response under local differential privacy")
}
