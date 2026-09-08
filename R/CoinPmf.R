# SPDX-License-Identifier: AGPL-3.0-or-later

#' Fair-coin Heads-count pmf
#'
#' P(k Heads in n fair flips) = C(n,k)/2^n.
#'
#' @param k heads, >= 0.
#' @param n flips, >= 0.
#' @return list(k, n, probability).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.8).
#' @examples
#' CoinPmf(2, 4)$probability
#' @export
CoinPmf <- function(k, n) {
  r <- BinomPmf(k, n, 0.5)
  list(k = r$k, n = r$n, probability = r$probability)
}
