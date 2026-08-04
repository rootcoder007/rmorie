# SPDX-License-Identifier: AGPL-3.0-or-later
#' Tsallis entropy of a probability vector that is already known
#'
#' The vector is renormalised on entry, so a pmf that sums to 0.999
#' through rounding does not silently shift the entropy and a caller who
#' passes counts gets the answer they meant.
#'
#' Formula: \code{S_q = (1 - sum_i p_i^q) / (q - 1)}.
#'
#' @param p Non-negative weights, renormalised to sum to one.
#' @param q Entropic index; \code{q = 1} gives Shannon entropy in nats.
#' @return List with \code{estimate}, \code{k}, \code{q}.
#' @references Tsallis, C. (1988). J Stat Phys 52:479-487, equation (1).
#' @export
Tsalls <- function(p, q) {
  v <- as.numeric(unlist(p))
  pp <- v / sum(v)
  q <- as.numeric(q)
  val <- if (q == 1) -sum(pp[pp > 0] * log(pp[pp > 0])) else (1 - sum(pp^q)) / (q - 1)
  .t1_result(estimate = val, k = length(pp), q = q,
             method = "Tsallis q-entropy of a supplied pmf")
}
