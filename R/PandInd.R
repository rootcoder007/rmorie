# SPDX-License-Identifier: AGPL-3.0-or-later

#' And-rule for independent events
#'
#' P(A1 and ... and Ak) = prod P(Ai).
#'
#' @param ps marginal probabilities, each in \[0, 1\].
#' @return list(ps, p_and); p_a and p_b are added when exactly two
#'   events are given, for the eq (2.2) callers.
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (2.2), (2.3), (2.70).
#' @examples
#' PandInd(c(1 / 6, 1 / 6))$p_and
#' @export
PandInd <- function(ps) {
  ps <- as.numeric(ps)
  if (length(ps) == 0L || any(is.na(ps)) || any(ps < 0) || any(ps > 1)) {
    stop("ps must be a non-empty vector of probabilities in [0, 1].", call. = FALSE)
  }
  out <- list(ps = ps, p_and = prod(ps))
  if (length(ps) == 2L) {
    out$p_a <- ps[1]
    out$p_b <- ps[2]
  }
  out
}
