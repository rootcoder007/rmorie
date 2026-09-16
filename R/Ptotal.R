# SPDX-License-Identifier: AGPL-3.0-or-later

#' Law of total probability
#'
#' P(Z) = sum_i P(Z | Ai) P(Ai) over a complete, mutually exclusive set.
#'
#' @param priors prior probabilities, summing to 1.
#' @param likelihoods P(Z | Ai), same length as priors.
#' @return list(p_total, p_event, p_z, p_b); the last three are aliases
#'   of p_total kept for the eq (2.29)/(2.55)/(2.86) callers.
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (2.29), (2.55), (2.86).
#' @examples
#' Ptotal(c(0.02, 0.98), c(0.95, 0.10))$p_total
#' @export
Ptotal <- function(priors, likelihoods) {
  priors <- as.numeric(priors)
  likelihoods <- as.numeric(likelihoods)
  if (length(priors) == 0L || length(priors) != length(likelihoods)) {
    stop("priors and likelihoods must be equal-length, non-empty.", call. = FALSE)
  }
  if (any(is.na(priors)) || any(is.na(likelihoods)) ||
        any(priors < 0) || any(priors > 1) ||
        any(likelihoods < 0) || any(likelihoods > 1)) {
    stop("all probabilities must be in [0, 1].", call. = FALSE)
  }
  if (abs(sum(priors) - 1) > 1e-9) {
    stop("priors must sum to 1 (complete, mutually exclusive set).", call. = FALSE)
  }
  value <- sum(priors * likelihoods)
  list(p_total = value, p_event = value, p_z = value, p_b = value)
}
