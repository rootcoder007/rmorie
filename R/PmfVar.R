# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of a discrete pmf
#'
#' Var(X) = E\[(X - mu)^2\] (eq 3.19), cross-checked against the
#' computational form E(X^2) - mu^2 (eq 3.34); their agreement is
#' eq (3.35) and the square root is eq (3.40).
#'
#' @param values,probs the pmf; probs must be >= 0 and sum to 1.
#' @return list(variance, mean, e_x2, sd, forms_agree).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (3.19), (3.34)-(3.35), (3.40), (3.59).
#' @examples
#' PmfVar(1:6, rep(1 / 6, 6))$variance
#' @export
PmfVar <- function(values, probs) {
  values <- as.numeric(values)
  probs <- as.numeric(probs)
  if (length(values) != length(probs) || length(values) == 0L) {
    stop("values and probs must be equal-length, non-empty.", call. = FALSE)
  }
  if (any(is.na(probs)) || any(probs < 0) || abs(sum(probs) - 1) > 1e-9) {
    stop("probs must be >= 0 and sum to 1.", call. = FALSE)
  }
  mu <- sum(values * probs)
  v_def <- sum(probs * (values - mu)^2)
  e_x2 <- sum(probs * values^2)
  v_comp <- e_x2 - mu^2
  if (abs(v_def - v_comp) > 1e-9 * max(1, abs(v_def))) {
    stop("definition and computational forms disagree.", call. = FALSE)
  }
  list(variance = v_def, mean = mu, e_x2 = e_x2, sd = sqrt(v_def),
       forms_agree = TRUE)
}
