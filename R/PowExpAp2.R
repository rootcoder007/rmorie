# SPDX-License-Identifier: AGPL-3.0-or-later

#' Second-order (1+a)^n approximation
#'
#' valid is the decision n a^3 < 0.1.
#'
#' @param a perturbation, > -1.
#' @param n exponent.
#' @return list(exact, approx, na3, valid).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.24).
#' @examples
#' PowExpAp2(-1 / 365, 23)$approx
#' @export
PowExpAp2 <- function(a, n) {
  a <- as.numeric(a)
  n <- as.numeric(n)
  if (length(a) != 1L || is.na(a) || a <= -1) stop("need a > -1.", call. = FALSE)
  if (length(n) != 1L || is.na(n)) stop("n must be a single value.", call. = FALSE)
  validity <- abs(n * a^3)
  list(exact = (1 + a)^n, approx = exp(n * a - n * a * a / 2),
       na3 = validity, valid = validity < 0.1)
}
