# SPDX-License-Identifier: AGPL-3.0-or-later

#' First-order (1+a)^n approximation
#'
#' valid is the decision n a^2 < 0.1, the book's rule of thumb for when
#' the first-order form may be used.
#'
#' @param a perturbation, > -1.
#' @param n exponent.
#' @return list(exact, approx, na2, valid).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (7.14), (7.23).
#' @examples
#' PowExpApx(-1 / 365, 23)$approx
#' @export
PowExpApx <- function(a, n) {
  a <- as.numeric(a)
  n <- as.numeric(n)
  if (length(a) != 1L || is.na(a) || a <= -1) stop("need a > -1.", call. = FALSE)
  if (length(n) != 1L || is.na(n)) stop("n must be a single value.", call. = FALSE)
  validity <- abs(n * a * a)
  list(exact = (1 + a)^n, approx = exp(n * a), na2 = validity,
       valid = validity < 0.1)
}
