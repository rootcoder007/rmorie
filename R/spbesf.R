# SPDX-License-Identifier: AGPL-3.0-or-later
#' Modified Bessel function of the second kind, K_nu.
#'
#' The book defines K_nu(t) = (pi/2) (I_-nu(t) - I_nu(t)) / sin(pi nu),
#' eq (4.73). That identity is a DEFINITION, not an algorithm: both
#' I_(+/-)nu(t) grow like exp(t) while their difference decays like
#' exp(-t), so evaluating it directly sheds roughly 2t/log(10) digits to
#' cancellation and is unusable past t of about 15. Since covariance
#' modelling needs that tail, evaluation uses R's stable `besselK`.
#'
#' @param x Numeric vector, strictly positive.
#' @param nu Order, real. The Matern class requires nu > 0.
#' @return Named list: value, nu.
#' @references Schabenberger & Gotway (2005), Sec 4.9.2, eq (4.73), p. 210.
#' @examples
#' spbesf(x = c(0.1, 1, 5), nu = 1.5)
#' @export
spbesf <- function(x, nu = 0.5) {
  t <- as.numeric(x)
  if (any(t <= 0)) stop("`x` must be positive; K_nu diverges at the origin")
  list(value = besselK(t, nu), nu = as.numeric(nu))
}
