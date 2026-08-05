# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian differential privacy: the trade-off function
#'
#' Privacy is expressed as the trade-off between the two error rates of
#' the hypothesis test that distinguishes neighbouring databases; a
#' mechanism is mu-GDP when that trade-off is at least that of testing
#' N(0, 1) against N(mu, 1).  mu = 0 gives the diagonal 1 - alpha,
#' perfect privacy, and delta = 0 for every epsilon; both limits are
#' exact and are what the tests check.
#'
#' Formula: G_mu(alpha) = Phi(Phi^{-1}(1 - alpha) - mu);
#'   delta(eps) = Phi(-eps/mu + mu/2) - exp(eps) Phi(-eps/mu - mu/2).
#'
#' @param mech Placeholder for the mechanism description; unused.
#' @param mu Non-negative GDP parameter.
#' @param alpha Type I error rates at which to evaluate the trade-off.
#' @param epsilon Epsilon at which the (eps, delta) conversion is given.
#' @return List with \code{estimate} (delta), \code{trade_off},
#'   \code{alpha}, \code{delta}, \code{mu}, \code{epsilon}, \code{n},
#'   \code{method}.
#' @references Dong, Roth and Su (2022), Gaussian differential privacy,
#'   JRSS B 84(1):3-37, Corollary 2.13. \doi{10.1111/rssb.12454}
#' @export
Gdpf <- function(mech, mu, alpha = NULL, epsilon = 1) {
  m <- as.numeric(mu)
  if (m < 0) stop("gaussian_dp: mu must be non-negative")
  a <- if (is.null(alpha)) c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9) else .s03vec(alpha)
  if (length(a) == 0L) stop("gaussian_dp: alpha is empty")
  if (any(a <= 0 | a >= 1)) stop("gaussian_dp: every alpha must lie in (0, 1)")
  e <- as.numeric(epsilon)
  trade <- vapply(a, function(v) .s03pnorm(.s03qnorm(1 - v) - m), 0)
  delta <- if (m == 0) 0 else .s03pnorm(-e / m + m / 2) - exp(e) * .s03pnorm(-e / m - m / 2)
  if (delta < 0) delta <- 0
  .t1_result(estimate = delta, trade_off = trade, alpha = a, delta = delta,
             mu = m, epsilon = e, n = length(a),
             method = "G_mu(alpha) = Phi(Phi^{-1}(1-alpha) - mu) with the (eps, delta) dual of Corollary 2.13, Dong, Roth & Su (2022)")
}
