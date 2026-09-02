# SPDX-License-Identifier: AGPL-3.0-or-later
#' Compensatory multidimensional item response probabilities
#'
#' Verified against Chalmers (2012), JSS 48(6), p.3 eq. (1), read from a
#' rendered image of the page rather than the text layer:
#' \code{Phi(x = 1 | theta, alpha, d, gamma) =
#' gamma + (1 - gamma) / (1 + exp\[-D (alpha' theta + d)\])}.
#'
#' "Compensatory" is the content of \code{alpha' theta}: the abilities
#' enter as a single weighted sum, so a low \code{theta_1} can be repaid
#' by a high \code{theta_2}. A noncompensatory model multiplies
#' per-dimension probabilities and admits no such trade.
#'
#' \code{D} is a scaling adjustment, 1 for the logistic metric and 1.702
#' for the normal-ogive metric.
#'
#' The stub this replaces printed the exponent as
#' \code{-(a1 theta1 + a2 theta2) + d}, which puts \code{d} outside the
#' negation and inverts the role of the intercept. Equation (1) negates
#' the whole linear predictor.
#'
#' Nothing restricts this to two dimensions: \code{a} sets the dimension,
#' and \code{Mirt3} is this function with three slopes and no guessing.
#'
#' @param y Binary 0/1 responses, length n.
#' @param theta Ability matrix, n by m (a plain vector is read as m = 1).
#' @param a Item slopes, length m.
#' @param d Item intercept.
#' @param c Lower asymptote (guessing); 0 gives the M2PL.
#' @param D Metric constant; 1.702 gives the normal-ogive metric.
#' @return List with estimate (log-likelihood), loglik, p, pbar,
#'   deviance, n, m.
#' @references Chalmers (2012), Journal of Statistical Software 48(6),
#'   1-29, \doi{10.18637/jss.v048.i06}, eq. (1) p.3. Reckase (2009),
#'   Multidimensional Item Response Theory, Springer, which Chalmers
#'   cites for D; the book was not in the local corpus and was not
#'   consulted.
#' @export
Mirt2 <- function(y, theta, a, d, c = 0, D = 1) {
  av <- .t1_vec(a); m <- length(av)
  if (m == 0L) stop("a must name at least one dimension")
  yv <- .t1_vec(y); n <- length(yv)
  if (n == 0L) stop("y is empty")
  if (any(yv != 0 & yv != 1)) stop("y must be binary 0/1")
  cc <- as.numeric(c)
  if (!(cc >= 0 && cc < 1)) stop("c must lie in [0, 1)")
  dd <- as.numeric(d); Dm <- as.numeric(D)
  th <- if (m == 1L) matrix(.t1_vec(theta), ncol = 1L) else .t1_mat(theta)
  if (nrow(th) != n) stop("theta must have one row per response")
  if (ncol(th) != m) stop("theta must have one column per element of a")
  z <- dd + as.numeric(th %*% av)
  # .s03sigmoid is the shared numerically stable logistic, matching the
  # Python arm's k.sigmoid; the naive 1/(1 + exp(-t)) overflows there.
  p <- cc + (1 - cc) * vapply(Dm * z, .s03sigmoid, numeric(1))
  ll <- sum(ifelse(yv == 1, log(p), log(1 - p)))
  .t1_result(estimate = ll, loglik = ll, p = p, pbar = mean(p),
             deviance = -2 * ll, n = n, m = m,
             method = "Compensatory multidimensional IRT (Chalmers 2012 eq. 1)")
}
