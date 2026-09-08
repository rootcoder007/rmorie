# SPDX-License-Identifier: AGPL-3.0-or-later

#' Nonparametric convergence rate and the curse of dimensionality
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Appendix A.1 (page 238) and A.2 (page 241).  With p
#' (or g) s times continuously differentiable and K an order s kernel,
#' the fastest possible rate of convergence in probability of a
#' d-dimensional kernel estimator is n^(-s/(2s+d)), achieved at
#' bandwidth h_n = c n^(-1/(2s+d)).  Squaring gives the mean-square
#' error rate n^(-2s/(2s+d)), the familiar n^(-4/(4+d)) when s = 2.
#' The rate degrades as d grows: the curse of dimensionality.
#'
#' Closed-form rate arithmetic; no estimation and no randomness.
#'
#' @param d Integer dimension of the covariate.
#' @param n Integer sample size.
#' @param s Integer kernel order, also the assumed number of continuous
#'   derivatives; s = 2 is the ordinary second-order kernel.
#' @param c Numeric bandwidth constant in h_n = c n^(-1/(2s+d)).
#' @param dref Integer reference dimension for the sample-size penalty.
#' @return Named list with exponent, rate, mseexponent, mse, bandwidth,
#'   nequiv, penalty, d, s, n, method.
#' @keywords internal
#' @examples
#' Nprate(1, 1000)$exponent   # 2/5
#' Nprate(5, 1000)$penalty
#' @export
Nprate <- function(d, n, s = 2L, c = 1, dref = 1L) {
  d <- as.integer(d)
  n <- as.integer(n)
  s <- as.integer(s)
  if (d < 1L || n < 1L || s < 1L) {
    stop("d, n and s must all be positive integers.", call. = FALSE)
  }
  expo <- s / (2 * s + d)
  rate <- n^(-expo)
  bw <- c * n^(-1 / (2 * s + d))
  expref <- s / (2 * s + as.integer(dref))
  nequiv <- n^(expref / expo)
  list(exponent = expo, rate = rate, mseexponent = 2 * expo,
       mse = rate * rate, bandwidth = bw, nequiv = nequiv,
       penalty = nequiv / n, d = d, s = s, n = n,
       method = "Horowitz (2009) Appendix A.1/A.2 optimal rates")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Nprate
#' @keywords internal
#' @export
morie_horowitz_curse_dimensionality <- Nprate
