# SPDX-License-Identifier: AGPL-3.0-or-later

#' Zero-concentrated differential privacy (zCDP) accounting
#'
#' A mechanism \eqn{M} is \eqn{\rho}-zCDP when
#' \eqn{D_\alpha(M(x) \| M(x')) \le \rho\alpha} for every
#' \eqn{\alpha \in (1, \infty)} and every pair of adjacent inputs
#' (Bun & Steinke 2016, Definition 1.1, in the \eqn{\xi = 0} case).
#'
#' \code{mech} gives the per-mechanism parameters \eqn{\rho_i} of the
#' mechanisms being composed. Composition is additive (Lemma 1.7), so the
#' composed mechanism is \eqn{\sum_i \rho_i}-zCDP. \code{rho} is the budget
#' the composition is checked against and the budget used to size the
#' Gaussian noise.
#'
#' The reported quantities are
#' \itemize{
#'   \item Proposition 1.3 --- \eqn{\rho}-zCDP implies
#'     \eqn{(\rho + 2\sqrt{\rho \log(1/\delta)}, \delta)}-differential
#'     privacy for every \eqn{\delta > 0}.
#'   \item Proposition 1.4 --- \eqn{\varepsilon}-differential privacy
#'     implies \eqn{(\varepsilon^2/2)}-zCDP; reported as
#'     \code{rho_from_eps}.
#'   \item Proposition 1.6 / Lemma 2.4 --- the Gaussian mechanism that
#'     answers a sensitivity-\eqn{\Delta} query with \eqn{N(0, \sigma^2)}
#'     noise is \eqn{\Delta^2/(2\sigma^2)}-zCDP; inverted at the budget
#'     this gives \eqn{\sigma = \Delta/\sqrt{2\rho}}.
#' }
#'
#' Mirrors \code{morie.fn.zcdp} on the Python side.
#'
#' @param mech Numeric vector of per-mechanism zCDP parameters
#'   \eqn{\rho_i}. A scalar is treated as a single mechanism.
#' @param rho Total zCDP privacy budget, \code{rho > 0}.
#' @param delta The \eqn{\delta} at which the Proposition 1.3
#'   \eqn{(\varepsilon, \delta)}-DP statement is reported, in (0, 1).
#' @param sensitivity Query sensitivity \eqn{\Delta} used to size the
#'   Gaussian noise.
#' @return Named list with \code{rho_total}, \code{rho_budget},
#'   \code{within_budget}, \code{epsilon}, \code{delta}, \code{sigma},
#'   \code{rho_from_eps}, \code{n_mechanisms}, \code{method}.
#' @references Bun M & Steinke T (2016). Concentrated differential
#'   privacy: simplifications, extensions, and lower bounds.
#'   \emph{Theory of Cryptography (TCC 2016-B)}, 635--658.
#'   arXiv:1605.02065.
#' @examples
#' Zcdp(c(0.1, 0.2, 0.3), rho = 1)$epsilon
#' @export
Zcdp <- function(mech, rho, delta = 1e-6, sensitivity = 1) {
  vals <- as.numeric(mech)
  if (any(!is.finite(vals)) || any(vals < 0)) {
    stop("zCDP parameters rho_i must be finite and non-negative",
         call. = FALSE)
  }
  rho <- as.numeric(rho)[1L]
  if (!is.finite(rho) || rho <= 0) {
    stop("rho must be positive", call. = FALSE)
  }
  delta <- as.numeric(delta)[1L]
  if (!is.finite(delta) || delta <= 0 || delta >= 1) {
    stop("delta must lie in (0, 1)", call. = FALSE)
  }
  sensitivity <- as.numeric(sensitivity)[1L]
  if (!is.finite(sensitivity) || sensitivity <= 0) {
    stop("sensitivity must be positive", call. = FALSE)
  }

  ## Lemma 1.7: composition of rho_i-zCDP mechanisms is (sum rho_i)-zCDP.
  rho_total <- 0
  for (v in vals) rho_total <- rho_total + v
  ## Proposition 1.3.
  epsilon <- rho_total + 2 * sqrt(rho_total * log(1 / delta))
  ## Proposition 1.6 inverted at the budget rho.
  sigma <- sensitivity / sqrt(2 * rho)
  ## Proposition 1.4, applied to the epsilon just derived.
  rho_from_eps <- 0.5 * epsilon * epsilon
  list(
    rho_total = rho_total,
    rho_budget = rho,
    within_budget = rho_total <= rho,
    epsilon = epsilon,
    delta = delta,
    sigma = sigma,
    rho_from_eps = rho_from_eps,
    n_mechanisms = length(vals),
    method = "Zero-concentrated DP (Bun & Steinke 2016)"
  )
}
