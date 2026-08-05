# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mean functional of a normalized random measure
#'
#' An NRMI is a completely random measure divided by its own total
#' mass. Regazzini, Lijoi & Prunster asked what the linear functional
#' \code{M = int x P~(dx)} then looks like; for the gamma CRM, whose
#' normalization is the Dirichlet process, the first two moments are
#' closed form: \code{E[M] = mu0} and \code{Var[M] = sigma0^2 /
#' (alpha + 1)}. After observing \code{y_1, ..., y_n} the posterior is
#' again a Dirichlet process with mass \code{alpha + n} and base
#' measure \code{(alpha P0 + sum delta_yi) / (alpha + n)}, so
#' \code{E[M | y] = (alpha mu0 + sum y_i) / (alpha + n)} and
#' \code{Var[M | y] = s2_post / (alpha + n + 1)}. None of these depend
#' on \code{tau}.
#'
#' @param y Observed values.
#' @param alpha Total mass of the CRM, positive.
#' @param tau Scale of the CRM, positive; carried through only to the
#'   unnormalized total mass.
#' @param mu0 Mean of the base measure.
#' @param sigma0 Standard deviation of the base measure, non-negative.
#' @return List with \code{estimate}, \code{prior_mean},
#'   \code{prior_var}, \code{post_mean}, \code{post_var},
#'   \code{post_base_var}, \code{post_mass}, \code{total_mass},
#'   \code{alpha}, \code{tau}, \code{n}.
#' @references Regazzini, E., Lijoi, A. & Prunster, I. (2003).
#'   Distributional results for means of normalized random measures
#'   with independent increments. Annals of Statistics, 31(2), 560-585.
#'   doi:10.1214/aos/1051027881
#' @export
Nrgmwd <- function(y, alpha = 1, tau = 1, mu0 = 0, sigma0 = 1) {
  a <- as.numeric(alpha); tt <- as.numeric(tau)
  if (a <= 0) stop("Nrgmwd: alpha must be positive")
  if (tt <= 0) stop("Nrgmwd: tau must be positive")
  s0 <- as.numeric(sigma0)
  if (s0 < 0) stop("Nrgmwd: sigma0 must be non-negative")
  m0 <- as.numeric(mu0)
  v <- as.numeric(y)
  n <- length(v)
  if (n == 0L) stop("Nrgmwd: y is empty")
  mass <- a + n
  w0 <- a / mass
  post_mean <- (a * m0 + sum(v)) / mass
  m2 <- w0 * (s0^2 + m0^2) + sum(v * v) / mass
  s2p <- m2 - post_mean^2
  if (s2p < 0) s2p <- 0
  .t1_result(estimate = post_mean, prior_mean = m0,
             prior_var = s0^2 / (a + 1), post_mean = post_mean,
             post_var = s2p / (mass + 1), post_base_var = s2p,
             post_mass = mass, total_mass = a * tt, alpha = a, tau = tt,
             n = n,
             method = "Mean functional of a normalized random measure")
}
