# SPDX-License-Identifier: AGPL-3.0-or-later
#' Normalized gamma process prior (the Dirichlet process)
#'
#' The gamma completely random measure has Levy intensity
#' \code{nu(du, dx) = alpha P0(dx) u^-1 exp(-u / tau) du}, so its
#' Laplace exponent is \code{psi(lam) = alpha log(1 + lam tau)} and its
#' expected total mass is \code{alpha tau}. Normalizing by the total
#' mass gives \code{DP(alpha, P0)}; because normalizing divides out the
#' scale, the resulting law does not depend on \code{tau} at all.
#'
#' Formula: \code{E[K_n] = sum_i alpha / (alpha + i - 1) =
#' alpha (digamma(alpha + n) - digamma(alpha))} and
#' \code{Var[K_n] = sum_i alpha (i - 1) / (alpha + i - 1)^2}. Both
#' expressions for the mean are computed, so each checks the other.
#'
#' @param y Observed values; \code{n} and the number of distinct values
#'   are read off them.
#' @param alpha Concentration parameter, positive.
#' @param tau Scale of the gamma CRM, positive.
#' @return List with \code{estimate}, \code{e_k}, \code{e_k_digamma},
#'   \code{var_k}, \code{k_observed}, \code{total_mass}, \code{psi1},
#'   \code{alpha}, \code{tau}, \code{n}.
#' @references Lijoi, A. & Prunster, I. (2010). Models beyond the
#'   Dirichlet process. In Bayesian Nonparametrics, 80-136. Cambridge
#'   University Press. Ferguson, T. S. (1973). A Bayesian analysis of
#'   some nonparametric problems. Annals of Statistics, 1(2), 209-230.
#' @export
Ngppr <- function(y, alpha = 1, tau = 1) {
  a <- as.numeric(alpha); tt <- as.numeric(tau)
  if (a <= 0) stop("Ngppr: alpha must be positive")
  if (tt <= 0) stop("Ngppr: tau must be positive")
  v <- as.numeric(y)
  n <- length(v)
  if (n == 0L) stop("Ngppr: y is empty")
  ek <- 0; vk <- 0
  for (i in seq_len(n)) {
    ek <- ek + a / (a + i - 1)
    vk <- vk + a * (i - 1) / (a + i - 1)^2
  }
  ekd <- a * (.s03digamma(a + n) - .s03digamma(a))
  .t1_result(estimate = ek, e_k = ek, e_k_digamma = ekd, var_k = vk,
             k_observed = length(unique(v)), total_mass = a * tt,
             psi1 = a * log(1 + tt), alpha = a, tau = tt, n = n,
             method = "Normalized gamma process (Dirichlet process) prior")
}
