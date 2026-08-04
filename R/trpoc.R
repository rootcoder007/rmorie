# SPDX-License-Identifier: AGPL-3.0-or-later
#' TRPO's constrained surrogate and its step size
#'
#' Schulman, Levine, Moritz, Jordan and Abbeel (2015), Trust region policy
#' optimization, ICML 37, 1889-1897 (arXiv:1502.05477 -- FETCHED),
#' equation (14): maximise E[pi_theta(a|s)/pi_old(a|s) A] subject to
#' Dbar_KL^(rho_old)(theta_old, theta) <= delta -- the AVERAGE KL over the
#' state distribution, substituted for the maximum KL of equation (12)
#' because the max "is impractical to solve".  Section 6 solves the
#' quadratic approximation: with g the surrogate gradient and F the Fisher
#' information, the step is s = sqrt(2 delta / (x' F x)) x with x = F^-1 g,
#' the largest step in that direction inside the trust region.
#'
#' @param env the advantages A_t.
#' @param policy the probability ratios r_t.
#' @param kl_max the trust-region radius delta.
#' @param ratio,adv,kl explicit ratios, advantages, per-state KLs.
#' @param g gradient of the surrogate.
#' @param F Fisher information matrix.
#' @return list: estimate, surrogate, kl_mean, feasible, step, step_size,
#'   delta, n, method.
#' @keywords internal
#' @examples
#' Trpostep(c(1, -1), c(1.1, 0.9), 0.01)$surrogate
#' @export
Trpostep <- function(env, policy = NULL, kl_max = 0.01, ratio = NULL,
                     adv = NULL, kl = NULL, g = NULL, F = NULL) {
  a <- .s03vec(if (!is.null(adv)) adv else env)
  r <- .s03vec(if (!is.null(ratio)) ratio else policy)
  n <- length(a)
  s <- 0
  for (i in seq_len(n)) s <- s + r[i] * a[i]
  surr <- if (n) s / n else NaN
  klm <- if (!is.null(kl)) .s03mean(.s03vec(kl)) else NaN
  step <- numeric(0); ss <- NaN
  if (!is.null(g) && !is.null(F)) {
    gv <- .s03vec(g); Fm <- .s03mat(F)
    x <- .s03ridgesolve(Fm, gv, 1e-10)
    Fx <- .s03matvec(Fm, x)
    q <- 0
    for (i in seq_along(x)) q <- q + x[i] * Fx[i]
    if (q > 0) {
      ss <- sqrt(2 * as.numeric(kl_max) / q)
      step <- ss * x
    }
  }
  list(estimate = surr, surrogate = surr, kl_mean = klm,
       feasible = if (!is.nan(klm)) klm <= as.numeric(kl_max) else TRUE,
       step = step, step_size = ss, delta = as.numeric(kl_max), n = n,
       method = "TRPO surrogate under a mean-KL trust region (eq. 14; step from sec. 6)")
}
