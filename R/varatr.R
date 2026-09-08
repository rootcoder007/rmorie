# SPDX-License-Identifier: AGPL-3.0-or-later
#' Value at Risk and expected shortfall from a GARCH(1,1) volatility
#'
#' SOURCE. Jorion, P. (2007), Value at Risk: The New Benchmark for
#' Managing Financial Risk, 3rd edition, McGraw-Hill, ISBN
#' 978-0-07-146495-6. The parametric ("delta-normal") one-period VaR at
#' confidence 1-alpha is the alpha-quantile of the return distribution
#' with the sign flipped, so a loss is a positive number:
#' VaR_alpha = -(mu + sigma z_alpha) = -mu + sigma z_\{1-alpha\}, with
#' z_alpha = qnorm(alpha). The matching Gaussian expected shortfall is
#' ES_alpha = -mu + sigma dnorm(z_alpha)/alpha.
#'
#' The conditional sigma is the one-step-ahead GARCH(1,1) standard
#' deviation of Bollerslev, T. (1986), Journal of Econometrics
#' 31(3):307-327, doi:10.1016/0304-4076(86)90063-1, Eqs. (1)-(2):
#' sigma^2_t = omega + a e^2_\{t-1\} + b sigma^2_\{t-1\}, e_t = y_t - mu.
#'
#' VARIANCE TARGETING. omega is pinned to the sample unconditional
#' variance s^2 by omega = s^2 (1 - a - b), the stationary-variance
#' identity of Bollerslev Eq. (5) solved for omega. a and b are then
#' chosen by exhaustive search over a fixed lattice maximising the
#' Gaussian quasi-likelihood. A lattice rather than an optimiser is what
#' makes both language arms land on identical numbers; it is this
#' implementation's choice, not a quotation.
#'
#' Jorion (2007) is not in the local corpus; the delta-normal formula is
#' the standard statement, and the module is anchored on the degenerate
#' a = b = 0 case where VaR collapses to a closed form in qnorm.
#'
#' @param y Return series.
#' @param alpha Tail probability in (0, 1); 0.05 is the 95 percent VaR.
#' @param a_grid,b_grid Lattices for the ARCH and GARCH coefficients.
#' @return List with \code{var}, \code{es}, \code{mu}, \code{sigma},
#'   \code{sigma2_next}, \code{omega}, \code{a}, \code{b},
#'   \code{loglik}, \code{sigma2_path}, \code{z}, \code{alpha}, \code{n}.
#' @references Jorion, P. (2007). Value at Risk, 3rd ed. McGraw-Hill.
#'   Bollerslev, T. (1986). Journal of Econometrics 31(3):307-327.
#'   doi:10.1016/0304-4076(86)90063-1.
#' @examples
#' Varatr(c(0.01, -0.02, 0.015, -0.005, 0.02, -0.03, 0.01, 0.005), 0.05)$var
#' @export
Varatr <- function(y, alpha = 0.05, a_grid = NULL, b_grid = NULL) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n < 2L) stop("value_at_risk: need at least two observations")
  alpha <- as.numeric(alpha)
  if (!(alpha > 0 && alpha < 1)) {
    stop("value_at_risk: alpha must lie strictly in (0, 1)")
  }
  mu <- 0
  for (v in yv) mu <- mu + v
  mu <- mu / n
  e <- yv - mu
  s2u <- 0
  for (v in e) s2u <- s2u + v * v
  s2u <- s2u / n
  if (!(s2u > 0)) stop("value_at_risk: series has zero variance")
  ag <- if (is.null(a_grid)) c(0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.15, 0.20) else as.numeric(a_grid)
  bg <- if (is.null(b_grid)) c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.85, 0.90) else as.numeric(b_grid)
  if (length(ag) == 0L || length(bg) == 0L) {
    stop("value_at_risk: coefficient lattice is empty")
  }
  best <- NULL
  for (a in ag) for (b in bg) {
    if (a < 0 || b < 0 || a + b >= 1) next
    om <- s2u * (1 - a - b)
    s2 <- s2u
    ll <- 0
    path <- numeric(n)
    ok <- TRUE
    for (t in seq_len(n)) {
      if (t > 1L) s2 <- om + a * e[t - 1L] * e[t - 1L] + b * s2
      if (!(s2 > 0)) { ok <- FALSE
      break }
      ll <- ll + -0.5 * (log(2 * pi) + log(s2) + e[t] * e[t] / s2)
      path[t] <- s2
    }
    if (!ok) next
    nxt <- om + a * e[n] * e[n] + b * s2
    if (is.null(best) || ll > best$ll) {
      best <- list(ll = ll, a = a, b = b, path = path, nxt = nxt)
    }
  }
  if (is.null(best)) stop("value_at_risk: no admissible (a, b) on the lattice")
  om <- s2u * (1 - best$a - best$b)
  sigma <- sqrt(best$nxt)
  z <- .s03qnorm(alpha)
  vr <- -(mu + sigma * z)
  dens <- exp(-0.5 * z * z) / sqrt(2 * pi)
  .t1_result(estimate = vr, var = vr, es = -mu + sigma * dens / alpha,
             mu = mu, sigma = sigma, sigma2_next = best$nxt, omega = om,
             a = best$a, b = best$b, loglik = best$ll,
             sigma2_path = best$path, z = z, alpha = alpha, n = n,
             method = paste("Delta-normal VaR on a GARCH(1,1) conditional",
                            "sigma, variance targeting (Jorion 2007;",
                            "Bollerslev 1986)"))
}
