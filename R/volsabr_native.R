# SABR implied volatility.
# Source: Hagan, Kumar, Lesniewski & Woodward (2002), Managing smile
# risk, Wilmott Magazine, Sept., 84-108, Eqs. 2.17a-c and 2.18
# (fetched-wave3/hagan-2002-sabr-managing-smile-risk.pdf).  Mirrors
# Python morie.fn.volsabr exactly (same small-z series switch).

#' SABR model Black implied volatility
#'
#' Hagan et al. (2002) Eq. 2.17: sigma_B = alpha / ((fK)^((1-beta)/2)
#' (1 + (1-beta)^2/24 log^2(f/K) + (1-beta)^4/1920 log^4(f/K))) *
#' z/x(z) * (1 + correction T), with z = (nu/alpha)(fK)^((1-beta)/2)
#' log(f/K) and x(z) = log((sqrt(1-2 rho z + z^2)+z-rho)/(1-rho));
#' reduces to Eq. 2.18 at the money.
#'
#' @param K,f Strike and forward (positive).
#' @param T Time to exercise (years).
#' @param alpha Volatility parameter (> 0).
#' @param beta CEV exponent in [0, 1].
#' @param rho Correlation in (-1, 1).
#' @param nu Vol-of-vol (>= 0).
#' @return A list with elements \code{estimate}, \code{z},
#'   \code{x_z}, \code{atm}, \code{method}.
#' @references Hagan, P. S., Kumar, D., Lesniewski, A. S. and
#'   Woodward, D. E. (2002). Managing smile risk. Wilmott Magazine,
#'   September, 84-108.
#' @export
morie_volsabr <- function(K, f, T, alpha, beta, rho, nu) {
  K <- as.numeric(K); f <- as.numeric(f); T <- as.numeric(T)
  alpha <- as.numeric(alpha); beta <- as.numeric(beta)
  rho <- as.numeric(rho); nu <- as.numeric(nu)
  if (K <= 0 || f <= 0 || alpha <= 0 || T < 0) {
    stop("K, f, alpha must be positive; T >= 0")
  }
  if (beta < 0 || beta > 1 || rho <= -1 || rho >= 1 || nu < 0) {
    stop("need 0 <= beta <= 1, -1 < rho < 1, nu >= 0")
  }
  omb <- 1 - beta
  lfk <- log(f / K)
  fkb <- (f * K)^(omb / 2)
  denom <- fkb * (1 + omb^2 / 24 * lfk^2 + omb^4 / 1920 * lfk^4)
  z <- (nu / alpha) * fkb * lfk
  if (abs(z) < 1e-7) {
    zoxz <- 1 + rho * z / 2 + (2 - 3 * rho^2) * z * z / 12
    x_z <- if (z != 0) z / zoxz else NaN
  } else {
    x_z <- log((sqrt(1 - 2 * rho * z + z * z) + z - rho) / (1 - rho))
    zoxz <- z / x_z
  }
  corr <- 1 + (omb^2 / 24 * alpha^2 / (f * K)^omb +
               rho * beta * nu * alpha / (4 * fkb) +
               (2 - 3 * rho^2) / 24 * nu^2) * T
  sigma <- alpha / denom * zoxz * corr
  atm <- alpha / f^omb * (1 + (omb^2 / 24 * alpha^2 / f^(2 * omb) +
                               rho * beta * alpha * nu / (4 * f^omb) +
                               (2 - 3 * rho^2) / 24 * nu^2) * T)
  list(estimate = sigma, z = z, x_z = x_z, atm = atm,
       method = "SABR implied vol (Hagan et al. 2002, Eq. 2.17)")
}
