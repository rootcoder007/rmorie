# Peaks-over-threshold GPD analysis.
# Source: Pickands (1975), Ann. Statist. 3, 119-131; Davison & Smith
# (1990), JRSS-B 52, 393-442; Coles (2001), An Introduction to
# Statistical Modeling of Extreme Values, Sec. 4.2-4.3, Eq. 4.13.
# Mirrors Python morie.fn.potM exactly; the GPD MLE is the
# shelf-verified morie_evt_gpd_mle (R/evt_coles.R).

#' Peaks-over-threshold GPD fit with return levels
#'
#' Extracts exceedances of threshold u, fits the generalized Pareto
#' distribution by maximum likelihood, estimates the exceedance rate
#' zeta_u = k/n, and computes m-observation return levels (Coles
#' Eq. 4.13): x_m = u + (sigma/xi)((m zeta_u)^xi - 1), with the
#' xi -> 0 limit u + sigma log(m zeta_u).
#'
#' @param y Numeric vector of raw observations.
#' @param u Threshold.
#' @param return_periods Numeric vector of return periods m (in
#'   numbers of observations).
#' @return A list with elements \code{sigma}, \code{xi},
#'   \code{loglik}, \code{cov}, \code{n_exceedances}, \code{n},
#'   \code{rate}, \code{return_levels}, \code{threshold},
#'   \code{converged}, \code{method}.
#' @references Pickands, J. (1975). Annals of Statistics, 3, 119-131.
#'   Davison, A. C. and Smith, R. L. (1990). JRSS-B, 52, 393-442.
#'   Coles, S. (2001). An Introduction to Statistical Modeling of
#'   Extreme Values. Springer.
#' @export
morie_potm <- function(y, u, return_periods = c(10, 100)) {
  yv <- as.numeric(y)
  u <- as.numeric(u)
  n <- length(yv)
  exc <- yv[yv > u] - u
  k <- length(exc)
  if (k < 2) stop("need at least two exceedances above u")
  fit <- morie_evt_gpd_mle(exc)
  # polish: the shelf core's Nelder-Mead can stop ~1e-6 above the true
  # optimum; refine from its solution with tight tolerances so the
  # cross-language objective gap closes below 1e-9 (plateau doctrine).
  nll <- function(th) -morie_evt_gpd_loglik(exc, exp(th[1]), th[2])
  pol <- stats::optim(c(log(fit$sigma), fit$xi), nll,
                      method = "Nelder-Mead",
                      control = list(reltol = 1e-15, maxit = 10000))
  if (-pol$value >= fit$loglik) {
    fit$sigma <- exp(pol$par[1])
    fit$xi <- pol$par[2]
    fit$loglik <- -pol$value
  }
  sigma <- fit$sigma
  xi <- fit$xi
  rate <- k / n
  rl <- setNames(numeric(length(return_periods)),
                 as.character(return_periods))
  for (i in seq_along(return_periods)) {
    m <- as.numeric(return_periods[i])
    if (m * rate <= 1) {
      rl[i] <- NaN
    } else if (abs(xi) < 1e-9) {
      rl[i] <- u + sigma * log(m * rate)
    } else {
      rl[i] <- u + sigma / xi * ((m * rate)^xi - 1)
    }
  }
  list(sigma = sigma, xi = xi,
       loglik = fit$loglik, cov = fit$cov,
       n_exceedances = k, n = n, rate = rate,
       return_levels = rl, threshold = u,
       converged = fit$converged,
       method = "POT/GPD (Davison-Smith 1990; Coles Eq. 4.13)")
}
