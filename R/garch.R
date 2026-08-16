# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: GARCH(1,1) Gaussian negative log-likelihood for the base-R
# fallback. Extracted from the morie_garch_fit() optimiser closure so the
# parameter-domain guard is directly unit-testable.
#' Internal: GARCH(1,1) Gaussian negative log-likelihood for the base-R
#'
#' fallback. Extracted from the morie_garch_fit() optimiser closure so
#' the parameter-domain guard is directly unit-testable.
#'
#' @param p See Usage.
#' @param r See Usage.
#' @param n See Usage.
#' @return A numeric value.
#' @export
.garch_negll <- function(p, r, n) {
  omega <- p[1]
  alpha <- p[2]
  beta <- p[3]
  if (omega <= 0 || alpha < 0 || beta < 0 || alpha + beta >= 1) {
    return(1e10)
  }
  s2 <- numeric(n)
  s2[1] <- var(r)
  for (t in 2:n) s2[t] <- max(omega + alpha * r[t - 1]^2 + beta * s2[t - 1], 1e-12)
  0.5 * sum(log(2 * pi * s2) + r^2 / s2)
}

#' Fit a GARCH(1,1) model to a return series
#'
#' \deqn{\sigma_t^2 = \omega + \alpha \epsilon_{t-1}^2 + \beta \sigma_{t-1}^2.}{sigma_t^2 = omega + alpha epsilon_t-1^2 + beta sigma_t-1^2.}
#'
#' Native Gaussian quasi-maximum-likelihood fit; no GARCH package is
#' loaded or called. Stationarity requires \eqn{\alpha + \beta < 1},
#' which the likelihood enforces by returning a large penalty outside the
#' admissible region.
#'
#' @param x Numeric return series.
#' @return Named list with \code{omega, alpha, beta, persistence, loglik,
#'   conditional_variance, n, method}.
#' @references
#' Bollerslev, T. (1986). Generalized autoregressive conditional
#' heteroskedasticity. \emph{Journal of Econometrics}, 31(3), 307-327.
#' @examples
#' morie_garch_fit(x = rnorm(50))
#' @export
morie_garch_fit <- function(x) {
  r <- as.numeric(x) - mean(as.numeric(x))
  n <- length(r)
  if (n < 10) stop("Need >=10 obs.")
  neg_ll <- function(p) .garch_negll(p, r, n)
  var_r <- var(r)
  opt <- nlminb(c(var_r * 0.05, 0.1, 0.85), neg_ll,
    lower = c(1e-8, 1e-8, 1e-8),
    upper = c(var_r * 10, 0.999, 0.999)
  )
  omega <- opt$par[1]
  alpha <- opt$par[2]
  beta <- opt$par[3]
  s2 <- numeric(n)
  s2[1] <- var_r
  for (t in 2:n) s2[t] <- omega + alpha * r[t - 1]^2 + beta * s2[t - 1]
  list(
    omega = omega, alpha = alpha, beta = beta,
    persistence = alpha + beta, loglik = -opt$objective,
    conditional_variance = s2, n = n,
    method = "GARCH(1,1) Gaussian QMLE"
  )
}
