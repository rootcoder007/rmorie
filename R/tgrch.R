# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: GJR-GARCH(1,1) Gaussian negative log-likelihood for the
# base-R fallback. Extracted from the morie_tgarch_model() optimiser closure
# so the parameter-domain guard is directly unit-testable.
#' Internal: GJR-GARCH(1,1) Gaussian negative log-likelihood for the
#'
#' base-R fallback. Extracted from the morie_tgarch_model() optimiser
#' closure so the parameter-domain guard is directly unit-testable.
#'
#' @param p A vector; indexed elementwise.
#' @param r A vector; indexed elementwise.
#' @param n A count; the body uses it as \code{numeric(...)}.
#' @return A numeric value.
#' @export
.tgarch_negll <- function(p, r, n) {
  omega <- p[1]
  alpha <- p[2]
  gamma <- p[3]
  beta <- p[4]
  if (omega <= 0 || alpha < 0 || beta < 0 || alpha + 0.5 * gamma + beta >= 1) {
    return(1e10)
  }
  s2 <- numeric(n)
  s2[1] <- var(r) + 1e-10
  for (t in 2:n) {
    I <- if (r[t - 1] <= 0) 1 else 0
    s2[t] <- max(
      omega + (alpha + gamma * I) * r[t - 1]^2 + beta * s2[t - 1],
      1e-12
    )
  }
  0.5 * sum(log(2 * pi * s2) + r^2 / s2)
}

#' GJR-GARCH(1,1) threshold GARCH
#'
#' \deqn{\sigma_t^2 = \omega + (\alpha + \gamma I_{t-1})\epsilon_{t-1}^2
#'   + \beta \sigma_{t-1}^2}
#' where \eqn{I_{t-1} = 1} when \eqn{\epsilon_{t-1} \le 0} and 0
#' otherwise, so \eqn{\gamma} is the leverage term. Persistence is
#' \eqn{\alpha + \beta + \gamma\kappa} with \eqn{\kappa} the
#' probability of a negative standardised residual -- 0.5 under the
#' symmetric Gaussian likelihood used here.
#'
#' Native Gaussian quasi-maximum-likelihood fit; no GARCH package is
#' loaded or called.
#'
#' @inheritParams morie_garch_fit
#' @references
#' Glosten, L. R., Jagannathan, R. & Runkle, D. E. (1993). On the relation
#' between the expected value and the volatility of the nominal excess
#' return on stocks. \emph{Journal of Finance}, 48(5), 1779-1801.
#' @return Named list with \code{omega, alpha, gamma, beta, persistence,
#'   loglik, conditional_variance, n, method}.
#' @examples
#' morie_tgarch_model(x = rnorm(50))
#' @export
morie_tgarch_model <- function(x) {
  r <- as.numeric(x) - mean(as.numeric(x))
  n <- length(r)
  if (n < 20) stop("Need >=20 obs.")
  neg_ll <- function(p) .tgarch_negll(p, r, n)
  var_r <- var(r)
  opt <- nlminb(c(var_r * 0.05, 0.05, 0.05, 0.85), neg_ll,
    lower = c(1e-8, 1e-8, -0.5, 1e-8),
    upper = c(var_r * 10, 0.5, 0.999, 0.999)
  )
  omega <- opt$par[1]
  alpha <- opt$par[2]
  gamma <- opt$par[3]
  beta <- opt$par[4]
  s2 <- numeric(n)
  s2[1] <- var_r
  for (t in 2:n) {
    I <- if (r[t - 1] <= 0) 1 else 0
    s2[t] <- omega + (alpha + gamma * I) * r[t - 1]^2 + beta * s2[t - 1]
  }
  list(
    omega = omega, alpha = alpha, gamma = gamma, beta = beta,
    persistence = alpha + 0.5 * gamma + beta,
    loglik = -opt$objective,
    conditional_variance = s2, n = n,
    method = "GJR-GARCH(1,1) Gaussian QMLE"
  )
}
