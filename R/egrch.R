# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: EGARCH(1,1) Gaussian negative log-likelihood. Extracted from
# the morie_egarch_model() optimiser closure so the |beta| >= 1
# stationarity guard is directly unit-testable. `r` is the centred
# series, `n` its length, `EZ` = E|Z| for a standard normal.
#
# Parameter naming for EGARCH is not standard across implementations:
#   arch (Python):  log s2 = w + a(|z| - E|z|) + g z + b log s2   -- a = size
#   rugarch (R):    log s2 = w + a z + g(|z| - E|z|) + b log s2   -- a = sign
# We follow the first: alpha multiplies the centred magnitude (the size
# effect) and gamma the signed z (the sign / leverage effect), matching the
# Python sibling. This module used to compute that internally while reading
# alpha1/gamma1 out of a package using the opposite order, so the returned
# alpha and gamma swapped meaning depending on whether that package
# happened to be installed.
#' Parameter naming for EGARCH is not standard across implementations:
#'
#' arch (Python): log s2 = w + a(|z| - E|z|) + g z + b log s2 -- a =
#' size rugarch (R): log s2 = w + a z + g(|z| - E|z|) + b log s2 -- a =
#' sign We follow the first: alpha multiplies the centred magnitude (the
#' size effect) and gamma the signed z (the sign / leverage effect),
#' matching the Python sibling. This module used to compute that
#' internally while reading alpha1/gamma1 out of a package using the
#' opposite order, so the returned alpha and gamma swapped meaning
#' depending on whether that package happened to be installed.
#'
#' @param p A vector; indexed elementwise.
#' @param r A vector; indexed elementwise.
#' @param n A count; the body uses it as \code{numeric(...)}.
#' @param EZ Numeric; combined arithmetically in the body.
#' @return The value of \code{res}, as built in the body.
#' @export
.egrch_negll <- function(p, r, n, EZ) {
  omega <- p[1]
  alpha <- p[2]
  gamma <- p[3]
  beta <- p[4]
  if (abs(beta) >= 1) {
    return(1e10)
  }
  log_s2 <- numeric(n)
  log_s2[1] <- log(var(r) + 1e-12)
  for (t in 2:n) {
    z <- r[t - 1] / sqrt(exp(log_s2[t - 1]) + 1e-12)
    log_s2[t] <- omega + beta * log_s2[t - 1] + alpha * (abs(z) - EZ) + gamma * z
    # Bound log-variance so exp(log_s2) cannot overflow during nlminb's
    # gradient probing. Without this, the optimiser occasionally lands in
    # regions where s2 = Inf / 0 and the loglik becomes NA/NaN, producing
    # noisy "NA/NaN function evaluation" warnings (correct result, harmless
    # warning). Bounds chosen so exp(log_s2) stays in [1e-30, 1e30] -- far
    # wider than any realistic variance.
    if (!is.finite(log_s2[t])) log_s2[t] <- log_s2[t - 1]
    log_s2[t] <- max(-70, min(70, log_s2[t]))
  }
  s2 <- exp(log_s2)
  res <- 0.5 * sum(log(2 * pi * s2) + r^2 / s2)
  if (!is.finite(res)) {
    return(1e10)
  }
  res
}

#' EGARCH(1,1) asymmetric volatility model
#'
#' \deqn{\log \sigma_t^2 = \omega + \alpha (|z_{t-1}| - E|z_{t-1}|)
#'   + \gamma z_{t-1} + \beta \log \sigma_{t-1}^2}
#' \eqn{\alpha} captures the size effect and \eqn{\gamma} the sign
#' (leverage) effect. Note that this naming is not universal -- \pkg{rugarch}
#' uses the two letters the other way round -- so compare coefficients by
#' which term they multiply, not by name.
#' \eqn{E|z| = \sqrt{2/\pi}} under the
#' Gaussian likelihood used here. Modelling the log variance means no
#' positivity constraints are needed; stationarity requires
#' \eqn{|\beta| < 1}.
#'
#' Native Gaussian quasi-maximum-likelihood fit; no GARCH package is
#' loaded or called.
#'
#' @inheritParams morie_garch_fit
#' @return Named list with \code{omega, alpha, gamma, beta, loglik,
#'   conditional_variance, n, method}.
#' @references
#' Nelson, D. B. (1991). Conditional heteroskedasticity in asset returns:
#' a new approach. \emph{Econometrica}, 59(2), 347-370.
#' @examples
#' morie_egarch_model(x = rnorm(50))
#' @export
morie_egarch_model <- function(x) {
  r <- as.numeric(x) - mean(as.numeric(x))
  n <- length(r)
  if (n < 20) stop("Need >=20 obs.")
  EZ <- sqrt(2 / pi)
  neg_ll <- function(p) .egrch_negll(p, r, n, EZ)
  # Start (omega, alpha, gamma, beta): the size effect alpha is the one
  # typically well away from zero, the sign effect gamma starts neutral.
  opt <- nlminb(c(0, 0.1, 0, 0.9), neg_ll,
    lower = c(-5, -1, -1, -0.999),
    upper = c(5, 1, 1, 0.999)
  )
  log_s2 <- numeric(n)
  log_s2[1] <- log(var(r) + 1e-12)
  for (t in 2:n) {
    z <- r[t - 1] / sqrt(exp(log_s2[t - 1]) + 1e-12)
    log_s2[t] <- opt$par[1] + opt$par[4] * log_s2[t - 1] +
      opt$par[2] * (abs(z) - EZ) + opt$par[3] * z
  }
  list(
    omega = opt$par[1], alpha = opt$par[2],
    gamma = opt$par[3], beta = opt$par[4],
    loglik = -opt$objective,
    conditional_variance = exp(log_s2), n = n,
    method = "EGARCH(1,1) Gaussian QMLE"
  )
}
