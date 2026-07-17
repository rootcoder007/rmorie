# SPDX-License-Identifier: AGPL-3.0-or-later

#' Local-level state-space model (Kalman filter+smoother)
#'
#' Native local-level (random-walk-plus-noise) model: the observation
#' and state variances are estimated by maximum likelihood on the
#' prediction-error decomposition, then the series is filtered and
#' smoothed (Rauch-Tung-Striebel). Equivalent to the
#' \code{dlm::dlmModPoly(1)} + \code{dlmMLE} + \code{dlmFilter} +
#' \code{dlmSmooth} pipeline it replaces, cross-validated in tests.
#'
#' @param x Numeric univariate series.
#' @return Named list with \code{filtered_state, filtered_state_variance,
#'   smoothed_state, loglik, Q, R, n, method}.
#' @examples
#' morie_state_space_model(x = rnorm(50))
#' @export
morie_state_space_model <- function(x) {
  y <- as.numeric(x)
  n <- length(y)
  if (n < 4) stop("Need >=4 obs.")

  # Kalman pass for the local-level model with diffuse-ish init.
  # Returns filtered mean/variance and the prediction-error loglik.
  kf <- function(R, Q) {
    a <- numeric(n)
    P <- numeric(n)
    a_prev <- y[1]
    P_prev <- 1e7
    ll <- 0
    for (t in seq_len(n)) {
      Pp <- if (t == 1) P_prev else P[t - 1] + Q
      ap <- if (t == 1) a_prev else a[t - 1]
      Fv <- Pp + R
      v <- y[t] - ap
      K <- Pp / Fv
      a[t] <- ap + K * v
      P[t] <- Pp - K * Pp
      if (t > 1) ll <- ll + -0.5 * (log(2 * pi * Fv) + v^2 / Fv)
    }
    list(a = a, P = P, ll = ll)
  }

  # MLE over (log R, log Q) on the prediction-error decomposition --
  # what dlm::dlmMLE does for this model.
  v0 <- stats::var(diff(y)) / 2
  opt <- stats::optim(c(log(v0), log(v0)), function(p) {
    -kf(exp(p[1]), exp(p[2]))$ll
  }, method = "Nelder-Mead")
  R <- exp(opt$par[1])
  Q <- exp(opt$par[2])
  f <- kf(R, Q)
  a <- f$a
  P <- f$P

  # Rauch-Tung-Striebel smoother.
  a_s <- a
  P_s <- P
  for (t in (n - 1):1) {
    Pp <- P[t] + Q
    J <- P[t] / Pp
    a_s[t] <- a[t] + J * (a_s[t + 1] - a[t])
    P_s[t] <- P[t] + J^2 * (P_s[t + 1] - Pp)
  }
  list(
    filtered_state = a, filtered_state_variance = P,
    smoothed_state = a_s, loglik = f$ll, Q = Q, R = R, n = n,
    method = "Local-level Kalman filter+smoother, native MLE"
  )
}
