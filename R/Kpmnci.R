# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pointwise confidence interval for a Kaplan-Meier curve (Greenwood)
#'
#' Greenwood's variance of the product-limit estimator, with n_j at risk
#' and d_j events at t_j.  The plain (linear) pointwise interval is
#' S(t) +/- z sqrt(Var), truncated to [0, 1] because the linear scale can
#' leave the unit interval near the tails.  The curve is rebuilt from the
#' risk table so the same fit object can be reused by the simultaneous
#' band.
#'
#' Formula: Var[S(t)] = S(t)^2 sum_{t_j <= t} d_j / (n_j (n_j - d_j)).
#'
#' @param fit Risk table: a list with \code{time}, \code{n_risk} and
#'   \code{n_event}, one entry per distinct event time in increasing
#'   time order.
#' @param alpha Two-sided error rate.
#' @return List with \code{estimate} (S at the last time), \code{time},
#'   \code{surv}, \code{se}, \code{sigma2}, \code{lower}, \code{upper},
#'   \code{z}, \code{alpha}, \code{n_times}, \code{n_risk_start},
#'   \code{n}, \code{method}.
#' @references Greenwood (1926), The natural duration of cancer, Reports
#'   on Public Health and Medical Subjects 33:1-26, HMSO.
#' @export
Kpmnci <- function(fit, alpha) {
  rt <- .kpm_risk_table(fit)
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("km_pointwise_ci: alpha must lie in (0, 1)")
  z <- .s03qnorm(1 - a / 2)
  m <- rt$m; nr <- rt$nr; d <- rt$d
  S <- numeric(m); sig2 <- numeric(m); s <- 1; v <- 0
  for (j in seq_len(m)) {
    s <- s * (1 - d[j] / nr[j])
    v <- if (nr[j] > d[j]) v + d[j] / (nr[j] * (nr[j] - d[j])) else Inf
    S[j] <- s; sig2[j] <- v
  }
  se <- ifelse(is.finite(sig2), S * sqrt(sig2), NaN)
  lo <- pmax(S - z * se, 0); hi <- pmin(S + z * se, 1)
  .t1_result(estimate = S[m], time = rt$t, surv = S, se = se, sigma2 = sig2,
             lower = lo, upper = hi, z = z, alpha = a, n_times = m,
             n_risk_start = nr[1], n = m,
             method = "S(t) +/- z sqrt(S^2 sum d/(n(n-d))), Greenwood (1926)")
}

#' @keywords internal
#' @noRd
.kpm_risk_table <- function(fit) {
  if (is.null(fit)) stop("km_pointwise_ci: fit is empty")
  t <- .s03vec(fit$time); nr <- .s03vec(fit$n_risk); d <- .s03vec(fit$n_event)
  m <- length(t)
  if (m == 0L) stop("km_pointwise_ci: fit has no event times")
  if (length(nr) != m || length(d) != m) stop("km_pointwise_ci: time, n_risk and n_event have different lengths")
  if (any(nr <= 0)) stop("km_pointwise_ci: n_risk must be positive")
  if (any(d < 0 | d > nr)) stop("km_pointwise_ci: n_event must lie between 0 and n_risk")
  list(t = t, nr = nr, d = d, m = m)
}
