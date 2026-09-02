# SPDX-License-Identifier: AGPL-3.0-or-later
#' Schoenfeld residuals
#'
#' Schoenfeld (1982), Partial residuals for the proportional hazards
#' regression model, Biometrika 69(1), 239-241: r_ki = x_ki - xbar_k(t_i,
#' betahat) with xbar_k the risk-set average weighted by the fitted risk
#' scores, so the residual is defined only at EVENT times, one per event.
#' The note is paywalled; both expressions are quoted in their standard
#' published form.  The scaled residuals of Grambsch and Therneau (1994),
#' Biometrika 81(3), 515-526 -- r* = d V^-1 r + betahat -- are returned
#' too, because it is their correlation with time, not the raw residuals',
#' that tests proportional hazards.
#'
#' @param time,event follow-up times and event indicators.
#' @param X covariates.
#' @param beta the fitted Cox coefficients.
#' @return list: estimate, residuals, scaled, event_times, rho, V,
#'   n_events, method.
#' @keywords internal
#' @examples
#' Schoenres(c(1, 2, 3, 4), c(1, 1, 0, 1), matrix(c(0, 1, 0, 1), 4, 1),
#'           0.5)$rho
#' @export
Schoenres <- function(time, event, X, beta = NULL) {
  t <- .s03vec(time)
  e <- .s03vec(event)
  Xm <- .s03mat(X)
  n <- length(t)
  p <- ncol(Xm)
  b <- if (!is.null(beta)) .s03vec(beta) else numeric(p)
  ord <- order(t, seq_len(n))
  ets <- numeric(0)
  res <- list()
  V <- matrix(0, p, p)
  for (pos in seq_len(n)) {
    i <- ord[pos]
    if (e[i] < 0.5) next
    risk <- ord[t[ord] >= t[i]]
    wsum <- 0
    xbar <- numeric(p)
    for (j in risk) {
      eta <- 0
      for (a in seq_len(p)) eta <- eta + b[a] * Xm[j, a]
      wj <- exp(eta)
      wsum <- wsum + wj
      for (a in seq_len(p)) xbar[a] <- xbar[a] + wj * Xm[j, a]
    }
    xbar <- if (wsum > 0) xbar / wsum else numeric(p)
    for (j in risk) {
      eta <- 0
      for (a in seq_len(p)) eta <- eta + b[a] * Xm[j, a]
      wj <- if (wsum > 0) exp(eta) / wsum else 0
      for (a in seq_len(p)) for (cc in seq_len(p)) {
        V[a, cc] <- V[a, cc] + wj * (Xm[j, a] - xbar[a]) * (Xm[j, cc] - xbar[cc])
      }
    }
    ets <- c(ets, t[i])
    res[[length(res) + 1L]] <- as.numeric(Xm[i, ]) - xbar
  }
  d <- length(res)
  Vm <- if (d) V / d else V
  scaled <- list()
  for (r in res) {
    s <- .s03ridgesolve(Vm, r, 1e-10)
    scaled[[length(scaled) + 1L]] <- if (d) s / d + b else b
  }
  rho <- numeric(p)
  for (a in seq_len(p)) {
    col <- vapply(scaled, function(z) z[a], 0)
    rho[a] <- if (d > 1L) .s03corr(ets, col) else NaN
  }
  list(estimate = if (p) rho[1] else NaN, residuals = res, scaled = scaled,
       event_times = ets, rho = rho, V = Vm, n_events = d,
       method = "Schoenfeld (1982) residuals with the Grambsch-Therneau (1994) scaling")
}
