# SPDX-License-Identifier: AGPL-3.0-or-later
#' Proportional hazards by partial likelihood, with the Tsiatis baseline
#'
#' Horowitz, J. L. (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Springer, Section 6.2.1, pages 201-203 (volume
#' [Pages 189-232], read as rendered page images).  Note the sign convention:
#' the book writes the model as log Lambda_0(Y) = X beta + U with U
#' extreme-value (6.31), so the conditional hazard carries exp(-x beta), not
#' exp(+x beta), and every formula below follows that.  The partial likelihood
#' (6.32) is L_np(b) = prod_i exp(-X_i b) / sum_{j in R(Y_i)} exp(-X_j b),
#' with R(y) = {i : Y_i >= y} the risk set at y, maximised here by
#' Newton-Raphson on its logarithm.  The integrated baseline hazard is the
#' Tsiatis (1981) sample analogue of (6.35), printed as (6.36),
#' Lambda_n0(y) = sum_{i : Y_i <= y} 1 / sum_{j in R(Y_i)} exp(-X_j b_n).
#' The covariance of b_n is the inverse observed information, V_nb of (6.34).
#' A coefficient here is therefore the NEGATIVE of the usual Cox coefficient
#' for the same data; that is the book convention, not an error.
#'
#' @param t Observed durations Y.
#' @param x n-by-p covariate matrix WITHOUT an intercept: the baseline absorbs
#'   it, exactly as in the Cox model.
#' @param event Optional, 1 if the duration ended in the event and 0 if right
#'   censored; all ones if omitted.
#' @param max_iter,tol Newton controls.
#' @return list: estimate, beta_hat, se, h0_hat, event_times, Lambda0, n,
#'   n_events, method.
#' @keywords internal
#' @examples
#' Hrzph(c(1, 2, 3, 4), cbind(c(0, 1, 0, 1)))$beta_hat
#' @export
Hrzph <- function(t, x, event = NULL, max_iter = 100L, tol = 1e-12) {
  tt <- .s03vec(t)
  XX <- .s03mat(x)
  n <- length(tt)
  if (n == 0L) stop("horowitz_proportional_hazards: t is empty")
  if (nrow(XX) != n) {
    stop("horowitz_proportional_hazards: x has a different number of rows than t")
  }
  p <- ncol(XX)
  if (is.null(event)) {
    ev <- rep(1, n)
  } else {
    ev <- .s03vec(event)
    if (length(ev) != n) {
      stop("horowitz_proportional_hazards: event has a different length than t")
    }
  }
  ord <- order(tt, seq_len(n))
  beta <- numeric(p)
  H <- matrix(0, p, p)
  for (it in seq_len(as.integer(max_iter))) {
    g <- numeric(p)
    H <- matrix(0, p, p)
    for (i in ord) {
      if (ev[i] == 0) next
      s0 <- 0
      s1 <- numeric(p)
      s2 <- matrix(0, p, p)
      for (j in seq_len(n)) {
        if (tt[j] < tt[i]) next
        e <- 0
        for (k in seq_len(p)) e <- e + XX[j, k] * beta[k]
        w <- exp(min(max(-e, -300), 300))
        s0 <- s0 + w
        for (a in seq_len(p)) {
          s1[a] <- s1[a] + w * XX[j, a]
          for (b in seq_len(p)) s2[a, b] <- s2[a, b] + w * XX[j, a] * XX[j, b]
        }
      }
      for (a in seq_len(p)) {
        g[a] <- g[a] - XX[i, a] + s1[a] / s0
        for (b in seq_len(p)) {
          H[a, b] <- H[a, b] - (s2[a, b] / s0 - (s1[a] / s0) * (s1[b] / s0))
        }
      }
    }
    step <- .s03ridgesolve(-H, g, 1e-12)
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  times <- numeric(0)
  jumps <- numeric(0)
  cum <- numeric(0)
  running <- 0
  for (i in ord) {
    if (ev[i] == 0) next
    s0 <- 0
    for (j in seq_len(n)) {
      if (tt[j] < tt[i]) next
      e <- 0
      for (k in seq_len(p)) e <- e + XX[j, k] * beta[k]
      s0 <- s0 + exp(min(max(-e, -300), 300))
    }
    running <- running + 1 / s0
    times <- c(times, tt[i])
    jumps <- c(jumps, 1 / s0)
    cum <- c(cum, running)
  }
  se <- numeric(p)
  for (a in seq_len(p)) {
    e <- numeric(p)
    e[a] <- 1
    col <- .s03ridgesolve(-H, e, 1e-12)
    se[a] <- if (col[a] > 0) sqrt(col[a]) else NA_real_
  }
  list(estimate = beta[1], beta_hat = beta, se = se, h0_hat = jumps,
       event_times = times, Lambda0 = cum, n = n, n_events = length(times),
       method = paste0("Horowitz (2009) eq. (6.32) partial likelihood with ",
                       "exp(-x b); baseline by (6.36)"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzph
#' @keywords internal
#' @export
morie_horowitz_proportional_hazards <- Hrzph
