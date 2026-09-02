# SPDX-License-Identifier: AGPL-3.0-or-later

#' EM for state-space parameters
#'
#' Formula: E-step Kalman filter + RTS smoother (including the lag-one
#' smoothed covariance); M-step in closed form.  For the scalar model
#' \preformatted{
#'   x_t = phi x_{t-1} + w_t,  w_t ~ N(0, Q)
#'   y_t = x_t + v_t,          v_t ~ N(0, R)
#' }
#' with the smoothed sums S11 = sum(xs_t^2 + Ps_t),
#' S10 = sum(xs_t xs_\{t-1\} + Pcs_t), S00 = sum(xs_\{t-1\}^2 + Ps_\{t-1\}),
#' the M-step is exactly phi = S10 / S00, Q = (S11 - phi S10) / n and
#' R = mean((y_t - xs_t)^2 + Ps_t).
#'
#' @param y Observation sequence.
#' @param init Starting values (phi, Q, R); default (0.9, var/2, var/2).
#' @param max_iter Number of EM iterations; 0 returns the log-likelihood
#'   at the starting values without moving them.
#' @return List with \code{estimate}, \code{phi}, \code{Q}, \code{R},
#'   \code{loglik}, \code{loglik_path}, \code{iters}, \code{n},
#'   \code{method}.
#' @references Shumway & Stoffer (1982), J. Time Series Analysis
#'   3(4):253-264, doi:10.1111/j.1467-9892.1982.tb00349.x.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Emkfst(V)
Emkfst <- function(y, init = NULL, max_iter = 50) {
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  max_iter <- as.integer(max_iter)
  if (max_iter < 0L) stop("max_iter must be non-negative")
  if (is.null(init)) {
    m <- sum(y) / n
    v <- if (n > 1L) sum((y - m)^2) / (n - 1) else 1
    if (v <= 0) v <- 1
    phi <- 0.9; Q <- v / 2; R <- v / 2
  } else {
    init <- as.numeric(init)
    if (length(init) != 3L) stop("init must be (phi, Q, R)")
    phi <- init[1]; Q <- init[2]; R <- init[3]
  }
  if (Q < 0 || R <= 0) stop("Q must be non-negative and R positive")
  mu0 <- 0; Sig0 <- 1
  path <- numeric(0)
  loglik <- NaN
  for (.it in seq_len(max_iter + 1L)) {
    xp <- numeric(n + 1L); Pp <- numeric(n + 1L)
    xf <- numeric(n + 1L); Pf <- numeric(n + 1L)
    Kg <- numeric(n + 1L)
    xf[1] <- mu0; Pf[1] <- Sig0
    loglik <- 0
    for (t in seq_len(n)) {
      xp[t + 1L] <- phi * xf[t]
      Pp[t + 1L] <- phi * phi * Pf[t] + Q
      S <- Pp[t + 1L] + R
      Kg[t + 1L] <- Pp[t + 1L] / S
      e <- y[t] - xp[t + 1L]
      xf[t + 1L] <- xp[t + 1L] + Kg[t + 1L] * e
      Pf[t + 1L] <- (1 - Kg[t + 1L]) * Pp[t + 1L]
      loglik <- loglik - 0.5 * (log(2 * pi * S) + e * e / S)
    }
    path <- c(path, loglik)
    if (length(path) > max_iter) break
    xs <- xf; Ps <- Pf; J <- numeric(n + 1L)
    for (t in seq(n, 1L)) {
      J[t] <- if (Pp[t + 1L] > 0) Pf[t] * phi / Pp[t + 1L] else 0
      xs[t] <- xf[t] + J[t] * (xs[t + 1L] - xp[t + 1L])
      Ps[t] <- Pf[t] + J[t] * J[t] * (Ps[t + 1L] - Pp[t + 1L])
    }
    Pcs <- numeric(n + 1L)
    Pcs[n + 1L] <- (1 - Kg[n + 1L]) * phi * Pf[n]
    if (n > 1L) for (t in seq(n, 2L)) {
      Pcs[t] <- Pf[t] * J[t - 1L] + J[t] * (Pcs[t + 1L] - phi * Pf[t]) * J[t - 1L]
    }
    S11 <- 0; S10 <- 0; S00 <- 0
    for (t in seq_len(n)) {
      S11 <- S11 + xs[t + 1L] * xs[t + 1L] + Ps[t + 1L]
      S10 <- S10 + xs[t + 1L] * xs[t] + Pcs[t + 1L]
      S00 <- S00 + xs[t] * xs[t] + Ps[t]
    }
    phi <- if (S00 > 0) S10 / S00 else 0
    Q <- (S11 - phi * S10) / n
    if (Q < 0) Q <- 0
    R <- sum((y - xs[-1L])^2 + Ps[-1L]) / n
    if (R <= 0) R <- 1e-12
  }
  .t1_result(estimate = phi, phi = phi, Q = Q, R = R, loglik = loglik,
             loglik_path = path, iters = max_iter, n = n,
             method = "EM for state-space parameters")
}
