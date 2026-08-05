# SPDX-License-Identifier: AGPL-3.0-or-later

# Least squares of y on an intercept, x1 and optionally x2.
.dssoot_ols <- function(y, x1, x2 = NULL) {
  Z <- if (is.null(x2)) cbind(1, x1) else cbind(1, x1, x2)
  .s03lstsq(Z, y)
}

#' Percentile bootstrap confidence interval for the indirect effect ab
#'
#' The sampling distribution of the product \code{ab} is skewed, so the
#' Sobel normal approximation is wrong in finite samples; the remedy is
#' to resample cases with replacement, recompute \code{ab} in each
#' resample and read the interval off the empirical percentiles.  The
#' fitted equations are \code{M = i1 + a X} and
#' \code{Y = i2 + c' X + b M}, so the total effect \code{c = c' + ab} is
#' an exact least-squares identity in the single-mediator case.  The
#' resampling stream is a deterministic linear congruential generator,
#' not a system RNG, so both language arms draw the same case indices.
#'
#' @param Y Outcome.
#' @param X Independent variable.
#' @param M Mediator.
#' @param n_boot Number of bootstrap resamples, at least 1.
#' @param alpha Two-sided level of the percentile interval.
#' @param seed Seed of the deterministic congruential stream.
#' @return List with \code{estimate}, \code{a}, \code{b},
#'   \code{c_prime}, \code{c_total}, \code{ci_lo}, \code{ci_hi},
#'   \code{se_boot}, \code{bias}, \code{n_boot}, \code{alpha}, \code{n}.
#' @references Preacher, K. J. and Hayes, A. F. (2008). Asymptotic and
#'   resampling strategies for assessing and comparing indirect effects
#'   in multiple mediator models. Behavior Research Methods 40(3),
#'   879-891.
#' @export
Dssoot <- function(Y, X, M, n_boot = 1000L, alpha = 0.05, seed = 42L) {
  yv <- .s03vec(Y); xv <- .s03vec(X); mv <- .s03vec(M)
  n <- length(yv)
  if (n == 0L) stop("Dssoot: empty input, Y has no observations")
  if (length(xv) != n || length(mv) != n)
    stop("Dssoot: Y, X and M must have the same length")
  B <- as.integer(n_boot)
  if (B < 1L) stop("Dssoot: n_boot must be at least 1")
  if (!(alpha > 0 && alpha < 1))
    stop("Dssoot: alpha must lie strictly between 0 and 1")
  if (n < 3L) stop("Dssoot: need at least three observations")
  LA <- 1664525; LC <- 1013904223; LM <- 4294967296
  pa <- .dssoot_ols(mv, xv)
  pb <- .dssoot_ols(yv, xv, mv)
  pc <- .dssoot_ols(yv, xv)
  a <- pa[2L]; cp <- pb[2L]; b <- pb[3L]
  ab <- a * b
  state <- as.numeric(as.integer(seed) %% LM)
  draws <- numeric(B)
  for (bb in seq_len(B)) {
    idx <- integer(n)
    for (j in seq_len(n)) {
      state <- (LA * state + LC) %% LM
      idx[j] <- as.integer(floor((state / LM) * n)) %% n + 1L
    }
    qa <- .dssoot_ols(mv[idx], xv[idx])
    qb <- .dssoot_ols(yv[idx], xv[idx], mv[idx])
    draws[bb] <- qa[2L] * qb[3L]
  }
  draws <- sort(draws)
  .t1_result(estimate = ab, a = a, b = b, c_prime = cp, c_total = pc[2L],
             ci_lo = .s03quantile7(draws, alpha / 2),
             ci_hi = .s03quantile7(draws, 1 - alpha / 2),
             se_boot = if (B > 1L) .s03sd(draws) else NaN,
             bias = .s03mean(draws) - ab, n_boot = B, alpha = alpha,
             n = n, method = "Bootstrap CI for indirect effect ab")
}
