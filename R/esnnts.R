# SPDX-License-Identifier: AGPL-3.0-or-later

#' Echo state network (reservoir computing)
#'
#' Formula: a fixed random recurrent reservoir with a trained linear
#' readout,
#' \preformatted{
#'   x_t = (1 - a) x_{t-1} + a tanh(W x_{t-1} + W_in u_t)
#'   yhat_t = v' \[1; x_t\]
#' }
#' driven in one-step prediction mode (u_t = y_{t-1}, target y_t).  Only
#' v is fitted, by ridge regression; W and W_in are fixed and never
#' trained.
#'
#' The reservoir is generated deterministically -- entry k is drawn from
#' van der Corput base \code{PRIMES\[k mod 12\]} -- so both language arms
#' build the identical network.  W is rescaled so its induced infinity
#' norm equals \code{spectral_radius}; ||W||_inf < 1 is Jaeger's
#' sufficient condition for the echo state property (2001, Prop. 3).
#'
#' @param y Series (at least 3 points).
#' @param reservoir_size Number of reservoir units.
#' @param spectral_radius Target induced infinity norm of W.
#' @param leak Leaking rate a in (0, 1].
#' @param ridge Tikhonov regularisation for the readout.
#' @param washout Initial steps discarded; default min(size, n %/% 4).
#' @return List with \code{estimate}, \code{mse}, \code{nrmse},
#'   \code{coef}, \code{win}, \code{size}, \code{washout}, \code{nfit},
#'   \code{n}, \code{method}.
#' @references Jaeger (2001), GMD Report 148; Jaeger & Haas (2004),
#'   Science 304(5667):78-80, doi:10.1126/science.1091277.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Esnnts(V)
Esnnts <- function(y, reservoir_size = 20, spectral_radius = 0.9, leak = 1,
                   ridge = 1e-6, washout = NULL) {
  PR <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
  .drw <- function(k) {
    b <- PR[(k %% 12L) + 1L]
    2 * .s03vdc(k %/% 12L + 1L, b) - 1
  }
  y <- as.numeric(y)
  n <- length(y)
  if (n < 3L) stop("need at least 3 observations")
  size <- as.integer(reservoir_size)
  if (size < 1L) stop("reservoir_size must be positive")
  sr <- as.numeric(spectral_radius)
  if (sr < 0) stop("spectral_radius must be non-negative")
  a <- as.numeric(leak)
  if (!(a > 0 && a <= 1)) stop("leak must lie in (0, 1]")
  lam <- as.numeric(ridge)
  if (lam < 0) stop("ridge must be non-negative")
  wo <- if (is.null(washout)) min(size, n %/% 4L) else as.integer(washout)
  if (wo < 0L || wo >= n - 1L) stop("washout must lie in [0, n-1)")
  W <- matrix(0, size, size)
  for (i in seq_len(size)) for (j in seq_len(size))
    W[i, j] <- .drw((i - 1L) * size + (j - 1L))
  Win <- vapply(seq_len(size), function(i) .drw(size * size + i - 1L), 0)
  nrm <- max(rowSums(abs(W)))
  scale <- if (nrm > 0) sr / nrm else 0
  W <- W * scale
  x <- numeric(size)
  k <- size + 1L
  rows <- list(); targ <- numeric(0)
  for (t in 2:n) {
    u <- y[t - 1L]
    nx <- numeric(size)
    for (i in seq_len(size)) {
      z <- Win[i] * u
      for (j in seq_len(size)) z <- z + W[i, j] * x[j]
      nx[i] <- (1 - a) * x[i] + a * tanh(z)
    }
    x <- nx
    if ((t - 2L) >= wo) {
      rows[[length(rows) + 1L]] <- c(1, x)
      targ <- c(targ, y[t])
    }
  }
  nfit <- length(rows)
  if (nfit < 1L) stop("no rows left after washout")
  X <- matrix(unlist(rows), nrow = nfit, ncol = k, byrow = TRUE)
  XtX <- matrix(0, k, k)
  for (i in seq_len(k)) for (j in seq_len(k)) XtX[i, j] <- sum(X[, i] * X[, j])
  for (i in seq_len(k)) XtX[i, i] <- XtX[i, i] + lam
  Xty <- vapply(seq_len(k), function(i) sum(X[, i] * targ), 0)
  v <- .s03cholsolve(XtX, Xty)
  fit <- as.numeric(X %*% v)
  mse <- sum((targ - fit)^2) / nfit
  mt <- sum(targ) / nfit
  vt <- sum((targ - mt)^2) / nfit
  nrmse <- if (vt > 0) sqrt(mse / vt) else NaN
  .t1_result(estimate = mse, mse = mse, nrmse = nrmse, coef = v, win = Win,
             size = size, washout = wo, nfit = nfit, n = n,
             method = "Echo state network (reservoir computing)")
}
