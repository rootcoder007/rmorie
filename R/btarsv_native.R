# AR-sieve bootstrap for stationary time series.
# Source: Buhlmann (1997), Bernoulli 3(2), 123-148, Sec. 2
# (fetched-wave3/Sieve_bootstrap_for_time_series.pdf).  Mirrors
# Python morie.fn.btarsv exactly (same Levinson-Durbin fit, same
# SplitMix64 residual-index stream).

#' .btarsv_yw
#'
#' A step of the btarsv_native implementation. Called by \code{morie_btarsv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param xc A vector; its length is taken and its elements indexed.
#' @param p A count; the body uses it as \code{numeric(...)}.
#' @return A list with \code{phi}, \code{v}.
#' @export
.btarsv_yw <- function(xc, p) {
  n <- length(xc)
  g <- sapply(0:p, function(k) sum(xc[1:(n - k)] * xc[(1 + k):n]) / n)
  if (g[1] <= 0) stop("degenerate series")
  phi <- numeric(p)
  prev <- numeric(p)
  v <- g[1]
  for (k in 1:p) {
    acc <- g[k + 1]
    if (k > 1) acc <- acc - sum(prev[1:(k - 1)] * g[k:2])
    ref <- acc / v
    phi[k] <- ref
    if (k > 1) phi[1:(k - 1)] <- prev[1:(k - 1)] - ref * prev[(k - 1):1]
    v <- v * (1 - ref^2)
    prev <- phi
  }
  list(phi = phi[1:p], v = v)
}

#' Buhlmann AR-sieve bootstrap
#'
#' Subtract the sample mean, fit AR(p) by Yule-Walker (Levinson-
#' Durbin), centre the residuals, and regenerate B series by the
#' fitted recursion driven by iid draws from the centred residuals
#' (with burn-in), plus the mean.  p defaults to AIC selection.
#'
#' @param x Numeric series.
#' @param p Optional AR order (default AIC).
#' @param statistic Function(series) -> scalar (default mean).
#' @param B Bootstrap replicates.
#' @param burn Burn-in length.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @param p_max AIC search bound.
#' @return A list with elements \code{estimate}, \code{se},
#'   \code{replicates}, \code{phi}, \code{p}, \code{sigma2},
#'   \code{residual_mean}, \code{B}, \code{seed}, \code{method}.
#' @references Buhlmann, P. (1997). Sieve bootstrap for time series.
#'   Bernoulli, 3(2), 123-148.
#' @export
#' @examples
#' set.seed(1)
#' morie_btarsv(rnorm(60), B = 50, burn = 20)
morie_btarsv <- function(x, p = NULL, statistic = NULL, B = 500,
                         burn = 100, seed = 0, p_max = NULL) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 20) stop("need at least 20 observations")
  if (is.null(statistic)) statistic <- mean
  xbar <- mean(xv)
  xc <- xv - xbar
  if (is.null(p)) {
    pm <- if (is.null(p_max)) max(1, floor(10 * log10(n))) else p_max
    best_aic <- Inf
    best_p <- 1L
    for (cand in seq_len(min(pm, n %/% 3))) {
      v <- .btarsv_yw(xc, cand)$v
      aic <- n * log(max(v, 1e-300)) + 2 * cand
      if (aic < best_aic) { best_aic <- aic
      best_p <- cand }
    }
    p <- best_p
  }
  p <- as.integer(p)
  if (p < 1 || p >= n %/% 2) stop("order p out of range")
  fit <- .btarsv_yw(xc, p)
  phi <- fit$phi
  resid <- numeric(n - p)
  for (t in (p + 1):n) {
    resid[t - p] <- xc[t] - sum(phi * xc[(t - 1):(t - p)])
  }
  resid <- resid - mean(resid)
  m <- length(resid)
  e <- .ghc_rng(seed)
  that <- as.numeric(statistic(xv))
  reps <- numeric(B)
  for (b in seq_len(B)) {
    state <- numeric(p)
    series <- numeric(n)
    si <- 0L
    for (t in seq_len(burn + n)) {
      eps <- resid[min(floor(.ghc_unif(e, 1) * m), m - 1) + 1]
      val <- sum(phi * state) + eps
      state <- c(val, state[-p])
      if (t > burn) {
        si <- si + 1L
        series[si] <- val + xbar
      }
    }
    reps[b] <- as.numeric(statistic(series))
  }
  se <- stats::sd(reps)
  list(estimate = that, se = se, replicates = reps, phi = phi,
       p = p, sigma2 = fit$v, residual_mean = mean(resid),
       B = as.integer(B), seed = seed,
       method = "AR-sieve bootstrap (Buhlmann 1997, Sec. 2)")
}
