# R arm of baytsm -- the first-order polynomial DLM, forward filter and
# retrospective smoother. West, M. & Harrison, J. (1997) Bayesian
# Forecasting and Dynamic Models, 2nd ed., Springer, Ch. 2 and Sec. 4.8.
# Mirrors src/morie/fn/baytsm.py.

.baytsm_EPS <- 1e-12

#' morie_baytsm_dlm_local_level
#'
#' A step of the baytsm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param V Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param W Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param m0 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param C0 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e+06}.
#' @return A list with \code{estimate}, \code{smoothed}, \code{smoothed_var},
#' \code{filtered}, \code{filtered_var}, \code{forecast}, \code{forecast_var},
#' \code{adaptive_coefficient}, \code{forecast_error}, \code{loglik},
#' \code{signal_to_noise}, \code{n}, \code{V}, \code{W}, \code{method}, \code{note}.
#' @export
morie_baytsm_dlm_local_level <- function(y, V = 1.0, W = 0.1, m0 = 0.0,
                                         C0 = 1e6) {
  obs <- as.numeric(y)
  n <- length(obs)
  if (n == 0L) stop("baytsm: an empty series has nothing to filter")
  V <- as.numeric(V)
  W <- as.numeric(W)
  if (V <= 0.0) stop("baytsm: the observation variance must be positive")
  if (W < 0.0) stop("baytsm: the evolution variance cannot be negative")

  m <- as.numeric(m0)
  C <- as.numeric(C0)
  ms <- numeric(n)
  Cs <- numeric(n)
  Rs <- numeric(n)
  fs <- numeric(n)
  Qs <- numeric(n)
  As <- numeric(n)
  es <- numeric(n)
  loglik <- 0.0
  for (t in seq_len(n)) {
    R <- C + W
    f <- m
    Q <- R + V
    e <- obs[t] - f
    A <- R / Q
    m <- f + A * e
    C <- R - A * A * Q
    ms[t] <- m
    Cs[t] <- C
    Rs[t] <- R
    fs[t] <- f
    Qs[t] <- Q
    As[t] <- A
    es[t] <- e
    loglik <- loglik - 0.5 * (log(2.0 * pi * Q) + e * e / Q)
  }

  sm <- numeric(n)
  sC <- numeric(n)
  sm[n] <- ms[n]
  sC[n] <- Cs[n]
  if (n >= 2L) for (t in seq(n - 1L, 1L)) {
    B <- if (Rs[t + 1L] > .baytsm_EPS) Cs[t] / Rs[t + 1L] else 0.0
    sm[t] <- ms[t] + B * (sm[t + 1L] - ms[t])
    sC[t] <- Cs[t] + B * B * (sC[t + 1L] - Rs[t + 1L])
  }

  list(estimate = sm, smoothed = sm, smoothed_var = sC,
       filtered = ms, filtered_var = Cs,
       forecast = fs, forecast_var = Qs,
       adaptive_coefficient = As, forecast_error = es,
       loglik = loglik, signal_to_noise = W / V, n = as.integer(n),
       V = V, W = W,
       method = paste0("first-order polynomial DLM, forward filter and ",
                       "retrospective smoother (West & Harrison 1997 ",
                       "Ch. 2, Sec. 4.8)"),
       note = paste0("the adaptive coefficient A = R/(R+V) is the fraction ",
                     "of each forecast error taken into the state; it ",
                     "converges, so the filter forgets the past ",
                     "geometrically at a rate W/V fixes"))
}

#' .baytsm_cheatsheet
#'
#' A step of the baytsm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .baytsm_cheatsheet()
#' res
.baytsm_cheatsheet <- function() {
  paste0("baytsm: morie_baytsm_dlm_local_level(y, V, W, m0, C0) -> filtered ",
         "and smoothed states of the first-order DLM (West & Harrison 1997)")
}

morie_baytsm <- morie_baytsm_dlm_local_level
