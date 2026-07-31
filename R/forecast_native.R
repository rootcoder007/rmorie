# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Forecasting shelf. R mirrors of the morie.fn modules jonaiv, driftF,
# joses, hwadd, hwmul, croston, jocros, esttsl and jomint.
#
# All deterministic. Holt-Winters additive and multiplicative run
# through one recursion here rather than two copies of it: the Python
# modules duplicate the loop with a flag, and a seasonal update
# implemented twice is a seasonal update that eventually disagrees with
# itself.

#' Naive and seasonal-naive forecast
#'
#' Carries the last observation forward, or the last observation from
#' the same point in the season.
#'
#' This is the benchmark every other method has to beat, and it is not a
#' straw man: for a random walk it is the OPTIMAL forecast, so a model
#' that fails to beat it on a near-random-walk series is not
#' underperforming, it is being told the series has no structure. The
#' in-sample mean absolute change returned here is the MASE denominator.
#'
#' @param y series.
#' @param horizon steps ahead.
#' @param season seasonal period; NULL for the plain naive forecast.
#' @return list with \code{forecast}, \code{method_used},
#'   \code{last_value}, \code{in_sample_mae}.
#' @references Hyndman, R. J. and Athanasopoulos, G. (2021).
#'   \emph{Forecasting: Principles and Practice}, 3rd ed., Sec. 5.2.
#' @examples
#' morie_joseph_naive_forecast(c(3, 5, 4, 6), horizon = 2)$forecast
#' @export
morie_joseph_naive_forecast <- function(y, horizon = 1, season = NULL) {
  y <- as.numeric(y)
  if (length(y) == 0L) stop("y must be non-empty", call. = FALSE)
  horizon <- as.integer(horizon)
  if (horizon < 1L) stop("horizon must be at least 1", call. = FALSE)
  n <- length(y)
  if (is.null(season)) {
    fc <- rep(y[n], horizon)
    name <- "naive"
    denom <- if (n > 1L) mean(abs(diff(y))) else NA_real_
  } else {
    m <- as.integer(season)
    if (m < 1L || m > n) {
      stop(sprintf("season must be between 1 and %d", n), call. = FALSE)
    }
    fc <- vapply(seq_len(horizon),
                 function(h) y[n - m + ((h - 1L) %% m) + 1L], numeric(1))
    name <- sprintf("seasonal naive (m=%d)", m)
    denom <- if (n > m) mean(abs(y[(m + 1L):n] - y[seq_len(n - m)])) else NA_real_
  }
  list(forecast = fc, method_used = name, last_value = y[n],
       in_sample_mae = denom, horizon = horizon, season = season,
       method = "joseph_naive_forecast")
}


#' Random-walk-with-drift forecast
#'
#' Extrapolates the straight line through the FIRST and LAST
#' observations: the drift is \eqn{(y_n - y_1)/(n-1)} and everything in
#' between is ignored.
#'
#' That is not an approximation, it is the estimator -- which is why a
#' single unusual endpoint moves the whole forecast, and why the method
#' should be looked at rather than trusted when either end of the series
#' is atypical. Interval width grows with \eqn{\sqrt{h(1 + h/(n-1))}},
#' faster than the naive forecast's \eqn{\sqrt h}, because the drift
#' itself is estimated.
#'
#' @param y series.
#' @param h steps ahead.
#' @return list with \code{forecast}, \code{drift}, \code{se},
#'   \code{lower}, \code{upper}, \code{sigma}.
#' @references Hyndman, R. J. and Athanasopoulos, G. (2021).
#'   \emph{Forecasting: Principles and Practice}, 3rd ed., Sec. 5.2.
#' @examples
#' round(morie_drift_forecast(c(1, 3, 2, 5, 7), h = 3)$forecast, 2)
#' @export
morie_drift_forecast <- function(y, h = 1) {
  y <- as.numeric(y)
  n <- length(y)
  if (n < 2L) {
    stop("need at least 2 observations to estimate a drift", call. = FALSE)
  }
  h <- as.integer(h)
  if (h < 1L) stop("h must be at least 1", call. = FALSE)
  drift <- (y[n] - y[1L]) / (n - 1)
  steps <- seq_len(h)
  fc <- y[n] + steps * drift
  resid <- diff(y) - drift
  sigma <- if (length(resid) > 1L) stats::sd(resid) else 0
  se <- sigma * sqrt(steps * (1 + steps / max(n - 1, 1)))
  list(forecast = fc, drift = drift, se = se, lower = fc - 1.96 * se,
       upper = fc + 1.96 * se, sigma = sigma, h = h,
       method = "drift_forecast")
}


.morie_ses_run <- function(a, series) {
  lev <- series[1L]
  fit <- numeric(length(series))
  for (i in seq_along(series)) {
    fit[i] <- lev
    lev <- a * series[i] + (1 - a) * lev
  }
  list(fitted = fit, level = lev)
}


#' Simple exponential smoothing
#'
#' \eqn{\ell_t = \alpha y_t + (1-\alpha)\ell_{t-1}}, with alpha chosen
#' by grid search on the in-sample SSE when not supplied.
#'
#' The forecast is FLAT at every horizon, because the model has a level
#' and nothing else. On a trending series that is not a mild
#' approximation -- the forecast is wrong by an amount that grows with
#' the horizon, and Holt's method is the fix. The reported
#' \code{effective_window} is \eqn{2/\alpha - 1}, the number of past
#' observations the smoother is effectively averaging.
#'
#' @param y series.
#' @param alpha smoothing parameter in (0, 1]; fitted when NULL.
#' @param horizon steps ahead.
#' @return list with \code{forecast}, \code{level}, \code{fitted},
#'   \code{residuals}, \code{alpha}, \code{sse},
#'   \code{effective_window}.
#' @references Hyndman, R. J. et al. (2008). \emph{Forecasting with
#'   Exponential Smoothing}. Springer.
#' @examples
#' round(morie_joseph_simple_exponential_smoothing(c(3, 5, 4, 6, 5))$alpha, 2)
#' @export
morie_joseph_simple_exponential_smoothing <- function(y, alpha = NULL,
                                                      horizon = 1) {
  y <- as.numeric(y)
  if (length(y) < 2L) stop("need at least 2 observations", call. = FALSE)
  horizon <- as.integer(horizon)
  if (horizon < 1L) stop("horizon must be at least 1", call. = FALSE)
  if (is.null(alpha)) {
    grid <- seq(0.01, 1, length.out = 100L)
    sses <- vapply(grid,
                   function(a) sum((y - .morie_ses_run(a, y)$fitted)^2),
                   numeric(1))
    alpha <- grid[which.min(sses)]
  } else {
    alpha <- as.numeric(alpha)
    if (alpha <= 0 || alpha > 1) {
      stop("alpha must be in (0, 1]", call. = FALSE)
    }
  }
  r <- .morie_ses_run(alpha, y)
  resid <- y - r$fitted
  list(forecast = rep(r$level, horizon), level = r$level, fitted = r$fitted,
       residuals = resid, alpha = alpha, sse = sum(resid^2),
       effective_window = 2 / alpha - 1, horizon = horizon,
       method = "joseph_simple_exponential_smoothing")
}


# One Holt-Winters recursion, additive or multiplicative.
.morie_holt_winters <- function(y, period, alpha, beta, gamma, horizon,
                                mult) {
  y <- as.numeric(y)
  m <- as.integer(period)
  if (m < 2L) stop("period must be at least 2", call. = FALSE)
  if (length(y) < 2L * m) {
    stop(sprintf("need at least %d observations for period %d", 2L * m, m),
         call. = FALSE)
  }
  for (nm in c("alpha", "beta", "gamma")) {
    v <- switch(nm, alpha = alpha, beta = beta, gamma = gamma)
    if (v < 0 || v > 1) {
      stop(sprintf("%s must be in [0, 1]", nm), call. = FALSE)
    }
  }
  horizon <- if (is.null(horizon)) m else as.integer(horizon)
  if (horizon < 1L) stop("horizon must be at least 1", call. = FALSE)
  if (mult && any(y <= 0)) {
    stop("multiplicative seasonality needs strictly positive data",
         call. = FALSE)
  }
  s0 <- mean(y[seq_len(m)])
  s1 <- mean(y[(m + 1L):(2L * m)])
  level <- s0
  trend <- (s1 - s0) / m
  seas <- if (mult) y[seq_len(m)] / s0 else y[seq_len(m)] - s0
  n <- length(y)
  fitted <- numeric(n)
  for (t in seq_len(n)) {
    sea <- seas[t]
    fitted[t] <- if (mult) (level + trend) * sea else level + trend + sea
    prev <- level
    if (mult) {
      level <- alpha * (y[t] / max(sea, 1e-12)) + (1 - alpha) * (level + trend)
      new_sea <- gamma * (y[t] / max(level, 1e-12)) + (1 - gamma) * sea
    } else {
      level <- alpha * (y[t] - sea) + (1 - alpha) * (level + trend)
      new_sea <- gamma * (y[t] - level) + (1 - gamma) * sea
    }
    trend <- beta * (level - prev) + (1 - beta) * trend
    seas <- c(seas, new_sea)
  }
  steps <- seq_len(horizon)
  ns <- length(seas)
  tail_s <- vapply(steps, function(h) seas[ns - m + ((h - 1L) %% m) + 1L],
                   numeric(1))
  fc <- if (mult) {
    (level + steps * trend) * tail_s
  } else {
    level + steps * trend + tail_s
  }
  resid <- y - fitted
  list(forecast = fc, level = level, trend = trend,
       seasonal = seas[(ns - m + 1L):ns], fitted = fitted, residuals = resid,
       sse = sum(resid^2), alpha = alpha, beta = beta, gamma = gamma,
       period = m, horizon = horizon)
}


#' Holt-Winters with additive seasonality
#'
#' Level, trend and seasonal recursions with the season ADDED, which is
#' the right choice when the seasonal swing is a fixed number of units
#' regardless of the level.
#'
#' Three smoothing parameters is a great deal to ask of a short series,
#' and the method needs two full periods before it can even initialise.
#' On anything under about four periods the seasonal estimates are
#' mostly the initialisation.
#'
#' @param y series.
#' @param period seasonal period.
#' @param alpha,beta,gamma smoothing parameters in \eqn{[0, 1]}.
#' @param horizon steps ahead; defaults to one full period.
#' @return list with \code{forecast}, \code{level}, \code{trend},
#'   \code{seasonal}, \code{fitted}, \code{residuals}, \code{sse}.
#' @references Winters, P. R. (1960). Forecasting sales by exponentially
#'   weighted moving averages. \emph{Management Science}, 6(3), 324-342.
#' @examples
#' y <- rep(c(10, 20, 30, 20), 5) + seq_len(20)
#' round(morie_holt_winters_additive(y)$forecast, 1)
#' @export
morie_holt_winters_additive <- function(y, period = 4, alpha = 0.3,
                                        beta = 0.1, gamma = 0.1,
                                        horizon = NULL) {
  out <- .morie_holt_winters(y, period, alpha, beta, gamma, horizon,
                             mult = FALSE)
  out$method <- "holt_winters_additive"
  out
}


#' Holt-Winters with multiplicative seasonality
#'
#' As \code{\link{morie_holt_winters_additive}} but with the season
#' MULTIPLYING the level, which is the right choice when the seasonal
#' swing grows in proportion to the level.
#'
#' The data must be strictly positive -- a zero makes the seasonal ratio
#' undefined, and the function refuses rather than dividing by a fudge
#' factor and returning a number.
#'
#' @inheritParams morie_holt_winters_additive
#' @return as \code{\link{morie_holt_winters_additive}}.
#' @references Winters, P. R. (1960). Forecasting sales by
#'   exponentially weighted moving averages. \emph{Management Science},
#'   6(3), 324-342.
#' @examples
#' y <- rep(c(10, 20, 30, 20), 5) * (1 + seq_len(20) / 40)
#' round(morie_holt_winters_mult(y)$forecast, 1)
#' @export
morie_holt_winters_mult <- function(y, period = 4, alpha = 0.3, beta = 0.1,
                                    gamma = 0.1, horizon = NULL) {
  out <- .morie_holt_winters(y, period, alpha, beta, gamma, horizon,
                             mult = TRUE)
  out$method <- "holt_winters_mult"
  out
}


#' Croston's method for intermittent demand
#'
#' Smooths the demand SIZE and the INTERVAL between demands separately,
#' and forecasts the rate as their ratio.
#'
#' Ordinary exponential smoothing on a series full of zeros chases the
#' zeros and produces a forecast that is always too low right after a
#' demand and too high long after one. Splitting size from interval
#' removes that.
#'
#' The plain estimator is biased UPWARD by roughly \eqn{1/(1-\alpha/2)}
#' -- about 5% at alpha = 0.1 -- because the ratio of two smoothed
#' quantities is not the smoothed ratio. \code{variant = "sba"} applies
#' the Syntetos-Boylan correction and is the better default.
#'
#' @param y non-negative demand series.
#' @param alpha smoothing parameter in (0, 1].
#' @param variant \code{"croston"} or \code{"sba"}.
#' @return list with \code{forecast}, \code{rate}, \code{demand_size},
#'   \code{interval}, \code{bias_factor}, \code{intermittency}.
#' @references Croston, J. D. (1972). Forecasting and stock control for
#'   intermittent demands. \emph{Operational Research Quarterly}, 23(3),
#'   289-303. Syntetos, A. A. and Boylan, J. E. (2005). The accuracy of
#'   intermittent demand estimates. \emph{IJF}, 21(2), 303-314.
#' @examples
#' round(morie_croston(c(0, 0, 5, 0, 0, 0, 3, 0, 4))$forecast, 3)
#' @export
morie_croston <- function(y, alpha = 0.1, variant = c("croston", "sba")) {
  variant <- match.arg(variant)
  y <- as.numeric(y)
  if (any(y < 0)) stop("demand must be non-negative", call. = FALSE)
  alpha <- as.numeric(alpha)
  if (alpha <= 0 || alpha > 1) {
    stop("alpha must be in (0, 1]", call. = FALSE)
  }
  nz <- which(y > 0)
  if (length(nz) == 0L) {
    stop("the series has no nonzero demand", call. = FALSE)
  }
  z <- y[nz[1L]]
  p <- as.numeric(nz[1L])
  last <- nz[1L]
  if (length(nz) > 1L) {
    for (i in nz[-1L]) {
      z <- alpha * y[i] + (1 - alpha) * z
      p <- alpha * (i - last) + (1 - alpha) * p
      last <- i
    }
  }
  rate <- z / max(p, 1e-12)
  bias <- if (identical(variant, "sba")) 1 - alpha / 2 else 1
  list(forecast = rate * bias, rate = rate, demand_size = z, interval = p,
       bias_factor = bias, n_nonzero = length(nz),
       intermittency = 1 - length(nz) / length(y), alpha = alpha,
       variant = variant,
       warnings = if (identical(variant, "croston")) {
         paste("the plain Croston estimator is biased upward by about",
               "1/(1 - alpha/2); consider variant='sba'")
       } else {
         character(0)
       },
       method = "croston")
}


#' Croston with demand classification
#'
#' \code{\link{morie_croston}} with the Syntetos-Boylan-Croston
#' quadrant: average interval against squared coefficient of variation
#' of the nonzero demands, split at 1.32 and 0.49.
#'
#' The classification is the useful part. Croston's method is designed
#' for INTERMITTENT demand, and applying it to a smooth series makes
#' things worse rather than better -- ordinary exponential smoothing
#' wins there, and the function says so.
#'
#' @inheritParams morie_croston
#' @return list as \code{\link{morie_croston}} plus
#'   \code{classification} (smooth / erratic / intermittent / lumpy),
#'   \code{cv_squared}, \code{average_interval}.
#' @references Syntetos, A. A., Boylan, J. E. and Croston, J. D. (2005).
#'   On the categorization of demand patterns. \emph{JORS}, 56(5),
#'   495-503.
#' @examples
#' morie_joseph_croston_intermittent(c(0, 0, 5, 0, 0, 0, 3, 0, 4))$classification
#' @export
morie_joseph_croston_intermittent <- function(y, alpha = 0.1,
                                              variant = c("sba", "croston")) {
  variant <- match.arg(variant)
  r <- morie_croston(y, alpha = alpha, variant = variant)
  v <- as.numeric(y)
  nz <- v[v > 0]
  cv2 <- if (length(nz) > 1L) (stats::sd(nz) / mean(nz))^2 else 0
  p <- r$interval
  cls <- if (p >= 1.32 && cv2 >= 0.49) {
    "lumpy"
  } else if (p >= 1.32) {
    "intermittent"
  } else if (cv2 >= 0.49) {
    "erratic"
  } else {
    "smooth"
  }
  list(forecast = r$forecast, rate = r$rate, demand_size = r$demand_size,
       interval = r$interval, bias_factor = r$bias_factor,
       n_nonzero = r$n_nonzero, intermittency = r$intermittency,
       classification = cls, cv_squared = cv2, average_interval = p,
       alpha = r$alpha, variant = variant,
       warnings = c(r$warnings, if (identical(cls, "smooth")) {
         paste("demand is smooth; ordinary exponential smoothing is more",
               "appropriate than Croston here")
       }),
       method = "joseph_croston_intermittent")
}


#' Theta method
#'
#' Decomposes the series into theta-lines, smooths, and recombines.
#'
#' The result is worth stating plainly because it is what makes the
#' method usable: for theta = 2 the whole procedure is EXACTLY simple
#' exponential smoothing with a drift of half the fitted linear slope
#' (Hyndman & Billah 2003). The M3 competition winner is two lines of
#' arithmetic on top of SES, not a black box.
#'
#' @param y series.
#' @param horizon steps ahead.
#' @param theta theta parameter.
#' @return list with \code{forecast}, \code{drift}, \code{alpha},
#'   \code{linear_slope}, \code{theta_line}, \code{level}.
#' @references Assimakopoulos, V. and Nikolopoulos, K. (2000). The theta
#'   model. \emph{IJF}, 16(4), 521-530. Hyndman, R. J. and Billah, B.
#'   (2003). Unmasking the Theta method. \emph{IJF}, 19(2), 287-290.
#' @examples
#' round(morie_theta_method(c(1, 3, 2, 5, 4, 7, 6, 9), horizon = 2)$drift, 3)
#' @export
morie_theta_method <- function(y, horizon = 1, theta = 2) {
  y <- as.numeric(y)
  n <- length(y)
  if (n < 3L) stop("need at least 3 observations", call. = FALSE)
  horizon <- as.integer(horizon)
  if (horizon < 1L) stop("horizon must be at least 1", call. = FALSE)
  t <- seq_len(n)
  A <- cbind(1, t)
  coef <- as.vector(qr.solve(A, y))
  slope <- coef[2L]
  line0 <- as.vector(A %*% coef)
  line_theta <- theta * y + (1 - theta) * line0
  grid <- seq(0.05, 1, length.out = 60L)
  sses <- vapply(grid, function(a) {
    sum((line_theta - .morie_ses_run(a, line_theta)$fitted)^2)
  }, numeric(1))
  alpha <- grid[which.min(sses)]
  level <- .morie_ses_run(alpha, line_theta)$level
  drift <- slope / 2
  steps <- seq_len(horizon)
  list(forecast = level + drift * steps, drift = drift, alpha = alpha,
       linear_slope = slope, theta_line_0 = line0, theta_line = line_theta,
       level = level, horizon = horizon, theta = theta,
       method = "theta_method")
}


#' MinT / OLS / WLS forecast reconciliation
#'
#' Projects base forecasts of a hierarchy onto the coherent subspace
#' \eqn{\{Sb\}} using \eqn{G = (S'W^{-1}S)^{-1}S'W^{-1}}.
#'
#' The projection is orthogonal in the W metric, which has a consequence
#' worth relying on: reconciling CANNOT increase expected error. That is
#' not true of bottom-up or top-down, both of which throw away the
#' information in the levels they do not use.
#'
#' \code{method = "ols"} ignores W entirely, \code{"wls"} keeps only its
#' diagonal, and \code{"mint"} uses the full covariance.
#'
#' @param y_hat base forecasts, one per node.
#' @param S summing matrix, nodes by bottom-level series.
#' @param W covariance of the base forecast errors.
#' @param method \code{"ols"}, \code{"wls"} or \code{"mint"}.
#' @return list with \code{reconciled}, \code{bottom}, \code{coherent},
#'   \code{adjustment}, \code{incoherence_before}, \code{G}.
#' @references Wickramasuriya, S. L., Athanasopoulos, G. and Hyndman,
#'   R. J. (2019). Optimal forecast reconciliation for hierarchical and
#'   grouped time series through trace minimization. \emph{JASA},
#'   114(526), 804-819.
#' @examples
#' S <- rbind(c(1, 1), diag(2))
#' morie_joseph_mint_reconciliation(c(10, 4, 5), S)$coherent
#' @export
morie_joseph_mint_reconciliation <- function(y_hat, S, W = NULL,
                                             method = c("ols", "wls",
                                                        "mint")) {
  method <- match.arg(method)
  y <- as.numeric(y_hat)
  S <- as.matrix(S)
  storage.mode(S) <- "double"
  if (length(y) != nrow(S)) {
    stop(sprintf("y_hat has %d entries but S has %d rows", length(y),
                 nrow(S)), call. = FALSE)
  }
  n <- nrow(S)
  Wm <- if (is.null(W)) diag(n) else as.matrix(W)
  if (!identical(dim(Wm), c(n, n))) {
    stop(sprintf("W must be (%d, %d)", n, n), call. = FALSE)
  }
  Wm <- switch(method, ols = diag(n), wls = diag(diag(Wm), n, n), mint = Wm)
  Wi <- .morie_ginv(Wm)
  G <- .morie_ginv(t(S) %*% Wi %*% S) %*% t(S) %*% Wi
  bottom <- as.vector(G %*% y)
  rec <- as.vector(S %*% bottom)
  resid <- y - as.vector(S %*% (.morie_ginv(S) %*% y))
  list(reconciled = rec, bottom = bottom,
       coherent = isTRUE(all.equal(rec, as.vector(S %*% bottom))),
       adjustment = rec - y, incoherence_before = sqrt(sum(resid^2)), G = G,
       method_used = method, method = "joseph_mint_reconciliation")
}
