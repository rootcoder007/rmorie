# Unit-root testing and robust regression.
#
# R mirror of the corresponding block in
# morie/src/morie/fn/_robust_core.py, verified against urca::ur.df and
# MASS::rlm so morie provides them without depending on those packages.

#' Augmented Dickey-Fuller test
#'
#' Regresses `dy_t` on `y_{t-1}`, optionally a drift and trend, and
#' `lags` lagged differences, and reports the t statistic on the level
#' term.  The null is a unit root, so LARGE NEGATIVE values reject it,
#' and the statistic is not t-distributed under that null -- compare it
#' with the Dickey-Fuller critical values returned here, never with
#' normal ones.  Matches `urca::ur.df`.
#' @param y numeric series
#' @param lags number of lagged differences
#' @param kind "none", "drift" or "trend"
#' @return list with `statistic`, `critical_values` and `reject_5pct`
#' @export
#' @examples
#' morie_adf_test(y = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5))
morie_adf_test <- function(y, lags = 1, kind = "drift") {
  if (!kind %in% c("none", "drift", "trend"))
    stop('kind must be "none", "drift" or "trend"')
  n <- length(y)
  if (n < lags + 3) stop("series too short for ", lags, " lags")
  dy <- diff(y)
  idx <- (lags + 1):length(dy)
  X <- cbind(y[idx])
  if (kind %in% c("drift", "trend")) X <- cbind(X, 1)
  if (kind == "trend") X <- cbind(X, idx + 1)
  if (lags >= 1) for (i in 1:lags) X <- cbind(X, dy[idx - i])
  fit <- morie_ols(dy[idx], X, add_intercept = FALSE)
  stat <- fit$t[1]
  TAB <- switch(kind,
    none  = rbind(c(25,-2.66,-1.95,-1.60), c(50,-2.62,-1.95,-1.61),
                  c(100,-2.60,-1.95,-1.61), c(250,-2.58,-1.95,-1.62),
                  c(500,-2.58,-1.95,-1.62), c(1000,-2.58,-1.95,-1.62)),
    drift = rbind(c(25,-3.75,-3.00,-2.63), c(50,-3.58,-2.93,-2.60),
                  c(100,-3.51,-2.89,-2.58), c(250,-3.46,-2.88,-2.57),
                  c(500,-3.44,-2.87,-2.57), c(1000,-3.43,-2.86,-2.57)),
    trend = rbind(c(25,-4.38,-3.60,-3.24), c(50,-4.15,-3.50,-3.18),
                  c(100,-4.04,-3.45,-3.15), c(250,-3.99,-3.43,-3.13),
                  c(500,-3.98,-3.42,-3.13), c(1000,-3.96,-3.41,-3.12)))
  m <- length(idx)
  lo <- max(TAB[TAB[, 1] <= m, 1], TAB[1, 1])
  hi <- min(TAB[TAB[, 1] >= m, 1], TAB[nrow(TAB), 1])
  rl <- TAB[TAB[, 1] == lo, 2:4]
  rh <- TAB[TAB[, 1] == hi, 2:4]
  crit <- if (lo == hi) rl else rl + (m - lo) / (hi - lo) * (rh - rl)
  list(statistic = stat, kind = kind, lags = lags, n_used = m,
       coef = fit$coef, se = fit$se,
       critical_values = c(`1pct` = crit[1], `5pct` = crit[2],
                           `10pct` = crit[3]),
       reject_5pct = stat < crit[2])
}
