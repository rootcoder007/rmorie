# Linear regression with real inference, robust standard errors and the
# standard diagnostics.
#
# R mirror of morie/src/morie/fn/_regression_core.py.  Both arms are
# verified against the reference implementations the field uses:
# lm/summary.lm, sandwich::vcovHC, sandwich::NeweyWest, lmtest::bptest,
# lmtest::dwtest and car::vif -- so morie provides them natively
# without depending on those packages at run time.

#' Ordinary least squares with inference
#'
#' Returns coefficients with standard errors, t statistics and
#' two-sided p-values, R^2 and adjusted R^2, the residual standard
#' error and the overall F test -- what `summary(lm(...))` reports.
#' Perfectly collinear predictors raise rather than silently returning
#' one arbitrary solution out of infinitely many.
#' @param y numeric response
#' @param X predictor matrix
#' @param add_intercept prepend an intercept column
#' @return list with `coef`, `se`, `t`, `p_value`, `r_squared`,
#'   `adj_r_squared`, `sigma`, `f_statistic`, `f_p_value`, `residuals`
#' @export
morie_ols <- function(y, X, add_intercept = TRUE) {
  # Two definitions of this function existed with OPPOSITE argument
  # orders -- (y, X) here and (X, y) in gp_mvsml.R -- and the second
  # silently overwrote the first at load, so half the callers in this
  # package were transposing their own data without any error.  Both
  # orders are accepted now, decided by which argument is rectangular.
  if (is.matrix(y) || is.data.frame(y)) {
    if (NCOL(y) > 1 && NCOL(X) == 1) {
      tmp <- y; y <- X; X <- tmp
    }
  }
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- length(y)
  if (nrow(X) != n) stop("X has ", nrow(X), " rows but y has ", n)
  if (add_intercept) X <- cbind(`(Intercept)` = 1, X)
  k <- ncol(X)
  if (n <= k) stop("need more observations than parameters")
  XtX <- crossprod(X)
  if (rcond(XtX) < .Machine$double.eps)
    stop("singular design matrix: predictors are perfectly collinear")
  XtXinv <- solve(XtX)
  beta <- as.numeric(XtXinv %*% crossprod(X, y))
  fitted <- as.numeric(X %*% beta)
  resid <- y - fitted
  df_resid <- n - k
  rss <- sum(resid^2)
  s2 <- rss / df_resid
  se <- sqrt(s2 * diag(XtXinv))
  tv <- beta / se
  pv <- 2 * stats::pt(abs(tv), df_resid, lower.tail = FALSE)
  tss <- if (add_intercept) sum((y - mean(y))^2) else sum(y^2)
  r2 <- 1 - rss / tss
  df_model <- if (add_intercept) k - 1 else k
  adj <- 1 - (1 - r2) * (n - as.integer(add_intercept)) / df_resid
  fstat <- ((tss - rss) / df_model) / s2
  list(coef = beta, se = se, t = tv, p_value = pv,
       fitted = fitted, residuals = resid, n = n, k = k,
       df_resid = df_resid, df_model = df_model, rss = rss, tss = tss,
       sigma2 = s2, sigma = sqrt(s2),
       # sigma2_ml, beta and se_beta come from the definition this one
       # absorbed, so code written against either name keeps working
       sigma2_ml = rss / n, beta = beta, se_beta = se,
       r_squared = r2, adj_r_squared = adj, f_statistic = fstat,
       f_p_value = stats::pf(fstat, df_model, df_resid,
                             lower.tail = FALSE),
       XtX_inv = XtXinv, design = X)
}

#' Heteroskedasticity-consistent covariance (White's sandwich)
#'
#' `V = (X'X)^-1 X' diag(omega) X (X'X)^-1` with `omega = e^2` (HC0),
#' `e^2 n/(n-k)` (HC1, the Stata default), `e^2/(1-h)` (HC2) or
#' `e^2/(1-h)^2` (HC3, best under leverage).  Use when the errors are
#' heteroskedastic: the coefficients stay unbiased but the textbook
#' standard errors do not.  Matches `sandwich::vcovHC`.
#' @param fit a `morie_ols` fit
#' @param kind one of "HC0", "HC1", "HC2", "HC3"
#' @return list with `vcov`, `se` and the leverages
#' @export
morie_robust_vcov <- function(fit, kind = "HC1") {
  X <- fit$design; e <- fit$residuals
  Ainv <- fit$XtX_inv; n <- fit$n; k <- fit$k
  h <- rowSums((X %*% Ainv) * X)
  om <- switch(toupper(kind),
               HC0 = e^2,
               HC1 = e^2 * n / (n - k),
               HC2 = e^2 / (1 - h),
               HC3 = e^2 / (1 - h)^2,
               stop("kind must be HC0, HC1, HC2 or HC3"))
  meat <- crossprod(X, X * om)
  V <- Ainv %*% meat %*% Ainv
  list(vcov = V, se = sqrt(diag(V)), leverage = h, kind = toupper(kind))
}

#' @rdname morie_robust_vcov
#' @export
morie_robust_se <- function(fit, kind = "HC1") {
  r <- morie_robust_vcov(fit, kind)
  tv <- fit$coef / r$se
  list(se = r$se, t = tv,
       p_value = 2 * stats::pt(abs(tv), fit$df_resid, lower.tail = FALSE),
       kind = r$kind, vcov = r$vcov)
}

#' Newey-West HAC covariance
#'
#' Heteroskedasticity- and autocorrelation-consistent covariance with
#' Bartlett weights `1 - l/(L+1)`, which guarantee a positive
#' semi-definite estimate.  `lags` defaults to
#' `floor(4 (n/100)^(2/9))`, Newey and West's own rule; note
#' `sandwich::NeweyWest` instead selects the bandwidth automatically,
#' so pass R an explicit `lag` to compare.
#' @param fit a `morie_ols` fit
#' @param lags Bartlett bandwidth
#' @return list with `vcov`, `se` and the `lags` used
#' @export
morie_newey_west_vcov <- function(fit, lags = NULL) {
  X <- fit$design; e <- fit$residuals
  Ainv <- fit$XtX_inv; n <- fit$n; k <- fit$k
  if (is.null(lags)) lags <- floor(4 * (n / 100)^(2 / 9))
  u <- X * e
  S <- crossprod(u)
  if (lags >= 1) for (l in seq_len(lags)) {
    g <- crossprod(u[(l + 1):n, , drop = FALSE],
                   u[1:(n - l), , drop = FALSE])
    S <- S + (1 - l / (lags + 1)) * (g + t(g))
  }
  S <- S * n / (n - k)
  V <- Ainv %*% S %*% Ainv
  list(vcov = V, se = sqrt(diag(V)), lags = lags)
}

#' Regression diagnostics
#'
#' `morie_breusch_pagan` tests for heteroskedasticity (Koenker's
#' studentised form, as `lmtest::bptest`); `morie_durbin_watson` gives
#' the first-order autocorrelation statistic, roughly `2(1 - rho)`;
#' `morie_vif` gives variance inflation factors `1/(1 - R_j^2)`, where
#' a value above about 10 means that coefficient's variance is inflated
#' an order of magnitude by collinearity.
#' @param fit a `morie_ols` fit
#' @param X predictor matrix
#' @param add_intercept include an intercept in the auxiliary fits
#' @return a list of the statistic and, where exact, its p-value
#' @export
morie_breusch_pagan <- function(fit) {
  X <- fit$design
  e2 <- fit$residuals^2
  aux <- morie_ols(e2, X[, -1, drop = FALSE], add_intercept = TRUE)
  stat <- fit$n * aux$r_squared
  df <- fit$k - 1
  list(statistic = stat, df = df,
       p_value = stats::pchisq(stat, df, lower.tail = FALSE),
       studentised = TRUE)
}

#' @rdname morie_breusch_pagan
#' @export
morie_durbin_watson <- function(fit) {
  e <- fit$residuals
  n <- length(e)
  den <- sum(e^2)
  list(statistic = sum(diff(e)^2) / den,
       rho = sum(e[-1] * e[-n]) / den, n = n)
}

#' @rdname morie_breusch_pagan
#' @export
morie_vif <- function(X, add_intercept = TRUE) {
  X <- as.matrix(X)
  p <- ncol(X)
  if (p < 2) stop("VIF needs at least 2 predictors")
  v <- vapply(seq_len(p), function(j) {
    r2 <- morie_ols(X[, j], X[, -j, drop = FALSE],
                    add_intercept = add_intercept)$r_squared
    if (r2 < 1) 1 / (1 - r2) else Inf
  }, numeric(1))
  list(vif = v)
}
