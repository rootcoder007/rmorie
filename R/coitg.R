# SPDX-License-Identifier: AGPL-3.0-or-later

# Native augmented Dickey-Fuller regression with optional information-
# criterion lag selection, reproducing urca::ur.df() (Pfaff 2008) term
# for term: the sample is always trimmed by the MAXIMUM lag (so every
# candidate regression uses the same observations), the AIC/BIC search
# runs over 1..lags augmentation lags (lag 0 is never a candidate once
# lags >= 1), the criterion is stats::AIC() of the Gaussian lm fit, and
# the Dickey-Fuller critical values are urca's Fuller (1976) table rows
# chosen by the number of differences.  Shared by morie_eg_coint() and
# morie_ts_stationarity().
#
# Returns list(statistic, lags, cval) where cval is the c(1pct, 5pct,
# 10pct) row for the tau statistic.
.morie_urdf <- function(y, type = c("none", "drift", "trend"), lags = 1L,
                        selectlags = c("Fixed", "AIC", "BIC")) {
  type <- match.arg(type)
  selectlags <- match.arg(selectlags)
  y <- as.numeric(y)
  if (anyNA(y)) stop("NAs in y.", call. = FALSE)
  lags <- as.integer(lags)
  if (lags < 0L) stop("Lags must be a non-negative integer.", call. = FALSE)
  L <- lags + 1L
  z <- diff(y)
  n <- length(z)
  if (n - L + 1L < L + 2L) stop("series too short for ", lags, " lags.",
                                call. = FALSE)
  emb <- stats::embed(z, L)
  dz <- emb[, 1]
  zlag1 <- y[L:n]
  tt <- L:n
  design <- function(nl) {
    X <- cbind(zlag1)
    if (type %in% c("drift", "trend")) X <- cbind(X, 1)
    if (type == "trend") X <- cbind(X, tt)
    if (nl >= 1L) X <- cbind(X, emb[, 2:(nl + 1L), drop = FALSE])
    X
  }
  fit_ols <- function(X) {
    f <- stats::lm.fit(X, dz)
    rss <- sum(f$residuals^2)
    list(fit = f, rss = rss, p = f$rank)
  }
  used <- lags
  if (lags >= 1L && selectlags != "Fixed") {
    N <- length(dz)
    pen <- if (selectlags == "AIC") 2 else log(N)
    crit <- rep(NA_real_, L)
    for (i in 2:L) {
      o <- fit_ols(design(i - 1L))
      # stats::logLik.lm (unweighted): 0.5 * (-N (log(2 pi) + 1 - log N
      # + log RSS)), with df = rank + 1 (the residual variance).
      ll <- 0.5 * (-N * (log(2 * pi) + 1 - log(N) + log(o$rss)))
      crit[i] <- -2 * ll + pen * (o$p + 1)
    }
    used <- which.min(crit) - 1L
  }
  o <- fit_ols(design(used))
  p <- o$p
  rdf <- length(dz) - p
  R <- o$fit$qr$qr[seq_len(p), seq_len(p), drop = FALSE]
  covu <- chol2inv(R)
  piv <- o$fit$qr$pivot[seq_len(p)]
  se <- sqrt(diag(covu) * (o$rss / rdf))
  est <- o$fit$coefficients[piv]
  pos <- match(1L, piv)
  stat <- as.numeric(est[pos] / se[pos])
  rowselec <- if (n < 25) 1L else if (n < 50) 2L else if (n < 100) 3L else
    if (n < 250) 4L else if (n < 500) 5L else 6L
  TAB <- switch(type,
    none = rbind(c(-2.66, -1.95, -1.6), c(-2.62, -1.95, -1.61),
                 c(-2.6, -1.95, -1.61), c(-2.58, -1.95, -1.62),
                 c(-2.58, -1.95, -1.62), c(-2.58, -1.95, -1.62)),
    drift = rbind(c(-3.75, -3, -2.63), c(-3.58, -2.93, -2.6),
                  c(-3.51, -2.89, -2.58), c(-3.46, -2.88, -2.57),
                  c(-3.44, -2.87, -2.57), c(-3.43, -2.86, -2.57)),
    trend = rbind(c(-4.38, -3.6, -3.24), c(-4.15, -3.5, -3.18),
                  c(-4.04, -3.45, -3.15), c(-3.99, -3.43, -3.13),
                  c(-3.98, -3.42, -3.13), c(-3.96, -3.41, -3.12)))
  cval <- stats::setNames(TAB[rowselec, ], c("1pct", "5pct", "10pct"))
  list(statistic = stat, lags = as.integer(used), cval = cval)
}

#' Engle-Granger two-step cointegration test
#'
#' Step 1 regresses \code{y1} on \code{y2} by OLS; step 2 runs an
#' augmented Dickey-Fuller regression without deterministic terms on the
#' step-1 residuals, choosing the number of augmentation lags in
#' \code{1..max_lag} by AIC on a common sample.  The ADF step is computed
#' natively and reproduces \code{urca::ur.df(type = "none", lags =
#' max_lag, selectlags = "AIC")} exactly; no external package is used.
#' The statistic is compared with the Engle-Granger (two-variable,
#' constant) critical values \code{-3.90 / -3.34 / -3.04}, not the plain
#' Dickey-Fuller ones, and \code{p_value} is a coarse bracket from them.
#'
#' @param y1 Numeric, first series.
#' @param y2 Numeric, second series.
#' @param max_lag Max ADF augmentation lags. Default \code{floor(12*(n/100)^0.25)}.
#' @return Named list with \code{adf_statistic, p_value, beta,
#'   critical_values, n, method}.
#' @references Engle, R. F. and Granger, C. W. J. (1987). Co-integration
#'   and error correction. Econometrica 55, 251-276.
#' @examples
#' set.seed(1)
#' morie_eg_coint(y1 = rnorm(100), y2 = rnorm(100))
#' @export
morie_eg_coint <- function(y1, y2, max_lag = NULL) {
  y1 <- as.numeric(y1)
  y2 <- as.numeric(y2)
  if (length(y1) != length(y2)) stop("Length mismatch.")
  n <- length(y1)
  if (n < 20) stop("Need >=20 obs.")
  if (is.null(max_lag)) max_lag <- floor(12 * (n / 100)^0.25)
  fit_ls <- lm(y1 ~ y2)
  beta <- coef(fit_ls)
  resid <- residuals(fit_ls)
  stat <- .morie_urdf(resid, type = "none", lags = max_lag,
                      selectlags = "AIC")$statistic
  crit <- c(`1%` = -3.90, `5%` = -3.34, `10%` = -3.04)
  approx_p <- if (stat < crit["1%"]) {
    0.005
  } else if (stat < crit["5%"]) {
    0.03
  } else if (stat < crit["10%"]) {
    0.07
  } else {
    min(1, 2 * pnorm(stat))
  }
  list(
    adf_statistic = as.numeric(stat),
    p_value = as.numeric(approx_p),
    beta = unname(beta),
    critical_values = crit,
    n = n,
    method = "Engle-Granger 2-step cointegration (Engle & Granger 1987)"
  )
}
