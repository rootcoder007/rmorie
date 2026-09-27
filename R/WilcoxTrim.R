# SPDX-License-Identifier: AGPL-3.0-or-later
.wil_tmean <- function(x, tr) {
  s <- sort(as.numeric(x))
  g <- floor(tr * length(s))
  mean(s[(g + 1):(length(s) - g)])
}

.wil_winvar <- function(x, tr) {
  s <- sort(as.numeric(x))
  n <- length(s)
  g <- floor(tr * n)
  w <- pmin(pmax(s, s[g + 1]), s[n - g])
  sum((w - mean(w))^2) / (n - 1)
}

#' Standard error of the trimmed mean
#'
#' s_w / ((1 - 2 tr) sqrt(n)) with s_w^2 the Winsorized variance.
#'
#' @param x Numeric vector.
#' @param tr Trimming proportion.
#' @return list(se, trimmed_mean, winsorized_variance, n).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eqs (4.9)-(4.10).
#' @examples
#' TrimSE(c(2.1, 3.4, 1.9, 5.6, 4.4, 3.3, 2.8, 6.1, 3.9, 4.2, 2.2, 5.0, 40))$se
#' @export
TrimSE <- function(x, tr = 0.2) {
  n <- length(x)
  if (n < 2 || tr < 0 || tr >= 0.5) stop("need n >= 2 and 0 <= tr < 0.5", call. = FALSE)
  wv <- .wil_winvar(x, tr)
  list(se = sqrt(wv) / ((1 - 2 * tr) * sqrt(n)), trimmed_mean = .wil_tmean(x, tr), winsorized_variance = wv, n = n)
}

#' Tukey-McLaughlin inference for a trimmed mean
#'
#' T = (trimmed mean - null) / trimmed-mean standard error on n - 2g - 1 df.
#'
#' @param x Numeric vector.
#' @param tr Trimming proportion.
#' @param alpha Level.
#' @param null_value Hypothesised trimmed mean.
#' @return list(estimate, se, statistic, df, ci, p_value).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (4.11).
#' @examples
#' TrimCI(c(2.1, 3.4, 1.9, 5.6, 4.4, 3.3, 2.8, 6.1, 3.9, 4.2, 2.2, 5.0, 40), null_value = 3)$statistic
#' @export
TrimCI <- function(x, tr = 0.2, alpha = 0.05, null_value = 0) {
  n <- length(x)
  df <- n - 2 * floor(tr * n) - 1
  if (df < 1 || tr < 0 || tr >= 0.5) stop("too few observations left after trimming", call. = FALSE)
  est <- .wil_tmean(x, tr)
  se <- sqrt(.wil_winvar(x, tr)) / ((1 - 2 * tr) * sqrt(n))
  t <- (est - null_value) / se
  crit <- qt(1 - alpha / 2, df)
  list(estimate = est, se = se, statistic = t, df = df, ci = est + c(-1, 1) * crit * se, p_value = 2 * pt(-abs(t), df))
}

#' Algina-Keselman-Penfield robust effect size (one group)
#'
#' d = k (trimmed mean - null) / s_w with k^2 the Winsorized variance of N(0, 1).
#'
#' @param x Numeric vector.
#' @param y Optional paired vector (differences x - y are used).
#' @param tr Trimming proportion.
#' @param null_value Null value.
#' @return list(d, k).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (8.6).
#' @examples
#' AkpD(c(2.1, 3.4, 1.9, 5.6, 4.4))$k
#' @export
AkpD <- function(x, y = NULL, tr = 0.2, null_value = 0) {
  if (!is.null(y)) {
    if (length(y) != length(x)) stop("x and y must be paired", call. = FALSE)
    x <- x - y
  }
  if (length(x) < 2 || tr < 0 || tr >= 0.5) stop("need n >= 2 and 0 <= tr < 0.5", call. = FALSE)
  k <- 1
  if (tr > 0) {
    z <- qnorm(1 - tr)
    k <- sqrt((1 - 2 * tr) - 2 * z * dnorm(z) + 2 * tr * z^2)
  }
  list(d = k * (.wil_tmean(x, tr) - null_value) / sqrt(.wil_winvar(x, tr)), k = k)
}

#' Yuen's test for two trimmed means
#'
#' d_j = (n_j - 1) s_wj^2 / (h_j (h_j - 1)); T = difference / sqrt(d_1 + d_2)
#' with Welch-type degrees of freedom.
#'
#' @param x,y Numeric vectors.
#' @param tr Trimming proportion.
#' @param alpha Level.
#' @return list(statistic, df, diff, ci, p_value, se).
#' @references Yuen, K. K. (1974). Biometrika 61, 165-170. Wilcox (2017), eq (8.12).
#' @examples
#' Yuen(1:10, 3:12)$df
#' @export
Yuen <- function(x, y, tr = 0.2, alpha = 0.05) {
  dh <- function(v) {
    h <- length(v) - 2 * floor(tr * length(v))
    c((length(v) - 1) * .wil_winvar(v, tr) / (h * (h - 1)), h)
  }
  a <- dh(x)
  b <- dh(y)
  if (a[2] < 2 || b[2] < 2) stop("too few observations left after trimming", call. = FALSE)
  diff <- .wil_tmean(x, tr) - .wil_tmean(y, tr)
  se <- sqrt(a[1] + b[1])
  df <- (a[1] + b[1])^2 / (a[1]^2 / (a[2] - 1) + b[1]^2 / (b[2] - 1))
  t <- diff / se
  list(statistic = t, df = df, diff = diff, ci = diff + c(-1, 1) * qt(1 - alpha / 2, df) * se,
       p_value = 2 * pt(-abs(t), df), se = se)
}

#' Error-bar overlap rule
#'
#' Rejects when |mean difference| / (se_1 + se_2) >= crit, i.e. when the error
#' bars do not overlap; the sum of standard errors overstates the standard error
#' of the difference.
#'
#' @param x,y Numeric vectors.
#' @param crit Error-bar multiplier.
#' @return list(ratio, reject, welch_ratio, overlap).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (7.36).
#' @examples
#' CiOvlap(1:3, 4:6)$reject
#' @export
CiOvlap <- function(x, y, crit = 2) {
  if (length(x) < 2 || length(y) < 2) stop("need at least 2 observations per group", call. = FALSE)
  e1 <- sd(x) / sqrt(length(x))
  e2 <- sd(y) / sqrt(length(y))
  m1 <- mean(x)
  m2 <- mean(y)
  ratio <- abs(m1 - m2) / (e1 + e2)
  list(ratio = ratio, reject = ratio >= crit, welch_ratio = abs(m1 - m2) / sqrt(e1^2 + e2^2),
       overlap = !(m1 + crit * e1 < m2 - crit * e2 || m1 - crit * e1 > m2 + crit * e2))
}

#' F test that all slopes are zero, from R-squared
#'
#' F = ((n - p - 1)/p) R^2 / (1 - R^2) on (p, n - p - 1) df.
#'
#' @param r2 R-squared.
#' @param n Sample size.
#' @param p Number of predictors.
#' @return list(statistic, df1, df2, p_value).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (6.13).
#' @examples
#' R2FTest(0.5, 23, 2)$statistic
#' @export
R2FTest <- function(r2, n, p) {
  if (r2 < 0 || r2 >= 1 || p < 1 || n - p - 1 < 1) stop("need 0 <= R^2 < 1, p >= 1 and n > p + 1", call. = FALSE)
  f <- (n - p - 1) / p * r2 / (1 - r2)
  list(statistic = f, df1 = p, df2 = n - p - 1, p_value = pf(f, p, n - p - 1, lower.tail = FALSE))
}

#' Explanatory power
#'
#' eta^2 = tau^2(fitted) / tau^2(Y) with the variance or the Winsorized variance.
#'
#' @param y,yhat Outcomes and fitted values.
#' @param measure "variance" or "winsorized".
#' @param tr Winsorizing proportion.
#' @return list(eta2, eta).
#' @references Wilcox, R. R. (2017). Modern Statistics for the Social and
#'   Behavioral Sciences, 2nd ed. Eq (14.10).
#' @examples
#' ExplPow(1:4, 1:4)$eta2
#' @export
ExplPow <- function(y, yhat, measure = c("variance", "winsorized"), tr = 0.2) {
  measure <- match.arg(measure)
  if (length(y) != length(yhat) || length(y) < 2) stop("y and yhat must have equal length >= 2", call. = FALSE)
  tau2 <- if (measure == "variance") stats::var else function(v) .wil_winvar(v, tr)
  e2 <- tau2(yhat) / tau2(y)
  list(eta2 = e2, eta = sqrt(e2))
}
