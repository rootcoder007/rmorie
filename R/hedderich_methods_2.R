# SPDX-License-Identifier: AGPL-3.0-or-later
#' Likelihood-ratio (G) test for frequencies
#'
#' G = 2 sum n_i log(n_i / e_i) (Hedderich, Sachs & Reynarowych 2023, eq 7.48),
#' chi-square on k - 1 - m df for k counts against expected counts or
#' probabilities with m estimated parameters, or on (r - 1)(c - 1) df for an r x
#' c table against independence (DescTools::GTest). Zero cells contribute
#' zero. The book's Hardy-Weinberg example prints 1.92; its counts give 1.19.
#'
#' @param observed Counts (vector) or an r x c matrix.
#' @param expected Optional expected counts or probabilities.
#' @param n_estimated Parameters estimated from the data (vector case).
#' @return Named list: statistic, df, p_value, expected.
#' @references Agresti, A. (2013). Categorical Data Analysis, Sec. 3.2.
#' @examples
#' lrgtst(c(18, 55, 27), c(0.207, 0.496, 0.297), n_estimated = 1)$statistic
#' @export
lrgtst <- function(observed, expected = NULL, n_estimated = 0) {
  if (is.matrix(observed)) {
    o <- observed
    e <- outer(rowSums(o), colSums(o)) / sum(o)
    df <- (nrow(o) - 1) * (ncol(o) - 1)
  } else {
    o <- as.numeric(observed)
    e <- if (is.null(expected)) rep(sum(o) / length(o), length(o)) else expected * sum(o) / sum(expected)
    df <- length(o) - 1 - n_estimated
  }
  if (df < 1) stop("no degrees of freedom left", call. = FALSE)
  keep <- o > 0
  g <- 2 * sum(o[keep] * log(o[keep] / e[keep]))
  list(statistic = g, df = df, p_value = stats::pchisq(g, df, lower.tail = FALSE), expected = e)
}

#' Cochran-Armitage test for a linear trend in proportions
#'
#' Splits the k x 2 chi-square sum n_i (p_i - p)^2 / (p (1 - p)) into the trend
#' part b^2 sum n_i (x_i - xbar)^2 / (p (1 - p)) on 1 df (prop.trend.test) and
#' the deviation on k - 2 df (Hedderich, Sachs & Reynarowych 2023, eqs
#' 7.340-7.343), and gives the unpooled z = sum x_i p_i / sqrt(sum x_i^2 p_i
#' (1 - p_i) / n_i) of eq 7.344.
#'
#' @param successes,totals Counts per group.
#' @param scores Group scores, default 1..k.
#' @return Named list: chi2_trend, p_trend, chi2_deviation, df_deviation,
#'   p_deviation, chi2_total, slope, z_unpooled, p_z_unpooled.
#' @references Cochran, W. G. (1954). Biometrics 10, 417-451. Armitage, P.
#'   (1955). Biometrics 11, 375-386.
#' @examples
#' catrnd(c(22, 16, 2), c(36, 34, 10), c(1, 0, -1))$chi2_trend
#' @export
catrnd <- function(successes, totals, scores = NULL) {
  y <- as.numeric(successes)
  n <- as.numeric(totals)
  k <- length(y)
  if (k < 2L || length(n) != k || any(y < 0 | y > n)) {
    stop("need k >= 2 groups with 0 <= successes <= totals", call. = FALSE)
  }
  x <- if (is.null(scores)) seq_len(k) else as.numeric(scores)
  N <- sum(n)
  p <- sum(y) / N
  pi <- y / n
  xb <- sum(n * x) / N
  sxx <- sum(n * (x - xb)^2)
  b <- sum(n * (pi - p) * (x - xb)) / sxx
  total <- sum(n * (pi - p)^2) / (p * (1 - p))
  trend <- b^2 * sxx / (p * (1 - p))
  dev <- total - trend
  zd <- sum(x^2 * pi * (1 - pi) / n)
  zu <- if (zd > 0) sum(x * pi) / sqrt(zd) else NaN
  list(chi2_trend = trend, p_trend = stats::pchisq(trend, 1, lower.tail = FALSE), chi2_deviation = dev,
       df_deviation = k - 2, p_deviation = if (k > 2) stats::pchisq(dev, k - 2, lower.tail = FALSE) else NaN,
       chi2_total = total, slope = b, z_unpooled = zu,
       p_z_unpooled = 2 * stats::pnorm(abs(zu), lower.tail = FALSE))
}

#' Sample size for testing Kendall's tau (Noether)
#'
#' n = (z_(1 - alpha/2) + z_(1 - beta))^2 / (9 (pi_c - 1/2)^2), pi_c = (1 + tau) / 2
#' (Noether 1987; Hedderich, Sachs & Reynarowych 2023, eq 7.415).
#'
#' @param tau Kendall's tau to detect.
#' @param alpha Two-sided level.
#' @param power Desired power.
#' @return Named list: n, n_exact, tau, alpha, power.
#' @references Noether, G. E. (1987). JASA 82, 645-647.
#' @examples
#' ntrtau(0.3)$n
#' @export
ntrtau <- function(tau, alpha = 0.05, power = 0.8) {
  if (tau == 0 || abs(tau) >= 1) stop("`tau` must be non-zero and in (-1, 1)", call. = FALSE)
  ne <- (stats::qnorm(1 - alpha / 2) + stats::qnorm(power))^2 / (9 * ((1 + tau) / 2 - 0.5)^2)
  list(n = ceiling(ne), n_exact = ne, tau = tau, alpha = alpha, power = power)
}

#' Fitting the gamma distribution
#'
#' Method of moments k = n xbar^2 / sum (x - xbar)^2, lambda = n xbar / sum (x -
#' xbar)^2 (Hedderich, Sachs & Reynarowych 2023, eq 5.154), or maximum
#' likelihood: log k - digamma(k) = log xbar - mean(log x) by bisection and
#' lambda = k / xbar (MASS::fitdistr).
#'
#' @param x Positive observations.
#' @param method "mle" or "moments".
#' @return Named list: shape, rate, scale, method.
#' @references Choi, S. C. & Wette, R. (1969). Technometrics 11, 683-690.
#' @examples
#' gamfit(c(2.1, 0.7, 3.3, 1.2, 5.4, 0.9))$shape
#' @export
gamfit <- function(x, method = "mle") {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L || any(!(x > 0))) stop("need at least two positive observations", call. = FALSE)
  m <- mean(x)
  if (method == "moments") {
    ss <- sum((x - m)^2)
    k <- n * m^2 / ss
    lam <- n * m / ss
  } else if (method == "mle") {
    s <- log(m) - mean(log(x))
    if (!(s > 0)) stop("all observations equal; the shape is unbounded", call. = FALSE)
    f <- function(a) log(a) - digamma(a) - s
    lo <- 1e-10
    hi <- 1
    while (f(hi) > 0) hi <- hi * 2
    for (i in 1:400) {
      mid <- (lo + hi) / 2
      if (f(mid) > 0) lo <- mid else hi <- mid
      if (hi - lo <= 1e-15 * hi) break
    }
    k <- (lo + hi) / 2
    lam <- k / m
  } else {
    stop("`method` must be 'mle' or 'moments'", call. = FALSE)
  }
  list(shape = k, rate = lam, scale = 1 / lam, method = method)
}

#' Pearson chi-square test of normality
#'
#' Classes equiprobable under N(xbar, s^2), class floor(1 + k pnorm(x, xbar,
#' s)), default k = ceiling(2 n^(2/5)); P = sum (C - n/k)^2 / (n/k) on k - 3 df
#' (adjust) or k - 1 (nortest::pearson.test).
#'
#' @param x Numeric sample.
#' @param n_classes Number of classes.
#' @param adjust Subtract the two estimated parameters from the df.
#' @return Named list: statistic, p_value, n_classes, df.
#' @references Moore, D. S. (1986). Tests of chi-squared type. In
#'   D'Agostino & Stephens (eds), Goodness-of-Fit Techniques.
#' @examples
#' pchnrm(qnorm(ppoints(30)))$statistic
#' @export
pchnrm <- function(x, n_classes = NULL, adjust = TRUE) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 5L) stop("need at least 5 observations", call. = FALSE)
  k <- if (is.null(n_classes)) ceiling(2 * n^0.4) else as.integer(n_classes)
  cls <- pmin(pmax(floor(1 + k * stats::pnorm(x, mean(x), stats::sd(x))), 1), k)
  cnt <- tabulate(cls, k)
  e <- n / k
  stat <- sum((cnt - e)^2 / e)
  df <- k - (if (adjust) 3 else 1)
  if (df < 1) stop("too few classes for the degrees of freedom", call. = FALSE)
  list(statistic = stat, p_value = stats::pchisq(stat, df, lower.tail = FALSE), n_classes = k, df = df)
}
