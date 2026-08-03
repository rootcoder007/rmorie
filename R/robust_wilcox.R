# Robust estimation and hypothesis testing (Wilcox 2017).
#
# R mirror of morie/src/morie/fn/_robust_core.py, so the Python and R
# arms of the package compute the same thing.  Both were ported from
# Wilcox's own reference implementation, WRS Rallfun-v45.R
# (github.com/nicebread/WRS); the parity tests in
# tests/testthat/test-robust-wilcox.R check the two against each other
# and against the values that R itself produces.
#
# These call base R (median, sort, var, pt, qt, pf, rank) rather than
# re-deriving the distribution functions, which the Python side must do
# because it carries no numeric dependency.

#' @noRd
morie_robust_trim_counts <- function(n, tr) {
  if (tr < 0 || tr >= 0.5) stop("tr must satisfy 0 <= tr < 0.5")
  floor(tr * n)
}

#' Ideal fourths and the interquartile range
#'
#' The lower and upper ideal fourths of Wilcox (2017) eq. (2.6)-(2.7),
#' and the interquartile range they define, eq. (2.8).  These are the
#' quartile estimates the book uses for outlier detection.
#' @param x numeric vector
#' @return `morie_ideal_fourths` a list with `q1`, `q2`, `j`, `h`, `k`;
#'   `morie_idealf_iqr` the single number `q2 - q1`
#' @export
morie_ideal_fourths <- function(x) {
  v <- sort(x[!is.na(x)])
  n <- length(v)
  if (n < 4) stop("ideal fourths need at least 4 observations")
  j <- floor(n / 4 + 5 / 12)
  h <- n / 4 + 5 / 12 - j
  k <- n - j + 1
  list(q1 = (1 - h) * v[j] + h * v[j + 1],
       q2 = (1 - h) * v[k] + h * v[k - 1], j = j, h = h, k = k)
}

#' @rdname morie_ideal_fourths
#' @export
morie_idealf_iqr <- function(x) {
  f <- morie_ideal_fourths(x)
  f$q2 - f$q1
}

#' Trimming and Winsorizing
#'
#' The trimmed mean removes `g = floor(tr * n)` values from each tail;
#' Winsorizing pulls them in to the nearest retained value instead, and
#' the Winsorized variance is the ordinary sample variance of the
#' Winsorized values (Wilcox sec. 2.4.5).
#' @param x numeric vector
#' @param tr amount of trimming or Winsorizing, `0 <= tr < 0.5`
#' @return a numeric scalar, except `morie_winsorize` which returns the
#'   Winsorized sample in the ORIGINAL order
#' @export
morie_trimmed_mean <- function(x, tr = 0.2) {
  v <- sort(x[!is.na(x)])
  n <- length(v)
  g <- morie_robust_trim_counts(n, tr)
  if (g > 0) v <- v[(g + 1):(n - g)]
  if (!length(v)) stop("trimming removed every observation")
  mean(v)
}

#' @rdname morie_trimmed_mean
#' @export
morie_winsorize <- function(x, tr = 0.2) {
  v <- x[!is.na(x)]
  y <- sort(v)
  n <- length(v)
  g <- morie_robust_trim_counts(n, tr)
  if (g == 0) return(v)
  lo <- y[g + 1]
  hi <- y[n - g]
  pmin(pmax(v, lo), hi)
}

#' @rdname morie_trimmed_mean
#' @export
morie_winsorized_mean <- function(x, tr = 0.2) mean(morie_winsorize(x, tr))

#' @rdname morie_trimmed_mean
#' @export
morie_winsorized_variance <- function(x, tr = 0.2) {
  stats::var(morie_winsorize(x, tr))
}

#' Median absolute deviation and the MAD-median outlier rule
#'
#' `morie_madn` is the book's MADN, MAD / 0.6745.  `morie_mad_rescaled`
#' uses R's `mad()` constant 1.4826, which is what WRS relies on -- the
#' two differ in the fifth significant figure and the estimators ported
#' from WRS use the R one.  `morie_mad_median_rule` is the Hampel
#' identifier of eq. (2.14): declare X an outlier when
#' `|X - M| / MADN > 2.24`.
#' @param x numeric vector
#' @param constant MAD scaling constant
#' @param crit cut-off for the outlier rule
#' @return `morie_mad`, `morie_madn`, `morie_mad_rescaled` numbers;
#'   `morie_mad_median_rule` a list with `median`, `madn`, `ratio`,
#'   `is_outlier`, `outliers`, `n_outliers`
#' @export
morie_mad <- function(x) {
  v <- x[!is.na(x)]
  stats::median(abs(v - stats::median(v)))
}

#' @rdname morie_mad
#' @export
morie_madn <- function(x) morie_mad(x) / 0.6745

#' @rdname morie_mad
#' @export
morie_mad_rescaled <- function(x, constant = 1.4826) morie_mad(x) * constant

#' @rdname morie_mad
#' @export
morie_mad_median_rule <- function(x, crit = 2.24) {
  v <- x[!is.na(x)]
  m <- stats::median(v)
  s <- morie_madn(v)
  ratio <- if (s == 0) ifelse(v == m, 0, Inf) else abs(v - m) / s
  flag <- ratio > crit
  list(median = m, madn = s, ratio = ratio, is_outlier = flag,
       outliers = v[flag], n_outliers = sum(flag), crit = crit)
}

#' Boxplot outlier rules on the ideal fourths
#'
#' Unlike `boxplot()`, the quartiles come from the ideal fourths.  With
#' `carling = TRUE` the fence is Carling's (2000) modification, centred
#' on the MEDIAN with a sample-size dependent multiplier
#' `(17.63 n - 23.64) / (7.74 n - 3.71)`.
#' @param x numeric vector
#' @param carling use Carling's modification
#' @param gval override the fence multiplier
#' @return list with `lower`, `upper`, `gval`, `iqr`, `is_outlier`,
#'   `outliers`, `keep`, `n_outliers`
#' @export
morie_boxplot_outliers <- function(x, carling = FALSE, gval = NULL) {
  v <- x[!is.na(x)]
  n <- length(v)
  f <- morie_ideal_fourths(v)
  iqr <- f$q2 - f$q1
  if (carling) {
    g <- if (is.null(gval)) (17.63 * n - 23.64) / (7.74 * n - 3.71) else gval
    m <- stats::median(v)
    cl <- m - g * iqr
    cu <- m + g * iqr
  } else {
    g <- if (is.null(gval)) 1.5 else gval
    cl <- f$q1 - g * iqr
    cu <- f$q2 + g * iqr
  }
  flag <- v < cl | v > cu
  list(lower = cl, upper = cu, gval = g, iqr = iqr, is_outlier = flag,
       outliers = v[flag], keep = v[!flag], n = n, n_outliers = sum(flag))
}

#' Welch's and Yuen's tests for two groups
#'
#' `morie_welch_test` compares means without assuming equal variances;
#' `morie_yuen_test` compares trimmed means and reduces exactly to
#' Welch's method when `tr = 0`; `morie_yuen_paired` is the dependent
#' groups version, whose standard error subtracts the Winsorized
#' covariance.
#' @param x,y numeric vectors
#' @param tr amount of trimming
#' @param alpha significance level
#' @return list with the estimate, `statistic`, `df`, `se` and `p_value`
#' @export
morie_welch_test <- function(x, y) {
  a <- x[!is.na(x)]; b <- y[!is.na(y)]
  n1 <- length(a); n2 <- length(b)
  q1 <- stats::var(a) / n1
  q2 <- stats::var(b) / n2
  se <- sqrt(q1 + q2)
  tstat <- (mean(a) - mean(b)) / se
  df <- (q1 + q2)^2 / (q1^2 / (n1 - 1) + q2^2 / (n2 - 1))
  list(estimate = mean(a) - mean(b), statistic = tstat, df = df, se = se,
       p_value = 2 * (1 - stats::pt(abs(tstat), df)))
}

#' @rdname morie_welch_test
#' @export
morie_yuen_test <- function(x, y, tr = 0.2) {
  a <- x[!is.na(x)]; b <- y[!is.na(y)]
  n1 <- length(a); n2 <- length(b)
  h1 <- n1 - 2 * morie_robust_trim_counts(n1, tr)
  h2 <- n2 - 2 * morie_robust_trim_counts(n2, tr)
  if (h1 < 2 || h2 < 2) stop("too much trimming")
  d1 <- (n1 - 1) * morie_winsorized_variance(a, tr) / (h1 * (h1 - 1))
  d2 <- (n2 - 1) * morie_winsorized_variance(b, tr) / (h2 * (h2 - 1))
  t1 <- morie_trimmed_mean(a, tr); t2 <- morie_trimmed_mean(b, tr)
  se <- sqrt(d1 + d2)
  tstat <- (t1 - t2) / se
  df <- (d1 + d2)^2 / (d1^2 / (h1 - 1) + d2^2 / (h2 - 1))
  list(estimate = t1 - t2, statistic = tstat, df = df, se = se,
       p_value = 2 * (1 - stats::pt(abs(tstat), df)))
}

#' @rdname morie_welch_test
#' @export
morie_yuen_paired <- function(x, y, tr = 0.2, alpha = 0.05) {
  if (length(x) != length(y)) stop("dependent groups must have equal length")
  n <- length(x)
  h1 <- n - 2 * morie_robust_trim_counts(n, tr)
  q1 <- (n - 1) * morie_winsorized_variance(x, tr)
  q2 <- (n - 1) * morie_winsorized_variance(y, tr)
  q3 <- (n - 1) * morie_winsorized_correlation(x, y, tr)$cov
  df <- h1 - 1
  v <- (q1 + q2 - 2 * q3) / (h1 * (h1 - 1))
  dif <- morie_trimmed_mean(x, tr) - morie_trimmed_mean(y, tr)
  if (v <= 0) {
    return(list(estimate = dif, ci = c(dif, dif), statistic = NaN,
                se = 0, df = df, p_value = NaN, degenerate = TRUE))
  }
  se <- sqrt(v)
  tstat <- dif / se
  crit <- stats::qt(1 - alpha / 2, df)
  list(estimate = dif, ci = c(dif - crit * se, dif + crit * se),
       statistic = tstat, se = se, df = df,
       p_value = 2 * (1 - stats::pt(abs(tstat), df)), degenerate = FALSE)
}

#' Inference for a single trimmed mean
#'
#' The Tukey-McLaughlin standard error,
#' `sqrt(winvar) / ((1 - 2 tr) sqrt(n))`, and the confidence interval it
#' gives with `df = n - 2 floor(tr n) - 1`.
#' @param x numeric vector
#' @param tr amount of trimming
#' @param alpha significance level
#' @param null_value value tested against
#' @return `morie_trimmed_mean_se` a number; `morie_trimmed_mean_ci` a
#'   list with `estimate`, `ci`, `statistic`, `se`, `df`, `p_value`
#' @export
morie_trimmed_mean_se <- function(x, tr = 0.2) {
  v <- x[!is.na(x)]
  sqrt(morie_winsorized_variance(v, tr)) / ((1 - 2 * tr) * sqrt(length(v)))
}

#' @rdname morie_trimmed_mean_se
#' @export
morie_trimmed_mean_ci <- function(x, tr = 0.2, alpha = 0.05,
                                  null_value = 0) {
  v <- x[!is.na(x)]
  n <- length(v)
  se <- morie_trimmed_mean_se(v, tr)
  df <- n - 2 * morie_robust_trim_counts(n, tr) - 1
  est <- morie_trimmed_mean(v, tr)
  crit <- stats::qt(1 - alpha / 2, df)
  tstat <- (est - null_value) / se
  list(estimate = est, ci = c(est - crit * se, est + crit * se),
       statistic = tstat, se = se, df = df, n = n,
       p_value = 2 * (1 - stats::pt(abs(tstat), df)))
}

#' Robust measures of location
#'
#' `morie_harrell_davis` is the Harrell-Davis quantile estimator, a
#' Beta-weighted average of all the order statistics.  `morie_pbos` is
#' the one-step percentage bend location.  `morie_mom_estimator` drops
#' values more than `bend` MADNs from the median and averages the rest.
#' `morie_one_step_m` is the one-step M-estimator with Huber's Psi.
#' @param x numeric vector
#' @param q quantile to estimate
#' @param beta bending constant for the percentage bend
#' @param bend bending constant
#' @param constant MAD scaling constant; the R value 1.4826 by default,
#'   as WRS uses
#' @return a numeric scalar
#' @export
morie_harrell_davis <- function(x, q = 0.5) {
  v <- sort(x[!is.na(x)])
  n <- length(v)
  m1 <- (n + 1) * q
  m2 <- (n + 1) * (1 - q)
  i <- seq_len(n)
  w <- stats::pbeta(i / n, m1, m2) - stats::pbeta((i - 1) / n, m1, m2)
  sum(w * v)
}

#' @rdname morie_harrell_davis
#' @export
morie_pbos <- function(x, beta = 0.2) {
  v <- x[!is.na(x)]
  n <- length(v)
  m <- stats::median(v)
  omega <- sort(abs(v - m))[floor((1 - beta) * n)]
  if (omega == 0) return(m)
  psi <- (v - m) / omega
  i1 <- sum(psi < -1)
  i2 <- sum(psi > 1)
  keep <- v
  keep[psi < -1 | psi > 1] <- 0
  (sum(keep) + omega * (i2 - i1)) / (n - i1 - i2)
}

#' @rdname morie_harrell_davis
#' @export
morie_mom_estimator <- function(x, bend = 2.24, constant = 1.4826) {
  v <- x[!is.na(x)]
  m <- stats::median(v)
  s <- morie_mad_rescaled(v, constant)
  if (s == 0) return(m)
  mean(v[v >= m - bend * s & v <= m + bend * s])
}

#' @rdname morie_harrell_davis
#' @export
morie_one_step_m <- function(x, bend = 1.28, constant = 1.4826) {
  v <- x[!is.na(x)]
  m <- stats::median(v)
  s <- morie_mad_rescaled(v, constant)
  if (s == 0) return(m)
  y <- (v - m) / s
  psi <- ifelse(abs(y) <= bend, y, bend * sign(y))
  b <- sum(abs(y) <= bend)
  if (b == 0) return(m)
  m + s * sum(psi) / b
}

#' Robust correlations
#'
#' The percentage bend correlation clips standardised deviations to
#' `[-1, 1]`; the Winsorized correlation is Pearson's on the Winsorized
#' values, tested with `n - 2g - 2` degrees of freedom.
#' @param x,y numeric vectors
#' @param beta bending constant
#' @param tr amount of Winsorizing
#' @return list with `cor`, `statistic`, `p_value` and, for the
#'   Winsorized version, `cov` and `df`
#' @export
morie_percentage_bend_correlation <- function(x, y, beta = 0.2) {
  n <- length(x)
  scaled <- function(v) {
    m <- stats::median(v)
    omega <- sort(abs(v - m))[floor((1 - beta) * length(v))]
    if (omega == 0) stop("omega is zero; the data are too tied")
    s <- (v - morie_pbos(v, beta)) / omega
    pmin(pmax(s, -1), 1)
  }
  a <- scaled(x); b <- scaled(y)
  r <- sum(a * b) / sqrt(sum(a^2) * sum(b^2))
  tstat <- r * sqrt((n - 2) / (1 - r^2))
  list(cor = r, statistic = tstat, n = n,
       p_value = 2 * (1 - stats::pt(abs(tstat), n - 2)))
}

#' @rdname morie_percentage_bend_correlation
#' @export
morie_winsorized_correlation <- function(x, y, tr = 0.2) {
  n <- length(x)
  g <- morie_robust_trim_counts(n, tr)
  a <- morie_winsorize(x, tr)
  b <- morie_winsorize(y, tr)
  r <- stats::cor(a, b)
  df <- n - 2 * g - 2
  tstat <- r * sqrt((n - 2) / (1 - r^2))
  list(cor = r, cov = stats::var(a, b), statistic = tstat, df = df, n = n,
       p_value = 2 * (1 - stats::pt(abs(tstat), df)))
}

#' Rank-based comparisons of two groups
#'
#' `morie_cliff_delta` estimates `P(X>Y) - P(X<Y)` with the asymmetric
#' interval of Cliff (1996, p.140, eq. 5.12).  `morie_brunner_munzel` is
#' the heteroscedastic analogue of Wilcoxon-Mann-Whitney: it does not
#' assume the two distributions share a shape.
#' @param x,y numeric vectors
#' @param alpha significance level
#' @return list with the effect estimate, interval and p-value
#' @export
morie_cliff_delta <- function(x, y, alpha = 0.05) {
  a <- x[!is.na(x)]; b <- y[!is.na(y)]
  n1 <- length(a); n2 <- length(b)
  m <- sign(outer(a, b, "-"))
  d <- mean(m)
  phat <- (1 - d) / 2
  out <- list(delta = d, p_hat = phat, n1 = n1, n2 = n2,
              P_x_less_y = mean(m < 0), P_equal = mean(m == 0),
              P_x_greater_y = mean(m > 0))
  if (phat == 0 || phat == 1) {
    out$ci <- c(NA_real_, NA_real_)
    return(out)
  }
  sigdih <- sum((m - d)^2) / (n1 * n2 - 1)
  di <- vapply(a, function(v) mean(v > b) - mean(v < b), numeric(1))
  dh <- vapply(b, function(v) mean(v > a) - mean(v < a), numeric(1))
  sh <- ((n2 - 1) * stats::var(di) + (n1 - 1) * stats::var(dh) + sigdih) /
    (n1 * n2)
  zv <- stats::qnorm(alpha / 2)
  root <- sqrt(sh) * sqrt((1 - d^2)^2 + zv^2 * sh)
  den <- 1 - d^2 + zv^2 * sh
  out$ci <- c((d - d^3 + zv * root) / den, (d - d^3 - zv * root) / den)
  out
}

#' @rdname morie_cliff_delta
#' @export
morie_brunner_munzel <- function(x, y, alpha = 0.05) {
  a <- x[!is.na(x)]; b <- y[!is.na(y)]
  n1 <- length(a); n2 <- length(b)
  N <- n1 + n2
  R <- rank(c(a, b))
  R1 <- mean(R[1:n1]); R2 <- mean(R[(n1 + 1):N])
  Rg1 <- rank(a); Rg2 <- rank(b)
  s1 <- sum((R[1:n1] - Rg1 - R1 + (n1 + 1) / 2)^2) / (n1 - 1)
  s2 <- sum((R[(n1 + 1):N] - Rg2 - R2 + (n2 + 1) / 2)^2) / (n2 - 1)
  se <- sqrt(N) * sqrt(N * (s1 / n2^2 / n1 + s2 / n1^2 / n2))
  phat <- (R2 - (n2 + 1) / 2) / n1
  if (se == 0) {
    return(list(statistic = if (phat > 0.5) Inf else -Inf, df = NaN,
                p_value = NaN, p_hat = phat, delta = 1 - 2 * phat,
                se = 0, n1 = n1, n2 = n2, separated = TRUE))
  }
  stat <- (R2 - R1) / se
  df <- (s1 / n2 + s2 / n1)^2 /
    ((s1 / n2)^2 / (n1 - 1) + (s2 / n1)^2 / (n2 - 1))
  list(statistic = stat, df = df, p_hat = phat, delta = 1 - 2 * phat,
       se = se, n1 = n1, n2 = n2, separated = FALSE,
       p_value = 2 * (1 - stats::pt(abs(stat), df)))
}

#' Heteroscedastic one-way designs
#'
#' `morie_trimmed_mean_anova` is Wilcox's generalisation of Welch's test
#' to trimmed means.  `morie_brunner_dette_munk` is the fully
#' nonparametric rank-based alternative of Brunner, Dette and Munk
#' (1997), which assumes nothing about the shapes of the distributions.
#' @param groups list of numeric vectors, one per group
#' @param tr amount of trimming
#' @return list with `statistic`, `df1`, `df2` and `p_value`
#' @export
morie_trimmed_mean_anova <- function(groups, tr = 0.2) {
  J <- length(groups)
  if (J < 2) stop("need at least 2 groups")
  if (tr >= 0.5) stop("tr = 0.5 compares medians; use a median method")
  h <- w <- xbar <- numeric(J)
  for (j in seq_len(J)) {
    g <- groups[[j]][!is.na(groups[[j]])]
    n <- length(g)
    h[j] <- n - 2 * morie_robust_trim_counts(n, tr)
    wv <- morie_winsorized_variance(g, tr)
    if (wv == 0) stop("the Winsorized variance is zero for a group")
    w[j] <- h[j] * (h[j] - 1) / ((n - 1) * wv)
    xbar[j] <- morie_trimmed_mean(g, tr)
  }
  u <- sum(w)
  xtil <- sum(w * xbar) / u
  A <- sum(w * (xbar - xtil)^2) / (J - 1)
  tail <- sum((1 - w / u)^2 / (h - 1))
  B <- 2 * (J - 2) * tail / (J^2 - 1)
  test <- A / (B + 1)
  nu2 <- 1 / (3 * tail / (J^2 - 1))
  list(statistic = test, df1 = J - 1, df2 = nu2,
       trimmed_means = xbar,
       p_value = 1 - stats::pf(test, J - 1, nu2))
}

#' @rdname morie_trimmed_mean_anova
#' @export
morie_brunner_dette_munk <- function(groups) {
  J <- length(groups)
  if (J < 2) stop("need at least 2 groups")
  gs <- lapply(groups, function(g) g[!is.na(g)])
  nvec <- vapply(gs, length, integer(1))
  if (any(nvec < 2)) stop("every group needs at least 2 observations")
  pool <- unlist(gs)
  N <- length(pool)
  rval <- rank(pool)
  rvec <- list()
  pos <- 0
  rbar <- numeric(J)
  for (j in seq_len(J)) {
    rvec[[j]] <- rval[(pos + 1):(pos + nvec[j])]
    pos <- pos + nvec[j]
    rbar[j] <- mean(rvec[[j]])
  }
  phat <- (rbar - 0.5) / N
  svec <- vapply(seq_len(J), function(j)
    sum((rvec[[j]] - rbar[j])^2) / (nvec[j] - 1), numeric(1)) / N^2
  VN <- diag(N * svec / nvec, J, J)
  C <- diag(1, J, J) - matrix(1, J, J) / J
  trVN <- sum(diag(VN))
  c11 <- C[1, 1]
  F <- N * as.numeric(t(phat) %*% C %*% phat) / (c11 * trVN)
  nu1 <- c11^2 * trVN^2 / sum(diag(C %*% VN %*% C %*% VN))
  lam <- diag(1 / (nvec - 1), J, J)
  nu2 <- trVN^2 / sum(diag(VN %*% VN %*% lam))
  list(statistic = F, df1 = nu1, df2 = nu2, q_hat = phat, n = nvec,
       p_value = 1 - stats::pf(F, nu1, nu2))
}

#' Robust effect size, median error and Winsorized regression
#'
#' `morie_akp_effect_size` is the trimmed-mean analogue of Cohen's d of
#' Algina, Keselman and Penfield (2005).  `morie_median_se` is the
#' McKean-Shrader (1984) standard error of the median, which Wilcox
#' warns is unreliable with ties.  `morie_winsorized_regression` solves
#' the normal equations built from Winsorized covariances.
#' @param x,y numeric vectors
#' @param X predictor matrix or vector
#' @param tr amount of trimming or Winsorizing
#' @param equal_variance pool the Winsorized variances
#' @param n_iter maximum refinement iterations
#' @param tol convergence tolerance
#' @return a list; see each method's description
#' @export
morie_akp_effect_size <- function(x, y, tr = 0.2, equal_variance = TRUE) {
  a <- x[!is.na(x)]; b <- y[!is.na(y)]
  n1 <- length(a); n2 <- length(b)
  s1 <- morie_winsorized_variance(a, tr)
  s2 <- morie_winsorized_variance(b, tr)
  cterm <- 1
  if (tr > 0) {
    lo <- stats::qnorm(tr); hi <- stats::qnorm(1 - tr)
    f <- function(u) u^2 * stats::dnorm(u)
    cterm <- stats::integrate(f, lo, hi)$value + 2 * lo^2 * tr
  }
  cterm <- sqrt(cterm)
  dif <- morie_trimmed_mean(a, tr) - morie_trimmed_mean(b, tr)
  if (equal_variance) {
    sp <- sqrt(((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2))
    return(list(effect_size = cterm * dif / sp, cterm = cterm,
                pooled_sd = sp, n1 = n1, n2 = n2))
  }
  list(effect_size = c(cterm * dif / sqrt(s1), cterm * dif / sqrt(s2)),
       cterm = cterm, n1 = n1, n2 = n2)
}

#' @rdname morie_akp_effect_size
#' @export
morie_median_se <- function(x) {
  v <- sort(x[!is.na(x)])
  n <- length(v)
  z <- stats::qnorm(0.995)
  av <- round((n + 1) / 2 - z * sqrt(n / 4))
  if (av == 0) av <- 1
  top <- n - av + 1
  list(se = (v[top] - v[av]) / (2 * z), av = av, top = top, n = n,
       ties = anyDuplicated(v) > 0)
}

#' @rdname morie_akp_effect_size
#' @export
morie_winsorized_regression <- function(X, y, tr = 0.2, n_iter = 20,
                                        tol = 1e-4) {
  Xm <- as.matrix(X)
  p <- ncol(Xm)
  mvals <- apply(Xm, 2, function(c) morie_winsorized_mean(c, tr))
  M <- matrix(0, p, p)
  for (i in seq_len(p)) for (j in seq_len(p))
    M[i, j] <- morie_winsorized_correlation(Xm[, i], Xm[, j], tr)$cov
  ma <- vapply(seq_len(p), function(i)
    morie_winsorized_correlation(Xm[, i], y, tr)$cov, numeric(1))
  slope <- solve(M, ma)
  b0 <- morie_winsorized_mean(y, tr) - sum(slope * mvals)
  res <- as.numeric(y - Xm %*% slope - b0)
  converged <- FALSE
  for (it in seq_len(n_iter)) {
    ma <- vapply(seq_len(p), function(i)
      morie_winsorized_correlation(Xm[, i], res, tr)$cov, numeric(1))
    slope_add <- solve(M, ma)
    b0_add <- morie_winsorized_mean(res, tr) - sum(slope_add * mvals)
    if (max(abs(slope_add), abs(b0_add)) < tol) {
      converged <- TRUE
      break
    }
    slope <- slope + slope_add
    b0 <- b0 + b0_add
    res <- as.numeric(y - Xm %*% slope - b0)
  }
  list(coef = c(b0, slope), intercept = b0, slope = slope,
       residuals = res, converged = converged, n = nrow(Xm))
}
