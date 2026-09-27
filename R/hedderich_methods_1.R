# SPDX-License-Identifier: AGPL-3.0-or-later
#' Goodman and Kruskal's tau
#'
#' The proportional reduction in the error of predicting the column category
#' from the row (tau_col_given_row) and the reverse (Hedderich, Sachs &
#' Reynarowych 2023, eqs 3.9-3.10): tau_C|R = (n sum_ij n_ij^2 / n_i. - sum_j
#' n_.j^2) / (n^2 - sum_j n_.j^2).
#'
#' @param table An r x c matrix of counts.
#' @return Named list: tau_col_given_row, tau_row_given_col, n.
#' @references Goodman, L. A. & Kruskal, W. H. (1954). JASA 49, 732-764.
#' @examples
#' gktau(matrix(c(10, 30, 5, 0, 20, 30, 5, 0, 0), 3, byrow = TRUE))
#' @export
gktau <- function(table) {
  t <- as.matrix(table)
  if (nrow(t) < 2L || ncol(t) < 2L || any(t < 0)) {
    stop("`table` must be an r x c matrix of non-negative counts, r, c >= 2", call. = FALSE)
  }
  n <- sum(t)
  one <- function(m) {
    rs <- rowSums(m)
    cs <- colSums(m)
    keep <- rs > 0
    (n * sum(m[keep, , drop = FALSE]^2 / rs[keep]) - sum(cs^2)) / (n^2 - sum(cs^2))
  }
  list(tau_col_given_row = one(t), tau_row_given_col = one(t(t)), n = n)
}

#' Tolerance factor for normally distributed populations
#'
#' k with xbar + k s (one-sided, exact: the noncentral t quantile
#' qt(confidence, n - 1, z_coverage sqrt(n)) / sqrt(n)) or xbar -/+ k s
#' (two-sided, Howe's approximation sqrt((n - 1)(1 + 1/n) z^2 / chi2_(1 -
#' confidence, n - 1))) covering a proportion `coverage` with the given
#' confidence (Hedderich, Sachs & Reynarowych 2023, eqs 6.174-6.175).
#'
#' @param n Sample size, at least 2.
#' @param coverage Proportion of the population covered.
#' @param confidence Confidence level.
#' @param sided "one" or "two".
#' @return Named list: k, n, coverage, confidence, sided.
#' @references Howe, W. G. (1969). JASA 64, 610-620.
#' @examples
#' normtl(10, 0.95, 0.95, "two")$k
#' @export
normtl <- function(n, coverage = 0.95, confidence = 0.95, sided = "two") {
  n <- as.integer(n)
  if (n < 2L || coverage <= 0 || coverage >= 1 || confidence <= 0 || confidence >= 1) {
    stop("need n >= 2 and coverage, confidence in (0, 1)", call. = FALSE)
  }
  k <- switch(sided,
              one = stats::qt(confidence, n - 1, stats::qnorm(coverage) * sqrt(n)) / sqrt(n),
              two = sqrt((n - 1) * (1 + 1 / n) * stats::qnorm((1 - coverage) / 2)^2 /
                           stats::qchisq(1 - confidence, n - 1)),
              stop("`sided` must be 'one' or 'two'", call. = FALSE))
  list(k = k, n = n, coverage = coverage, confidence = confidence, sided = sided)
}

.corr_p <- function(stat, alternative, sf) {
  switch(alternative,
         "two-sided" = min(1, 2 * sf(abs(stat))),
         greater = sf(stat),
         less = sf(-stat),
         stop("`alternative` must be 'two-sided', 'greater' or 'less'", call. = FALSE))
}

#' Test of a Pearson correlation against zero or a hypothesised value
#'
#' rho0 = 0: t = r sqrt((n - 2) / (1 - r^2)) on n - 2 df (cor.test, eq 7.389);
#' "fisher": z = (atanh(r) - atanh(rho0)) sqrt(n - 3) (eq 7.399); "samiuddin":
#' t = (r - rho0) sqrt(n - 2) / sqrt((1 - r^2)(1 - rho0^2)) on n - 2 df (eq 7.393).
#'
#' @param r Sample correlation.
#' @param n Number of pairs.
#' @param rho0 Hypothesised correlation.
#' @param alternative "two-sided", "greater" or "less".
#' @param method "fisher" or "samiuddin".
#' @return Named list: statistic, p_value, distribution, df, method, r, n, rho0.
#' @references Samiuddin, M. (1970). Biometrika 57, 461-464. Hedderich, Sachs
#'   & Reynarowych (2023), eqs (7.389), (7.393), (7.399).
#' @examples
#' corrho(0.966, 14, 0.8)$statistic
#' @export
corrho <- function(r, n, rho0 = 0, alternative = "two-sided", method = "fisher") {
  n <- as.integer(n)
  if (abs(r) >= 1 || abs(rho0) >= 1 || n < 4L) stop("need -1 < r, rho0 < 1 and n >= 4", call. = FALSE)
  if (rho0 == 0) {
    stat <- r * sqrt((n - 2) / (1 - r^2))
    df <- n - 2
    p <- .corr_p(stat, alternative, function(v) stats::pt(v, df, lower.tail = FALSE))
    how <- "t test of rho = 0"
    dist <- "t"
  } else if (method == "fisher") {
    stat <- (atanh(r) - atanh(rho0)) * sqrt(n - 3)
    df <- NA
    p <- .corr_p(stat, alternative, function(v) stats::pnorm(v, lower.tail = FALSE))
    how <- "Fisher z test"
    dist <- "normal"
  } else if (method == "samiuddin") {
    stat <- (r - rho0) * sqrt(n - 2) / sqrt((1 - r^2) * (1 - rho0^2))
    df <- n - 2
    p <- .corr_p(stat, alternative, function(v) stats::pt(v, df, lower.tail = FALSE))
    how <- "Samiuddin t test"
    dist <- "t"
  } else {
    stop("`method` must be 'fisher' or 'samiuddin'", call. = FALSE)
  }
  list(statistic = stat, p_value = p, distribution = dist, df = df, method = how, r = r, n = n, rho0 = rho0)
}

#' Comparison and pooling of independent correlation coefficients
#'
#' Fisher-z pooled estimate with interval (eqs 7.401, 7.407-7.408), the
#' homogeneity chi-square sum (n_i - 3)(z_i - zbar)^2 on k - 1 df (7.409) or
#' against rho0 on k df (7.406), the two-sample z (7.400), and the weighted
#' r_gem = sum (n_i - 1) r_i / sum (n_i - 1) with its t on n - k - 1 df
#' (7.395-7.396).
#'
#' @param r Correlations (at least two).
#' @param n Their sample sizes.
#' @param rho0 Optional hypothesised common correlation.
#' @param confidence Interval level.
#' @return Named list: r_pooled, ci, z_pooled, se_pooled, chi2, df, p_value,
#'   chi2_rho0, p_rho0, z_two, p_two, r_gem, t_gem, p_gem (the rho0 and
#'   two-sample entries only when they apply).
#' @references Hedderich, Sachs & Reynarowych (2023), eqs (7.395)-(7.409).
#' @examples
#' corcmp(c(0.6, 0.7, 0.8), c(28, 33, 23))$chi2
#' @export
corcmp <- function(r, n, rho0 = NULL, confidence = 0.95) {
  k <- length(r)
  if (k < 2L || length(n) != k || any(abs(r) >= 1) || any(n < 4)) {
    stop("need at least two correlations in (-1, 1) with n_i >= 4", call. = FALSE)
  }
  z <- atanh(r)
  w <- n - 3
  zb <- sum(w * z) / sum(w)
  se <- 1 / sqrt(sum(w))
  q <- stats::qnorm(1 - (1 - confidence) / 2)
  x2 <- sum(w * (z - zb)^2)
  out <- list(r_pooled = tanh(zb), ci = tanh(zb + c(-1, 1) * q * se), z_pooled = zb, se_pooled = se,
              chi2 = x2, df = k - 1, p_value = stats::pchisq(x2, k - 1, lower.tail = FALSE))
  if (!is.null(rho0)) {
    x2r <- sum(w * (z - atanh(rho0))^2)
    out$chi2_rho0 <- x2r
    out$p_rho0 <- stats::pchisq(x2r, k, lower.tail = FALSE)
  }
  if (k == 2L) {
    zz <- abs(z[1L] - z[2L]) / sqrt(1 / w[1L] + 1 / w[2L])
    out$z_two <- zz
    out$p_two <- 2 * stats::pnorm(zz, lower.tail = FALSE)
  }
  nt <- sum(n)
  rg <- sum((n - 1) * r) / sum(n - 1)
  tg <- rg * sqrt((nt - k - 1) / (1 - rg^2))
  out$r_gem <- rg
  out$t_gem <- tg
  out$p_gem <- 2 * stats::pt(abs(tg), nt - k - 1, lower.tail = FALSE)
  out
}

#' Williams' test for two dependent correlations
#'
#' Steiger's (1980) T2 for H0: rho12 = rho13, (r12 - r13) sqrt((n - 1)(1 + r23)
#' / (2 (n - 1)/(n - 3) |R| + rbar^2 (1 - r23)^3)), rbar = (r12 + r13) / 2, on n -
#' 3 df (psych::r.test). Hedderich, Sachs & Reynarowych (2023, eq 7.394) print
#' (r12 + r13)^2 / 2 in place of rbar^2.
#'
#' @param r12,r13 Correlations with the common variable 1.
#' @param r23 Correlation of variables 2 and 3.
#' @param n Sample size.
#' @return Named list: statistic, df, p_value, det_R.
#' @references Williams, E. J. (1959). JRSS B 21, 396-399. Steiger, J. H.
#'   (1980). Psychological Bulletin 87, 245-251.
#' @examples
#' corwil(0.85, 0.71, 0.80, 30)$statistic
#' @export
corwil <- function(r12, r13, r23, n) {
  if (n < 5) stop("need n >= 5", call. = FALSE)
  det <- 1 - r12^2 - r13^2 - r23^2 + 2 * r12 * r13 * r23
  rb <- (r12 + r13) / 2
  a <- 2 * (n - 1) / (n - 3) * det + rb^2 * (1 - r23)^3
  stat <- (r12 - r13) * sqrt((n - 1) * (1 + r23) / a)
  list(statistic = stat, df = n - 3, p_value = 2 * stats::pt(abs(stat), n - 3, lower.tail = FALSE),
       det_R = det)
}

#' Sample size or power for the test of a correlation
#'
#' n = ((z_(1 - alpha/2) + z_(1 - beta)) / atanh(r))^2 + 3, or power =
#' pnorm(sqrt(n - 3) atanh(r) - z_(1 - alpha/2)) (Hedderich, Sachs &
#' Reynarowych 2023, eqs 7.404-7.405). Give exactly one of `power` and `n`.
#'
#' @param r Correlation to detect.
#' @param alpha Two-sided level.
#' @param power Desired power.
#' @param n Sample size.
#' @return Named list: n (rounded up) and n_exact, or power; r, alpha.
#' @references Hedderich, Sachs & Reynarowych (2023), eq (7.405), Table 7.83.
#' @examples
#' corss(0.6, power = 0.9)$n
#' @export
corss <- function(r, alpha = 0.05, power = NULL, n = NULL) {
  if (abs(r) >= 1 || r == 0) stop("`r` must be non-zero and in (-1, 1)", call. = FALSE)
  if (is.null(power) == is.null(n)) stop("give exactly one of `power` and `n`", call. = FALSE)
  za <- stats::qnorm(1 - alpha / 2)
  zr <- abs(atanh(r))
  if (is.null(n)) {
    ne <- ((za + stats::qnorm(power)) / zr)^2 + 3
    list(n = ceiling(ne), n_exact = ne, r = r, alpha = alpha)
  } else {
    list(power = stats::pnorm(sqrt(n - 3) * zr - za), n = n, r = r, alpha = alpha)
  }
}

#' Correlation between and within subjects for repeated measurements
#'
#' Bland and Altman (1995): the correlation of the subject means weighted by
#' the numbers of measurements (Hedderich, Sachs & Reynarowych 2023, eq 7.402)
#' and the within-subject correlation sign(beta_x) sqrt(SS_x / (SS_x +
#' SS_resid)) from the analysis of covariance of y on subject and x (eq 7.403),
#' tested by its F on 1 and N - k - 1 df; the within-subject value is the
#' repeated-measures correlation of rmcorr.
#'
#' @param x,y Numeric measurements.
#' @param subject Subject identifier of each measurement.
#' @return Named list: r_between, r_within, p_within, df_within, n_subjects.
#' @references Bland, J. M. & Altman, D. G. (1995). BMJ 310, 446 and 633.
#'   Bakdash, J. Z. & Marusich, L. R. (2017). Frontiers in Psychology 8, 456.
#' @examples
#' rpmcor(c(1, 2, 3, 2, 3, 5, 4, 6, 7), c(2, 3, 3, 1, 4, 4, 6, 6, 8), rep(1:3, each = 3))$r_within
#' @export
rpmcor <- function(x, y, subject) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  s <- factor(subject, levels = unique(subject))
  N <- length(x)
  k <- nlevels(s)
  if (length(y) != N || length(s) != N) stop("`x`, `y` and `subject` must be the same length", call. = FALSE)
  if (k < 3L || N - k - 1 < 1) stop("need at least three subjects and N > k + 1", call. = FALSE)
  m <- as.vector(table(s))
  mx <- as.vector(tapply(x, s, mean))
  my <- as.vector(tapply(y, s, mean))
  M <- sum(m)
  sxy <- sum(m * mx * my) - sum(m * mx) * sum(m * my) / M
  sxx <- sum(m * mx^2) - sum(m * mx)^2 / M
  syy <- sum(m * my^2) - sum(m * my)^2 / M
  fit_s <- stats::lm.fit(stats::model.matrix(~ s - 1), y)
  fit_f <- stats::lm.fit(cbind(stats::model.matrix(~ s - 1), x), y)
  rss_s <- sum(fit_s$residuals^2)
  rss_f <- sum(fit_f$residuals^2)
  ssx <- rss_s - rss_f
  df <- N - k - 1
  bx <- fit_f$coefficients[length(fit_f$coefficients)]
  list(r_between = sxy / sqrt(sxx * syy), r_within = unname(sign(bx) * sqrt(ssx / (ssx + rss_f))),
       p_within = stats::pf(ssx / (rss_f / df), 1, df, lower.tail = FALSE), df_within = df,
       n_subjects = k)
}
