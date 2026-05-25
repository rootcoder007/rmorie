#' Frequentist inference helpers for MORIE
#'
#' Confidence intervals, hypothesis tests, effect sizes, and power analysis.
#' All distribution functions follow R's standard naming convention
#' (`dnorm`, `pnorm`, `qnorm`, `rnorm`, etc.) and are re-exported with
#' additional MORIE-specific wrappers for epidemiological use cases.
#'
#' @name inference
#' @keywords internal
NULL


# ---------------------------------------------------------------------------
# Two-sample comparison helpers
# ---------------------------------------------------------------------------

#' Two-sample t-test with tidy output
#'
#' @param x1 Numeric vector (group 1).
#' @param x2 Numeric vector (group 2).
#' @param equal_var Assume equal variances? Default `FALSE` (Welch test).
#' @param alternative `"two.sided"`, `"greater"`, or `"less"`.
#' @return Named list: `t`, `df`, `p_value`, `ci_diff`, `morie_cohens_d`.
#' @export
#' @examples
#' morie_two_sample_t_test(rnorm(50, 0.5), rnorm(50, 0))
morie_two_sample_t_test <- function(x1, x2,
                                    equal_var = FALSE,
                                    alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  result <- stats::t.test(x1, x2,
    var.equal = equal_var,
    alternative = alternative
  )
  d <- morie_cohens_d(x1, x2)
  list(
    t = as.numeric(result$statistic),
    df = as.numeric(result$parameter),
    p_value = result$p.value,
    ci_diff = as.numeric(result$conf.int),
    morie_cohens_d = d
  )
}

#' One-sample t-test
#'
#' @param x Numeric vector.
#' @param mu0 Null hypothesis mean (default 0).
#' @param alternative `"two.sided"`, `"greater"`, or `"less"`.
#' @return Named list: `t`, `df`, `p_value`, `ci`.
#' @examples
#' morie_one_sample_t_test(x = rnorm(50))
#' @export
morie_one_sample_t_test <- function(x, mu0 = 0,
                                    alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  result <- stats::t.test(x, mu = mu0, alternative = alternative)
  list(
    t = as.numeric(result$statistic),
    df = as.numeric(result$parameter),
    p_value = result$p.value,
    ci = as.numeric(result$conf.int)
  )
}

#' Paired t-test
#'
#' @param x1 Numeric vector (before/condition 1).
#' @param x2 Numeric vector (after/condition 2).
#' @param alternative `"two.sided"`, `"greater"`, or `"less"`.
#' @return Named list: `t`, `df`, `p_value`, `ci_diff`, `mean_diff`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_paired_t_test <- function(x1, x2,
                                alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  result <- stats::t.test(x1, x2, paired = TRUE, alternative = alternative)
  list(
    t = as.numeric(result$statistic),
    df = as.numeric(result$parameter),
    p_value = result$p.value,
    ci_diff = as.numeric(result$conf.int),
    mean_diff = mean(x1 - x2, na.rm = TRUE)
  )
}

#' Chi-square test of independence or goodness-of-fit
#'
#' @param observed Observed counts (matrix for independence, vector for GOF).
#' @param expected Expected counts for GOF (optional; uniform if NULL).
#' @return Named list: `chi_sq`, `df`, `p_value`, `morie_cramers_v`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_chi_square_test <- function(observed, expected = NULL) {
  if (is.matrix(observed) || is.data.frame(observed)) {
    result <- stats::chisq.test(observed)
    v <- morie_cramers_v(as.matrix(observed))
  } else {
    result <- if (is.null(expected)) {
      stats::chisq.test(observed)
    } else {
      stats::chisq.test(observed, p = expected)
    }
    v <- NA_real_
  }
  list(
    chi_sq = as.numeric(result$statistic),
    df = as.numeric(result$parameter),
    p_value = result$p.value,
    morie_cramers_v = v
  )
}

#' Fisher's exact test for 2x2 tables
#'
#' @param table_2x2 A 2x2 matrix or data frame of counts.
#' @param alternative `"two.sided"`, `"greater"`, or `"less"`.
#' @return Named list: `odds_ratio`, `ci`, `p_value`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_fisher_exact_test <- function(table_2x2,
                              alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  result <- stats::fisher.test(as.matrix(table_2x2), alternative = alternative)
  list(
    odds_ratio = as.numeric(result$estimate),
    ci = as.numeric(result$conf.int),
    p_value = result$p.value
  )
}

#' One-way ANOVA
#'
#' @param ... Numeric vectors, one per group.
#' @return Named list: `F`, `df_between`, `df_within`, `p_value`,
#'   `morie_eta_squared`.
#' @export
#' @examples
#' morie_anova_one_way(rnorm(30, 0), rnorm(30, 0.5), rnorm(30, 1))
morie_anova_one_way <- function(...) {
  groups <- list(...)
  if (length(groups) < 2) stop("At least two groups required.")
  df_long <- do.call(rbind, lapply(seq_along(groups), function(i) {
    data.frame(y = groups[[i]], grp = factor(i))
  }))
  fit <- stats::aov(y ~ grp, data = df_long)
  s <- summary(fit)[[1]]
  f_val <- s["grp", "F value"]
  df_b <- s["grp", "Df"]
  df_w <- s["Residuals", "Df"]
  ss_b <- s["grp", "Sum Sq"]
  ss_t <- sum(s[, "Sum Sq"])
  list(
    F = f_val,
    df_between = df_b,
    df_within = df_w,
    p_value = s["grp", "Pr(>F)"],
    morie_eta_squared = ss_b / ss_t
  )
}

#' Kruskal-Wallis non-parametric ANOVA
#'
#' @param ... Numeric vectors, one per group.
#' @return Named list: `H`, `df`, `p_value`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_kruskal_wallis_test <- function(...) {
  groups <- list(...)
  df_long <- do.call(rbind, lapply(seq_along(groups), function(i) {
    data.frame(y = groups[[i]], grp = factor(i))
  }))
  result <- stats::kruskal.test(y ~ grp, data = df_long)
  list(
    H = as.numeric(result$statistic),
    df = as.numeric(result$parameter),
    p_value = result$p.value
  )
}

#' Mann-Whitney U test (Wilcoxon rank-sum)
#'
#' @param x1 Numeric vector (group 1).
#' @param x2 Numeric vector (group 2).
#' @param alternative `"two.sided"`, `"greater"`, or `"less"`.
#' @return Named list: `W`, `p_value`, `r` (effect size).
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_mann_whitney_test <- function(x1, x2,
                                    alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  result <- stats::wilcox.test(x1, x2,
    alternative = alternative,
    exact = FALSE
  )
  # Total sample size N = n1 + n2 (Rosenthal-style r = Z / sqrt(N))
  n_total <- length(x1) + length(x2)
  r_effect <- abs(stats::qnorm(result$p.value / 2)) / sqrt(n_total)
  list(W = as.numeric(result$statistic), p_value = result$p.value, r = r_effect)
}

#' Wilcoxon signed-rank test (paired)
#'
#' @param x1 Numeric vector (before).
#' @param x2 Numeric vector (after).
#' @param alternative `"two.sided"`, `"greater"`, or `"less"`.
#' @return Named list: `V`, `p_value`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_wilcoxon_signed_rank_test <- function(x1, x2,
                                      alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  result <- stats::wilcox.test(x1, x2,
    paired = TRUE,
    alternative = alternative, exact = FALSE
  )
  list(V = as.numeric(result$statistic), p_value = result$p.value)
}

#' Shapiro-Wilk normality test
#'
#' @param x Numeric vector.
#' @param alpha Significance level for the `is_normal` flag (default 0.05).
#' @return Named list: `W`, `p_value`, `is_normal`.
#' @examples
#' morie_shapiro_wilk_test(x = rnorm(50))
#' @export
morie_shapiro_wilk_test <- function(x, alpha = 0.05) {
  result <- stats::shapiro.test(x)
  list(
    W = as.numeric(result$statistic),
    p_value = result$p.value,
    is_normal = result$p.value > alpha
  )
}

#' Levene test for equality of variances
#'
#' @param ... Numeric vectors, one per group.
#' @return Named list: `F`, `p_value`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_levene_test <- function(...) {
  groups <- list(...)
  df_long <- do.call(rbind, lapply(seq_along(groups), function(i) {
    data.frame(y = groups[[i]], grp = factor(i))
  }))
  # Levene statistic via absolute deviations from group medians
  df_long$dev <- abs(df_long$y - ave(df_long$y, df_long$grp, FUN = median))
  fit <- stats::aov(dev ~ grp, data = df_long)
  s <- summary(fit)[[1]]
  list(F = s["grp", "F value"], p_value = s["grp", "Pr(>F)"])
}


# ---------------------------------------------------------------------------
# Confidence intervals
# ---------------------------------------------------------------------------

#' Wilson score confidence interval for a proportion
#'
#' @param successes Number of successes.
#' @param n Total observations.
#' @param alpha Significance level (default 0.05 -> 95% CI).
#' @param method `"wilson"` (default), `"exact"` (Clopper-Pearson),
#'   or `"wald"`.
#' @return Named list: `p_hat`, `ci_lower`, `ci_upper`.
#' @export
#' @examples
#' morie_proportion_ci(35, 100)
morie_proportion_ci <- function(successes, n, alpha = 0.05,
                          method = c("wilson", "exact", "wald")) {
  method <- match.arg(method)
  p <- successes / n
  z <- stats::qnorm(1 - alpha / 2)

  if (method == "wilson") {
    denom <- 1 + z^2 / n
    centre <- (p + z^2 / (2 * n)) / denom
    margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
    ci <- c(centre - margin, centre + margin)
  } else if (method == "exact") {
    # Clopper-Pearson, with explicit edge-case handling so that
    # qbeta(., 0, .) (which is undefined) never gets called.
    lo <- if (successes == 0L) 0
          else stats::qbeta(alpha / 2, successes, n - successes + 1)
    hi <- if (successes == n)  1
          else stats::qbeta(1 - alpha / 2, successes + 1, n - successes)
    ci <- c(lo, hi)
  } else {
    margin <- z * sqrt(p * (1 - p) / n)
    ci <- c(p - margin, p + margin)
  }

  list(p_hat = p, ci_lower = pmax(0, ci[1]), ci_upper = pmin(1, ci[2]))
}

#' Odds ratio and 95% CI from a 2x2 contingency table
#'
#' @param table_2x2 A 2x2 matrix: rows are treatment, columns are outcome.
#' @param alpha Significance level.
#' @return Named list: `or`, `ci_lower`, `ci_upper`, `p_value`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_odds_ratio_ci <- function(table_2x2, alpha = 0.05) {
  m <- as.matrix(table_2x2)
  result <- stats::fisher.test(m)
  list(
    or = as.numeric(result$estimate),
    ci_lower = result$conf.int[1],
    ci_upper = result$conf.int[2],
    p_value = result$p.value
  )
}

#' Risk ratio (relative risk) with log-normal CI
#'
#' @param table_2x2 A 2x2 matrix: rows are exposure, columns are outcome (disease = col 1).
#' @param alpha Significance level.
#' @return Named list: `rr`, `ci_lower`, `ci_upper`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_risk_ratio_ci <- function(table_2x2, alpha = 0.05) {
  m <- as.matrix(table_2x2)
  a <- m[1, 1]
  b <- m[1, 2]
  c <- m[2, 1]
  d <- m[2, 2]
  n1 <- a + b
  n2 <- c + d
  p1 <- a / n1
  p2 <- c / n2
  rr <- p1 / p2
  log_se <- sqrt(1 / a - 1 / n1 + 1 / c - 1 / n2)
  z <- stats::qnorm(1 - alpha / 2)
  list(
    rr = rr,
    ci_lower = exp(log(rr) - z * log_se),
    ci_upper = exp(log(rr) + z * log_se)
  )
}

#' Risk difference (ARD) with Newcombe CI
#'
#' @param table_2x2 A 2x2 matrix: rows are exposure, columns are outcome.
#' @param alpha Significance level.
#' @return Named list: `rd`, `ci_lower`, `ci_upper`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_risk_difference_ci <- function(table_2x2, alpha = 0.05) {
  # Newcombe (1998) hybrid score CI:
  # 1. Wilson score CI for each proportion separately: (l_i, u_i)
  # 2. RD CI = (rd - delta_lo, rd + delta_hi) where
  #    delta_lo = sqrt((p1 - l1)^2 + (u2 - p2)^2)
  #    delta_hi = sqrt((u1 - p1)^2 + (p2 - l2)^2)
  # The simple Wald form (rd +- z*sqrt(p1q1/n1 + p2q2/n2)) does not
  # have the correct coverage near 0/1; was the pre-2026-05-22 form.
  m <- as.matrix(table_2x2)
  a <- m[1, 1]
  b <- m[1, 2]
  cc <- m[2, 1]
  d <- m[2, 2]  # 'c' shadows base c()
  n1 <- a + b
  n2 <- cc + d
  p1 <- a / n1
  p2 <- cc / n2
  rd <- p1 - p2
  z <- stats::qnorm(1 - alpha / 2)
  wilson <- function(x, n, z) {
    if (n == 0) return(c(0, 1))
    p <- x / n
    denom <- 1 + z^2 / n
    centre <- (p + z^2 / (2 * n)) / denom
    half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
    c(max(0, centre - half), min(1, centre + half))
  }
  ci1 <- wilson(a,  n1, z)
  ci2 <- wilson(cc, n2, z)
  l1 <- ci1[1]
  u1 <- ci1[2]
  l2 <- ci2[1]
  u2 <- ci2[2]
  delta_lo <- sqrt((p1 - l1)^2 + (u2 - p2)^2)
  delta_hi <- sqrt((u1 - p1)^2 + (p2 - l2)^2)
  list(
    rd       = rd,
    ci_lower = rd - delta_lo,
    ci_upper = rd + delta_hi,
    method   = "Newcombe (1998) hybrid score"
  )
}


# ---------------------------------------------------------------------------
# Effect sizes
# ---------------------------------------------------------------------------

#' Cohen's d effect size
#'
#' @param x1 Numeric vector (group 1).
#' @param x2 Numeric vector (group 2).
#' @param pooled Use pooled SD (default `TRUE`). If `FALSE`, uses `sd(x2)`.
#' @return Numeric Cohen's d.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_cohens_d <- function(x1, x2, pooled = TRUE) {
  m1 <- mean(x1, na.rm = TRUE)
  m2 <- mean(x2, na.rm = TRUE)
  n1 <- sum(!is.na(x1))
  n2 <- sum(!is.na(x2))
  s1 <- stats::sd(x1, na.rm = TRUE)
  s2 <- stats::sd(x2, na.rm = TRUE)
  sd_denom <- if (pooled) {
    sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  } else {
    s2
  }
  (m1 - m2) / sd_denom
}

#' Hedges' g (bias-corrected Cohen's d)
#'
#' @inheritParams morie_cohens_d
#' @return Numeric Hedges' g.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_hedges_g <- function(x1, x2) {
  d <- morie_cohens_d(x1, x2, pooled = TRUE)
  n1 <- sum(!is.na(x1))
  n2 <- sum(!is.na(x2))
  m <- n1 + n2 - 2  # degrees of freedom
  if (m <= 0) return(d)
  # Exact gamma-based small-sample correction (Hedges 1981):
  # J(m) = Gamma(m/2) / (sqrt(m/2) * Gamma((m-1)/2))
  # Matches Python inference.py:hedges_g; the older 1 - 3/(4m-1)
  # approximation diverged from this at small m.
  log_J <- lgamma(m / 2) - 0.5 * log(m / 2) - lgamma((m - 1) / 2)
  d * exp(log_J)
}

#' Eta-squared from F-statistic
#'
#' @param f_stat F statistic.
#' @param df_between Degrees of freedom (numerator).
#' @param df_within Degrees of freedom (denominator).
#' @return Numeric eta-squared.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_eta_squared <- function(f_stat, df_between, df_within) {
  ss_between <- f_stat * df_between
  ss_total <- ss_between + df_within
  ss_between / ss_total
}

#' Omega-squared (less biased than eta-squared)
#'
#' @inheritParams morie_eta_squared
#' @param n Total sample size.
#' @return Numeric omega-squared.
#' @export
#' @examples
#' morie_omega_squared(f_stat = 5.2, df_between = 2, df_within = 87, n = 90)
morie_omega_squared <- function(f_stat, df_between, df_within, n) {
  (df_between * (f_stat - 1)) / (df_between * (f_stat - 1) + n)
}

#' Cramer's V for categorical association
#'
#' @param contingency_table A numeric matrix of observed counts.
#' @return Numeric Cramer's V in the interval \[0, 1\].
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_cramers_v <- function(contingency_table) {
  m <- as.matrix(contingency_table)
  result <- stats::chisq.test(m, correct = FALSE)
  chi2 <- as.numeric(result$statistic)
  n <- sum(m)
  k <- min(nrow(m), ncol(m))
  sqrt(chi2 / (n * (k - 1)))
}

#' Spearman rank correlation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @return Named list: `rho`, `p_value`.
#' @examples
#' morie_spearman_rho(x = rnorm(50), y = rnorm(50))
#' @export
morie_spearman_rho <- function(x, y) {
  result <- stats::cor.test(x, y, method = "spearman", exact = FALSE)
  list(rho = as.numeric(result$estimate), p_value = result$p.value)
}

#' Kendall's tau-b
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @return Named list: `tau`, `p_value`.
#' @examples
#' morie_kendall_tau(x = rnorm(50), y = rnorm(50))
#' @export
morie_kendall_tau <- function(x, y) {
  result <- stats::cor.test(x, y, method = "kendall", exact = FALSE)
  list(tau = as.numeric(result$estimate), p_value = result$p.value)
}

#' Point-biserial correlation
#'
#' @param binary_var Binary numeric vector (0/1).
#' @param continuous_var Continuous numeric vector.
#' @return Named list: `r`, `p_value`.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_point_biserial_r <- function(binary_var, continuous_var) {
  result <- stats::cor.test(binary_var, continuous_var)
  list(r = as.numeric(result$estimate), p_value = result$p.value)
}


# ---------------------------------------------------------------------------
# Power analysis
# ---------------------------------------------------------------------------

#' Power for a two-sample t-test
#'
#' Solve for any missing parameter (`n`, `delta`, `sd`, `sig.level`,
#' or `power`). Mirrors R's `power.t.test()`.
#'
#' @param n Sample size per group (NULL to solve for it).
#' @param delta Effect size (difference in means).
#' @param sd Standard deviation (pooled).
#' @param sig_level Type I error rate (alpha).
#' @param power Desired power (1 - beta).
#' @param alternative `"two.sided"` or `"one.sided"`.
#' @param type `"two.sample"`, `"one.sample"`, or `"paired"`.
#' @return Result of `stats::power.t.test()`.
#' @export
#' @examples
#' morie_power_t_test(n = NULL, delta = 0.5, power = 0.80)
morie_power_t_test <- function(n = NULL, delta = NULL, sd = 1,
                               sig_level = 0.05, power = NULL,
                               alternative = c("two.sided", "one.sided"),
                               type = c("two.sample", "one.sample", "paired")) {
  alternative <- match.arg(alternative)
  type <- match.arg(type)
  stats::power.t.test(
    n = n, delta = delta, sd = sd,
    sig.level = sig_level, power = power,
    alternative = alternative, type = type
  )
}

#' Power for a two-proportion z-test
#'
#' Mirrors R's `power.prop.test()`.
#'
#' @param n Sample size per group.
#' @param p1 Proportion in group 1.
#' @param p2 Proportion in group 2.
#' @param sig_level Type I error rate.
#' @param power Desired power.
#' @param alternative `"two.sided"` or `"one.sided"`.
#' @return Result of `stats::power.prop.test()`.
#' @export
#' @examples
#' morie_power_prop_test(p1 = 0.30, p2 = 0.20, power = 0.80)
morie_power_prop_test <- function(n = NULL, p1 = NULL, p2 = NULL,
                                  sig_level = 0.05, power = NULL,
                                  alternative = c("two.sided", "one.sided")) {
  alternative <- match.arg(alternative)
  stats::power.prop.test(
    n = n, p1 = p1, p2 = p2,
    sig.level = sig_level, power = power,
    alternative = alternative
  )
}

#' Sample size for logistic regression detecting a target odds ratio
#'
#' Uses the formula from Hsieh et al. (1998):
#' \deqn{n = \frac{(z_{\alpha/2} + z_\beta)^2}{p_1(1-p_1) [\log(OR)]^2}}{n = frac{(z_alpha/2 + z_beta)^2}{p_1(1-p_1) [log(OR)]^2}}
#'
#' @param p0 Prevalence under control.
#' @param or Target odds ratio.
#' @param alpha Significance level.
#' @param power Desired power.
#' @param two_sided Logical.
#' @return Integer sample size.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
#' @references
#'   Hsieh FY, Bloch DA, Larsen MD (1998). A simple method of sample size
#'   calculation for linear and logistic regression.
#'   *Statistics in Medicine*, 17(14):1623-1634.
morie_sample_size_logistic <- function(p0, or, alpha = 0.05, power = 0.80,
                                 two_sided = TRUE) {
  p1 <- (or * p0) / (1 - p0 + or * p0)
  z_a <- stats::qnorm(if (two_sided) 1 - alpha / 2 else 1 - alpha)
  z_b <- stats::qnorm(power)
  p_bar <- (p0 + p1) / 2
  n <- as.integer(ceiling((z_a + z_b)^2 / (p_bar * (1 - p_bar) * (log(or))^2)))
  n
}
