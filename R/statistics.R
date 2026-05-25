# SPDX-License-Identifier: AGPL-3.0-or-later
#' Comprehensive hypothesis testing suite for epidemiological research
#'
#' R port of the Python module \code{morie.statistics}. Every function
#' returns a named \code{list} (class \code{"morie_test_result"})
#' carrying the test statistic, p-value, degrees of freedom, confidence
#' interval, effect size, point estimate, sample size and a free-form
#' \code{extra} list, so downstream code can post-process programmatically.
#'
#' Categories
#' ----------
#' \itemize{
#'   \item Location: \code{one_sample_ttest}, \code{two_sample_ttest},
#'     \code{welch_ttest}, \code{paired_ttest}
#'   \item ANOVA / non-parametric ANOVA: \code{one_way_anova},
#'     \code{two_way_anova}, \code{repeated_measures_anova},
#'     \code{kruskal_wallis}, \code{friedman_test}
#'   \item Chi-squared family: \code{chi2_goodness_of_fit},
#'     \code{chi2_independence}, \code{mcnemar_test}, \code{cochrans_q}
#'   \item Correlation: \code{pearson_correlation},
#'     \code{spearman_correlation}, \code{kendall_correlation},
#'     \code{point_biserial_correlation}, \code{partial_correlation},
#'     \code{semi_partial_correlation}
#'   \item Non-parametric: \code{mann_whitney_u},
#'     \code{wilcoxon_signed_rank}, \code{ks_test_one_sample},
#'     \code{ks_test_two_sample}, \code{anderson_darling},
#'     \code{levene_test}, \code{bartlett_test}, \code{runs_test}
#'   \item Normality: \code{shapiro_wilk}, \code{dagostino_pearson},
#'     \code{jarque_bera}, \code{lilliefors_test}
#'   \item Proportions: \code{one_proportion_ztest},
#'     \code{two_proportion_ztest}, \code{fisher_exact_test}
#'   \item Agreement: \code{cohens_kappa}, \code{fleiss_kappa},
#'     \code{intraclass_correlation}
#'   \item Convenience: \code{normality_suite},
#'     \code{variance_equality_suite}, \code{correlation_matrix},
#'     \code{auto_test}
#' }
#'
#' @name statistics
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.stat_validate <- function(x, name = "x") {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}

.stat_result <- function(method, test_statistic, p_value,
                         df = NA_real_, ci_lower = NA_real_,
                         ci_upper = NA_real_, effect_size = NA_real_,
                         estimate = NA_real_, n = NA_integer_,
                         extra = list()) {
  out <- list(
    method = method,
    test_statistic = as.numeric(test_statistic),
    p_value = as.numeric(p_value),
    df = if (is.null(df)) NA_real_ else as.numeric(df),
    ci_lower = if (is.null(ci_lower)) NA_real_ else as.numeric(ci_lower),
    ci_upper = if (is.null(ci_upper)) NA_real_ else as.numeric(ci_upper),
    effect_size = if (is.null(effect_size)) NA_real_ else as.numeric(effect_size),
    estimate = if (is.null(estimate)) NA_real_ else as.numeric(estimate),
    n = if (is.null(n)) NA_integer_ else as.integer(n),
    extra = extra
  )
  class(out) <- c("morie_test_result", "morie_rich_result", "list")
  out
}

#' @export
print.morie_test_result <- function(x, ...) {
  cat(x$method, "\
", sep = "")
  cat(strrep("-", nchar(x$method)), "\
", sep = "")
  cat(sprintf("  statistic = %.6g\
", x$test_statistic))
  cat(sprintf("  p-value   = %.6g\
", x$p_value))
  if (is.finite(x$df))          cat(sprintf("  df        = %.6g\
", x$df))
  if (is.finite(x$estimate))    cat(sprintf("  estimate  = %.6g\
", x$estimate))
  if (is.finite(x$effect_size)) cat(sprintf("  effect    = %.6g\
", x$effect_size))
  if (is.finite(x$ci_lower) && is.finite(x$ci_upper))
    cat(sprintf("  CI        = [%.6g, %.6g]\
", x$ci_lower, x$ci_upper))
  if (!is.na(x$n)) cat(sprintf("  n         = %d\
", x$n))
  invisible(x)
}

.cohens_d_ind <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  sp <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  if (sp == 0) return(0)
  (mean(x) - mean(y)) / sp
}

.cohens_d_one <- function(x, mu0) {
  s <- sd(x)
  if (s == 0) return(0)
  (mean(x) - mu0) / s
}

.cohens_d_paired <- function(d) {
  s <- sd(d)
  if (s == 0) return(0)
  mean(d) / s
}

.mean_ci <- function(x, confidence = 0.95) {
  n <- length(x)
  se <- sd(x) / sqrt(n)
  tcrit <- stats::qt((1 + confidence) / 2, n - 1)
  c(mean(x) - tcrit * se, mean(x) + tcrit * se)
}

.diff_ci <- function(x, y, confidence = 0.95, equal_var = TRUE) {
  nx <- length(x)
  ny <- length(y)
  diff <- mean(x) - mean(y)
  if (equal_var) {
    sp2 <- ((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2)
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
    df_val <- nx + ny - 2
  } else {
    s1 <- var(x)
    s2 <- var(y)
    se <- sqrt(s1 / nx + s2 / ny)
    num <- (s1 / nx + s2 / ny)^2
    denom <- (s1 / nx)^2 / (nx - 1) + (s2 / ny)^2 / (ny - 1)
    df_val <- if (denom > 0) num / denom else 1
  }
  tcrit <- stats::qt((1 + confidence) / 2, df_val)
  c(diff - tcrit * se, diff + tcrit * se)
}


# ===================================================================
# T-TESTS
# ===================================================================

#' One-sample Student's t-test
#' @param x Numeric vector.
#' @param mu0 Hypothesised mean.
#' @param confidence Confidence level (default 0.95).
#' @return A \code{morie_test_result}.
#' @export
one_sample_ttest <- function(x, mu0 = 0, confidence = 0.95) {
  x <- .stat_validate(x)
  n <- length(x)
  if (n < 2L) stop("Need at least 2 observations for a t-test.")
  tt <- stats::t.test(x, mu = mu0, conf.level = confidence)
  ci <- .mean_ci(x, confidence)
  .stat_result(
    method = "One-sample t-test",
    test_statistic = unname(tt$statistic),
    p_value = tt$p.value, df = n - 1,
    ci_lower = ci[1], ci_upper = ci[2],
    effect_size = .cohens_d_one(x, mu0),
    estimate = mean(x), n = n
  )
}

#' Independent two-sample t-test (equal or unequal variance)
#' @param x,y Numeric vectors.
#' @param equal_var If FALSE, use Welch's correction.
#' @param confidence Confidence level.
#' @export
two_sample_ttest <- function(x, y, equal_var = TRUE, confidence = 0.95) {
  x <- .stat_validate(x)
  y <- .stat_validate(y)
  tt <- stats::t.test(x, y, var.equal = equal_var, conf.level = confidence)
  ci <- .diff_ci(x, y, confidence, equal_var)
  nx <- length(x)
  ny <- length(y)
  if (equal_var) {
    df_val <- nx + ny - 2
    label <- "Two-sample t-test (equal var)"
  } else {
    s1 <- var(x)
    s2 <- var(y)
    num <- (s1 / nx + s2 / ny)^2
    denom <- (s1 / nx)^2 / (nx - 1) + (s2 / ny)^2 / (ny - 1)
    df_val <- if (denom > 0) num / denom else 1
    label <- "Welch's t-test"
  }
  .stat_result(
    method = label,
    test_statistic = unname(tt$statistic),
    p_value = tt$p.value, df = df_val,
    ci_lower = ci[1], ci_upper = ci[2],
    effect_size = .cohens_d_ind(x, y),
    estimate = mean(x) - mean(y),
    n = nx + ny
  )
}

#' Welch's t-test (convenience wrapper)
#' @inheritParams two_sample_ttest
#' @export
welch_ttest <- function(x, y, confidence = 0.95) {
  two_sample_ttest(x, y, equal_var = FALSE, confidence = confidence)
}

#' Paired-sample t-test
#' @param x,y Equal-length numeric vectors.
#' @param confidence Confidence level.
#' @export
paired_ttest <- function(x, y, confidence = 0.95) {
  x <- .stat_validate(x)
  y <- .stat_validate(y)
  if (length(x) != length(y)) stop("Paired t-test requires equal-length vectors.")
  d <- x - y
  n <- length(d)
  tt <- stats::t.test(x, y, paired = TRUE, conf.level = confidence)
  ci <- .mean_ci(d, confidence)
  .stat_result(
    method = "Paired t-test",
    test_statistic = unname(tt$statistic),
    p_value = tt$p.value, df = n - 1,
    ci_lower = ci[1], ci_upper = ci[2],
    effect_size = .cohens_d_paired(d),
    estimate = mean(d), n = n
  )
}


# ===================================================================
# ANOVA FAMILY
# ===================================================================

#' One-way between-subjects ANOVA
#' @param ... Two or more numeric vectors (groups).
#' @return \code{morie_test_result} with eta-squared effect size.
#' @export
one_way_anova <- function(...) {
  groups <- list(...)
  if (length(groups) < 2L) stop("ANOVA requires at least 2 groups.")
  cleaned <- lapply(groups, .stat_validate)
  vals <- unlist(cleaned)
  grp <- factor(rep(seq_along(cleaned), lengths(cleaned)))
  fit <- stats::aov(vals ~ grp)
  s <- summary(fit)[[1L]]
  f_stat <- s[["F value"]][1]
  p_val <- s[["Pr(>F)"]][1]
  grand <- mean(vals)
  ss_b <- sum(lengths(cleaned) * (vapply(cleaned, mean, 0) - grand)^2)
  ss_t <- sum((vals - grand)^2)
  eta2 <- if (ss_t > 0) ss_b / ss_t else 0
  k <- length(cleaned)
  n_total <- length(vals)
  .stat_result(
    method = "One-way ANOVA",
    test_statistic = f_stat, p_value = p_val,
    df = k - 1, effect_size = eta2, n = n_total,
    extra = list(df_between = k - 1, df_within = n_total - k,
                 eta_squared = eta2)
  )
}

#' Two-way factorial ANOVA (Type-II SS)
#'
#' Uses base R \code{aov}, then \code{drop1} for Type-II sums of squares.
#'
#' @param data A data frame.
#' @param outcome Name of dependent-variable column.
#' @param factor_a,factor_b Names of factor columns.
#' @export
two_way_anova <- function(data, outcome, factor_a, factor_b) {
  data <- stats::na.omit(data[, c(outcome, factor_a, factor_b)])
  data[[factor_a]] <- factor(data[[factor_a]])
  data[[factor_b]] <- factor(data[[factor_b]])
  fml <- stats::as.formula(
    sprintf("%s ~ %s * %s", outcome, factor_a, factor_b))
  fit <- stats::aov(fml, data = data)
  tab <- as.data.frame(stats::anova(fit))
  interaction_key <- sprintf("%s:%s", factor_a, factor_b)
  f_int <- if (interaction_key %in% rownames(tab)) tab[interaction_key, "F value"] else NA_real_
  p_int <- if (interaction_key %in% rownames(tab)) tab[interaction_key, "Pr(>F)"] else NA_real_
  ss_total <- sum(tab[, "Sum Sq"], na.rm = TRUE)
  eta2_a <- tab[factor_a, "Sum Sq"] / ss_total
  .stat_result(
    method = "Two-way ANOVA",
    test_statistic = f_int, p_value = p_int,
    effect_size = eta2_a, n = nrow(data),
    extra = list(anova_table = tab)
  )
}

#' One-way repeated-measures ANOVA
#'
#' Sphericity assumption is left to the user; this routine computes the
#' uncorrected F. Pair with \code{ez::ezANOVA} for GG/HF correction if
#' needed.
#'
#' @param data Long-format data frame.
#' @param outcome,subject,within Column names.
#' @export
repeated_measures_anova <- function(data, outcome, subject, within) {
  df <- stats::na.omit(data[, c(outcome, subject, within)])
  levels_w <- unique(df[[within]])
  k <- length(levels_w)
  if (k < 2L) stop("Need at least 2 levels for repeated-measures ANOVA.")
  wide <- stats::reshape(df, idvar = subject, timevar = within,
                         direction = "wide", v.names = outcome)
  wide <- stats::na.omit(wide)
  mat <- as.matrix(wide[, grep(paste0("^", outcome, "\\."), names(wide))])
  n <- nrow(mat)
  grand_mean <- mean(mat)
  subj_means <- rowMeans(mat)
  cond_means <- colMeans(mat)
  ss_between <- k * sum((subj_means - grand_mean)^2)
  ss_cond <- n * sum((cond_means - grand_mean)^2)
  ss_total <- sum((mat - grand_mean)^2)
  ss_error <- ss_total - ss_between - ss_cond
  df_cond <- k - 1
  df_error <- (n - 1) * (k - 1)
  ms_cond <- if (df_cond > 0) ss_cond / df_cond else 0
  ms_error <- if (df_error > 0) ss_error / df_error else 0
  f_stat <- if (ms_error > 0) ms_cond / ms_error else 0
  p <- 1 - stats::pf(f_stat, df_cond, df_error)
  eta2 <- if ((ss_cond + ss_error) > 0) ss_cond / (ss_cond + ss_error) else 0
  .stat_result(
    method = "Repeated-measures ANOVA",
    test_statistic = f_stat, p_value = p,
    df = df_cond, effect_size = eta2, n = n,
    extra = list(df_error = df_error, ss_cond = ss_cond, ss_error = ss_error)
  )
}

#' Kruskal-Wallis H-test
#' @param ... Two or more numeric vectors.
#' @export
kruskal_wallis <- function(...) {
  groups <- list(...)
  cleaned <- lapply(groups, .stat_validate)
  vals <- unlist(cleaned)
  grp <- factor(rep(seq_along(cleaned), lengths(cleaned)))
  kt <- stats::kruskal.test(vals, grp)
  k <- length(cleaned)
  n_total <- length(vals)
  h_stat <- unname(kt$statistic)
  eta2_h <- if ((n_total - k) > 0) (h_stat - k + 1) / (n_total - k) else 0
  .stat_result(
    method = "Kruskal-Wallis H-test",
    test_statistic = h_stat, p_value = kt$p.value,
    df = k - 1, effect_size = max(eta2_h, 0), n = n_total
  )
}

#' Friedman test (repeated-measures rank ANOVA)
#' @param ... Three or more equal-length numeric vectors.
#' @export
friedman_test <- function(...) {
  groups <- list(...)
  cleaned <- lapply(groups, .stat_validate)
  ln <- vapply(cleaned, length, 0L)
  if (length(unique(ln)) != 1L)
    stop("Friedman test requires equal-length groups.")
  mat <- do.call(cbind, cleaned)
  fr <- stats::friedman.test(mat)
  k <- length(cleaned)
  n <- ln[1]
  chi2 <- unname(fr$statistic)
  w <- if (n * (k - 1) > 0) chi2 / (n * (k - 1)) else 0
  .stat_result(
    method = "Friedman test",
    test_statistic = chi2, p_value = fr$p.value,
    df = k - 1, effect_size = w, n = n,
    extra = list(kendall_w = w)
  )
}


# ===================================================================
# CHI-SQUARED FAMILY
# ===================================================================

#' Chi-squared goodness-of-fit test
#' @param observed Observed counts.
#' @param expected Expected counts or NULL for uniform.
#' @export
chi2_goodness_of_fit <- function(observed, expected = NULL) {
  obs <- as.numeric(observed)
  if (is.null(expected)) {
    p <- rep(1 / length(obs), length(obs))
  } else {
    p <- as.numeric(expected) / sum(expected)
  }
  ct <- suppressWarnings(stats::chisq.test(obs, p = p, rescale.p = TRUE))
  n <- sum(obs)
  w <- if (n > 0) sqrt(unname(ct$statistic) / n) else 0
  .stat_result(
    method = "Chi-squared goodness-of-fit",
    test_statistic = unname(ct$statistic),
    p_value = ct$p.value,
    df = length(obs) - 1, effect_size = w, n = as.integer(n)
  )
}

#' Chi-squared test of independence
#' @param contingency_table A matrix or table of counts.
#' @param correction Yates's continuity correction (2x2).
#' @export
chi2_independence <- function(contingency_table, correction = TRUE) {
  tab <- as.matrix(contingency_table)
  ct <- suppressWarnings(stats::chisq.test(tab, correct = correction))
  n <- sum(tab)
  k <- min(dim(tab)) - 1
  v <- if (n * k > 0) sqrt(unname(ct$statistic) / (n * k)) else 0
  .stat_result(
    method = "Chi-squared test of independence",
    test_statistic = unname(ct$statistic),
    p_value = ct$p.value, df = unname(ct$parameter),
    effect_size = v, n = as.integer(n),
    extra = list(expected = ct$expected, cramers_v = v)
  )
}

#' McNemar's test (paired nominal data)
#' @param contingency_table 2x2 table.
#' @param exact Use exact binomial.
#' @export
mcnemar_test <- function(contingency_table, exact = FALSE) {
  tab <- as.matrix(contingency_table)
  if (!all(dim(tab) == c(2, 2))) stop("McNemar test requires a 2x2 table.")
  b <- tab[1, 2]
  c <- tab[2, 1]
  n <- sum(tab)
  if (exact) {
    if ((b + c) > 0) {
      p <- stats::binom.test(min(b, c), b + c, 0.5)$p.value
      chi2 <- b + c
    } else { p <- 1
    chi2 <- 0 }
  } else {
    chi2 <- if ((b + c) > 0) (abs(b - c) - 1)^2 / (b + c) else 0
    p <- if ((b + c) > 0) 1 - stats::pchisq(chi2, 1) else 1
  }
  .stat_result(
    method = paste0("McNemar's test", if (exact) " (exact)" else ""),
    test_statistic = chi2, p_value = p, df = 1, n = as.integer(n)
  )
}

#' Cochran's Q test
#' @param ... Three or more matched binary 0/1 vectors.
#' @export
cochrans_q <- function(...) {
  groups <- lapply(list(...), as.numeric)
  n <- length(groups[[1]])
  k <- length(groups)
  if (any(vapply(groups, length, 0L) != n))
    stop("All groups must have the same length.")
  mat <- do.call(cbind, groups)
  rs <- rowSums(mat)
  cs <- colSums(mat)
  T_tot <- sum(mat)
  num <- (k - 1) * (k * sum(cs^2) - T_tot^2)
  denom <- k * T_tot - sum(rs^2)
  q <- if (denom > 0) num / denom else 0
  p <- 1 - stats::pchisq(q, k - 1)
  .stat_result(
    method = "Cochran's Q test",
    test_statistic = q, p_value = p, df = k - 1, n = n
  )
}


# ===================================================================
# CORRELATION
# ===================================================================

.fisher_z_ci <- function(r, n, confidence) {
  z <- atanh(r)
  se <- if (n > 3) 1 / sqrt(n - 3) else Inf
  zcrit <- stats::qnorm((1 + confidence) / 2)
  c(tanh(z - zcrit * se), tanh(z + zcrit * se))
}

#' Pearson product-moment correlation
#' @param x,y Numeric vectors.
#' @param confidence Confidence level.
#' @export
pearson_correlation <- function(x, y, confidence = 0.95) {
  x <- .stat_validate(x)
  y <- .stat_validate(y)
  n <- min(length(x), length(y))
  x <- x[seq_len(n)]
  y <- y[seq_len(n)]
  ct <- stats::cor.test(x, y, method = "pearson", conf.level = confidence)
  r <- unname(ct$estimate)
  ci <- .fisher_z_ci(r, n, confidence)
  .stat_result(
    method = "Pearson correlation",
    test_statistic = r, p_value = ct$p.value, df = n - 2,
    ci_lower = ci[1], ci_upper = ci[2],
    effect_size = r^2, estimate = r, n = n
  )
}

#' Spearman rank correlation
#' @inheritParams pearson_correlation
#' @export
spearman_correlation <- function(x, y, confidence = 0.95) {
  x <- .stat_validate(x)
  y <- .stat_validate(y)
  n <- min(length(x), length(y))
  x <- x[seq_len(n)]
  y <- y[seq_len(n)]
  ct <- suppressWarnings(stats::cor.test(x, y, method = "spearman"))
  rho <- unname(ct$estimate)
  ci <- .fisher_z_ci(rho, n, confidence)
  .stat_result(
    method = "Spearman correlation",
    test_statistic = rho, p_value = ct$p.value, df = n - 2,
    ci_lower = ci[1], ci_upper = ci[2],
    effect_size = rho^2, estimate = rho, n = n
  )
}

#' Kendall's tau-b correlation
#' @inheritParams pearson_correlation
#' @export
kendall_correlation <- function(x, y) {
  x <- .stat_validate(x)
  y <- .stat_validate(y)
  n <- min(length(x), length(y))
  x <- x[seq_len(n)]
  y <- y[seq_len(n)]
  ct <- suppressWarnings(stats::cor.test(x, y, method = "kendall"))
  tau <- unname(ct$estimate)
  .stat_result(
    method = "Kendall tau-b",
    test_statistic = tau, p_value = ct$p.value,
    estimate = tau, n = n
  )
}

#' Point-biserial correlation
#' @param binary 0/1 vector.
#' @param continuous Numeric vector.
#' @param confidence Confidence level.
#' @export
point_biserial_correlation <- function(binary, continuous, confidence = 0.95) {
  b <- .stat_validate(binary)
  c <- .stat_validate(continuous)
  n <- min(length(b), length(c))
  b <- b[seq_len(n)]
  c <- c[seq_len(n)]
  if (length(unique(b)) != 2L)
    stop("Binary variable must have exactly 2 unique values.")
  ct <- stats::cor.test(b, c)
  r <- unname(ct$estimate)
  ci <- .fisher_z_ci(r, n, confidence)
  .stat_result(
    method = "Point-biserial correlation",
    test_statistic = r, p_value = ct$p.value, df = n - 2,
    ci_lower = ci[1], ci_upper = ci[2],
    effect_size = r^2, estimate = r, n = n
  )
}

#' Partial Pearson correlation controlling for covariates
#' @param x,y Numeric vectors of interest.
#' @param covariates Matrix or data frame of covariates.
#' @param confidence Confidence level.
#' @export
partial_correlation <- function(x, y, covariates, confidence = 0.95) {
  x <- .stat_validate(x)
  y <- .stat_validate(y)
  Z <- as.matrix(covariates)
  if (is.null(dim(Z))) Z <- matrix(Z, ncol = 1)
  n <- min(length(x), length(y), nrow(Z))
  x <- x[seq_len(n)]
  y <- y[seq_len(n)]
  Z <- Z[seq_len(n), , drop = FALSE]
  res_x <- stats::residuals(stats::lm.fit(cbind(1, Z), x))
  res_y <- stats::residuals(stats::lm.fit(cbind(1, Z), y))
  ct <- stats::cor.test(res_x, res_y)
  r <- unname(ct$estimate)
  df_val <- n - 2 - ncol(Z)
  z <- atanh(r)
  se <- if (df_val > 0) 1 / sqrt(df_val) else Inf
  zcrit <- stats::qnorm((1 + confidence) / 2)
  .stat_result(
    method = "Partial correlation",
    test_statistic = r, p_value = ct$p.value, df = df_val,
    ci_lower = tanh(z - zcrit * se),
    ci_upper = tanh(z + zcrit * se),
    effect_size = r^2, estimate = r, n = n
  )
}

#' Semi-partial (part) correlation
#' @inheritParams partial_correlation
#' @export
semi_partial_correlation <- function(x, y, covariates) {
  x <- .stat_validate(x)
  y <- .stat_validate(y)
  Z <- as.matrix(covariates)
  if (is.null(dim(Z))) Z <- matrix(Z, ncol = 1)
  n <- min(length(x), length(y), nrow(Z))
  x <- x[seq_len(n)]
  y <- y[seq_len(n)]
  Z <- Z[seq_len(n), , drop = FALSE]
  res_x <- stats::residuals(stats::lm.fit(cbind(1, Z), x))
  ct <- stats::cor.test(res_x, y)
  r <- unname(ct$estimate)
  .stat_result(
    method = "Semi-partial correlation",
    test_statistic = r, p_value = ct$p.value,
    effect_size = r^2, estimate = r, n = n
  )
}


# ===================================================================
# NON-PARAMETRIC TESTS
# ===================================================================

#' Mann-Whitney U / Wilcoxon rank-sum test
#' @param x,y Numeric vectors.
#' @param alternative One of "two.sided", "less", "greater".
#' @export
mann_whitney_u <- function(x, y, alternative = "two.sided") {
  alternative <- sub("-", ".", alternative, fixed = TRUE)
  x <- .stat_validate(x)
  y <- .stat_validate(y)
  wt <- suppressWarnings(stats::wilcox.test(x, y, alternative = alternative))
  u <- unname(wt$statistic)
  nx <- length(x)
  ny <- length(y)
  r_rb <- if (nx * ny > 0) 1 - 2 * u / (nx * ny) else 0
  .stat_result(
    method = "Mann-Whitney U test",
    test_statistic = u, p_value = wt$p.value,
    effect_size = r_rb, n = nx + ny,
    extra = list(rank_biserial = r_rb)
  )
}

#' Wilcoxon signed-rank test (one-sample or paired)
#' @param x Numeric vector.
#' @param y Optional paired vector.
#' @param alternative One of "two.sided", "less", "greater".
#' @export
wilcoxon_signed_rank <- function(x, y = NULL, alternative = "two.sided") {
  alternative <- sub("-", ".", alternative, fixed = TRUE)
  x <- .stat_validate(x)
  if (!is.null(y)) {
    y <- .stat_validate(y)
    if (length(x) != length(y)) stop("x and y must have equal length.")
    d <- x - y
  } else { d <- x }
  wt <- suppressWarnings(stats::wilcox.test(d, alternative = alternative))
  n <- length(d)
  z_approx <- stats::qnorm(wt$p.value / 2)
  r <- if (n > 0) abs(z_approx) / sqrt(n) else 0
  .stat_result(
    method = "Wilcoxon signed-rank test",
    test_statistic = unname(wt$statistic),
    p_value = wt$p.value, effect_size = r, n = n
  )
}

#' One-sample Kolmogorov-Smirnov test
#' @param x Numeric vector.
#' @param cdf Name of a CDF function (e.g. "pnorm", "pexp"). Defaults to
#'   "pnorm". A bare distribution name like "norm" is auto-prefixed.
#' @param args List of extra arguments to pass to \code{cdf}.
#' @export
ks_test_one_sample <- function(x, cdf = "pnorm", args = list()) {
  x <- .stat_validate(x)
  if (!startsWith(cdf, "p")) cdf <- paste0("p", cdf)
  kt <- suppressWarnings(do.call(stats::ks.test, c(list(x, cdf), args)))
  .stat_result(
    method = sprintf("KS test (1-sample, %s)", cdf),
    test_statistic = unname(kt$statistic),
    p_value = kt$p.value, n = length(x)
  )
}

#' Two-sample Kolmogorov-Smirnov test
#' @inheritParams pearson_correlation
#' @export
ks_test_two_sample <- function(x, y) {
  x <- .stat_validate(x)
  y <- .stat_validate(y)
  kt <- suppressWarnings(stats::ks.test(x, y))
  .stat_result(
    method = "KS test (2-sample)",
    test_statistic = unname(kt$statistic),
    p_value = kt$p.value, n = length(x) + length(y)
  )
}

#' Anderson-Darling test
#'
#' For \code{dist != "norm"} this is an API stub returning NA p-values;
#' Anderson-Darling for arbitrary distributions requires the
#' \pkg{ADGofTest} or \pkg{goftest} packages. For the normal case we
#' fall back on \code{nortest::ad.test} when available.
#'
#' @param x Numeric vector.
#' @param dist Distribution name.
#' @export
anderson_darling <- function(x, dist = "norm") {
  x <- .stat_validate(x)
  stat <- NA_real_
  p <- NA_real_
  if (dist == "norm" && requireNamespace("nortest", quietly = TRUE)) {
    res <- nortest::ad.test(x)
    stat <- unname(res$statistic)
    p <- res$p.value
  } else {
    # Compute statistic manually for normal; p approximate
    n <- length(x)
    z <- (sort(x) - mean(x)) / sd(x)
    i <- seq_len(n)
    A2 <- -n - mean((2 * i - 1) * (stats::pnorm(z, log.p = TRUE) +
        stats::pnorm(z[n + 1 - i], lower.tail = FALSE, log.p = TRUE)))
    stat <- A2
  }
  .stat_result(
    method = sprintf("Anderson-Darling test (%s)", dist),
    test_statistic = stat, p_value = p, n = length(x)
  )
}

#' Levene's test for equality of variances
#' @param ... Two or more numeric vectors.
#' @param center One of "median" (Brown-Forsythe), "mean", "trimmed".
#' @export
levene_test <- function(..., center = "median") {
  groups <- list(...)
  cleaned <- lapply(groups, .stat_validate)
  vals <- unlist(cleaned)
  grp <- factor(rep(seq_along(cleaned), lengths(cleaned)))
  centre_fn <- switch(center,
                      "median" = stats::median,
                      "mean" = mean,
                      "trimmed" = function(x) mean(x, trim = 0.1))
  centres <- tapply(vals, grp, centre_fn)
  z <- abs(vals - centres[as.integer(grp)])
  fit <- stats::aov(z ~ grp)
  s <- summary(fit)[[1L]]
  k <- length(cleaned)
  n_total <- length(vals)
  .stat_result(
    method = sprintf("Levene's test (center=%s)", center),
    test_statistic = s[["F value"]][1],
    p_value = s[["Pr(>F)"]][1],
    df = k - 1, n = n_total
  )
}

#' Bartlett's test for equality of variances
#' @param ... Two or more numeric vectors.
#' @export
bartlett_test <- function(...) {
  groups <- list(...)
  cleaned <- lapply(groups, .stat_validate)
  vals <- unlist(cleaned)
  grp <- factor(rep(seq_along(cleaned), lengths(cleaned)))
  bt <- stats::bartlett.test(vals, grp)
  .stat_result(
    method = "Bartlett's test",
    test_statistic = unname(bt$statistic),
    p_value = bt$p.value,
    df = unname(bt$parameter), n = length(vals)
  )
}

#' Wald-Wolfowitz runs test for randomness
#' @param x Numeric sequence.
#' @param cutoff Cut-off (median by default).
#' @export
runs_test <- function(x, cutoff = NULL) {
  x <- .stat_validate(x)
  n <- length(x)
  if (is.null(cutoff)) cutoff <- stats::median(x)
  binary <- as.integer(x >= cutoff)
  n1 <- sum(binary)
  n0 <- n - n1
  if (n1 == 0 || n0 == 0)
    return(.stat_result("Runs test", 0, 1, n = n))
  runs <- 1L + sum(diff(binary) != 0L)
  mu <- 1 + 2 * n0 * n1 / n
  v <- if (n > 1) 2 * n0 * n1 * (2 * n0 * n1 - n) / (n^2 * (n - 1)) else 0
  if (v <= 0) return(.stat_result("Runs test", runs, 1, n = n))
  z <- (runs - mu) / sqrt(v)
  p <- 2 * stats::pnorm(-abs(z))
  .stat_result(
    method = "Runs test",
    test_statistic = z, p_value = p, n = n,
    extra = list(n_runs = runs, expected_runs = mu)
  )
}


# ===================================================================
# NORMALITY TESTS
# ===================================================================

#' Shapiro-Wilk test for normality
#' @param x Numeric vector (n <= 5000).
#' @export
shapiro_wilk <- function(x) {
  x <- .stat_validate(x)
  sw <- stats::shapiro.test(x)
  .stat_result(
    method = "Shapiro-Wilk test",
    test_statistic = unname(sw$statistic),
    p_value = sw$p.value, n = length(x)
  )
}

#' D'Agostino-Pearson omnibus normality test
#'
#' API stub: implemented via the K2 statistic = Z(skew)^2 + Z(kurt)^2
#' (D'Agostino & Pearson 1973). Recommended n >= 20.
#'
#' @param x Numeric vector.
#' @export
dagostino_pearson <- function(x) {
  x <- .stat_validate(x)
  n <- length(x)
  if (n < 8L)
    return(.stat_result("D'Agostino-Pearson test", NA, NA, df = 2, n = n))
  # Skewness Z (D'Agostino 1970)
  g1 <- mean((x - mean(x))^3) / (sd(x))^3
  Y <- g1 * sqrt((n + 1) * (n + 3) / (6 * (n - 2)))
  beta2 <- 3 * (n^2 + 27 * n - 70) * (n + 1) * (n + 3) /
    ((n - 2) * (n + 5) * (n + 7) * (n + 9))
  W2 <- -1 + sqrt(2 * (beta2 - 1))
  delta <- 1 / sqrt(log(sqrt(W2)))
  alpha <- sqrt(2 / (W2 - 1))
  Z1 <- delta * log(Y / alpha + sqrt((Y / alpha)^2 + 1))
  # Kurtosis Z
  g2 <- mean((x - mean(x))^4) / (sd(x))^4 - 3
  E_g2 <- -6 / (n + 1)
  var_g2 <- 24 * n * (n - 2) * (n - 3) / ((n + 1)^2 * (n + 3) * (n + 5))
  x_g2 <- (g2 - E_g2) / sqrt(var_g2)
  sqrt_b1_g2 <- 6 * (n^2 - 5 * n + 2) / ((n + 7) * (n + 9)) *
    sqrt(6 * (n + 3) * (n + 5) / (n * (n - 2) * (n - 3)))
  A <- 6 + 8 / sqrt_b1_g2 *
    (2 / sqrt_b1_g2 + sqrt(1 + 4 / sqrt_b1_g2^2))
  Z2 <- ((1 - 2 / (9 * A)) -
           ((1 - 2 / A) / (1 + x_g2 * sqrt(2 / (A - 4))))^(1 / 3)) /
    sqrt(2 / (9 * A))
  K2 <- Z1^2 + Z2^2
  p <- 1 - stats::pchisq(K2, df = 2)
  .stat_result(
    method = "D'Agostino-Pearson test",
    test_statistic = K2, p_value = p, df = 2, n = n
  )
}

#' Jarque-Bera test for normality
#' @param x Numeric vector.
#' @export
jarque_bera <- function(x) {
  x <- .stat_validate(x)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  if (s == 0)
    return(.stat_result("Jarque-Bera test", 0, 1, df = 2, n = n))
  skew <- mean((x - m)^3) / s^3
  kurt <- mean((x - m)^4) / s^4 - 3
  jb <- n / 6 * (skew^2 + kurt^2 / 4)
  p <- 1 - stats::pchisq(jb, df = 2)
  .stat_result(
    method = "Jarque-Bera test",
    test_statistic = jb, p_value = p, df = 2, n = n
  )
}

#' Lilliefors test for normality
#'
#' Uses \pkg{nortest::lillie.test} when available; otherwise falls back
#' to a plain KS test with estimated parameters (p-value approximate).
#'
#' @param x Numeric vector.
#' @export
lilliefors_test <- function(x) {
  x <- .stat_validate(x)
  if (requireNamespace("nortest", quietly = TRUE)) {
    res <- nortest::lillie.test(x)
    stat <- unname(res$statistic)
    p <- res$p.value
  } else {
    kt <- suppressWarnings(stats::ks.test(x, "pnorm", mean(x), sd(x)))
    stat <- unname(kt$statistic)
    p <- kt$p.value
    warning("nortest not available; Lilliefors p-value approximate (plain KS).")
  }
  .stat_result(
    method = "Lilliefors test",
    test_statistic = stat, p_value = p, n = length(x)
  )
}


# ===================================================================
# PROPORTION TESTS
# ===================================================================

#' One-sample z-test for a proportion
#' @param count Successes.
#' @param nobs Total observations.
#' @param value Hypothesised proportion.
#' @param confidence Confidence level (Wilson CI).
#' @export
one_proportion_ztest <- function(count, nobs, value = 0.5, confidence = 0.95) {
  p_hat <- if (nobs > 0) count / nobs else 0
  se <- if (nobs > 0) sqrt(value * (1 - value) / nobs) else 0
  z <- if (se > 0) (p_hat - value) / se else 0
  p_val <- 2 * stats::pnorm(-abs(z))
  zcrit <- stats::qnorm((1 + confidence) / 2)
  denom <- 1 + zcrit^2 / nobs
  centre <- (p_hat + zcrit^2 / (2 * nobs)) / denom
  margin <- zcrit * sqrt(p_hat * (1 - p_hat) / nobs +
                          zcrit^2 / (4 * nobs^2)) / denom
  .stat_result(
    method = "One-proportion z-test",
    test_statistic = z, p_value = p_val,
    ci_lower = centre - margin, ci_upper = centre + margin,
    estimate = p_hat, n = nobs
  )
}

#' Two-sample z-test for the difference in proportions
#' @param count1,nobs1 First sample.
#' @param count2,nobs2 Second sample.
#' @param confidence Confidence level.
#' @export
two_proportion_ztest <- function(count1, nobs1, count2, nobs2,
                                  confidence = 0.95) {
  p1 <- if (nobs1 > 0) count1 / nobs1 else 0
  p2 <- if (nobs2 > 0) count2 / nobs2 else 0
  p_pool <- if ((nobs1 + nobs2) > 0)
    (count1 + count2) / (nobs1 + nobs2) else 0
  se <- if ((nobs1 + nobs2) > 0)
    sqrt(p_pool * (1 - p_pool) * (1 / nobs1 + 1 / nobs2)) else 0
  z <- if (se > 0) (p1 - p2) / se else 0
  p_val <- 2 * stats::pnorm(-abs(z))
  zcrit <- stats::qnorm((1 + confidence) / 2)
  se_d <- if (nobs1 > 0 && nobs2 > 0)
    sqrt(p1 * (1 - p1) / nobs1 + p2 * (1 - p2) / nobs2) else 0
  diff <- p1 - p2
  .stat_result(
    method = "Two-proportion z-test",
    test_statistic = z, p_value = p_val,
    ci_lower = diff - zcrit * se_d,
    ci_upper = diff + zcrit * se_d,
    estimate = diff, n = nobs1 + nobs2
  )
}

#' Fisher's exact test for a 2x2 table
#' @param contingency_table 2x2 matrix.
#' @param alternative One of "two.sided", "less", "greater".
#' @export
fisher_exact_test <- function(contingency_table, alternative = "two.sided") {
  alternative <- sub("-", ".", alternative, fixed = TRUE)
  tab <- as.matrix(contingency_table)
  if (!all(dim(tab) == c(2, 2)))
    stop("Fisher exact test requires a 2x2 table.")
  ft <- stats::fisher.test(tab, alternative = alternative)
  or_val <- unname(ft$estimate)
  .stat_result(
    method = "Fisher's exact test",
    test_statistic = or_val, p_value = ft$p.value,
    estimate = or_val, n = as.integer(sum(tab))
  )
}


# ===================================================================
# AGREEMENT
# ===================================================================

#' Cohen's kappa for two raters
#' @param rater1,rater2 Equal-length categorical vectors.
#' @param confidence Confidence level.
#' @export
cohens_kappa <- function(rater1, rater2, confidence = 0.95) {
  r1 <- as.vector(rater1)
  r2 <- as.vector(rater2)
  if (length(r1) != length(r2))
    stop("Raters must have the same number of observations.")
  n <- length(r1)
  cats <- sort(unique(c(r1, r2)))
  k <- length(cats)
  mat <- matrix(0, k, k)
  idx <- function(v) match(v, cats)
  for (i in seq_along(r1)) mat[idx(r1[i]), idx(r2[i])] <-
    mat[idx(r1[i]), idx(r2[i])] + 1
  p_o <- sum(diag(mat)) / n
  rs <- rowSums(mat) / n
  cs <- colSums(mat) / n
  p_e <- sum(rs * cs)
  kap <- if ((1 - p_e) > 0) (p_o - p_e) / (1 - p_e) else 0
  se <- if ((1 - p_e) > 0 && n > 0) sqrt(p_e / (n * (1 - p_e)^2)) else 0
  zcrit <- stats::qnorm((1 + confidence) / 2)
  z_stat <- if (se > 0) kap / se else 0
  p_val <- if (se > 0) 2 * stats::pnorm(-abs(z_stat)) else 1
  .stat_result(
    method = "Cohen's kappa",
    test_statistic = z_stat, p_value = p_val,
    ci_lower = kap - zcrit * se,
    ci_upper = kap + zcrit * se,
    effect_size = kap, estimate = kap, n = n
  )
}

#' Fleiss' kappa for multiple raters
#' @param ratings_matrix Matrix; rows = subjects, cols = categories,
#'   cells = number of raters assigning subject i to category j.
#' @export
fleiss_kappa <- function(ratings_matrix) {
  tab <- as.matrix(ratings_matrix)
  n <- nrow(tab)
  k <- ncol(tab)
  N_raters <- sum(tab[1, ])
  p_j <- colSums(tab) / (n * N_raters)
  P_i <- (rowSums(tab^2) - N_raters) / (N_raters * (N_raters - 1))
  P_bar <- mean(P_i)
  P_e <- sum(p_j^2)
  kap <- if ((1 - P_e) > 0) (P_bar - P_e) / (1 - P_e) else 0
  se_num <- 2 / (n * N_raters * (N_raters - 1))
  se_term <- sum(p_j * (1 - p_j))^2
  denom <- (1 - P_e)^2
  se <- if (denom > 0) sqrt(se_num * (se_term / denom)) else 0
  z <- if (se > 0) kap / se else 0
  p_val <- 2 * stats::pnorm(-abs(z))
  .stat_result(
    method = "Fleiss' kappa",
    test_statistic = z, p_value = p_val,
    effect_size = kap, estimate = kap, n = n,
    extra = list(n_raters = N_raters, n_categories = k)
  )
}

#' Intraclass correlation coefficient (Shrout & Fleiss 1979)
#' @param data Long-format data frame.
#' @param targets Subject ID column.
#' @param raters Rater ID column.
#' @param ratings Numeric rating column.
#' @param icc_type One of "ICC1", "ICC1k", "ICC2", "ICC2k", "ICC3", "ICC3k".
#' @export
intraclass_correlation <- function(data, targets, raters, ratings,
                                    icc_type = "ICC3k") {
  df <- stats::na.omit(data[, c(targets, raters, ratings)])
  wide <- stats::reshape(df, idvar = targets, timevar = raters,
                         direction = "wide", v.names = ratings)
  wide <- stats::na.omit(wide)
  Y <- as.matrix(wide[, grep(paste0("^", ratings, "\\."), names(wide))])
  n <- nrow(Y)
  k <- ncol(Y)
  gm <- mean(Y)
  rm_ <- rowMeans(Y)
  cm <- colMeans(Y)
  ss_total <- sum((Y - gm)^2)
  ss_rows <- k * sum((rm_ - gm)^2)
  ss_cols <- n * sum((cm - gm)^2)
  ss_error <- ss_total - ss_rows - ss_cols
  ms_rows <- if (n > 1) ss_rows / (n - 1) else 0
  ms_cols <- if (k > 1) ss_cols / (k - 1) else 0
  ms_error <- if ((n - 1) * (k - 1) > 0)
    ss_error / ((n - 1) * (k - 1)) else 0
  ms_within <- if (n * (k - 1) > 0)
    (ss_cols + ss_error) / (n * (k - 1)) else 0
  icc <- switch(icc_type,
    "ICC1"  = (ms_rows - ms_within) / (ms_rows + (k - 1) * ms_within),
    "ICC1k" = if (ms_rows > 0) (ms_rows - ms_within) / ms_rows else 0,
    "ICC2"  = (ms_rows - ms_error) /
               (ms_rows + (k - 1) * ms_error + k * (ms_cols - ms_error) / n),
    "ICC2k" = (ms_rows - ms_error) /
               (ms_rows + (ms_cols - ms_error) / n),
    "ICC3"  = (ms_rows - ms_error) / (ms_rows + (k - 1) * ms_error),
    "ICC3k" = if (ms_rows > 0) (ms_rows - ms_error) / ms_rows else 0,
    stop(sprintf("Unknown ICC type: %s", icc_type)))
  f_stat <- if (ms_error > 0) ms_rows / ms_error else 0
  df1 <- n - 1
  df2 <- (n - 1) * (k - 1)
  p_val <- 1 - stats::pf(f_stat, df1, df2)
  .stat_result(
    method = sprintf("Intraclass correlation (%s)", icc_type),
    test_statistic = f_stat, p_value = p_val, df = df1,
    effect_size = icc, estimate = icc, n = n,
    extra = list(icc_type = icc_type, n_raters = k,
                 ms_rows = ms_rows, ms_error = ms_error)
  )
}


# ===================================================================
# CONVENIENCE / SUITES
# ===================================================================

#' Run a suite of normality tests
#' @param x Numeric vector.
#' @return A list of \code{morie_test_result}.
#' @export
normality_suite <- function(x) {
  x <- .stat_validate(x)
  out <- list()
  if (length(x) >= 3L && length(x) <= 5000L)
    out <- c(out, list(shapiro_wilk(x)))
  if (length(x) >= 20L) out <- c(out, list(dagostino_pearson(x)))
  out <- c(out, list(jarque_bera(x)), list(lilliefors_test(x)))
  out
}

#' Run a suite of homogeneity-of-variance tests
#' @param ... Two or more numeric vectors.
#' @export
variance_equality_suite <- function(...) {
  list(levene_test(..., center = "median"),
       bartlett_test(...))
}

#' Pairwise correlation matrix with p-values
#' @param data Data frame; numeric columns are used.
#' @param method One of "pearson", "spearman", "kendall".
#' @return List with components \code{r} (correlations) and \code{p}
#'   (p-values), both \code{data.frame} objects with matching dimensions.
#' @export
correlation_matrix <- function(data, method = "pearson") {
  num <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
  cols <- names(num)
  n <- length(cols)
  r_mat <- matrix(0, n, n, dimnames = list(cols, cols))
  p_mat <- matrix(0, n, n, dimnames = list(cols, cols))
  for (i in seq_len(n)) for (j in seq(i, n)) {
    if (i == j) { r_mat[i, j] <- 1
    p_mat[i, j] <- 0
    next }
    valid <- stats::complete.cases(num[, c(i, j)])
    ct <- suppressWarnings(stats::cor.test(
      num[valid, i], num[valid, j], method = method))
    r_mat[i, j] <- r_mat[j, i] <- unname(ct$estimate)
    p_mat[i, j] <- p_mat[j, i] <- ct$p.value
  }
  list(r = as.data.frame(r_mat), p = as.data.frame(p_mat))
}

#' Automatic test selection
#'
#' Decision logic:
#' \enumerate{
#'   \item If \code{y} is NULL, one-sample t-test against zero.
#'   \item If \code{paired = TRUE}, paired t-test if differences are
#'     normal, otherwise Wilcoxon signed-rank.
#'   \item If two independent samples, check both-normal (Shapiro-Wilk
#'     for n<=5000, otherwise D'Agostino-Pearson); if both normal, run
#'     Student's or Welch's t depending on Levene's test; otherwise
#'     Mann-Whitney U.
#' }
#' @param x Numeric vector.
#' @param y Optional second sample.
#' @param paired Whether samples are paired.
#' @param confidence Confidence level.
#' @export
auto_test <- function(x, y = NULL, paired = FALSE, confidence = 0.95) {
  x <- .stat_validate(x)
  if (is.null(y)) return(one_sample_ttest(x, mu0 = 0, confidence = confidence))
  y <- .stat_validate(y)
  if (paired) {
    if (length(x) != length(y))
      stop("Paired comparison requires equal-length arrays.")
    sw <- stats::shapiro.test(x - y)
    if (sw$p.value >= 0.05)
      return(paired_ttest(x, y, confidence = confidence))
    return(wilcoxon_signed_rank(x, y))
  }
  sw_x <- if (length(x) <= 5000L)
    stats::shapiro.test(x)$p.value else dagostino_pearson(x)$p_value
  sw_y <- if (length(y) <= 5000L)
    stats::shapiro.test(y)$p.value else dagostino_pearson(y)$p_value
  if (sw_x >= 0.05 && sw_y >= 0.05) {
    lev <- levene_test(x, y, center = "median")
    eq_var <- lev$p_value >= 0.05
    return(two_sample_ttest(x, y, equal_var = eq_var, confidence = confidence))
  }
  mann_whitney_u(x, y)
}
