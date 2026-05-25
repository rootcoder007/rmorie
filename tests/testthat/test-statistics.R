# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Coverage tests for R/statistics.R — t-tests, ANOVA, chi-squared, correlation,
# non-parametric, normality, proportions, agreement, suites, auto-test.
# ---------------------------------------------------------------------------

set.seed(1)

make_two_samples <- function(n = 60, seed = 1) {
  set.seed(seed)
  list(x = rnorm(n, mean = 0, sd = 1),
       y = rnorm(n, mean = 0.4, sd = 1))
}

# ---------------------------------------------------------------------------
# t-tests
# ---------------------------------------------------------------------------

test_that("one_sample_ttest returns morie_test_result fields", {
  set.seed(1)
  x <- rnorm(50, mean = 0.5)
  res <- one_sample_ttest(x, mu0 = 0)
  expect_s3_class(res, "morie_test_result")
  expect_true(all(c("method", "test_statistic", "p_value",
                    "ci_lower", "ci_upper", "n") %in% names(res)))
  expect_equal(res$n, 50L)
  expect_true(is.finite(res$test_statistic))
})

test_that("one_sample_ttest CI brackets the mean", {
  set.seed(1)
  x <- rnorm(80, mean = 2, sd = 1)
  res <- one_sample_ttest(x, mu0 = 0)
  expect_lt(res$ci_lower, mean(x))
  expect_gt(res$ci_upper, mean(x))
})

test_that("two_sample_ttest detects mean shift", {
  set.seed(1); s <- make_two_samples(200)
  res <- two_sample_ttest(s$x, s$y, equal_var = TRUE)
  expect_s3_class(res, "morie_test_result")
  expect_lt(res$p_value, 0.05)
})

test_that("welch_ttest agrees roughly with two_sample equal_var=FALSE", {
  set.seed(1); s <- make_two_samples(80)
  a <- welch_ttest(s$x, s$y)
  b <- two_sample_ttest(s$x, s$y, equal_var = FALSE)
  expect_equal(a$test_statistic, b$test_statistic, tolerance = 1e-6)
})

test_that("paired_ttest requires equal-length input", {
  set.seed(1)
  expect_error(paired_ttest(rnorm(10), rnorm(11)))
})

test_that("paired_ttest returns finite p", {
  set.seed(1)
  x <- rnorm(40); y <- x + rnorm(40, sd = 0.5)
  res <- paired_ttest(x, y)
  expect_true(is.finite(res$p_value))
})

# ---------------------------------------------------------------------------
# ANOVA family
# ---------------------------------------------------------------------------

test_that("one_way_anova needs at least 2 groups", {
  expect_error(one_way_anova(rnorm(10)))
})

test_that("one_way_anova runs on 3 groups", {
  set.seed(1)
  res <- one_way_anova(rnorm(30), rnorm(30, mean = 0.5), rnorm(30, mean = 1))
  expect_s3_class(res, "morie_test_result")
  expect_true(is.finite(res$test_statistic))
})

test_that("two_way_anova returns interaction-row F", {
  set.seed(1)
  d <- data.frame(
    y = rnorm(120),
    a = rep(letters[1:3], each = 40),
    b = rep(letters[4:5], 60))
  res <- two_way_anova(d, "y", "a", "b")
  expect_s3_class(res, "morie_test_result")
  expect_equal(res$n, 120L)
})

test_that("repeated_measures_anova rejects k<2 within levels", {
  set.seed(1)
  d <- data.frame(y = rnorm(10), s = 1:10, w = rep("only", 10))
  expect_error(repeated_measures_anova(d, "y", "s", "w"))
})

test_that("repeated_measures_anova runs on long-format synthetic", {
  set.seed(1)
  d <- data.frame(
    y = c(rnorm(20), rnorm(20, mean = 0.3), rnorm(20, mean = 0.5)),
    s = rep(1:20, 3),
    w = rep(c("t1", "t2", "t3"), each = 20))
  res <- repeated_measures_anova(d, "y", "s", "w")
  expect_true(is.finite(res$test_statistic))
  expect_equal(res$df, 2)
})

test_that("kruskal_wallis returns Kruskal-Wallis test result", {
  set.seed(1)
  res <- kruskal_wallis(rnorm(20), rnorm(20, mean = 1), rnorm(20, mean = 2))
  expect_s3_class(res, "morie_test_result")
  expect_equal(res$df, 2)
})

test_that("friedman_test rejects unequal lengths", {
  expect_error(friedman_test(1:5, 1:6, 1:5))
})

test_that("friedman_test produces a finite W effect size", {
  set.seed(1)
  res <- friedman_test(rnorm(15), rnorm(15) + 0.5, rnorm(15) + 1)
  expect_s3_class(res, "morie_test_result")
  expect_true(is.finite(res$effect_size))
})

# ---------------------------------------------------------------------------
# Chi-squared family
# ---------------------------------------------------------------------------

test_that("chi2_goodness_of_fit defaults to uniform expected", {
  res <- chi2_goodness_of_fit(c(20, 30, 25, 25))
  expect_s3_class(res, "morie_test_result")
  expect_equal(res$df, 3)
})

test_that("chi2_goodness_of_fit accepts user-supplied expected", {
  res <- chi2_goodness_of_fit(c(20, 30, 25, 25), c(25, 25, 25, 25))
  expect_true(is.finite(res$p_value))
})

test_that("chi2_independence returns Cramer's V", {
  tab <- matrix(c(30, 20, 10, 40), 2, 2)
  res <- chi2_independence(tab)
  expect_true("cramers_v" %in% names(res$extra))
})

test_that("mcnemar_test rejects non-2x2 input", {
  expect_error(mcnemar_test(matrix(1, 3, 3)))
})

test_that("mcnemar_test exact branch works", {
  tab <- matrix(c(20, 10, 5, 30), 2, 2)
  res <- mcnemar_test(tab, exact = TRUE)
  expect_true(is.finite(res$p_value))
})

test_that("cochrans_q runs on 3 binary vectors", {
  set.seed(1)
  v1 <- rbinom(30, 1, 0.4); v2 <- rbinom(30, 1, 0.5); v3 <- rbinom(30, 1, 0.6)
  res <- cochrans_q(v1, v2, v3)
  expect_s3_class(res, "morie_test_result")
  expect_equal(res$df, 2)
})

test_that("cochrans_q rejects unequal lengths", {
  expect_error(cochrans_q(c(1, 0), c(1, 0, 1)))
})

# ---------------------------------------------------------------------------
# Correlation
# ---------------------------------------------------------------------------

test_that("pearson_correlation recovers strong r ~ 0.9", {
  set.seed(1)
  x <- rnorm(200); y <- 0.9 * x + 0.3 * rnorm(200)
  res <- pearson_correlation(x, y)
  expect_gt(res$estimate, 0.8)
  expect_lt(res$ci_lower, res$ci_upper)
})

test_that("spearman_correlation runs and returns rho", {
  set.seed(1)
  x <- runif(80); y <- x + runif(80, -0.1, 0.1)
  res <- spearman_correlation(x, y)
  expect_gt(res$estimate, 0.5)
})

test_that("kendall_correlation runs without error", {
  set.seed(1)
  res <- kendall_correlation(rnorm(40), rnorm(40))
  expect_s3_class(res, "morie_test_result")
})

test_that("point_biserial_correlation rejects non-binary", {
  expect_error(point_biserial_correlation(1:10, 1:10))
})

test_that("point_biserial_correlation runs on synthetic binary/continuous", {
  set.seed(1)
  b <- rbinom(60, 1, 0.5); c <- rnorm(60) + b
  res <- point_biserial_correlation(b, c)
  expect_true(is.finite(res$estimate))
})

test_that("partial_correlation handles single covariate vector", {
  set.seed(1)
  x <- rnorm(80); y <- 0.5 * x + rnorm(80); z <- rnorm(80)
  res <- partial_correlation(x, y, z)
  expect_true(is.finite(res$estimate))
})

test_that("semi_partial_correlation runs", {
  set.seed(1)
  x <- rnorm(60); y <- rnorm(60); z <- rnorm(60)
  res <- semi_partial_correlation(x, y, z)
  expect_s3_class(res, "morie_test_result")
})

# ---------------------------------------------------------------------------
# Non-parametric
# ---------------------------------------------------------------------------

test_that("mann_whitney_u runs and reports rank biserial", {
  set.seed(1); s <- make_two_samples(60)
  res <- mann_whitney_u(s$x, s$y)
  expect_true("rank_biserial" %in% names(res$extra))
})

test_that("wilcoxon_signed_rank one-sample and paired forms run", {
  set.seed(1)
  a <- wilcoxon_signed_rank(rnorm(30))
  b <- wilcoxon_signed_rank(rnorm(30), rnorm(30))
  expect_s3_class(a, "morie_test_result")
  expect_s3_class(b, "morie_test_result")
})

test_that("wilcoxon_signed_rank paired rejects unequal lengths", {
  expect_error(wilcoxon_signed_rank(1:5, 1:6))
})

test_that("ks_test_one_sample handles cdf-name and prefixed forms", {
  set.seed(1)
  a <- ks_test_one_sample(rnorm(40), cdf = "norm")
  b <- ks_test_one_sample(rnorm(40), cdf = "pnorm")
  expect_s3_class(a, "morie_test_result")
  expect_s3_class(b, "morie_test_result")
})

test_that("ks_test_two_sample runs", {
  set.seed(1)
  res <- ks_test_two_sample(rnorm(40), rnorm(40, mean = 0.5))
  expect_true(is.finite(res$test_statistic))
})

test_that("anderson_darling normal-branch returns finite statistic", {
  set.seed(1)
  res <- anderson_darling(rnorm(60))
  expect_true(is.finite(res$test_statistic))
})

test_that("levene_test variants 'mean' / 'median' / 'trimmed'", {
  set.seed(1)
  for (c in c("mean", "median", "trimmed")) {
    res <- levene_test(rnorm(40), rnorm(40, sd = 2), center = c)
    expect_s3_class(res, "morie_test_result")
  }
})

test_that("bartlett_test runs on 2 groups", {
  set.seed(1)
  res <- bartlett_test(rnorm(30), rnorm(30, sd = 2))
  expect_s3_class(res, "morie_test_result")
})

test_that("runs_test handles a constant-binary input gracefully", {
  res <- runs_test(rep(1, 20))
  expect_equal(res$p_value, 1)
})

test_that("runs_test runs on numeric input with default cutoff", {
  set.seed(1)
  res <- runs_test(rnorm(80))
  expect_true(is.finite(res$test_statistic))
})

# ---------------------------------------------------------------------------
# Normality
# ---------------------------------------------------------------------------

test_that("shapiro_wilk runs on n <= 5000", {
  set.seed(1)
  res <- shapiro_wilk(rnorm(100))
  expect_s3_class(res, "morie_test_result")
})

test_that("dagostino_pearson degrades when n < 8", {
  res <- dagostino_pearson(rnorm(5))
  expect_true(is.na(res$test_statistic) || is.na(res$p_value))
})

test_that("dagostino_pearson runs on n >= 20", {
  set.seed(1)
  res <- dagostino_pearson(rnorm(60))
  expect_true(is.finite(res$test_statistic))
})

test_that("jarque_bera degenerate input has p=1", {
  res <- jarque_bera(rep(5, 30))
  expect_equal(res$p_value, 1)
})

test_that("jarque_bera detects highly non-normal", {
  set.seed(1)
  res <- jarque_bera(rexp(200))
  expect_true(is.finite(res$test_statistic))
})

test_that("lilliefors_test runs in either branch", {
  set.seed(1)
  res <- suppressWarnings(lilliefors_test(rnorm(80)))
  expect_s3_class(res, "morie_test_result")
})

# ---------------------------------------------------------------------------
# Proportions
# ---------------------------------------------------------------------------

test_that("one_proportion_ztest reports Wilson CI", {
  res <- one_proportion_ztest(60, 100, value = 0.5)
  expect_lt(res$ci_lower, res$estimate)
  expect_gt(res$ci_upper, res$estimate)
})

test_that("two_proportion_ztest runs and returns difference", {
  res <- two_proportion_ztest(40, 100, 25, 100)
  expect_equal(res$estimate, 0.15, tolerance = 1e-10)
})

test_that("fisher_exact_test rejects non-2x2", {
  expect_error(fisher_exact_test(matrix(1, 3, 3)))
})

test_that("fisher_exact_test runs on 2x2", {
  res <- fisher_exact_test(matrix(c(10, 5, 3, 12), 2, 2))
  expect_s3_class(res, "morie_test_result")
})

# ---------------------------------------------------------------------------
# Agreement
# ---------------------------------------------------------------------------

test_that("cohens_kappa rejects unequal-length raters", {
  expect_error(cohens_kappa(1:5, 1:6))
})

test_that("cohens_kappa returns kappa estimate", {
  set.seed(1)
  r1 <- sample(c("a", "b", "c"), 60, replace = TRUE)
  r2 <- ifelse(runif(60) > 0.2, r1, sample(c("a", "b", "c"), 60, replace = TRUE))
  res <- cohens_kappa(r1, r2)
  expect_true(is.finite(res$estimate))
})

test_that("fleiss_kappa runs on a ratings matrix", {
  set.seed(1)
  mat <- matrix(0, nrow = 20, ncol = 3)
  for (i in seq_len(20)) {
    pick <- sample.int(3, 5, replace = TRUE)
    for (k in pick) mat[i, k] <- mat[i, k] + 1
  }
  res <- fleiss_kappa(mat)
  expect_s3_class(res, "morie_test_result")
})

test_that("intraclass_correlation supports several types", {
  set.seed(1)
  d <- data.frame(
    s = rep(1:20, each = 3),
    r = rep(1:3, 20),
    v = rep(rnorm(20), each = 3) + rnorm(60, sd = 0.4))
  for (t in c("ICC1", "ICC2", "ICC3", "ICC1k", "ICC2k", "ICC3k")) {
    res <- intraclass_correlation(d, "s", "r", "v", icc_type = t)
    expect_s3_class(res, "morie_test_result")
  }
})

test_that("intraclass_correlation errors on unknown type", {
  set.seed(1)
  d <- data.frame(s = rep(1:5, each = 2), r = rep(1:2, 5), v = rnorm(10))
  expect_error(intraclass_correlation(d, "s", "r", "v", icc_type = "FOO"))
})

# ---------------------------------------------------------------------------
# Suites + dispatch
# ---------------------------------------------------------------------------

test_that("normality_suite returns a list of test results", {
  set.seed(1)
  out <- normality_suite(rnorm(80))
  expect_true(length(out) >= 3L)
  expect_s3_class(out[[1]], "morie_test_result")
})

test_that("variance_equality_suite returns Levene + Bartlett", {
  set.seed(1)
  out <- variance_equality_suite(rnorm(40), rnorm(40, sd = 2))
  expect_length(out, 2L)
})

test_that("correlation_matrix returns r and p tables", {
  set.seed(1)
  d <- data.frame(a = rnorm(40), b = rnorm(40), c = rnorm(40))
  cm <- correlation_matrix(d)
  expect_true(all(c("r", "p") %in% names(cm)))
  expect_equal(dim(cm$r), c(3, 3))
})

test_that("auto_test routes to one_sample when y is NULL", {
  set.seed(1)
  res <- morie::auto_test(rnorm(40))
  expect_match(res$method, "One.sample", ignore.case = TRUE)
})

test_that("auto_test paired requires equal-length input", {
  set.seed(1)
  expect_error(morie::auto_test(rnorm(10), rnorm(11), paired = TRUE))
})

test_that("auto_test paired routes to a paired test", {
  set.seed(1)
  x <- rnorm(40); y <- x + rnorm(40, sd = 0.5)
  res <- morie::auto_test(x, y, paired = TRUE)
  expect_true(grepl("Paired|Wilcoxon", res$method))
})

test_that("auto_test unpaired routes to either t-test or Mann-Whitney", {
  set.seed(1); s <- make_two_samples(80)
  res <- morie::auto_test(s$x, s$y)
  expect_true(is.finite(res$p_value))
})

test_that("print.morie_test_result emits the method header", {
  set.seed(1)
  res <- one_sample_ttest(rnorm(30))
  expect_output(print(res), res$method, fixed = TRUE)
})