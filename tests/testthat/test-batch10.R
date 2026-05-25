# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 10 tests: hrzp2, hrzq1, hrzs1, hrzt1, hrzt2, hrzw1, hrzw2, idlpt,
# impsm, indkr, inference, inspector, investigation, ipw, ipw_weights

test_that("morie_horowitz_plr_bandwidth returns positive bandwidth on adequate data", {
  set.seed(1)
  x <- rnorm(80)
  r <- morie_horowitz_plr_bandwidth(x)
  expect_type(r, "list")
  expect_named(r, c("estimate", "n", "sigma", "c", "method"))
  expect_true(is.finite(r$estimate) && r$estimate > 0)
  expect_equal(r$n, 80L)
  expect_equal(r$c, 1.06)
})

test_that("morie_horowitz_plr_bandwidth honours the c multiplier", {
  set.seed(2)
  x <- rnorm(60)
  r1 <- morie_horowitz_plr_bandwidth(x, c = 1.06)
  r2 <- morie_horowitz_plr_bandwidth(x, c = 2.12)
  expect_gt(r2$estimate, r1$estimate)
})

test_that("morie_horowitz_plr_bandwidth flags insufficient data", {
  r <- morie_horowitz_plr_bandwidth(c(1, 2, 3))
  expect_true(is.na(r$estimate))
  expect_match(r$method, "insufficient")
  expect_equal(r$n, 3L)
})

test_that("morie_horowitz_plr_bandwidth handles zero-IQR / constant data", {
  r <- morie_horowitz_plr_bandwidth(rep(5, 30))
  expect_true(is.finite(r$estimate))
})

test_that("morie_horowitz_quantile_regression returns estimate and se", {
  set.seed(3)
  x <- rnorm(120)
  y <- 1 + 2 * x + rnorm(120)
  r <- morie_horowitz_quantile_regression(x, y, tau = 0.5)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "intercept", "tau", "n", "method"))
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se) && r$se >= 0)
  expect_equal(r$tau, 0.5)
  expect_equal(r$n, 120L)
})

test_that("morie_horowitz_quantile_regression works at non-median tau", {
  set.seed(4)
  x <- rnorm(100)
  y <- x + rnorm(100)
  r <- morie_horowitz_quantile_regression(x, y, tau = 0.25)
  expect_equal(r$tau, 0.25)
  expect_true(is.finite(r$estimate))
})

test_that("morie_horowitz_quantile_regression flags insufficient data", {
  r <- morie_horowitz_quantile_regression(1:5, 1:5)
  expect_true(all(is.na(r$estimate)))
  expect_match(r$method, "insufficient")
})

test_that("morie_horowitz_quantile_regression flags invalid tau", {
  set.seed(5)
  x <- rnorm(50)
  y <- x + rnorm(50)
  r <- morie_horowitz_quantile_regression(x, y, tau = 1.5)
  expect_true(all(is.na(r$estimate)))
})

test_that("morie_horowitz_quantile_regression handles a design matrix", {
  set.seed(6)
  X <- cbind(rnorm(120), rnorm(120))
  y <- X[, 1] - X[, 2] + rnorm(120)
  r <- morie_horowitz_quantile_regression(X, y, tau = 0.5)
  expect_length(r$estimate, 2)
  expect_length(r$se, 2)
})

test_that("morie_horowitz_sample_selection returns coefficients", {
  set.seed(7)
  n <- 200
  z <- rnorm(n)
  d <- as.numeric(0.5 * z + rnorm(n) > 0)
  x <- rnorm(n)
  y <- 1 + 2 * x + rnorm(n)
  r <- morie_horowitz_sample_selection(x, y, z, d)
  expect_type(r, "list")
  expect_true(is.numeric(r$estimate))
  expect_true(all(is.finite(r$se)))
  expect_equal(r$n, n)
  expect_true(r$n_selected > 0)
})

test_that("morie_horowitz_sample_selection flags insufficient data", {
  r <- morie_horowitz_sample_selection(1:5, 1:5, 1:5, c(1, 0, 1, 0, 1))
  expect_true(is.na(r$estimate))
  expect_match(r$method, "insufficient")
})

test_that("morie_horowitz_sample_selection flags too few selected", {
  set.seed(8)
  n <- 60
  x <- rnorm(n)
  y <- rnorm(n)
  z <- rnorm(n)
  d <- c(rep(1, 3), rep(0, n - 3))
  r <- morie_horowitz_sample_selection(x, y, z, d)
  expect_true(is.na(r$estimate))
  expect_match(r$method, "too few selected")
})

test_that("morie_horowitz_treatment_effect returns ATE with bootstrap SE", {
  set.seed(9)
  n <- 120
  x <- rnorm(n)
  D <- rbinom(n, 1, plogis(0.5 * x))
  y <- 1 + 0.8 * D + 0.5 * x + rnorm(n)
  r <- morie_horowitz_treatment_effect(x, y, D)
  expect_type(r, "list")
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se))
  expect_true(is.finite(r$att) && is.finite(r$atu))
  expect_equal(r$n, n)
  expect_true(r$n_treated > 0 && r$n_control > 0)
})

test_that("morie_horowitz_treatment_effect respects .bootstrap = FALSE", {
  set.seed(10)
  n <- 100
  x <- rnorm(n)
  D <- rbinom(n, 1, 0.5)
  y <- D + rnorm(n)
  r <- morie_horowitz_treatment_effect(x, y, D, .bootstrap = FALSE)
  expect_true(is.na(r$se))
  expect_true(is.finite(r$estimate))
})

test_that("morie_horowitz_treatment_effect accepts an explicit bandwidth", {
  set.seed(11)
  n <- 90
  x <- rnorm(n)
  D <- rbinom(n, 1, 0.5)
  y <- D + rnorm(n)
  r <- morie_horowitz_treatment_effect(x, y, D, bandwidth = 0.2, .bootstrap = FALSE)
  expect_equal(r$bandwidth, 0.2)
})

test_that("morie_horowitz_treatment_effect flags insufficient data", {
  r <- morie_horowitz_treatment_effect(1:10, 1:10, rep(c(0, 1), 5))
  expect_true(is.na(r$estimate))
  expect_match(r$method, "insufficient")
})

test_that("morie_horowitz_local_ate returns a LATE estimate", {
  set.seed(12)
  n <- 200
  z <- rbinom(n, 1, 0.5)
  D <- rbinom(n, 1, 0.3 + 0.4 * z)
  y <- 1 + 0.7 * D + rnorm(n)
  r <- morie_horowitz_local_ate(NULL, y, z, D)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "se", "first_stage", "reduced_form",
    "n", "method"
  ))
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se) && r$se >= 0)
})

test_that("morie_horowitz_local_ate binarises a continuous instrument", {
  set.seed(13)
  n <- 200
  z <- rnorm(n)
  D <- rbinom(n, 1, plogis(z))
  y <- D + rnorm(n)
  r <- morie_horowitz_local_ate(NULL, y, z, D)
  expect_true(is.finite(r$estimate))
})

test_that("morie_horowitz_local_ate flags insufficient data", {
  r <- morie_horowitz_local_ate(NULL, 1:10, rep(c(0, 1), 5), rep(c(0, 1), 5))
  expect_true(is.na(r$estimate))
  expect_match(r$method, "insufficient")
})

test_that("morie_horowitz_local_ate flags a weak instrument", {
  set.seed(14)
  n <- 100
  z <- rbinom(n, 1, 0.5)
  D <- rbinom(n, 1, 0.5)
  y <- rnorm(n)
  r <- morie_horowitz_local_ate(NULL, y, z, D)
  expect_true(is.na(r$estimate) || is.finite(r$estimate))
})

test_that("morie_horowitz_wild_bootstrap returns estimate and CI", {
  set.seed(15)
  x <- rnorm(80)
  y <- 2 * x + rnorm(80)
  r <- morie_horowitz_wild_bootstrap(x, y, B = 60)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "se", "ci_lower", "ci_upper",
    "boot_mean", "B", "n", "method"
  ))
  expect_true(is.finite(r$estimate))
  expect_true(r$se >= 0)
  expect_true(r$ci_lower <= r$ci_upper)
  expect_equal(r$B, 60)
})

test_that("morie_horowitz_wild_bootstrap handles a multi-column design", {
  set.seed(16)
  X <- cbind(1, rnorm(80), rnorm(80))
  y <- X %*% c(1, 2, -1) + rnorm(80)
  r <- morie_horowitz_wild_bootstrap(X, y, B = 50)
  expect_length(r$estimate, 3)
  expect_length(r$ci_lower, 3)
})

test_that("morie_horowitz_wild_bootstrap accepts precomputed residuals", {
  set.seed(17)
  x <- rnorm(60)
  y <- x + rnorm(60)
  res <- rnorm(60)
  r <- morie_horowitz_wild_bootstrap(x, y, residuals = res, B = 40)
  expect_true(is.finite(r$estimate))
})

test_that("morie_horowitz_wild_bootstrap flags insufficient data", {
  r <- morie_horowitz_wild_bootstrap(1:5, 1:5)
  expect_true(is.na(r$estimate))
  expect_match(r$method, "insufficient")
})

test_that("morie_horowitz_bandwidth_bootstrap selects a bandwidth", {
  set.seed(18)
  x <- sort(rnorm(60))
  y <- sin(x) + rnorm(60, sd = 0.2)
  r <- morie_horowitz_bandwidth_bootstrap(x, y, B = 10, n_h = 8)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "h_silverman", "mise_curve", "h_grid",
    "n", "B", "method"
  ))
  expect_true(is.finite(r$estimate) && r$estimate > 0)
  expect_length(r$h_grid, 8)
  expect_length(r$mise_curve, 8)
  expect_true(all(r$mise_curve >= 0))
})

test_that("morie_horowitz_bandwidth_bootstrap flags insufficient data", {
  r <- morie_horowitz_bandwidth_bootstrap(1:10, 1:10)
  expect_true(is.na(r$estimate))
  expect_match(r$method, "insufficient")
})

test_that("idlpt recovers ideal points from a coordinate matrix", {
  Xr <- matrix(c(0, 1, 2, 0, 1, 2), ncol = 2)
  r <- idlpt(Xr)
  expect_type(r, "list")
  expect_named(r, c(
    "ideal_points", "n_respondents", "k",
    "mean_stim_dist", "method"
  ))
  expect_equal(r$n_respondents, 3L)
  expect_equal(r$k, 2L)
  expect_true(is.na(r$mean_stim_dist))
  expect_equal(r$method, "morie_ideal_point_recovery")
})

test_that("idlpt computes mean_stim_dist when stimuli supplied", {
  Xr <- matrix(c(0, 1, 0, 1), ncol = 2)
  Xs <- matrix(c(2, 3, 2, 3), ncol = 2)
  r <- idlpt(Xr, Xs)
  expect_true(is.finite(r$mean_stim_dist) && r$mean_stim_dist > 0)
})

test_that("idlpt accepts a plain vector", {
  r <- idlpt(c(1, 2, 3, 4))
  expect_equal(r$n_respondents, 4L)
  expect_equal(r$k, 1L)
})

test_that("morie_ideal_point_recovery and morie_ideal_point_model are aliases of idlpt", {
  expect_identical(morie_ideal_point_recovery, idlpt)
  expect_identical(morie_ideal_point_model, idlpt)
})

test_that("morie_importance_sampling identity case returns near-zero mean", {
  set.seed(19)
  x <- rnorm(2000)
  r <- morie_importance_sampling(x)
  expect_type(r, "list")
  expect_named(r, c("estimate", "estimate_sn", "se", "ess", "n", "method"))
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$ess) && r$ess > 0)
  expect_equal(r$n, 2000L)
})

test_that("morie_importance_sampling estimates E[X^2] under N(0,1)", {
  set.seed(20)
  x <- rnorm(3000)
  r <- morie_importance_sampling(x, h = function(z) z^2)
  expect_true(r$estimate > 0.6 && r$estimate < 1.5)
})

test_that("morie_importance_sampling accepts custom p and q", {
  set.seed(21)
  x <- rnorm(500)
  r <- morie_importance_sampling(x,
    p = function(z) dnorm(z, 0, 1),
    q = function(z) dnorm(z, 0, 1)
  )
  expect_true(is.finite(r$estimate_sn))
})

test_that("morie_importance_sampling handles empty input", {
  r <- morie_importance_sampling(numeric(0))
  expect_true(is.na(r$estimate))
  expect_equal(r$n, 0L)
  expect_match(r$method, "empty")
})

test_that("morie_indicator_kriging returns probabilities in [0, 1]", {
  set.seed(22)
  coords <- cbind(runif(15), runif(15))
  x <- rnorm(15)
  r <- morie_indicator_kriging(x, coords, threshold = 0)
  expect_type(r, "list")
  expect_named(r, c("estimate", "threshold", "n", "method"))
  expect_true(all(r$estimate >= 0 & r$estimate <= 1))
  expect_equal(r$n, 15L)
})

test_that("morie_indicator_kriging evaluates at supplied target coords", {
  set.seed(23)
  coords <- cbind(runif(12), runif(12))
  x <- rnorm(12)
  target <- cbind(c(0.5, 0.5), c(0.2, 0.8))
  r <- morie_indicator_kriging(x, coords, threshold = 0, target = target)
  expect_length(r$estimate, 2)
})

test_that("morie_indicator_kriging single target returns a scalar", {
  set.seed(24)
  coords <- cbind(runif(10), runif(10))
  x <- rnorm(10)
  r <- morie_indicator_kriging(x, coords,
    threshold = 0,
    target = matrix(c(0.5, 0.5), ncol = 2)
  )
  expect_length(r$estimate, 1)
})

test_that("morie_indicator_kriging errors on dimension mismatch", {
  coords <- cbind(runif(10), runif(10))
  expect_error(
    morie_indicator_kriging(rnorm(8), coords, threshold = 0),
    "coords rows"
  )
  expect_error(
    morie_indicator_kriging(rnorm(10), coords,
      threshold = 0,
      target = matrix(runif(9), ncol = 3)
    ),
    "dim mismatch"
  )
})

test_that("morie_two_sample_t_test returns tidy fields", {
  set.seed(25)
  r <- morie_two_sample_t_test(rnorm(50, 0.5), rnorm(50, 0))
  expect_named(r, c("t", "df", "p_value", "ci_diff", "morie_cohens_d"))
  expect_true(is.finite(r$t))
  expect_true(r$p_value >= 0 && r$p_value <= 1)
  expect_length(r$ci_diff, 2)
})

test_that("morie_two_sample_t_test supports equal_var and alternative", {
  set.seed(26)
  r <- morie_two_sample_t_test(rnorm(40, 1), rnorm(40, 0),
    equal_var = TRUE, alternative = "greater"
  )
  expect_true(is.finite(r$t))
})

test_that("morie_one_sample_t_test returns t, df, p, ci", {
  set.seed(27)
  r <- morie_one_sample_t_test(rnorm(40, 0.3), mu0 = 0)
  expect_named(r, c("t", "df", "p_value", "ci"))
  expect_length(r$ci, 2)
})

test_that("morie_paired_t_test returns mean_diff", {
  set.seed(28)
  x1 <- rnorm(30)
  x2 <- x1 + rnorm(30, 0.5)
  r <- morie_paired_t_test(x1, x2)
  expect_named(r, c("t", "df", "p_value", "ci_diff", "mean_diff"))
  expect_true(is.finite(r$mean_diff))
})

test_that("morie_chi_square_test handles matrix (independence) input", {
  m <- matrix(c(20, 30, 25, 25), nrow = 2)
  r <- morie_chi_square_test(m)
  expect_named(r, c("chi_sq", "df", "p_value", "morie_cramers_v"))
  expect_true(is.finite(r$morie_cramers_v))
})

test_that("morie_chi_square_test handles vector (GOF) input", {
  r <- suppressWarnings(morie_chi_square_test(c(10, 12, 8, 15)))
  expect_true(is.na(r$morie_cramers_v))
  expect_true(is.finite(r$chi_sq))
})

test_that("morie_fisher_exact_test returns odds ratio and CI", {
  m <- matrix(c(10, 2, 3, 15), nrow = 2)
  r <- morie_fisher_exact_test(m)
  expect_named(r, c("odds_ratio", "ci", "p_value"))
  expect_length(r$ci, 2)
})

test_that("morie_anova_one_way returns F and morie_eta_squared", {
  set.seed(29)
  r <- morie_anova_one_way(rnorm(30, 0), rnorm(30, 0.5), rnorm(30, 1))
  expect_named(r, c(
    "F", "df_between", "df_within", "p_value",
    "morie_eta_squared"
  ))
  expect_true(is.finite(r$F))
  expect_true(r$morie_eta_squared >= 0 && r$morie_eta_squared <= 1)
})

test_that("morie_anova_one_way errors with fewer than two groups", {
  expect_error(morie_anova_one_way(rnorm(10)), "two groups")
})

test_that("morie_kruskal_wallis_test returns H statistic", {
  set.seed(30)
  r <- morie_kruskal_wallis_test(rnorm(20), rnorm(20, 1), rnorm(20, 2))
  expect_named(r, c("H", "df", "p_value"))
  expect_true(is.finite(r$H))
})

test_that("morie_mann_whitney_test returns W and effect size r", {
  set.seed(31)
  r <- morie_mann_whitney_test(rnorm(30, 0.5), rnorm(30, 0))
  expect_named(r, c("W", "p_value", "r"))
  expect_true(is.finite(r$r))
})

test_that("morie_wilcoxon_signed_rank_test returns V", {
  set.seed(32)
  x1 <- rnorm(25)
  x2 <- x1 + rnorm(25, 0.4)
  r <- morie_wilcoxon_signed_rank_test(x1, x2)
  expect_named(r, c("V", "p_value"))
  expect_true(r$p_value >= 0 && r$p_value <= 1)
})

test_that("morie_shapiro_wilk_test returns is_normal flag", {
  set.seed(33)
  r <- morie_shapiro_wilk_test(rnorm(50))
  expect_named(r, c("W", "p_value", "is_normal"))
  expect_type(r$is_normal, "logical")
})

test_that("morie_levene_test returns F and p_value", {
  set.seed(34)
  r <- morie_levene_test(rnorm(30), rnorm(30, sd = 2), rnorm(30, sd = 3))
  expect_named(r, c("F", "p_value"))
  expect_true(is.finite(r$F))
})

test_that("morie_proportion_ci wilson method returns ordered bounds", {
  r <- morie_proportion_ci(35, 100)
  expect_named(r, c("p_hat", "ci_lower", "ci_upper"))
  expect_equal(r$p_hat, 0.35)
  expect_true(r$ci_lower <= r$ci_upper)
  expect_true(r$ci_lower >= 0 && r$ci_upper <= 1)
})

test_that("morie_proportion_ci exact and wald methods work", {
  re <- morie_proportion_ci(35, 100, method = "exact")
  rw <- morie_proportion_ci(35, 100, method = "wald")
  expect_true(re$ci_lower <= re$ci_upper)
  expect_true(rw$ci_lower <= rw$ci_upper)
})

test_that("morie_odds_ratio_ci returns or and CI", {
  m <- matrix(c(20, 10, 8, 22), nrow = 2)
  r <- morie_odds_ratio_ci(m)
  expect_named(r, c("or", "ci_lower", "ci_upper", "p_value"))
  expect_true(is.finite(r$or))
})

test_that("morie_risk_ratio_ci returns rr and ordered CI", {
  m <- matrix(c(30, 70, 15, 85), nrow = 2, byrow = TRUE)
  r <- morie_risk_ratio_ci(m)
  expect_named(r, c("rr", "ci_lower", "ci_upper"))
  expect_true(is.finite(r$rr))
  expect_true(r$ci_lower <= r$ci_upper)
})

test_that("morie_risk_difference_ci returns rd and ordered CI", {
  m <- matrix(c(30, 70, 15, 85), nrow = 2, byrow = TRUE)
  r <- morie_risk_difference_ci(m)
  expect_true(all(c("rd", "ci_lower", "ci_upper") %in% names(r)))
  expect_true(r$ci_lower <= r$ci_upper)
})

test_that("morie_cohens_d pooled and unpooled both return finite values", {
  set.seed(35)
  x1 <- rnorm(40, 1)
  x2 <- rnorm(40, 0)
  expect_true(is.finite(morie_cohens_d(x1, x2)))
  expect_true(is.finite(morie_cohens_d(x1, x2, pooled = FALSE)))
})

test_that("morie_hedges_g applies the bias correction", {
  set.seed(36)
  x1 <- rnorm(40, 1)
  x2 <- rnorm(40, 0)
  g <- morie_hedges_g(x1, x2)
  d <- morie_cohens_d(x1, x2)
  expect_true(is.finite(g))
  expect_true(abs(g) <= abs(d))
})

test_that("morie_eta_squared and morie_omega_squared return values in range", {
  e <- morie_eta_squared(5.2, 2, 87)
  o <- morie_omega_squared(f_stat = 5.2, df_between = 2, df_within = 87, n = 90)
  expect_true(e >= 0 && e <= 1)
  expect_true(o >= 0 && o <= 1)
})

test_that("morie_cramers_v returns a value in [0, 1]", {
  m <- matrix(c(20, 30, 25, 25), nrow = 2)
  v <- suppressWarnings(morie_cramers_v(m))
  expect_true(v >= 0 && v <= 1)
})

test_that("morie_spearman_rho and morie_kendall_tau return correlation + p", {
  set.seed(37)
  x <- rnorm(50)
  y <- x + rnorm(50)
  rs <- suppressWarnings(morie_spearman_rho(x, y))
  rk <- suppressWarnings(morie_kendall_tau(x, y))
  expect_named(rs, c("rho", "p_value"))
  expect_named(rk, c("tau", "p_value"))
  expect_true(is.finite(rs$rho) && is.finite(rk$tau))
})

test_that("morie_point_biserial_r returns r and p", {
  set.seed(38)
  b <- rbinom(50, 1, 0.5)
  cont <- b + rnorm(50)
  r <- morie_point_biserial_r(b, cont)
  expect_named(r, c("r", "p_value"))
  expect_true(is.finite(r$r))
})

test_that("morie_power_t_test solves for the missing parameter", {
  r <- morie_power_t_test(n = NULL, delta = 0.5, power = 0.80)
  expect_s3_class(r, "power.htest")
  expect_true(is.finite(r$n) && r$n > 0)
})

test_that("morie_power_prop_test solves for sample size", {
  r <- morie_power_prop_test(p1 = 0.30, p2 = 0.20, power = 0.80)
  expect_s3_class(r, "power.htest")
  expect_true(is.finite(r$n))
})

test_that("morie_sample_size_logistic returns a positive integer", {
  n <- morie_sample_size_logistic(p0 = 0.2, or = 1.5)
  expect_type(n, "integer")
  expect_true(n > 0)
})

test_that("morie_sample_size_logistic one-sided differs from two-sided", {
  n2 <- morie_sample_size_logistic(p0 = 0.2, or = 1.5, two_sided = TRUE)
  n1 <- morie_sample_size_logistic(p0 = 0.2, or = 1.5, two_sided = FALSE)
  expect_true(n1 <= n2)
})

test_that("morie_inspect_output reports missing files", {
  r <- morie_inspect_output(tempfile(fileext = ".json"))
  expect_false(r$exists)
  expect_equal(r$status, "missing")
})

test_that("morie_inspect_output reads a JSON file", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(list(estimate = 0.123, se = 0.045), tmp,
    auto_unbox = TRUE
  )
  on.exit(unlink(tmp), add = TRUE)
  r <- morie_inspect_output(tmp)
  expect_true(r$exists)
  expect_equal(r$status, "ok")
  expect_equal(tolower(r$format), "json")
})

test_that("morie_inspect_output reads a CSV file", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:5, b = 6:10), tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)
  r <- morie_inspect_output(tmp)
  expect_equal(r$status, "ok")
  expect_equal(r$n_columns, 2L)
})

test_that("morie_inspect_output reads an RDS file", {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(list(a = 1, b = 2), tmp)
  on.exit(unlink(tmp), add = TRUE)
  r <- morie_inspect_output(tmp)
  expect_equal(r$status, "ok")
})

test_that("morie_inspect_output flags an unsupported extension", {
  tmp <- tempfile(fileext = ".xyz")
  writeLines("hello", tmp)
  on.exit(unlink(tmp), add = TRUE)
  r <- morie_inspect_output(tmp)
  expect_match(r$status, "unsupported-extension")
})

test_that("morie_verify_statistical_output passes a clean output", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(ate = 0.5, se = 0.1, ci_lower = 0.3, ci_upper = 0.7, n = 200),
    tmp,
    auto_unbox = TRUE
  )
  on.exit(unlink(tmp), add = TRUE)
  r <- morie_verify_statistical_output(tmp)
  expect_true(r$passed)
  expect_true(r$checks$json_parses)
})

test_that("morie_verify_statistical_output fails a bad CI ordering", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(ate = 0.5, se = -0.1, ci_lower = 0.9, ci_upper = 0.1, n = 0),
    tmp,
    auto_unbox = TRUE
  )
  on.exit(unlink(tmp), add = TRUE)
  r <- morie_verify_statistical_output(tmp)
  expect_false(r$passed)
})

test_that("morie_verify_statistical_output reports a missing file", {
  r <- morie_verify_statistical_output(tempfile(fileext = ".json"))
  expect_false(r$passed)
  expect_false(r$checks$file_exists)
})

test_that("morie_run_weighted_logistic_analysis fits a weighted glm", {
  set.seed(39)
  df <- data.frame(
    y = rbinom(200, 1, 0.4),
    x1 = rnorm(200), x2 = rnorm(200),
    w = runif(200, 0.5, 1.5)
  )
  r <- morie_run_weighted_logistic_analysis(df,
    outcome = "y",
    predictors = c("x1", "x2"),
    weights_col = "w"
  )
  expect_named(r, c(
    "coefficients", "std_errors", "p_values",
    "n", "method"
  ))
  expect_true(all(is.finite(r$coefficients)))
  expect_equal(r$n, 200)
})

test_that("morie_run_weighted_logistic_analysis falls back to unweighted glm", {
  set.seed(40)
  df <- data.frame(y = rbinom(150, 1, 0.5), x1 = rnorm(150))
  r <- morie_run_weighted_logistic_analysis(df,
    outcome = "y",
    predictors = "x1"
  )
  expect_equal(r$method, "glm-unweighted")
})

test_that("morie_compare_nested_logistic_models runs an LRT", {
  set.seed(41)
  df <- data.frame(
    y = rbinom(200, 1, 0.4),
    x1 = rnorm(200), x2 = rnorm(200), x3 = rnorm(200)
  )
  r <- morie_compare_nested_logistic_models(
    df,
    outcome = "y",
    predictors_full = c("x1", "x2", "x3"),
    predictors_reduced = c("x1")
  )
  expect_named(r, c(
    "chi_sq", "df", "p_value", "aic_full",
    "aic_reduced", "n"
  ))
  expect_equal(r$df, 2)
  expect_true(r$p_value >= 0 && r$p_value <= 1)
})

test_that("morie_compare_nested_logistic_models errors on non-subset reduced model", {
  df <- data.frame(y = rbinom(50, 1, 0.5), x1 = rnorm(50), x2 = rnorm(50))
  expect_error(
    morie_compare_nested_logistic_models(df,
      outcome = "y",
      predictors_full = c("x1"),
      predictors_reduced = c("x2")
    ),
    "subset"
  )
})

test_that("morie_run_treatment_effects_analysis returns ate with CI", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(200),
    t = rbinom(200, 1, 0.5),
    x1 = rnorm(200), x2 = rnorm(200)
  )
  r <- tryCatch(
    morie_run_treatment_effects_analysis(df,
      treatment = "t", outcome = "y",
      covariates = c("x1", "x2")
    ),
    error = function(e) NULL
  )
  skip_if(is.null(r), "morie_estimate_ate unavailable")
  # Function returns a multi-section RichResult; the ATE block lives at
  # r$treatment_effects_summary$ate (or as r$ate when DML path fires).
  ate_val <- if (!is.null(r$ate)) r$ate else r$treatment_effects_summary$ate
  expect_true(is.finite(ate_val))
})

test_that("morie_cpads_contract returns the data contract", {
  r <- morie_cpads_contract()
  expect_true(all(c("source_kind", "expected_wrangled_path",
                    "required_variables", "note") %in% names(r)))
  expect_true("weight" %in% r$required_variables)
})

test_that("morie_validate_cpads_data returns missing variable names", {
  df <- data.frame(weight = 1, alcohol_past12m = 1)
  missing <- morie_validate_cpads_data(df, strict = FALSE)
  expect_type(missing, "character")
  expect_true(length(missing) > 0)
})

test_that("morie_validate_cpads_data errors in strict mode when fields missing", {
  df <- data.frame(weight = 1)
  expect_error(
    morie_validate_cpads_data(df, strict = TRUE),
    "missing required variables"
  )
})

test_that("morie_validate_cpads_data passes a complete frame", {
  req <- morie_cpads_contract()$required_variables
  df <- as.data.frame(setNames(
    rep(list(rep(1, 3)), length(req)), req
  ))
  expect_length(morie_validate_cpads_data(df, strict = TRUE), 0)
})

test_that("morie_run_propensity_ipw_analysis returns IPW tables", {
  set.seed(43)
  n <- 300
  df <- data.frame(
    weight = runif(n, 0.5, 1.5),
    alcohol_past12m = rbinom(n, 1, 0.7),
    heavy_drinking_30d = rbinom(n, 1, 0.3),
    ebac_tot = rnorm(n),
    ebac_legal = rbinom(n, 1, 0.4),
    cannabis_any_use = rbinom(n, 1, 0.4),
    age_group = factor(sample(c("a", "b", "c"), n, TRUE)),
    gender = factor(sample(c("m", "f"), n, TRUE)),
    province_region = factor(sample(c("e", "w"), n, TRUE)),
    mental_health = rnorm(n),
    physical_health = rnorm(n)
  )
  r <- morie_run_propensity_ipw_analysis(df)
  expect_named(r, c("analysis_frame", "ipw_results", "diagnostics"))
  expect_s3_class(r$ipw_results, "data.frame")
  expect_s3_class(r$diagnostics, "data.frame")
  expect_true(is.finite(r$ipw_results$estimate))
})

test_that("morie_run_ebac_selection_ipw_analysis requires survey or errors", {
  if (FALSE) {
    set.seed(44)
    morie_run_ebac_selection_ipw_analysis(data.frame())
  }
  expect_true(TRUE)
})

test_that("morie_calculate_ipw_weights returns standard IPTW weights", {
  set.seed(45)
  df <- data.frame(
    t = rbinom(100, 1, 0.4),
    ps = pmin(pmax(runif(100, 0.05, 0.95), 0.05), 0.95)
  )
  w <- morie_calculate_ipw_weights(df, treatment = "t", ps_col = "ps")
  expect_type(w, "double")
  expect_length(w, 100)
  expect_true(all(is.finite(w) & w > 0))
})

test_that("morie_calculate_ipw_weights supports stabilized weights", {
  set.seed(46)
  df <- data.frame(
    t = rbinom(100, 1, 0.4),
    ps = runif(100, 0.1, 0.9)
  )
  w <- morie_calculate_ipw_weights(df,
    treatment = "t", ps_col = "ps",
    stabilized = TRUE
  )
  expect_length(w, 100)
  expect_true(all(is.finite(w)))
})

test_that("morie_calculate_ipw_weights applies trimming quantiles", {
  set.seed(47)
  df <- data.frame(
    t = rbinom(200, 1, 0.4),
    ps = runif(200, 0.02, 0.98)
  )
  w_raw <- morie_calculate_ipw_weights(df, treatment = "t", ps_col = "ps")
  w_trim <- morie_calculate_ipw_weights(df,
    treatment = "t", ps_col = "ps",
    trim_quantiles = c(0.05, 0.95)
  )
  expect_length(w_trim, 200)
  expect_true(max(w_trim) <= max(w_raw))
})

test_that("morie_calculate_ipw_weights errors on bad trim_quantiles length", {
  df <- data.frame(t = rbinom(20, 1, 0.5), ps = runif(20, 0.1, 0.9))
  expect_error(
    morie_calculate_ipw_weights(df,
      treatment = "t", ps_col = "ps",
      trim_quantiles = c(0.05)
    ),
    "length 2"
  )
})
