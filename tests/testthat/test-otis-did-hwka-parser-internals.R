# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2TT: tests for internal helpers across study_reporting.R,
# otis_causal.R, did.R, matching.R, siu_parser.R, and
# tps_hawkes_advanced.R.

# ============================================================ study_reporting.R

test_that(".binary_power_required_n returns a positive integer-ish n", {
  out <- morie:::.binary_power_required_n(p1 = 0.30, p2 = 0.50)
  expect_true(is.numeric(out) && is.finite(out) && out > 0)
})

test_that(".binary_power_required_n returns NA on equal proportions", {
  expect_true(is.na(morie:::.binary_power_required_n(0.5, 0.5)))
})

test_that(".continuous_power_required_n returns a positive n", {
  out <- morie:::.continuous_power_required_n(
    mean1 = 10, mean2 = 12, sd_pooled = 4)
  expect_true(is.numeric(out) && out > 0)
})

test_that(".continuous_power_required_n returns NA on equal means", {
  expect_true(is.na(morie:::.continuous_power_required_n(5, 5, 1)))
})

test_that(".block_schedule returns the canonical empty df on NA n / empty strata", {
  out <- morie:::.block_schedule(endpoint = "ep",
                                   required_n = NA_real_,
                                   strata_levels = c("Male"))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_true("endpoint" %in% names(out))
  expect_true("block_id" %in% names(out))
})

test_that(".block_schedule emits per-stratum rows for positive n", {
  out <- morie:::.block_schedule(endpoint = "heavy_drinking",
                                   required_n = 8L,
                                   strata_levels = c("Male", "Female"))
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0L)
  expect_setequal(unique(out$gender), c("Male", "Female"))
})

test_that(".legacy_reference_root returns NA_character_ or a path", {
  out <- morie:::.legacy_reference_root()
  expect_true(is.na(out) || is.character(out))
})

# ============================================================== otis_causal.R

test_that(".otis_design_matrix returns an intercept + dummy matrix", {
  df <- data.frame(x1 = stats::rnorm(20),
                   x2 = stats::rbinom(20, 1, 0.5),
                   x3 = sample(c("a", "b", "c"), 20, TRUE))
  X <- morie:::.otis_design_matrix(df, c("x1", "x2", "x3"))
  expect_true(is.matrix(X))
  expect_equal(nrow(X), 20L)
  # Intercept + 3 covariates with x3 expanding to 2 dummies = 5 cols.
  expect_equal(ncol(X), 5L)
})

test_that(".otis_logit_fit converges to a non-trivial coef vector", {
  set.seed(1L); n <- 200L
  X <- cbind(1, stats::rnorm(n), stats::rnorm(n))
  beta <- c(-0.5, 1.0, -0.5)
  p <- 1 / (1 + exp(-X %*% beta))
  d <- stats::rbinom(n, 1L, as.numeric(p))
  out <- morie:::.otis_logit_fit(X, d)
  expect_length(out, 3L)
  # Slopes should have the right sign.
  expect_true(out[2] > 0 && out[3] < 0)
})

test_that(".otis_clip_ps clips into [eps, 1-eps]", {
  expect_equal(morie:::.otis_clip_ps(c(0, 0.5, 1), eps = 0.02),
               c(0.02, 0.5, 0.98))
})

test_that(".otis_predict_ps returns probabilities in (0,1)", {
  set.seed(2L); n <- 50L
  X <- cbind(1, stats::rnorm(n))
  beta <- c(0, 1)
  ps <- morie:::.otis_predict_ps(X, beta, eps = 0.02)
  expect_true(all(ps > 0 & ps < 1))
})

test_that(".otis_propensity_diagnostics returns brier + log_loss + prev", {
  set.seed(3L)
  d <- stats::rbinom(100, 1L, 0.4)
  p <- runif(100, 0.1, 0.9)
  out <- morie:::.otis_propensity_diagnostics(p, d)
  expect_named(out, c("brier", "obs_prevalence",
                      "predicted_prevalence", "log_loss"))
  expect_true(out$brier >= 0 && out$brier <= 1)
})

test_that(".otis_cluster_se returns a non-negative scalar", {
  set.seed(4L)
  scores <- stats::rnorm(100)
  cluster <- rep(1:10, each = 10)
  out <- morie:::.otis_cluster_se(scores, cluster)
  expect_true(is.numeric(out) && out >= 0)
})

test_that(".otis_multiway_cluster_se 1-way matches .otis_cluster_se", {
  set.seed(5L)
  scores <- stats::rnorm(100)
  cluster <- rep(1:10, each = 10)
  out1 <- morie:::.otis_multiway_cluster_se(scores, list(cluster))
  out2 <- morie:::.otis_cluster_se(scores, cluster)
  expect_equal(out1, out2, tolerance = 1e-12)
})

test_that(".otis_multiway_cluster_se 2-way returns a non-negative scalar", {
  set.seed(6L)
  scores <- stats::rnorm(100)
  a <- rep(1:10, each = 10)
  b <- rep(1:5, times = 20)
  out <- morie:::.otis_multiway_cluster_se(scores, list(a, b))
  expect_true(is.numeric(out) && out >= 0)
})

test_that(".otis_causal_estimate builds a structured estimate", {
  out <- morie:::.otis_causal_estimate(
    estimator = "AIPW", ate = 0.05, ate_se = 0.02, ate_pval = 0.01,
    n = 500L, n_treated = 200L, p_treat = 0.4)
  expect_type(out, "list")
})

# ======================================================================== did.R

test_that(".morie_did_result builds a result list with sensible defaults", {
  out <- morie:::.morie_did_result(
    estimate = 1.5, std_error = 0.5,
    n_treated = 100L, n_control = 100L,
    method = "twfe")
  expect_type(out, "list")
  expect_named(out, c("estimate", "std_error", "t_stat", "p_value",
                      "ci_lower", "ci_upper", "n_treated", "n_control",
                      "method", "details"))
  expect_equal(out$t_stat, 3.0, tolerance = 1e-10)
})

test_that(".morie_did_result NA-fills CI when SE is non-finite", {
  out <- morie:::.morie_did_result(
    estimate = 1.5, std_error = NA_real_,
    n_treated = 100L, n_control = 100L,
    method = "twfe")
  expect_true(is.na(out$ci_lower))
  expect_true(is.na(out$ci_upper))
})

# ================================================================== matching.R

test_that(".morie_matching_require errors when pkg is missing", {
  expect_error(
    morie:::.morie_matching_require("nonexistent_pkg_xyz",
                                      "fake_fn"),
    regexp = "morie requires")
})

test_that(".morie_matching_te_empty returns NA fields + class morie_te_result", {
  out <- morie:::.morie_matching_te_empty("ATT")
  expect_s3_class(out, "morie_te_result")
  expect_true(is.na(out$estimate))
  expect_equal(out$n_obs, 0L)
})

test_that(".morie_matching_te_result computes 2-sided p-value + CI bounds", {
  out <- morie:::.morie_matching_te_result(
    estimand = "ATT", estimate = 2.0, se = 1.0,
    n_obs = 100L, alpha = 0.05)
  expect_s3_class(out, "morie_te_result")
  expect_equal(out$estimate, 2.0)
  # z = 2; two-sided p = 2 * Phi(-2)
  expect_equal(out$p_value, 2 * stats::pnorm(-2), tolerance = 1e-10)
  # CI = estimate +/- 1.96 * SE
  expect_equal(out$ci_lower,
               2.0 - stats::qnorm(0.975) * 1.0, tolerance = 1e-10)
})

# ================================================================ siu_parser.R

test_that(".siu_p_extract_summary returns the first long paragraph", {
  paras <- paste(c(
    "short.",
    paste(rep("x", 100L), collapse = " "),
    paste(rep("y", 100L), collapse = " ")), collapse = "\n\n")
  out <- morie:::.siu_p_extract_summary(paras)
  expect_type(out, "character")
  expect_match(out, "x x x")
})

test_that(".siu_p_extract_summary returns NA on too-short input", {
  expect_true(is.na(morie:::.siu_p_extract_summary("short")))
})

test_that(".siu_p_scan_mh_race finds matched keywords + concatenates", {
  out <- morie:::.siu_p_scan_mh_race(
    "He was in mental health crisis. The man was Indigenous.")
  expect_match(out, "mental health")
  expect_match(out, "Indigenous")
})

test_that(".siu_p_scan_mh_race returns '' on empty / no matches", {
  expect_equal(morie:::.siu_p_scan_mh_race("plain text no keywords"),
               "")
  expect_equal(morie:::.siu_p_scan_mh_race(""), "")
  expect_equal(morie:::.siu_p_scan_mh_race(NA), "")
})

test_that(".siu_p_extract_narrative_full returns text on plain input", {
  out <- morie:::.siu_p_extract_narrative_full(
    html = "<html></html>",
    text = paste("The Investigation began on the same day.",
                 "Officers responded. Endnotes follow."))
  expect_type(out, "character")
})

# ======================================================== tps_hawkes_advanced.R

test_that(".tps_hwka_kernel_density returns positive density for exp kernel", {
  out <- morie:::.tps_hwka_kernel_density(
    u = c(0.1, 0.5, 1, 2),
    kind = "exponential", psi = c(2.0))
  expect_length(out, 4L)
  expect_true(all(out > 0))
  # exponential: beta * exp(-beta * u); at u=0, density = beta = 2.
  expect_equal(
    morie:::.tps_hwka_kernel_density(0, "exponential", c(2.0))[1],
    2, tolerance = 1e-10)
})

test_that(".tps_hwka_kernel_cdf returns CDF in [0,1] and is monotone", {
  u <- seq(0, 5, by = 0.5)
  out <- morie:::.tps_hwka_kernel_cdf(u, "exponential", c(1.0))
  expect_true(all(out >= 0 & out <= 1))
  expect_true(all(diff(out) >= -1e-10))
})

test_that(".tps_hwka_kernel_density errors on unknown kernel kind", {
  expect_error(morie:::.tps_hwka_kernel_density(
    c(0.5), "garch", c(1.0)),
    regexp = "unknown kernel kind")
})

test_that(".tps_hwka_baseline constant returns a flat vector", {
  out <- morie:::.tps_hwka_baseline(t = c(1, 50, 100),
                                      kind = "constant",
                                      alpha = c(log(0.5)),
                                      T_ = 100)
  expect_equal(out, rep(0.5, 3L), tolerance = 1e-10)
})

test_that(".tps_hwka_baseline sinusoidal varies with t", {
  out <- morie:::.tps_hwka_baseline(
    t = c(0, 100, 200), kind = "sinusoidal",
    alpha = c(log(0.5), 0, 0.3, -0.2), T_ = 200)
  expect_length(out, 3L)
  expect_true(all(out > 0))
})

test_that(".tps_hwka_baseline_integral constant = exp(alpha[1]) * T", {
  out <- morie:::.tps_hwka_baseline_integral(
    T_ = 100, kind = "constant", alpha = c(log(0.5)))
  expect_equal(out, 0.5 * 100, tolerance = 1e-10)
})

test_that(".tps_hwka_neg_loglik_external_exp returns numeric when hawkes is present", {
  skip_if_not_installed("hawkes")
  if (!requireNamespace("hawkes", quietly = TRUE)) {
    skip("hawkes package not installed")
  }
  set.seed(7L)
  t <- sort(stats::runif(30L, 0, 100))
  # hawkes::likelihoodHawkes infers the horizon from max(history);
  # pass T_ = max(t) so the delegation path is exercised.
  out <- morie:::.tps_hwka_neg_loglik_external_exp(
    theta = c(log(0.5), 0.3, 1.0),
    t = t, T_ = max(t))
  expect_true(is.numeric(out) && is.finite(out))
})
