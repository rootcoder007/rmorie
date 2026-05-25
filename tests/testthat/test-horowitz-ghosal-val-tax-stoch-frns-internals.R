# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2UU: tests for internal helpers across aaa_helpers_horowitz.R,
# aaa_helpers_ghosal_bnp.R, validation.R, variable_taxonomy.R,
# tps_stochastic.R, frns_metrics.R, and siu_analyze.R.

# ====================================================== aaa_helpers_horowitz.R

test_that(".hrz_silverman returns 1.0 on n < 2 (degenerate fallback)", {
  expect_equal(morie:::.hrz_silverman(c(5)), 1.0)
})

test_that(".hrz_silverman returns positive bandwidth on a sample", {
  set.seed(1L); x <- stats::rnorm(100L)
  h <- morie:::.hrz_silverman(x)
  expect_true(is.numeric(h) && h > 0)
})

test_that(".hrz_gauss_kernel peaks at 1/sqrt(2*pi) at u=0", {
  expect_equal(morie:::.hrz_gauss_kernel(0),
               1 / sqrt(2 * pi), tolerance = 1e-12)
  # Symmetric.
  expect_equal(morie:::.hrz_gauss_kernel(1.0),
               morie:::.hrz_gauss_kernel(-1.0), tolerance = 1e-12)
})

test_that(".hrz_nw_loo returns LOO-smoothed vector of length n", {
  set.seed(2L); n <- 30L
  z <- stats::rnorm(n); y <- z + stats::rnorm(n, sd = 0.1)
  out <- morie:::.hrz_nw_loo(z, y, h = 0.5)
  expect_length(out, n)
  expect_true(all(is.finite(out)))
})

test_that(".hrz_hermite returns n x J orthonormalised basis matrix", {
  H <- morie:::.hrz_hermite(seq(-2, 2, length.out = 25L), J = 4L)
  expect_true(is.matrix(H))
  expect_equal(dim(H), c(25L, 4L))
})

test_that(".hrz_qreg_irls returns a coef vector of length p", {
  set.seed(3L); n <- 50L
  X <- cbind(1, stats::rnorm(n))
  y <- 1.0 + 2.0 * X[, 2] + stats::rnorm(n, sd = 0.3)
  b <- morie:::.hrz_qreg_irls(X, y, tau = 0.5)
  expect_length(b, 2L)
  expect_true(all(is.finite(b)))
})

# ====================================================== aaa_helpers_ghosal_bnp.R

test_that(".gh_have returns logical", {
  expect_true(morie:::.gh_have("stats"))
  expect_false(morie:::.gh_have("nonexistent_pkg_xyz"))
})

test_that(".gh_pairwise_sq returns ||a_i - a_j||^2 symmetric matrix", {
  set.seed(4L); a <- matrix(stats::rnorm(20L), 5L, 4L)
  D <- morie:::.gh_pairwise_sq(a)
  expect_true(is.matrix(D))
  expect_equal(dim(D), c(5L, 5L))
  expect_equal(D, t(D), tolerance = 1e-10)
  expect_equal(diag(D), rep(0, 5L), tolerance = 1e-10)
})

test_that(".gh_bernstein returns length(u) x K basis matrix", {
  u <- seq(0, 1, length.out = 11L)
  B <- morie:::.gh_bernstein(u, K = 4L)
  expect_true(is.matrix(B))
  expect_equal(dim(B), c(11L, 4L))
  expect_true(all(B >= 0))
})

# ============================================================== validation.R

test_that(".val_result stamps the requested class hierarchy", {
  out <- morie:::.val_result(class_name = "morie_cv_summary",
                              n = 10L, score = 0.85)
  expect_s3_class(out, "morie_cv_summary")
  expect_s3_class(out, "morie_validation_result")
  expect_s3_class(out, "morie_rich_result")
})

test_that(".val_auc returns 1 on perfectly separable data", {
  y_true <- c(0, 0, 0, 1, 1, 1)
  y_pred <- c(0.1, 0.2, 0.3, 0.8, 0.9, 0.95)
  expect_equal(morie:::.val_auc(y_true, y_pred), 1.0)
})

test_that(".val_auc returns 0.5 on random predictions", {
  set.seed(5L)
  y_true <- stats::rbinom(200L, 1L, 0.5)
  y_pred <- stats::runif(200L)
  out <- morie:::.val_auc(y_true, y_pred)
  expect_true(abs(out - 0.5) < 0.1)
})

test_that(".val_auc returns NA on degenerate (one-class) data", {
  expect_true(is.na(morie:::.val_auc(c(1, 1, 1), c(0.1, 0.2, 0.3))))
})

test_that(".val_brier matches mean squared deviation", {
  expect_equal(morie:::.val_brier(c(1, 0, 1, 0),
                                    c(0.9, 0.1, 0.8, 0.2)),
               mean(c(0.1, 0.1, 0.2, 0.2)^2),
               tolerance = 1e-12)
})

test_that(".val_score dispatches scoring -> metric", {
  y <- c(0, 0, 1, 1); p <- c(0.1, 0.2, 0.8, 0.9)
  expect_equal(morie:::.val_score("roc_auc", y, p), 1)
  expect_equal(morie:::.val_score("auc",     y, p), 1)
  expect_equal(morie:::.val_score("accuracy", y, p), 1)
  # brier is negated so higher is better.
  expect_true(morie:::.val_score("brier", y, p) < 0)
})

test_that(".val_score errors on unknown scoring", {
  expect_error(morie:::.val_score("unknown", c(0, 1), c(0.5, 0.5)),
               regexp = "Unknown scoring")
})

# ============================================================ variable_taxonomy.R

test_that(".is_boolean_value_set detects yes/no, true/false, 0/1", {
  expect_true(morie:::.is_boolean_value_set(c("yes", "no")))
  expect_true(morie:::.is_boolean_value_set(c("True", "False")))
  expect_true(morie:::.is_boolean_value_set(c("0", "1")))
  expect_false(morie:::.is_boolean_value_set(c("maybe", "yes")))
  expect_false(morie:::.is_boolean_value_set(NULL))
})

test_that(".cardinality_from_vv steps through the n thresholds", {
  expect_equal(morie:::.cardinality_from_vv(NULL), "unknown")
  expect_equal(morie:::.cardinality_from_vv(c("a", "b")), "binary")
  expect_equal(morie:::.cardinality_from_vv(letters[1:5]),
               "discrete_low")
  expect_equal(morie:::.cardinality_from_vv(letters[1:20]),
               "discrete_medium")
  expect_equal(morie:::.cardinality_from_vv(letters[1:26]),
               "discrete_medium")
})

test_that(".level_from_spec identifies identifiers / dates / booleans", {
  expect_equal(
    morie:::.level_from_spec("Record_ID", "int", NULL, "otis"),
    "identifier")
  expect_equal(
    morie:::.level_from_spec("placement_date", "date", NULL, "otis"),
    "date")
  expect_equal(
    morie:::.level_from_spec("MentalHealth_Alert", "bool", NULL, "otis"),
    "boolean")
})

test_that(".level_from_spec ratio vs nominal heuristics", {
  expect_equal(
    morie:::.level_from_spec("number_of_days", "int", NULL, "otis"),
    "ratio")
  expect_equal(
    morie:::.level_from_spec("severity_level", "int", NULL, "otis"),
    "ordinal")
})

test_that(".role_from_name identifies identifier / outcome / covariate", {
  expect_equal(morie:::.role_from_name("Record_ID"), "identifier")
  expect_equal(morie:::.role_from_name("injury_status"), "outcome")
  expect_equal(morie:::.role_from_name("officer_age"), "covariate")
})

# ============================================================= tps_stochastic.R

test_that(".tps_stoch_result builds a stochastic-result list", {
  out <- morie:::.tps_stoch_result(title = "Test", call = "demo",
                                     summary_lines = list(N = 10))
  expect_type(out, "list")
  expect_s3_class(out, "morie_tps_stochastic_result")
  expect_s3_class(out, "morie_rich_result")
})

test_that(".tps_stoch_round rounds to k digits + NA on non-finite", {
  expect_equal(morie:::.tps_stoch_round(3.14159, 2L), 3.14)
  expect_true(is.na(morie:::.tps_stoch_round(Inf, 2L)))
  expect_true(is.na(morie:::.tps_stoch_round(NA, 2L)))
})

test_that(".tps_stoch_date_series uses OCC_YEAR/MONTH/DAY triple", {
  df <- data.frame(OCC_YEAR  = c(2020, 2021, 2022),
                   OCC_MONTH = c(1, 6, 12),
                   OCC_DAY   = c(15, 1, 31))
  ts <- morie:::.tps_stoch_date_series(df, min_year = 2014L)
  expect_s3_class(ts, "POSIXct")
  expect_length(ts, 3L)
})

test_that(".tps_stoch_daily on empty input returns zero-row list", {
  out <- morie:::.tps_stoch_daily(as.POSIXct(character(0), tz = "UTC"))
  expect_named(out, c("dates", "counts"))
  expect_length(out$counts, 0L)
})

test_that(".tps_stoch_daily aggregates dates into per-day counts", {
  ts <- as.POSIXct(c("2024-01-01", "2024-01-01", "2024-01-02"),
                   tz = "UTC")
  out <- morie:::.tps_stoch_daily(ts)
  expect_length(out$dates, 2L)
  expect_equal(sort(out$counts), c(1L, 2L))
})

test_that(".tps_stoch_monthly on empty input returns zero-row list", {
  out <- morie:::.tps_stoch_monthly(as.POSIXct(character(0), tz = "UTC"))
  expect_named(out, c("dates", "counts"))
  expect_length(out$counts, 0L)
})

test_that(".tps_stoch_neg_loglik_hawkes returns 1e12 on infeasible params", {
  t <- sort(stats::runif(20L, 0, 100))
  out <- morie:::.tps_stoch_neg_loglik_hawkes(
    params = c(0, 0, 0), t = t, T_window = 100)
  expect_equal(out, 1e12)
})

# ============================================================= frns_metrics.R

test_that(".frns_check_aligned passes on equal-length inputs", {
  expect_silent(morie:::.frns_check_aligned(
    list("a", c(1, 2, 3)),
    list("b", c(4, 5, 6))))
})

test_that(".frns_check_aligned errors on length mismatch", {
  expect_error(morie:::.frns_check_aligned(
    list("a", c(1, 2)),
    list("b", c(4, 5, 6))),
    regexp = "length mismatch")
})

test_that(".frns_check_aligned errors on empty inputs", {
  expect_error(morie:::.frns_check_aligned(list("a", integer(0))),
               regexp = "empty")
})

test_that(".frns_favorable_rates computes per-group rates", {
  outcome <- c("hire", "hire", "no", "hire", "no", "no")
  group   <- c("A", "A", "A", "B", "B", "B")
  out <- morie:::.frns_favorable_rates(outcome, group, "hire")
  expect_named(out, c("A", "B"))
  expect_equal(out$A$rate, 2 / 3, tolerance = 1e-10)
  expect_equal(out$B$rate, 1 / 3, tolerance = 1e-10)
})

test_that(".frns_resolve_privileged passes explicit + infers when NULL", {
  rates <- list(
    A = list(value = "A", n = 3L, rate = 2 / 3),
    B = list(value = "B", n = 3L, rate = 1 / 3))
  out1 <- morie:::.frns_resolve_privileged("B", rates)
  expect_equal(out1$privileged, "B")
  expect_null(out1$warning)
  out2 <- morie:::.frns_resolve_privileged(NULL, rates)
  expect_equal(out2$privileged, "A")
  expect_true(is.character(out2$warning) && nzchar(out2$warning))
})

test_that(".frns_resolve_privileged errors on unknown explicit group", {
  rates <- list(A = list(value = "A", n = 1L, rate = 0.5))
  expect_error(morie:::.frns_resolve_privileged("Z", rates),
               regexp = "not found")
})

test_that(".frns_rates_from_labels computes per-group TPR + FPR", {
  y_true <- c(1, 1, 0, 0)
  y_pred <- c(1, 0, 0, 1)
  group  <- c("A", "A", "B", "B")
  out <- morie:::.frns_rates_from_labels(y_true, y_pred, group, 1)
  expect_named(out, c("A", "B"))
  expect_true(is.numeric(out$A$tpr))
})

test_that(".frns_gini returns 0 on degenerate input + >0 on skew", {
  expect_equal(morie:::.frns_gini(rep(5, 10L)), 0)
  expect_equal(morie:::.frns_gini(numeric(0)), 0)
  expect_true(morie:::.frns_gini(c(rep(0, 99L), 1000)) > 0.9)
})

test_that(".frns_worst_abs returns the largest-magnitude finite value", {
  expect_equal(morie:::.frns_worst_abs(c(0.1, -0.5, 0.3, NaN)), -0.5)
  expect_true(is.na(morie:::.frns_worst_abs(c(NaN, Inf, -Inf))))
})

# =============================================================== siu_analyze.R

test_that(".siu_an_truthy counts true-like values", {
  expect_equal(morie:::.siu_an_truthy(c(TRUE, FALSE, TRUE)), 2L)
  expect_equal(morie:::.siu_an_truthy(c("yes", "no", "Yes", "1")), 3L)
})

test_that(".siu_an_falsy counts false-like values", {
  expect_equal(morie:::.siu_an_falsy(c(TRUE, FALSE, FALSE)), 2L)
  expect_equal(morie:::.siu_an_falsy(c("no", "yes", "false", "0")), 3L)
})

test_that(".siu_an_interval returns 'n/a' on empty / NA dates", {
  out <- morie:::.siu_an_interval("label",
                                    character(0),
                                    character(0))
  expect_length(out, 6L)
  expect_equal(out[[1]], "label")
  expect_equal(out[[2]], "n/a")
})

test_that(".siu_an_interval computes label + n + mean/median/min/max", {
  out <- morie:::.siu_an_interval(
    "Notification",
    c("2024-01-01", "2024-01-10"),
    c("2024-01-05", "2024-01-20"))
  expect_length(out, 6L)
  expect_equal(out[[1]], "Notification")
  expect_equal(out[[2]], 2L)
})
