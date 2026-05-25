# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Coverage tests for R/validation.R
# ---------------------------------------------------------------------------

set.seed(1)

mk_classify_data <- function(n = 200, seed = 1) {
  set.seed(seed)
  X <- matrix(rnorm(n * 3), n, 3)
  y <- as.integer(plogis(X[, 1] - 0.5 * X[, 2]) > runif(n))
  list(X = X, y = y)
}

logit_fit <- function(X, y) {
  df <- data.frame(y = y, X)
  suppressWarnings(stats::glm(y ~ ., data = df, family = stats::binomial()))
}

logit_predict <- function(model, X) {
  stats::predict(model, newdata = data.frame(X), type = "response")
}

# ---------------------------------------------------------------------------
# column_rule + validate_schema
# ---------------------------------------------------------------------------

test_that("column_rule constructs the expected list", {
  r <- column_rule("x", dtype = "numeric", min_val = 0, max_val = 10)
  expect_s3_class(r, "morie_column_rule")
  expect_equal(r$min_val, 0)
  expect_equal(r$max_val, 10)
})

test_that("validate_schema passes a clean frame", {
  d <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
  res <- validate_schema(d, list(
    column_rule("x", dtype = "numeric"),
    column_rule("y", dtype = "character")))
  expect_true(res$passed)
})

test_that("validate_schema flags missing required column", {
  d <- data.frame(x = 1:5)
  res <- validate_schema(d, list(column_rule("missing", required = TRUE)))
  expect_false(res$passed)
  expect_true(length(res$errors) > 0L)
})

test_that("validate_schema raise_on_error throws", {
  d <- data.frame(x = 1:5)
  expect_error(validate_schema(d, list(column_rule("missing")),
                               raise_on_error = TRUE))
})

test_that("validate_schema catches dtype mismatch", {
  d <- data.frame(x = letters[1:5])
  res <- validate_schema(d, list(column_rule("x", dtype = "numeric")))
  expect_false(res$passed)
})

test_that("validate_schema range checks fire", {
  d <- data.frame(x = c(-1, 1, 5, 11))
  res <- validate_schema(d, list(
    column_rule("x", dtype = "numeric", min_val = 0, max_val = 10)))
  expect_false(res$passed)
  expect_true(any(grepl("below min", res$errors)))
  expect_true(any(grepl("above max", res$errors)))
})

test_that("validate_schema allowed_values catches outliers", {
  d <- data.frame(x = c("a", "b", "c"))
  res <- validate_schema(d, list(
    column_rule("x", allowed_values = c("a", "b"))))
  expect_false(res$passed)
})

test_that("validate_schema uniqueness check fires", {
  d <- data.frame(x = c(1, 2, 2, 3))
  res <- validate_schema(d, list(
    column_rule("x", dtype = "numeric", unique = TRUE)))
  expect_false(res$passed)
})

test_that("validate_schema regex catches non-matches", {
  d <- data.frame(x = c("abc-1", "abc-2", "BAD"), stringsAsFactors = FALSE)
  res <- validate_schema(d, list(column_rule("x", regex = "^abc-")))
  expect_false(res$passed)
})

test_that("validate_schema custom predicate runs", {
  d <- data.frame(x = c(1, 2, 3))
  ok <- validate_schema(d, list(column_rule(
    "x", custom = function(c) all(c > 0))))
  bad <- validate_schema(d, list(column_rule(
    "x", custom = function(c) all(c > 100))))
  expect_true(ok$passed)
  expect_false(bad$passed)
})

test_that("validate_schema custom that raises is reported", {
  d <- data.frame(x = c(1, 2))
  res <- validate_schema(d, list(column_rule(
    "x", custom = function(c) stop("boom"))))
  expect_false(res$passed)
})

test_that("validate_schema null_threshold and nullable=FALSE both fire", {
  d <- data.frame(x = c(1, NA, NA, NA))
  res1 <- validate_schema(d, list(
    column_rule("x", dtype = "numeric", nullable = FALSE)))
  expect_false(res1$passed)
  res2 <- validate_schema(d, list(
    column_rule("x", dtype = "numeric", null_threshold = 0.5)))
  expect_false(res2$passed)
})

# ---------------------------------------------------------------------------
# referential integrity
# ---------------------------------------------------------------------------

test_that("check_referential_integrity passes when all FKs are in PKs", {
  child <- data.frame(fk = c(1, 2, 3))
  parent <- data.frame(pk = c(1, 2, 3, 4))
  res <- check_referential_integrity(child, parent, "fk", "pk")
  expect_true(res$passed)
})

test_that("check_referential_integrity flags orphans", {
  child <- data.frame(fk = c(1, 2, 999))
  parent <- data.frame(pk = c(1, 2, 3))
  res <- check_referential_integrity(child, parent, "fk", "pk")
  expect_false(res$passed)
})

# ---------------------------------------------------------------------------
# score_data_quality
# ---------------------------------------------------------------------------

test_that("score_data_quality returns components in [0, 1]", {
  set.seed(1)
  d <- data.frame(a = c(1, NA, 3, 4), b = c("x", "y", "y", "z"),
                  stringsAsFactors = FALSE)
  res <- score_data_quality(d)
  for (k in c("completeness", "consistency", "timeliness", "uniqueness", "overall")) {
    expect_true(res[[k]] >= 0 && res[[k]] <= 1)
  }
})

test_that("score_data_quality runs date_cols and key_cols branches", {
  d <- data.frame(
    a = c(1, 2, 2),
    dt = as.POSIXct(c("2024-01-01", "2024-06-01", "2024-12-01")))
  res <- score_data_quality(d, date_cols = "dt", key_cols = "a",
                            consistency_rules = list(function(df) all(df$a > 0)))
  expect_true("key_duplicates" %in% names(res$details))
  expect_true(any(grepl("^timeliness_", names(res$details))))
})

# ---------------------------------------------------------------------------
# cross_validate
# ---------------------------------------------------------------------------

test_that("cross_validate kfold returns named scalar means", {
  d <- mk_classify_data(200)
  res <- cross_validate(logit_fit, logit_predict, d$X, d$y,
                        method = "kfold", n_folds = 5L)
  expect_true(is.finite(res$mean))
  expect_lt(res$ci_lower, res$ci_upper)
})

test_that("cross_validate stratified_kfold runs", {
  d <- mk_classify_data(200)
  res <- cross_validate(logit_fit, logit_predict, d$X, d$y,
                        method = "stratified_kfold", n_folds = 4L)
  expect_true(is.finite(res$mean))
})

test_that("cross_validate grouped_kfold requires groups", {
  d <- mk_classify_data(100)
  expect_error(cross_validate(logit_fit, logit_predict, d$X, d$y,
                              method = "grouped_kfold", n_folds = 3L))
})

test_that("cross_validate grouped_kfold runs with groups", {
  d <- mk_classify_data(120)
  grp <- rep(1:6, each = 20)
  res <- cross_validate(logit_fit, logit_predict, d$X, d$y,
                        method = "grouped_kfold", n_folds = 3L, groups = grp)
  expect_true(is.finite(res$mean))
})

test_that("cross_validate monte_carlo runs", {
  d <- mk_classify_data(120)
  res <- cross_validate(logit_fit, logit_predict, d$X, d$y,
                        method = "monte_carlo", n_folds = 5L, n_repeats = 4L,
                        scoring = "accuracy")
  expect_true(is.finite(res$mean))
})

test_that("cross_validate time_series runs", {
  d <- mk_classify_data(120)
  res <- cross_validate(logit_fit, logit_predict, d$X, d$y,
                        method = "time_series", n_folds = 4L,
                        scoring = "accuracy")
  expect_true(is.finite(res$mean))
})

test_that("cross_validate brier scoring runs", {
  d <- mk_classify_data(120)
  res <- cross_validate(logit_fit, logit_predict, d$X, d$y,
                        method = "kfold", n_folds = 3L, scoring = "brier")
  expect_true(is.finite(res$mean))
})

test_that("cross_validate errors on unknown method", {
  d <- mk_classify_data(80)
  expect_error(cross_validate(logit_fit, logit_predict, d$X, d$y,
                              method = "no_method"))
})

# ---------------------------------------------------------------------------
# nested_cross_validate
# ---------------------------------------------------------------------------

test_that("nested_cross_validate legacy stub form runs", {
  d <- mk_classify_data(120)
  res <- nested_cross_validate(tune_fn = logit_fit,
                               predict_fn = logit_predict,
                               X = d$X, y = d$y,
                               outer_folds = 3L)
  expect_true(is.finite(res$mean))
})

test_that("nested_cross_validate full form runs with dummy grid", {
  d <- mk_classify_data(120)
  fit_hp <- function(X, y, hp) logit_fit(X, y)
  res <- nested_cross_validate(fit_fn = fit_hp, predict_fn = logit_predict,
                               X = d$X, y = d$y,
                               hyperparam_grid = list(dummy = c(1, 2)),
                               outer_k = 3L, inner_k = 2L)
  expect_true(is.finite(res$mean_score))
  expect_equal(length(res$outer_scores), 3L)
})

test_that("nested_cross_validate errors when required args missing", {
  expect_error(nested_cross_validate())
})

test_that("nested_cross_validate errors when outer_k > n", {
  d <- mk_classify_data(10)
  fit_hp <- function(X, y, hp) logit_fit(X, y)
  expect_error(nested_cross_validate(fit_fn = fit_hp, predict_fn = logit_predict,
                                     X = d$X, y = d$y,
                                     hyperparam_grid = list(dummy = c(1)),
                                     outer_k = 20L, inner_k = 2L))
})

# ---------------------------------------------------------------------------
# bootstrap_validate
# ---------------------------------------------------------------------------

test_that("bootstrap_validate 632 form runs", {
  d <- mk_classify_data(120)
  res <- bootstrap_validate(logit_fit, logit_predict, d$X, d$y,
                            n_bootstraps = 20L, method = "632",
                            scoring = "accuracy")
  expect_true(is.finite(res$mean))
})

test_that("bootstrap_validate 632plus form runs", {
  d <- mk_classify_data(120)
  res <- bootstrap_validate(logit_fit, logit_predict, d$X, d$y,
                            n_bootstraps = 20L, method = "632plus",
                            scoring = "accuracy")
  expect_true(is.finite(res$mean))
})

test_that("bootstrap_validate errors on unknown method", {
  d <- mk_classify_data(60)
  expect_error(bootstrap_validate(logit_fit, logit_predict, d$X, d$y,
                                  n_bootstraps = 5L, method = "999"))
})

# ---------------------------------------------------------------------------
# assess_calibration + assess_discrimination + decision_curve_analysis
# ---------------------------------------------------------------------------

test_that("assess_calibration returns Hosmer-Lemeshow + slope + Brier", {
  set.seed(1)
  y <- rbinom(200, 1, 0.4)
  p <- plogis(rnorm(200, mean = -0.4))
  res <- assess_calibration(y, p, n_groups = 10L)
  for (k in c("hosmer_lemeshow_stat", "calibration_slope",
              "calibration_intercept", "brier_score", "scaled_brier",
              "calibration_in_the_large")) {
    expect_true(is.finite(res[[k]]))
  }
})

test_that("assess_discrimination returns AUROC in [0, 1] + CI", {
  set.seed(1)
  d <- mk_classify_data(200)
  p <- plogis(d$X %*% c(1, -0.5, 0))
  res <- assess_discrimination(d$y, p, n_bootstrap = 50L)
  expect_gt(res$auroc, 0.5)
  expect_lt(res$auroc, 1)
  expect_lte(res$auroc_ci_lower, res$auroc_ci_upper)
})

test_that("assess_discrimination NRI/IDI branch runs with ref", {
  set.seed(1)
  d <- mk_classify_data(200)
  p <- plogis(d$X %*% c(1, -0.5, 0))
  p_ref <- plogis(d$X %*% c(0.5, 0, 0))
  res <- assess_discrimination(d$y, p, y_pred_ref = p_ref, n_bootstrap = 50L)
  expect_true(is.finite(res$nri))
  expect_true(is.finite(res$idi))
})

test_that("decision_curve_analysis returns matching-length vectors", {
  set.seed(1)
  d <- mk_classify_data(200)
  p <- plogis(d$X %*% c(1, -0.5, 0))
  res <- decision_curve_analysis(d$y, p)
  expect_equal(length(res$net_benefit), length(res$thresholds))
  expect_equal(length(res$net_benefit_all), length(res$thresholds))
})

# ---------------------------------------------------------------------------
# detect_overfitting
# ---------------------------------------------------------------------------

test_that("detect_overfitting reports recommendation", {
  d <- mk_classify_data(120)
  res <- detect_overfitting(logit_fit, logit_predict, d$X, d$y,
                            n_bootstrap = 20L, scoring = "accuracy")
  expect_true(is.character(res$recommendation))
  expect_true(is.finite(res$optimism))
})

# ---------------------------------------------------------------------------
# temporal_validate
# ---------------------------------------------------------------------------

test_that("temporal_validate splits by date and runs", {
  set.seed(1)
  n <- 200
  X <- as.data.frame(matrix(rnorm(n * 2), n, 2))
  names(X) <- c("a", "b")
  X$date <- as.POSIXct("2024-01-01") + (seq_len(n) - 1) * 86400
  y <- as.integer(plogis(X$a) > runif(n))
  fit_fn <- function(X, y) logit_fit(X, y)
  predict_fn <- function(m, X) logit_predict(m, X)
  res <- temporal_validate(fit_fn, predict_fn, X, y, "date",
                           scoring = "accuracy")
  expect_true(is.finite(res$train_score))
  expect_true(is.finite(res$test_score))
})

test_that("temporal_validate errors when test has only one class", {
  set.seed(1)
  X <- data.frame(a = rnorm(40))
  X$date <- as.POSIXct("2024-01-01") + (seq_len(40) - 1) * 86400
  y <- c(rep(0, 20), rep(0, 20))
  fit_fn <- function(X, y) logit_fit(X, y)
  predict_fn <- function(m, X) logit_predict(m, X)
  expect_error(temporal_validate(fit_fn, predict_fn, X, y, "date"))
})

# ---------------------------------------------------------------------------
# external_validate
# ---------------------------------------------------------------------------

test_that("external_validate returns discrimination + calibration", {
  set.seed(1)
  d <- mk_classify_data(200)
  mdl <- logit_fit(d$X, d$y)
  predict_fn <- function(X) logit_predict(mdl, X)
  res <- external_validate(predict_fn, d$X, d$y)
  expect_true(is.list(res$discrimination))
  expect_true(is.list(res$calibration))
})

test_that("external_validate domain-shift KS branch runs", {
  set.seed(1)
  d <- mk_classify_data(200)
  mdl <- logit_fit(d$X, d$y)
  predict_fn <- function(X) logit_predict(mdl, X)
  d2 <- mk_classify_data(150, seed = 2)
  res <- external_validate(predict_fn, d2$X, d2$y, X_development = d$X)
  expect_true(length(res$domain_shift) > 0L)
})

# ---------------------------------------------------------------------------
# create_reproducibility_manifest
# ---------------------------------------------------------------------------

test_that("create_reproducibility_manifest returns expected fields", {
  d <- data.frame(x = 1:5)
  mf <- create_reproducibility_manifest(d, parameters = list(seed = 1),
                                        seeds = list(global = 1L))
  for (k in c("r_version", "package_versions", "random_seeds",
              "parameters", "timestamp")) {
    expect_true(!is.null(mf[[k]]))
  }
})