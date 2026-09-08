# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native DML (PLR + IRM) vs the DoubleML package
# (module 10). Suggests allowed here ONLY.

.dml_linear_dgp <- function(n = 2000L, theta = 0.5, seed = 101L) {
  set.seed(seed)
  X <- matrix(rnorm(n * 5), n, 5)
  d <- rbinom(n, 1, plogis(0.6 * X[, 1] - 0.4 * X[, 2]))
  y <- theta * d + X[, 1] + 0.5 * X[, 2] - 0.3 * X[, 3] + rnorm(n)
  df <- data.frame(y = y, d = d, X)
  names(df)[3:7] <- paste0("x", 1:5)
  df
}

test_that("cross: native PLR agrees with DoubleML on a linear DGP", {
  skip_if_not_installed("DoubleML")
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ranger")
  df <- .dml_linear_dgp()
  covs <- paste0("x", 1:5)

  ours <- morie_estimate_double_ml(df, "y", "d", covs,
                                   n_folds = 5L, random_state = 42L)
  expect_identical(ours$method, "PLR (rmorie native)")

  dml_data <- DoubleML::double_ml_data_from_data_frame(
    df = df, y_col = "y", d_cols = "d", x_cols = covs)
  ml <- mlr3::lrn("regr.ranger", num.trees = 100L, max.depth = 5L)
  .gst <- rmorie:::.morie_dml_guard_begin()
  on.exit(rmorie:::.morie_dml_guard_end(.gst), add = TRUE)
  plr <- DoubleML::DoubleMLPLR$new(dml_data, ml_l = ml, ml_m = ml$clone(),
                                   n_folds = 5L)
  set.seed(42)
  plr$fit()
  ref <- as.numeric(plr$coef[1])
  ref_se <- as.numeric(plr$se[1])

  # both estimators target theta = 0.5; agreement within joint 95% margin
  expect_lt(abs(ours$ate - ref), 1.96 * sqrt(ours$se^2 + ref_se^2))
  expect_lt(abs(ours$ate - 0.5), 3 * ours$se)
  expect_lt(abs(ref - 0.5), 3 * ref_se + 0.05)
})

test_that("cross: native IRM agrees with DoubleML IRM on a linear DGP", {
  skip_if_not_installed("DoubleML")
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ranger")
  df <- .dml_linear_dgp(seed = 102L)
  covs <- paste0("x", 1:5)

  ours <- morie_estimate_irm(df, treatment = "d", outcome = "y",
                             covariates = covs, n_folds = 5L,
                             random_state = 42L)
  expect_identical(ours$method, "IRM (rmorie native)")

  irm_frame <- df
  irm_frame$d <- as.integer(irm_frame$d)
  dml_data <- DoubleML::double_ml_data_from_data_frame(
    df = irm_frame, y_col = "y", d_cols = "d", x_cols = covs)
  ml_g <- mlr3::lrn("regr.ranger", num.trees = 100L, max.depth = 5L)
  ml_m <- mlr3::lrn("classif.ranger", num.trees = 100L, max.depth = 5L,
                    predict_type = "prob")
  .gst <- rmorie:::.morie_dml_guard_begin()
  on.exit(rmorie:::.morie_dml_guard_end(.gst), add = TRUE)
  irm <- DoubleML::DoubleMLIRM$new(dml_data, ml_g = ml_g, ml_m = ml_m,
                                   n_folds = 5L)
  set.seed(42)
  irm$fit()
  ref <- as.numeric(irm$coef[1])
  ref_se <- as.numeric(irm$se[1])

  expect_lt(abs(ours$ate - ref), 1.96 * sqrt(ours$se^2 + ref_se^2))
  expect_lt(abs(ours$ate - 0.5), 3 * ours$se)
})
