# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/effects.R

set.seed(2026L)
mk_data <- function(n = 60L) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  z  <- rbinom(n, 1, 0.5)
  d  <- as.integer(plogis(0.3 + 0.8 * z + 0.4 * x1) > runif(n))
  y  <- 1 + 0.5 * d + 0.3 * x1 + 0.2 * x2 + rnorm(n)
  data.frame(y = y, d = d, z = z, x1 = x1, x2 = x2,
             wt = 1 + abs(rnorm(n, mean = 1)))
}

test_that("estimate_ate returns ate and se", {
  df <- mk_data(60L)
  res <- estimate_ate(df, "y", "d", "wt")
  expect_true(is.list(res))
  expect_true(is.numeric(res$ate))
  expect_true(is.numeric(res$se))
  expect_true(res$se > 0)
})

test_that("estimate_plr falls back to base R cross-fit ridge", {
  df <- mk_data(80L)
  res <- estimate_plr(df, "d", "y", c("x1", "x2"), n_folds = 3L)
  expect_true(is.list(res))
  expect_true(is.numeric(res$ate))
  expect_true(is.numeric(res$se))
  expect_true(is.numeric(res$ci_lower))
  expect_true(is.numeric(res$ci_upper))
  expect_true(is.numeric(res$pval))
  expect_equal(res$n_obs, 80L)
  expect_true(is.character(res$method))
})

test_that("estimate_plr errors on missing columns", {
  df <- mk_data(40L)
  expect_error(estimate_plr(df, "d", "y", c("x1", "missing")),
               "missing")
})

test_that("estimate_plr errors on n_folds < 2", {
  df <- mk_data(40L)
  expect_error(estimate_plr(df, "d", "y", c("x1"), n_folds = 1L),
               "n_folds")
})

test_that("estimate_pliv 2SLS fallback works", {
  df <- mk_data(80L)
  # Suppress the fallback warning
  res <- suppressWarnings(
    estimate_pliv(df, "d", "y", "z", c("x1", "x2"))
  )
  expect_true(is.list(res))
  expect_true(is.numeric(res$late))
  expect_true(is.numeric(res$se))
  expect_equal(res$n_obs, 80L)
})

test_that("estimate_pliv errors on missing columns", {
  df <- mk_data(40L)
  expect_error(estimate_pliv(df, "d", "y", "z", c("missing")),
               "missing")
})

test_that("estimate_ate_gcomputation linear path", {
  df <- mk_data(60L)
  res <- estimate_ate_gcomputation(df, "d", "y", c("x1", "x2"),
                                    outcome_model = "linear")
  expect_true(is.list(res))
  expect_true(is.numeric(res$ate))
  expect_equal(res$n_obs, 60L)
  expect_equal(res$outcome_model, "linear")
})

test_that("estimate_ate_gcomputation logistic path", {
  df <- mk_data(60L)
  df$y_bin <- as.integer(df$y > median(df$y))
  res <- estimate_ate_gcomputation(df, "d", "y_bin", c("x1", "x2"),
                                    outcome_model = "logistic")
  expect_true(is.list(res))
  expect_true(is.numeric(res$ate))
  expect_equal(res$outcome_model, "logistic")
})

test_that("estimate_ate_gcomputation rejects bad outcome_model", {
  df <- mk_data(40L)
  expect_error(estimate_ate_gcomputation(df, "d", "y", c("x1"),
                                         outcome_model = "poisson"),
               "outcome_model")
})

test_that("estimate_ate_gcomputation errors on missing columns", {
  df <- mk_data(40L)
  expect_error(estimate_ate_gcomputation(df, "d", "y", c("missing")),
               "missing")
})

test_that("estimate_ate_gcomputation errors on too few obs", {
  df <- mk_data(5L)
  expect_error(estimate_ate_gcomputation(df, "d", "y", c("x1")),
               "10")
})

test_that("sensitivity_rosenbaum returns data frame of gammas", {
  df <- mk_data(40L)
  res <- sensitivity_rosenbaum(df, "d", "y", c("x1"),
                               gamma_range = c(1, 2), n_gamma = 5L)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5L)
  expect_named(res, c("Gamma", "p_lower", "p_upper"))
})

test_that("sensitivity_rosenbaum errors on bad inputs", {
  df <- mk_data(40L)
  expect_error(sensitivity_rosenbaum(df, "d", "missing", c("x1")),
               "missing")
  expect_error(sensitivity_rosenbaum(df, "d", "y", c("x1"),
                                     gamma_range = c(0.5, 2)),
               "Minimum")
  expect_error(sensitivity_rosenbaum(df, "d", "y", c("x1"),
                                     gamma_range = c(2, 1)),
               "gamma_range")
  expect_error(sensitivity_rosenbaum(df, "d", "y", c("x1"),
                                     gamma_range = c(1, 2),
                                     n_gamma = 1L),
               "n_gamma")
})

test_that("sensitivity_rosenbaum errors on too few treated/control", {
  df <- mk_data(20L)
  df$d <- 0L  # no treated
  expect_error(sensitivity_rosenbaum(df, "d", "y", c("x1"),
                                     gamma_range = c(1, 2),
                                     n_gamma = 3L),
               "treated")
})

test_that("e_value returns scalar >= 1", {
  expect_equal(e_value(0, 1), 1)
  v <- e_value(0.5, 0.1)
  expect_true(v >= 1)
  v2 <- e_value(2, 1, null = 0)
  expect_true(v2 >= 1)
})

test_that("e_value errors on non-positive se", {
  expect_error(e_value(0.5, 0), "se must be > 0")
  expect_error(e_value(0.5, -1), "se must be > 0")
})

test_that("e_value sd_y routes through EValue when installed", {
  skip_if_not_installed("EValue")
  v <- e_value(0.5, 0.1, null = 0, sd_y = 1.5)
  expect_true(is.numeric(v) && v >= 1)
})

# ---------------------------------------------------------------------
# Marginal-effects extenders (Phase 1.j)
# ---------------------------------------------------------------------

mk_model <- function(n = 80L) {
  df <- mk_data(n)
  list(
    df  = df,
    fit = stats::lm(y ~ d + x1 + x2, data = df)
  )
}

test_that("morie_effects_emmeans wraps emmeans::emmeans", {
  skip_if_not_installed("emmeans")
  obj <- mk_model()
  em  <- morie_effects_emmeans(obj$fit, specs = ~ d)
  expect_s4_class(em, "emmGrid")
})

test_that("morie_effects_emmeans errors when emmeans absent", {
  skip_if(requireNamespace("emmeans", quietly = TRUE))
  obj <- mk_model()
  expect_error(morie_effects_emmeans(obj$fit, specs = ~ d),
               "emmeans")
})

test_that("morie_effects_predictions wraps marginaleffects::predictions", {
  skip_if_not_installed("marginaleffects")
  obj <- mk_model()
  out <- morie_effects_predictions(obj$fit)
  expect_true(is.data.frame(out) ||
                inherits(out, "predictions"))
  expect_true(nrow(out) > 0L)
})

test_that("morie_effects_predictions respects newdata", {
  skip_if_not_installed("marginaleffects")
  obj <- mk_model()
  nd  <- obj$df[seq_len(5L), ]
  out <- morie_effects_predictions(obj$fit, newdata = nd)
  expect_equal(nrow(out), 5L)
})

test_that("morie_effects_comparisons wraps marginaleffects::comparisons", {
  skip_if_not_installed("marginaleffects")
  obj <- mk_model()
  out <- morie_effects_comparisons(obj$fit, variables = "d")
  expect_true(is.data.frame(out) ||
                inherits(out, "comparisons"))
  expect_true(nrow(out) > 0L)
})

test_that("morie_effects_slopes wraps marginaleffects::slopes", {
  skip_if_not_installed("marginaleffects")
  obj <- mk_model()
  out <- morie_effects_slopes(obj$fit, variables = "x1")
  expect_true(is.data.frame(out) ||
                inherits(out, "slopes"))
  expect_true(nrow(out) > 0L)
})

test_that("morie_effects_tidy uses broom when installed", {
  skip_if_not_installed("broom")
  obj <- mk_model()
  td  <- morie_effects_tidy(obj$fit)
  expect_s3_class(td, "data.frame")
  expect_true(all(c("term", "estimate", "std.error", "p.value") %in%
                    names(td)))
})

test_that("morie_effects_tidy has summary-based fallback", {
  obj <- mk_model()
  # Always works on lm/glm via the fallback path; broom path returns
  # the same canonical columns, so the assertion holds either way.
  td <- morie_effects_tidy(obj$fit)
  expect_s3_class(td, "data.frame")
  expect_true("term" %in% names(td))
  expect_true("estimate" %in% names(td))
})

test_that("estimate_ate_gcomputation stdReg path runs when installed", {
  skip_if_not_installed("stdReg")
  df <- mk_data(60L)
  res <- estimate_ate_gcomputation(df, "d", "y", c("x1", "x2"),
                                    outcome_model = "linear")
  expect_true(is.list(res))
  expect_true(is.numeric(res$ate))
  expect_equal(res$n_obs, 60L)
})
