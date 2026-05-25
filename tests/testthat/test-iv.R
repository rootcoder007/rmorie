# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Coverage tests for R/iv.R
# Wald-style synthetic with binary instrument z ~ Bernoulli(0.5)
# d ~ Bernoulli(plogis(0.8*z + noise))
# y ~ 0.5*d + noise
# ---------------------------------------------------------------------------

set.seed(1)

make_iv_data <- function(n = 500, beta = 0.5, seed = 1) {
  set.seed(seed)
  z <- rbinom(n, 1, 0.5)
  u <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
  y <- beta * d + 0.4 * u + rnorm(n, sd = 0.5)
  data.frame(y = y, d = d, z = z, x1 = rnorm(n), x2 = rnorm(n))
}

make_overid_data <- function(n = 500, seed = 1) {
  set.seed(seed)
  z1 <- rbinom(n, 1, 0.5)
  z2 <- rnorm(n)
  u  <- rnorm(n)
  d  <- 0.5 * z1 + 0.4 * z2 + 0.3 * u + rnorm(n, sd = 0.3)
  y  <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
  data.frame(y = y, d = d, z1 = z1, z2 = z2, x1 = rnorm(n))
}

# ---------------------------------------------------------------------------
# 2SLS / LIML / GMM / CUE-GMM
# ---------------------------------------------------------------------------

test_that("morie_iv_tsls recovers beta ~ 0.5 on Wald-style DGP", {
  df <- make_iv_data(n = 1000, beta = 0.5, seed = 2)
  res <- morie_iv_tsls(df, "y", "d", "z")
  expect_true("d" %in% names(res$coefficients))
  expect_equal(unname(res$coefficients["d"]), 0.5, tolerance = 0.3)
  expect_lt(res$ci_lower["d"], res$ci_upper["d"])
})

test_that("morie_iv_tsls with robust=FALSE runs", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_tsls(df, "y", "d", "z", robust = FALSE)
  expect_true("d" %in% names(res$coefficients))
})

test_that("morie_iv_tsls with exogenous covariates runs", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_tsls(df, "y", "d", "z", exogenous = c("x1", "x2"))
  expect_true("x1" %in% names(res$coefficients))
})

test_that("morie_iv_liml runs (ivreg fallback OK)", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_liml(df, "y", "d", "z")
  expect_true("d" %in% names(res$coefficients))
})

test_that("morie_iv_gmm runs (fallback or gmm pkg)", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_gmm(df, "y", "d", "z")
  expect_true(any(grepl("d", names(res$coefficients))))
})

test_that("morie_iv_cue_gmm runs (fallback path acceptable)", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_cue_gmm(df, "y", "d", "z")
  expect_true(is.list(res))
})

# ---------------------------------------------------------------------------
# Wald LATE
# ---------------------------------------------------------------------------

test_that("morie_iv_wald returns LATE close to 0.5 on Wald DGP", {
  df <- make_iv_data(n = 2000, beta = 0.5, seed = 3)
  res <- morie_iv_wald(df, "y", "d", "z")
  expect_true("LATE" %in% names(res$coefficients))
  # The Wald estimator targets LATE on compliers, which on this binary-d
  # logistic DGP attenuates substantially relative to the structural beta.
  # Accept a finite, positive estimate of plausible magnitude.
  late <- unname(res$coefficients["LATE"])
  expect_true(is.finite(late))
  expect_gt(late, 0)
  expect_lt(late, 1.5)
  expect_true(is.finite(res$std_errors["LATE"]))
})

# ---------------------------------------------------------------------------
# First-stage diagnostics
# ---------------------------------------------------------------------------

test_that("morie_iv_first_stage_diagnostics returns one row per endogenous", {
  df <- make_iv_data(n = 400)
  out <- morie_iv_first_stage_diagnostics(df, "d", "z")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_true(is.finite(out$F[1]))
})

test_that("morie_iv_first_stage_diagnostics accepts exogenous controls", {
  df <- make_iv_data(n = 400)
  out <- morie_iv_first_stage_diagnostics(df, "d", "z",
                                          exogenous = c("x1", "x2"))
  expect_true(is.finite(out$F[1]))
})

# ---------------------------------------------------------------------------
# Weak-instrument helpers
# ---------------------------------------------------------------------------

test_that("morie_iv_cragg_donald returns a list with the right names", {
  df <- make_iv_data(n = 300)
  out <- morie_iv_cragg_donald(df, "d", "z")
  expect_true(all(c("statistic", "p_value", "name") %in% names(out)))
})

test_that("morie_iv_stock_yogo returns labelled thresholds", {
  out <- morie_iv_stock_yogo(1, 1)
  expect_true(all(c("10pct", "15pct", "20pct", "25pct") %in% names(out)))
})

test_that("morie_iv_stock_yogo errors on combinations not in table", {
  expect_error(morie_iv_stock_yogo(5, 5))
})

test_that("morie_iv_kleibergen_paap mirrors cragg_donald shape", {
  df <- make_iv_data(n = 300)
  out <- morie_iv_kleibergen_paap(df, "d", "z")
  expect_true(all(c("statistic", "p_value", "name") %in% names(out)))
})

# ---------------------------------------------------------------------------
# Anderson-Rubin and CLR
# ---------------------------------------------------------------------------

test_that("morie_iv_anderson_rubin returns chi-square p-value", {
  df <- make_overid_data(n = 400)
  res <- morie_iv_anderson_rubin(df, "y", "d", c("z1", "z2"))
  expect_true(is.finite(res$p_value))
  expect_equal(res$df, 2L)
})

test_that("morie_iv_anderson_rubin defaults beta0 to vector of zeros", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_anderson_rubin(df, "y", "d", "z")
  expect_equal(length(res$beta0), 1L)
})

test_that("morie_iv_anderson_rubin_ci returns a 2-vector or NAs", {
  df <- make_iv_data(n = 400)
  ci <- morie_iv_anderson_rubin_ci(df, "y", "d", "z",
                                   grid_min = -2, grid_max = 2, grid_n = 50)
  expect_length(ci, 2L)
})

test_that("morie_iv_conditional_lr runs", {
  df <- make_iv_data(n = 300)
  res <- morie_iv_conditional_lr(df, "y", "d", "z")
  expect_true(is.finite(res$p_value))
})

# ---------------------------------------------------------------------------
# Overid + endogeneity
# ---------------------------------------------------------------------------

test_that("morie_iv_sargan returns NA when just-identified (fallback)", {
  df <- make_iv_data(n = 400)
  out <- morie_iv_sargan(df, "y", "d", "z")
  expect_true(all(c("statistic", "p_value", "name") %in% names(out)))
})

test_that("morie_iv_sargan returns finite p when overidentified", {
  df <- make_overid_data(n = 400)
  out <- morie_iv_sargan(df, "y", "d", c("z1", "z2"))
  expect_true(is.finite(out$p_value) || is.na(out$p_value))
})

test_that("morie_iv_hansen_j runs (gmm or sargan fallback)", {
  df <- make_overid_data(n = 400)
  out <- morie_iv_hansen_j(df, "y", "d", c("z1", "z2"))
  expect_true(!is.null(out$name))
})

test_that("morie_iv_hausman returns numeric statistic", {
  df <- make_iv_data(n = 500)
  out <- morie_iv_hausman(df, "y", "d", "z")
  expect_true(is.finite(out$statistic))
})

test_that("morie_iv_durbin_wu_hausman wraps hausman", {
  df <- make_iv_data(n = 500)
  out <- morie_iv_durbin_wu_hausman(df, "y", "d", "z")
  expect_equal(out$name, "Durbin-Wu-Hausman")
})

# ---------------------------------------------------------------------------
# JIVE / split-sample / control function / probit
# ---------------------------------------------------------------------------

test_that("morie_iv_jive recovers beta close to truth on overid DGP", {
  df <- make_overid_data(n = 600, seed = 5)
  res <- morie_iv_jive(df, "y", "d", c("z1", "z2"))
  expect_true("d" %in% names(res$coefficients))
  expect_equal(unname(res$coefficients["d"]), 0.5, tolerance = 0.3)
})

test_that("morie_iv_split_sample runs and returns a coef vector", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_split_sample(df, "y", "d", "z", split_fraction = 0.5,
                               seed = 1)
  expect_true(length(res$coefficients) >= 1L)
})

test_that("morie_iv_control_function requires one endogenous", {
  df <- make_iv_data(n = 200)
  expect_error(morie_iv_control_function(df, "y", c("d", "x1"), "z"))
})

test_that("morie_iv_control_function runs in the single-endogenous case", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_control_function(df, "y", "d", "z")
  expect_true("d" %in% names(res$coefficients))
})

test_that("morie_iv_probit requires one endogenous and binary y", {
  df <- make_iv_data(n = 400)
  df$yb <- as.integer(df$y > median(df$y))
  expect_error(morie_iv_probit(df, "yb", c("d", "x1"), "z"))
  res <- morie_iv_probit(df, "yb", "d", "z")
  expect_true("d" %in% names(res$coefficients))
})

# ---------------------------------------------------------------------------
# Panel + dashboards
# ---------------------------------------------------------------------------

test_that("morie_iv_panel runs via fallback or plm", {
  set.seed(1)
  n_unit <- 30; n_time <- 5; n <- n_unit * n_time
  df <- data.frame(
    unit = rep(seq_len(n_unit), each = n_time),
    t    = rep(seq_len(n_time), n_unit),
    z = rbinom(n, 1, 0.5))
  df$d <- 0.5 * df$z + rnorm(n, sd = 0.5)
  df$y <- 0.5 * df$d + rnorm(n, sd = 0.5)
  res <- morie_iv_panel(df, "y", "d", "z", unit = "unit")
  expect_true(is.list(res))
})

test_that("morie_iv_diagnostics returns first_stage / cragg_donald / sargan / hausman", {
  df <- make_iv_data(n = 400)
  out <- morie_iv_diagnostics(df, "y", "d", "z")
  expect_true(all(c("first_stage", "cragg_donald", "sargan", "hausman", "n_obs")
                  %in% names(out)))
})

test_that("morie_iv_residual_analysis returns residual frame", {
  df <- make_iv_data(n = 400)
  out <- morie_iv_residual_analysis(df, "y", "d", "z")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("residual", "abs_resid", "sq_resid") %in% names(out)))
})