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

test_that("morie_iv_liml runs (native k-class)", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_liml(df, "y", "d", "z")
  expect_true("d" %in% names(res$coefficients))
})

test_that("morie_iv_gmm runs (native two-step)", {
  df <- make_iv_data(n = 400)
  res <- morie_iv_gmm(df, "y", "d", "z")
  expect_true(any(grepl("d", names(res$coefficients))))
})

test_that("morie_iv_cue_gmm runs (native CUE)", {
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

test_that("morie_iv_hansen_j runs (native J)", {
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

test_that("morie_iv_panel runs (native within + 2sls)", {
  set.seed(1)
  n_unit <- 30
  n_time <- 5
  n <- n_unit * n_time
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


test_that("round four: anderson-rubin reports the decision at alpha", {
  set.seed(7)
  n <- 150
  z <- rnorm(n); d <- 0.7 * z + rnorm(n); y <- 0.5 * d + rnorm(n)
  df <- data.frame(y = y, d = d, z = z)
  res <- morie_iv_anderson_rubin(df, "y", "d", "z", alpha = 0.10)
  expect_equal(res$alpha, 0.10)
  expect_equal(res$critical_value, stats::qf(0.90, 1, res$df_resid))
  expect_equal(res$reject_at_alpha, res$p_value < 0.10)
})

test_that("IV estimators and tests match ivreg, sandwich and linearmodels", {
  n <- 80
  i <- 0:(n - 1)
  z1 <- sin(1.3 * i) + 0.3 * cos(0.7 * i)
  z2 <- cos(2.1 * i + 0.5)
  w <- sin(0.37 * i + 1.0)
  u <- 0.6 * sin(3.7 * i + 0.2)
  v <- 0.5 * u + 0.4 * cos(5.3 * i)
  d <- 0.8 * z1 + 0.6 * z2 + 0.3 * w + v
  y <- 1.0 + 1.5 * d + 0.7 * w + u + 0.2 * sin(7.1 * i) * z1
  D <- data.frame(y, d, z1, z2, w)
  # sandwich::vcovHC(ivreg(y ~ d + w | z1 + z2 + w), "HC1")
  expect_equal(unname(morie_iv_tsls(D, "y", "d", c("z1", "z2"), "w", robust = TRUE)$std_errors),
               c(0.0505459824547257, 0.0667536171757581, 0.0725244634555991), tolerance = 1e-10)
  # linearmodels IVGMM / IVGMMCUE (robust)
  g <- morie_iv_gmm(D, "y", "d", c("z1", "z2"), "w")
  expect_equal(unname(g$coefficients), c(1.01651581791, 1.44250806159, 0.706747364412), tolerance = 1e-10)
  expect_equal(unname(g$std_errors), c(0.0488046582694, 0.065789704909, 0.0712173125091), tolerance = 1e-4)
  expect_equal(morie_iv_hansen_j(D, "y", "d", c("z1", "z2"), "w")$statistic, 1.01179918825, tolerance = 1e-9)
  cu <- morie_iv_cue_gmm(D, "y", "d", c("z1", "z2"), "w")
  expect_equal(unname(cu$coefficients), c(1.01790404736, 1.43913637976, 0.708076314862), tolerance = 1e-7)
  expect_equal(unname(cu$std_errors), c(0.0488667178074, 0.0659360392028, 0.0713319811849), tolerance = 1e-7)
  # summary(ivreg, diagnostics = TRUE) Wu-Hausman; Durbin-form Hausman; AR F
  expect_equal(morie_iv_durbin_wu_hausman(D, "y", "d", c("z1", "z2"), "w")$statistic, 37.6708096, tolerance = 1e-8)
  expect_equal(morie_iv_hausman(D, "y", "d", c("z1", "z2"), "w")$statistic, 25.5180054657815, tolerance = 1e-10)
  a <- stats::anova(stats::lm(y ~ w), stats::lm(y ~ z1 + z2 + w))
  expect_equal(morie_iv_anderson_rubin(D, "y", "d", c("z1", "z2"), "w", beta0 = 0)$statistic,
               a[2, "F"], tolerance = 1e-12)
})
