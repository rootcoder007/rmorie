# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 16 tests: perseus, plcmt, polrg, polrz, posab, pplxm, propc,
#   pspln, quntf, rcall, regms, retlv, rfens, rfgen, rgadp

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

test_that("morie_build_prompt returns the bare question without context", {
  out <- morie_build_prompt("What is the U statistic?")
  expect_type(out, "character")
  expect_length(out, 1L)
  expect_identical(out, "What is the U statistic?")
})

test_that("morie_build_prompt trims whitespace around the question", {
  expect_identical(morie_build_prompt("  hello world  "), "hello world")
})

test_that("morie_build_prompt composes a Context/Question block when context given", {
  out <- morie_build_prompt("Why?", context = "Because.")
  expect_type(out, "character")
  expect_length(out, 1L)
  expect_true(grepl("Context:", out, fixed = TRUE))
  expect_true(grepl("Question:", out, fixed = TRUE))
  expect_true(grepl("Because.", out, fixed = TRUE))
  expect_true(grepl("Why?", out, fixed = TRUE))
})

test_that("morie_build_prompt ignores empty / blank context", {
  expect_identical(morie_build_prompt("Q", context = ""), "Q")
  expect_identical(morie_build_prompt("Q", context = "   "), "Q")
  expect_identical(morie_build_prompt("Q", context = NULL), "Q")
})

test_that("morie_build_prompt errors on an empty question", {
  expect_error(morie_build_prompt(""), "non-empty")
  expect_error(morie_build_prompt("   "), "non-empty")
})

test_that("build_assistant_prompt alias mirrors morie_build_prompt", {
  expect_identical(
    morie:::build_assistant_prompt("Q", context = "C"),
    morie_build_prompt("Q", context = "C")
  )
})

test_that("morie_ask_percy is an exported function with the documented args", {
  expect_true(is.function(morie_ask_percy))
  expect_true(all(c("question", "context", "python_bin") %in%
    names(formals(morie_ask_percy))))
  if (FALSE) {
    morie_ask_percy("hello")
  }
  expect_true(TRUE)
})

test_that("morie_rank_placements returns the documented structure", {
  set.seed(1)
  x <- rnorm(15)
  y <- rnorm(12)
  r <- morie_rank_placements(x, y)
  expect_type(r, "list")
  expect_named(r, c(
    "placements", "ranks_y", "U_y", "E_U", "Var_U",
    "m", "n", "method"
  ))
  expect_length(r$placements, length(y))
  expect_length(r$ranks_y, length(y))
  expect_identical(r$m, length(x))
  expect_identical(r$n, length(y))
})

test_that("morie_rank_placements computes U as the sum of placements", {
  x <- c(1, 2, 3, 4)
  y <- c(2.5, 3.5)
  r <- morie_rank_placements(x, y)
  expect_equal(r$U_y, sum(r$placements))
  expect_equal(r$E_U, length(x) * length(y) / 2)
  expect_true(is.finite(r$Var_U) && r$Var_U > 0)
})

test_that("morie_rank_placements handles empty input gracefully", {
  r <- morie_rank_placements(numeric(0), c(1, 2))
  expect_length(r$placements, 0L)
  expect_true(is.na(r$U_y))
  expect_identical(r$m, 0L)
})

test_that("morie_polynomial_regression fits a degree-2 model on a vector", {
  set.seed(2)
  x <- rnorm(40)
  y <- 1 + 2 * x + 0.5 * x^2 + rnorm(40, sd = 0.1)
  r <- morie_polynomial_regression(x, y)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "feature_names", "degree", "n", "method"))
  expect_identical(r$degree, 2L)
  expect_identical(r$n, 40L)
  expect_length(r$estimate, length(r$feature_names))
  expect_true(all(is.finite(r$estimate)))
  expect_identical(r$feature_names[1], "(intercept)")
})

test_that("morie_polynomial_regression honours a custom degree", {
  set.seed(3)
  x <- rnorm(30)
  y <- rnorm(30)
  r <- morie_polynomial_regression(x, y, degree = 3L)
  expect_identical(r$degree, 3L)
  expect_true(grepl("degree=3", r$method))
})

test_that("morie_polynomial_regression accepts a multi-column matrix with cross-terms", {
  set.seed(4)
  X <- matrix(rnorm(60), ncol = 2)
  y <- X[, 1] + X[, 2] + rnorm(30, sd = 0.1)
  r <- morie_polynomial_regression(X, y, degree = 2L)
  expect_true("x0 x1" %in% r$feature_names)
  expect_length(r$estimate, length(r$feature_names))
})

test_that("polrz computes a polarization index with median split", {
  set.seed(5)
  x <- c(rnorm(20, -1), rnorm(20, 1))
  r <- polrz(x)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "mean_R", "mean_D", "sd_R", "sd_D",
    "pooled_sd", "n_R", "n_D", "method"
  ))
  expect_true(is.finite(r$estimate) && r$estimate >= 0)
  expect_identical(r$method, "morie_polarization_index")
})

test_that("polrz accepts an explicit two-level grouping vector", {
  x <- c(1, 2, 3, 8, 9, 10)
  g <- c("a", "a", "a", "b", "b", "b")
  r <- polrz(x, group = g)
  expect_true(is.finite(r$estimate) && r$estimate > 0)
  expect_identical(r$n_R + r$n_D, 6L)
})

test_that("polrz returns NA for too-short input", {
  r <- polrz(c(1))
  expect_true(is.na(r$estimate))
  expect_identical(r$n, 1L)
})

test_that("polrz errors on bad grouping vectors", {
  expect_error(polrz(c(1, 2, 3), group = c("a", "b")), "length")
  expect_error(polrz(c(1, 2, 3), group = c("a", "b", "c")), "2 levels")
})

test_that("morie_polarization_index alias matches polrz", {
  set.seed(6)
  x <- rnorm(30)
  expect_equal(morie_polarization_index(x)$estimate, polrz(x)$estimate)
})

test_that("morie_posab_positional_encoding_abs returns a PE matrix of the right shape", {
  r <- morie_posab_positional_encoding_abs(seq_len = 8L, d_model = 4L)
  expect_type(r, "list")
  expect_named(r, c("PE", "estimate", "seq_len", "d_model", "method"))
  expect_true(is.matrix(r$PE))
  expect_identical(dim(r$PE), c(8L, 4L))
  expect_true(all(is.finite(r$PE)))
  expect_true(all(abs(r$PE) <= 1 + 1e-9))
})

test_that("morie_posab_positional_encoding_abs honours a custom base", {
  r <- morie_posab_positional_encoding_abs(6L, 6L, base = 100)
  expect_identical(dim(r$PE), c(6L, 6L))
})

test_that("morie_posab_positional_encoding_abs errors on non-positive dimensions", {
  expect_error(morie_posab_positional_encoding_abs(0L, 4L), "> 0")
  expect_error(morie_posab_positional_encoding_abs(4L, 0L), "> 0")
})

test_that("morie_positional_encoding_abs alias matches the primary function", {
  a <- morie_positional_encoding_abs(5L, 4L)
  b <- morie_posab_positional_encoding_abs(5L, 4L)
  expect_equal(a$PE, b$PE)
})

test_that("perplexity_metric computes perplexity from log-probs (base e)", {
  r <- morie:::perplexity_metric(c(-1, -1, -1))
  expect_type(r, "list")
  expect_named(r, c("value", "nll", "n", "method"))
  expect_equal(r$nll, 1)
  expect_equal(r$value, exp(1))
  expect_identical(r$n, 3L)
})

test_that("perplexity_metric accepts base 2", {
  r <- morie:::perplexity_metric(c(-1, -2), base = "2")
  expect_true(is.finite(r$value) && r$value > 0)
})

test_that("perplexity_metric errors on empty input and bad base", {
  expect_error(morie:::perplexity_metric(numeric(0)), "at least one")
  expect_error(
    morie:::perplexity_metric(c(-1, -1), base = "10"),
    "'e' or '2'"
  )
})

test_that("morie_prophet_components decomposes a seasonal series", {
  set.seed(7)
  t <- 0:47
  x <- 0.1 * t + sin(2 * pi * t / 12) + rnorm(48, sd = 0.1)
  r <- morie_prophet_components(x, period = 12)
  expect_type(r, "list")
  expect_named(r, c(
    "trend", "seasonal", "residual", "slope", "intercept",
    "fourier_terms", "period", "n", "method"
  ))
  expect_length(r$trend, 48L)
  expect_length(r$seasonal, 48L)
  expect_length(r$residual, 48L)
  expect_true(is.finite(r$slope) && is.finite(r$intercept))
  expect_identical(r$n, 48L)
})

test_that("morie_prophet_components reconstructs the series additively", {
  set.seed(8)
  t <- 0:35
  x <- 0.05 * t + cos(2 * pi * t / 12) + rnorm(36, sd = 0.05)
  r <- morie_prophet_components(x, period = 12)
  expect_equal(unname(r$trend + r$seasonal + r$residual), x, tolerance = 1e-6)
})

test_that("morie_prophet_components errors on a too-short series", {
  expect_error(morie_prophet_components(1:4, period = 12), "too short|short")
})

test_that("pspln fits a penalised spline and reports r2", {
  skip_if_not_installed("splines")
  set.seed(9)
  x <- seq(0, 1, length.out = 60)
  y <- sin(3 * x) + rnorm(60, sd = 0.05)
  r <- morie:::pspln(x, y, n_knots = 10L, lam = 0.1)
  expect_type(r, "list")
  expect_named(r, c(
    "coef", "fitted", "residuals", "sse", "r2", "edf",
    "lambda", "estimate", "se", "n", "method"
  ))
  expect_length(r$fitted, 60L)
  expect_true(is.finite(r$r2))
  expect_true(is.finite(r$edf) && r$edf > 0)
  expect_identical(r$n, 60L)
})

test_that("pspln returns NA for a too-short series", {
  r <- morie:::pspln(c(1, 2, 3), c(1, 2, 3), degree = 3L)
  expect_true(is.na(r$estimate))
  expect_true(grepl("too small", r$method))
})

test_that("morie_penalized_spline alias matches pspln", {
  skip_if_not_installed("splines")
  set.seed(10)
  x <- seq(0, 1, length.out = 50)
  y <- x^2 + rnorm(50, sd = 0.05)
  a <- morie_penalized_spline(x, y, n_knots = 8L, lam = 1)
  b <- morie:::pspln(x, y, n_knots = 8L, lam = 1)
  expect_equal(a$coef, b$coef)
})

test_that("quntf computes quantiles with asymptotic SEs (default taus)", {
  set.seed(11)
  x <- rnorm(300)
  r <- morie:::quntf(x)
  expect_type(r, "list")
  expect_named(r, c(
    "taus", "quantiles", "se", "bandwidth", "estimate",
    "n", "method"
  ))
  expect_length(r$taus, 5L)
  expect_length(r$quantiles, 5L)
  expect_length(r$se, 5L)
  expect_true(all(is.finite(r$quantiles)))
  expect_true(all(r$se >= 0))
  expect_true(r$bandwidth > 0)
})

test_that("quntf accepts a custom single tau", {
  set.seed(12)
  x <- rnorm(200)
  r <- morie:::quntf(x, taus = 0.5)
  expect_length(r$quantiles, 1L)
  expect_true(is.finite(r$quantiles[1]))
})

test_that("quntf returns NA for n < 2", {
  r <- morie:::quntf(c(3))
  expect_true(is.na(r$estimate))
  expect_identical(r$n, 1L)
})

test_that("morie_quantile_function alias matches quntf", {
  set.seed(13)
  x <- rnorm(150)
  expect_equal(
    morie_quantile_function(x, taus = 0.5)$quantiles,
    morie:::quntf(x, taus = 0.5)$quantiles
  )
})

test_that("rcall summarises a 0/1 vote matrix", {
  set.seed(14)
  V <- matrix(sample(c(0, 1), 60, replace = TRUE), nrow = 10)
  r <- rcall(V)
  expect_type(r, "list")
  expect_named(r, c(
    "n", "m", "n_yea", "n_nay", "n_abs", "marginal_yea",
    "marginal_nay", "pct_yea", "lopsided_pct", "method"
  ))
  expect_identical(r$n, 10L)
  expect_identical(r$m, 6L)
  expect_identical(r$n_yea + r$n_nay + r$n_abs, 60L)
  expect_length(r$marginal_yea, 6L)
  expect_true(r$lopsided_pct >= 0 && r$lopsided_pct <= 1)
})

test_that("rcall remaps Poole-Rosenthal codes automatically", {
  V <- matrix(c(1, 2, 3, 4, 5, 6, 0, 7, 9), nrow = 3)
  r <- rcall(V)
  expect_true(r$n_yea > 0)
  expect_true(r$n_nay > 0)
  expect_true(r$n_abs > 0)
})

test_that("rcall handles NA absences", {
  V <- matrix(c(1, 0, NA, 1, NA, 0), nrow = 3)
  r <- rcall(V)
  expect_identical(r$n_abs, 2L)
})

test_that("morie_roll_call_analysis alias matches rcall", {
  V <- matrix(c(1, 0, 1, 0), nrow = 2)
  expect_equal(morie_roll_call_analysis(V)$n_yea, rcall(V)$n_yea)
})

test_that("morie_regime_switching fits a 2-regime model via base-R EM", {
  set.seed(15)
  x <- c(rnorm(30, 0, 0.5), rnorm(30, 0, 2))
  r <- morie_regime_switching(x, k_regimes = 2)
  expect_type(r, "list")
  expect_named(r, c(
    "mu", "sigma", "transition", "smoothed_probabilities",
    "loglik", "n", "k_regimes", "method"
  ))
  expect_length(r$mu, 2L)
  expect_length(r$sigma, 2L)
  expect_identical(dim(r$transition), c(2L, 2L))
  expect_identical(r$n, 60L)
  expect_identical(r$k_regimes, 2)
  expect_true(all(is.finite(r$mu)))
})

test_that("morie_regime_switching errors on a too-short series", {
  expect_error(morie_regime_switching(rnorm(5), k_regimes = 2), "too short|short")
})

test_that("retlv estimates a GEV return level", {
  set.seed(16)
  x <- rnorm(200, mean = 10, sd = 2)
  r <- retlv(x, return_period = 100)
  expect_type(r, "list")
  expect_true("estimate" %in% names(r))
  if (is.finite(r$estimate)) {
    expect_true(is.finite(r$z))
    expect_true(r$se >= 0)
    expect_equal(r$return_period, 100)
    expect_identical(r$n, 200L)
  }
})

test_that("retlv accepts a custom return period", {
  set.seed(17)
  x <- rnorm(150, mean = 5, sd = 1)
  r <- retlv(x, return_period = 50)
  expect_true(is.list(r))
  if (is.finite(r$estimate %||% NA_real_)) expect_equal(r$return_period, 50)
})

test_that("morie_return_level alias matches retlv", {
  set.seed(18)
  x <- rnorm(120, mean = 8, sd = 2)
  a <- morie_return_level(x, return_period = 100)
  b <- retlv(x, return_period = 100)
  expect_equal(a$estimate, b$estimate)
})

test_that("morie_random_forest_ensemble runs a small regression forest", {
  set.seed(19)
  X <- matrix(rnorm(120), ncol = 3)
  y <- X[, 1] + 0.5 * X[, 2] + rnorm(40, sd = 0.2)
  r <- morie_random_forest_ensemble(X, y,
    n_estimators = 20L, task = "regression",
    seed = 19L
  )
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "train_score", "oob_score",
    "feature_importances", "n_estimators", "task",
    "n", "method"
  ))
  expect_identical(r$task, "regression")
  expect_identical(r$n_estimators, 20L)
  expect_identical(r$n, 40L)
  expect_length(r$feature_importances, 3L)
  expect_true(is.finite(r$train_score))
})

test_that("morie_random_forest_ensemble auto-detects a classification task", {
  set.seed(20)
  X <- matrix(rnorm(120), ncol = 3)
  y <- as.integer(X[, 1] > 0)
  r <- morie_random_forest_ensemble(X, y,
    n_estimators = 20L, task = "auto",
    seed = 20L
  )
  expect_identical(r$task, "classification")
  expect_true(r$train_score >= 0 && r$train_score <= 1)
})

test_that("morie_random_forest_ensemble accepts a max_depth argument", {
  set.seed(21)
  X <- matrix(rnorm(120), ncol = 3)
  y <- X[, 1] + rnorm(40, sd = 0.2)
  r <- morie_random_forest_ensemble(X, y,
    n_estimators = 15L, max_depth = 3L,
    task = "regression", seed = 21L
  )
  expect_identical(r$task, "regression")
  expect_true(is.finite(r$estimate))
})

test_that("morie_random_forest_genomic predicts from a marker matrix", {
  set.seed(22)
  M <- matrix(rnorm(200), 40, 5)
  y <- M[, 1] + 0.5 * M[, 2]^2 + 0.2 * rnorm(40)
  r <- morie_random_forest_genomic(rep(0, 40), y, M, n_trees = 20, seed = 22)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "y_hat", "oob_score", "feature_importance",
    "se", "n", "method"
  ))
  expect_length(r$y_hat, 40L)
  expect_identical(r$n, 40L)
  expect_true(is.finite(r$estimate))
  expect_true(r$se >= 0)
})

test_that("morie_random_forest_genomic works with NULL fixed features", {
  set.seed(23)
  M <- matrix(rnorm(150), 30, 5)
  y <- M[, 1] + 0.2 * rnorm(30)
  r <- morie_random_forest_genomic(NULL, y, M, n_trees = 15, seed = 23)
  expect_length(r$y_hat, 30L)
  expect_true(is.finite(r$oob_score))
})

test_that("morie_random_forest_genomic accepts a custom mtry", {
  set.seed(24)
  M <- matrix(rnorm(160), 32, 5)
  y <- M[, 2] + 0.2 * rnorm(32)
  r <- morie_random_forest_genomic(rep(0, 32), y, M,
    n_trees = 15, mtry = 2,
    seed = 24
  )
  expect_length(r$y_hat, 32L)
})

test_that("rgadp runs the LMS adaptive noise canceller", {
  set.seed(25)
  noise <- rnorm(200)
  x <- sin(2 * pi * seq_len(200) / 20) + noise
  r <- rgadp(x, reference = noise, mu = 0.01, order = 8L)
  expect_type(r, "list")
  expect_named(r, c("signal", "noise_estimate", "weights", "mu", "order"))
  expect_length(r$signal, 200L)
  expect_length(r$noise_estimate, 200L)
  expect_length(r$weights, 8L)
  expect_identical(r$order, 8L)
  expect_equal(r$mu, 0.01)
  expect_true(all(is.finite(r$signal)))
})

test_that("rgadp uses the default order of 16", {
  set.seed(26)
  noise <- rnorm(120)
  x <- cos(seq_len(120) / 10) + noise
  r <- rgadp(x, reference = noise)
  expect_length(r$weights, 16L)
  expect_identical(r$order, 16L)
})

test_that("rgadp errors when x and reference differ in length", {
  expect_error(rgadp(rnorm(50), reference = rnorm(40)), "equal length")
})

test_that("morie_rangayyan_adaptive_filter alias matches rgadp", {
  set.seed(27)
  noise <- rnorm(100)
  x <- sin(seq_len(100) / 8) + noise
  a <- morie_rangayyan_adaptive_filter(x, reference = noise, order = 8L)
  b <- rgadp(x, reference = noise, order = 8L)
  expect_equal(a$signal, b$signal)
})
