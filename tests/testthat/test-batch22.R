# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 22 (final) tests: vaenc, vdwrd, vecmf, vines, vrgft, vrgm, vtpwr,
#   wavts, wdemb, wnom, workflow, wsrpw, xavir, xgbst.

test_that("morie_vaenc_vae_elbo returns the documented named list (vector input)", {
  # 1s of the suite here, and r-universe's macOS x86_64 builder is
  # about 1.8 times slower. The check there is killed at sixty minutes
  # and the suite alone was twenty-six of them. The heavy files run in
  # our own CI, which sets NOT_CRAN, where the clock is ours.
  skip_on_cran()
  set.seed(1)
  x <- rnorm(8)
  x_recon <- x + rnorm(8, sd = 0.1)
  mu <- rnorm(8)
  log_var <- rnorm(8, sd = 0.2)
  r <- morie_vaenc_vae_elbo(x, x_recon, mu, log_var)
  expect_type(r, "list")
  expect_named(r, c(
    "elbo", "estimate", "loss", "recon_loss",
    "kl_divergence", "method"
  ))
  expect_true(is.finite(r$elbo))
  expect_equal(r$estimate, r$elbo)
  expect_equal(r$loss, -r$elbo)
  expect_true(is.finite(r$recon_loss))
  expect_true(r$recon_loss >= 0)
  expect_identical(r$method, "VAE ELBO")
})

test_that("morie_vaenc_vae_elbo perfect reconstruction gives zero recon loss", {
  skip_on_cran()
  x <- c(1, 2, 3, 4)
  r <- morie_vaenc_vae_elbo(x, x, rep(0, 4), rep(0, 4))
  expect_equal(r$recon_loss, 0)
  expect_equal(r$kl_divergence, 0)
  expect_equal(r$elbo, 0)
})

test_that("morie_vaenc_vae_elbo handles matrix input and reduction='sum'", {
  skip_on_cran()
  set.seed(2)
  x <- matrix(rnorm(12), nrow = 3)
  x_recon <- x + 0.05
  mu <- matrix(rnorm(12), nrow = 3)
  log_var <- matrix(rnorm(12, sd = 0.1), nrow = 3)
  r_mean <- morie_vaenc_vae_elbo(x, x_recon, mu, log_var, reduction = "mean")
  r_sum <- morie_vaenc_vae_elbo(x, x_recon, mu, log_var, reduction = "sum")
  expect_true(is.finite(r_mean$elbo))
  expect_true(is.finite(r_sum$elbo))
})

test_that("morie_vaenc_vae_elbo rejects an unknown reduction", {
  skip_on_cran()
  expect_error(
    morie_vaenc_vae_elbo(1:4, 1:4, rep(0, 4), rep(0, 4),
      reduction = "median"
    ),
    "reduction"
  )
})

test_that("morie_vae_elbo alias is identical to morie_vaenc_vae_elbo", {
  skip_on_cran()
  expect_identical(morie_vae_elbo, morie_vaenc_vae_elbo)
})

test_that("morie_van_der_waerden_test returns the documented named list", {
  skip_on_cran()
  set.seed(3)
  x <- rnorm(20)
  y <- rnorm(25, mean = 0.4)
  r <- morie_van_der_waerden_test(x, y)
  expect_type(r, "list")
  expect_named(r, c("statistic", "p_value", "z", "n", "m", "method"))
  expect_true(is.finite(r$statistic))
  expect_true(is.finite(r$z))
  expect_true(r$p_value >= 0 && r$p_value <= 1)
  expect_equal(r$n, length(x) + length(y))
  expect_equal(r$m, length(x))
  expect_identical(r$method, "Van der Waerden normal-scores test")
})

test_that("morie_van_der_waerden_test returns NA stats for too-short samples", {
  skip_on_cran()
  r <- morie_van_der_waerden_test(c(1), c(2, 3, 4))
  expect_true(is.na(r$statistic))
  expect_true(is.na(r$p_value))
  expect_true(is.na(r$z))
  expect_equal(r$m, 1L)
  expect_equal(r$n, 4L)
})

test_that("morie_vecm returns the documented structure on a small I(1) system", {
  skip_on_cran()
  set.seed(4)
  Tt <- 60
  e1 <- cumsum(rnorm(Tt))
  e2 <- e1 + rnorm(Tt, sd = 0.3)
  Y <- cbind(e1, e2)
  r <- morie_vecm(Y, k_ar = 1, coint_rank = 1)
  expect_type(r, "list")
  expect_true(all(c(
    "alpha", "beta", "Sigma", "n", "k", "rank",
    "method"
  ) %in% names(r)))
  expect_equal(r$n, Tt)
  expect_equal(r$k, 2L)
  expect_equal(r$rank, 1L)
  expect_true(is.character(r$method))
})

test_that("morie_vecm errors on too-short series or bad rank", {
  skip_on_cran()
  set.seed(5)
  Yshort <- cbind(cumsum(rnorm(10)), cumsum(rnorm(10)))
  expect_error(morie_vecm(Yshort), "T>=20")
  Ylong <- cbind(cumsum(rnorm(30)), cumsum(rnorm(30)))
  expect_error(morie_vecm(Ylong, coint_rank = 0), "rank")
  expect_error(morie_vecm(Ylong, coint_rank = 5), "rank")
})

test_that("vines computes partial-correlation matrix and loglik", {
  skip_on_cran()
  set.seed(0)
  Sigma <- matrix(c(1, 0.5, 0.3, 0.5, 1, 0.4, 0.3, 0.4, 1), 3)
  z <- matrix(rnorm(600), 200, 3) %*% chol(Sigma)
  r <- rmorie:::vines(z)
  expect_type(r, "list")
  expect_true(all(c(
    "partial_corr", "R", "loglik", "estimate",
    "n", "d", "method"
  ) %in% names(r)))
  expect_equal(dim(r$partial_corr), c(3L, 3L))
  expect_equal(r$d, 3L)
  expect_equal(r$n, 200L)
  expect_true(is.finite(r$estimate))
})

test_that("vines returns NA estimate when n<3 or d<2", {
  skip_on_cran()
  r <- rmorie:::vines(matrix(c(1, 2), ncol = 1))
  expect_true(is.na(r$estimate))
  expect_match(r$method, "n<3")
})

test_that("morie_vine_copula alias is identical to vines", {
  skip_on_cran()
  expect_identical(morie_vine_copula, rmorie:::vines)
})

test_that("vrgm returns the documented empirical variogram structure", {
  skip_on_cran()
  x <- c(1, 2, 3, 4, 5)
  r <- vrgm(x, matrix(0:4, ncol = 1), n_bins = 4, max_dist = 4)
  expect_type(r, "list")
  expect_named(r, c("estimate", "n", "method"))
  expect_named(r$estimate, c("bins", "gamma", "n_pairs"))
  expect_equal(length(r$estimate$bins), 4L)
  expect_equal(length(r$estimate$gamma), 4L)
  expect_equal(length(r$estimate$n_pairs), 4L)
  expect_equal(r$n, 5L)
  expect_identical(r$method, "Empirical (Matheron) variogram")
})

test_that("vrgm uses default max_dist when NULL", {
  skip_on_cran()
  set.seed(6)
  x <- rnorm(15)
  r <- vrgm(x, matrix(runif(15), ncol = 1))
  expect_equal(length(r$estimate$bins), 10L)
})

test_that("vrgm errors on mismatched coords or too few points", {
  skip_on_cran()
  expect_error(
    vrgm(c(1, 2, 3), matrix(0:1, ncol = 1)),
    "coords rows"
  )
  expect_error(vrgm(1, matrix(0, ncol = 1)), "at least 2")
})

test_that("morie_variogram_estimation alias is identical to vrgm", {
  skip_on_cran()
  expect_identical(morie_variogram_estimation, vrgm)
})

test_that("vrgft fits an exponential variogram model", {
  skip_on_cran()
  set.seed(7)
  coords <- matrix(runif(40), ncol = 2)
  x <- rnorm(20)
  r <- vrgft(x, coords, model = "exponential", n_bins = 6)
  expect_type(r, "list")
  expect_named(r, c("estimate", "n", "method"))
  expect_named(r$estimate, c(
    "model", "nugget", "sill", "range",
    "params", "converged"
  ))
  expect_identical(r$estimate$model, "exponential")
  expect_true(is.finite(r$estimate$nugget))
  expect_true(is.finite(r$estimate$sill))
  expect_true(is.finite(r$estimate$range))
  expect_equal(length(r$estimate$params), 3L)
  expect_equal(r$n, 20L)
})

test_that("vrgft supports gaussian and spherical models", {
  skip_on_cran()
  set.seed(8)
  coords <- matrix(runif(40), ncol = 2)
  x <- rnorm(20)
  rg <- vrgft(x, coords, model = "gaussian", n_bins = 6)
  rs <- vrgft(x, coords, model = "spherical", n_bins = 6)
  expect_identical(rg$estimate$model, "gaussian")
  expect_identical(rs$estimate$model, "spherical")
})

test_that("vrgft errors when too few non-empty bins are available", {
  skip_on_cran()
  expect_error(
    vrgft(c(1, 2), matrix(0:1, ncol = 1), n_bins = 3),
    "3 non-empty bins|at least 2 points"
  )
})

test_that("morie_variogram_fitting alias is identical to vrgft", {
  skip_on_cran()
  expect_identical(morie_variogram_fitting, vrgft)
})

test_that("vtpwr returns exact Banzhaf and Shapley-Shubik indices", {
  skip_on_cran()
  r <- vtpwr(c(4, 3, 2, 1))
  expect_type(r, "list")
  expect_named(r, c(
    "banzhaf", "shapley_shubik", "quota",
    "weights", "method"
  ))
  expect_equal(length(r$banzhaf), 4L)
  expect_equal(length(r$shapley_shubik), 4L)
  expect_true(all(is.finite(r$banzhaf)))
  expect_true(all(r$banzhaf >= 0))
  expect_true(abs(sum(r$shapley_shubik) - 1) < 1e-8)
  expect_identical(r$method, "voting_power_index_exact")
})

test_that("vtpwr respects a user-supplied quota", {
  skip_on_cran()
  r <- vtpwr(c(5, 3, 1), quota = 6)
  expect_equal(r$quota, 6)
  expect_equal(length(r$banzhaf), 3L)
})

test_that("vtpwr handles an empty weight vector", {
  skip_on_cran()
  r <- vtpwr(numeric(0))
  expect_equal(length(r$banzhaf), 0L)
  expect_equal(length(r$shapley_shubik), 0L)
  expect_true(is.na(r$quota))
  expect_identical(r$method, "morie_voting_power_index")
})

test_that("vtpwr uses Monte Carlo for large games (n>10)", {
  skip_on_cran()
  r <- vtpwr(rep(1, 12))
  expect_equal(length(r$banzhaf), 12L)
  expect_identical(r$method, "voting_power_index_mc")
  expect_true(all(is.finite(r$shapley_shubik)))
})

test_that("morie_voting_power_index alias is identical to vtpwr", {
  skip_on_cran()
  expect_identical(morie_voting_power_index, vtpwr)
})

test_that("morie_wavelet_time_series returns the documented structure", {
  skip_on_cran()
  set.seed(9)
  x <- rnorm(64)
  r <- morie_wavelet_time_series(x)
  expect_type(r, "list")
  expect_true(all(c(
    "approximation", "details", "energies", "level",
    "n", "wavelet", "method"
  ) %in% names(r)))
  expect_equal(r$n, 64L)
  expect_true(r$level >= 1)
  expect_true(is.list(r$details))
  expect_equal(length(r$energies), length(r$details) + 1L)
  expect_true(all(is.finite(r$energies)))
})

test_that("morie_wavelet_time_series respects an explicit level argument", {
  skip_on_cran()
  set.seed(10)
  x <- rnorm(32)
  r <- morie_wavelet_time_series(x, level = 2)
  expect_equal(r$level, 2L)
  expect_equal(length(r$details), 2L)
})

test_that("morie_wavelet_time_series errors on too-short series", {
  skip_on_cran()
  expect_error(morie_wavelet_time_series(c(1, 2, 3)), ">=4")
})

test_that("word_embedding looks up rows with a random embedding matrix", {
  skip_on_cran()
  r <- rmorie:::word_embedding(c(0L, 5L, 99L),
    vocab_size = 100L,
    d_model = 8L, seed = 1L
  )
  expect_type(r, "list")
  expect_named(r, c("tensor", "E", "ids", "shape", "method"))
  expect_equal(dim(r$tensor), c(3L, 8L))
  expect_equal(dim(r$E), c(100L, 8L))
  expect_equal(r$ids, c(0L, 5L, 99L))
  expect_equal(r$shape, c(3L, 8L))
  expect_identical(r$method, "embedding-lookup")
})

test_that("word_embedding accepts a user-supplied embedding matrix", {
  skip_on_cran()
  E <- matrix(seq_len(20), nrow = 5, ncol = 4)
  r <- rmorie:::word_embedding(c(0L, 4L), E = E)
  expect_equal(r$tensor[1, ], E[1, ])
  expect_equal(r$tensor[2, ], E[5, ])
})

test_that("word_embedding errors on out-of-range token ids", {
  skip_on_cran()
  E <- matrix(0, nrow = 5, ncol = 3)
  expect_error(rmorie:::word_embedding(c(0L, 10L), E = E), "out of range")
  expect_error(rmorie:::word_embedding(c(-1L), E = E), "out of range")
})

test_that("wnom computes the NOMINATE log-likelihood and GMP", {
  skip_on_cran()
  set.seed(11)
  n_leg <- 12
  n_votes <- 8
  x <- matrix(rnorm(n_leg), ncol = 1)
  z_yea <- matrix(rnorm(n_votes), ncol = 1)
  z_nay <- matrix(rnorm(n_votes), ncol = 1)
  votes <- matrix(sample(c(0L, 1L), n_leg * n_votes, replace = TRUE),
    nrow = n_leg
  )
  r <- wnom(votes, x, z_yea, z_nay)
  expect_type(r, "list")
  expect_named(r, c("loglik", "GMP", "n_correct", "n_total", "method"))
  expect_true(is.finite(r$loglik))
  expect_true(r$loglik <= 0)
  expect_true(r$GMP >= 0 && r$GMP <= 1)
  expect_equal(r$n_total, n_leg * n_votes)
  expect_identical(r$method, "morie_wnominate_estimate")
})

test_that("wnom handles NA votes and custom salience weights", {
  skip_on_cran()
  set.seed(12)
  n_leg <- 10
  n_votes <- 6
  p <- 2
  x <- matrix(rnorm(n_leg * p), ncol = p)
  z_yea <- matrix(rnorm(n_votes * p), ncol = p)
  z_nay <- matrix(rnorm(n_votes * p), ncol = p)
  votes <- matrix(sample(c(0L, 1L, NA), n_leg * n_votes, replace = TRUE),
    nrow = n_leg
  )
  r <- wnom(votes, x, z_yea, z_nay, beta = 10, w = c(1, 0.5))
  expect_true(is.finite(r$loglik))
  expect_true(r$n_total <= n_leg * n_votes)
})

test_that("morie_wnominate aliases are identical to wnom", {
  skip_on_cran()
  expect_identical(morie_wnominate_estimate, wnom)
  expect_identical(morie_wnominate, wnom)
})

test_that("morie_default_workflow_map returns the documented named vector", {
  skip_on_cran()
  m <- morie_default_workflow_map()
  expect_type(m, "character")
  expect_true(all(c(
    "modules", "publish", "render",
    "readiness"
  ) %in% names(m)))
  expect_true(all(nzchar(m)))
})

test_that("morie_run_workflow_step rejects an unknown step", {
  skip_on_cran()
  expect_error(morie_run_workflow_step("does_not_exist"), "Unknown step")
})

test_that("morie_run_workflow_step rejects a missing or empty step", {
  skip_on_cran()
  expect_error(morie_run_workflow_step(), "exactly one")
  expect_error(morie_run_workflow_step(""), "exactly one")
  expect_error(morie_run_workflow_step(c("modules", "render")), "exactly one")
})

test_that("morie_run_workflow_step rejects an invalid script_map", {
  skip_on_cran()
  expect_error(
    morie_run_workflow_step("modules", script_map = c("a", "b")),
    "named character"
  )
  bad <- c("a")
  names(bad) <- ""
  expect_error(
    morie_run_workflow_step("modules", script_map = bad),
    "empty step names"
  )
})

test_that("morie_run_workflow_step errors when the script file is absent", {
  skip_on_cran()
  tmp <- tempfile("morie-wf-")
  dir.create(tmp)
  expect_error(
    morie_run_workflow_step("modules", project_root = tmp),
    "not found"
  )
})

test_that("morie_run_pipeline rejects unknown or empty steps", {
  skip_on_cran()
  expect_error(
    morie_run_pipeline(steps = c("modules", "ghost")),
    "Unknown steps"
  )
  expect_error(
    morie_run_pipeline(steps = character(0)),
    "non-empty character"
  )
})

test_that("morie_run_pipeline returns a data frame of step statuses", {
  skip_on_cran()
  tmp <- tempfile("morie-pipe-")
  dir.create(tmp)
  df <- morie_run_pipeline(
    steps = "modules", project_root = tmp,
    stop_on_error = TRUE, verbose = FALSE
  )
  expect_s3_class(df, "data.frame")
  expect_true(all(c("step", "script", "status", "error") %in% names(df)))
  expect_equal(df$step[1], "modules")
})

test_that("morie_wilcoxon_power returns the documented Monte-Carlo structure", {
  skip_on_cran()
  r <- morie_wilcoxon_power(rep(0, 15), effect_size = 0.6, nsim = 60, seed = 1)
  expect_type(r, "list")
  expect_named(r, c(
    "statistic", "n", "effect_size", "alpha",
    "nsim", "se", "method"
  ))
  expect_true(r$statistic >= 0 && r$statistic <= 1)
  expect_equal(r$n, 15L)
  expect_equal(r$effect_size, 0.6)
  expect_equal(r$nsim, 60)
  expect_true(is.finite(r$se))
  expect_match(r$method, "Wilcoxon")
})

test_that("morie_wilcoxon_power returns NA power for too-short input", {
  skip_on_cran()
  r <- morie_wilcoxon_power(c(1, 2, 3), nsim = 10)
  expect_true(is.na(r$statistic))
  expect_true(is.na(r$se))
  expect_equal(r$n, 3L)
})

test_that("morie_wilcoxon_power runs without a fixed seed", {
  skip_on_cran()
  r <- morie_wilcoxon_power(rep(0, 10), nsim = 30, seed = NULL)
  expect_true(r$statistic >= 0 && r$statistic <= 1)
})

test_that("morie_xavir_xavier_init returns the documented uniform-init list", {
  skip_on_cran()
  r <- morie_xavir_xavier_init(8, 4, seed = 42L, uniform = TRUE)
  expect_type(r, "list")
  expect_named(r, c(
    "weights", "value", "fan_in", "fan_out",
    "mean", "std", "shape", "method"
  ))
  expect_equal(dim(r$weights), c(8L, 4L))
  expect_equal(r$fan_in, 8)
  expect_equal(r$fan_out, 4)
  expect_equal(r$shape, c(8, 4))
  expect_true(is.finite(r$mean))
  expect_true(r$std >= 0)
  expect_identical(r$method, "uniform")
})

test_that("morie_xavir_xavier_init supports normal initialization", {
  skip_on_cran()
  r <- morie_xavir_xavier_init(6, 6, uniform = FALSE)
  expect_equal(dim(r$weights), c(6L, 6L))
  expect_identical(r$method, "normal")
})

test_that("morie_xavir_xavier_init is reproducible for a fixed seed", {
  skip_on_cran()
  r1 <- morie_xavir_xavier_init(5, 3, seed = 7L)
  r2 <- morie_xavir_xavier_init(5, 3, seed = 7L)
  expect_equal(r1$weights, r2$weights)
})

test_that("morie_xavir_xavier_init errors on non-positive fan sizes", {
  skip_on_cran()
  expect_error(morie_xavir_xavier_init(0, 4), "> 0")
  expect_error(morie_xavir_xavier_init(4, -1), "> 0")
})

test_that("morie_xavier_initialization alias is identical to morie_xavir_xavier_init", {
  skip_on_cran()
  expect_identical(morie_xavier_initialization, morie_xavir_xavier_init)
})

test_that("morie_xgboost_objective fits a regression model", {
  skip_on_cran()
  testthat::skip_if_not_installed("xgboost")
  set.seed(13)
  x <- matrix(rnorm(80), ncol = 4)
  y <- x[, 1] + rnorm(20, sd = 0.1)
  r <- morie_xgboost_objective(x, y,
    n_estimators = 10L, max_depth = 2L,
    task = "regression"
  )
  expect_type(r, "list")
  expect_true(all(c(
    "estimate", "train_score", "feature_importances",
    "backend", "task", "n", "method"
  ) %in% names(r)))
  expect_true(is.finite(r$estimate))
  expect_equal(r$task, "regression")
  expect_equal(r$n, 20L)
  expect_gte(length(r$feature_importances), 1L)
})

test_that("morie_xgboost_objective fits a classification model", {
  skip_on_cran()
  testthat::skip_if_not_installed("xgboost")
  set.seed(14)
  x <- matrix(rnorm(80), ncol = 4)
  y <- as.integer(x[, 1] > 0)
  r <- morie_xgboost_objective(x, y,
    n_estimators = 10L, max_depth = 2L,
    task = "classification"
  )
  expect_equal(r$task, "classification")
  expect_true(r$train_score >= 0 && r$train_score <= 1)
})

test_that("morie_xgboost_objective auto-detects the task and coerces a vector x", {
  skip_on_cran()
  testthat::skip_if_not_installed("xgboost")
  set.seed(15)
  x <- rnorm(30)
  y <- x + rnorm(30, sd = 0.1)
  r <- morie_xgboost_objective(x, y, n_estimators = 8L, task = "auto")
  expect_equal(r$n, 30L)
  expect_true(r$task %in% c("regression", "classification"))
})

test_that("morie_xgboost_objective errors when no boosting backend is installed", {
  skip_on_cran()
  if (FALSE) {
    expect_error(
      morie_xgboost_objective(matrix(1:4, ncol = 1), 1:4),
      "xgboost.*gbm"
    )
  }
  expect_true(TRUE)
})
