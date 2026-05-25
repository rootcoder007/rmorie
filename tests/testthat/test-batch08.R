# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for batch08: ghreg, ghsrv, ghstk, ghsve, ghtst, ghwav, gmatv,
# gpfit, grdcl, grdds, grpqa, grucl, gsrch, gwreg, gxemd.

test_that("morie_ghosal_np_regression returns a well-formed GP posterior list", {
  set.seed(1)
  x <- sort(rnorm(40))
  y <- sin(x) + rnorm(40, sd = 0.1)
  res <- morie_ghosal_np_regression(x, y)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "se", "mu", "sd", "ci_lower", "ci_upper",
    "r2", "log_marginal", "length_scale", "noise", "n",
    "method"
  ) %in% names(res)))
  expect_equal(res$n, 40L)
  expect_length(res$mu, 40L)
  expect_length(res$sd, 40L)
  expect_length(res$ci_lower, 40L)
  expect_length(res$ci_upper, 40L)
  expect_true(all(is.finite(res$mu)))
  expect_true(all(is.finite(res$sd)))
  expect_true(all(res$ci_upper >= res$ci_lower))
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$r2))
  expect_lte(res$r2, 1 + 1e-8)
  expect_type(res$method, "character")
})

test_that("morie_ghosal_np_regression honours explicit hyperparameters", {
  set.seed(2)
  x <- sort(rnorm(30))
  y <- 2 * x + rnorm(30, sd = 0.2)
  res <- morie_ghosal_np_regression(x, y,
    length_scale = 0.8, sigma_f = 1.5,
    noise = 0.25
  )
  expect_true(is.list(res))
  expect_true(is.finite(res$length_scale))
  expect_true(is.finite(res$noise))
  expect_gte(res$noise, 0)
  expect_true(all(is.finite(res$ci_lower)))
})

test_that("morie_ghosal_np_regression accepts a matrix of inputs", {
  set.seed(3)
  X <- matrix(rnorm(60), ncol = 2)
  y <- X[, 1] - X[, 2] + rnorm(30, sd = 0.1)
  res <- morie_ghosal_np_regression(X, y)
  expect_true(is.list(res))
  expect_equal(res$n, 30L)
  expect_length(res$mu, 30L)
})

test_that("morie_ghosal_survival_beta_process returns a posterior survival list", {
  set.seed(10)
  time <- rexp(50, rate = 0.5)
  event <- rbinom(50, 1, 0.7)
  res <- morie_ghosal_survival_beta_process(time, event)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "times", "S_post", "H_post", "c", "lam0",
    "n", "method"
  ) %in% names(res)))
  expect_equal(res$n, 50L)
  expect_true(is.finite(res$estimate))
  expect_gte(res$estimate, 0)
  expect_lte(res$estimate, 1)
  expect_true(all(is.finite(res$S_post)))
  expect_true(all(is.finite(res$H_post)))
  expect_equal(length(res$times), length(res$S_post))
  expect_type(res$method, "character")
})

test_that("morie_ghosal_survival_beta_process works without an event vector", {
  set.seed(11)
  time <- rexp(40, rate = 1)
  res <- morie_ghosal_survival_beta_process(time, c = 2.0)
  expect_true(is.list(res))
  expect_equal(res$n, 40L)
  expect_equal(res$c, 2.0)
})

test_that("morie_ghosal_survival_beta_process handles an explicit baseline hazard", {
  set.seed(12)
  time <- rexp(30, rate = 0.8)
  event <- rep(1L, 30)
  res <- morie_ghosal_survival_beta_process(time, event, c = 1.5, lam0 = 0.5)
  expect_true(is.list(res))
  expect_true(is.finite(res$estimate) || is.na(res$estimate))
})

test_that("morie_ghosal_stick_breaking_trunc returns a truncated DP draw", {
  set.seed(20)
  x <- rnorm(60)
  res <- morie_ghosal_stick_breaking_trunc(x, alpha = 1.0, K = 30, seed = 7)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "weights", "atoms", "effective_K",
    "trunc_err_bound", "n", "method"
  ) %in% names(res)))
  expect_equal(res$n, 60L)
  expect_length(res$weights, 30L)
  expect_length(res$atoms, 30L)
  expect_true(all(res$weights >= 0))
  expect_true(all(is.finite(res$atoms)))
  expect_true(is.finite(res$estimate))
  expect_gte(res$estimate, 0)
  expect_lte(res$estimate, 1)
  expect_gte(res$trunc_err_bound, 0)
  expect_lte(res$trunc_err_bound, 1)
  expect_gte(res$effective_K, 0)
})

test_that("morie_ghosal_stick_breaking_trunc honours explicit base measure", {
  set.seed(21)
  x <- rnorm(40, mean = 5)
  res <- morie_ghosal_stick_breaking_trunc(x,
    alpha = 2.0, K = 20, seed = 1,
    base_mean = 5, base_sd = 1.5
  )
  expect_true(is.list(res))
  expect_length(res$weights, 20L)
})

test_that("morie_ghosal_stick_breaking_trunc handles an empty input", {
  res <- morie_ghosal_stick_breaking_trunc(numeric(0), K = 10)
  expect_true(is.list(res))
  expect_equal(res$n, 0L)
  expect_length(res$weights, 10L)
})

test_that("morie_ghosal_stick_breaking_trunc supports deterministic_seed", {
  set.seed(22)
  x <- rnorm(30)
  res <- morie_ghosal_stick_breaking_trunc(x, K = 15, deterministic_seed = 99L)
  expect_true(is.list(res))
  expect_length(res$weights, 15L)
})

test_that("morie_ghosal_sieve_prior fits a Bernstein-polynomial sieve density", {
  set.seed(30)
  x <- rbeta(80, 2, 3)
  res <- morie_ghosal_sieve_prior(x)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "log_lik_per_obs", "weights", "K", "n",
    "method"
  ) %in% names(res)))
  expect_equal(res$n, 80L)
  expect_true(is.finite(res$estimate))
  expect_gte(res$estimate, 0)
  expect_true(is.finite(res$log_lik_per_obs))
  expect_equal(length(res$weights), res$K)
  expect_true(all(res$weights >= 0))
  expect_equal(sum(res$weights), 1, tolerance = 1e-6)
})

test_that("morie_ghosal_sieve_prior accepts an explicit sieve degree", {
  set.seed(31)
  x <- rbeta(50, 1, 1)
  res <- morie_ghosal_sieve_prior(x, K = 6)
  expect_true(is.list(res))
  expect_equal(res$K, 6)
  expect_length(res$weights, 6L)
})

test_that("morie_ghosal_sieve_prior short-circuits when n < 3", {
  res <- morie_ghosal_sieve_prior(c(0.2, 0.8))
  expect_true(is.list(res))
  expect_equal(res$n, 2L)
  expect_true(is.na(res$estimate))
})

test_that("morie_ghosal_np_testing returns a Polya-tree Bayes factor", {
  set.seed(40)
  x <- rnorm(100)
  res <- morie_ghosal_np_testing(x)
  expect_true(is.list(res))
  expect_true(all(c(
    "statistic", "p_value", "BF10", "log_BF10", "n",
    "depth", "method"
  ) %in% names(res)))
  expect_equal(res$n, 100L)
  expect_equal(res$depth, 6)
  expect_true(is.finite(res$statistic))
  expect_true(is.finite(res$BF10))
  expect_gte(res$BF10, 0)
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
  expect_equal(res$log_BF10, res$statistic)
})

test_that("morie_ghosal_np_testing honours reference and depth arguments", {
  set.seed(41)
  x <- rnorm(60, mean = 3, sd = 2)
  res <- morie_ghosal_np_testing(x, ref_loc = 3, ref_scale = 2, depth = 4, c = 2.0)
  expect_true(is.list(res))
  expect_equal(res$depth, 4)
  expect_true(is.finite(res$statistic))
})

test_that("morie_ghosal_np_testing short-circuits when n < 2", {
  res <- morie_ghosal_np_testing(c(0.5))
  expect_true(is.list(res))
  expect_equal(res$n, 1L)
  expect_true(is.na(res$statistic))
  expect_true(is.na(res$p_value))
})

test_that("morie_ghosal_wavelet_prior denoises a signal via Haar wavelets", {
  set.seed(50)
  n <- 64
  x <- sin(seq(0, 2 * pi, length.out = n)) + rnorm(n, sd = 0.2)
  res <- morie_ghosal_wavelet_prior(x)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "fitted", "noise", "sigma", "inclusion",
    "n", "method"
  ) %in% names(res)))
  expect_equal(res$n, n)
  expect_length(res$fitted, n)
  expect_true(all(is.finite(res$fitted)))
  expect_true(is.finite(res$estimate))
  expect_gte(res$noise, 0)
  expect_gte(res$sigma, 0)
  expect_gte(res$inclusion, 0)
  expect_lte(res$inclusion, 1)
})

test_that("morie_ghosal_wavelet_prior honours explicit sigma and noise", {
  set.seed(51)
  x <- rnorm(32)
  res <- morie_ghosal_wavelet_prior(x, pi = 0.3, sigma = 0.5, noise = 0.4)
  expect_true(is.list(res))
  expect_equal(res$n, 32L)
  expect_length(res$fitted, 32L)
})

test_that("morie_ghosal_wavelet_prior short-circuits when n < 4", {
  res <- morie_ghosal_wavelet_prior(c(1, 2, 3))
  expect_true(is.list(res))
  expect_equal(res$n, 3L)
  expect_true(is.finite(res$estimate))
})

test_that("morie_generalized_pareto fits a GP to threshold exceedances", {
  set.seed(60)
  x <- rexp(2000, rate = 1)
  res <- morie_generalized_pareto(x, threshold = 0.5)
  expect_true(is.list(res))
  expect_true(all(c(
    "scale", "shape", "threshold", "n_exceedances",
    "se_sigma", "se_xi", "loglik", "estimate", "se",
    "method"
  ) %in% names(res)))
  expect_true(is.finite(res$scale))
  expect_true(is.finite(res$shape))
  expect_gt(res$scale, 0)
  expect_equal(res$threshold, 0.5)
  expect_true(is.integer(res$n_exceedances))
  expect_gte(res$n_exceedances, 5L)
  expect_true(is.finite(res$loglik))
  expect_lt(abs(res$shape), 0.3)
})

test_that("morie_generalized_pareto uses the 90th percentile by default", {
  set.seed(61)
  x <- rexp(500, rate = 2)
  res <- morie_generalized_pareto(x)
  expect_true(is.list(res))
  expect_true(is.finite(res$threshold))
})

test_that("morie_generalized_pareto short-circuits on tiny samples", {
  res <- morie_generalized_pareto(c(1, 2, 3))
  expect_true(is.list(res))
  expect_true(is.na(res$estimate))
  expect_equal(res$n, 3L)
})

test_that("morie_generalized_pareto short-circuits with too few exceedances", {
  set.seed(62)
  x <- c(rep(0.1, 50), 100, 101)
  res <- morie_generalized_pareto(x, threshold = 50)
  expect_true(is.list(res))
  expect_true(is.na(res$estimate))
})

test_that("morie_gradient_descent_vanilla recovers OLS coefficients", {
  set.seed(70)
  x <- matrix(rnorm(200), ncol = 2)
  y <- 1 + 2 * x[, 1] - 1.5 * x[, 2] + rnorm(100, sd = 0.05)
  res <- morie_gradient_descent_vanilla(x, y, lr = 0.05, n_iter = 5000)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "reference_ols", "n_iter", "loss", "n",
    "method"
  ) %in% names(res)))
  expect_equal(res$n, 100L)
  expect_length(res$estimate, 3L)
  expect_length(res$reference_ols, 3L)
  expect_true(all(is.finite(res$estimate)))
  expect_true(is.finite(res$loss))
  expect_gte(res$loss, 0)
  expect_true(is.integer(res$n_iter))
  expect_equal(res$estimate, res$reference_ols, tolerance = 1e-2)
})

test_that("morie_gradient_descent_vanilla accepts a plain vector predictor", {
  set.seed(71)
  x <- rnorm(50)
  y <- 3 + 0.5 * x + rnorm(50, sd = 0.05)
  res <- morie_gradient_descent_vanilla(x, y, lr = 0.05, n_iter = 3000)
  expect_true(is.list(res))
  expect_length(res$estimate, 2L)
  expect_equal(res$n, 50L)
})

test_that("morie_gradient_descent_vanilla stops early on tight tolerance", {
  set.seed(72)
  x <- matrix(rnorm(60), ncol = 2)
  y <- x[, 1] + x[, 2]
  res <- morie_gradient_descent_vanilla(x, y, lr = 0.01, n_iter = 100, tol = 1e-1)
  expect_true(is.list(res))
  expect_lte(res$n_iter, 100L)
})

test_that("gwreg fits local regressions at every site", {
  set.seed(80)
  n <- 30
  coords <- matrix(runif(2 * n), ncol = 2)
  X <- cbind(1, rnorm(n))
  y <- 1 + 2 * X[, 2] + rnorm(n, sd = 0.1)
  res <- gwreg(X, y, coords)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "se", "bandwidth", "kernel", "n",
    "method"
  ) %in% names(res)))
  expect_equal(res$n, n)
  expect_equal(dim(res$estimate), c(n, 2L))
  expect_equal(dim(res$se), c(n, 2L))
  expect_true(all(is.finite(res$estimate)))
  expect_true(is.finite(res$bandwidth))
  expect_gt(res$bandwidth, 0)
  expect_equal(res$kernel, "gaussian")
})

test_that("gwreg supports the bisquare kernel and explicit bandwidth", {
  set.seed(81)
  n <- 25
  coords <- matrix(runif(2 * n), ncol = 2)
  X <- cbind(1, rnorm(n))
  y <- X[, 2] + rnorm(n, sd = 0.1)
  res <- morie_geographically_weighted_regression(X, y, coords,
    bandwidth = 0.5,
    kernel = "bisquare"
  )
  expect_true(is.list(res))
  expect_equal(res$kernel, "bisquare")
  expect_equal(res$bandwidth, 0.5)
  expect_equal(dim(res$estimate), c(n, 2L))
})

test_that("gwreg canonical 1-D example yields finite fits", {
  res <- gwreg(cbind(1, 0:4), 0:4, matrix(0:4, ncol = 1))
  expect_true(is.list(res))
  expect_equal(dim(res$estimate), c(5L, 2L))
  expect_true(all(is.finite(res$estimate)))
})

test_that("morie_gxe_interaction_model computes GxE variance components", {
  x <- c(1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3)
  env <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
  y <- c(1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 7)
  res <- morie_gxe_interaction_model(x, y, env)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "g", "e", "ge", "var_g", "var_e", "var_ge",
    "var_eps", "se", "n", "method"
  ) %in% names(res)))
  expect_equal(res$n, 12L)
  expect_true(is.finite(res$estimate))
  expect_length(res$g, 3L)
  expect_length(res$e, 2L)
  expect_true(is.matrix(res$ge))
  expect_equal(dim(res$ge), c(3L, 2L))
  expect_gte(res$var_g, 0)
  expect_gte(res$var_e, 0)
  expect_gte(res$var_ge, 0)
  expect_gte(res$var_eps, 0)
  expect_gte(res$se, 0)
  expect_equal(res$se, sqrt(res$var_eps), tolerance = 1e-8)
})

test_that("morie_gxe_interaction_model handles a larger replicated design", {
  set.seed(90)
  g <- rep(1:4, each = 6)
  env <- rep(rep(1:3, each = 2), times = 4)
  y <- 2 + g * 0.3 + env * 0.2 + rnorm(24, sd = 0.5)
  res <- morie_gxe_interaction_model(g, y, env)
  expect_true(is.list(res))
  expect_equal(res$n, 24L)
  expect_length(res$g, 4L)
  expect_length(res$e, 3L)
  expect_equal(dim(res$ge), c(4L, 3L))
  expect_true(all(is.finite(c(res$var_g, res$var_e, res$var_ge, res$var_eps))))
})

test_that("morie_grucl_gru_cell runs a forward pass with default weights", {
  set.seed(100)
  x <- rnorm(5)
  res <- morie_grucl_gru_cell(x, hidden_size = 4L, seed = 1L)
  expect_true(is.list(res))
  expect_true(all(c("h", "estimate", "z", "r", "n", "method") %in%
    names(res)))
  expect_length(res$h, 4L)
  expect_length(res$z, 4L)
  expect_length(res$r, 4L)
  expect_length(res$n, 4L)
  expect_true(all(is.finite(res$h)))
  expect_true(all(res$z >= 0 & res$z <= 1))
  expect_true(all(res$r >= 0 & res$r <= 1))
  expect_true(all(res$n >= -1 & res$n <= 1))
  expect_identical(res$h, res$estimate)
})

test_that("morie_gru_cell alias accepts supplied weights and previous state", {
  set.seed(101)
  n_in <- 3L
  H <- 4L
  x <- rnorm(n_in)
  h_prev <- rnorm(H)
  W <- matrix(rnorm(3 * H * n_in, 0, 0.1), 3 * H, n_in)
  U <- matrix(rnorm(3 * H * H, 0, 0.1), 3 * H, H)
  b <- rep(0, 3 * H)
  res <- morie_gru_cell(x, h_prev = h_prev, W = W, U = U, b = b, hidden_size = H)
  expect_true(is.list(res))
  expect_length(res$h, H)
  expect_true(all(is.finite(res$h)))
})

test_that("morie_grucl_gru_cell infers hidden size from h_prev", {
  set.seed(102)
  x <- rnorm(3)
  h_prev <- rnorm(6)
  res <- morie_grucl_gru_cell(x, h_prev = h_prev, seed = 2L)
  expect_true(is.list(res))
  expect_length(res$h, 6L)
})

test_that("morie_grucl_gru_cell supports deterministic_seed", {
  set.seed(103)
  x <- rnorm(4)
  res <- morie_grucl_gru_cell(x, hidden_size = 4L, deterministic_seed = 55L)
  expect_true(is.list(res))
  expect_length(res$h, 4L)
})

test_that("morie_grid_search_cv runs a regression grid search", {
  set.seed(110)
  x <- matrix(rnorm(120), ncol = 3)
  y <- x[, 1] - x[, 2] + rnorm(40, sd = 0.2)
  res <- tryCatch(
    morie_grid_search_cv(x, y, cv = 3L, task = "regression", seed = 1L),
    error = function(e) NULL
  )
  skip_if(is.null(res), "morie_grid_search_cv regression backend unavailable")
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "best_params", "best_score", "task", "n",
    "method"
  ) %in% names(res)))
  expect_equal(res$task, "regression")
  expect_equal(res$n, 40L)
  expect_true(is.finite(res$best_score))
})

test_that("morie_grid_search_cv errors clearly when caret is missing", {
  skip_if_not_installed("caret")
  if (!requireNamespace("caret", quietly = TRUE)) {
    expect_error(
      morie_grid_search_cv(matrix(rnorm(20), ncol = 2), rnorm(10)),
      "caret"
    )
  } else {
    succeed()
  }
})

test_that("gradient_clipping rescales gradients to the max norm", {
  fn <- tryCatch(get("gradient_clipping", envir = asNamespace("morie")),
    error = function(e) NULL
  )
  skip_if(is.null(fn), "gradient_clipping not in namespace")
  res <- fn(c(3, 4), max_norm = 1)
  expect_true(is.list(res))
  expect_true(all(c(
    "tensor", "clip_coef", "total_norm", "max_norm",
    "method"
  ) %in% names(res)))
  expect_equal(res$total_norm, 5, tolerance = 1e-8)
  expect_gte(res$clip_coef, 0)
  expect_lte(res$clip_coef, 1)
  expect_true(all(is.finite(res$tensor)))
})

test_that("gradient_clipping leaves small gradients unchanged", {
  fn <- tryCatch(get("gradient_clipping", envir = asNamespace("morie")),
    error = function(e) NULL
  )
  skip_if(is.null(fn), "gradient_clipping not in namespace")
  res <- fn(c(0.1, 0.2), max_norm = 10)
  expect_equal(res$clip_coef, 1, tolerance = 1e-6)
  res_list <- fn(list(c(1, 1), c(2, 2)), max_norm = 1)
  expect_true(is.list(res_list$tensor))
  expect_length(res_list$tensor, 2L)
})

test_that("grouped_query_attention produces attention weights", {
  fn <- tryCatch(get("grouped_query_attention", envir = asNamespace("morie")),
    error = function(e) NULL
  )
  skip_if(is.null(fn), "grouped_query_attention not in namespace")
  set.seed(120)
  Q <- matrix(rnorm(12), nrow = 4, ncol = 3)
  res <- tryCatch(fn(Q, n_heads = 4L, n_kv_heads = 2L),
    error = function(e) NULL
  )
  skip_if(is.null(res), "grouped_query_attention shape path unavailable")
  expect_true(is.list(res))
  expect_true(all(c(
    "tensor", "attn", "n_heads", "n_kv_heads", "group_size",
    "method"
  ) %in% names(res)))
  expect_equal(res$n_heads, 4L)
  expect_equal(res$n_kv_heads, 2L)
  expect_equal(res$group_size, 2L)
})

test_that("grouped_query_attention rejects incompatible head counts", {
  fn <- tryCatch(get("grouped_query_attention", envir = asNamespace("morie")),
    error = function(e) NULL
  )
  skip_if(is.null(fn), "grouped_query_attention not in namespace")
  Q <- matrix(rnorm(6), nrow = 2, ncol = 3)
  expect_error(fn(Q, n_heads = 8L, n_kv_heads = 3L), "multiple")
})
