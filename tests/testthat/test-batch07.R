# SPDX-License-Identifier: AGPL-3.0-or-later
# Generated tests for batch07: gcvgn, ghadp, ghbvm, ghcls, ghcon, ghcrt,
# ghdir, ghdpm, ghebp, ghgpm, ghgps, ghhbp, ghlgd, ghmmt, ghntr.

test_that("morie_genomic_cross_validation returns a well-formed list", {
  set.seed(15)
  X <- matrix(rnorm(200), 50, 4)
  b <- c(1, -1, 0.5, 0)
  y <- as.numeric(X %*% b + 0.3 * rnorm(50))
  res <- morie_genomic_cross_validation(X, y, K = 5, seed = 15)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "r_per_fold", "y_hat", "mse", "mspe",
    "slope", "n", "K", "method"
  ))
  expect_type(res$method, "character")
  expect_identical(res$n, 50L)
  expect_identical(res$K, 5)
  expect_length(res$r_per_fold, 5L)
  expect_length(res$y_hat, 50L)
  expect_true(is.finite(res$mse))
  expect_gte(res$mse, 0)
  expect_identical(res$mse, res$mspe)
  expect_true(all(is.finite(res$y_hat)))
})

test_that("morie_genomic_cross_validation works with a data.frame and 3 folds", {
  set.seed(7)
  X <- as.data.frame(matrix(rnorm(120), 40, 3))
  y <- rnorm(40)
  res <- morie_genomic_cross_validation(X, y, K = 3, lam = 2.0, seed = 7)
  expect_identical(res$K, 3)
  expect_length(res$r_per_fold, 3L)
  expect_true(is.finite(res$mse))
})

test_that("morie_genomic_cross_validation pooled correlation is plausible", {
  set.seed(1)
  X <- matrix(rnorm(300), 60, 5)
  beta <- c(2, -1, 0, 1, 0.5)
  y <- as.numeric(X %*% beta + 0.2 * rnorm(60))
  res <- morie_genomic_cross_validation(X, y, K = 5, seed = 1)
  expect_true(is.na(res$estimate) || (res$estimate >= -1 && res$estimate <= 1))
})

test_that("morie_ghosal_adaptation returns rates over a default beta grid", {
  set.seed(2)
  x <- rnorm(100)
  res <- morie_ghosal_adaptation(x)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "betas", "rates", "best_beta",
    "n", "d", "method"
  ))
  expect_identical(res$n, 100L)
  expect_identical(res$d, 1)
  expect_length(res$betas, 11L)
  expect_length(res$rates, 11L)
  expect_true(all(is.finite(res$rates)))
  expect_true(all(res$rates > 0))
  expect_equal(res$estimate, min(res$rates))
  expect_true(res$best_beta %in% res$betas)
})

test_that("morie_ghosal_adaptation accepts a custom beta grid and dimension", {
  x <- rnorm(50)
  betas <- c(0.5, 1, 2, 4)
  res <- morie_ghosal_adaptation(x, betas = betas, d = 3)
  expect_identical(res$d, 3)
  expect_length(res$rates, 4L)
  expect_identical(res$betas, betas)
  expect_true(is.finite(res$estimate))
})

test_that("morie_ghosal_bernstein_von_mises returns BvM diagnostics", {
  set.seed(3)
  x <- rnorm(80)
  res <- morie_ghosal_bernstein_von_mises(x, B = 100, seed = 3)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "se", "theta_hat", "z_ks_stat",
    "z_ks_pvalue", "wald", "wald_pvalue", "n", "B",
    "method"
  ))
  expect_identical(res$n, 80L)
  expect_identical(res$B, 100)
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se))
  expect_gte(res$se, 0)
  expect_true(is.finite(res$theta_hat))
  expect_gte(res$z_ks_pvalue, 0)
  expect_lte(res$z_ks_pvalue, 1)
  expect_true(is.na(res$wald))
  expect_true(is.na(res$wald_pvalue))
})

test_that("morie_ghosal_bernstein_von_mises computes a Wald test when theta0 given", {
  set.seed(4)
  x <- rnorm(60, mean = 1)
  res <- morie_ghosal_bernstein_von_mises(x, theta0 = 0, B = 80, seed = 4)
  expect_true(is.finite(res$wald))
  expect_gte(res$wald_pvalue, 0)
  expect_lte(res$wald_pvalue, 1)
})

test_that("morie_ghosal_bernstein_von_mises handles n<2 gracefully", {
  res <- morie_ghosal_bernstein_von_mises(c(1.0), B = 10)
  expect_true(is.na(res$estimate))
  expect_identical(res$n, 1L)
  expect_match(res$method, "n<2")
})

test_that("morie_ghosal_bernstein_von_mises supports deterministic_seed path", {
  skip_if_not(
    exists("morie_det_rng",
      where = asNamespace("morie"), inherits = FALSE
    ),
    "morie_det_rng unavailable"
  )
  set.seed(5)
  x <- rnorm(40)
  res <- morie_ghosal_bernstein_von_mises(x, B = 60, deterministic_seed = 11L)
  expect_true(is.finite(res$estimate))
  expect_identical(res$n, 40L)
})

test_that("morie_ghosal_np_classification returns probit-GP results", {
  set.seed(6)
  x <- matrix(rnorm(80), 40, 2)
  y <- rbinom(40, 1, plogis(x[, 1]))
  res <- morie_ghosal_np_classification(x, y, n_iter = 50, seed = 6)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "p_hat", "accuracy", "length_scale",
    "n", "method"
  ))
  expect_identical(res$n, 40L)
  expect_length(res$p_hat, 40L)
  expect_true(all(res$p_hat >= 0 & res$p_hat <= 1))
  expect_gte(res$accuracy, 0)
  expect_lte(res$accuracy, 1)
  expect_gt(res$length_scale, 0)
  expect_true(is.finite(res$estimate))
})

test_that("morie_ghosal_np_classification honours a user length_scale", {
  set.seed(8)
  x <- matrix(rnorm(60), 30, 2)
  y <- rbinom(30, 1, 0.5)
  res <- morie_ghosal_np_classification(x, y,
    length_scale = 1.5,
    sigma_f = 2.0, n_iter = 40, seed = 8
  )
  expect_equal(res$length_scale, 1.5)
  expect_length(res$p_hat, 30L)
})

test_that("morie_ghosal_posterior_consistency returns Schwartz diagnostics", {
  set.seed(9)
  x <- rnorm(70)
  res <- morie_ghosal_posterior_consistency(x, K = 50, seed = 9)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "ks_mean", "ks_se", "schwartz_bound",
    "n", "eps", "method"
  ))
  expect_identical(res$n, 70L)
  expect_identical(res$eps, 0.1)
  expect_gte(res$estimate, 0)
  expect_lte(res$estimate, 1)
  expect_gte(res$ks_mean, 0)
  expect_gte(res$ks_se, 0)
  expect_gte(res$schwartz_bound, 0)
  expect_lte(res$schwartz_bound, 1)
})

test_that("morie_ghosal_posterior_consistency uses a parametric reference", {
  set.seed(10)
  x <- rnorm(50)
  res <- morie_ghosal_posterior_consistency(x,
    ref_loc = 0, ref_scale = 1,
    eps = 0.2, K = 40, seed = 10
  )
  expect_identical(res$eps, 0.2)
  expect_true(is.finite(res$ks_mean))
})

test_that("morie_ghosal_posterior_consistency handles empty input", {
  res <- morie_ghosal_posterior_consistency(numeric(0))
  expect_true(is.na(res$estimate))
  expect_identical(res$n, 0)
})

test_that("morie_ghosal_contraction_rate returns minimax rate", {
  res <- morie_ghosal_contraction_rate(rnorm(100))
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "log_rate_correction", "parametric_rate",
    "n", "beta", "d", "method"
  ))
  expect_identical(res$n, 100L)
  expect_identical(res$beta, 1.0)
  expect_identical(res$d, 1)
  expect_true(is.finite(res$estimate))
  expect_gt(res$estimate, 0)
  expect_equal(res$parametric_rate, 100^(-0.5))
  expect_true(is.finite(res$log_rate_correction))
})

test_that("morie_ghosal_contraction_rate honours beta and d arguments", {
  res <- morie_ghosal_contraction_rate(rnorm(64), beta = 2.0, d = 3)
  expect_identical(res$beta, 2.0)
  expect_identical(res$d, 3)
  expect_equal(res$estimate, 64^(-2.0 / (2 * 2.0 + 3)))
})

test_that("morie_ghosal_contraction_rate handles n too small", {
  res <- morie_ghosal_contraction_rate(c(1.0))
  expect_true(is.na(res$estimate))
  expect_match(res$method, "too small")
})

test_that("morie_ghosal_dirichlet_posterior returns conjugate DP posterior", {
  set.seed(11)
  x <- rnorm(40)
  res <- morie_ghosal_dirichlet_posterior(x, alpha = 1.0)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "alpha_post", "n", "cdf_grid",
    "cdf_post", "cdf_var", "method"
  ))
  expect_identical(res$n, 40L)
  expect_equal(res$alpha_post, 41)
  expect_length(res$cdf_grid, 51L)
  expect_length(res$cdf_post, 51L)
  expect_length(res$cdf_var, 51L)
  expect_true(all(res$cdf_post >= 0 & res$cdf_post <= 1))
  expect_true(all(res$cdf_var >= 0))
  expect_gte(res$estimate, 0)
  expect_lte(res$estimate, 1)
})

test_that("morie_ghosal_dirichlet_posterior accepts a custom grid", {
  set.seed(12)
  x <- rnorm(30)
  g <- seq(-4, 4, length.out = 25)
  res <- morie_ghosal_dirichlet_posterior(x,
    alpha = 2.5, base_mean = 0.5,
    base_sd = 2, grid = g
  )
  expect_identical(res$cdf_grid, g)
  expect_length(res$cdf_post, 25L)
})

test_that("morie_ghosal_dirichlet_posterior handles empty input", {
  res <- morie_ghosal_dirichlet_posterior(numeric(0), alpha = 1)
  expect_identical(res$n, 0L)
  expect_length(res$cdf_grid, 51L)
  expect_equal(res$alpha_post, 1)
})

test_that("morie_ghosal_dpmixture_density returns a density estimate", {
  set.seed(13)
  x <- rnorm(30)
  res <- morie_ghosal_dpmixture_density(x, n_iter = 30, burn = 10, seed = 13)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "grid", "density", "k_post", "n",
    "alpha", "sigma", "method"
  ))
  expect_identical(res$n, 30L)
  expect_length(res$grid, 51L)
  expect_length(res$density, 51L)
  expect_true(all(is.finite(res$density)))
  expect_true(all(res$density >= 0))
  expect_gt(res$sigma, 0)
  expect_true(is.finite(res$k_post))
  expect_gte(res$k_post, 1)
  expect_true(is.finite(res$estimate))
})

test_that("morie_ghosal_dpmixture_density accepts custom sigma and grid", {
  set.seed(14)
  x <- rnorm(25)
  g <- seq(-3, 3, length.out = 41)
  res <- morie_ghosal_dpmixture_density(x,
    alpha = 2.0, sigma = 0.5, grid = g,
    n_iter = 25, burn = 8, seed = 14
  )
  expect_equal(res$sigma, 0.5)
  expect_identical(res$grid, g)
  expect_length(res$density, 41L)
})

test_that("morie_ghosal_dpmixture_density handles empty input", {
  res <- morie_ghosal_dpmixture_density(numeric(0))
  expect_true(is.na(res$estimate))
  expect_identical(res$n, 0)
})

test_that("morie_ghosal_dpmixture_density supports deterministic_seed path", {
  skip_if_not(
    exists("morie_det_rng",
      where = asNamespace("morie"), inherits = FALSE
    ),
    "morie_det_rng unavailable"
  )
  set.seed(16)
  x <- rnorm(20)
  res <- morie_ghosal_dpmixture_density(x,
    n_iter = 25, burn = 8,
    deterministic_seed = 22L
  )
  expect_identical(res$n, 20L)
  expect_length(res$density, 51L)
})

test_that("morie_ghosal_empirical_bayes returns alpha-hat via optimisation", {
  set.seed(17)
  x <- round(rnorm(60), 1)
  res <- morie_ghosal_empirical_bayes(x)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "K_n", "log_lik_at_estimate", "n",
    "method"
  ))
  expect_identical(res$n, 60L)
  expect_true(is.finite(res$estimate))
  expect_gt(res$estimate, 0)
  expect_true(is.finite(res$log_lik_at_estimate))
  expect_gte(res$K_n, 2)
})

test_that("morie_ghosal_empirical_bayes accepts an alpha grid", {
  set.seed(18)
  x <- round(rnorm(50), 1)
  grid <- seq(0.1, 10, length.out = 30)
  res <- morie_ghosal_empirical_bayes(x, alpha_grid = grid)
  expect_true(res$estimate %in% grid)
  expect_true(is.finite(res$log_lik_at_estimate))
})

test_that("morie_ghosal_empirical_bayes handles n<2", {
  res <- morie_ghosal_empirical_bayes(c(1.0))
  expect_true(is.na(res$estimate))
  expect_match(res$method, "n<2")
})

test_that("morie_ghosal_gp_matern returns GP posterior with default nu", {
  set.seed(19)
  x <- sort(rnorm(30))
  y <- sin(x) + 0.1 * rnorm(30)
  res <- morie_ghosal_gp_matern(x, y)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "se", "mu", "sd", "length_scale",
    "nu", "noise", "n", "method"
  ))
  expect_identical(res$n, 30L)
  expect_identical(res$nu, 1.5)
  expect_length(res$mu, 30L)
  expect_length(res$sd, 30L)
  expect_true(all(is.finite(res$mu)))
  expect_true(all(res$sd >= 0))
  expect_gt(res$length_scale, 0)
  expect_gt(res$noise, 0)
})

test_that("morie_ghosal_gp_matern handles nu = 0.5 and 2.5 branches", {
  set.seed(20)
  x <- sort(rnorm(25))
  y <- cos(x) + 0.1 * rnorm(25)
  r05 <- morie_ghosal_gp_matern(x, y, nu = 0.5)
  r25 <- morie_ghosal_gp_matern(x, y, nu = 2.5)
  expect_identical(r05$nu, 0.5)
  expect_identical(r25$nu, 2.5)
  expect_true(all(is.finite(r05$mu)))
  expect_true(all(is.finite(r25$mu)))
})

test_that("morie_ghosal_gp_matern handles the general besselK branch", {
  set.seed(21)
  x <- sort(rnorm(20))
  y <- x^2 + 0.1 * rnorm(20)
  res <- morie_ghosal_gp_matern(x, y,
    nu = 1.0, length_scale = 1.0,
    noise = 0.2
  )
  expect_identical(res$nu, 1.0)
  expect_equal(res$length_scale, 1.0)
  expect_equal(res$noise, 0.2)
  expect_true(all(is.finite(res$mu)))
})

test_that("morie_ghosal_gp_matern accepts x_star prediction points and a matrix x", {
  set.seed(22)
  x <- matrix(rnorm(40), 20, 2)
  y <- rowSums(x) + 0.1 * rnorm(20)
  xs <- matrix(rnorm(10), 5, 2)
  res <- morie_ghosal_gp_matern(x, y, x_star = xs)
  expect_length(res$mu, 5L)
  expect_length(res$sd, 5L)
  expect_true(all(is.finite(res$mu)))
})

test_that("morie_ghosal_gp_squared_exponential returns GP posterior", {
  set.seed(23)
  x <- sort(rnorm(30))
  y <- sin(x) + 0.1 * rnorm(30)
  res <- morie_ghosal_gp_squared_exponential(x, y)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "se", "mu", "sd", "length_scale",
    "noise", "n", "method"
  ))
  expect_identical(res$n, 30L)
  expect_length(res$mu, 30L)
  expect_true(all(is.finite(res$mu)))
  expect_true(all(res$sd >= 0))
  expect_gt(res$length_scale, 0)
})

test_that("morie_ghosal_gp_squared_exponential honours optional args", {
  set.seed(24)
  x <- matrix(rnorm(40), 20, 2)
  y <- rowSums(x) + 0.1 * rnorm(20)
  xs <- matrix(rnorm(8), 4, 2)
  res <- morie_ghosal_gp_squared_exponential(x, y,
    length_scale = 2.0,
    sigma_f = 1.5, noise = 0.3,
    x_star = xs
  )
  expect_equal(res$length_scale, 2.0)
  expect_equal(res$noise, 0.3)
  expect_length(res$mu, 4L)
})

test_that("morie_ghosal_hierarchical_bayes returns alpha posterior summary", {
  set.seed(25)
  x <- round(rnorm(50), 1)
  res <- morie_ghosal_hierarchical_bayes(x, M = 120, seed = 25)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "alpha_se", "alpha_draws", "K_n",
    "n", "method"
  ))
  expect_identical(res$n, 50L)
  expect_true(is.finite(res$estimate))
  expect_gt(res$estimate, 0)
  expect_gte(res$alpha_se, 0)
  expect_length(res$alpha_draws, 120L - 120L %/% 4L)
  expect_true(all(res$alpha_draws > 0))
  expect_gte(res$K_n, 2)
})

test_that("morie_ghosal_hierarchical_bayes accepts custom hyperpriors", {
  set.seed(26)
  x <- round(rnorm(40), 1)
  res <- morie_ghosal_hierarchical_bayes(x,
    a_prior = 2.0, b_prior = 0.5,
    M = 100, seed = 26
  )
  expect_true(is.finite(res$estimate))
  expect_true(all(is.finite(res$alpha_draws)))
})

test_that("morie_ghosal_hierarchical_bayes handles n<2", {
  res <- morie_ghosal_hierarchical_bayes(c(1.0))
  expect_true(is.na(res$estimate))
  expect_match(res$method, "n<2")
})

test_that("morie_ghosal_hierarchical_bayes supports deterministic_seed path", {
  skip_if_not(
    exists("morie_det_rng",
      where = asNamespace("morie"), inherits = FALSE
    ),
    "morie_det_rng unavailable"
  )
  set.seed(27)
  x <- round(rnorm(30), 1)
  res <- morie_ghosal_hierarchical_bayes(x, M = 80, deterministic_seed = 33L)
  expect_true(is.finite(res$estimate))
  expect_identical(res$n, 30L)
})

test_that("morie_ghosal_log_density returns a log-spline density", {
  set.seed(28)
  x <- rnorm(80)
  res <- morie_ghosal_log_density(x, K = 4)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "theta", "log_lik", "grid",
    "log_density", "K", "n", "method"
  ))
  expect_identical(res$n, 80L)
  expect_identical(res$K, 4)
  expect_length(res$theta, 4L)
  expect_true(all(is.finite(res$theta)))
  expect_true(is.finite(res$log_lik))
  expect_true(all(is.finite(res$log_density)))
  expect_equal(length(res$grid), length(res$log_density))
  expect_true(is.finite(res$estimate))
})

test_that("morie_ghosal_log_density accepts a custom grid", {
  set.seed(29)
  x <- rnorm(60)
  g <- seq(-3, 3, length.out = 50)
  res <- morie_ghosal_log_density(x, K = 3, grid = g)
  expect_length(res$grid, 50L)
  expect_length(res$log_density, 50L)
})

test_that("morie_ghosal_log_density handles n<5", {
  res <- morie_ghosal_log_density(c(1, 2, 3))
  expect_true(is.na(res$estimate))
  expect_match(res$method, "n<5")
})

test_that("morie_ghosal_moment_matching returns DP moment-matching summary", {
  set.seed(30)
  x <- rnorm(50)
  res <- morie_ghosal_moment_matching(x)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "se", "prior_mean", "prior_var",
    "n_A", "n", "alpha", "method"
  ))
  expect_identical(res$n, 50L)
  expect_identical(res$alpha, 1.0)
  expect_gte(res$estimate, 0)
  expect_lte(res$estimate, 1)
  expect_gte(res$se, 0)
  expect_gte(res$prior_mean, 0)
  expect_lte(res$prior_mean, 1)
  expect_gte(res$prior_var, 0)
  expect_type(res$n_A, "integer")
})

test_that("morie_ghosal_moment_matching honours explicit set bounds", {
  set.seed(31)
  x <- rnorm(40)
  res <- morie_ghosal_moment_matching(x,
    alpha = 3.0, A_lower = -1, A_upper = 1,
    base_mean = 0.5, base_sd = 2
  )
  expect_identical(res$alpha, 3.0)
  expect_gte(res$n_A, 0L)
  expect_lte(res$n_A, 40L)
  expect_true(is.finite(res$estimate))
})

test_that("morie_ghosal_moment_matching handles empty input", {
  res <- morie_ghosal_moment_matching(numeric(0))
  expect_identical(res$n, 0L)
  expect_identical(res$n_A, 0L)
})

test_that("morie_ghosal_neutral_right returns NTR posterior survival", {
  set.seed(32)
  time <- rexp(50, rate = 0.5)
  res <- morie_ghosal_neutral_right(time)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "times", "S_post", "H_post", "c",
    "lam0", "n", "method"
  ))
  expect_identical(res$n, 50L)
  expect_identical(res$c, 1.0)
  expect_gt(res$lam0, 0)
  expect_equal(length(res$times), length(res$S_post))
  expect_equal(length(res$times), length(res$H_post))
  expect_true(all(res$S_post >= 0 & res$S_post <= 1))
  expect_true(all(is.finite(res$H_post)))
  expect_gte(res$estimate, 0)
  expect_lte(res$estimate, 1)
})

test_that("morie_ghosal_neutral_right handles censoring and custom lam0", {
  set.seed(33)
  time <- rexp(40, rate = 0.8)
  event <- rbinom(40, 1, 0.7)
  res <- morie_ghosal_neutral_right(time, event = event, c = 2.0, lam0 = 0.5)
  expect_equal(res$lam0, 0.5)
  expect_identical(res$c, 2.0)
  expect_true(all(res$S_post >= 0 & res$S_post <= 1))
})

test_that("morie_ghosal_neutral_right handles empty input", {
  res <- morie_ghosal_neutral_right(numeric(0))
  expect_true(is.na(res$estimate))
  expect_identical(res$n, 0)
  expect_match(res$method, "empty")
})
