# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for batch 02: bglup, bkprp, blasf, bnfwd, bpblm, brdgf, brdgr,
# brreg, btsrp, bysid, causal, cncrd, cndrc, cnn1d, cnn2d.

test_that("morie_bayes_cpi_genomic returns a well-formed list", {
  set.seed(11)
  X <- matrix(rnorm(180), 30, 6)
  b <- c(1, 0, 0, -1, 0, 0)
  y <- as.numeric(X %*% b + 0.1 * rnorm(30))
  res <- morie_bayes_cpi_genomic(X, y, n_iter = 80, burn = 30, seed = 11)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "beta", "beta_pip", "pi", "sigma_b2",
    "sigma2", "intercept", "n_iter", "n", "p", "method"
  ))
  expect_length(res$beta, 6)
  expect_length(res$beta_pip, 6)
  expect_true(all(is.finite(res$beta)))
  expect_true(all(res$beta_pip >= 0 & res$beta_pip <= 1))
  expect_equal(res$n, 30)
  expect_equal(res$p, 6)
  expect_true(res$n_iter > 0)
  expect_type(res$method, "character")
})

test_that("morie_bayes_cpi_genomic respects pi_init and is finite", {
  set.seed(5)
  X <- matrix(rnorm(120), 20, 6)
  y <- as.numeric(X %*% c(0.8, -0.6, 0, 0, 0, 0) + 0.2 * rnorm(20))
  res <- morie_bayes_cpi_genomic(X, y,
    n_iter = 60, burn = 20,
    pi_init = 0.3, seed = 7
  )
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$pi))
  expect_gte(res$pi, 0)
  expect_lte(res$pi, 1)
  expect_true(res$sigma_b2 > 0)
  expect_true(res$sigma2 > 0)
})

test_that("morie_bkprp_backpropagation sigmoid path returns gradients", {
  set.seed(1)
  x <- matrix(rnorm(12), 3, 4)
  y <- matrix(rnorm(6), 3, 2)
  w <- matrix(rnorm(8), 2, 4)
  b <- rnorm(2)
  res <- morie_bkprp_backpropagation(x, y, w = w, b = b, activation = "sigmoid")
  expect_named(res, c(
    "loss", "estimate", "dW", "db", "dx", "a", "z",
    "method"
  ))
  expect_true(is.finite(res$loss))
  expect_gte(res$loss, 0)
  expect_equal(dim(res$dW), c(2, 4))
  expect_length(res$db, 2)
  expect_equal(dim(res$dx), c(3, 4))
  expect_equal(dim(res$a), c(3, 2))
  expect_true(all(is.finite(res$dW)))
})

test_that("morie_bkprp_backpropagation supports all activations", {
  set.seed(2)
  x <- matrix(rnorm(6), 3, 2)
  y <- matrix(rnorm(6), 3, 2)
  for (act in c("identity", "linear", "none", "sigmoid", "tanh", "relu")) {
    res <- morie_bkprp_backpropagation(x, y, activation = act)
    expect_true(is.finite(res$loss))
    expect_true(all(is.finite(res$dW)))
  }
})

test_that("morie_bkprp_backpropagation uses default w and b", {
  set.seed(3)
  x <- matrix(rnorm(6), 3, 2)
  y <- matrix(rnorm(6), 3, 2)
  res <- morie_bkprp_backpropagation(x, y)
  expect_true(is.finite(res$loss))
  expect_equal(dim(res$z), c(3, 2))
})

test_that("morie_bkprp_backpropagation errors on unknown activation", {
  x <- matrix(rnorm(6), 3, 2)
  y <- matrix(rnorm(6), 3, 2)
  expect_error(morie_bkprp_backpropagation(x, y, activation = "bogus"))
})

test_that("morie_backpropagation alias is identical to morie_bkprp_backpropagation", {
  expect_identical(morie_backpropagation, morie_bkprp_backpropagation)
})

test_that("morie_bayesian_lasso_full returns a well-formed list", {
  set.seed(3)
  X <- matrix(rnorm(100), 20, 5)
  y <- as.numeric(X %*% c(1, -1, 0, 0, 0) + 0.2 * rnorm(20))
  res <- morie_bayesian_lasso_full(X, y, n_iter = 80, burn = 20, seed = 3)
  expect_named(res, c(
    "estimate", "beta", "intercept", "se", "beta_se",
    "lam", "sigma2", "n_iter", "n", "p", "method"
  ))
  expect_length(res$beta, 5)
  expect_length(res$beta_se, 5)
  expect_true(all(is.finite(res$beta)))
  expect_equal(res$n, 20)
  expect_equal(res$p, 5)
  expect_true(res$sigma2 > 0)
})

test_that("morie_bayesian_lasso_full accepts a fixed lambda", {
  set.seed(8)
  X <- matrix(rnorm(80), 20, 4)
  y <- as.numeric(X %*% c(0.5, -0.5, 0, 0) + 0.2 * rnorm(20))
  res <- morie_bayesian_lasso_full(X, y,
    n_iter = 60, burn = 15, lam = 2,
    seed = 8
  )
  expect_true(is.finite(res$estimate))
  expect_equal(res$lam, 2)
  expect_true(all(res$beta_se >= 0))
})

test_that("morie_bnfwd_batch_norm_forward normalizes per feature", {
  set.seed(1)
  x <- matrix(rnorm(40), 10, 4)
  res <- morie_bnfwd_batch_norm_forward(x)
  expect_named(res, c(
    "y", "estimate", "x_hat", "mu", "var", "eps",
    "method"
  ))
  expect_equal(dim(res$y), c(10, 4))
  expect_length(res$mu, 4)
  expect_length(res$var, 4)
  expect_true(all(is.finite(res$y)))
  expect_true(all(abs(colMeans(res$x_hat)) < 1e-6))
})

test_that("morie_bnfwd_batch_norm_forward applies gamma and beta", {
  set.seed(2)
  x <- matrix(rnorm(30), 10, 3)
  res <- morie_bnfwd_batch_norm_forward(x,
    gamma = c(2, 2, 2),
    beta = c(1, 1, 1), eps = 1e-3
  )
  expect_equal(res$eps, 1e-3)
  expect_true(all(is.finite(res$y)))
  expect_equal(dim(res$y), c(10, 3))
})

test_that("morie_batch_norm_forward alias is identical", {
  expect_identical(morie_batch_norm_forward, morie_bnfwd_batch_norm_forward)
})

test_that("morie_bayes_ridge_gibbs returns a well-formed list", {
  set.seed(4)
  X <- matrix(rnorm(100), 20, 5)
  y <- as.numeric(X %*% c(1, -1, 0.5, 0, 0) + 0.2 * rnorm(20))
  res <- morie_bayes_ridge_gibbs(X, y, n_iter = 80, burn = 20, seed = 4)
  expect_named(res, c(
    "estimate", "beta", "beta_se", "se", "sigma_j2",
    "sigma2", "intercept", "n_iter", "n", "p", "method"
  ))
  expect_length(res$beta, 5)
  expect_length(res$sigma_j2, 5)
  expect_true(all(is.finite(res$beta)))
  expect_true(all(res$sigma_j2 > 0))
  expect_true(res$sigma2 > 0)
  expect_equal(res$n, 20)
  expect_equal(res$p, 5)
})

test_that("morie_bayes_ridge_gibbs accepts custom df0 and S0", {
  set.seed(9)
  X <- matrix(rnorm(80), 20, 4)
  y <- as.numeric(X %*% c(0.7, -0.3, 0, 0) + 0.2 * rnorm(20))
  res <- morie_bayes_ridge_gibbs(X, y,
    n_iter = 60, burn = 15,
    df0 = 6, S0 = 0.05, seed = 9
  )
  expect_true(is.finite(res$estimate))
  expect_true(all(res$beta_se >= 0))
})

test_that("brdgr with single logical vector counts non-empty entries", {
  v <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  res <- brdgr(v)
  expect_named(res, c(
    "n_bridges", "bridge_ids", "share", "n1", "n2",
    "method"
  ))
  expect_equal(res$n_bridges, 3)
  expect_equal(res$bridge_ids, c(1L, 3L, 4L))
  expect_equal(res$share, 3 / 5)
  expect_equal(res$n1, 5)
})

test_that("brdgr with two ID vectors returns intersection", {
  res <- brdgr(c(1, 2, 3, 4), c(3, 4, 5, 6))
  expect_equal(res$n_bridges, 2)
  expect_equal(res$bridge_ids, c(3, 4))
  expect_equal(res$n1, 4)
  expect_equal(res$n2, 4)
})

test_that("brdgr with two matrices counts rows non-empty in both", {
  x <- matrix(c(1, NA, 3, NA, 5, 6), 3, 2)
  y <- matrix(c(NA, 2, 3, 4, NA, 6), 3, 2)
  res <- brdgr(x, y)
  expect_true(res$n_bridges >= 0)
  expect_true(is.numeric(res$share))
  expect_equal(res$method, "morie_bridge_observations")
})

test_that("brdgr errors on mismatched matrix rows", {
  x <- matrix(rnorm(6), 3, 2)
  y <- matrix(rnorm(8), 4, 2)
  expect_error(brdgr(x, y))
})

test_that("morie_bridge_observations alias is identical", {
  expect_identical(morie_bridge_observations, brdgr)
})

test_that("morie_bayesian_ridge_regression returns a well-formed list", {
  set.seed(2)
  X <- matrix(rnorm(100), 20, 5)
  y <- as.numeric(X %*% c(1, -1, 0.5, 0, 0) + 0.1 * rnorm(20))
  res <- morie_bayesian_ridge_regression(X, y)
  expect_named(res, c(
    "estimate", "beta", "intercept", "se", "beta_se",
    "lam", "n", "p", "method"
  ))
  expect_length(res$beta, 5)
  expect_true(all(is.finite(res$beta)))
  expect_true(all(res$beta_se >= 0))
  expect_true(res$lam > 0)
  expect_equal(res$n, 20)
  expect_equal(res$p, 5)
})

test_that("morie_bayesian_ridge_regression accepts a fixed lambda", {
  set.seed(12)
  X <- matrix(rnorm(60), 15, 4)
  y <- as.numeric(X %*% c(0.5, 0, -0.5, 0) + 0.1 * rnorm(15))
  res <- morie_bayesian_ridge_regression(X, y, lam = 5)
  expect_equal(res$lam, 5)
  expect_true(is.finite(res$estimate))
})

test_that("btsrp percentile method brackets the estimate", {
  set.seed(0)
  x <- rnorm(100)
  res <- btsrp(x, B = 400, seed = 0, method = "percentile")
  expect_named(res, c(
    "estimate", "se", "ci_lower", "ci_upper", "alpha",
    "B", "n", "method"
  ))
  expect_equal(res$estimate, mean(x))
  expect_true(res$ci_lower < res$estimate)
  expect_true(res$estimate < res$ci_upper)
  expect_true(res$se > 0)
  expect_equal(res$n, 100L)
})

test_that("btsrp bca method returns finite CI", {
  set.seed(1)
  x <- rnorm(60)
  res <- btsrp(x, B = 300, seed = 1, method = "bca")
  expect_true(is.finite(res$ci_lower))
  expect_true(is.finite(res$ci_upper))
  expect_true(res$ci_lower <= res$ci_upper)
})

test_that("btsrp studentized method returns finite CI", {
  set.seed(2)
  x <- rnorm(50)
  res <- btsrp(x, B = 100, seed = 2, method = "studentized")
  expect_true(is.finite(res$ci_lower))
  expect_true(is.finite(res$ci_upper))
  expect_match(res$method, "studentized")
})

test_that("btsrp accepts a custom statistic", {
  set.seed(3)
  x <- rnorm(80)
  res <- btsrp(x, statistic = stats::median, B = 200, seed = 3)
  expect_equal(res$estimate, stats::median(x))
})

test_that("btsrp handles degenerate short input", {
  res <- btsrp(c(1.5), B = 50)
  expect_true(is.na(res$estimate))
  expect_equal(res$n, 1L)
})

test_that("morie_bootstrap_ci alias is identical", {
  expect_identical(morie_bootstrap_ci, btsrp)
})

test_that("bysid returns ideal-point estimates from a vote matrix", {
  set.seed(1)
  M <- matrix(rbinom(120, 1, 0.5), 20, 6)
  res <- bysid(M, n_iter = 120, burn = 40, seed = 1)
  expect_named(res, c(
    "x_mean", "x_sd", "x_ci", "alpha", "beta",
    "n_iter", "method"
  ))
  expect_length(res$x_mean, 20)
  expect_length(res$x_sd, 20)
  expect_equal(dim(res$x_ci), c(20, 2))
  expect_length(res$alpha, 6)
  expect_length(res$beta, 6)
  expect_true(all(is.finite(res$x_mean)))
})

test_that("bysid handles degenerate single-row input", {
  res <- bysid(matrix(c(1, 0, 1), 1, 3), n_iter = 20, burn = 5)
  expect_true(all(is.na(res$x_mean)))
  expect_equal(res$method, "morie_bayesian_ideal_points")
})

test_that("bysid handles all-burn-in (no samples) gracefully", {
  set.seed(4)
  M <- matrix(rbinom(60, 1, 0.5), 10, 6)
  res <- bysid(M, n_iter = 5, burn = 10, seed = 4)
  expect_true(all(is.na(res$x_mean)))
  expect_equal(res$n_iter, 5L)
})

test_that("morie_bayesian_ideal_points alias is identical", {
  expect_identical(morie_bayesian_ideal_points, bysid)
})

test_that("morie_estimate_propensity_scores returns clipped scores", {
  set.seed(1)
  df <- data.frame(t = rbinom(60, 1, 0.4), x = rnorm(60))
  ps <- morie_estimate_propensity_scores(df, "t", "x")
  expect_length(ps, 60)
  expect_true(all(ps > 0 & ps < 1))
  expect_true(all(is.finite(ps)))
})

test_that("morie_estimate_ate returns ATE with CI", {
  set.seed(1)
  df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
  res <- morie_estimate_ate(df, "t", "y", "x")
  expect_named(res, c("ate", "se", "ci_lower", "ci_upper", "n", "ess"))
  expect_true(is.finite(res$ate))
  expect_true(res$ci_lower <= res$ci_upper)
  expect_equal(res$n, 200)
  expect_true(res$ess > 0)
})

test_that("morie_estimate_ate accepts a pre-computed propensity column", {
  set.seed(2)
  df <- data.frame(t = rbinom(120, 1, 0.5), y = rnorm(120), x = rnorm(120))
  df$ps <- morie_estimate_propensity_scores(df, "t", "x")
  res <- morie_estimate_ate(df, "t", "y", "x", propensity_col = "ps")
  expect_true(is.finite(res$ate))
})

test_that("morie_estimate_att returns ATT with CI", {
  set.seed(2)
  df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
  res <- morie_estimate_att(df, "t", "y", "x")
  expect_named(res, c("att", "se", "ci_lower", "ci_upper", "n_treated"))
  expect_true(is.finite(res$att))
  expect_true(res$n_treated > 0)
})

test_that("morie_estimate_atc returns ATC with CI", {
  set.seed(3)
  df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
  res <- morie_estimate_atc(df, "t", "y", "x")
  expect_named(res, c("atc", "se", "ci_lower", "ci_upper", "n_control"))
  expect_true(is.finite(res$atc))
  expect_true(res$n_control > 0)
})

test_that("morie_estimate_aipw returns doubly-robust ATE (linear)", {
  set.seed(4)
  df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
  res <- morie_estimate_aipw(df, "t", "y", "x", outcome_model = "linear")
  expect_named(res, c("ate", "se", "ci_lower", "ci_upper", "n"))
  expect_true(is.finite(res$ate))
  expect_equal(res$n, 200)
})

test_that("morie_estimate_aipw supports a logistic outcome model", {
  set.seed(5)
  df <- data.frame(
    t = rbinom(200, 1, 0.4),
    y = rbinom(200, 1, 0.5),
    x = rnorm(200)
  )
  res <- morie_estimate_aipw(df, "t", "y", "x", outcome_model = "logistic")
  expect_true(is.finite(res$ate))
})

test_that("morie_estimate_gate returns one row per group", {
  set.seed(3)
  df <- data.frame(
    t = rbinom(300, 1, 0.4), y = rnorm(300), x = rnorm(300),
    g = sample(c("A", "B"), 300, replace = TRUE)
  )
  res <- morie_estimate_gate(df, "t", "y", "x", "g")
  expect_s3_class(res, "data.frame")
  expect_true(all(c("group", "ate", "se", "ci_lower", "ci_upper", "n")
  %in% names(res)))
  expect_equal(nrow(res), 2)
})

test_that("morie_estimate_cate t-learner returns per-unit effects", {
  set.seed(6)
  df <- data.frame(t = rbinom(200, 1, 0.5), y = rnorm(200), x = rnorm(200))
  cate <- morie_estimate_cate(df, "t", "y", "x", meta_learner = "t_learner")
  expect_length(cate, 200)
  expect_true(all(is.finite(cate)))
})

test_that("morie_estimate_cate s-learner returns per-unit effects", {
  set.seed(7)
  df <- data.frame(t = rbinom(200, 1, 0.5), y = rnorm(200), x = rnorm(200))
  cate <- morie_estimate_cate(df, "t", "y", "x", meta_learner = "s_learner")
  expect_length(cate, 200)
  expect_true(all(is.finite(cate)))
})

test_that("morie_estimate_late returns Wald estimate without covariates", {
  set.seed(8)
  n <- 300
  z <- rbinom(n, 1, 0.5)
  t <- rbinom(n, 1, 0.3 + 0.4 * z)
  y <- 2 * t + rnorm(n)
  df <- data.frame(t = t, y = y, z = z)
  res <- morie_estimate_late(df, "t", "y", "z")
  expect_named(res, c(
    "late", "se", "ci_lower", "ci_upper",
    "first_stage_f", "n"
  ))
  expect_true(is.finite(res$late))
  expect_true(res$first_stage_f > 0)
  expect_equal(res$n, n)
})

test_that("morie_estimate_late with covariates runs (ivreg or fallback)", {
  set.seed(9)
  n <- 300
  z <- rbinom(n, 1, 0.5)
  x <- rnorm(n)
  t <- rbinom(n, 1, 0.3 + 0.4 * z)
  y <- 2 * t + 0.5 * x + rnorm(n)
  df <- data.frame(t = t, y = y, z = z, x = x)
  res <- morie_estimate_late(df, "t", "y", "z", covariates = "x")
  expect_true(is.finite(as.numeric(res$late)))
  expect_true(is.finite(as.numeric(res$se)))
})

test_that("morie_e_value computes E-value and CI bound", {
  res <- morie_e_value(rr = 3.9, rr_lower = 2.4)
  expect_named(res, c("morie_e_value", "e_value_ci"))
  expect_true(res$morie_e_value > 1)
  expect_true(res$e_value_ci > 1)
  expect_true(is.finite(res$morie_e_value))
})

test_that("morie_e_value without CI bound returns NA for e_value_ci", {
  res <- morie_e_value(rr = 2.0)
  expect_true(res$morie_e_value > 1)
  expect_true(is.na(res$e_value_ci))
})

test_that("morie_sensitivity_rosenbaum returns a gamma grid data frame", {
  set.seed(10)
  treated <- rnorm(20, mean = 1)
  control <- rnorm(25, mean = 0)
  res <- morie_sensitivity_rosenbaum(treated, control,
    gamma_range = c(1, 1.5, 2)
  )
  expect_s3_class(res, "data.frame")
  expect_named(res, c("gamma", "p_lower", "p_upper"))
  expect_equal(nrow(res), 3)
  expect_true(all(res$p_lower >= 0 & res$p_lower <= 1))
  expect_true(all(res$p_upper >= 0 & res$p_upper <= 1))
})

test_that("morie_estimate_g_computation returns outcome-regression ATE", {
  set.seed(11)
  df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
  res <- morie_estimate_g_computation(df, "t", "y", "x")
  expect_named(res, c("ate", "se", "ci_lower", "ci_upper"))
  expect_true(is.finite(res$ate))
  expect_true(res$ci_lower <= res$ci_upper)
})

test_that("morie_estimate_g_computation supports a logistic outcome model", {
  set.seed(12)
  df <- data.frame(
    t = rbinom(200, 1, 0.4),
    y = rbinom(200, 1, 0.5),
    x = rnorm(200)
  )
  res <- morie_estimate_g_computation(df, "t", "y", "x",
    outcome_model = "logistic"
  )
  expect_true(is.finite(res$ate))
})

test_that("morie_concordance_incomplete returns W for complete rankings", {
  X <- matrix(c(
    1, 2, 3, 4,
    1, 2, 3, 4,
    2, 1, 4, 3
  ), nrow = 4)
  res <- morie_concordance_incomplete(X)
  expect_named(res, c(
    "statistic", "p_value", "df", "chi2", "n", "k",
    "method"
  ))
  expect_true(res$statistic >= 0 && res$statistic <= 1)
  expect_true(res$p_value >= 0 && res$p_value <= 1)
  expect_equal(res$n, 4)
  expect_equal(res$k, 3)
  expect_equal(res$df, 3)
})

test_that("morie_concordance_incomplete handles incomplete rankings with NA", {
  X <- matrix(c(
    1, 2, 3, 4,
    NA, 1, 2, 3,
    2, 1, NA, 3
  ), nrow = 4)
  res <- morie_concordance_incomplete(X)
  expect_true(is.finite(res$statistic) || is.na(res$statistic))
})

test_that("morie_concordance_incomplete handles degenerate small input", {
  res <- morie_concordance_incomplete(matrix(1, 1, 1))
  expect_true(is.na(res$statistic))
  expect_equal(res$n, 1)
  expect_equal(res$k, 1)
})

test_that("cndrc detects a Condorcet winner", {
  M <- matrix(c(
    0, 60, 70,
    40, 0, 55,
    30, 45, 0
  ), nrow = 3, byrow = TRUE)
  res <- cndrc(M)
  expect_named(res, c("winner", "n_candidates", "has_winner", "method"))
  expect_equal(res$winner, 1L)
  expect_true(res$has_winner)
  expect_equal(res$n_candidates, 3)
})

test_that("cndrc reports no winner for a cycle", {
  M <- matrix(c(
    0, 60, 40,
    40, 0, 60,
    60, 40, 0
  ), nrow = 3, byrow = TRUE)
  res <- cndrc(M)
  expect_equal(res$winner, -1L)
  expect_false(res$has_winner)
})

test_that("morie_condorcet_winner alias is identical", {
  expect_identical(morie_condorcet_winner, cndrc)
})

test_that("morie_cnn1d_conv1d_forward computes valid cross-correlation", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(1, 0, -1)
  res <- morie_cnn1d_conv1d_forward(x, w, b = 0)
  expect_named(res, c("y", "estimate", "output_length", "method"))
  expect_equal(res$output_length, 3L)
  expect_length(res$y, 3)
  expect_true(all(is.finite(res$y)))
})

test_that("morie_cnn1d_conv1d_forward respects stride and padding", {
  x <- c(1, 2, 3, 4, 5, 6)
  w <- c(0.5, 0.5)
  res <- morie_cnn1d_conv1d_forward(x, w, b = 1, stride = 2L, padding = 1L)
  expect_true(res$output_length >= 1)
  expect_length(res$y, res$output_length)
})

test_that("morie_cnn1d_conv1d_forward errors when input shorter than kernel", {
  expect_error(morie_cnn1d_conv1d_forward(c(1, 2), c(1, 2, 3)))
})

test_that("morie_conv1d_forward alias is identical", {
  expect_identical(morie_conv1d_forward, morie_cnn1d_conv1d_forward)
})

test_that("morie_cnn2d_conv2d_forward computes valid 2D cross-correlation", {
  x <- matrix(1:16, 4, 4)
  w <- matrix(c(1, 0, 0, -1), 2, 2)
  res <- morie_cnn2d_conv2d_forward(x, w, b = 0)
  expect_named(res, c("y", "estimate", "output_shape", "method"))
  expect_equal(res$output_shape, c(3L, 3L))
  expect_equal(dim(res$y), c(3, 3))
  expect_true(all(is.finite(res$y)))
})

test_that("morie_cnn2d_conv2d_forward respects stride and padding", {
  x <- matrix(rnorm(36), 6, 6)
  w <- matrix(rnorm(9), 3, 3)
  res <- morie_cnn2d_conv2d_forward(x, w, b = 0.5, stride = 2L, padding = 1L)
  expect_equal(length(res$output_shape), 2)
  expect_equal(dim(res$y), res$output_shape)
})

test_that("morie_cnn2d_conv2d_forward errors when input smaller than kernel", {
  expect_error(morie_cnn2d_conv2d_forward(
    matrix(1:4, 2, 2),
    matrix(1:9, 3, 3)
  ))
})

test_that("morie_conv2d_forward alias is identical", {
  expect_identical(morie_conv2d_forward, morie_cnn2d_conv2d_forward)
})
