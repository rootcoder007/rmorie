# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for batch 06: fz* Fauzi nonparametric functions, GAN loss,
# GARCH, gradient boosting, and GBLUP.

test_that("fzkdf returns a structured KDFE bias-variance list", {
  set.seed(1)
  x <- rnorm(300)
  r <- fzkdf(x, t = 0)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "bias", "variance", "se", "h", "t", "n",
    "method"
  ))
  expect_true(is.finite(r$estimate))
  expect_gte(r$estimate, 0)
  expect_lte(r$estimate, 1)
  expect_gte(r$variance, 0)
  expect_equal(r$se, sqrt(r$variance))
  expect_equal(r$n, 300L)
  expect_equal(r$t, 0)
})

test_that("fzkdf uses defaults for t and h when NULL", {
  set.seed(2)
  x <- rnorm(120)
  r <- fzkdf(x)
  expect_true(is.finite(r$h))
  expect_gt(r$h, 0)
  expect_equal(r$t, stats::median(x))
})

test_that("fzkdf handles too-few observations", {
  r <- fzkdf(c(1))
  expect_true(is.na(r$estimate))
  expect_equal(r$n, 1L)
  expect_match(r$method, "too few")
})

test_that("fzkdf alias morie_fauzi_kdfe_properties is identical", {
  expect_identical(morie_fauzi_kdfe_properties, fzkdf)
})

test_that("fzksm runs the smoothed KS test with named normal cdf", {
  set.seed(3)
  x <- rnorm(200)
  r <- fzksm(x, cdf = "norm", args = list(0, 1))
  expect_type(r, "list")
  expect_named(r, c("statistic", "p_value", "h", "n", "method"))
  expect_true(is.finite(r$statistic))
  expect_gte(r$statistic, 0)
  expect_gte(r$p_value, 0)
  expect_lte(r$p_value, 1)
  expect_equal(r$n, 200L)
})

test_that("fzksm accepts a user-supplied cdf function and MLE args", {
  set.seed(4)
  x <- rnorm(150)
  r1 <- fzksm(x, cdf = function(t) pnorm(t))
  expect_true(is.finite(r1$statistic))
  r2 <- fzksm(x, cdf = "norm", args = NULL, n_grid = 128L)
  expect_true(is.finite(r2$statistic))
})

test_that("fzksm errors on non-normal string cdf and handles few obs", {
  set.seed(5)
  expect_error(fzksm(rnorm(20), cdf = "exp"))
  r <- fzksm(rnorm(3))
  expect_true(is.na(r$statistic))
  expect_match(r$method, "too few")
})

test_that("fzksm alias morie_fauzi_ks_smoothed is identical", {
  expect_identical(morie_fauzi_ks_smoothed, fzksm)
})

test_that("fzlst default score recovers the sample mean", {
  set.seed(6)
  x <- rnorm(80)
  r <- fzlst(x)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_lt(abs(r$estimate - mean(x)), 0.1)
  expect_true(is.finite(r$se))
  expect_gte(r$se, 0)
})

test_that("fzlst accepts a custom score function", {
  set.seed(7)
  x <- rnorm(60)
  r <- fzlst(x, score = function(u) 2 * u, n_quad = 100L)
  expect_true(is.finite(r$estimate))
  expect_gte(r$se, 0)
  expect_equal(r$n, 60L)
})

test_that("fzlst handles too-few observations", {
  r <- fzlst(c(2))
  expect_true(is.na(r$estimate))
  expect_match(r$method, "too few")
})

test_that("fzlst alias morie_fauzi_l_statistic is identical", {
  expect_identical(morie_fauzi_l_statistic, fzlst)
})

test_that("fzmis returns a MISE decomposition with positive parts", {
  set.seed(8)
  x <- rnorm(250)
  r <- fzmis(x)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "bias_part", "var_part", "h", "h_opt",
    "R_fpp", "sigma", "n", "method"
  ))
  expect_gt(r$bias_part, 0)
  expect_gt(r$var_part, 0)
  expect_equal(r$estimate, r$bias_part + r$var_part, tolerance = 1e-10)
  expect_gt(r$h_opt, 0)
  expect_gt(r$sigma, 0)
})

test_that("fzmis accepts an explicit bandwidth", {
  set.seed(9)
  x <- rnorm(100)
  r <- fzmis(x, h = 0.5)
  expect_equal(r$h, 0.5)
  expect_true(is.finite(r$estimate))
})

test_that("fzmis handles too-few observations", {
  r <- fzmis(c(1, 2, 3))
  expect_true(is.na(r$estimate))
  expect_match(r$method, "too few")
})

test_that("fzmis alias morie_fauzi_mise_computation is identical", {
  expect_identical(morie_fauzi_mise_computation, fzmis)
})

test_that("fzmrb estimates boundary-free MRL on positive data", {
  set.seed(10)
  x <- rexp(500, 1)
  r <- fzmrb(x, t = 0.5)
  expect_type(r, "list")
  expect_true(is.finite(r$estimate))
  expect_gte(r$estimate, 0)
  expect_gt(r$S_hat, 0)
  expect_equal(r$t, 0.5)
})

test_that("fzmrb uses default t and h when NULL", {
  set.seed(11)
  x <- rexp(300, 2)
  r <- fzmrb(x)
  expect_true(is.finite(r$h))
  expect_equal(r$t, stats::median(x))
})

test_that("fzmrb errors on non-positive x and non-positive t", {
  expect_error(fzmrb(c(1, 2, -1, 3)), "strictly positive")
  expect_error(fzmrb(c(1, 2, 3, 4), t = -1), "positive")
})

test_that("fzmrb handles few obs and the no-x-above-t branch", {
  r1 <- fzmrb(c(1))
  expect_true(is.na(r1$estimate))
  expect_match(r1$method, "too few")
  r2 <- fzmrb(c(0.1, 0.2, 0.3, 0.4), t = 100)
  expect_true(is.na(r2$estimate) || r2$estimate == 0)
})

test_that("fzmrb alias morie_fauzi_mrl_boundary_free is identical", {
  expect_identical(morie_fauzi_mrl_boundary_free, fzmrb)
})

test_that("fzmrl estimates kernel MRL with asymptotic se", {
  set.seed(12)
  x <- rexp(500, 1)
  r <- fzmrl(x, t = 0.5)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "S_hat", "t", "h", "n", "method"))
  expect_true(is.finite(r$estimate))
  expect_gte(r$estimate, 0)
  expect_gte(r$se, 0)
})

test_that("fzmrl uses default t and h", {
  set.seed(13)
  x <- rexp(200, 1)
  r <- fzmrl(x)
  expect_equal(r$t, stats::median(x))
  expect_gt(r$h, 0)
})

test_that("fzmrl handles few obs and no-x-above-t branch", {
  r1 <- fzmrl(c(5))
  expect_true(is.na(r1$estimate))
  expect_match(r1$method, "too few")
  r2 <- fzmrl(c(1, 2, 3, 4), t = 1000)
  expect_true(is.na(r2$estimate) || r2$estimate == 0)
})

test_that("fzmrl alias morie_fauzi_mrl_asymptotic is identical", {
  expect_identical(morie_fauzi_mrl_asymptotic, fzmrl)
})

test_that("fzqnt estimates the kernel median", {
  set.seed(14)
  x <- rnorm(400)
  r <- fzqnt(x, p = 0.5)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "se", "p", "h", "density_at_Q", "n",
    "method"
  ))
  expect_true(is.finite(r$estimate))
  expect_lt(abs(r$estimate), 0.5)
  expect_equal(r$p, 0.5)
  expect_gte(r$density_at_Q, 0)
})

test_that("fzqnt works at non-central probabilities", {
  set.seed(15)
  x <- rnorm(300)
  r <- fzqnt(x, p = 0.9)
  expect_true(is.finite(r$estimate))
  expect_gt(r$estimate, 0)
})

test_that("fzqnt errors on out-of-range p and handles few obs", {
  expect_error(fzqnt(rnorm(50), p = 0), "p must be")
  expect_error(fzqnt(rnorm(50), p = 1), "p must be")
  r <- fzqnt(c(1, 2, 3))
  expect_true(is.na(r$estimate))
  expect_match(r$method, "too few")
})

test_that("fzqnt alias morie_fauzi_kernel_quantile_asymptotic is identical", {
  expect_identical(morie_fauzi_kernel_quantile_asymptotic, fzqnt)
})

test_that("fzsgn runs the smoothed sign test two-sided", {
  set.seed(16)
  x <- rnorm(300)
  r <- fzsgn(x, theta0 = 0)
  expect_type(r, "list")
  expect_named(r, c(
    "statistic", "z", "p_value", "theta0", "h", "n",
    "method"
  ))
  expect_true(is.finite(r$z))
  expect_gte(r$p_value, 0)
  expect_lte(r$p_value, 1)
})

test_that("fzsgn supports greater and less alternatives", {
  set.seed(17)
  x <- rnorm(200)
  rg <- fzsgn(x, alternative = "greater")
  rl <- fzsgn(x, alternative = "less")
  expect_gte(rg$p_value, 0)
  expect_lte(rg$p_value, 1)
  expect_gte(rl$p_value, 0)
  expect_lte(rl$p_value, 1)
  expect_error(fzsgn(x, alternative = "bogus"))
})

test_that("fzsgn handles too-few observations", {
  r <- fzsgn(c(1, 2, 3))
  expect_true(is.na(r$statistic))
  expect_match(r$method, "too few")
})

test_that("fzsgn alias morie_fauzi_smoothed_sign is identical", {
  expect_identical(morie_fauzi_smoothed_sign, fzsgn)
})

test_that("fzsrv estimates the kernel survival with a 95% CI", {
  set.seed(18)
  x <- rexp(500, 1)
  r <- fzsrv(x, t = 1)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "se", "ci_lower", "ci_upper", "t", "h", "n",
    "method"
  ))
  expect_gte(r$estimate, 0)
  expect_lte(r$estimate, 1)
  expect_gte(r$ci_lower, 0)
  expect_lte(r$ci_upper, 1)
  expect_lte(r$ci_lower, r$ci_upper)
})

test_that("fzsrv uses default t and h", {
  set.seed(19)
  x <- rexp(200, 1)
  r <- fzsrv(x)
  expect_equal(r$t, stats::median(x))
  expect_gt(r$h, 0)
})

test_that("fzsrv handles too-few observations", {
  r <- fzsrv(c(7))
  expect_true(is.na(r$estimate))
  expect_match(r$method, "too few")
})

test_that("fzsrv alias morie_fauzi_survival_kernel is identical", {
  expect_identical(morie_fauzi_survival_kernel, fzsrv)
})

test_that("fzwlc runs the smoothed Wilcoxon signed-rank test", {
  set.seed(20)
  x <- rnorm(120)
  r <- fzwlc(x, theta0 = 0)
  expect_type(r, "list")
  expect_named(r, c(
    "statistic", "z", "p_value", "theta0", "h", "n",
    "method"
  ))
  expect_true(is.finite(r$z))
  expect_gte(r$p_value, 0)
  expect_lte(r$p_value, 1)
})

test_that("fzwlc supports greater and less alternatives", {
  set.seed(21)
  x <- rnorm(100)
  rg <- fzwlc(x, alternative = "greater")
  rl <- fzwlc(x, alternative = "less")
  expect_gte(rg$p_value, 0)
  expect_lte(rg$p_value, 1)
  expect_gte(rl$p_value, 0)
  expect_lte(rl$p_value, 1)
  expect_error(fzwlc(x, alternative = "bogus"))
})

test_that("fzwlc handles too-few observations", {
  r <- fzwlc(c(1, 2, 3))
  expect_true(is.na(r$statistic))
  expect_match(r$method, "too few")
})

test_that("fzwlc alias morie_fauzi_smoothed_wilcoxon is identical", {
  expect_identical(morie_fauzi_smoothed_wilcoxon, fzwlc)
})

test_that("morie_ganls_gan_loss computes minimax losses", {
  set.seed(22)
  D_real <- runif(50, 0.5, 1)
  D_fake <- runif(50, 0, 0.5)
  r <- morie_ganls_gan_loss(D_real, D_fake, kind = "minimax")
  expect_type(r, "list")
  expect_named(r, c("d_loss", "g_loss", "v", "estimate", "kind", "method"))
  expect_true(is.finite(r$d_loss))
  expect_true(is.finite(r$g_loss))
  expect_equal(r$estimate, r$d_loss)
  expect_equal(r$kind, "minimax")
})

test_that("morie_ganls_gan_loss supports the non-saturating objective", {
  set.seed(23)
  D_real <- runif(40, 0.5, 1)
  D_fake <- runif(40, 0, 0.5)
  r <- morie_ganls_gan_loss(D_real, D_fake, kind = "nonsaturating")
  expect_equal(r$kind, "nonsaturating")
  expect_true(is.finite(r$g_loss))
})

test_that("morie_ganls_gan_loss errors on an unknown kind", {
  expect_error(morie_ganls_gan_loss(c(0.6, 0.7), c(0.2, 0.3), kind = "bad"))
})

test_that("morie_gan_loss alias is identical to morie_ganls_gan_loss", {
  expect_identical(morie_gan_loss, morie_ganls_gan_loss)
})

test_that("morie_garch_fit fits a GARCH(1,1) return series", {
  set.seed(24)
  x <- rnorm(300, sd = 0.02)
  r <- morie_garch_fit(x)
  expect_type(r, "list")
  expect_true(all(c(
    "omega", "alpha", "beta", "persistence", "loglik",
    "conditional_variance", "n", "method"
  ) %in% names(r)))
  expect_gt(r$omega, 0)
  expect_gte(r$alpha, 0)
  expect_gte(r$beta, 0)
  expect_lt(r$persistence, 1)
  expect_true(is.finite(r$loglik))
  expect_equal(r$n, 300L)
  expect_true(all(r$conditional_variance > 0))
})

test_that("morie_garch_fit errors on too-short series", {
  expect_error(morie_garch_fit(rnorm(5)), ">=10")
})

test_that("morie_gradient_boosting_ensemble fits a regression task", {
  set.seed(25)
  x <- matrix(rnorm(200), ncol = 4)
  y <- x[, 1] + 0.3 * rnorm(50)
  r <- morie_gradient_boosting_ensemble(x, y,
    n_estimators = 20L, task = "regression",
    seed = 25L
  )
  expect_type(r, "list")
  expect_true(all(c(
    "estimate", "train_score", "feature_importances",
    "n_estimators", "task", "n", "method"
  ) %in% names(r)))
  expect_equal(r$task, "regression")
  expect_length(r$feature_importances, 4L)
  expect_true(is.finite(r$estimate))
  expect_equal(r$n, 50L)
})

test_that("morie_gradient_boosting_ensemble fits a classification task", {
  set.seed(26)
  x <- matrix(rnorm(200), ncol = 4)
  y <- as.integer(x[, 1] + rnorm(50) > 0)
  r <- morie_gradient_boosting_ensemble(x, y, n_estimators = 20L, seed = 26L)
  expect_equal(r$task, "classification")
  expect_gte(r$train_score, 0)
  expect_lte(r$train_score, 1)
})

test_that("morie_gradient_boosting_genomic predicts from a marker matrix", {
  set.seed(14)
  M <- matrix(rnorm(160), 40, 4)
  y <- sign(M[, 1]) + 0.3 * rnorm(40)
  r <- morie_gradient_boosting_genomic(rep(0, 40), y, M,
    n_estimators = 20,
    seed = 14
  )
  expect_type(r, "list")
  expect_named(r, c("estimate", "y_hat", "train_loss", "se", "n", "method"))
  expect_length(r$y_hat, 40L)
  expect_true(all(is.finite(r$y_hat)))
  expect_gte(r$se, 0)
  expect_equal(r$n, 40L)
})

test_that("morie_gradient_boosting_genomic works with NULL fixed features", {
  set.seed(27)
  M <- matrix(rnorm(120), 30, 4)
  y <- M[, 2] + 0.2 * rnorm(30)
  r <- morie_gradient_boosting_genomic(NULL, y, M, n_estimators = 15, seed = 27)
  expect_length(r$y_hat, 30L)
  expect_true(is.finite(r$estimate))
})

test_that("morie_gblup_full solves the mixed model with default lambda", {
  set.seed(28)
  M <- matrix(sample(0:2, 200, TRUE), 40, 5)
  y <- M %*% rnorm(5) + rnorm(40)
  r <- morie_gblup_full(rep(0, 40), as.numeric(y), M)
  expect_type(r, "list")
  expect_true(all(c(
    "estimate", "g_hat", "beta", "se", "y_hat",
    "lambda_gblup", "n", "method"
  ) %in% names(r)))
  expect_length(r$g_hat, 40L)
  expect_length(r$y_hat, 40L)
  expect_true(all(is.finite(r$y_hat)))
  expect_gt(r$se, 0)
  expect_equal(r$n, 40L)
})

test_that("morie_gblup_full accepts an explicit lambda and NULL fixed effects", {
  set.seed(29)
  M <- matrix(sample(0:2, 150, TRUE), 30, 5)
  y <- as.numeric(M %*% rnorm(5) + rnorm(30))
  r <- morie_gblup_full(NULL, y, M, lambda_gblup = 2)
  expect_equal(r$lambda_gblup, 2)
  expect_true(is.finite(r$estimate))
  expect_length(r$g_hat, 30L)
})
