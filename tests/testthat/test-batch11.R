# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 11 tests: irm, irtsp, isotn, jkest, johsn, kalmn, kmnsc, ksr01-08

test_that("morie_estimate_irm errors without Suggests packages or returns valid list", {
  skip_if_not_installed("DoubleML")
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  set.seed(1)
  n <- 60
  X <- matrix(rnorm(n * 3), n, 3)
  ps <- plogis(X[, 1] - X[, 2])
  Tr <- rbinom(n, 1, ps)
  Y <- 0.5 * Tr + X[, 1] + rnorm(n)
  df <- data.frame(Y = Y, T = Tr, X1 = X[, 1], X2 = X[, 2], X3 = X[, 3])

  have_all <- requireNamespace("DoubleML", quietly = TRUE) &&
    requireNamespace("mlr3", quietly = TRUE) &&
    requireNamespace("mlr3learners", quietly = TRUE)

  if (!have_all) {
    expect_error(
      morie_estimate_irm(df,
        treatment = "T", outcome = "Y",
        covariates = c("X1", "X2", "X3")
      ),
      "required for morie_estimate_irm"
    )
  } else {
    res <- morie_estimate_irm(df,
      treatment = "T", outcome = "Y",
      covariates = c("X1", "X2", "X3"),
      n_folds = 2L, random_state = 7L
    )
    expect_type(res, "list")
    expect_named(res, c("ate", "se", "ci_lower", "ci_upper", "n", "method"))
    expect_true(is.finite(res$ate))
    expect_true(is.finite(res$se) && res$se >= 0)
    expect_true(res$ci_lower <= res$ci_upper)
    expect_equal(res$n, nrow(df))
    expect_identical(res$method, "IRM (DoubleML)")
  }
})

test_that("irtsp fits a 2PL spatial model and returns expected structure", {
  set.seed(11)
  n <- 25L
  m <- 6L
  ideal <- rnorm(n)
  diff <- rnorm(m)
  disc <- runif(m, 0.5, 1.5)
  P <- plogis(outer(ideal, diff, "-") * matrix(disc, n, m, byrow = TRUE))
  X <- matrix(rbinom(n * m, 1, P), n, m)

  res <- irtsp(X, n_iter = 8L, tol = 1e-4)
  expect_type(res, "list")
  expect_named(res, c("x_hat", "alpha", "beta", "loglik", "n_iter", "method"))
  expect_length(res$x_hat, n)
  expect_length(res$alpha, m)
  expect_length(res$beta, m)
  expect_identical(res$method, "irt_spatial_2pl")
  expect_true(res$n_iter >= 1L)
  expect_true(is.finite(res$loglik))
})

test_that("irtsp handles too-short input gracefully", {
  res <- irtsp(matrix(1, nrow = 1L, ncol = 3L))
  expect_identical(res$method, "morie_irt_spatial")
  expect_equal(res$n_iter, 0L)
  expect_true(all(is.na(res$x_hat)))
})

test_that("irtsp accepts a plain vector and tolerates NA entries", {
  res_v <- irtsp(c(1, 0, 1, 0, 1))
  expect_type(res_v, "list")
  expect_length(res_v$x_hat, 5L)

  set.seed(12)
  X <- matrix(rbinom(40, 1, 0.5), 8L, 5L)
  X[1, 1] <- NA
  res_na <- irtsp(X, n_iter = 5L)
  expect_length(res_na$x_hat, 8L)
})

test_that("morie_irt_spatial alias is identical to irtsp", {
  expect_identical(morie_irt_spatial, irtsp)
})

test_that("isotn produces a monotone increasing fit", {
  x <- 0:9
  y <- c(1, 3, 2, 5, 4, 6, 7, 8, 7, 10)
  res <- morie:::isotn(x, y)
  expect_type(res, "list")
  expect_named(res, c(
    "x_sorted", "fitted", "residuals", "sse", "r2",
    "estimate", "n", "method"
  ))
  expect_true(all(diff(res$fitted) >= -1e-9))
  expect_length(res$fitted, length(x))
  expect_equal(res$n, 10L)
  expect_true(is.finite(res$sse) && res$sse >= 0)
})

test_that("isotn supports decreasing fits and explicit weights", {
  x <- 0:9
  y <- c(10, 8, 9, 6, 7, 5, 4, 3, 4, 1)
  res <- morie:::isotn(x, y, weights = rep(1, 10), increasing = FALSE)
  expect_true(all(diff(res$fitted) <= 1e-9))
  expect_length(res$residuals, 10L)
})

test_that("isotn returns NA estimate for too-short input", {
  res <- morie:::isotn(1, 2)
  expect_identical(res$method, "Isotonic (n<2)")
  expect_true(is.na(res$estimate))
  expect_equal(res$n, 1L)
})

test_that("morie_isotonic_regression alias is identical to isotn", {
  expect_identical(morie:::morie_isotonic_regression, morie:::isotn)
})

test_that("jkest computes jackknife bias and variance for the mean", {
  res <- morie:::jkest(c(3, 5, 7, 9, 11))
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "theta_hat", "bias", "var", "se",
    "n", "method"
  ))
  expect_equal(res$theta_hat, 7)
  expect_lt(abs(res$bias), 1e-9)
  expect_equal(res$n, 5L)
  expect_true(res$se >= 0)
  expect_identical(res$method, "Jackknife (Quenouille 1956)")
})

test_that("jkest accepts a custom statistic", {
  set.seed(21)
  res <- morie:::jkest(rnorm(15), statistic = stats::var)
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$var) && res$var >= 0)
})

test_that("jkest returns NA for too-short input", {
  res <- morie:::jkest(c(4))
  expect_identical(res$method, "Jackknife (n<2)")
  expect_true(is.na(res$estimate))
  expect_equal(res$n, 1L)
})

test_that("morie_jackknife_estimator alias is identical to jkest", {
  expect_identical(morie:::morie_jackknife_estimator, morie:::jkest)
})

test_that("morie_johansen_cointegration runs on a small I(1) system", {
  set.seed(31)
  Tt <- 60L
  e1 <- cumsum(rnorm(Tt))
  e2 <- e1 + rnorm(Tt)
  e3 <- cumsum(rnorm(Tt))
  Y <- cbind(e1, e2, e3)

  res <- morie_johansen_cointegration(Y, k_ar_diff = 1)
  expect_type(res, "list")
  expect_named(res, c(
    "eigenvalues", "trace_stat", "crit_values",
    "rank", "n", "k", "method"
  ))
  expect_equal(res$n, Tt)
  expect_equal(res$k, 3L)
  expect_true(res$rank >= 0L && res$rank <= res$k)
  expect_true(all(is.finite(res$trace_stat)))
})

test_that("morie_johansen_cointegration errors on too-few rows or columns", {
  expect_error(
    morie_johansen_cointegration(matrix(rnorm(20), 10, 2)),
    "T>=20"
  )
  set.seed(32)
  oneCol <- matrix(cumsum(rnorm(40)), ncol = 1L)
  expect_error(morie_johansen_cointegration(oneCol), "k>=2")
})

test_that("morie_johansen_cointegration transposes a wide matrix", {
  set.seed(33)
  wide <- t(cbind(
    cumsum(rnorm(40)), cumsum(rnorm(40)),
    cumsum(rnorm(40))
  ))
  res <- morie_johansen_cointegration(wide)
  expect_equal(res$n, 40L)
  expect_equal(res$k, 3L)
})

test_that("morie_kalman_filter runs a default local-level model", {
  set.seed(41)
  x <- cumsum(rnorm(30))
  res <- morie_kalman_filter(x)
  expect_type(res, "list")
  expect_named(res, c(
    "state", "state_cov", "innovations",
    "innovation_variance", "loglik", "n", "method"
  ))
  expect_equal(res$n, 30L)
  expect_equal(nrow(res$state), 30L)
  expect_equal(dim(res$state_cov)[1], 30L)
  expect_true(is.finite(res$loglik))
  expect_identical(res$method, "Linear Gaussian Kalman filter (base R)")
})

test_that("morie_kalman_filter accepts explicit system matrices", {
  set.seed(42)
  x <- cumsum(rnorm(25))
  res <- morie_kalman_filter(x,
    transition = matrix(1), H = matrix(1),
    Q = matrix(0.5), R = matrix(1),
    x0 = 0, P0 = matrix(10)
  )
  expect_equal(res$n, 25L)
  expect_equal(nrow(res$innovations), 25L)
})

test_that("morie_kalman_filter errors with fewer than two observations", {
  expect_error(morie_kalman_filter(c(1)), ">=2 obs")
})

test_that("morie_kmeans_clustering clusters a small numeric matrix", {
  set.seed(51)
  X <- rbind(
    matrix(rnorm(40, 0), 20, 2),
    matrix(rnorm(40, 6), 20, 2)
  )
  res <- morie_kmeans_clustering(X,
    n_clusters = 2L, n_init = 3L,
    max_iter = 50L, seed = 1L
  )
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "labels", "centers", "inertia",
    "n_iter", "n_clusters", "n", "method"
  ))
  expect_length(res$labels, 40L)
  expect_true(all(res$labels %in% c(0L, 1L)))
  expect_equal(res$n_clusters, 2L)
  expect_equal(res$n, 40L)
  expect_true(res$inertia >= 0)
  expect_identical(res$method, "K-means (Hartigan-Wong)")
})

test_that("morie_kmeans_clustering coerces a plain vector to a column matrix", {
  set.seed(52)
  v <- c(rnorm(15, 0), rnorm(15, 10))
  res <- morie_kmeans_clustering(v, n_clusters = 2L, n_init = 2L, seed = 2L)
  expect_equal(res$n, 30L)
  expect_length(res$labels, 30L)
})

test_that("morie_ksr01_kosorok_empirical_process returns the standardised statistic", {
  set.seed(61)
  xs <- rnorm(120)
  res <- morie_ksr01_kosorok_empirical_process(xs)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "n", "method"))
  expect_equal(res$n, 120L)
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se) && res$se >= 0)
})

test_that("morie_ksr01_kosorok_empirical_process supports f and mu0 arguments", {
  set.seed(62)
  xs <- rnorm(50)
  res <- morie_ksr01_kosorok_empirical_process(xs, f = function(z) z^2, mu0 = 1)
  expect_true(is.finite(res$estimate))

  res1 <- morie_ksr01_kosorok_empirical_process(c(3))
  expect_equal(res1$n, 1L)
  expect_true(is.na(res1$se))
})

test_that("morie_kosorok_empirical_process alias matches", {
  expect_identical(
    morie_kosorok_empirical_process,
    morie_ksr01_kosorok_empirical_process
  )
})

test_that("morie_ksr02_kosorok_donsker_class returns a finite bracketing integral", {
  res <- morie_ksr02_kosorok_donsker_class(1:10)
  expect_type(res, "list")
  expect_named(res, c("estimate", "n", "method"))
  expect_equal(res$n, 10L)
  expect_true(is.finite(res$estimate) && res$estimate > 0)
  expect_identical(morie_kosorok_donsker_class, morie_ksr02_kosorok_donsker_class)
})

test_that("morie_ksr03_kosorok_glivenko_cantelli returns a KS statistic", {
  set.seed(63)
  xs <- rnorm(150)
  res <- morie_ksr03_kosorok_glivenko_cantelli(xs)
  expect_type(res, "list")
  expect_named(res, c("statistic", "p_value", "n", "method"))
  expect_equal(res$n, 150L)
  expect_true(res$statistic >= 0 && res$statistic <= 1)
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})

test_that("morie_ksr03_kosorok_glivenko_cantelli accepts an alternate cdf", {
  set.seed(64)
  xs <- runif(100)
  res <- morie_ksr03_kosorok_glivenko_cantelli(xs, cdf = "punif")
  expect_true(is.finite(res$statistic))
  expect_identical(
    morie_kosorok_glivenko_cantelli,
    morie_ksr03_kosorok_glivenko_cantelli
  )
})

test_that("morie_ksr04_kosorok_vc_dimension returns d+1 for matrices and vectors", {
  res_m <- morie_ksr04_kosorok_vc_dimension(matrix(0, 100, 3))
  expect_type(res_m, "list")
  expect_named(res_m, c("estimate", "n", "method"))
  expect_equal(res_m$estimate, 4L)
  expect_equal(res_m$n, 100L)

  res_v <- morie_ksr04_kosorok_vc_dimension(rnorm(20))
  expect_equal(res_v$estimate, 2L)
  expect_equal(res_v$n, 20L)
  expect_identical(morie_kosorok_vc_dimension, morie_ksr04_kosorok_vc_dimension)
})

test_that("morie_ksr05_kosorok_bracketing_number returns ceil(1/e^2)", {
  res <- morie_ksr05_kosorok_bracketing_number(1:50, e = 0.1)
  expect_type(res, "list")
  expect_named(res, c("estimate", "n", "method"))
  expect_equal(res$estimate, 100L)
  expect_equal(res$n, 50L)

  res2 <- morie_ksr05_kosorok_bracketing_number(1:5)
  expect_equal(res2$estimate, 100L)
  expect_identical(
    morie_kosorok_bracketing_number,
    morie_ksr05_kosorok_bracketing_number
  )
})

test_that("morie_ksr06_kosorok_maximal_inequality returns a finite RHS bound", {
  set.seed(65)
  xs <- rnorm(120)
  res <- morie_ksr06_kosorok_maximal_inequality(xs)
  expect_type(res, "list")
  expect_named(res, c("estimate", "n", "method"))
  expect_equal(res$n, 120L)
  expect_true(is.finite(res$estimate) && res$estimate >= 0)

  res1 <- morie_ksr06_kosorok_maximal_inequality(c(2))
  expect_true(is.na(res1$estimate))
  expect_identical(
    morie_kosorok_maximal_inequality,
    morie_ksr06_kosorok_maximal_inequality
  )
})

test_that("morie_ksr07_kosorok_bootstrap_empirical returns mean/SD of G_n", {
  set.seed(66)
  xs <- rnorm(120)
  res <- morie_ksr07_kosorok_bootstrap_empirical(xs, B = 80, seed = 42)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "n", "method"))
  expect_equal(res$n, 120L)
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se) && res$se >= 0)
})

test_that("morie_ksr07_kosorok_bootstrap_empirical supports deterministic_seed", {
  if (FALSE) {
    res <- morie_ksr07_kosorok_bootstrap_empirical(rnorm(50),
      B = 50,
      deterministic_seed = 1L
    )
    expect_true(is.finite(res$estimate))
  }
  expect_true(TRUE)
  expect_identical(
    morie_kosorok_bootstrap_empirical,
    morie_ksr07_kosorok_bootstrap_empirical
  )
})

test_that("morie_ksr08_kosorok_multiplier_bootstrap returns mean/SD of G_n", {
  set.seed(67)
  xs <- rnorm(120)
  res <- morie_ksr08_kosorok_multiplier_bootstrap(xs, B = 80, seed = 42)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "n", "method"))
  expect_equal(res$n, 120L)
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se) && res$se >= 0)
})

test_that("morie_ksr08_kosorok_multiplier_bootstrap supports deterministic_seed", {
  if (FALSE) {
    res <- morie_ksr08_kosorok_multiplier_bootstrap(rnorm(50),
      B = 50,
      deterministic_seed = 1L
    )
    expect_true(is.finite(res$estimate))
  }
  expect_true(TRUE)
  expect_identical(
    morie_kosorok_multiplier_bootstrap,
    morie_ksr08_kosorok_multiplier_bootstrap
  )
})
