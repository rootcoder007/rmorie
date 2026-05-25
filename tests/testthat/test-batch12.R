# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 12: ksr09-ksr20, ktaup, kvcmp, latnh

test_that("morie_ksr09_kosorok_z_estimator: location path (y NULL)", {
  set.seed(1)
  x <- rnorm(120)
  r <- morie_ksr09_kosorok_z_estimator(x)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 120L)
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se) && r$se > 0)
  expect_equal(r$estimate, mean(x))
  expect_type(r$method, "character")
})

test_that("morie_ksr09_kosorok_z_estimator: OLS slope path (y supplied)", {
  set.seed(2)
  x <- rnorm(150)
  y <- 1.5 * x + rnorm(150)
  r <- morie_ksr09_kosorok_z_estimator(x, y)
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 150L)
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se) && r$se > 0)
  expect_gt(r$estimate, 0.5)
})

test_that("morie_ksr09_kosorok_z_estimator: alias matches and accepts integer input", {
  expect_identical(morie_kosorok_z_estimator, morie_ksr09_kosorok_z_estimator)
  r <- morie_ksr09_kosorok_z_estimator(1:10)
  expect_equal(r$estimate, 5.5)
  expect_equal(r$n, 10L)
})

test_that("morie_ksr10_kosorok_m_estimator: default path", {
  set.seed(3)
  x <- rnorm(200)
  r <- morie_ksr10_kosorok_m_estimator(x)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 200L)
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se) && r$se > 0)
  expect_true(grepl("Huber", r$method))
})

test_that("morie_ksr10_kosorok_m_estimator: custom tuning and iteration args", {
  set.seed(4)
  x <- rnorm(80)
  r <- morie_ksr10_kosorok_m_estimator(x, k = 2.0, max_iter = 50, tol = 1e-8)
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_true(is.finite(r$estimate))
  expect_true(grepl("2.000", r$method))
})

test_that("morie_ksr10_kosorok_m_estimator: zero-spread input (eta fallback)", {
  r <- morie_ksr10_kosorok_m_estimator(rep(5, 20))
  expect_equal(r$n, 20L)
  expect_equal(r$estimate, 5)
  expect_true(is.finite(r$estimate))
})

test_that("morie_ksr10_kosorok_m_estimator: alias identity", {
  expect_identical(morie_kosorok_m_estimator, morie_ksr10_kosorok_m_estimator)
})

test_that("morie_ksr11_kosorok_efficient_score: standard path", {
  set.seed(5)
  x <- rnorm(150)
  y <- 1.5 * x + rnorm(150)
  r <- morie_ksr11_kosorok_efficient_score(x, y)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 150L)
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se) && r$se > 0)
})

test_that("morie_ksr11_kosorok_efficient_score: mean efficient score near zero at MLE", {
  set.seed(6)
  x <- rnorm(200)
  y <- 0.8 * x + rnorm(200)
  r <- morie_ksr11_kosorok_efficient_score(x, y)
  expect_lt(abs(r$estimate), 1e-6)
})

test_that("morie_ksr11_kosorok_efficient_score: alias identity", {
  expect_identical(morie_kosorok_efficient_score, morie_ksr11_kosorok_efficient_score)
})

test_that("morie_ksr12_kosorok_information_bound: standard path", {
  set.seed(7)
  x <- rnorm(150)
  y <- 1.5 * x + rnorm(150)
  r <- morie_ksr12_kosorok_information_bound(x, y)
  expect_type(r, "list")
  expect_named(r, c("estimate", "n", "method"))
  expect_equal(r$n, 150L)
  expect_true(is.finite(r$estimate) && r$estimate > 0)
})

test_that("morie_ksr12_kosorok_information_bound: alias identity", {
  expect_identical(morie_kosorok_information_bound, morie_ksr12_kosorok_information_bound)
})

test_that("morie_ksr13_kosorok_tangent_space: rank of empirical Gram matrix", {
  set.seed(8)
  x <- rnorm(200)
  r <- morie_ksr13_kosorok_tangent_space(x)
  expect_type(r, "list")
  expect_named(r, c("estimate", "n", "method"))
  expect_equal(r$n, 200L)
  expect_type(r$estimate, "integer")
  expect_true(r$estimate >= 0L && r$estimate <= 2L)
})

test_that("morie_ksr13_kosorok_tangent_space: full-rank for generic data", {
  set.seed(9)
  r <- morie_ksr13_kosorok_tangent_space(rnorm(100))
  expect_equal(r$estimate, 2L)
})

test_that("morie_ksr13_kosorok_tangent_space: alias identity", {
  expect_identical(morie_kosorok_tangent_space, morie_ksr13_kosorok_tangent_space)
})

test_that("morie_ksr14_kosorok_profile_likelihood: standard path", {
  set.seed(10)
  x <- rnorm(150)
  y <- 1.5 * x + rnorm(150)
  r <- morie_ksr14_kosorok_profile_likelihood(x, y)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 150L)
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se) && r$se > 0)
  expect_gt(r$estimate, 0.5)
})

test_that("morie_ksr14_kosorok_profile_likelihood: alias identity", {
  expect_identical(morie_kosorok_profile_likelihood, morie_ksr14_kosorok_profile_likelihood)
})

test_that("morie_ksr15_kosorok_one_step_estimator: default path", {
  set.seed(11)
  x <- rnorm(200)
  r <- morie_ksr15_kosorok_one_step_estimator(x)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 200L)
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se) && r$se > 0)
})

test_that("morie_ksr15_kosorok_one_step_estimator: one-step from median equals mean", {
  x <- c(1, 2, 3, 4, 100)
  r <- morie_ksr15_kosorok_one_step_estimator(x)
  expect_equal(r$estimate, mean(x))
})

test_that("morie_ksr15_kosorok_one_step_estimator: alias identity", {
  expect_identical(morie_kosorok_one_step_estimator, morie_ksr15_kosorok_one_step_estimator)
})

test_that("morie_ksr16_kosorok_influence_function: standard path", {
  set.seed(12)
  x <- rnorm(150)
  y <- 1.5 * x + rnorm(150)
  r <- morie_ksr16_kosorok_influence_function(x, y)
  expect_type(r, "list")
  expect_named(r, c("estimate", "n", "method"))
  expect_equal(r$n, 150L)
  expect_true(is.finite(r$estimate))
})

test_that("morie_ksr16_kosorok_influence_function: mean IF near zero", {
  set.seed(13)
  x <- rnorm(200)
  y <- 0.5 * x + rnorm(200)
  r <- morie_ksr16_kosorok_influence_function(x, y)
  expect_lt(abs(r$estimate), 1e-6)
})

test_that("morie_ksr16_kosorok_influence_function: alias identity", {
  expect_identical(morie_kosorok_influence_function, morie_ksr16_kosorok_influence_function)
})

test_that("morie_ksr17_kosorok_counting_process: counts events", {
  ev <- c(1, 1, 0, 1, 1, 0, 1, 1, 1, 0)
  r <- morie_ksr17_kosorok_counting_process(1:10, ev)
  expect_type(r, "list")
  expect_named(r, c("estimate", "n", "method"))
  expect_equal(r$estimate, 7L)
  expect_equal(r$n, 10L)
  expect_type(r$estimate, "integer")
})

test_that("morie_ksr17_kosorok_counting_process: logical events and all-censored", {
  r1 <- morie_ksr17_kosorok_counting_process(1:5, c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(r1$estimate, 3L)
  r0 <- morie_ksr17_kosorok_counting_process(1:5, rep(0, 5))
  expect_equal(r0$estimate, 0L)
})

test_that("morie_ksr17_kosorok_counting_process: alias identity", {
  expect_identical(morie_kosorok_counting_process, morie_ksr17_kosorok_counting_process)
})

test_that("morie_ksr18_kosorok_nelson_aalen: cumulative hazard", {
  ev <- c(1, 1, 0, 1, 1, 0, 1, 1, 1, 0)
  r <- morie_ksr18_kosorok_nelson_aalen(1:10, ev)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 10L)
  expect_true(is.finite(r$estimate) && r$estimate > 0)
  expect_true(is.finite(r$se) && r$se > 0)
})

test_that("morie_ksr18_kosorok_nelson_aalen: handles tied times and unsorted input", {
  r <- morie_ksr18_kosorok_nelson_aalen(c(3, 1, 2, 2, 1), c(1, 1, 1, 0, 0))
  expect_equal(r$n, 5L)
  expect_true(is.finite(r$estimate) && r$estimate >= 0)
})

test_that("morie_ksr18_kosorok_nelson_aalen: all-censored gives zero hazard", {
  r <- morie_ksr18_kosorok_nelson_aalen(1:6, rep(0, 6))
  expect_equal(r$estimate, 0)
  expect_equal(r$se, 0)
})

test_that("morie_ksr18_kosorok_nelson_aalen: alias identity", {
  expect_identical(morie_kosorok_nelson_aalen, morie_ksr18_kosorok_nelson_aalen)
})

test_that("morie_ksr19_kosorok_cox_partial_likelihood: default path", {
  set.seed(14)
  x <- rnorm(100)
  t <- rexp(100, rate = exp(0.5 * x))
  r <- morie_ksr19_kosorok_cox_partial_likelihood(x, t, rep(1, 100))
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 100L)
  expect_true(is.finite(r$estimate))
  expect_true(is.na(r$se) || (is.finite(r$se) && r$se > 0))
})

test_that("morie_ksr19_kosorok_cox_partial_likelihood: custom tol / max_iter", {
  set.seed(15)
  x <- rnorm(60)
  t <- rexp(60, rate = exp(0.3 * x))
  r <- morie_ksr19_kosorok_cox_partial_likelihood(x, t, rep(1, 60),
    tol = 1e-6, max_iter = 30
  )
  expect_equal(r$n, 60L)
  expect_true(is.finite(r$estimate))
})

test_that("morie_ksr19_kosorok_cox_partial_likelihood: censored observations", {
  set.seed(16)
  x <- rnorm(80)
  t <- rexp(80, rate = exp(0.4 * x))
  ev <- rbinom(80, 1, 0.7)
  r <- morie_ksr19_kosorok_cox_partial_likelihood(x, t, ev)
  expect_equal(r$n, 80L)
  expect_true(is.finite(r$estimate))
})

test_that("morie_ksr19_kosorok_cox_partial_likelihood: alias identity", {
  expect_identical(
    morie_kosorok_cox_partial_likelihood,
    morie_ksr19_kosorok_cox_partial_likelihood
  )
})

test_that("morie_ksr20_kosorok_censoring_survival: standard path", {
  ev <- c(1, 1, 0, 1, 1, 0, 1, 1, 1, 0)
  r <- morie_ksr20_kosorok_censoring_survival(1:10, ev)
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 10L)
  expect_true(is.finite(r$estimate) && r$estimate >= 0 && r$estimate <= 1)
  expect_true(is.finite(r$se) && r$se >= 0)
})

test_that("morie_ksr20_kosorok_censoring_survival: no censoring keeps S at 1", {
  r <- morie_ksr20_kosorok_censoring_survival(1:8, rep(1, 8))
  expect_equal(r$estimate, 1)
  expect_equal(r$se, 0)
})

test_that("morie_ksr20_kosorok_censoring_survival: tied/unsorted times", {
  r <- morie_ksr20_kosorok_censoring_survival(c(2, 1, 2, 3, 1), c(1, 0, 0, 1, 0))
  expect_equal(r$n, 5L)
  expect_true(is.finite(r$estimate) && r$estimate >= 0 && r$estimate <= 1)
})

test_that("morie_ksr20_kosorok_censoring_survival: alias identity", {
  expect_identical(morie_kosorok_censoring_survival, morie_ksr20_kosorok_censoring_survival)
})

test_that("morie_kendall_tau_partial: standard path", {
  set.seed(17)
  z <- rnorm(60)
  x <- z + rnorm(60)
  y <- z + rnorm(60)
  r <- morie_kendall_tau_partial(x, y, z)
  expect_type(r, "list")
  expect_named(r, c(
    "statistic", "p_value", "tau_xy", "tau_xz",
    "tau_yz", "z", "n", "method"
  ))
  expect_equal(r$n, 60L)
  expect_true(is.finite(r$statistic) && abs(r$statistic) <= 1)
  expect_true(is.finite(r$p_value) && r$p_value >= 0 && r$p_value <= 1)
  expect_true(all(is.finite(c(r$tau_xy, r$tau_xz, r$tau_yz))))
})

test_that("morie_kendall_tau_partial: short input (n < 4) returns NA result", {
  r <- morie_kendall_tau_partial(1:3, 1:3, 1:3)
  expect_true(is.na(r$statistic))
  expect_true(is.na(r$p_value))
  expect_equal(r$n, 3L)
  expect_equal(r$method, "Kendall partial tau")
})

test_that("morie_kendall_tau_partial: unequal lengths truncate to min", {
  set.seed(18)
  r <- morie_kendall_tau_partial(rnorm(20), rnorm(15), rnorm(10))
  expect_equal(r$n, 10L)
})

test_that("morie_kendall_tau_partial: degenerate denom returns NA statistic", {
  x <- 1:10
  z <- 1:10
  set.seed(19)
  y <- rnorm(10)
  r <- morie_kendall_tau_partial(x, y, z)
  expect_true(is.na(r$statistic))
  expect_true(is.na(r$p_value))
  expect_true(is.finite(r$tau_xz))
})

test_that("kv_cache_management: fresh cache (NULL inputs)", {
  k <- matrix(rnorm(6), nrow = 2)
  v <- matrix(rnorm(6), nrow = 2)
  r <- morie:::kv_cache_management(NULL, NULL, k, v)
  expect_type(r, "list")
  expect_named(r, c("K", "V", "T", "max_len", "method"))
  expect_equal(r$T, 2L)
  expect_equal(nrow(r$K), 2L)
  expect_equal(nrow(r$V), 2L)
  expect_null(r$max_len)
})

test_that("kv_cache_management: appends to existing cache", {
  k0 <- matrix(rnorm(9), nrow = 3)
  v0 <- matrix(rnorm(9), nrow = 3)
  k1 <- matrix(rnorm(6), nrow = 2)
  v1 <- matrix(rnorm(6), nrow = 2)
  r <- morie:::kv_cache_management(k0, v0, k1, v1)
  expect_equal(r$T, 5L)
  expect_equal(nrow(r$K), 5L)
})

test_that("kv_cache_management: max_len truncates to most recent rows", {
  k0 <- matrix(seq_len(12), nrow = 6)
  v0 <- matrix(seq_len(12), nrow = 6)
  k1 <- matrix(rnorm(4), nrow = 2)
  v1 <- matrix(rnorm(4), nrow = 2)
  r <- morie:::kv_cache_management(k0, v0, k1, v1, max_len = 4L)
  expect_equal(r$T, 4L)
  expect_equal(nrow(r$K), 4L)
  expect_equal(r$max_len, 4L)
})

test_that("latnh / morie_latin_hypercube: default args", {
  r <- morie_latin_hypercube()
  expect_type(r, "list")
  expect_true(all(c("sample", "N", "d", "method") %in% names(r)))
  expect_equal(r$N, 100L)
  expect_equal(r$d, 1L)
  expect_equal(dim(r$sample), c(100L, 1L))
  expect_true(all(r$sample >= 0 & r$sample <= 1))
  expect_null(r$estimate)
})

test_that("latnh: multi-dimensional sample with integrand f", {
  r <- morie_latin_hypercube(N = 400, d = 2, f = function(u) u[1] + u[2], seed = 0)
  expect_equal(dim(r$sample), c(400L, 2L))
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se) && r$se > 0)
  expect_lt(abs(r$estimate - 1), 0.1)
})

test_that("latnh: seed makes the sample reproducible", {
  r1 <- morie_latin_hypercube(N = 50, d = 3, seed = 123)
  r2 <- morie_latin_hypercube(N = 50, d = 3, seed = 123)
  expect_identical(r1$sample, r2$sample)
})

test_that("latnh: each dimension has one point per stratum", {
  N <- 80L
  r <- morie_latin_hypercube(N = N, d = 2, seed = 7)
  for (j in 1:2) {
    strata <- floor(r$sample[, j] * N)
    expect_equal(length(unique(strata)), N)
  }
})

test_that("latnh: alias identity", {
  expect_identical(morie_latin_hypercube, latnh)
})
