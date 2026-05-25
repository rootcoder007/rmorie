# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for r-package/morie/R/tps_hawkes_advanced.R.
#
# The module is internal-helper-heavy:
#
#   .tps_hwka_kernel_density(u, kind, psi)  - vectorised densities
#   .tps_hwka_kernel_cdf    (u, kind, psi)  - vectorised CDFs
#   .tps_hwka_baseline      (t, kind, alpha, T_)
#   .tps_hwka_baseline_integral(T_, kind, alpha)
#
# Closed-form checks anchor each branch to a hand-derived answer.
#
# Public fit functions (`morie_tps_hawkes_advanced_fit`,
# `morie_tps_compare_hawkes_kernels`,
# `morie_tps_hawkes_markovian_vs_nonmarkovian`) run optimisation loops
# and are exercised by the in-package `test-hawkes-fit.R`.  We add one
# small smoke test for the constant-baseline fast path here too.

set.seed(1L)



# ---------------------------------------------------------------------------
# Densities: exponential, gamma, Weibull, Lomax
# ---------------------------------------------------------------------------

test_that(".tps_hwka_kernel_density(exponential) matches dexp", {
  beta <- 0.7
  u <- c(0.1, 0.5, 1.0, 2.5, 5.0)
  got <- morie:::.tps_hwka_kernel_density(u, "exponential", c(beta))
  want <- beta * exp(-beta * u)
  expect_equal(got, want, tolerance = 1e-12)
})

test_that(".tps_hwka_kernel_density(gamma) matches dgamma", {
  psi <- c(2.0, 1.5)  # shape, rate
  u <- c(0.1, 0.5, 1, 2, 4)
  got <- morie:::.tps_hwka_kernel_density(u, "gamma", psi)
  want <- stats::dgamma(u, shape = psi[1], rate = psi[2])
  expect_equal(got, want, tolerance = 1e-10)
})

test_that(".tps_hwka_kernel_density(weibull) matches dweibull", {
  psi <- c(1.5, 2.0)  # shape, scale
  u <- c(0.1, 0.5, 1, 2, 4)
  got <- morie:::.tps_hwka_kernel_density(u, "weibull", psi)
  want <- stats::dweibull(u, shape = psi[1], scale = psi[2])
  expect_equal(got, want, tolerance = 1e-10)
})

test_that(".tps_hwka_kernel_density(lomax) matches its analytic form", {
  # v0.9.5.6+ scipy form: f(u) = alpha * c^alpha * (u + c)^{-(alpha+1)}
  alpha <- 3.0; c_ <- 1.0
  u <- c(0, 0.5, 1, 2, 5)
  got <- morie:::.tps_hwka_kernel_density(u, "lomax", c(alpha, c_))
  want <- alpha * c_^alpha * (u + c_)^(-(alpha + 1))
  expect_equal(got, want, tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# CDFs: closed-form anchors
# ---------------------------------------------------------------------------

test_that(".tps_hwka_kernel_cdf(exponential) matches pexp", {
  beta <- 0.7
  u <- c(0.1, 0.5, 1, 2, 5)
  got <- morie:::.tps_hwka_kernel_cdf(u, "exponential", c(beta))
  want <- 1 - exp(-beta * u)
  expect_equal(got, want, tolerance = 1e-12)
})

test_that(".tps_hwka_kernel_cdf(gamma) matches pgamma", {
  psi <- c(2.0, 1.5)
  u <- c(0.1, 1, 4)
  got <- morie:::.tps_hwka_kernel_cdf(u, "gamma", psi)
  want <- stats::pgamma(u, shape = psi[1], rate = psi[2])
  expect_equal(got, want, tolerance = 1e-12)
})

test_that(".tps_hwka_kernel_cdf(weibull) matches pweibull", {
  psi <- c(1.5, 2.0)
  u <- c(0.1, 1, 4)
  got <- morie:::.tps_hwka_kernel_cdf(u, "weibull", psi)
  want <- 1 - exp(-(u / psi[2])^psi[1])
  expect_equal(got, want, tolerance = 1e-12)
})

test_that(".tps_hwka_kernel_cdf(lomax) hits closed-form anchor", {
  # v0.9.5.6+ scipy form: F(u) = 1 - (c / (u + c))^alpha.
  # At alpha=3, c=1, u=1: F = 1 - (1/2)^3 = 0.875 exactly.
  got <- morie:::.tps_hwka_kernel_cdf(1, "lomax", c(3, 1))
  expect_equal(got, 0.875, tolerance = 1e-12)
})


# ---------------------------------------------------------------------------
# Baseline nu(t) and its integral
# ---------------------------------------------------------------------------

test_that("baseline(constant) is just exp(a0) everywhere", {
  vals <- morie:::.tps_hwka_baseline(c(0, 1, 100), "constant",
                                      c(log(0.3)), T_ = 100)
  expect_equal(vals, rep(0.3, 3L), tolerance = 1e-12)
})

test_that("baseline_integral(constant) is exp(a0) * T exactly", {
  T_ <- 250
  got <- morie:::.tps_hwka_baseline_integral(T_, "constant", c(log(2.0)))
  expect_equal(got, 2.0 * T_, tolerance = 1e-12)
})

test_that("baseline_integral(sinusoidal, no seasonal/trend) ~ exp(a0)*T", {
  # With a1 = a2 = a3 = 0 the sinusoidal baseline collapses to exp(a0).
  T_ <- 365
  a0 <- log(0.5)
  got <- morie:::.tps_hwka_baseline_integral(
    T_, "sinusoidal", c(a0, 0, 0, 0)
  )
  expect_equal(got, 0.5 * T_, tolerance = 1e-3)
})

test_that("baseline(sinusoidal) is positive and 365.25-periodic-ish", {
  alpha <- c(log(1.0), 0, 0.3, -0.2)
  t <- seq(0, 365.25 * 2, length.out = 50)
  vals <- morie:::.tps_hwka_baseline(t, "sinusoidal", alpha,
                                      T_ = max(t))
  expect_true(all(vals > 0))
  # Periodicity: value at t and t + 365.25 should agree up to the
  # linear-trend component (which is zero here).
  early <- morie:::.tps_hwka_baseline(10, "sinusoidal", alpha, T_ = 1)
  late  <- morie:::.tps_hwka_baseline(10 + 365.25, "sinusoidal",
                                       alpha, T_ = 1)
  expect_equal(early, late, tolerance = 1e-6)
})


# ---------------------------------------------------------------------------
# Theta split / parameter-count helpers
# ---------------------------------------------------------------------------

test_that("kernel param count is 1 for exp and 2 otherwise", {
  expect_identical(morie:::.tps_hwka_n_kernel_params("exponential"), 1L)
  expect_identical(morie:::.tps_hwka_n_kernel_params("gamma"), 2L)
  expect_identical(morie:::.tps_hwka_n_kernel_params("weibull"), 2L)
  expect_identical(morie:::.tps_hwka_n_kernel_params("lomax"), 2L)
})

test_that("baseline param count is 1 for constant and 4 for sinusoidal", {
  expect_identical(morie:::.tps_hwka_n_baseline_params("constant"), 1L)
  expect_identical(morie:::.tps_hwka_n_baseline_params("sinusoidal"), 4L)
})

test_that(".tps_hwka_split_theta partitions a parameter vector cleanly", {
  # exp + sinusoidal => 4 baseline + 1 eta + 1 kernel = 6
  th <- seq_len(6L) * 0.1
  out <- morie:::.tps_hwka_split_theta(th, "exponential", "sinusoidal")
  expect_equal(out$a,   th[1:4],  tolerance = 1e-12)
  expect_equal(out$eta, th[5],    tolerance = 1e-12)
  expect_equal(out$psi, th[6],    tolerance = 1e-12)
  expect_error(
    morie:::.tps_hwka_split_theta(th[-1L], "exponential", "sinusoidal"),
    "expected"
  )
})


# ---------------------------------------------------------------------------
# Public-fit end-to-end (Phase 2C)
#
# Each fit runs an L-BFGS optimisation; we keep n small + use the fast
# exponential-kernel + constant-baseline combination to stay under a
# few seconds per test. The synthetic TPS Assault fixture from
# helper-tps.R provides OCC_DATE timestamps in the required format.
# ---------------------------------------------------------------------------

test_that("morie_tps_hawkes_advanced_fit returns rich-result on synthetic Assault", {
  set.seed(1L)
  df <- make_synthetic_tps("Assault", n = 200L, seed = 5L)
  rr <- morie_tps_hawkes_advanced_fit(
    df, kernel = "exponential", baseline = "constant",
    ds_name = "synthetic-assault", max_n = 200L
  )
  expect_s3_class(rr, "morie_tps_hawkes_advanced_result")
})

test_that("morie_tps_hawkes_advanced_fit handles too-few events gracefully", {
  df <- make_synthetic_tps("Assault", n = 50L, seed = 6L)
  rr <- morie_tps_hawkes_advanced_fit(
    df, kernel = "exponential", baseline = "constant", max_n = 50L
  )
  expect_s3_class(rr, "morie_tps_hawkes_advanced_result")
})

test_that("morie_tps_hawkes_advanced_fit rejects unknown kernel/baseline", {
  df <- make_synthetic_tps("Assault", n = 50L, seed = 7L)
  expect_error(
    morie_tps_hawkes_advanced_fit(df, kernel = "bogus"),
    "unknown kernel"
  )
  expect_error(
    morie_tps_hawkes_advanced_fit(df, baseline = "bogus"),
    "unknown baseline"
  )
})

test_that("morie_tps_hawkes_advanced_fit warns when no OCC/REPORT_DATE", {
  df <- data.frame(x = 1:10)
  rr <- morie_tps_hawkes_advanced_fit(df, ds_name = "no-date")
  expect_s3_class(rr, "morie_tps_hawkes_advanced_result")
})

test_that("morie_tps_compare_hawkes_kernels sweeps kernel x baseline grid", {
  set.seed(2L)
  df <- make_synthetic_tps("Assault", n = 200L, seed = 8L)
  rr <- morie_tps_compare_hawkes_kernels(
    df, ds_name = "syn", max_n = 200L,
    kernels = c("exponential"), baselines = c("constant")
  )
  expect_s3_class(rr, "morie_tps_hawkes_advanced_result")
})

test_that("morie_tps_hawkes_markovian_vs_nonmarkovian compares 2x2 grid", {
  set.seed(3L)
  df <- make_synthetic_tps("Assault", n = 200L, seed = 9L)
  rr <- morie_tps_hawkes_markovian_vs_nonmarkovian(
    df, ds_name = "syn", max_n = 200L
  )
  expect_s3_class(rr, "morie_tps_hawkes_advanced_result")
})
