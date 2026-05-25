# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/effect_sizes.R

set.seed(2026L)
x_a <- rnorm(30, mean = 0)
y_a <- rnorm(30, mean = 0.6)

test_that("effect_size_result builds expected list", {
  r <- effect_size_result("test", 0.5, 0.1, 0.9, 0.2, 50L,
                          extra = list(foo = 1))
  expect_s3_class(r, "morie_effect_size")
  expect_equal(r$estimate, 0.5)
  expect_equal(r$ci_lower, 0.1)
  expect_equal(r$ci_upper, 0.9)
  expect_equal(r$extra$foo, 1)
})

test_that("cohens_d returns morie_effect_size with finite estimate", {
  r <- cohens_d(x_a, y_a)
  expect_s3_class(r, "morie_effect_size")
  expect_true(is.finite(r$estimate))
  expect_true(is.finite(r$se))
})

test_that("cohens_d errors on tiny groups", {
  expect_error(cohens_d(c(1), c(2)), "at least 2")
})

test_that("hedges_g returns bias-corrected d", {
  r <- hedges_g(x_a, y_a)
  expect_s3_class(r, "morie_effect_size")
  expect_true(is.finite(r$estimate))
  expect_true(r$extra$correction_factor <= 1)
})

test_that("glass_delta x and y control work", {
  ry <- glass_delta(x_a, y_a, control = "y")
  rx <- glass_delta(x_a, y_a, control = "x")
  expect_s3_class(ry, "morie_effect_size")
  expect_s3_class(rx, "morie_effect_size")
})

test_that("cles returns probability of superiority", {
  r <- cles(x_a, y_a)
  expect_true(r$estimate >= 0 && r$estimate <= 1)
})

test_that("r_effect_size and r_squared work", {
  r <- r_effect_size(x_a, y_a)
  expect_s3_class(r, "morie_effect_size")
  r2 <- r_squared(x_a, y_a)
  expect_s3_class(r2, "morie_effect_size")
  expect_true(r2$estimate >= 0)
})

test_that("r_effect_size handles n <= 3", {
  r <- r_effect_size(c(1, 2, 3), c(2, 3, 4))
  expect_s3_class(r, "morie_effect_size")
})

test_that("eta/partial_eta/omega/epsilon squared all work", {
  expect_equal(eta_squared(0, 0)$estimate, 0)
  expect_true(eta_squared(10, 20)$estimate > 0)
  expect_equal(partial_eta_squared(0, 0)$estimate, 0)
  expect_true(partial_eta_squared(5, 5)$estimate > 0)
  expect_equal(omega_squared(0, 0, 1, 1)$estimate, 0)
  expect_true(omega_squared(20, 30, 1, 1)$estimate > 0)
  expect_equal(epsilon_squared(0, 0, 1, 1)$estimate, 0)
  expect_true(epsilon_squared(20, 30, 1, 1)$estimate > 0)
})

test_that("contingency-table effect sizes work", {
  expect_true(is.finite(odds_ratio(10, 5, 5, 10)$estimate))
  expect_true(is.finite(risk_ratio(10, 5, 5, 10)$estimate))
  expect_true(is.finite(risk_difference(10, 5, 5, 10)$estimate))
  expect_true(is.finite(number_needed_to_treat(10, 5, 5, 10)$estimate))
  expect_true(is.finite(number_needed_to_harm(10, 5, 5, 10)$estimate))
  # Edge: zero in denominator
  r <- odds_ratio(0, 5, 5, 10)
  expect_s3_class(r, "morie_effect_size")
  r2 <- risk_ratio(0, 5, 5, 10)
  expect_s3_class(r2, "morie_effect_size")
  r3 <- risk_difference(0, 0, 0, 0)
  expect_s3_class(r3, "morie_effect_size")
  r4 <- number_needed_to_treat(5, 5, 5, 5)
  expect_s3_class(r4, "morie_effect_size")
})

test_that("rate_ratio and incidence_rate_difference", {
  expect_true(is.finite(rate_ratio(10, 100, 5, 100)$estimate))
  expect_true(is.finite(incidence_rate_difference(10, 100, 5, 100)$estimate))
  # Edge: zero person-time
  r <- rate_ratio(10, 0, 5, 0)
  expect_s3_class(r, "morie_effect_size")
  r2 <- incidence_rate_difference(10, 0, 5, 0)
  expect_s3_class(r2, "morie_effect_size")
})

test_that("association measures work", {
  r <- cohens_w(c(20, 30, 50))
  expect_s3_class(r, "morie_effect_size")
  r2 <- cohens_w(c(20, 30, 50), expected = c(33, 33, 34))
  expect_s3_class(r2, "morie_effect_size")
  expect_equal(cohens_f(0.5)$estimate, 1)
  expect_true(is.finite(cohens_f(0.25)$estimate))
  expect_true(is.infinite(cohens_f(1)$estimate))
  tbl <- matrix(c(20, 10, 5, 25), nrow = 2)
  expect_s3_class(cramers_v(tbl), "morie_effect_size")
  expect_s3_class(phi_coefficient(tbl), "morie_effect_size")
  expect_error(phi_coefficient(matrix(1:9, nrow = 3)), "2x2")
})

test_that("non-parametric effect sizes work", {
  r <- rank_biserial_correlation(x_a, y_a)
  expect_s3_class(r, "morie_effect_size")
  r2 <- cliffs_delta(x_a, y_a)
  expect_s3_class(r2, "morie_effect_size")
  r3 <- vargha_delaney_a(x_a, y_a)
  expect_s3_class(r3, "morie_effect_size")
})

test_that("standardized_coefficients works with data.frame and matrix", {
  X <- data.frame(a = rnorm(30), b = rnorm(30))
  y <- 1 + 0.5 * X$a - 0.3 * X$b + rnorm(30)
  r <- standardized_coefficients(X, y)
  expect_s3_class(r, "data.frame")
  expect_named(r, c("variable", "beta", "se", "t", "p_value"))
  X2 <- as.matrix(X)
  r2 <- standardized_coefficients(X2, y)
  expect_s3_class(r2, "data.frame")
})

test_that("coefficient_of_variation and variance_ratio", {
  expect_true(is.finite(coefficient_of_variation(c(1, 2, 3, 4, 5))$estimate))
  expect_true(is.infinite(coefficient_of_variation(c(0, 0, 0))$estimate))
  r <- variance_ratio(x_a, y_a)
  expect_s3_class(r, "morie_effect_size")
})

test_that("conversion functions are inverse-consistent in sign", {
  d <- 0.5
  expect_true(d_to_r(d) > 0)
  expect_true(d_to_r(d, n1 = 30, n2 = 30) > 0)
  expect_true(r_to_d(0.3) > 0)
  expect_true(r_to_d(0.99) > 0)
  expect_true(r_to_d(1) == Inf)
  expect_true(or_to_d(2) > 0)
  expect_equal(or_to_d(0), 0)
  expect_true(d_to_or(0.5) > 1)
  expect_true(or_to_r(2) > 0)
  expect_true(r_to_or(0.3) > 1)
  expect_true(is.finite(d_to_nnt(0.5)))
  expect_true(d_to_nnt(0) == Inf)
})

test_that("fixed_effects_meta and random_effects_meta", {
  est <- c(0.4, 0.5, 0.6)
  se  <- c(0.1, 0.1, 0.1)
  fe <- fixed_effects_meta(est, se)
  expect_s3_class(fe, "morie_effect_size")
  expect_true(is.numeric(fe$extra$Q))
  re <- random_effects_meta(est, se)
  expect_s3_class(re, "morie_effect_size")
  expect_true(is.numeric(re$extra$tau_squared))
  expect_true(is.numeric(re$extra$I_squared))
})

test_that("i_squared and prediction_interval", {
  est <- c(0.4, 0.5, 0.6)
  se  <- c(0.1, 0.1, 0.1)
  i2 <- i_squared(est, se)
  expect_true(is.numeric(i2))
  pi <- prediction_interval(est, se)
  expect_length(pi, 2L)
  expect_true(pi[1] < pi[2])
})

test_that("bootstrap_effect_size_ci wraps any function", {
  r <- bootstrap_effect_size_ci(function(a, b) mean(a) - mean(b),
                                 x_a, y_a, n_boot = 50L)
  expect_s3_class(r, "morie_effect_size")
  expect_true(is.finite(r$estimate))
})
