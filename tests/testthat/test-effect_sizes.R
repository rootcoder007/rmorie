# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/effect_sizes.R

set.seed(2026L)
x_a <- rnorm(30, mean = 0)
y_a <- rnorm(30, mean = 0.6)

test_that("effect_size_result builds expected list", {
  # 2s of the suite here, and r-universe's macOS x86_64 builder is
  # about 1.8 times slower. The check there is killed at sixty minutes
  # and the suite alone was twenty-six of them. The heavy files run in
  # our own CI, which sets NOT_CRAN, where the clock is ours.
  skip_heavy()
  r <- effect_size_result("test", 0.5, 0.1, 0.9, 0.2, 50L,
                          extra = list(foo = 1))
  expect_s3_class(r, "morie_effect_size")
  expect_equal(r$estimate, 0.5)
  expect_equal(r$ci_lower, 0.1)
  expect_equal(r$ci_upper, 0.9)
  expect_equal(r$extra$foo, 1)
})

test_that("glass_delta x and y control work", {
  skip_heavy()
  ry <- glass_delta(x_a, y_a, control = "y")
  rx <- glass_delta(x_a, y_a, control = "x")
  expect_s3_class(ry, "morie_effect_size")
  expect_s3_class(rx, "morie_effect_size")
})

test_that("cles returns probability of superiority", {
  skip_heavy()
  r <- cles(x_a, y_a)
  expect_true(r$estimate >= 0 && r$estimate <= 1)
})

test_that("r_effect_size and r_squared work", {
  skip_heavy()
  r <- r_effect_size(x_a, y_a)
  expect_s3_class(r, "morie_effect_size")
  r2 <- r_squared(x_a, y_a)
  expect_s3_class(r2, "morie_effect_size")
  expect_true(r2$estimate >= 0)
})

test_that("r_effect_size handles n <= 3", {
  skip_heavy()
  r <- r_effect_size(c(1, 2, 3), c(2, 3, 4))
  expect_s3_class(r, "morie_effect_size")
})

test_that("partial_eta/omega/epsilon squared all work", {
  skip_heavy()
  expect_equal(partial_eta_squared(0, 0)$estimate, 0)
  expect_true(partial_eta_squared(5, 5)$estimate > 0)
  expect_equal(omega_squared(0, 0, 1, 1)$estimate, 0)
  expect_true(omega_squared(20, 30, 1, 1)$estimate > 0)
  expect_equal(epsilon_squared(0, 0, 1, 1)$estimate, 0)
  expect_true(epsilon_squared(20, 30, 1, 1)$estimate > 0)
})

test_that("contingency-table effect sizes work", {
  skip_heavy()
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
  skip_heavy()
  expect_true(is.finite(rate_ratio(10, 100, 5, 100)$estimate))
  expect_true(is.finite(incidence_rate_difference(10, 100, 5, 100)$estimate))
  # Edge: zero person-time
  r <- rate_ratio(10, 0, 5, 0)
  expect_s3_class(r, "morie_effect_size")
  r2 <- incidence_rate_difference(10, 0, 5, 0)
  expect_s3_class(r2, "morie_effect_size")
})

test_that("association measures work", {
  skip_heavy()
  r <- cohens_w(c(20, 30, 50))
  expect_s3_class(r, "morie_effect_size")
  r2 <- cohens_w(c(20, 30, 50), expected = c(33, 33, 34))
  expect_s3_class(r2, "morie_effect_size")
  expect_equal(cohens_f(0.5)$estimate, 1)
  expect_true(is.finite(cohens_f(0.25)$estimate))
  expect_true(is.infinite(cohens_f(1)$estimate))
  tbl <- matrix(c(20, 10, 5, 25), nrow = 2)
  expect_s3_class(phi_coefficient(tbl), "morie_effect_size")
  expect_error(phi_coefficient(matrix(1:9, nrow = 3)), "2x2")
})

test_that("non-parametric effect sizes work", {
  skip_heavy()
  r <- rank_biserial_correlation(x_a, y_a)
  expect_s3_class(r, "morie_effect_size")
  r2 <- cliffs_delta(x_a, y_a)
  expect_s3_class(r2, "morie_effect_size")
  r3 <- vargha_delaney_a(x_a, y_a)
  expect_s3_class(r3, "morie_effect_size")
})

test_that("standardized_coefficients works with data.frame and matrix", {
  skip_heavy()
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
  skip_heavy()
  expect_true(is.finite(coefficient_of_variation(c(1, 2, 3, 4, 5))$estimate))
  expect_true(is.infinite(coefficient_of_variation(c(0, 0, 0))$estimate))
  r <- variance_ratio(x_a, y_a)
  expect_s3_class(r, "morie_effect_size")
})

test_that("conversion functions are inverse-consistent in sign", {
  skip_heavy()
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
  skip_heavy()
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
  skip_heavy()
  est <- c(0.4, 0.5, 0.6)
  se  <- c(0.1, 0.1, 0.1)
  i2 <- i_squared(est, se)
  expect_true(is.numeric(i2))
  pi <- prediction_interval(est, se)
  expect_length(pi, 2L)
  expect_true(pi[1] < pi[2])
})

test_that("bootstrap_effect_size_ci wraps any function", {
  skip_heavy()
  r <- bootstrap_effect_size_ci(function(a, b) mean(a) - mean(b),
                                 x_a, y_a, n_boot = 50L)
  expect_s3_class(r, "morie_effect_size")
  expect_true(is.finite(r$estimate))
})


test_that("round four: random_effects_meta honours method", {
  est <- c(0.2, 0.5, 0.35, 0.6, 0.1)
  se <- c(0.1, 0.12, 0.08, 0.15, 0.2)
  dl <- random_effects_meta(est, se, method = "DL")
  pm <- random_effects_meta(est, se, method = "PM")
  reml <- random_effects_meta(est, se, method = "REML")
  expect_match(pm$measure, "PM")
  expect_true(is.finite(pm$extra$tau_squared) && is.finite(reml$extra$tau_squared))
  expect_false(isTRUE(all.equal(dl$extra$tau_squared, pm$extra$tau_squared)) &&
                 isTRUE(all.equal(dl$extra$tau_squared, reml$extra$tau_squared)))
  expect_error(random_effects_meta(est, se, method = "HS"), "method")
})

test_that("random_effects_meta PM and REML match metafor::rma", {
  # metafor::rma(y, sei = s, method = m, control = list(tol = 1e-15,
  # threshold = 1e-15)); PM is also checked through its defining equation
  y <- c(0.20, 0.35, 0.15, 0.62, -0.05, 0.41)
  s <- c(0.08, 0.10, 0.07, 0.15, 0.12, 0.09)
  qg <- function(t) {
    w <- 1 / (s^2 + t)
    sum(w * (y - sum(w * y) / sum(w))^2)
  }
  pm <- random_effects_meta(y, s, method = "PM")$extra$tau_squared
  expect_equal(pm, 0.037823443092707444, tolerance = 1e-12)
  expect_equal(qg(pm), length(y) - 1, tolerance = 1e-12)
  expect_equal(random_effects_meta(y, s, method = "REML")$extra$tau_squared,
               0.0322950288465516, tolerance = 1e-12)
  expect_equal(random_effects_meta(y, s, method = "DL")$extra$tau_squared,
               0.0249786151186425, tolerance = 1e-12)
  # homogeneous studies: every estimator truncates at zero
  expect_equal(random_effects_meta(c(0.1, 0.12, 0.11), c(0.2, 0.25, 0.3),
                                   method = "PM")$extra$tau_squared, 0)
  expect_error(random_effects_meta(y, s, method = "SJ"), "method must be")
})

test_that("round four: cramers_v carries a noncentral chi-square interval", {
  r <- cramers_v(matrix(c(10, 30, 20, 15), 2))
  expect_true(is.finite(r$ci_lower) && is.finite(r$ci_upper))
  expect_true(r$ci_lower <= r$estimate && r$estimate <= r$ci_upper)
  expect_equal(r$extra$confidence, 0.95)
})

test_that("hedges_g uses the exact J; cohens_d SE is Hedges-Olkin; CLES counts ties", {
  # effectsize::hedges_g / cohens_d and metafor::escalc("SMD") on these data
  x <- c(5.1, 6.3, 4.8, 7.2, 5.9, 6.6, 5.4)
  y <- c(4.2, 5.0, 3.9, 5.8, 4.4, 4.9, 5.3, 4.1, 4.6)
  d <- cohens_d(x, y)
  expect_equal(d$estimate, 1.6559198522887781, tolerance = 1e-12)
  expect_equal(d$se, sqrt(16 / 63 + d$estimate^2 / 32), tolerance = 1e-14)
  m <- 14
  J <- exp(lgamma(m / 2) - 0.5 * log(m / 2) - lgamma((m - 1) / 2))
  g <- hedges_g(x, y)
  expect_equal(g$estimate, 1.5653207170841281, tolerance = 1e-12)
  expect_equal(g$extra$correction_factor, J, tolerance = 1e-14)
  # pairs: x > y in 5 of 9, ties in 2, counted as 1/2 each
  cl <- cles(c(1, 2, 3), c(2, 2, 0))
  expect_equal(cl$estimate, (5 + 0.5 * 2) / 9)
})

test_that("zero cells get the metafor 1/2 correction; NNT interval spans infinity", {
  # metafor::escalc("OR"/"RR"/"IRR") values
  o <- odds_ratio(7, 0, 4, 10)
  expect_equal(o$estimate, (7.5 * 10.5) / (0.5 * 4.5), tolerance = 1e-14)
  expect_equal(o$se, sqrt(1 / 7.5 + 1 / 0.5 + 1 / 4.5 + 1 / 10.5), tolerance = 1e-14)
  expect_equal(o$ci_lower, exp(log(35) - qnorm(0.975) * 1.565501086168148), tolerance = 1e-12)
  expect_equal(odds_ratio(12, 5, 3, 9)$se, 0.85309892613798188, tolerance = 1e-12)
  expect_equal(risk_ratio(7, 0, 4, 10)$estimate, 3.125, tolerance = 1e-14)
  expect_equal(risk_ratio(7, 0, 4, 10)$se, 0.40483192671637058, tolerance = 1e-12)
  expect_equal(rate_ratio(0, 120, 6, 150)$estimate, 0.096153846153846145, tolerance = 1e-12)
  expect_equal(rate_ratio(0, 120, 6, 150)$se, 1.4675987714106855, tolerance = 1e-12)
  rd <- risk_difference(5, 5, 4, 6)
  nn <- number_needed_to_treat(5, 5, 4, 6)
  expect_true(rd$ci_lower < 0 && rd$ci_upper > 0)
  expect_equal(nn$ci_lower, 1 / rd$ci_upper, tolerance = 1e-14)
  expect_equal(nn$ci_upper, Inf)
  expect_true(nn$extra$ci_spans_zero)
})

test_that("r_squared interval handles an r interval spanning zero", {
  x <- c(1, 2, 3, 4, 5, 6)
  y <- c(2, 1, 4, 3, 6, 2)
  r <- r_effect_size(x, y)
  r2 <- r_squared(x, y)
  expect_true(r$ci_lower < 0 && r$ci_upper > 0)
  expect_equal(r2$ci_lower, 0)
  expect_equal(r2$ci_upper, max(r$ci_lower^2, r$ci_upper^2))
})

test_that("rank-biserial is positive when x exceeds y; adjusted V is Bergsma's", {
  # effectsize::rank_biserial and effectsize::cramers_v(adjust = TRUE)
  x <- c(5.1, 6.3, 4.8, 7.2, 5.9, 6.6, 5.4)
  y <- c(4.2, 5.0, 3.9, 5.8, 4.4, 4.9, 5.3, 4.1, 4.6)
  expect_equal(rank_biserial_correlation(x, y)$estimate, 0.7777777777777778, tolerance = 1e-14)
  expect_equal(rank_biserial_correlation(x, y)$estimate, cliffs_delta(x, y)$estimate, tolerance = 1e-14)
  tb <- matrix(c(12, 5, 7, 3, 9, 6, 8, 4, 10), 3)
  cv <- cramers_v(tb)
  expect_equal(cv$estimate, 0.2556545, tolerance = 1e-6)
  expect_equal(cv$extra$bias_corrected_v, 0.1863203, tolerance = 1e-6)
  n <- sum(tb)
  chi2 <- n * 2 * cv$estimate^2
  expect_equal(cv$extra$bias_corrected_v,
               sqrt((chi2 / n - 4 / (n - 1)) / (3 - 4 / (n - 1) - 1)), tolerance = 1e-14)
})
