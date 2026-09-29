# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/opnclp_native.R (reproducible CLIP scaling laws,
# Cherti et al. 2023): total compute, the log-log least-squares power
# law (checked against lm on the logs and exact on noiseless data),
# the exponent comparison and the symmetric InfoNCE loss.

test_that("total_compute multiplies samples by parameters", {
  r <- total_compute(4e9, 3e8)
  expect_equal(r$compute, 1.2e18)
  expect_equal(r$gmac_scale, 1.2e9)
  expect_equal(c(r$samples_seen, r$params), c(4e9, 3e8))
  expect_error(total_compute(0, 1), "must be positive")
  expect_error(total_compute(1, -1), "must be positive")
})

test_that("fit_power_law recovers beta C^-alpha exactly and matches lm on the logs", {
  x <- c(1, 10, 100, 1000)
  y <- 5 * x^(-0.3)
  f <- fit_power_law(x, y)
  expect_equal(f$alpha, 0.3, tolerance = 1e-12)
  expect_equal(f$beta, 5, tolerance = 1e-12)
  expect_equal(f$slope, -0.3, tolerance = 1e-12)
  expect_equal(f$r_squared, 1, tolerance = 1e-12)
  expect_equal(f$range, c(1, 1000))
  expect_equal(f$n, 4L)
  # noisy data: the fit is the least-squares line on the logs
  yn <- y * c(1.1, 0.9, 1.05, 0.95)
  fn <- fit_power_law(x, yn)
  m <- lm(log(yn) ~ log(x))
  expect_equal(fn$slope, unname(coef(m)[2]), tolerance = 1e-12)
  expect_equal(fn$beta, exp(unname(coef(m)[1])), tolerance = 1e-12)
  expect_equal(fn$r_squared, summary(m)$r.squared, tolerance = 1e-12)
  for (fn2 in list(openclipscaling, open_clip, openclip, morie_opnclp)) {
    expect_equal(fn2(x, y)$alpha, 0.3, tolerance = 1e-12)
  }
  expect_error(fit_power_law(x, y[-1]), "4 x values but 3 y values")
  expect_error(fit_power_law(1, 1), "at least 2 points")
  expect_error(fit_power_law(c(0, 1), c(1, 1)), "strictly positive")
  expect_error(fit_power_law(c(2, 2), c(1, 2)), "no slope is identified")
})

test_that("the prediction reports how far beyond the fitted range it reaches", {
  f <- fit_power_law(c(10, 100), c(1, 0.5))
  inside <- .opnclp_predict(f, 50)
  expect_equal(inside$value, f$beta * 50^(-f$alpha), tolerance = 1e-12)
  expect_equal(inside$extrapolation_decades, 0)
  expect_true(inside$interpolated)
  up <- .opnclp_predict(f, 10000)
  expect_equal(up$extrapolation_decades, 2, tolerance = 1e-12)
  expect_false(up$interpolated)
  expect_equal(.opnclp_predict(f, 1)$extrapolation_decades, 1, tolerance = 1e-12)
  expect_error(.opnclp_predict(f, 0), "compute must be positive")
})

test_that("compare_scaling reports the gap between the two exponents", {
  xa <- c(1, 10, 100)
  c1 <- compare_scaling(xa, 2 * xa^(-0.2), xa, 3 * xa^(-0.35), "openai", "openclip")
  expect_equal(c1$alpha_gap, 0.15, tolerance = 1e-12)
  expect_false(c1$same_law)
  expect_equal(c1$openai$alpha, 0.2, tolerance = 1e-12)
  expect_equal(c1$openclip$beta, 3, tolerance = 1e-12)
  same <- compare_scaling(xa, 2 * xa^(-0.2), xa, 7 * xa^(-0.2))
  expect_true(same$same_law)
  expect_equal(same$estimate, 0, tolerance = 1e-12)
  expect_equal(same$A$alpha, same$B$alpha, tolerance = 1e-12)
})

test_that("infonce is the symmetric cross entropy over the scaled cosine matrix", {
  I <- rbind(c(1, 0), c(0.6, 0.8), c(-1, 0))
  T2 <- rbind(c(0.8, 0.6), c(0, 1), c(-0.6, -0.8))
  r <- infonce(I, T2, temperature = 0.2)
  Iu <- I / sqrt(rowSums(I^2))
  Tu <- T2 / sqrt(rowSums(T2^2))
  S <- Iu %*% t(Tu) / 0.2
  expect_equal(r$logits, S, tolerance = 1e-12)
  li <- mean(log(rowSums(exp(S))) - diag(S))
  lt <- mean(log(colSums(exp(S))) - diag(S))
  expect_equal(r$image_to_text, li, tolerance = 1e-12)
  expect_equal(r$text_to_image, lt, tolerance = 1e-12)
  expect_equal(r$loss, 0.5 * (li + lt), tolerance = 1e-12)
  # perfectly aligned pairs: the loss falls towards zero as the
  # temperature shrinks
  A <- rbind(c(1, 0), c(0, 1))
  expect_lt(infonce(A, A, temperature = 0.01)$loss, 1e-12)
  expect_gt(infonce(A, A, temperature = 10)$loss, 0.6)
  expect_error(infonce(I, T2[1:2, ]), "3 images but 2 texts")
  expect_error(infonce(I, T2, temperature = 0), "temperature must be positive")
  expect_error(infonce(rbind(c(0, 0)), rbind(c(1, 0))), "zero embedding")
})
