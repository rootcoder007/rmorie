# Coverage tests for R/bnskt2_native.R (Card, Lee, Pei and Weber 2015):
# one-sided local polynomial slopes against weighted lm, sharp and fuzzy
# RKD, and the density and covariate kink tests.

bk_v <- seq(-1, 1, by = 0.05)
bk_y <- ifelse(bk_v < 0, 1 + 2 * bk_v, 1 + 0.5 * bk_v) + 0.3 * bk_v^2 + 0.02 * sin(7 * bk_v)

test_that("one-sided local polynomial slope equals weighted lm with an intercept", {
  r <- local_polynomial_slope(bk_v, bk_y, 0, 0.6)
  sel <- bk_v >= 0 & bk_v <= 0.6
  d <- bk_v[sel]
  w <- 1 - d / 0.6
  f <- lm(bk_y[sel] ~ d + I(d^2), weights = w, subset = w > 0)
  expect_equal(r$slope, unname(coef(f)[2]), tolerance = 1e-9)
  expect_equal(r$n, sum(w > 0))
  l <- local_polynomial_slope(bk_v, bk_y, 0, 0.6, order = 1, side = "left", kernel = "uniform")
  sl <- bk_v <= 0 & bk_v >= -0.6
  expect_equal(l$slope, unname(coef(lm(bk_y[sl] ~ bk_v[sl]))[2]), tolerance = 1e-9)
  expect_error(local_polynomial_slope(bk_v, bk_y, 0, 0.6, side = "up"), "left or right")
  expect_error(local_polynomial_slope(bk_v, bk_y, 0, 0.6, kernel = "epan"), "triangular or uniform")
  expect_error(local_polynomial_slope(bk_v, bk_y, 0, 0.6, order = 0), "at least 1")
  expect_error(local_polynomial_slope(bk_v, bk_y, 0, 0), "bandwidth must be positive")
  expect_error(local_polynomial_slope(bk_v, bk_y, 0, 0.06, order = 3), "too few observations on the right")
})

test_that("sharp and fuzzy regression kink estimates", {
  lin <- ifelse(bk_v < 0, 1 + 2 * bk_v, 1 + 0.5 * bk_v)
  s <- rkd_estimate(bk_v, lin, 0, 0.5, order = 1, policy_slope_change = -0.75)
  # exact piecewise-linear data with a non-zero level: the slopes are 0.5 and 2
  expect_equal(c(s$slope_right, s$slope_left), c(0.5, 2), tolerance = 1e-10)
  expect_equal(s$tau, -1.5 / -0.75, tolerance = 1e-10)
  B <- ifelse(bk_v < 0, 3 + bk_v, 3 + 0.25 * bk_v)
  fz <- rkd_estimate(bk_v, lin, 0, 0.5, order = 1, B = B, fuzzy = TRUE)
  expect_equal(fz$policy_kink, -0.75, tolerance = 1e-10)
  expect_equal(fz$estimate, 2, tolerance = 1e-10)
  q <- rkd_estimate(bk_v, bk_y, 0, 0.6, policy_slope_change = 1)
  expect_equal(q$outcome_kink, local_polynomial_slope(bk_v, bk_y, 0, 0.6)$slope - local_polynomial_slope(bk_v, bk_y, 0, 0.6, side = "left")$slope)
  expect_identical(morie_bnskt2, rkd_estimate)
  expect_identical(kinktreatmentbound, rkd_estimate)
  expect_identical(bound_kink_te, rkd_estimate)
  expect_identical(boundkinkte, rkd_estimate)
  expect_error(rkd_estimate(bk_v, bk_y[-1], 0, 0.5), "agree in length")
  expect_error(rkd_estimate(bk_v, bk_y, 0, 0.5, fuzzy = TRUE), "needs the observed treatment")
  expect_error(rkd_estimate(bk_v, bk_y, 0, 0.5, B = 1:3, fuzzy = TRUE), "B has 3 entries")
  expect_error(rkd_estimate(bk_v, bk_y, 0, 0.5), "needs policy_slope_change")
  expect_error(rkd_estimate(bk_v, bk_y, 0, 0.5, policy_slope_change = 0), "no kink to identify")
})

test_that("density and covariate kink tests", {
  V <- qnorm(seq(0.005, 0.995, length.out = 400))
  dk <- density_kink_test(V, 0, 1, n_bins = 10)
  inside <- V[abs(V) <= 1]
  edges <- -1 + 0.2 * (0:10)
  ctr <- edges[-1] - 0.1
  dens <- vapply(1:10, function(b) mean(inside >= edges[b] & inside < edges[b + 1]), 0)
  r <- coef(lm(dens[ctr >= 0] ~ ctr[ctr >= 0]))[2]
  l <- coef(lm(dens[ctr <= 0] ~ ctr[ctr <= 0]))[2]
  expect_equal(dk$slope_change, unname(r - l), tolerance = 1e-9)
  expect_equal(dk$relative, dk$slope_change / mean(dens), tolerance = 1e-9)
  expect_equal(dk$n_inside, length(inside))
  expect_error(density_kink_test(V, 0, 1, n_bins = 100), "too few observations")
  Z <- 2 + 0.4 * bk_v
  ck <- covariate_kink_test(bk_v, Z, 0, 0.5, order = 1)
  expect_equal(ck$slope_change, 0, tolerance = 1e-10)
  expect_equal(ck$slope_right, 0.4, tolerance = 1e-10)
  expect_match(.bnskt2_cheatsheet(), "KINK")
})
