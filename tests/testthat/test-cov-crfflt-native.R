# Coverage tests for R/crfflt_native.R (Christiano and Fitzgerald 2003):
# ideal band-pass weights, the asymmetric, symmetric and one-sided
# filters against their weight formulas, and the frequency response.

cf_x <- cumsum(sin(1:40 / 2) + 0.3 * cos(1:40 * 1.3)) + 0.05 * (1:40)

cf_B <- function(n, pl = 6, pu = 32) {
  a <- 2 * pi / pu
  b <- 2 * pi / pl
  c((b - a) / pi, (sin((1:n) * b) - sin((1:n) * a)) / (pi * (1:n)))
}
cf_tail <- function(B, m) -0.5 * B[1] - sum(B[seq_len(m - 1) + 1])

test_that("ideal band-pass weights", {
  r <- morie_crfflt_ideal_weights(6, 32, 5)
  expect_equal(r$B, cf_B(5), tolerance = 1e-15)
  expect_equal(morie_crfflt_ideal_weights(6, 32, 0)$B, cf_B(1)[1])
  expect_identical(morie_crfflt, morie_crfflt_ideal_weights)
  expect_error(morie_crfflt_ideal_weights(1, 32, 3), "2 <= p_low < p_high")
  expect_error(morie_crfflt_ideal_weights(6, 32, -1), "non-negative")
})

test_that("asymmetric CF weights: eq. (1.2) with tails, summing to zero", {
  r <- morie_crfflt_cf_filter(cf_x)
  T <- 40
  mu <- (cf_x[T] - cf_x[1]) / (T - 1)
  v <- cf_x - (0:(T - 1)) * mu
  B <- cf_B(T)
  ref <- vapply(1:T, function(t) {
    f <- T - t
    b <- t - 1
    w <- numeric(T)
    w[t] <- if (f >= 1 && b >= 1) B[1] else 0.5 * B[1]
    for (j in seq_len(max(f - 1, 0))) w[t + j] <- w[t + j] + B[j + 1]
    for (j in seq_len(max(b - 1, 0))) w[t - j] <- w[t - j] + B[j + 1]
    if (f >= 1) w[T] <- w[T] + cf_tail(B, f)
    if (b >= 1) w[1] <- w[1] + cf_tail(B, b)
    sum(w * v)
  }, 0)
  expect_equal(r$cycle, ref, tolerance = 1e-12)
  expect_lt(r$max_abs_weight_sum, 1e-12)
  expect_equal(r$drift_removed, mu)
  expect_equal(r$trend, v - r$cycle)
  nd <- morie_crfflt_cf_filter(cf_x, drift = FALSE)
  expect_equal(nd$drift_removed, 0)
})

test_that("symmetric and one-sided filters", {
  s <- morie_crfflt_cf_filter(cf_x, method = "symmetric", p = 4)
  B <- cf_B(40)
  wts <- c(cf_tail(B, 4), B[4:2], B[1], B[2:4], cf_tail(B, 4))
  expect_equal(s$weights, wts, tolerance = 1e-15)
  expect_equal(s$weight_sum, 0, tolerance = 1e-14)
  expect_equal(s$cycle, as.numeric(stats::filter(cf_x, wts, sides = 2)), tolerance = 1e-12)
  expect_equal(s$n_missing, 8L)
  expect_equal(morie_crfflt_cf_filter(cf_x, method = "symmetric")$p, 12L)
  expect_equal(morie_crfflt_cf_filter(cf_x, method = "symmetric", p = 1)$weights, c(-0.5, 1, -0.5) * B[1])
  o <- morie_crfflt_cf_filter(cf_x, method = "one_sided")
  mu <- (cf_x[40] - cf_x[1]) / 39
  v <- cf_x - (0:39) * mu
  ref <- vapply(1:40, function(t) {
    back <- t - 1
    if (back < 2) return(NA_real_)
    0.5 * B[1] * v[t] + sum(B[2:back] * v[t - 1:(back - 1)]) + cf_tail(B, back) * v[1]
  }, 0)
  expect_equal(o$cycle, ref, tolerance = 1e-12)
  expect_error(morie_crfflt_cf_filter(cf_x, method = "hp"), "method must be one of")
  expect_error(morie_crfflt_cf_filter(1:4), "at least 5 observations")
  expect_error(morie_crfflt_cf_filter(cf_x, method = "symmetric", p = 20), "p must satisfy")
  expect_error(.drift_adjust(1), "at least 2")
})

test_that("frequency response of a weight vector", {
  w <- c(-0.1, 0.2, 0.5, 0.2, -0.1)
  for (om in c(0, 0.4, 1.3)) {
    expect_equal(morie_crfflt_frequency_response(w, om), Mod(sum(w * exp(-1i * om * (0:4 - 2)))), tolerance = 1e-14)
  }
  expect_equal(morie_crfflt_frequency_response(w, 0), 0.7)
})
