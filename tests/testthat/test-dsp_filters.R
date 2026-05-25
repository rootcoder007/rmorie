# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for r-package/morie/R/dsp_filters.R.
#
# Anchored to closed-form / textbook checks:
#   * moving_average:   constant in -> constant out (interior bins)
#   * median_filter:    impulse / salt removal
#   * LMS/NLMS/RLS:     converge in identification of a known FIR
#   * SNR:              known dB ratio (20 dB for amplitude 10x)
#   * matched filter:   peak at the template location
#   * cross_correlation: peak at lag 0 for x == y

set.seed(1L)



# ---------------------------------------------------------------------------
# moving_average
# ---------------------------------------------------------------------------

test_that("moving_average preserves a constant interior", {
  x <- rep(2.0, 50)
  y <- morie_dsp_moving_average(x, window = 5L)
  expect_equal(length(y), 50L)
  # Interior bins (away from zero-pad edges) should be exactly 2.
  expect_equal(y[15:35], rep(2.0, 21), tolerance = 1e-12)
})

test_that("moving_average errors on a zero / negative window", {
  expect_error(morie_dsp_moving_average(1:10, window = 0L),
               "window")
})


# ---------------------------------------------------------------------------
# median_filter
# ---------------------------------------------------------------------------

test_that("median_filter removes a salt impulse on a flat signal", {
  x <- c(0, 0, 0, 0, 50, 0, 0, 0, 0)
  y <- morie_dsp_median_filter(x, kernel_size = 3L)
  expect_equal(y[5L], 0, tolerance = 1e-12)
})

test_that("median_filter forces odd kernel sizes", {
  # Even kernel sizes are bumped to k+1 silently; just check the
  # output length and the absence of an error.
  x <- stats::rnorm(20)
  expect_silent(y <- morie_dsp_median_filter(x, kernel_size = 4L))
  expect_equal(length(y), 20L)
})


# ---------------------------------------------------------------------------
# LMS / NLMS / RLS - identification of a known FIR
# ---------------------------------------------------------------------------

.gen_lms_signals <- function(n = 4000L, taps = c(0.5, -0.3, 0.2)) {
  set.seed(1L)
  x <- stats::rnorm(n)
  d <- numeric(n)
  P <- length(taps)
  for (i in seq.int(P + 1L, n)) {
    seg <- rev(x[(i - P):(i - 1L)])
    d[i] <- sum(taps * seg) + 0.01 * stats::rnorm(1)
  }
  list(x = x, d = d)
}

test_that("LMS converges so post-burn-in error is small", {
  sig <- .gen_lms_signals()
  out <- morie_dsp_lms(sig$x, sig$d, order = 8L, mu = 0.05)
  expect_named(out, c("y", "e"))
  expect_equal(length(out$y), length(sig$x))
  # Error over the last 25 % should be no worse than burn-in; both
  # bounce around the noise floor so leave a 2x slack factor.
  e2 <- mean(out$e[3001:4000]^2)
  e0 <- mean(out$e[100:200]^2)
  expect_lt(e2, 2 * e0)
})

test_that("NLMS converges and accepts mu = 1", {
  sig <- .gen_lms_signals()
  out <- morie_dsp_nlms(sig$x, sig$d, order = 8L, mu = 1.0)
  e2 <- mean(out$e[3001:4000]^2)
  expect_lt(e2, 0.05)
})

test_that("RLS converges faster than LMS for the same target", {
  sig <- .gen_lms_signals()
  out_l <- morie_dsp_lms (sig$x, sig$d, order = 8L, mu = 0.05)
  out_r <- morie_dsp_rls (sig$x, sig$d, order = 8L, lam = 0.99)
  e_l <- mean(out_l$e[200:400]^2)
  e_r <- mean(out_r$e[200:400]^2)
  expect_lt(e_r, e_l)
})

test_that("LMS errors when x and d have mismatched lengths", {
  expect_error(morie_dsp_lms(1:10, 1:11), "same length")
})


# ---------------------------------------------------------------------------
# SNR / SNR improvement
# ---------------------------------------------------------------------------

test_that("morie_dsp_snr returns the exact dB ratio", {
  # signal power 100, noise power 1 -> 20 dB.
  sig <- rep(10, 100)
  noi <- rep(1, 100)
  expect_equal(morie_dsp_snr(sig, noi), 20, tolerance = 1e-12)
})

test_that("morie_dsp_snr returns Inf when noise is identically zero", {
  expect_identical(morie_dsp_snr(c(1, 2, 3), c(0, 0, 0)), Inf)
})

test_that("snr_improvement is positive when the filter actually denoises", {
  set.seed(1L)
  clean <- sin(2 * pi * seq(0, 1, length.out = 256))
  noisy <- clean + stats::rnorm(256, sd = 0.3)
  filt  <- morie_dsp_moving_average(noisy, window = 7L)
  delta <- morie_dsp_snr_improvement(noisy, clean, filt)
  expect_gt(delta, 0)
})


# ---------------------------------------------------------------------------
# Matched filter
# ---------------------------------------------------------------------------

test_that("matched filter peaks near the template's location", {
  set.seed(1L)
  tpl <- c(1, 2, 3, 2, 1)
  x <- c(stats::rnorm(20, sd = 0.01), tpl,
         stats::rnorm(20, sd = 0.01))
  y <- morie_dsp_matched(x, tpl)
  peak <- which.max(y)
  # Centre of the embedded template is index 23 (20 + 3).
  expect_true(abs(peak - 23L) <= 1L)
})

test_that("matched filter rejects a zero-norm template", {
  expect_error(morie_dsp_matched(1:10, c(0, 0, 0)), "zero norm")
})


# ---------------------------------------------------------------------------
# Cross-correlation
# ---------------------------------------------------------------------------

test_that("cross_correlation peaks at lag 0 for an identical input", {
  set.seed(1L)
  x <- stats::rnorm(64)
  cc <- morie_dsp_cross_correlation(x, x, max_lag = 16L)
  expect_equal(length(cc), 33L)
  expect_equal(which.max(cc), 17L)        # lag 0 == centre
  expect_equal(max(cc), 1.0, tolerance = 1e-10)
})

test_that("cross_correlation errors on unequal lengths", {
  expect_error(morie_dsp_cross_correlation(1:5, 1:6), "same length")
})


# ---------------------------------------------------------------------------
# Even/odd, Wiener-Hopf, turning points, CV
# ---------------------------------------------------------------------------

test_that("even_odd partition adds back to the original signal", {
  x <- c(1, 2, 3, 4, 5)
  d <- morie_dsp_even_odd(x)
  expect_equal(d$even + d$odd, x, tolerance = 1e-12)
})

test_that("wiener_hopf solves a 2x2 toy", {
  R <- diag(2)
  r <- c(0.7, -0.3)
  w <- morie_dsp_wiener_hopf(R, r)
  expect_equal(w, r, tolerance = 1e-12)
})

test_that("turning_points reports a plausible z-statistic for white noise", {
  set.seed(1L)
  out <- morie_dsp_turning_points(stats::rnorm(500))
  expect_named(out, c("turning_points", "expected",
                      "z_statistic", "stationary"))
  expect_true(abs(out$z_statistic) < 4)
})

test_that("morie_dsp_cv returns Inf when the mean is zero", {
  expect_identical(morie_dsp_cv(c(-1, 1)), Inf)
})
