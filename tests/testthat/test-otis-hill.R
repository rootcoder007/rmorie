# The power-law exponent behind the OTIS concentration analyses.
#
# Anchors outside the module: samples drawn from a discrete power law
# whose exponent is known by construction, so the estimator can be
# asked to recover a number it was not told. That is the only anchor
# that can fail here -- comparing against the closed form would compare
# against the thing being replaced.

test_that("the tail exponent is recovered from data built with it", {
  skip_if_not_installed("rmoriebricklayer")
  k <- 1:20000
  draw <- function(alpha, n, seed) {
    p <- k^(-alpha) / sum(k^(-alpha))
    set.seed(seed)
    sample(k, n, replace = TRUE, prob = p)
  }
  # The exact likelihood recovers the exponent at x_min = 1, which is
  # what both call sites in this package pass.
  for (al in c(2.0, 2.5, 3.0, 3.5)) {
    z <- draw(al, 20000L, 11L)
    expect_equal(rmorie:::.hill_mle(z, 1L), al, tolerance = 0.05)
  }

  # The closed-form continuity correction is an asymptotic
  # approximation IN x_min, not a correction that improves as x_min
  # shrinks. At x_min = 1 it is badly biased downward, and the bias
  # grows with the true exponent -- which is the whole reason for the
  # exact route.
  z <- draw(3.5, 20000L, 11L)
  expect_lt(rmorie:::.hill_mle(z, 1L, approx = TRUE), 2.5)
  expect_equal(rmorie:::.hill_mle(z, 1L), 3.5, tolerance = 0.05)

  # The signature of an asymptotic-in-x_min approximation: it converges
  # on the truth as the threshold rises, while the exact route is right
  # at every threshold.
  big <- draw(2.5, 200000L, 3L)
  approx_err <- vapply(c(1L, 3L, 6L, 12L), function(xm) {
    abs(rmorie:::.hill_mle(big, xm, approx = TRUE) - 2.5)
  }, 0)
  exact_err <- vapply(c(1L, 3L, 6L, 12L), function(xm) {
    abs(rmorie:::.hill_mle(big, xm) - 2.5)
  }, 0)
  expect_true(all(diff(approx_err) < 0))
  expect_gt(approx_err[1L], 0.4)
  expect_lt(approx_err[4L], 0.05)
  expect_true(all(exact_err < 0.02))
})

test_that("the exponent degrades rather than inventing a number", {
  skip_if_not_installed("rmoriebricklayer")
  # fewer than two observations in the tail
  expect_true(is.na(rmorie:::.hill_mle(5, 1L)))
  expect_true(is.na(rmorie:::.hill_mle(c(1, 2), 10L)))
  expect_true(is.na(rmorie:::.hill_mle(numeric(0), 1L)))
  # a zero cannot be logged, so it is not in the tail
  expect_equal(rmorie:::.hill_mle(c(0, 0, 3, 4, 5, 9), 1L),
               rmorie:::.hill_mle(c(3, 4, 5, 9), 1L))
  # nor can an infinity or a missing value be data
  expect_equal(rmorie:::.hill_mle(c(Inf, NA, 3, 4, 5, 9), 1L),
               rmorie:::.hill_mle(c(3, 4, 5, 9), 1L))
  # the exponent stays inside the range a power law can have: below one
  # the distribution cannot be normalised at all
  k <- 1:200
  p <- k^(-1.2) / sum(k^(-1.2))
  set.seed(4)
  heavy <- sample(k, 5000L, replace = TRUE, prob = p)
  a <- rmorie:::.hill_mle(heavy, 1L)
  expect_true(is.finite(a))
  expect_gt(a, 1)
})

test_that("the concentration measures agree with their definitions", {
  # Gini is the mean absolute difference over twice the mean; this is
  # the same function bricklayer verifies against its own Lorenz curve,
  # so the two packages must not drift apart
  for (x in list(c(1, 2, 3, 4, 5), c(1, 1, 1, 8), rep(4, 7),
                 c(2, 2, 3, 100))) {
    want <- mean(abs(outer(x, x, "-"))) / (2 * mean(x))
    expect_equal(rmorie:::.gini_int(x), want, tolerance = 1e-12)
  }
  # the maximum for n units is 1 - 1/n, not one
  expect_equal(rmorie:::.gini_int(c(0, 0, 0, 0, 1)), 1 - 1 / 5)
  expect_equal(rmorie:::.gini_int(rep(2, 10)), 0)
  # a total of zero has no distribution to be unequal about
  expect_true(is.na(rmorie:::.gini_int(c(0, 0, 0))))
  expect_true(is.na(rmorie:::.gini_int(numeric(0))))
  skip_if_not_installed("rmoriebricklayer")
  # and it agrees with bricklayer's, which is the shared backend
  for (x in list(c(1, 2, 3, 4, 5), c(1, 1, 1, 8), c(2, 2, 3, 100))) {
    expect_equal(rmorie:::.gini_int(x), rmoriebricklayer::gini(x),
                 tolerance = 1e-12)
  }
})
