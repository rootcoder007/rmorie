# Anchors for N-HiTS (Challu et al.).
#
# A forecaster can be held to series whose continuation is not in doubt: a
# linear ramp continues linearly, a constant stays constant. That is what
# caught the defect fixed alongside these tests. Each block fits a
# polynomial in normalised window time and evaluated it for the forecast
# at u = 1 + (j+1)/n_knots, putting the last knot a full WINDOW ahead
# rather than H steps ahead. On 1..60 with a lookback of 48 it forecast 75
# where the answer is 61, and the error grew with the lookback instead of
# with the horizon.

test_that("ridge least squares is the closed form", {
  set.seed(1); n <- 50; p <- 3
  X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))
  y <- as.numeric(X %*% c(1, 2, -1)) + rnorm(n, 0, 0.3)
  for (ridge in c(1e-8, 0.1, 1)) {
    expect_equal(as.numeric(unlist(.nhits_lstsq(X, y, ridge = ridge))),
                 as.numeric(solve(crossprod(X) + ridge * diag(p),
                                  crossprod(X, y))),
                 tolerance = 1e-8)
  }
  # a heavier ridge shrinks the fit toward zero
  small <- as.numeric(unlist(.nhits_lstsq(X, y, ridge = 1e-8)))
  big <- as.numeric(unlist(.nhits_lstsq(X, y, ridge = 1e4)))
  expect_lt(sum(big^2), sum(small^2))
})

test_that("max pooling takes the maximum of each window", {
  x <- c(1, 5, 2, 8, 3, 9, 4, 7)
  expect_equal(as.numeric(unlist(.nhits_max_pool(x, 2L))), c(5, 8, 9, 7))
  expect_equal(as.numeric(unlist(.nhits_max_pool(x, 4L))), c(8, 9))
  # a kernel of one changes nothing, which is the identity pooling the
  # blocks use when they want full resolution
  expect_equal(as.numeric(unlist(.nhits_max_pool(x, 1L))), x)
  # a stride narrower than the kernel overlaps the windows
  expect_equal(as.numeric(unlist(.nhits_max_pool(x, 2L, stride = 1L))),
               c(5, 5, 8, 8, 9, 9, 7))
  # pooling never returns less than the maximum of the whole series
  expect_equal(max(as.numeric(unlist(.nhits_max_pool(x, 8L)))), max(x))
})

test_that("the knot count follows the expressiveness ratio", {
  expect_equal(as.numeric(unlist(.nhits_expressiveness_knots(12, 1.0))), 12)
  expect_equal(as.numeric(unlist(.nhits_expressiveness_knots(12, 0.5))), 6)
  expect_equal(as.numeric(unlist(.nhits_expressiveness_knots(12, 0.25))), 3)
  # never fewer than the two the interpolator needs
  expect_equal(as.numeric(unlist(.nhits_expressiveness_knots(10, 0.1))), 2)
  expect_equal(as.numeric(unlist(.nhits_expressiveness_knots(4, 0.01))), 2)
  # a coarser ratio is never more knots than a finer one
  ks <- vapply(c(0.1, 0.25, 0.5, 1.0), function(r)
    as.numeric(unlist(.nhits_expressiveness_knots(24, r))), numeric(1))
  expect_true(all(diff(ks) >= 0))
})

test_that("interpolation is linear through its knots", {
  iv <- as.numeric(unlist(.nhits_linear_interpolate(c(0, 3, 6), 5L)))
  expect_equal(iv, c(0, 1.5, 3, 4.5, 6), tolerance = 1e-12)
  # the endpoints are the outer knots
  expect_equal(iv[1], 0, tolerance = 1e-12)
  expect_equal(iv[5], 6, tolerance = 1e-12)
  # equal knots interpolate to a constant
  expect_equal(as.numeric(unlist(.nhits_linear_interpolate(c(2, 2), 4L))),
               rep(2, 4), tolerance = 1e-12)
  # and the second difference of a linear interpolation is zero
  d2 <- diff(diff(as.numeric(unlist(.nhits_linear_interpolate(c(1, 9), 9L)))))
  expect_equal(d2, rep(0, length(d2)), tolerance = 1e-10)
  # fewer than two knots has no line to draw
  expect_error(.nhits_linear_interpolate(4, 4L), "at least 2 knots")
})

test_that("a block continues a ramp rather than overshooting it", {
  # the regression, at block level: the window is 1..24, so the six steps
  # that follow are 25..30. Evaluating the basis a full window ahead gave
  # 27.8, 31.7, ... 47 instead.
  b <- .nhits_nhits_block(as.numeric(1:24), horizon = 6L, kernel = 1L,
                          ratio = 1.0)
  expect_equal(as.numeric(unlist(b$forecast)), as.numeric(25:30),
               tolerance = 1e-4)
  # the backcast reproduces the window it was fitted to
  expect_equal(as.numeric(unlist(b$backcast)), as.numeric(1:24),
               tolerance = 1e-3)
  expect_length(unlist(b$forecast), 6L)
  # the knots are the basis evaluated at the forecast positions
  expect_length(unlist(b$knots), 6L)
})

test_that("a block needs enough pooled points for its polynomial", {
  expect_error(.nhits_nhits_block(as.numeric(1:4), horizon = 2L, kernel = 4L,
                                  degree = 2L),
               "too few for degree")
})

test_that("the forecast continues a linear trend exactly", {
  # the end-to-end regression. Before the fix the error was 12, 17 and 35
  # for these three horizons, growing with the lookback.
  y <- as.numeric(1:60)
  for (H in c(3L, 6L, 12L)) {
    f <- morie_nhits(y, horizon = H)
    fc <- as.numeric(unlist(f$forecast))
    expect_length(fc, H)
    expect_equal(fc, 60 + seq_len(H), tolerance = 1e-4)
  }
})

test_that("a constant series forecasts the constant", {
  fc <- as.numeric(unlist(morie_nhits(rep(7, 40), horizon = 5L)$forecast))
  expect_equal(fc, rep(7, 5), tolerance = 1e-6)
  # and a shifted constant shifts with it
  fc2 <- as.numeric(unlist(morie_nhits(rep(-3.5, 40), horizon = 4L)$forecast))
  expect_equal(fc2, rep(-3.5, 4), tolerance = 1e-6)
})

test_that("a ramp of any slope is continued at that slope", {
  for (slope in c(0.5, 2, -1.5)) {
    y <- slope * seq_len(50)
    fc <- as.numeric(unlist(morie_nhits(y, horizon = 5L)$forecast))
    expect_equal(fc, slope * (50 + seq_len(5)), tolerance = 1e-3)
    # the forecast advances by the slope each step
    expect_equal(diff(fc), rep(slope, 4), tolerance = 1e-4)
  }
})

test_that("the forecast length follows the horizon, not the lookback", {
  y <- as.numeric(1:80)
  for (H in c(1L, 4L, 10L, 20L)) {
    f <- morie_nhits(y, horizon = H, lookback = 40L)
    expect_length(unlist(f$forecast), H)
    # and it still lands on the truth whatever the horizon
    expect_equal(as.numeric(unlist(f$forecast)), 80 + seq_len(H),
                 tolerance = 1e-3)
  }
})

test_that("the vector coercion accepts what it documents", {
  expect_equal(.nhits_vec(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(.nhits_vec(list(1, 2, 3)), c(1, 2, 3))
})
