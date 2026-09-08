# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the Horowitz max-score extensions.
# Anchors printed from the Python modules at full double precision --
# testthat tolerances are RELATIVE, so a rounded anchor silently
# weakens the test.
#
# These estimators optimise STEP functions, so both languages use an
# exhaustive grid scan at d = 2 rather than a simplex. That is what
# makes an exact cross-language anchor meaningful here at all; with
# Nelder-Mead on either side the two would only agree by luck.

fixture <- function() {
  t <- seq(0, 8, length.out = 300)
  x <- cbind(sin(t), cos(1.7 * t))
  y <- as.numeric(x %*% c(1, -0.8) + 0.3 * sin(5 * t) > 0)
  list(x = x, y = y)
}

test_that("morie_choice_based_max_score matches morie.fn.hrzcbsm", {
  f <- fixture()
  o <- morie_choice_based_max_score(f$x, f$y, 0.4)
  expect_equal(o$beta[2], -1.1400000000000006, tolerance = 1e-12)
  expect_equal(o$score, 0.25631500169410293, tolerance = 1e-10)
  expect_equal(o$n1, 181L)
  expect_equal(o$n0, 119L)
  expect_equal(o$bandwidth, 0.3195771718380609, tolerance = 1e-12)
  expect_true(o$standard_errors_valid)

  o2 <- morie_choice_based_max_score(f$x, f$y, c(0.6, 0.4), smoothed = FALSE)
  expect_equal(o2$beta[2], -0.9199999999999999, tolerance = 1e-12)
  expect_equal(o2$score, 0.3378429824968662, tolerance = 1e-10)
  # the unsmoothed form has the slower n^{-1/3} rate and no valid SEs
  expect_equal(o2$rate_exponent, -1 / 3)
  expect_false(o2$standard_errors_valid)

  expect_error(morie_choice_based_max_score(f$x, f$y, 1.5), "must lie in")
  expect_error(morie_choice_based_max_score(f$x, rep(1, 300), 0.4), "both response")
})

test_that("morie_choice_based_shares follows the square-root rule", {
  s <- morie_choice_based_shares(0.1)
  expect_equal(s$q1, 0.25, tolerance = 1e-12)
  expect_equal(s$q0, 0.75, tolerance = 1e-12)
  expect_equal(s$factor, 1.6, tolerance = 1e-12)
  # genuinely better than a random sample AND than an even split,
  # and equal to neither
  expect_lt(s$factor, s$factor_at_random_sample)
  expect_lt(s$factor, 0.1 / 0.5 + 0.9 / 0.5)
  expect_false(isTRUE(all.equal(s$q1, 0.1)))
  expect_false(isTRUE(all.equal(s$q1, 0.5)))
  expect_equal(morie_choice_based_shares(0.5)$q1, 0.5, tolerance = 1e-12)
  expect_error(morie_choice_based_shares(0), "must lie in")
})

test_that("morie_panel_max_score matches morie.fn.hrzpanms", {
  f <- fixture()
  xp <- array(0, dim = c(150, 2, 2))
  xp[, 1, ] <- f$x[1:150, ]
  xp[, 2, ] <- f$x[151:300, ]
  yp <- cbind(f$y[1:150], f$y[151:300])
  p <- morie_panel_max_score(xp, yp, 2)
  expect_equal(p$beta[2], -1.5399999999999991, tolerance = 1e-12)
  expect_equal(p$score, 0.13163204058377684, tolerance = 1e-10)
  expect_equal(p$n_pairs, 150L)
  expect_equal(p$n_discordant_pairs, 59L)
  expect_equal(p$unidentified_columns, integer(0))
  expect_equal(p$bandwidth, 0.3670977715849853, tolerance = 1e-12)
  expect_false(p$intercept_identified)
  expect_error(morie_panel_max_score(xp, yp, 1), "at least 2 periods")
})

test_that("morie_panel_max_score names the columns differencing kills", {
  set.seed(4)
  n <- 120
  xp <- array(stats::rnorm(n * 3 * 3), dim = c(n, 3, 3))
  xp[, , 3] <- stats::rnorm(n)  # constant within individual
  yp <- matrix(as.numeric(stats::rnorm(n * 3) > 0), nrow = n)
  p <- morie_panel_max_score(xp, yp, 3, n_restarts = 2L)
  expect_equal(p$unidentified_columns, 3L)
  expect_equal(p$n_pairs, n * 3L)  # C(3, 2) pairs per individual
})

test_that("morie_ordered_max_score matches Python and MINIMISES", {
  f <- fixture()
  a <- c(-0.5, 0, 0.6)
  yo <- findInterval(as.numeric(f$x %*% c(1, -0.8)), a)
  q <- morie_ordered_max_score(f$x, yo, thresholds = a)
  expect_equal(q$beta[2], -0.7999999999999989, tolerance = 1e-12)
  expect_equal(q$objective, 0)
  expect_equal(q$M, 4L)
  expect_equal(q$sense, "minimised")
  expect_false(q$scale_normalisation_required)

  qs <- morie_ordered_max_score(f$x, yo, thresholds = a, smoothed = TRUE)
  expect_equal(qs$beta[2], -0.7899999999999991, tolerance = 1e-12)
  expect_equal(qs$objective, 1.3218467842851467, tolerance = 1e-10)
  expect_equal(qs$sense, "maximised")

  # the book prints "maximize" over (4.43); maximising the absolute
  # deviation does NOT recover beta -- it leaves the grid at its edge
  W <- 1 + yo
  s_of <- function(b2) {
    v <- as.numeric(f$x %*% c(1, b2))
    mean(abs(W - (1 + rowSums(outer(v, a, ">")))))
  }
  grid <- seq(-3, 3, length.out = 601)
  vals <- vapply(grid, s_of, numeric(1))
  expect_lt(abs(grid[which.min(vals)] - (-0.8)), 0.1)
  # maximising drives the estimate to the far edge of the search
  # region instead -- 2.9 here, nowhere near the true -0.8
  expect_gt(abs(grid[which.max(vals)]), 2.5)

  expect_error(morie_ordered_max_score(f$x, as.integer(f$y)), "at least 3 categories")
  expect_error(morie_ordered_max_score(f$x, yo, thresholds = c(0, 0, 1)),
               "strictly increasing")
})

test_that("morie_ordered_max_score estimates unknown thresholds", {
  f <- fixture()
  yo <- findInterval(as.numeric(f$x %*% c(1, -0.8)), c(0, 0.6))
  q <- morie_ordered_max_score(f$x, yo, n_restarts = 4L)
  expect_true(q$thresholds_estimated)
  expect_equal(q$thresholds[1], 0)          # Lee's normalisation
  expect_true(all(diff(q$thresholds) > 0))  # stays ordered
})

test_that("morie_sms_rate matches Python and is derived, not asserted", {
  z <- morie_sms_rate(10000, 3)
  expect_equal(z$rate, 0.019306977288832506, tolerance = 1e-12)
  expect_equal(z$exponent, -0.42857142857142855, tolerance = 1e-12)
  expect_equal(z$ratio_to_unsmoothed, 0.4159562163071847, tolerance = 1e-10)
  expect_equal(z$bandwidth_exponent, -0.14285714285714285, tolerance = 1e-12)
  # smoothing beats n^{-1/3} but never attains n^{-1/2}
  expect_lt(z$rate, z$unsmoothed_rate)
  for (s in c(2L, 3L, 5L, 50L)) expect_gt(morie_sms_rate(1000, s)$exponent, -0.5)
  expect_equal(morie_sms_rate(1000, 1000)$exponent, -0.5, tolerance = 1e-3)
  expect_false(z$attains_root_n)
  expect_error(morie_sms_rate(1000, 1), "at least 2")
})
