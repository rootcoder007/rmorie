# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the transformation-model mirrors.
# Anchors printed from the Python modules at full double precision --
# testthat tolerances are RELATIVE, so a rounded anchor silently
# weakens the test.
#
# The fixture is a linear congruential generator written out
# explicitly rather than either language's RNG: it produces the same
# bits in both, which is what makes an exact anchor possible. An
# earlier low-discrepancy fixture was rejected because its two
# columns were dependent enough to make the index nearly degenerate,
# which inverted the sign of T_n -- a fixture defect, not an
# estimator defect, but one worth not shipping.

hrz6_fixture <- function() {
  m <- 1000L
  u <- numeric(m)
  s <- 12345
  for (k in seq_len(m)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[k] <- (s + 0.5) / 4294967296
  }
  z <- stats::qnorm(u)
  n <- 200L
  x <- cbind(z[1:n], z[(n + 1):(2 * n)])
  y <- exp(x %*% c(1, -0.5) + 0.6 * z[(2 * n + 1):(3 * n)])
  list(x = x, y = as.numeric(y), z = z, n = n, beta = c(1, -0.5))
}

test_that("morie_transform_T_F matches morie.fn.hrzhot", {
  f <- hrz6_fixture()
  o <- morie_transform_T_F(f$x, f$y, c(0.5, 0.5), f$beta)
  expect_equal(o$T_hat[1], -6.141353387948381, tolerance = 1e-8)
  expect_equal(o$T_hat[21], 1.5238723054085925, tolerance = 1e-8)
  expect_equal(o$T_hat[41], 2.6657434861129774, tolerance = 1e-8)
  expect_equal(o$y0, 1.0002185460762656, tolerance = 1e-10)
  expect_equal(mean(o$F_hat, na.rm = TRUE), 0.3623206710989425,
               tolerance = 1e-8)
  expect_false(o$F_is_empirical_cdf)
  expect_equal(o$beta[1], 1)
  # the design has T(y) = log y exactly, up to the location
  # normalisation T(y0) = 0
  truth <- log(o$y_grid) - log(o$y0)
  expect_gt(stats::cor(o$T_hat, truth), 0.9)
})

test_that("morie_transform_T_F imposes the scale normalisation", {
  f <- hrz6_fixture()
  a <- morie_transform_T_F(f$x, f$y, 0.6, f$beta)$beta
  b <- morie_transform_T_F(f$x, f$y, 0.6, 2 * f$beta)$beta
  expect_equal(a, b)                       # scale-invariant
  expect_error(morie_transform_T_F(f$x, f$y, 0.5, c(0, 1)), "first component")
  expect_error(morie_transform_T_F(f$x, f$y, -1, f$beta), "positive")
  expect_error(morie_transform_T_F(f$x, f$y, 0.5, 1), "1 entries for 2")
  expect_error(morie_transform_T_F(f$x[1:10, ], f$y[1:10], 0.5, f$beta),
               "at least 20")
})

test_that("morie_transform_asymptotics matches Python and reads HT9", {
  f <- hrz6_fixture()
  a <- morie_transform_asymptotics(f$x, f$y, c(200^(-1 / 3), 200^(-1 / 10)))
  expect_equal(a$rate, 0.07071067811865475, tolerance = 1e-12)
  expect_equal(a$h_ny_reference, 0.17099759466766973, tolerance = 1e-12)
  expect_equal(a$h_nz_reference, 0.5887040186524746, tolerance = 1e-12)
  expect_true(a$bandwidths_consistent_with_HT9)
  expect_true(a$limit_is_process)
  expect_equal(a$Kz_order_required, 6L)
  # h_nz shrinks far more slowly than h_ny -- that asymmetry IS the
  # content of HT9, and one bandwidth for both breaks the theorem
  expect_gt(a$h_nz_reference, a$h_ny_reference)
  expect_false(morie_transform_asymptotics(
    f$x, f$y, 200^(-1 / 3))$bandwidths_consistent_with_HT9)
  expect_error(morie_transform_asymptotics(f$x, f$y, 0), "positive")
})

test_that("morie_chen_transform matches morie.fn.hrzchet", {
  f <- hrz6_fixture()
  c6 <- morie_chen_transform(f$x, f$y, beta_hat = f$beta)
  expect_equal(c6$T_hat[1], -1.7228918439054448, tolerance = 1e-9)
  expect_equal(c6$T_hat[11], 0.861445921952722, tolerance = 1e-9)
  expect_equal(c6$T_hat[21], 1.5314594168048394, tolerance = 1e-9)
  expect_equal(c6$objective_max[11], 0.09761306532663316, tolerance = 1e-10)
  expect_false(c6$uses_kernel)
  # both estimators are n^{-1/2}; neither dominates
  expect_false(c6$faster_than_horowitz)
  expect_equal(c6$rate_exponent, -0.5)
  expect_equal(c6$rate_exponent,
               morie_transform_asymptotics(f$x, f$y, c(0.3, 0.6))$rate_exponent)
})

test_that("morie_baseline_hazard matches morie.fn.hrzlam", {
  f <- hrz6_fixture()
  n <- f$n
  tt <- abs(f$z[(3 * n + 1):(4 * n)]) + 0.05
  ev <- ifelse(seq_len(n) %% 4 == 0, 0, 1)
  l <- morie_baseline_hazard(tt, f$x, ev, f$beta)
  expect_equal(l$lambda0_hat[11], 0.3601636726602739, tolerance = 1e-9)
  expect_equal(l$bandwidth, 1.2537268147849707, tolerance = 1e-10)
  expect_equal(l$n_events, 150L)
  expect_equal(l$cumhaz[length(l$cumhaz)], 2.026822357137152, tolerance = 1e-9)
  # the cumulative hazard is a step function -- strictly increasing at
  # the jumps, which is exactly why it cannot be differentiated
  expect_true(all(diff(l$cumhaz) > 0))
  expect_false(l$root_n_attainable)
  expect_equal(l$rate_exponent, -0.4)
  expect_error(morie_baseline_hazard(tt, f$x, rep(0, n), f$beta), "no events")
  expect_error(morie_baseline_hazard(-tt, f$x, ev, f$beta), "non-negative")
})

test_that("morie_transform_prediction matches Python", {
  yg <- seq(0.5, 8, length.out = 60)
  ug <- seq(-4, 4, length.out = 81)
  p <- morie_transform_prediction(c(0.3, 0.2), 2, log(yg), stats::plogis(ug),
                                  c(1, -0.5), y_grid = yg, u_grid = ug)
  expect_equal(p$probability, 0.6207514382462203, tolerance = 1e-10)
  expect_equal(p$quantile, 1.2627118644067796, tolerance = 1e-10)
  expect_equal(p$u_gamma, 0, tolerance = 1e-12)
  expect_equal(p$index, 0.19999999999999998, tolerance = 1e-12)
  # the section's actual result is the negative one
  expect_false(p$mean_root_n_estimable)
  expect_true(p$quantile_root_n_estimable)
  # P(Y<=y|x) = F[T(y) - x'b] with T = log and F logistic
  expect_equal(p$probability,
               stats::plogis(log(2) - 0.2), tolerance = 5e-3)
})

test_that("morie_transform_prediction validates its inputs", {
  yg <- seq(0.5, 8, length.out = 40)
  ug <- seq(-4, 4, length.out = 41)
  b <- c(1, -0.5)
  expect_error(morie_transform_prediction(c(0.3, 0.2), 2, log(yg),
                                          seq(0, 1, length.out = 41), b,
                                          gamma = 0, y_grid = yg, u_grid = ug),
               "must lie in")
  expect_error(morie_transform_prediction(c(0.3, 0.2), 2, log(yg),
                                          seq(0, 2, length.out = 41), b,
                                          y_grid = yg, u_grid = ug),
               "\\[0, 1\\]")
  expect_error(morie_transform_prediction(c(0.3, 0.2), 2, -log(yg),
                                          seq(0, 1, length.out = 41), b,
                                          y_grid = yg, u_grid = ug),
               "non-decreasing")
  expect_error(morie_transform_prediction(c(0.3, 0.2), 2, log(yg),
                                          seq(0, 1, length.out = 41), b,
                                          u_grid = ug),
               "y_grid is required")
})
