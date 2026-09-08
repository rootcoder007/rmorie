# SPDX-License-Identifier: AGPL-3.0-or-later
#
# spiso  -- the isotropy condition (Schabenberger & Gotway 2005, Sec 2.2)
# sprfss -- the stationarity hierarchy (Schabenberger & Gotway 2005, Sec 2.2)
#
# These mirror tests/fn/test_spiso.py and tests/fn/test_sprfss.py on the
# Python side; the numeric fixtures are shared so the two arms are compared
# against the same values, not merely against themselves.

lattice <- function(n, step) {
  g <- (0:(n - 1)) / step
  as.matrix(expand.grid(x = g, y = g))
}

smooth_field <- function(coords) sin(coords[, 1] * 0.8) + cos(coords[, 2] * 0.8)

test_that("spiso passes an isotropic field", {
  coords <- lattice(24, 2.4)
  res <- spiso(coords, smooth_field(coords))
  expect_true(res$is_isotropic)
  expect_lt(res$relative_spread, res$tol)
})

test_that("spiso detects geometric anisotropy", {
  coords <- lattice(24, 2.4)
  z <- smooth_field(coords)
  stretched <- coords
  stretched[, 1] <- stretched[, 1] * 3
  res <- spiso(stretched, z)
  expect_false(res$is_isotropic)
  expect_gt(res$relative_spread, spiso(coords, z)$relative_spread)
})

test_that("spiso does not depend on the order the points are listed in", {
  # A lag and its negation are the same direction. Before the lags were
  # oriented into one half-space this moved the answer in the third decimal:
  # atan2 of a reversed pair folds back one ulp away, and on a regular lattice
  # thousands of pairs sit exactly on a sector boundary.
  coords <- lattice(24, 2.4)
  z <- smooth_field(coords)
  o <- order(coords[, 2], coords[, 1])
  expect_equal(spiso(coords, z)$relative_spread,
               spiso(coords[o, ], z[o])$relative_spread,
               tolerance = 1e-12)
})

test_that("spiso does not let rounding split the diagonal lags", {
  # Every 45-degree lag here lies exactly on the pi/4 sector boundary.
  # Rescaling changes the floating-point offsets but not the geometry.
  coords <- lattice(24, 2.4)
  z <- smooth_field(coords)
  expect_equal(spiso(coords, z)$relative_spread,
               spiso(coords * 4, z)$relative_spread,
               tolerance = 1e-9)
})

test_that("spiso agrees with the Python arm on a fixed lattice", {
  coords <- lattice(24, 2.4)
  expect_equal(spiso(coords, smooth_field(coords))$relative_spread,
               0.173760543267416, tolerance = 1e-12)
})

test_that("spiso rejects bad input", {
  coords <- lattice(4, 2.4)
  expect_error(spiso(coords, rep(1, 3)))
  expect_error(spiso(matrix(1, 16, 3), rep(1, 16)))
  expect_error(spiso(coords, smooth_field(coords), n_dir = 1))
})

# ---------------------------------------------------------------- sprfss ---

# 40x40 puts 100 points in each of the 16 blocks. Smaller lattices make the
# block variances themselves noisy enough to trip the variance-drift screen on
# genuinely stationary noise -- sampling variability in the fixture, not a
# property of the field.
stat_coords <- function() lattice(40, 2.0)

test_that("sprfss passes a stationary field at every level", {
  coords <- stat_coords()
  set.seed(11)
  res <- sprfss(coords, rnorm(nrow(coords)))
  expect_true(res$mean_stationary)
  expect_true(res$variance_stationary)
  expect_true(res$second_order_plausible)
  expect_true(res$intrinsic_plausible)
})

test_that("sprfss rejects a linear trend as non-intrinsic", {
  # A linear trend leaves the increment VARIANCE flat, so a variance-drift
  # screen passes it for the wrong reason. The book's condition is on the
  # increment MEAN, which a trend violates outright.
  coords <- stat_coords()
  set.seed(11)
  base <- rnorm(nrow(coords))
  res <- sprfss(coords, base + 0.8 * coords[, 1])
  expect_false(res$intrinsic_plausible)
  expect_gt(res$increment_bias, sprfss(coords, base)$increment_bias)
})

test_that("sprfss needs oriented lags to see a trend at all", {
  # Binning on lag DISTANCE alone averages the +x and -x pairs together, so a
  # trend cancels itself exactly and passes.
  coords <- stat_coords()
  set.seed(11)
  base <- rnorm(nrow(coords))
  flat <- sprfss(coords, base)$increment_bias
  trend <- sprfss(coords, base + 0.8 * coords[, 1])$increment_bias
  expect_gt(trend, 5 * flat)
})

test_that("sprfss detects heteroscedasticity as variance drift", {
  coords <- stat_coords()
  set.seed(11)
  z <- rnorm(nrow(coords))
  z[coords[, 1] > mean(coords[, 1])] <- z[coords[, 1] > mean(coords[, 1])] * 6
  res <- sprfss(coords, z)
  expect_false(res$variance_stationary)
  expect_false(res$second_order_plausible)
})

test_that("sprfss keeps the hierarchy nested", {
  coords <- stat_coords()
  for (s in 1:5) {
    set.seed(s)
    res <- sprfss(coords, rnorm(nrow(coords)))
    if (isTRUE(res$strict_if_gaussian)) expect_true(res$second_order_plausible)
  }
})

test_that("sprfss agrees with the Python arm on a fixed lattice", {
  coords <- lattice(24, 2.4)
  res <- sprfss(coords, smooth_field(coords))
  expect_equal(res$increment_bias, 0.250275519620559, tolerance = 1e-12)
  expect_equal(res$mean_drift, 3.04400474071028, tolerance = 1e-12)
  expect_equal(res$variance_drift, 1.57483658320564, tolerance = 1e-12)
})

test_that("sprfss rejects bad input", {
  expect_error(sprfss(lattice(4, 2.4), rep(1, 3)))
})
