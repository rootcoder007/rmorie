# SPDX-License-Identifier: AGPL-3.0-or-later
# Book-certified tests for the Schabenberger & Gotway spatial family.
# Every assertion is an identity the book states, and they are the SAME
# identities the Python arm asserts, so this file is also the R leg of
# three-way parity.
#
# Schabenberger, O. & Gotway, C. A. (2005).

h <- c(0, 0.25, 0.5, 1, 2, 5)

test_that("the nugget is a discontinuity at the origin (Sec 4.3.6)", {
  for (f in list(spexp, spgaus, spsph)) {
    expect_identical(f(h, nugget = 0.4, sill = 1, range = 1)$gamma[1], 0)
    expect_equal(f(1e-12, nugget = 0.4, sill = 1, range = 1)$gamma, 0.4,
                 tolerance = 1e-6)
  }
})

test_that("alpha is the PRACTICAL range: R(alpha) = exp(-3) (eqs 4.10-4.11)", {
  expect_equal(spexp(2.5, 0, 3, 2.5)$gamma / 3, 1 - exp(-3), tolerance = 1e-12)
  expect_equal(spgaus(2.5, 0, 3, 2.5)$gamma / 3, 1 - exp(-3), tolerance = 1e-12)
})

test_that("the spherical model has a TRUE range (eq 4.13)", {
  g <- spsph(c(1.7, 17), 0, 2, 1.7)$gamma
  expect_equal(g, c(2, 2), tolerance = 1e-12)
})

test_that("spherical matches the printed polynomial (eq 4.15)", {
  hh <- c(0.5, 1, 1.9); u <- hh / 2
  expect_equal(spsph(hh, 0, 1.5, 2)$gamma, 1.5 * (1.5 * u - 0.5 * u^3),
               tolerance = 1e-12)
})

test_that("the power model is linear at lambda = 1 and rejects lambda >= 2", {
  expect_equal(sppow(c(1, 2, 3), 0, 2, 1)$gamma, c(2, 4, 6), tolerance = 1e-12)
  expect_error(sppow(h, 0, 1, 2), "intrinsic hypothesis")
})

test_that("nesting a white-noise component reproduces the nugget form (eq 4.23)", {
  a <- spnest(h, list(list(model = "nugget", sill = 0.3),
                      list(model = "exponential", sill = 1, range = 1)))$gamma
  expect_equal(a, spexp(h, 0.3, 1, 1)$gamma, tolerance = 1e-12)
})

test_that("gamma(h) = C(0) - C(h) under second-order stationarity", {
  got <- spssoc(function(x) exp(-3 * x), h)
  expect_equal(got$gamma, spexp(h, 0, 1, 1)$gamma, tolerance = 1e-12)
  expect_equal(got$sill, 1, tolerance = 1e-12)
})

test_that("spnug reports both sides of the jump", {
  r <- spnug(h, 0.25, 1, 1)
  expect_identical(r$gamma_at_zero, 0)
  expect_equal(r$limit_at_zero_plus, 0.25)
  expect_equal(r$total_sill, 1.25)
})

test_that("Matern variance at the origin is sigma2 (p. 143)", {
  for (nu in c(0.25, 0.5, 1, 2, 5)) {
    expect_equal(spmatr(h, 2, nu, 1.7)$covariance[1], 2, tolerance = 1e-12)
  }
})

test_that("Matern at nu = 1/2 is the exponential model (eq 4.11)", {
  expect_equal(spmatr(h, 2, 0.5, 1.7)$covariance, 2 * exp(-1.7 * h),
               tolerance = 1e-12)
})

test_that("Matern at nu = 1 is Whittle's model (eq 4.12)", {
  w <- ifelse(h > 0, 2 * 1.7 * h * besselK(ifelse(h > 0, 1.7 * h, 1), 1), 2)
  expect_equal(spmatr(h, 2, 1, 1.7)$covariance, w, tolerance = 1e-12)
})

test_that("Matern smoothness increases with nu (Figure 4.2)", {
  g <- vapply(c(0.25, 0.5, 1, 3),
              function(nu) spmatr(0.05, 1, nu, 1)$semivariogram, numeric(1))
  expect_true(all(diff(g) < 0))
})

test_that("K_{1/2}(t) has the closed form sqrt(pi/2t) exp(-t) (p. 143)", {
  tt <- c(0.1, 1, 5, 20)
  expect_equal(spbesf(tt, 0.5)$value, sqrt(pi / (2 * tt)) * exp(-tt),
               tolerance = 1e-12)
  expect_error(spbesf(0, 0.5), "positive")
})

test_that("empirical semivariogram recovers the variance of white noise", {
  set.seed(1)
  co <- matrix(runif(1200), 600, 2)
  z <- rnorm(600, 0, 2)
  g <- spsemv(co, z, n_bins = 6)$gamma
  expect_equal(mean(g, na.rm = TRUE), 4, tolerance = 0.15 * 4)
})

test_that("anisotropy correction with the identity map is a no-op", {
  set.seed(2)
  co <- matrix(runif(240), 120, 2)
  z <- rnorm(120)
  r <- spanis(co, z, diag(2), n_bins = 6)
  expect_equal(r$coords_corrected, co, tolerance = 1e-12)
  ok <- !is.na(r$gamma)
  expect_equal(r$gamma[ok], r$gamma_raw[ok], tolerance = 1e-12)
  expect_error(spanis(co, z, matrix(c(1, 2, 2, 4), 2, 2)), "singular")
})

test_that("K(h) = pi h^2 under CSR, and k_csr is exactly that (p. 101)", {
  set.seed(11)
  pts <- matrix(runif(1600), 800, 2) * 10
  reg <- c(0, 0, 10, 10)
  r <- seq(0.1, 1.5, length.out = 8)
  k <- spkfun(pts, r = r, region = reg)
  expect_equal(k$k_csr, pi * r^2, tolerance = 1e-12)
  expect_equal(k$lambda_est, 8, tolerance = 1e-12)     # N(A)/nu(A), eq (3.8)
  expect_lt(max(abs(k$k - k$k_csr) / k$k_csr), 0.15)
})

test_that("L = sqrt(K/pi) and L(h) - h is the CSR reference at zero (p. 103)", {
  set.seed(11)
  pts <- matrix(runif(1600), 800, 2) * 10
  reg <- c(0, 0, 10, 10)
  r <- seq(0.1, 1.5, length.out = 8)
  l <- splfun(pts, r = r, region = reg)
  expect_equal(l$l, sqrt(pmax(l$k, 0) / pi), tolerance = 1e-12)
  expect_lt(max(abs(l$l_minus_r)), 0.05)
})

test_that("clustering raises K and G but DEPRESSES F", {
  set.seed(2)
  par <- matrix(runif(80), 40, 2) * 10
  cl <- pmin(pmax(par[rep(1:40, each = 20), ] +
                    matrix(rnorm(1600, 0, 0.15), 800, 2), 0), 10)
  reg <- c(0, 0, 10, 10)
  r <- seq(0.1, 1.5, length.out = 8)
  k <- spkfun(cl, r = r, region = reg)
  g <- spgfun(cl, r = r, region = reg)
  f <- spffun(cl, reg, r, n_grid = 40)
  expect_true(all(k$k > k$k_csr))
  expect_true(all(g$g >= g$g_csr))
  expect_true(all(f$f <= f$f_csr))
})

test_that("G is the empirical nearest-neighbour CDF (p. 97)", {
  set.seed(3)
  pts <- matrix(runif(400), 200, 2) * 10
  r <- spgfun(pts, region = c(0, 0, 10, 10))
  nn <- r$nn_distances
  expect_equal(r$g, vapply(r$r, function(y) sum(nn <= y) / length(nn),
                           numeric(1)), tolerance = 1e-12)
  expect_true(all(diff(r$g) >= 0))
})

test_that("input validation", {
  expect_error(spexp(-1, 0, 1, 1), "non-negative")
  expect_error(spexp(1, 0, 1, 0), "`range` must be")
  expect_error(spgfun(matrix(c(1, 2), 1, 2)), "at least two events")
  expect_error(spkfun(matrix(runif(40), 20, 2), region = c(0, 0, 0, 5)),
               "positive area")
})
