# Anchors for the spatio-temporal primitives of Schabenberger & Gotway
# (2005), chapter 9.
#
# The file header explains that the Bessel functions are written out as
# quadrature rather than calling base R's besselJ/besselK, so that this
# arm and the Python arm run the same steps rather than merely agreeing
# to plotting accuracy. That design choice is exactly what makes base R
# an independent reference here, and nothing was checking it: the module
# sat at 30.8% with no test naming any of its functions.

test_that("Gauss-Legendre integrates exactly to degree 2n-1 and no further", {
  for (n in c(2L, 3L, 5L, 8L)) {
    gl <- .schab_gauss_legendre(n)
    x <- gl[[1]]; w <- gl[[2]]
    expect_length(x, n)
    expect_length(w, n)
    # the weights of an n-point rule on [-1, 1] sum to the interval length
    expect_equal(sum(w), 2, tolerance = 1e-12)
    # nodes lie inside the interval and are symmetric about zero
    expect_true(all(abs(x) < 1))
    expect_equal(sort(x), sort(-x), tolerance = 1e-12)
    # exact for every monomial up to 2n - 1
    for (deg in 0:(2 * n - 1)) {
      want <- if (deg %% 2 == 1) 0 else 2 / (deg + 1)
      expect_equal(sum(w * x^deg), want, tolerance = 1e-10)
    }
    # and not exact at 2n, which is what makes the rule an n-point one
    deg <- 2 * n
    expect_false(isTRUE(all.equal(sum(w * x^deg), 2 / (deg + 1),
                                  tolerance = 1e-10)))
  }
})

test_that("the quadrature J0 agrees with base R's besselJ", {
  for (x in c(0, 0.25, 0.5, 1, 2, 2.404825557695773, 3, 5, 8, 10)) {
    expect_equal(.schab_bessel_j0(x), besselJ(x, 0), tolerance = 1e-12)
  }
  # J0(0) is exactly one
  expect_equal(.schab_bessel_j0(0), 1, tolerance = 1e-14)
  # and it is very nearly zero at its first root
  expect_lt(abs(.schab_bessel_j0(2.404825557695773)), 1e-4)
  # J0 is even
  expect_equal(.schab_bessel_j0(3), .schab_bessel_j0(-3), tolerance = 1e-12)
})

test_that("the quadrature K1 agrees with base R's besselK", {
  for (z in c(0.05, 0.1, 0.5, 1, 2, 3, 5)) {
    expect_equal(.schab_bessel_k1(z), besselK(z, 1), tolerance = 1e-10)
  }
  # K1 decreases and stays positive
  vals <- vapply(c(0.1, 0.5, 1, 2, 5), .schab_bessel_k1, numeric(1))
  expect_true(all(vals > 0))
  expect_true(all(diff(vals) < 0))
})

test_that("the chi-square survival function agrees with pchisq", {
  for (df in c(1, 2, 3, 5, 10)) {
    for (q in c(0.01, 0.5, 1, 2, 5, 8, 20)) {
      expect_equal(.schab_st_chi2_sf(q, df),
                   pchisq(q, df, lower.tail = FALSE), tolerance = 1e-10)
    }
  }
  # a survival function runs from one down to zero
  expect_equal(.schab_st_chi2_sf(0, 3), 1, tolerance = 1e-10)
  expect_lt(.schab_st_chi2_sf(200, 3), 1e-10)
  s <- vapply(seq(0.1, 20, by = 0.5), .schab_st_chi2_sf, numeric(1), df = 4)
  expect_true(all(diff(s) < 0))
  expect_true(all(s >= 0 & s <= 1))
})

test_that("a separable covariance is the product of its margins", {
  cs <- function(h) exp(-h / 2)
  ct <- function(k) exp(-k / 3)
  for (h in c(0, 0.5, 2)) {
    for (k in c(0, 1, 4)) {
      expect_equal(.schab_st_separable_covariance(h, k, cs, ct),
                   cs(h) * ct(k), tolerance = 1e-12)
    }
  }
  # at the origin both margins are one, so the covariance is one
  expect_equal(.schab_st_separable_covariance(0, 0, cs, ct), 1, tolerance = 1e-12)
})

test_that("the separable exponential is parameterised by decay, not range", {
  # equation 9.4: exp(-theta_s h) exp(-theta_t k), so theta multiplies the
  # lag rather than dividing it
  expect_equal(.schab_st_exponential_separable(1, 2, 2, 3),
               exp(-2 * 1) * exp(-3 * 2), tolerance = 1e-12)
  expect_equal(.schab_st_exponential_separable(0, 0, 2, 3), 1, tolerance = 1e-12)
  # larger decay means faster fall-off
  expect_lt(.schab_st_exponential_separable(1, 1, 5, 5),
            .schab_st_exponential_separable(1, 1, 1, 1))
  expect_error(.schab_st_exponential_separable(1, 1, 0, 1), "must be positive")
  expect_error(.schab_st_exponential_separable(1, 1, 1, -1), "must be positive")
})

test_that("the anisotropic correlation uses squared lags, equation 9.3", {
  corr <- function(u) exp(-u)
  expect_equal(.schab_st_anisotropic_correlation(2, 3, 0.5, 0.25, corr),
               exp(-(0.5 * 4 + 0.25 * 9)), tolerance = 1e-12)
  expect_equal(.schab_st_anisotropic_correlation(0, 0, 1, 1, corr), 1,
               tolerance = 1e-12)
  expect_error(.schab_st_anisotropic_correlation(1, 1, -1, 1, corr),
               "must be positive")
})

test_that("the semivariogram is the sill minus the covariance", {
  cf <- function(h, k) 2 * exp(-h / 2) * exp(-k / 3)
  for (h in c(0, 1, 3)) {
    for (k in c(0, 2, 5)) {
      expect_equal(.schab_st_semivariogram_from_cov(h, k, cf),
                   cf(0, 0) - cf(h, k), tolerance = 1e-12)
    }
  }
  # gamma vanishes at the origin and rises toward the sill
  expect_equal(.schab_st_semivariogram_from_cov(0, 0, cf), 0, tolerance = 1e-12)
  far <- .schab_st_semivariogram_from_cov(50, 50, cf)
  expect_equal(far, cf(0, 0), tolerance = 1e-6)
})

test_that("Gneiting's covariance starts at the variance and decreases", {
  expect_equal(.schab_st_gneiting(0, 0, sigma2 = 3), 3, tolerance = 1e-12)
  # monotone in the spatial lag at a fixed temporal lag, and conversely
  hs <- vapply(c(0, 0.5, 1, 2, 4, 8), function(h)
    .schab_st_gneiting(h, 0.5), numeric(1))
  expect_true(all(diff(hs) < 0))
  ks <- vapply(c(0, 0.5, 1, 2, 4, 8), function(k)
    .schab_st_gneiting(0.5, k), numeric(1))
  expect_true(all(diff(ks) < 0))
  # a covariance cannot exceed its value at the origin
  expect_true(all(hs <= .schab_st_gneiting(0, 0.5) + 1e-12))
  # and scaling the variance scales the whole function
  expect_equal(.schab_st_gneiting(1, 1, sigma2 = 5),
               5 * .schab_st_gneiting(1, 1, sigma2 = 1), tolerance = 1e-12)
})

test_that("the Whittle covariance starts at the variance", {
  expect_equal(.schab_whittle_covariance(0, sigma2 = 2.5), 2.5, tolerance = 1e-10)
  v <- vapply(c(0, 0.5, 1, 2, 4), .schab_whittle_covariance, numeric(1),
              sigma2 = 1, theta = 1)
  expect_true(all(diff(v) < 0))
  expect_true(all(v >= 0))
})

test_that("the covariance matrix is symmetric and non-negative definite", {
  cf <- function(h, k) 2 * exp(-h / 2) * exp(-k / 3)
  coords <- cbind(c(0, 1, 2, 0.5), c(0, 0, 1, 2))
  times <- c(0, 1, 2, 3)
  M <- .schab_st_covariance_matrix(coords, times, cf)
  expect_identical(dim(M), c(4L, 4L))
  expect_equal(M, t(M), tolerance = 1e-12)
  # the diagonal is the variance at zero lag
  expect_equal(diag(M), rep(cf(0, 0), 4), tolerance = 1e-12)
  ev <- eigen(M, only.values = TRUE)$values
  expect_gt(min(ev), -1e-10)
  # and the module's own validity check agrees
  v <- .schab_st_is_valid_covariance(coords, times, cf)
  expect_true(v$valid)
  expect_equal(v$min_eigenvalue, min(ev), tolerance = 1e-8)
})

test_that("an invalid covariance is rejected", {
  # a function that is not a covariance: it grows with the lag, so the
  # matrix it builds has a negative eigenvalue
  bad <- function(h, k) h + k
  coords <- cbind(c(0, 1, 5), c(0, 2, 1))
  v <- .schab_st_is_valid_covariance(coords, c(0, 1, 4), bad)
  expect_false(v$valid)
  expect_lt(v$min_eigenvalue, 0)
})

test_that("the intensity is a count per unit area per unit time", {
  set.seed(1)
  pts <- cbind(runif(50, 0, 10), runif(50, 0, 5))
  tms <- runif(50, 0, 4)
  r <- .schab_st_intensity(pts, tms, c(0, 10, 0, 5), c(0, 4))
  expect_equal(r$n, 50L)
  expect_equal(r$area, 50, tolerance = 1e-12)        # 10 by 5
  expect_equal(r$duration, 4, tolerance = 1e-12)
  expect_equal(r$volume, 200, tolerance = 1e-12)
  expect_equal(r$intensity, 50 / 200, tolerance = 1e-12)
  # doubling the window halves the intensity for the same points
  r2 <- .schab_st_intensity(pts, tms, c(0, 20, 0, 5), c(0, 4))
  expect_equal(r2$intensity, r$intensity / 2, tolerance = 1e-12)
})

test_that("the region box is the bounding rectangle it was given", {
  b <- .schab_st_region_box(c(0, 10, 0, 5))
  expect_equal(as.numeric(unlist(b))[1:4], c(0, 10, 0, 5), tolerance = 1e-12)
})

test_that("the separability test is a likelihood ratio on the -2 logL scale", {
  s <- .schab_st_separability_test(neg2_unrestricted = 100, neg2_separable = 112)
  expect_equal(as.numeric(s$statistic), 12, tolerance = 1e-12)
  # a p-value, and the naive one-degree-of-freedom reference alongside it
  expect_true(s$p_value >= 0 && s$p_value <= 1)
  expect_equal(s$p_value_naive_chi2_1, pchisq(12, 1, lower.tail = FALSE),
               tolerance = 1e-10)
  # no improvement from relaxing separability gives a statistic of zero
  z <- .schab_st_separability_test(100, 100)
  expect_equal(as.numeric(z$statistic), 0, tolerance = 1e-12)
  expect_equal(z$p_value_naive_chi2_1, 1, tolerance = 1e-10)
  # a bigger gap is stronger evidence against separability
  big <- .schab_st_separability_test(100, 140)
  expect_lt(big$p_value_naive_chi2_1, s$p_value_naive_chi2_1)
})

test_that("the complete-spatial-randomness reference is the Poisson mean", {
  r <- .schab_cstr_reference(area = 50, duration = 4, lam = 0.25)
  # the expected count in a space-time volume is lambda times that volume
  expect_equal(as.numeric(r[[1]]), 0.25 * 50 * 4, tolerance = 1e-12)
})

test_that("lag coercion pairs the spatial and temporal lags", {
  lg <- .schab_st_as_lags(c(1, 2, 3), c(4, 5, 6))
  expect_equal(lg$h, c(1, 2, 3), tolerance = 1e-12)
  expect_equal(lg$k, c(4, 5, 6), tolerance = 1e-12)
  # a scalar lag against a vector recycles
  lg2 <- .schab_st_as_lags(1, c(4, 5))
  expect_length(lg2$k, 2L)
})

test_that("the lag matrices are the pairwise distances in space and time", {
  coords <- cbind(c(0, 3, 0), c(0, 4, 0))
  times <- c(0, 1, 5)
  lm <- .schab_st_lag_matrices(coords, times)
  # the 3-4-5 triangle, so the first two sites are five apart
  expect_equal(lm[[1]][1, 2], 5, tolerance = 1e-12)
  expect_equal(lm[[1]][1, 3], 0, tolerance = 1e-12)
  expect_equal(lm[[2]][1, 3], 5, tolerance = 1e-12)
  # both matrices are symmetric with a zero diagonal
  for (M in lm[1:2]) {
    expect_equal(M, t(M), tolerance = 1e-12)
    expect_equal(unname(diag(M)), rep(0, nrow(M)), tolerance = 1e-12)
  }
})
