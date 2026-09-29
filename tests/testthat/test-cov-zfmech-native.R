# zCDP accounting (Bun & Steinke 2016) recomputed from the propositions.

test_that("Renyi divergence of Gaussians is alpha d^2 / (2 sigma^2)", {
  # numeric check of D_alpha = log(int p^alpha q^(1-alpha)) / (alpha - 1)
  a <- 2.5
  f <- function(x) exp(a * stats::dnorm(x, 0.3, 1.7, log = TRUE) +
                         (1 - a) * stats::dnorm(x, -0.9, 1.7, log = TRUE))
  num <- log(stats::integrate(f, -40, 40, rel.tol = 1e-12)$value) / (a - 1)
  expect_equal(renyi_divergence_gaussian(0.3, -0.9, 1.7, a), num, tolerance = 1e-9)
  expect_equal(renyi_divergence_gaussian(1, 3, 2, 4), 4 * 4 / 8, tolerance = 1e-12)
  expect_error(renyi_divergence_gaussian(0, 1, 0, 2), "sigma")
  expect_error(renyi_divergence_gaussian(0, 1, 1, 1), "alpha")
  expect_error(renyi_divergence_gaussian(0, 1, 1, Inf), "alpha")
})

test_that("Gaussian zCDP, sigma for rho and the alias are inverse maps", {
  expect_equal(zcdp_of_gaussian(2, 0.5), 4 / (2 * 0.25), tolerance = 1e-12)
  expect_equal(zero_concentrated_dp(3, 1.5), 9 / 4.5, tolerance = 1e-12)
  s <- sigma_for_rho(1.3, 0.2)
  expect_equal(s, 1.3 / sqrt(0.4), tolerance = 1e-12)
  expect_equal(zcdp_of_gaussian(1.3, s), 0.2, tolerance = 1e-12)
  # tightness: D_alpha / alpha equals rho for every alpha
  for (a in c(1.5, 3, 10)) {
    expect_equal(renyi_divergence_gaussian(0, 1.3, s, a) / a, 0.2, tolerance = 1e-12)
  }
  expect_error(zcdp_of_gaussian(1, 0), "sigma")
  expect_error(zcdp_of_gaussian(-1, 1), "negative")
  expect_error(sigma_for_rho(1, 0), "rho must be positive")
  expect_error(sigma_for_rho(-1, 1), "negative")
})

test_that("the Gaussian mechanism adds sigma times a Box-Muller cosine draw", {
  r <- morie_zfmech(10, 2, 0.5, seed = 7, n = 4)
  sig <- 2 / sqrt(1)
  uu <- .ghc_unif(.ghc_rng(7), 8L)
  z <- sqrt(-2 * log(pmax(uu[c(1, 3, 5, 7)], 1e-12))) * cos(2 * pi * uu[c(2, 4, 6, 8)])
  expect_equal(r$release, 10 + sig * z, tolerance = 1e-12)
  expect_equal(r$sigma, sig, tolerance = 1e-12)
  one <- gaussian_mechanism(10, 2, 0.5, seed = 7)
  expect_length(one$release, 1L)
  expect_equal(one$release, r$release[1], tolerance = 1e-12)
  expect_error(morie_zfmech(1, 1, -1), "rho")
})

test_that("group privacy, conversions, round trip and post-processing", {
  g <- group_privacy(0.1, 3)
  expect_equal(g$rho, 0.9, tolerance = 1e-12)
  expect_error(group_privacy(0.1, 0), "at least 1")
  ad <- to_approx_dp(0.05, 1e-5)
  expect_equal(ad$epsilon, 0.05 + 2 * sqrt(0.05 * log(1e5)), tolerance = 1e-12)
  expect_error(to_approx_dp(0.05, 1), "delta")
  expect_error(to_approx_dp(0, 0.1), "rho")
  expect_equal(from_pure_dp(0.8)$rho, 0.32, tolerance = 1e-12)
  expect_error(from_pure_dp(-1), "negative")
  rt <- round_trip(0.8, 1e-6)
  eo <- 0.32 + 2 * sqrt(0.32 * log(1e6))
  expect_equal(rt$epsilon_out, eo, tolerance = 1e-12)
  expect_equal(rt$inflation, eo - 0.8, tolerance = 1e-12)
  expect_identical(round_trip(0, 0.1)$epsilon_out, 0)
  expect_equal(postprocessing(0.3)$rho, 0.3)
  expect_error(postprocessing(0), "rho")
})
