# Truncated CDP accounting (Bun et al. 2018; Bun & Steinke 2016; Mironov
# 2017): the (epsilon, delta) conversion is recomputed as the minimum
# over alpha of the Renyi bound, and the inverses are checked by
# round trip.

tm_eps_ref <- function(rho, delta, omega = Inf) {
  # minimiser of rho a + l / (a - 1) is a* = 1 + sqrt(l / rho); the
  # bound is convex in a, so the truncated minimum sits at min(a*, omega)
  l <- log(1 / delta)
  a <- min(1 + sqrt(l / rho), omega)
  rho * a + l / (a - 1)
}
tm_eps_num <- function(rho, delta, omega) {
  l <- log(1 / delta)
  stats::optimize(function(a) rho * a + l / (a - 1), c(1 + 1e-9, min(omega, 1e6)),
                  tol = 1e-10)$objective
}

test_that("epsilon is the best Renyi-order bound, truncated at omega", {
  for (rho in c(0.01, 0.2, 1.5)) for (delta in c(1e-5, 0.01)) {
    free <- rho + 2 * sqrt(rho * log(1 / delta))
    expect_equal(morie_tcmech_eps(rho, delta), free, tolerance = 1e-12)
    expect_equal(morie_tcmech_eps(rho, delta), tm_eps_ref(rho, delta), tolerance = 1e-9)
    for (w in c(1.5, 3, 50)) {
      expect_equal(morie_tcmech_eps(rho, delta, w), tm_eps_ref(rho, delta, w),
                   tolerance = 1e-12)
      # numerical minimisation agrees up to optimize()'s own accuracy
      expect_equal(morie_tcmech_eps(rho, delta, w), tm_eps_num(rho, delta, w),
                   tolerance = 1e-6)
    }
  }
  expect_identical(morie_tcmech_eps(0, 0.1), 0)
  expect_error(morie_tcmech_eps(-1, 0.1), "negative")
  expect_error(morie_tcmech_eps(1, 1), "strictly inside")
  expect_error(morie_tcmech_eps(1, 0.1, omega = 1), "exceed one")
})

test_that("the floor is log(1/delta)/(omega - 1), zero without truncation", {
  expect_identical(morie_tcmech_floor(0.01), 0)
  expect_equal(morie_tcmech_floor(0.01, 3), log(100) / 2, tolerance = 1e-12)
  expect_error(morie_tcmech_floor(0.01, 0.5), "exceed one")
})

test_that("rho inverts the epsilon conversion by bisection", {
  for (w in list(NULL, 8, 20)) {
    r <- morie_tcmech_rho(2, 1e-5, w)
    expect_equal(morie_tcmech_eps(r, 1e-5, w), 2, tolerance = 1e-9)
  }
  # closed form without truncation: sqrt(rho) = -sqrt(l) + sqrt(l + eps)
  l <- log(1e5)
  expect_equal(morie_tcmech_rho(2, 1e-5), (sqrt(l + 2) - sqrt(l))^2, tolerance = 1e-12)
  expect_error(morie_tcmech_rho(0, 0.1), "positive")
  expect_error(morie_tcmech_rho(1, 1e-5, 2), "irreducible")
})

test_that("Gaussian sigma, rho from sigma and from pure DP", {
  expect_equal(morie_tcmech_sigma(3, 0.5), 3, tolerance = 1e-12)
  expect_equal(morie_tcmech_rho_from_sigma(3, morie_tcmech_sigma(3, 0.37)), 0.37,
               tolerance = 1e-12)
  expect_equal(morie_tcmech_rho_from_pure(0.6), 0.18, tolerance = 1e-12)
  expect_error(morie_tcmech_sigma(0, 1), "sensitivity")
  expect_error(morie_tcmech_sigma(1, 0), "rho")
  expect_error(morie_tcmech_rho_from_sigma(1, 0), "noise scale")
  expect_error(morie_tcmech_rho_from_pure(-1), "negative")
})

test_that("composition adds rho and keeps the tightest truncation", {
  c1 <- morie_tcmech_compose(c(0.1, 0.25, 0.05))
  expect_equal(c1$rho, 0.4, tolerance = 1e-12)
  expect_null(c1$omega)
  c2 <- morie_tcmech_compose(c(0.1, 0.2), list(NULL, 7))
  expect_identical(c2$omega, 7)
  expect_identical(morie_tcmech_compose(numeric(0))$rho, 0)
})

test_that("the mechanism clips, splits rho and adds seeded Gaussian noise", {
  y <- c(-5, -0.4, 0.2, 3, 9)
  r <- morie_tcmech(y, f_value = 10, C = 2, epsilon = 1, delta = 1e-6,
                    seed = 3, n_release = 4)
  expect_equal(r$clipped, pmin(pmax(y, -2), 2))
  expect_identical(r$n_clipped, 3L)
  rho <- morie_tcmech_rho(1, 1e-6)
  expect_equal(r$rho_total, rho, tolerance = 1e-12)
  expect_equal(r$sigma, 2 / sqrt(2 * rho / 4), tolerance = 1e-12)
  noise <- .ghc_norm(.ghc_rng(3), 1L, 0, r$sigma)
  expect_equal(r$private_value, 10 + noise, tolerance = 1e-12)
  expect_false(r$truncation_binds)
  rt <- morie_tcmech(y, 10, 2, epsilon = 3, delta = 1e-6, omega = 10)
  expect_true(rt$truncation_binds)
  expect_equal(rt$epsilon_achieved, 3, tolerance = 1e-9)
  expect_error(morie_tcmech(y, 1, 0, 1, 0.1), "clipping bound")
  expect_error(morie_tcmech(y, 1, 1, 1, 0.1, n_release = 0), "at least one")
  expect_match(morie_tcmech_cheatsheet(), "sqrt(2 rho)", fixed = TRUE)
})
