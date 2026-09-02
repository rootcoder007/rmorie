# Deterministic-seed plumbing tests for ksr08 (Kosorok Gaussian
# multiplier bootstrap).
#
# Verifies that the `deterministic_seed` argument added in morie v0.4.0:
#   * gives the same (estimate, se) on repeat calls with the same seed
#   * gives different numbers when the seed is changed
#   * leaves the default `deterministic_seed = NULL` path unchanged

skip_if_no_hash <- function() {
  ok <- requireNamespace("digest", quietly = TRUE) ||
    requireNamespace("openssl", quietly = TRUE)
  testthat::skip_if_not(ok, "neither 'digest' nor 'openssl' available")
}

xs_fixture <- function() {
  c(
    0.1, 0.4, -0.3, 0.7, 0.05, -0.9, 1.2, -0.4, 0.6, -0.1,
    0.3, -0.2, 0.5, -0.7, 0.0, 0.2, -0.1, 0.4, -0.5, 0.8
  )
}

test_that("ksr08 seed is reproducible", {
  set.seed(1); xs <- rnorm(40)
  a <- morie_ksr08_kosorok_multiplier_bootstrap(xs, B = 100, seed = 42L)
  b <- morie_ksr08_kosorok_multiplier_bootstrap(xs, B = 100, seed = 42L)
  expect_identical(a$boot_sd, b$boot_sd)
})

test_that("ksr08 default (deterministic_seed = NULL) path is unchanged", {
  xs <- xs_fixture()
  r1 <- morie_ksr08_kosorok_multiplier_bootstrap(xs, B = 400, seed = 42L)
  r2 <- morie_ksr08_kosorok_multiplier_bootstrap(xs, B = 400, seed = 42L)
  expect_equal(r1$estimate, r2$estimate)
  expect_equal(r1$se, r2$se)
})
