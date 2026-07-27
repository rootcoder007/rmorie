# Checked against the identities in Imai, Keele & Yamamoto (2010),
# Statistical Science 25(1), 51-71, not against a reference package.

.sensmi_fixture <- function(seed = 11, n = 400) {
  set.seed(seed)
  tr <- rbinom(n, 1, 0.5)
  md <- 1 + 0.8 * tr + rnorm(n)
  yy <- 2 + 0.3 * tr + 0.5 * md + rnorm(n)
  list(y = yy, tr = tr, md = md)
}

test_that("morie_mediation_sensitivity recovers the indirect path", {
  f <- .sensmi_fixture()
  r <- morie_mediation_sensitivity(f$y, f$tr, f$md)
  expect_equal(r$beta2, 0.8, tolerance = 0.2)
  expect_equal(r$gamma, 0.5, tolerance = 0.15)
  # Theorem 2: the ACME is the product of the two fitted coefficients.
  # That identity is exact, and is what this line pins down.
  expect_equal(r$estimate, r$beta2 * r$gamma, tolerance = 1e-12)
  # The true product is 0.8 * 0.5 = 0.4, but at n = 400 the product has
  # a standard error near 0.065, so a single draw sits well away from it
  # -- this seed gives 0.31. testthat tolerance is relative, so 0.5 is
  # roughly a 3 SE band, not 50%.
  expect_equal(r$estimate, 0.4, tolerance = 0.5)
})

test_that("the ACME estimate is centred on the truth across seeds", {
  # One draw cannot separate a biased estimator from sampling noise.
  est <- vapply(1:8, function(s) {
    f <- .sensmi_fixture(seed = s)
    morie_mediation_sensitivity(f$y, f$tr, f$md)$estimate
  }, numeric(1))
  expect_equal(mean(est), 0.4, tolerance = 0.25)
})

test_that("Theorem 4 reduces to Theorem 2 at rho = 0", {
  # OLS makes e3 orthogonal to e2 and e1 = gamma*e2 + e3, so
  # rho_tilde * sigma1 / sigma2 == gamma identically. The agreement is
  # exact, not approximate.
  f <- .sensmi_fixture()
  r <- morie_mediation_sensitivity(f$y, f$tr, f$md, r2_grid = 0)
  expect_equal(r$acme_positive[1], r$estimate, tolerance = 1e-10)
  expect_equal(r$acme_negative[1], r$estimate, tolerance = 1e-10)
})

test_that("the ACME vanishes at the breakdown correlation", {
  f <- .sensmi_fixture()
  r <- morie_mediation_sensitivity(f$y, f$tr, f$md)
  at <- morie_mediation_sensitivity(f$y, f$tr, f$md,
                                    r2_grid = r$rho_breakdown^2)
  branch <- if (r$rho_breakdown > 0) at$acme_positive else at$acme_negative
  expect_equal(branch[1], 0, tolerance = 1e-10)
})

test_that("the ACME is monotone in rho", {
  f <- .sensmi_fixture()
  r <- morie_mediation_sensitivity(f$y, f$tr, f$md,
                                   r2_grid = seq(0, 0.9, length.out = 12))
  d <- diff(r$acme_positive)
  expect_true(all(d < 0) || all(d > 0))
})

test_that("rho_grid is the square root of the R^2 product", {
  f <- .sensmi_fixture()
  r <- morie_mediation_sensitivity(f$y, f$tr, f$md,
                                   r2_grid = c(0, 0.25, 0.64))
  expect_equal(r$rho_grid, c(0, 0.5, 0.8), tolerance = 1e-12)
})

test_that("morie_mediation_sensitivity validates its inputs", {
  f <- .sensmi_fixture()
  expect_error(morie_mediation_sensitivity(f$y, f$tr, f$md[-1]),
               "same length")
  expect_error(morie_mediation_sensitivity(f$y, f$tr, f$md, r2_grid = 1),
               "\\[0, 1\\)")
})
