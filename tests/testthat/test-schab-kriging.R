# SPDX-License-Identifier: AGPL-3.0-or-later
# Book-certified tests for the Schabenberger & Gotway kriging family.
# The same identities the Python arm asserts, so this doubles as the R
# leg of three-way parity.
#
# Schabenberger, O. & Gotway, C. A. (2005). Ch 5.

cm <- list(nugget = 0, sill = 1, range = 2, model = "exponential")

field <- function(n = 25, seed = 7) {
  set.seed(seed)
  co <- matrix(runif(2 * n), n, 2) * 5
  list(co = co, z = sin(co[, 1]) + cos(co[, 2]))
}

test_that("simple kriging is an exact interpolator (p. 224)", {
  f <- field()
  r <- spskrg(f$co, f$z, f$co, cm)
  expect_equal(r$prediction, f$z, tolerance = 1e-10)
  expect_lt(max(r$variance), 1e-10)
})

test_that("the simple kriging weight on the point itself is one", {
  f <- field()
  w <- spskrg(f$co, f$z, f$co, cm)$weights
  expect_equal(diag(w), rep(1, nrow(f$co)), tolerance = 1e-9)
})

test_that("simple kriging matches the closed form (5.10)-(5.11)", {
  f <- field(n = 12, seed = 3)
  tgt <- matrix(c(2, 2), 1, 2)
  mu <- 0.3
  Sigma <- .sp_cov_from_model(.sp_cross_dist(f$co, f$co), cm)
  sig <- as.numeric(.sp_cov_from_model(.sp_cross_dist(f$co, tgt), cm))
  s2 <- as.numeric(.sp_cov_from_model(0, cm))
  lam <- solve(Sigma, sig)
  r <- spskrg(f$co, f$z, tgt, cm, mu = mu)
  expect_equal(r$prediction[1], mu + sum(lam * (f$z - mu)), tolerance = 1e-10)
  expect_equal(r$variance[1], s2 - sum(sig * lam), tolerance = 1e-10)
})

test_that("BLUP weights sum to one and it honours the data", {
  f <- field()
  b <- spblup(f$co, f$z, f$co, cm)
  expect_equal(colSums(b$weights), rep(1, nrow(f$co)), tolerance = 1e-9)
  expect_equal(b$prediction, f$z, tolerance = 1e-9)
})

test_that("BLUP variance is never below simple kriging's", {
  f <- field()
  tgt <- matrix(c(1.3, 2.7, 4.0, 0.5), 2, 2, byrow = TRUE)
  expect_true(all(spblup(f$co, f$z, tgt, cm)$variance >=
                    spskrg(f$co, f$z, tgt, cm)$variance - 1e-12))
})

test_that("ordinary kriging weights sum to one exactly", {
  r <- spkwt(diag(3) * 2, c(1, 0.5, 0.25), unbiased = TRUE)
  expect_equal(r$weight_sum, 1, tolerance = 1e-12)
  expect_false(is.null(r$lagrange))
})

test_that("simple kriging weights solve Sigma lambda = sigma", {
  set.seed(2)
  A <- matrix(runif(16), 4, 4)
  Sigma <- crossprod(A) + 4 * diag(4)
  sig <- runif(4)
  expect_equal(as.numeric(Sigma %*% spkwt(Sigma, sig)$weights), sig,
               tolerance = 1e-10)
})

test_that("the sill is a pure variance factor (Sec 5.2.3)", {
  a <- spnsr(0, 1, 1)
  b <- spnsr(0, 7, 1)
  expect_equal(b$prediction, a$prediction, tolerance = 1e-12)
  expect_equal(b$variance / a$variance, 7, tolerance = 1e-10)
  expect_equal(b$weights, a$weights, tolerance = 1e-12)
})

test_that("a pure nugget collapses the prediction to the mean", {
  r <- spnsr(nugget = 1, sill = 0, range = 1)
  expect_equal(r$prediction, r$mean, tolerance = 1e-12)
  expect_equal(r$weight_spread, 0, tolerance = 1e-12)
})

test_that("a larger nugget flattens the weights", {
  sp <- vapply(c(0, 0.5, 2, 10), function(g) spnsr(g, 1, 1)$weight_spread,
               numeric(1))
  expect_true(all(diff(sp) < 0))
})

test_that("leave-one-out residuals are not identically zero", {
  f <- field()
  r <- spkfnn(f$co, f$z, cm)
  expect_gt(r$mspe, 0)
  expect_equal(r$rmspe, sqrt(r$mspe), tolerance = 1e-12)
  expect_length(r$residuals, nrow(f$co))
  expect_lt(abs(r$me), 0.5 * r$rmspe)
})

test_that("kriging input validation", {
  f <- field(n = 10)
  expect_error(spskrg(f$co, f$z[-1], matrix(c(1, 1), 1, 2), cm),
               "same number of rows")
  expect_error(spkfnn(matrix(0, 2, 2), c(0, 0), cm), "at least 3 points")
  expect_error(spkwt(matrix(1, 2, 3), c(1, 1)), "square")
  expect_error(spnsr(0, 1, 0), "`range` must be")
  expect_error(spnsr(-1, 1, 1), "must be >= 0")
})
