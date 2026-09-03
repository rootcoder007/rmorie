# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Sec 5.6 nonlinear prediction (Schabenberger & Gotway 2005).
#   splgk  Sec 5.6.1, eq (5.54)          lognormal kriging
#   sptgk  Sec 5.6.2, eqs (5.58)-(5.60)  trans-Gaussian kriging
#
# Both correct a bias the naive back-transform carries, so the tests check
# the CORRECTION, not merely that a number comes out. The pinned values are
# shared with the Python arm.

nlp_sites <- function(k = 5) {
  g <- 0:(k - 1)
  as.matrix(expand.grid(x = g, y = g))
}
nlp_y <- function(C) sin(C[, 1] * 0.6) + cos(C[, 2] * 0.4)
nlp_gamma <- function(h) .sp_semivariogram(h, 0.1, 1.0, 3.0, "exponential")

test_that("ordinary kriging weights satisfy the unbiasedness constraint", {
  C <- nlp_sites()
  r <- .sp_ordinary_kriging(C, nlp_y(C), c(2.3, 1.7), nlp_gamma)
  expect_equal(sum(r$weights), 1, tolerance = 1e-12)
})

test_that("the kriging variance has both forms of equation 5.22", {
  C <- nlp_sites()
  r <- .sp_ordinary_kriging(C, nlp_y(C), c(2.3, 1.7), nlp_gamma)
  gm <- matrix(nlp_gamma(.sp_cross_dist(C, C)), nrow(C), nrow(C))
  g0 <- as.numeric(nlp_gamma(as.numeric(.sp_cross_dist(C, matrix(c(2.3, 1.7), 1)))))
  expect_equal(r$variance, sum(r$weights * g0) + r$lagrange, tolerance = 1e-12)
  expect_equal(r$variance,
               2 * sum(r$weights * g0) -
                 as.numeric(t(r$weights) %*% gm %*% r$weights),
               tolerance = 1e-10)
})

test_that("ordinary kriging reproduces a constant field", {
  C <- nlp_sites()
  r <- .sp_ordinary_kriging(C, rep(7, nrow(C)), c(2.3, 1.7), nlp_gamma)
  expect_equal(r$prediction, 7, tolerance = 1e-9)
})

test_that("the lognormal correction uses the kriging variance", {
  # eq (5.54) corrects by sigma^2_sk/2. The process variance is larger by
  # c'Sigma^-1 c, so using it over-corrects -- the defect the module's
  # earlier formula carried.
  C <- nlp_sites()
  z <- exp(nlp_y(C))
  r <- splgk(C, z, c(2.3, 1.7))
  kr <- .sp_simple_kriging(C, log(z), matrix(c(2.3, 1.7), nrow = 1))
  expect_equal(r$log_variance, as.numeric(kr$variance)[1], tolerance = 1e-12)
  expect_equal(r$bias_factor, exp(0.5 * r$log_variance), tolerance = 1e-12)
  expect_equal(r$prediction, r$naive_prediction * r$bias_factor, tolerance = 1e-12)
})

test_that("the lognormal prediction exceeds the naive back-transform", {
  C <- nlp_sites()
  r <- splgk(C, exp(nlp_y(C)), c(2.3, 1.7))
  expect_gt(r$log_variance, 0)
  expect_gt(r$prediction, r$naive_prediction)
})

test_that("lognormal kriging rejects non-positive data", {
  C <- nlp_sites(3)
  z <- exp(nlp_y(C))
  z[1] <- 0
  expect_error(splgk(C, z, c(1, 1)))
})

test_that("the trans-Gaussian correction is equation 5.58", {
  # Including the SIGN of the Lagrange multiplier: a flipped m would shift
  # every prediction with no other symptom.
  C <- nlp_sites()
  y <- nlp_y(C)
  r <- sptgk(C, y, c(2.3, 1.7), exp, exp, exp, nlp_gamma)
  kr <- .sp_ordinary_kriging(C, y, c(2.3, 1.7), nlp_gamma)
  expect_equal(r$prediction,
               exp(kr$prediction) +
                 0.5 * exp(mean(y)) * (kr$variance - 2 * kr$lagrange),
               tolerance = 1e-12)
  expect_equal(r$lagrange, kr$lagrange, tolerance = 1e-14)
})

test_that("the trans-Gaussian MSPE is equation 5.59", {
  C <- nlp_sites()
  y <- nlp_y(C)
  r <- sptgk(C, y, c(2.3, 1.7), exp, exp, exp, nlp_gamma)
  kr <- .sp_ordinary_kriging(C, y, c(2.3, 1.7), nlp_gamma)
  expect_equal(r$mspe, exp(mean(y))^2 * kr$variance, tolerance = 1e-12)
})

test_that("an identity transformation needs no correction", {
  # phi(y) = y has phi'' = 0, so (5.58) collapses to ordinary kriging. A
  # correction here would mean the second-derivative term is wired to
  # something other than phi''.
  C <- nlp_sites()
  y <- nlp_y(C)
  r <- sptgk(C, y, c(2.3, 1.7), function(v) v, function(v) 1, function(v) 0,
             nlp_gamma)
  kr <- .sp_ordinary_kriging(C, y, c(2.3, 1.7), nlp_gamma)
  expect_equal(r$correction, 0, tolerance = 1e-15)
  expect_equal(r$prediction, kr$prediction, tolerance = 1e-12)
})

test_that("normal scores and anamorphosis invert each other", {
  z <- exp(nlp_y(nlp_sites()))
  s <- morie_normal_scores(z)
  expect_equal(sort(morie_anamorphosis(z, s)), sort(z), tolerance = 1e-12)
  expect_equal(mean(s), 0, tolerance = 0.05)
  expect_true(all(is.finite(s)))
  expect_true(all(diff(s[order(z)]) > 0))
})

test_that("the Sec 5.6 family matches the Python arm", {
  C <- nlp_sites()
  y <- nlp_y(C)
  z <- exp(y)
  r <- splgk(C, z, c(2.3, 1.7))
  t <- sptgk(C, y, c(2.3, 1.7), exp, exp, exp, nlp_gamma)
  expect_equal(r$prediction, 6.70711848875139, tolerance = 1e-11)
  expect_equal(r$bias_factor, 1.57202113531362, tolerance = 1e-11)
  expect_equal(t$prediction, 6.35358234291142, tolerance = 1e-11)
  expect_equal(t$correction, 0.969636334133602, tolerance = 1e-11)
  expect_equal(t$mspe, 6.73401581757023, tolerance = 1e-11)
})
