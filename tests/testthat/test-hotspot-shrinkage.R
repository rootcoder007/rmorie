# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P19 (continued): shrinkage must agree with research/lean/P19Shrinkage.lean.

noise_law <- function(theta, e, w) {
  # project e onto the orthogonal complement of {1, theta} under the weights, so sum w e = 0 and sum w theta e = 0
  X <- cbind(1, theta)
  e - X %*% solve(crossprod(X * sqrt(w)), crossprod(X * w, e))
}

test_that("loss_eq, loss_min, loss_bstar_eq, loss_bstar_le_raw under the finite noise law", {
  set.seed(19)
  for (rep in 1:40) {
    n <- sample(3:30, 1)
    theta <- runif(n, 0, 50)
    w <- runif(n, 0.5, 2)
    e <- as.numeric(noise_law(theta, rnorm(n, 0, 5), w))
    expect_lt(abs(sum(w * e)), 1e-9)
    expect_lt(abs(sum(w * theta * e)), 1e-9)
    Se <- sum(w * e^2); St <- sum(w * (theta - sum(w * theta) / sum(w))^2)
    Bstar <- Se / (Se + St)
    ls <- morie_shrinkage_loss(theta, e, w, B = Bstar)
    expect_true(ls$noise_law)
    expect_equal(ls$Se, Se, tolerance = 1e-12); expect_equal(ls$Stheta, St, tolerance = 1e-12)
    expect_equal(ls$B_star, Bstar, tolerance = 1e-12)
    expect_equal(ls$loss, ls$closed_form, tolerance = 1e-9)                      # loss_eq
    expect_equal(ls$loss_star, Se * St / (Se + St), tolerance = 1e-12)           # loss_bstar_eq
    expect_equal(ls$loss, ls$loss_star, tolerance = 1e-9)
    expect_lte(ls$loss_star, ls$loss_raw + 1e-12)                                # loss_bstar_le_raw
    expect_equal(morie_shrinkage_loss(theta, e, w, B = 0)$loss, Se, tolerance = 1e-9)   # loss_raw
    for (B in c(0, 0.1, 0.5, 0.9, 1, Bstar + 0.05)) {
      l <- morie_shrinkage_loss(theta, e, w, B = B)
      expect_equal(l$loss, (1 - B)^2 * Se + B^2 * St, tolerance = 1e-9)         # loss_eq
      expect_gte(l$loss, ls$loss - 1e-9)                                         # loss_min
    }
    expect_gte(Bstar, 0); expect_lte(Bstar, 1)                                   # bstar_mem
  }
})

test_that("without the noise law the closed form is reported as not applicable", {
  l <- morie_shrinkage_loss(c(1, 2, 3), c(1, 1, 1), B = 0.5)
  expect_false(l$noise_law)
  expect_false(isTRUE(all.equal(l$loss, l$closed_form)))
  z <- morie_shrinkage_loss(c(1, 1), c(0, 0), B = 0.3)
  expect_equal(z$B_star, 0); expect_equal(z$loss_star, 0)
})

test_that("hotspot_shrinkage: predicted_fall identity, B* from the variance split, bounds", {
  set.seed(191)
  for (rep in 1:30) {
    n <- sample(2:40, 1)
    y <- rpois(n, sample(5:60, 1))
    w <- runif(n, 0.5, 2)
    s <- morie_hotspot_shrinkage(y, noise_variance = mean(y), weights = w)
    ybar <- sum(w * y) / sum(w)
    expect_equal(s$mean, ybar, tolerance = 1e-12)
    total <- sum(w * (y - ybar)^2) / sum(w)
    expect_equal(s$total_variance, total, tolerance = 1e-12)
    expect_equal(s$signal_variance, max(total - mean(y), 0), tolerance = 1e-12)
    Se <- mean(y) * sum(w); St <- s$signal_variance * sum(w)
    expect_equal(s$B_star, if (Se + St > 0) Se / (Se + St) else 0, tolerance = 1e-12)
    expect_equal(s$B, s$B_star)
    expect_gte(s$B, 0); expect_lte(s$B, 1)                                      # bstar_mem
    expect_equal(y - s$shrunk, s$B * (y - ybar), tolerance = 1e-10)             # predicted_fall
    expect_equal(s$shrunk, (1 - s$B) * y + s$B * ybar, tolerance = 1e-12)
    s2 <- morie_hotspot_shrinkage(y, noise_variance = mean(y), weights = w, B = 0.25)
    expect_equal(s2$B, 0.25)
    expect_equal(y - s2$shrunk, 0.25 * (y - ybar), tolerance = 1e-10)
  }
  z <- morie_hotspot_shrinkage(c(0, 0, 0), noise_variance = 0)
  expect_equal(z$B, 0)
})

test_that("input checks", {
  expect_error(morie_hotspot_shrinkage(1, 1), "at least two")
  expect_error(morie_hotspot_shrinkage(c(1, 2), -1), "noise_variance")
  expect_error(morie_hotspot_shrinkage(c(1, 2), 1, weights = c(1, -1)), "weights")
  expect_error(morie_hotspot_shrinkage(c(1, 2), 1, B = 2), "B must")
  expect_error(morie_shrinkage_loss(c(1, 2), c(1), B = 0.5), "equal length")
  expect_error(morie_shrinkage_loss(c(1, 2), c(1, 2), B = c(0.5, 0.6)), "single")
  expect_error(morie_shrinkage_loss(c(1, 2), c(1, 2), weights = c(0, 0), B = 0.5), "weights")
})
