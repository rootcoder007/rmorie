# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the additive-model mirrors. Anchors from
# the Python modules at full double precision; LCG fixture so both
# languages see identical bits.

hrz9_fix <- function(n = 200L, seed = 13579) {
  m <- 3L * n
  u <- numeric(m)
  s <- seed
  for (k in seq_len(m)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[k] <- (s + 0.5) / 4294967296
  }
  x <- cbind(2 * u[1:n] - 1, 2 * u[(n + 1):(2 * n)] - 1)
  y <- 2 + sin(pi * x[, 1]) + x[, 2]^2 - 1 / 3 +
    0.2 * stats::qnorm(u[(2 * n + 1):(3 * n)])
  list(x = x, y = y, n = n)
}

test_that("morie_marginal_integration matches morie.fn.hrzmir", {
  f <- hrz9_fix()
  mi <- morie_marginal_integration(f$x, f$y, j = 1L)
  expect_equal(mi$m_hat[1], -0.6131162789222979, tolerance = 1e-9)
  expect_equal(mi$m_hat[21], 0.011598812529935776, tolerance = 1e-8)
  expect_equal(mi$m_hat[41], 0.5407464376286721, tolerance = 1e-9)
  expect_equal(mi$mu_hat, 1.99857386252222, tolerance = 1e-12)
  expect_equal(mi$h1, 0.20308620766598953, tolerance = 1e-12)
  expect_equal(mi$h2, 0.2562867924917901, tolerance = 1e-12)
  # mu = E(Y) exactly, by the location normalisation (3.7)
  expect_equal(mi$mu_hat, mean(f$y))
  # the component tracks sin(pi x)
  tr <- sin(pi * mi$grid)
  expect_gt(stats::cor(mi$m_hat, tr - mean(tr)), 0.9)
})

test_that("marginal integration reports the curse it carries", {
  f <- hrz9_fix()
  expect_equal(morie_marginal_integration(f$x, f$y, j = 1L)$smoothness_required, 2L)
  expect_true(morie_marginal_integration(f$x, f$y, j = 1L)$curse_of_dimensionality)
  # Theorem 3.1(b) needs q > d - 1, so the requirement GROWS with d
  x5 <- cbind(f$x, f$x[, 1] * 0.5, f$x[, 2] * 0.5, f$x[, 1] * -0.25)
  expect_equal(morie_marginal_integration(x5, f$y, j = 1L)$smoothness_required, 5L)
  expect_error(morie_marginal_integration(f$x, f$y, j = 9L), "must lie in 1..2")
  expect_error(morie_marginal_integration(f$x[, 1, drop = FALSE], f$y),
               "at least 2 components")
})

test_that("morie_two_step_additive matches morie.fn.hrzora", {
  f <- hrz9_fix()
  o <- morie_two_step_additive(f$x, f$y, kappa = 4L, bandwidth = 0.25)
  expect_equal(o$m_hat[1, 11], -0.7579876657310856, tolerance = 1e-9)
  expect_equal(o$m_hat[2, 11], -0.05572231009675045, tolerance = 1e-8)
  expect_equal(o$mu_hat, 1.9825243567051036, tolerance = 1e-10)
  expect_equal(o$theta[2], 1.00398625632515, tolerance = 1e-10)
  expect_equal(o$kappa, 4L)

  nw <- morie_two_step_additive(f$x, f$y, kappa = 4L, bandwidth = 0.25,
                                local_linear = FALSE)
  expect_equal(nw$m_hat[1, 11], -0.7614441285705607, tolerance = 1e-9)
  expect_equal(nw$m_hat[2, 11], -0.016752349237703787, tolerance = 1e-8)
})

test_that("the two-step estimator's claims are kept as checkable keys", {
  f <- hrz9_fix()
  o <- morie_two_step_additive(f$x, f$y)
  expect_true(o$oracle_efficient)
  expect_false(o$iterative)              # unlike backfitting
  expect_equal(o$rate_exponent, -0.4)
  expect_equal(o$max_smoothing_dimension, 1L)
  expect_false(o$curse_of_dimensionality)
  # both components recovered from one fit
  g <- o$grid
  t1 <- sin(pi * g); t1 <- t1 - mean(t1)
  t2 <- g^2 - 1 / 3; t2 <- t2 - mean(t2)
  expect_gt(stats::cor(o$m_hat[1, ], t1), 0.9)
  expect_gt(stats::cor(o$m_hat[2, ], t2), 0.9)
  expect_error(morie_two_step_additive(f$x, f$y, kappa = 1L), "at least 2")
  expect_error(morie_two_step_additive(f$x, f$y, bandwidth = -1), "positive")
})

test_that("the Fourier basis spans odd functions, unlike cosines alone", {
  # A cosine-only basis satisfies (3.15) and (3.16) but is EVEN, so it
  # cannot partial an odd component out -- and the damage lands on the
  # OTHER component. With sin(pi x) present, a cosine basis recovered
  # the x^2 component at only 0.92 regardless of kappa.
  f <- hrz9_fix()
  o <- morie_two_step_additive(f$x, f$y, kappa = 6L)
  g <- o$grid
  t2 <- g^2 - 1 / 3; t2 <- t2 - mean(t2)
  expect_gt(stats::cor(o$m_hat[2, ], t2), 0.95)
})
