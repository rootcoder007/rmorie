# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the single-index mirrors. Anchors printed
# from the Python modules at full double precision. Fixture is an
# explicit LCG so both languages see identical bits.

hrz8_fix <- function(n = 300L, seed = 2468) {
  m <- 4L * n
  u <- numeric(m)
  s <- seed
  for (k in seq_len(m)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[k] <- (s + 0.5) / 4294967296
  }
  z <- stats::qnorm(u)
  x <- cbind(z[1:n], z[(n + 1):(2 * n)])
  list(x = x, z = z, u = u, n = n, beta = c(1, -0.6))
}

test_that("morie_rank_index matches morie.fn.hrzrank", {
  f <- hrz8_fix()
  y <- as.numeric(tanh(f$x %*% f$beta) + 0.3 * f$z[601:900])
  o <- morie_rank_index(f$x, y)
  expect_equal(o$beta[2], -0.5800000000000001, tolerance = 1e-12)
  expect_equal(o$objective, 0.42962095875139356, tolerance = 1e-10)
  # the whole appeal: no smoothing parameter anywhere
  expect_false(o$requires_bandwidth)
  # and the whole cost: not efficient, no analytic SE offered
  expect_false(o$asymptotically_efficient)
  expect_null(o$se)
  expect_equal(o$inference, "bootstrap")
  expect_lt(abs(o$beta[2] - (-0.6)), 0.15)

  cs <- morie_rank_index(f$x, y, variant = "cs")
  expect_equal(cs$beta[2], -0.6099999999999994, tolerance = 1e-12)
  expect_equal(cs$objective, 97.99130434782609, tolerance = 1e-10)
  expect_error(morie_rank_index(f$x, y, variant = "spearman"), "mrc")
})

test_that("morie_rank_index uses only the ordering of y", {
  f <- hrz8_fix()
  y <- as.numeric(tanh(f$x %*% f$beta) + 0.3 * f$z[601:900])
  a <- morie_rank_index(f$x, y)
  b <- morie_rank_index(f$x, exp(y / 2))   # strictly increasing transform
  expect_equal(a$beta, b$beta)
})

test_that("morie_nls_weight_function matches morie.fn.hrzwfun", {
  f <- hrz8_fix()
  y <- as.numeric(tanh(f$x %*% f$beta) + 0.3 * f$z[601:900])
  w <- morie_nls_weight_function(f$x, y, beta_hat = f$beta)
  expect_equal(w$weights[11], 11.009094313706088, tolerance = 1e-9)
  expect_equal(w$sigma2_hat[11], 0.09083399337900315, tolerance = 1e-10)
  expect_equal(w$bandwidth, 0.3861012517457001, tolerance = 1e-12)
  expect_equal(w$omega[1, 1], 0.37807332308903213, tolerance = 1e-8)
  expect_equal(w$omega_SI[1, 1], 0.3249918061407782, tolerance = 1e-8)
  expect_equal(w$max_weight, 31.98829171662153, tolerance = 1e-9)
  expect_true(w$efficient_weight_used)
  expect_equal(w$weights, 1 / w$sigma2_hat)
  # unknown G costs EFFICIENCY, not rate -- kept as separate keys
  # because conflating them is the usual error
  expect_true(w$efficiency_loss_from_unknown_G)
  expect_false(w$rate_loss_from_unknown_G)
  expect_error(morie_nls_weight_function(f$x, y, weights = rep(-1, 300)),
               "non-negative")
})

test_that("morie_one_step_efficient matches morie.fn.hrzasym", {
  f <- hrz8_fix()
  y <- as.numeric(tanh(f$x %*% f$beta) + 0.3 * f$z[601:900])
  s <- morie_one_step_efficient(f$x, y, initial_estimator = c(1, -0.2))
  expect_equal(s$beta[2], -0.5628654376418598, tolerance = 1e-9)
  expect_equal(s$se[1], 0.026973635776377486, tolerance = 1e-9)
  expect_equal(s$bandwidth, 0.3395978814291146, tolerance = 1e-12)
  expect_equal(s$omega[1, 1], 0.2182731080990014, tolerance = 1e-8)
  expect_true(s$attains_omega_SI)
  # ONE step is what the theory asks for; iterating adds nothing
  expect_equal(s$theory_requires_steps, 1L)
  expect_lt(abs(s$beta[2] - (-0.6)), abs(-0.2 - (-0.6)))
  two <- morie_one_step_efficient(f$x, y, initial_estimator = c(1, -0.2),
                                  n_steps = 2L)
  expect_lt(abs(two$beta[2] - s$beta[2]), abs(s$beta[2] - (-0.2)))
  expect_error(morie_one_step_efficient(f$x, y, initial_estimator = c(0, 1)),
               "nonzero first coefficient")
})

test_that("morie_direct_discrete matches morie.fn.hrzdiscd", {
  f <- hrz8_fix(600L)
  zz <- floor(f$u[1801:2400] * 3)
  y <- as.numeric(tanh(f$x %*% f$beta + zz * 0.8) + 0.2 * f$z[1201:1800])
  d <- morie_direct_discrete(f$x, y, zz)
  expect_equal(d$beta[2], -0.5828041404832675, tolerance = 1e-9)
  expect_equal(d$alpha[1], 0.8583849173377388, tolerance = 1e-8)
  expect_equal(d$J, c(0.836825230106061, 1.4778446279757214,
                      2.2443559123114807), tolerance = 1e-9)
  expect_equal(d$c0, 0.06370545361079727, tolerance = 1e-9)
  expect_equal(d$c1, 0.8689574314762648, tolerance = 1e-9)
  expect_true(d$identified)
  expect_equal(d$dz, 1L)
  expect_equal(d$weights[[1]], 0.2966666666666667, tolerance = 1e-12)
  # the section's reason for existing: no derivative in a discrete
  # covariate exists, so alpha cannot come from an average derivative
  expect_false(d$average_derivative_can_estimate_alpha)
  # a shift in z shifts G horizontally, so J moves monotonically with
  # z when alpha > 0
  expect_true(all(diff(d$J) > 0))
  expect_lt(abs(d$beta[2] - (-0.6)), 0.2)
  expect_lt(abs(d$alpha[1] - 0.8), 0.2)
  expect_equal(d$beta_source, "stratum-wise (2.46)")

  # both routes are available: supplying beta skips (2.46) and goes
  # straight to alpha
  sup <- morie_direct_discrete(f$x, y, zz, beta = c(1, -0.6))
  expect_equal(sup$beta, c(1, -0.6))
  expect_equal(sup$beta_source, "supplied")
  expect_null(sup$delta_by_stratum)
  expect_lt(abs(sup$alpha[1] - 0.8), 0.25)

  expect_error(morie_direct_discrete(f$x, y, rep(1, 600)), "single value")
})
