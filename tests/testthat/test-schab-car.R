# SPDX-License-Identifier: AGPL-3.0-or-later
# CAR model, R side. The reference values are the Python arm's output on
# byte-identical deterministic input (no RNG on either side), so this is
# the R leg of three-way parity as well as a contract test.
#
# Schabenberger & Gotway (2005), Sec 6.2.2.2, eqs (6.43)-(6.45).

fixture <- function() {
  n <- 24
  W <- matrix(0, n, n)
  for (i in 1:(n - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }
  for (i in seq(1, n - 4, by = 4)) { W[i, i + 4] <- 1; W[i + 4, i] <- 1 }
  list(W = W,
       z = sapply(0:(n - 1), function(i) sin(0.7 * i) + 0.3 * cos(0.31 * i)),
       X = cbind(1, (0:(n - 1)) / n))
}

test_that("CAR with an intercept matches the Python arm exactly", {
  f <- fixture()
  r <- sgcar(f$z, f$W)
  expect_equal(r$statistic, 0.854827586206896, tolerance = 1e-12)
  expect_equal(r$extra$tau2, 0.802224912178700, tolerance = 1e-12)
  expect_equal(r$extra$beta, 0.121849035712408, tolerance = 1e-12)
  expect_identical(r$name, "conditional_autoregressive")
})

test_that("CAR with covariates matches the Python arm exactly", {
  f <- fixture()
  r <- sgcar(f$z, f$W, f$X)
  expect_equal(r$statistic, 0.854827586206896, tolerance = 1e-12)
  expect_equal(r$extra$tau2, 0.800142823991780, tolerance = 1e-12)
  expect_equal(r$extra$beta,
               c(0.001943970739632, 0.256204145188253), tolerance = 1e-12)
})

test_that("rho stays inside the searched interval", {
  f <- fixture()
  r <- sgcar(f$z, f$W)
  expect_gt(r$statistic, 0)
  expect_lt(r$statistic, 1)
})

test_that("tau2 is a positive variance", {
  f <- fixture()
  expect_gt(sgcar(f$z, f$W)$extra$tau2, 0)
})

test_that("spcar delegates to sgcar", {
  f <- fixture()
  a <- spcar(f$z, f$W)
  b <- sgcar(f$z, f$W)
  expect_identical(a$statistic, b$statistic)
  expect_equal(a$extra$beta, b$extra$beta, tolerance = 1e-15)
  expect_equal(a$extra$tau2, b$extra$tau2, tolerance = 1e-15)
  # and it forwards covariates rather than dropping them
  expect_equal(spcar(f$z, f$W, f$X)$extra$beta,
               sgcar(f$z, f$W, f$X)$extra$beta, tolerance = 1e-15)
  expect_false(isTRUE(all.equal(spcar(f$z, f$W, f$X)$extra$beta,
                                spcar(f$z, f$W)$extra$beta)))
})

test_that("CAR input validation", {
  f <- fixture()
  expect_error(sgcar(f$z[-1], f$W), "to match `Z`")
  expect_error(sgcar(f$z, f$W, f$X[-1, ]), "one row per element")
})
