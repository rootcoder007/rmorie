# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("party unity flags the defector and supports the CQ variant", {
  V <- rbind(c(1, 1), c(1, 1), c(1, 0), c(0, 0), c(0, 0))
  pid <- c("a", "a", "a", "b", "b")
  out <- morie_party_unity(V, pid)
  expect_equal(out$unity[3], 0.5)
  expect_equal(out$by_party$b, 1)
  cq <- morie_party_unity(V, pid, unity_votes_only = TRUE)
  expect_equal(cq$n_votes_scored[1], 2L)
  expect_error(morie_party_unity(V, pid[1:2]), "one entry")
})

test_that("heteroskedastic scales single out the noisy voter", {
  set.seed(42)
  n <- 25; q <- 70
  x <- seq(-2, 2, length.out = n)
  beta <- stats::rnorm(q, sd = 1.2)
  alpha <- stats::rnorm(q, sd = 0.5)
  psi_true <- rep(1, n); psi_true[1] <- 4
  P <- stats::pnorm(sweep(outer(x, beta), 2, alpha) / psi_true)
  V <- matrix(as.numeric(stats::runif(n * q) < P), n, q)
  out <- morie_heteroskedastic_scales(V, x, alpha, beta)
  expect_equal(which.max(out$psi), 1L)
  expect_gt(out$psi[1], 1.5 * stats::median(out$psi))
  expect_equal(exp(mean(log(out$psi))), 1, tolerance = 1e-8)
  expect_error(morie_heteroskedastic_scales(V, x[1:3], alpha, beta), "one entry")
  expect_error(morie_heteroskedastic_scales(V * 2, x, alpha, beta), "binary")
})
