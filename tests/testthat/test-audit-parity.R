# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the audit's survival, mediation, entropy and
# instrumental-variable modules. Anchors are the values printed by the
# Python core to full double precision.
#
# The fixture uses a linear congruential generator with exact integer
# arithmetic and a division by a power of two, so R doubles and Python
# integers agree bit for bit. No qnorm anywhere in the fixture.

audit_fixture <- function(n = 200L, seed = 20260728) {
  s <- seed
  nxt <- function() {
    s <<- (1664525 * s + 1013904223) %% 4294967296
    (s + 0.5) / 4294967296
  }
  T <- numeric(n); C <- numeric(n)
  for (i in seq_len(n)) {
    T[i] <- 1 + floor(20 * nxt())
    C[i] <- 1 + floor(25 * nxt())
  }
  X <- matrix(0, 150L, 2L)
  for (i in seq_len(150L)) X[i, ] <- c(2 * nxt() - 1, 2 * nxt() - 1)
  list(T = T, C = C, time = pmin(T, C),
       event = as.integer(T <= C), X = X)
}

test_that("the fixture is bit-identical across languages", {
  fx <- audit_fixture()
  expect_equal(sum(fx$T), 2084)
  expect_equal(sum(fx$C), 2531)
  expect_equal(sum(fx$event), 131L)
})

test_that("native Kaplan-Meier matches the Python core exactly", {
  fx <- audit_fixture()
  out <- morie_km_native(fx$time, fx$event)
  expect_equal(out$survival[1], 0.95999999999999996, tolerance = 1e-12)
  expect_equal(out$survival[6], 0.68726169732903053, tolerance = 1e-12)
  expect_equal(out$se[6], 0.035024901773551512, tolerance = 1e-10)
  expect_equal(out$ci_lower[6], 0.61293828356010593, tolerance = 1e-10)
  expect_equal(out$median, 9)
  expect_equal(out$rmst, 9.8233276807667131, tolerance = 1e-10)
})

test_that("the native estimator agrees with the survival-package wrapper", {
  # morie_survival_km calls survival::survfit. The native one must agree
  # with it, which is the check that replacing the dependency changed
  # nothing about the answer.
  skip_if_not_installed("survival")
  fx <- audit_fixture()
  native <- morie_km_native(fx$time, fx$event)
  fit <- survival::survfit(
    survival::Surv(fx$time, fx$event) ~ 1, error = "greenwood"
  )
  keep <- fit$n.event > 0
  expect_equal(as.numeric(fit$surv[keep]), native$survival,
               tolerance = 1e-10)
  expect_equal(as.numeric(fit$std.err[keep] * fit$surv[keep]),
               native$se, tolerance = 1e-8)
})

test_that("survival is monotone and the log-log interval stays in [0,1]", {
  fx <- audit_fixture()
  out <- morie_km_native(fx$time, fx$event)
  expect_true(all(diff(out$survival) <= 1e-12))
  expect_gte(min(out$ci_lower), 0)
  expect_lte(max(out$ci_upper), 1)
})

test_that("censored observations leave the risk set without a drop", {
  out <- morie_km_native(c(1, 2, 3), c(1, 0, 1))
  expect_equal(out$times, c(1, 3))
  expect_equal(out$n_censored, 1)
})

test_that("Kaplan-Meier recovers the exponential truth", {
  set.seed(11)
  n <- 4000L
  T <- stats::rexp(n, 1 / 10)
  C <- stats::rexp(n, 1 / 20)
  out <- morie_km_native(pmin(T, C), as.integer(T <= C))
  expect_lt(abs(out$median - log(2) * 10), 0.6)
  i <- max(which(out$times <= 10))
  expect_lt(abs(out$survival[i] - exp(-1)), 0.03)
})

test_that("c - c' equals ab exactly for a continuous OLS outcome", {
  set.seed(3)
  n <- 500L
  X <- stats::rnorm(n)
  M <- 0.6 * X + stats::rnorm(n)
  Y <- 0.3 * X + 0.5 * M + stats::rnorm(n)
  a <- stats::coef(stats::lm(M ~ X))[["X"]]
  fit <- stats::coef(stats::lm(Y ~ X + M))
  cprime <- fit[["X"]]; b <- fit[["M"]]
  cc <- stats::coef(stats::lm(Y ~ X))[["X"]]
  out <- morie_mediation_difference(cc, cprime, a = a, b = b)
  # an algebraic identity in every sample, not merely in expectation
  expect_lt(out$identity_residual, 1e-12)
  expect_true(out$matches_product)
})

test_that("the product estimator recovers the design", {
  set.seed(5)
  ests <- vapply(seq_len(30L), function(i) {
    n <- 800L
    X <- stats::rnorm(n)
    M <- 0.6 * X + stats::rnorm(n)
    Y <- 0.3 * X + 0.5 * M + stats::rnorm(n)
    a <- stats::coef(stats::lm(M ~ X))[["X"]]
    b <- stats::coef(stats::lm(Y ~ X + M))[["M"]]
    morie_mediation_product(a, b)$indirect
  }, numeric(1))
  expect_lt(abs(mean(ests) - 0.30), 0.03)
})

test_that("the Sobel interval is symmetric by construction", {
  out <- morie_mediation_product(0.5, 0.4, se_a = 0.1, se_b = 0.1)
  expect_true(out$sobel_symmetric)
  expect_equal(out$indirect - out$sobel_ci[1],
               out$sobel_ci[2] - out$indirect, tolerance = 1e-12)
})

test_that("k-NN entropy matches the Python core exactly", {
  fx <- audit_fixture()
  out <- morie_knn_entropy(fx$X, k = 3L)
  expect_equal(out$entropy, 1.5110887074754791, tolerance = 1e-10)
  expect_equal(out$distance_concentration, 0.36242621392618102,
               tolerance = 1e-10)
})

test_that("k-NN entropy recovers the Gaussian value", {
  set.seed(7)
  x <- matrix(stats::rnorm(3000), ncol = 1L)
  expect_lt(abs(morie_knn_entropy(x, k = 4L)$entropy -
                  0.5 * log(2 * pi * exp(1))), 0.06)
})

test_that("differential entropy shifts by exactly log(a) under rescaling", {
  set.seed(9)
  x <- matrix(stats::rnorm(2000), ncol = 1L)
  h1 <- morie_knn_entropy(x, k = 4L)$entropy
  h2 <- morie_knn_entropy(x * 5, k = 4L)$entropy
  # not scale invariant, which is why it is not comparable across units
  expect_equal(h2 - h1, log(5), tolerance = 1e-9)
})

test_that("the Wald standard error keeps the covariance term", {
  # Y and D are measured on the same subjects, so the reduced form and
  # first stage are correlated. Dropping Cov(num, den) -- which most
  # textbook formulas do -- inflated the SE by 25 % on this design.
  set.seed(13)
  n <- 4000L
  z <- stats::rbinom(n, 1L, 0.5)
  u <- stats::rnorm(n)
  d <- stats::rbinom(n, 1L, stats::plogis(0.9 * z + 0.5 * u))
  y <- 0.7 * d + 0.6 * u + stats::rnorm(n, sd = 0.5)
  out <- morie_iv_wald(data.frame(y = y, d = d, z = z), "y", "d", "z")

  num <- mean(y[z == 1]) - mean(y[z == 0])
  den <- mean(d[z == 1]) - mean(d[z == 0])
  n1 <- sum(z == 1); n0 <- sum(z == 0)
  v_y <- stats::var(y[z == 1]) / n1 + stats::var(y[z == 0]) / n0
  v_d <- stats::var(d[z == 1]) / n1 + stats::var(d[z == 0]) / n0
  naive <- sqrt(v_y / den^2 + (num^2 / den^4) * v_d)
  se <- as.numeric(out$std_errors[["LATE"]])
  expect_lt(se, naive)
  expect_gt(naive / se, 1.1)
})
