# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the extreme-value shelf, anchored to
# full-precision Python values on shared LCG fixtures. The exact
# oracles (Pareto xi = 1/alpha, max-AR theta = 1 - alpha, the
# independence and comonotone Pickands functions) are asserted on top
# of the anchors.

evt_unif <- function(n, s = 333) {
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[i] <- (s + 0.5) / 4294967296
  }
  u
}

evt_maxar <- function(alpha, n, s) {
  z <- (1 - evt_unif(n, s))^(-1)
  x <- numeric(n)
  x[1] <- z[1]
  for (t in 2:n) x[t] <- max(alpha * x[t - 1], (1 - alpha) * z[t])
  x
}

test_that("the fixture matches the one Python anchored against", {
  expect_equal(evt_unif(3), c(0.3651229311944917, 0.48292070801835507,
                              0.8273884492227808), tolerance = 1e-12)
})

test_that("Hill, Pickands and DEdH match morie.fn on the Pareto fixture", {
  u <- evt_unif(4000L)
  par <- (1 - u[1:2000])^(-1 / 4)      # Pareto, xi = 0.25
  h <- morie_evt_hill(par, k = 200)
  expect_equal(h$xi, 0.2354201234067287, tolerance = 1e-10)
  expect_equal(h$threshold, 1.801341285132658, tolerance = 1e-10)
  expect_equal(h$se, h$xi / sqrt(200), tolerance = 1e-12)
  p <- morie_evt_pickands(par, k = 300)
  expect_equal(p$xi, 0.2471537198685872, tolerance = 1e-10)
  d <- morie_evt_dedh(par, k = 200)
  expect_equal(d$xi, 0.20742565472425661, tolerance = 1e-10)
  expect_equal(d$M1, 0.23542012340672863, tolerance = 1e-10)
  expect_equal(d$M2, 0.1079067397295947, tolerance = 1e-10)
  # DEdH's first term IS the Hill estimator -- structure, not luck
  expect_equal(d$hill_part, h$xi, tolerance = 1e-12)
})

test_that("the Hill plot agrees with per-k calls and the alias shares code", {
  u <- evt_unif(1000L, 11)
  par <- (1 - u)^(-1 / 3)
  o <- morie_evt_hill(par)
  for (kp in c(10L, 50L, 200L)) {
    i <- which(o$hill_plot_k == kp)
    expect_equal(o$hill_plot_xi[i], morie_evt_hill(par, k = kp)$xi,
                 tolerance = 1e-10)
  }
  a <- morie_evt_hill_alias(par, k = 100)
  expect_equal(a$xi, morie_evt_hill(par, k = 100)$xi, tolerance = 1e-15)
  expect_equal(a$alias_of, "morie_evt_hill")
  expect_error(morie_evt_hill(par - 10, k = 50), "not positive")
})

test_that("Pickands and DEdH handle every sign of xi", {
  u <- evt_unif(20000L, 21)
  bounded <- u + 1                       # uniform: xi = -1
  light <- -log(1 - evt_unif(20000L, 23)) + 1   # exponential: xi = 0
  expect_equal(morie_evt_pickands(bounded, k = 1000)$xi, -1,
               tolerance = 0.2)
  expect_lt(abs(morie_evt_pickands(light, k = 1000)$xi), 0.2)
  expect_equal(morie_evt_dedh(bounded, k = 1000)$xi, -1, tolerance = 0.15)
  expect_lt(abs(morie_evt_dedh(light, k = 500)$xi), 0.2)
})

test_that("the GEV L-moment fit matches Python and recovers the truth", {
  u <- evt_unif(4000L)
  k <- -0.3
  g <- 10 + 2 / k * (1 - (-log(u[1:1500]))^k)   # GEV(10, 2, xi = 0.3)
  o <- morie_evt_gev_lmoments(g)
  expect_equal(o$mu, 9.966440260377, tolerance = 1e-9)
  expect_equal(o$sigma, 2.0661795069837576, tolerance = 1e-9)
  expect_equal(o$xi, 0.2965860861641094, tolerance = 1e-9)
  expect_equal(o$k_hosking, -o$xi, tolerance = 1e-12)
  expect_equal(o$return_level_fn(100), 30.26095687670403, tolerance = 1e-8)
  expect_true(startsWith(o$tail_type, "Frechet"))
  # PWM fit == L-moment fit exactly
  p <- morie_evt_gev_pwm(g)
  expect_equal(p$xi, o$xi, tolerance = 1e-14)
  expect_equal(p$alias_of, "morie_evt_gev_lmoments")
})

test_that("the GPD PWM fit matches Python and flags xi >= 0.5", {
  u <- evt_unif(4000L)
  e <- 1.5 / 0.25 * ((1 - u[2001:4000])^(-0.25) - 1)   # GPD(1.5, xi=.25)
  o <- morie_evt_gpd_pwm(e)
  expect_equal(o$sigma, 1.4483785734886567, tolerance = 1e-10)
  expect_equal(o$xi, 0.2608801696346754, tolerance = 1e-10)
  expect_true(o$reliable)
  # threshold path forms excesses identically
  o2 <- morie_evt_gpd_pwm(10 + e, threshold = 10)
  expect_equal(o2$xi, o$xi, tolerance = 1e-12)
  expect_equal(o2$n_excesses, length(e))
  # infinite-variance regime is flagged, not silently returned
  heavy <- 1 / 0.8 * ((1 - evt_unif(2000L, 41))^(-0.8) - 1)
  expect_false(morie_evt_gpd_pwm(heavy)$reliable)
  expect_error(morie_evt_gpd_pwm(c(-1, rep(1, 20))), "non-negative")
})

test_that("the extremal-index estimators match Python on the max-AR fixture", {
  x <- evt_maxar(0.5, 3000L, 77)
  u <- stats::quantile(x, 0.95, names = FALSE, type = 7L)
  r <- morie_evt_extremal_runs(x, u)
  expect_equal(r$theta, 0.44, tolerance = 1e-10)
  expect_equal(r$n_exceedances, 150L)
  expect_equal(r$n_clusters, 66L)
  expect_equal(r$mean_cluster_size, 1 / r$theta, tolerance = 1e-12)
  i <- morie_evt_extremal_intervals(x, u)
  expect_equal(i$theta, 0.3423141455859577, tolerance = 1e-10)
  expect_true(startsWith(i$form_used, "Eq. (34)"))
  s <- morie_evt_extremal_sliding(x, block_length = 50)
  expect_equal(s$theta, 0.3881820136626293, tolerance = 1e-10)
  expect_equal(s$theta_disjoint, 0.3558384309417256, tolerance = 1e-10)
})

test_that("independent data have extremal index one", {
  x <- evt_unif(20000L, 51)
  u <- stats::quantile(x, 0.98, names = FALSE, type = 7L)
  # the intervals estimator's own sampling error at ~400 exceedances
  # is a few percent; 0.9 is the honest bound for a single draw
  expect_gt(morie_evt_extremal_intervals(x, u)$theta, 0.9)
  expect_gt(morie_evt_extremal_runs(x, u)$theta, 0.9)
  expect_error(morie_evt_extremal_runs(x, max(x) + 1), "lower the threshold")
})

test_that("the madogram matches Python and identifies both extremes", {
  a <- evt_unif(500L, 11)
  b <- evt_unif(500L, 22)
  md <- morie_evt_madogram(a, b, t = c(0.25, 0.5, 0.75))
  expect_equal(md$A, c(1, 0.9982107566743397, 0.978939298409774),
               tolerance = 1e-10)
  expect_equal(md$dependence_summary, 0.003578486651320656,
               tolerance = 1e-10)
  # complete dependence: A = max(t, 1 - t), summary 1
  com <- morie_evt_madogram(a, a, t = c(0.25, 0.5, 0.75))
  expect_equal(com$A, pmax(c(0.25, 0.5, 0.75), c(0.75, 0.5, 0.25)),
               tolerance = 0.05)
  expect_equal(com$dependence_summary, 1, tolerance = 0.1)
  # the envelope always holds
  lower <- pmax(md$t, 1 - md$t)
  expect_true(all(md$A >= lower - 1e-12 & md$A <= 1 + 1e-12))
  expect_error(morie_evt_madogram(a, b, t = c(0, 0.5)), "strictly in")
})

test_that("validation errors fire", {
  expect_error(morie_evt_hill(1:5), "at least 10")
  expect_error(morie_evt_pickands(1:4), "at least 8")
  expect_error(morie_evt_pickands(1:100, k = 30), "4k <= n")
  expect_error(morie_evt_extremal_sliding(1:30), "at least 40")
  expect_error(morie_evt_madogram(1:10, 1:10), "at least 20")
})
