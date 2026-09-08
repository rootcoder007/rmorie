# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the design-based survey shelf. Anchors
# from the Python modules at full double precision; LCG fixture so
# both languages see identical bits.

sv_fix <- function(n = 100L, seed = 8642) {
  m <- 3L * n
  u <- numeric(m)
  s <- seed
  for (k in seq_len(m)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[k] <- (s + 0.5) / 4294967296
  }
  z <- stats::qnorm(u[1:n])
  list(u = u, z = z, n = n, y = 10 + 2 * z,
       pi = 0.05 + 0.4 * u[(n + 1):(2 * n)],
       x = 1 + u[(2 * n + 1):(3 * n)] * 9)
}

test_that("morie_horvitz_thompson and morie_hajek_mean match Python", {
  f <- sv_fix()
  ht <- morie_horvitz_thompson(f$y, f$pi)
  expect_equal(ht$total, 5272.30491600028, tolerance = 1e-10)
  expect_equal(ht$mean, 9.934229030940134, tolerance = 1e-12)
  expect_true(ht$design_unbiased)
  expect_false(ht$uses_known_N)
  hj <- morie_hajek_mean(f$y, f$pi)
  expect_equal(hj$mean, 9.934229030940134, tolerance = 1e-12)
  expect_false(hj$design_unbiased)
  # HT's mean divides by the ESTIMATED N, which is what Hajek does
  expect_equal(hj$mean, ht$mean)
  expect_error(morie_horvitz_thompson(f$y, rep(0, f$n)), "\\(0, 1\\]")
})

test_that("morie_ratio_estimator reports its efficiency threshold", {
  f <- sv_fix()
  ra <- morie_ratio_estimator(f$y, f$x, X_mean = 5.5)
  expect_equal(ra$ratio, 1.6385797121865517, tolerance = 1e-10)
  expect_equal(ra$mean, 9.012188417026035, tolerance = 1e-10)
  expect_equal(ra$efficiency_threshold, 1.1211029431887072, tolerance = 1e-10)
  expect_equal(ra$correlation, 0.00047791811793750235, tolerance = 1e-9)
  # y and x are unrelated here, so the auxiliary does NOT earn its place
  expect_false(ra$improves_on_simple_mean)
  expect_true(ra$biased)
  expect_error(morie_ratio_estimator(f$y, f$x), "population total or mean")
})

test_that("morie_regression_estimator fits an intercept", {
  f <- sv_fix()
  rg <- morie_regression_estimator(f$y, f$x, X_mean = 5.5)
  expect_equal(rg$mean, 9.957813054571329, tolerance = 1e-10)
  expect_equal(rg$slope, 0.00034925737056377265, tolerance = 1e-9)
  expect_equal(rg$intercept, 9.955892139033228, tolerance = 1e-10)
  expect_equal(rg$variance_ratio_to_simple_mean, 0.9999997715942726,
               tolerance = 1e-12)
  # the intercept is far from zero: the ratio estimator's origin
  # assumption would be badly wrong here
  expect_false(rg$passes_through_origin)
  expect_equal(rg$variance_ratio_to_simple_mean, 1 - rg$correlation^2,
               tolerance = 1e-12)
})

test_that("morie_greg and morie_calibration_chi2 agree, as they must", {
  f <- sv_fix()
  X <- cbind(1, f$x)
  w <- rep(20, f$n)
  totals <- c(2000, 11000)
  gg <- morie_greg(f$y, X, w, totals)
  expect_equal(gg$total, 19915.62610914268, tolerance = 1e-8)
  expect_equal(gg$ht_total, 19916.02930806193, tolerance = 1e-8)
  expect_equal(gg$correction, -0.4031989192507279, tolerance = 1e-8)
  expect_true(gg$design_consistent_regardless_of_model)

  cc <- morie_calibration_chi2(f$y, X, w, totals)
  expect_equal(cc$total, 19915.626109142664, tolerance = 1e-8)
  expect_equal(cc$max_margin_error, 3.637978807091713e-12, tolerance = 1e-6)
  expect_equal(cc$n_negative, 0L)
  expect_equal(cc$calibrated_weights[1], 25.64351434081349, tolerance = 1e-10)
  # chi-square calibration reproduces GREG exactly: one adjusts the
  # weights, the other the estimate
  expect_equal(cc$total, gg$total, tolerance = 1e-8)
  expect_true(cc$margins_reproduced)
})

test_that("morie_stratified_proportion has no between-stratum term", {
  f <- sv_fix()
  yb <- as.numeric(f$u[1:f$n] < 0.3)
  st <- as.numeric(seq_len(f$n) <= 50)
  sp <- morie_stratified_proportion(yb, st, weights = c(0.5, 0.5))
  expect_equal(sp$proportion, 0.27, tolerance = 1e-12)
  expect_equal(sp$se, 0.0448352995042149, tolerance = 1e-10)
  expect_true(sp$weights_are_population_shares)
  expect_error(morie_stratified_proportion(yb, st, weights = c(0.5, 0.9)),
               "sum to 1")
})

test_that("morie_cluster_variance counts clusters, not elements", {
  f <- sv_fix()
  cl <- rep(1:20, each = 5)
  cv <- morie_cluster_variance(f$y, cl)
  expect_equal(cv$variance, 0.030030356586298667, tolerance = 1e-10)
  expect_equal(cv$icc, -0.040365833154258096, tolerance = 1e-9)
  expect_equal(cv$deff, 0.8385366673829676, tolerance = 1e-10)
  expect_equal(cv$n_clusters, 20L)
  expect_equal(cv$n_elements, 100L)
  # this fixture has NO real cluster structure, so the icc is near
  # zero and the design effect is about one -- the estimator does not
  # invent clustering that is not there
  expect_lt(abs(cv$icc), 0.1)
  expect_error(morie_cluster_variance(f$y, rep(1, f$n)), "at least 2 clusters")
})

test_that("morie_actuarial_lifetable applies the half-withdrawal rule", {
  lt <- morie_actuarial_lifetable(c(0, 1, 2, 3), c(100, 70, 40),
                                  c(20, 20, 10), c(10, 10, 5))
  expect_equal(lt$q[1], 0.21052631578947367, tolerance = 1e-12)
  expect_equal(lt$survival[3], 0.4008097165991903, tolerance = 1e-10)
  expect_equal(lt$effective_n[2], 65)
  # effective exposure is n - w/2, exactly
  expect_equal(lt$effective_n, c(100, 70, 40) - c(10, 10, 5) / 2)
  expect_true(all(diff(lt$survival) <= 0))
  expect_error(morie_actuarial_lifetable(c(0, 2, 1), c(1, 1), c(0, 0)),
               "strictly increasing")
})
