# Cross-language anchors from morie.fn._kosorok / ksr0xx at full
# precision (testthat tolerance is relative).

test_that("bridge covariance matches Python and is tied down", {
  expect_equal(morie_bridge_covariance(0.3, 0.7)$covariance, 0.09,
    tolerance = 1e-12
  )
  expect_equal(morie_bridge_covariance(0.5, 0.5)$covariance, 0.25,
    tolerance = 1e-12
  )
  expect_equal(morie_bridge_covariance(0, 0.5)$covariance, 0)
  expect_equal(morie_bridge_covariance(1, 0.5)$covariance, 0)
})

test_that("sup norm is taken at the jumps, not on a grid", {
  set.seed(3)
  x <- runif(500)
  s <- morie_empirical_sup_norm(x)
  grid <- seq(0, 1, length.out = 50)
  Fn <- vapply(grid, function(t) mean(x <= t), 0)
  grid_sup <- sqrt(500) * max(abs(Fn - grid))
  expect_gte(s, grid_sup - 1e-9) # the exact sup can only be larger
  expect_error(morie_empirical_sup_norm(numeric(0)))
})

test_that("LIL reports the finite-n gap and Chung's constant", {
  set.seed(4)
  out <- morie_lil_ratio(runif(5000))
  expect_equal(out$lil_bound, 0.5)
  expect_equal(out$chung_liminf_constant, pi / 2)
  expect_true(out$lil_ratio > 0 && out$lil_ratio < 0.5)
  expect_equal(out$loglog_term, sqrt(2 * log(log(5000))), tolerance = 1e-12)
  expect_error(morie_lil_ratio(runif(4)))
})

test_that("KMT refuses to invent its universal constants", {
  expect_error(morie_kmt_bound(1000, x = 1), "universal")
  out <- morie_kmt_bound(1000, x = 2, a = 1, b = 1, c = 1)
  expect_equal(out$probability_bound, exp(-2), tolerance = 1e-12)
  expect_equal(out$threshold, (log(1000) + 2) / sqrt(1000), tolerance = 1e-12)
  expect_lt(morie_kmt_bound(1e5, x = 2, a = 1, b = 1, c = 1)$threshold,
            out$threshold)
})

test_that("U-statistic matches brute force and reports dependence", {
  set.seed(8)
  x <- runif(40)
  out <- morie_u_process(function(a, b) abs(a - b) / 2, x)
  idx <- utils::combn(40, 2)
  brute <- mean(abs(x[idx[1, ]] - x[idx[2, ]]) / 2)
  expect_equal(out$U, brute, tolerance = 1e-12)
  expect_equal(out$n_subsets, 780L)
  expect_gt(out$zeta1, 0) # summands are dependent
  expect_error(morie_u_process(function(a, b) a, x, m = 3L))
})

test_that("entropy integral separates polynomial from exponential growth", {
  poly <- morie_entropy_integral(function(e) (1 / e)^3)
  expect_true(poly$finite)
  expect_equal(poly$J, 1.5349900612293, tolerance = 1e-4)
  # GC and Donsker key on DIFFERENT envelope moments
  gc_bad <- morie_entropy_integral(function(e) (1 / e)^2, envelope_mean = Inf)
  expect_false(gc_bad$gc_conditions_met)
  d_ok <- morie_entropy_integral(function(e) (1 / e)^2, envelope_sq_mean = 3)
  expect_true(d_ok$donsker_conditions_met)
  expect_error(morie_entropy_integral(function(e) 2, delta = 0))
})

test_that("delta method shows its remainder and Frechet is not vacuous", {
  out <- morie_functional_delta(function(z) z^2, 2.01, 2, 100)
  # the DIRECTIONAL derivative, matching morie.fn.ksr042: 4 * 0.01
  expect_equal(out$derivative, 0.04, tolerance = 1e-4)
  expect_equal(out$jacobian, 4, tolerance = 1e-4) # d/dz z^2 at 2
  expect_lt(abs(out$remainder), 0.02)
  sm <- morie_frechet_check(function(z) z^2, 1, list(0.1, 0.05, 0.01))
  expect_true(sm$ratio_shrinking)
  # |.| at 0 is Hadamard but NOT Frechet: a single Jacobian exposes it
  kink <- morie_frechet_check(function(z) abs(z), 0, list(0.1, 0.05, 0.01))
  expect_gt(min(kink$ratios), 0.9)
  expect_error(morie_functional_delta(function(z) z, 1, 1, 0))
})

test_that("Kaplan-Meier derivative and quantile sandwich behave", {
  d <- morie_km_hadamard(function(u) exp(-0.5 * u), function(u) exp(-0.3 * u),
                         function(u) u, function(u) 1, 1)
  expect_lt(d$derivative, 0)
  expect_equal(d$boundary_term, exp(-0.3), tolerance = 1e-10)
  expect_error(morie_km_hadamard(function(u) 1, function(u) 1, function(u) u,
                                 function(u) 1, -1))
  q <- morie_quantile_hadamard(stats::pnorm,
                               function(z) 0.1 * stats::dnorm(z), 0.01, 0.7)
  expect_true(q$sandwich_holds)
  expect_equal(q$xi_perturbed, stats::qnorm(0.7), tolerance = 0.05)
  expect_error(morie_quantile_hadamard(stats::pnorm, function(z) 0, 0.01, 1.5))
})

test_that("DQM, BL distance and tightness separate the right cases", {
  dq <- morie_dqm_check(function(x, th) stats::dnorm(x, th), function(x) x)
  expect_true(dq$shrinking)
  expect_equal(dq$score_mean, 0, tolerance = 1e-6)
  bad <- morie_dqm_check(function(x, th) stats::dnorm(x, th), function(x) 5)
  expect_gt(bad$dqm_integrals[length(bad$dqm_integrals)],
            dq$dqm_integrals[length(dq$dqm_integrals)] * 100)
  set.seed(5)
  same <- morie_bl_distance(rnorm(2000), rnorm(2000))$bl_distance
  shifted <- morie_bl_distance(rnorm(2000), rnorm(2000) + 2)$bl_distance
  expect_gt(shifted, same * 3)
  # tightness: smooth paths yes, shrinking spikes no
  set.seed(6)
  g <- seq(0, 1, length.out = 40)
  smooth <- t(vapply(1:150, function(i) sin(2 * pi * g + runif(1) * 6), numeric(40)))
  expect_true(morie_tightness_check(smooth, g, eps = 0.3)$decreasing)
  expect_error(morie_tightness_check(smooth, g, eps = 0))
})
