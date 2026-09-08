# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the partial-identification shelf.
# Anchors printed from the Python modules at full double precision --
# testthat tolerances are RELATIVE, so a rounded anchor silently
# weakens the test. The fixture is a shared linear congruential
# generator, so both languages see identical numbers.

bnd_unif <- function(n, s = 888) {
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[i] <- (s + 0.5) / 4294967296
  }
  u
}

test_that("the fixture matches the one Python anchored against", {
  u <- bnd_unif(3)
  expect_equal(u, c(0.580214528250508, 0.823510373593308,
                    0.8404795977985486), tolerance = 1e-12)
  z <- stats::qnorm(bnd_unif(3, 99))
  expect_equal(z, c(-0.5994522748013467, -0.6487819596981876,
                    0.2680061867277303), tolerance = 1e-12)
})

test_that("morie_bnd_manski matches morie.fn.bndest", {
  u <- bnd_unif(1200L)
  y <- u[1:400]
  obs <- u[401:800] < 0.8
  o <- morie_bnd_manski(y, obs, c(0, 1))
  expect_equal(o$lower, 0.41543405205622547, tolerance = 1e-12)
  expect_equal(o$upper, 0.6129340520562254, tolerance = 1e-12)
  expect_equal(o$p_observed, 0.8025, tolerance = 1e-12)
  # the width identity is EXACT: (K1 - K0)(1 - P(obs))
  expect_equal(o$width, 1 * (1 - o$p_observed), tolerance = 1e-12)
  expect_false(o$identified)
  # fully observed data collapse to the sample mean
  full <- morie_bnd_manski(y, rep(TRUE, 400L), c(0, 1))
  expect_true(full$identified)
  expect_equal(full$lower, mean(y), tolerance = 1e-12)
  expect_equal(full$lower, full$upper, tolerance = 1e-12)
})

test_that("no-assumption ATE bounds always contain zero", {
  # Manski (1990): the worst-case ATE interval has width exactly
  # K1 - K0, so it can never sign an effect on its own. Bounds that
  # exclude zero have smuggled in an assumption.
  u <- bnd_unif(1200L)
  T <- as.numeric(u[801:1200] < 0.5)
  y2 <- 0.3 * T + 0.4 * u[1:400]
  a <- morie_bnd_manski(y2, NULL, c(0, 0.7), treatment = T)
  expect_equal(a$ate_lower, -0.2029884375436231, tolerance = 1e-12)
  expect_equal(a$ate_upper, 0.49701156245637684, tolerance = 1e-12)
  expect_equal(a$ate_width, 0.7, tolerance = 1e-12)
  expect_true(a$contains_zero)
  expect_true(a$ate_lower < 0 && 0 < a$ate_upper)
})

test_that("morie_bnd_manski validates the support", {
  expect_error(morie_bnd_manski(c(0.5, 1.5), c(TRUE, TRUE), c(0, 1)),
               "outside the declared support")
  expect_error(morie_bnd_manski(c(0.5), TRUE, c(1, 0)), "K0 < K1")
  expect_error(morie_bnd_manski(c(0.5, 0.2), NULL, c(0, 2),
                                treatment = c(0.5, 0.2)), "binary")
})

test_that("morie_bnd_imbens_manski matches Python and hits both z limits", {
  o <- morie_bnd_imbens_manski(0.5, 0.55, 1, 1, 400)
  expect_equal(o$c, 1.6814774423281535, tolerance = 1e-9)
  expect_equal(o$ci, c(0.4159261278835923, 0.6340738721164078),
               tolerance = 1e-9)
  # c = z_{1-a} when the identified set is wide relative to noise,
  # z_{1-a/2} when it collapses to a point -- both limits EXACT
  wide <- morie_bnd_imbens_manski(0.2, 0.8, 1, 1, 400)
  point <- morie_bnd_imbens_manski(0.5, 0.5, 1, 1, 400)
  expect_equal(wide$c, stats::qnorm(0.95), tolerance = 1e-9)
  expect_equal(point$c, stats::qnorm(0.975), tolerance = 1e-9)
  expect_true(wide$c < o$c && o$c < point$c)
  # and monotone decreasing in the set's width
  cs <- vapply(c(0, 0.02, 0.05, 0.2), function(d) {
    morie_bnd_imbens_manski(0.5, 0.5 + d, 1, 1, 400)$c
  }, numeric(1))
  expect_true(all(diff(cs) < 0))
})

test_that("morie_bnd_imbens_manski validates inputs", {
  expect_error(morie_bnd_imbens_manski(0.8, 0.2, 1, 1, 100),
               "at least lower_hat")
  expect_error(morie_bnd_imbens_manski(0.2, 0.8, 0, 1, 100), "positive")
  expect_error(morie_bnd_imbens_manski(0.2, 0.8, 1, 1, 100, alpha = 1.5),
               "alpha")
})

test_that("the CHT criterion is exactly zero on the identified set", {
  # E[L] <= theta <= E[U] with the identified set [1, 3]: deep inside
  # every sample moment is negative, and with the positive-part
  # criterion Q_n is EXACTLY zero -- not small, zero.
  z <- stats::qnorm(bnd_unif(1600L, 7))
  n <- 800L
  L <- 2 - 1 + 0.3 * z[1:n]
  U <- 2 + 1 + 0.3 * z[(n + 1):(2 * n)]
  d <- cbind(L, U)
  g <- function(d, th) cbind(d[, 1] - th, th - d[, 2])
  o <- morie_bnd_moment_inequality(d, g, seq(0, 4, length.out = 81L),
                                   B = 200, seed = 1)
  grid <- o$theta_grid
  inside <- grid > 1.2 & grid < 2.8
  expect_true(all(o$criterion[inside] == 0))
  outside <- grid < 0.5 | grid > 3.5
  expect_true(all(o$criterion[outside] > 0))
  # the set estimate recovers [1, 3] and the confidence set nests it
  est <- o$set_estimate
  expect_equal(min(est), 1, tolerance = 0.15)
  expect_equal(max(est), 3, tolerance = 0.15)
  cs <- o$confidence_set_bounds
  expect_lte(cs[1L], min(est))
  expect_gte(cs[2L], max(est))
})

test_that("the CHT region covers the identified-set boundary", {
  # the hard point for coverage is the BOUNDARY, where the
  # inequality binds; theta = 1 must be inside most of the time
  hits <- 0L
  reps <- 40L
  for (s in seq_len(reps)) {
    z <- stats::qnorm(bnd_unif(800L, 100 + s))
    n <- 400L
    d <- cbind(1 + 0.3 * z[1:n], 3 + 0.3 * z[(n + 1):(2 * n)])
    g <- function(d, th) cbind(d[, 1] - th, th - d[, 2])
    o <- morie_bnd_moment_inequality(d, g, 1, B = 200, seed = s)
    hits <- hits + as.integer(o$in_confidence_set[1L])
  }
  expect_gt(hits / reps, 0.85)
})

test_that("morie_bnd_moment_inequality validates inputs", {
  d <- cbind(stats::qnorm(bnd_unif(20L)), stats::qnorm(bnd_unif(20L, 2)) + 3)
  g <- function(d, th) cbind(d[, 1] - th, th - d[, 2])
  expect_error(morie_bnd_moment_inequality(d, g, 1, alpha = 2), "alpha")
  expect_error(morie_bnd_moment_inequality(d[1:5, ], g, 1), "at least 10")
})

test_that("morie_bnd_lp lands on the exact vertices Python found", {
  o <- morie_bnd_lp(c(1, 1), A_eq = rbind(c(1, 2)), b_eq = 1)
  expect_equal(o$lower, 0.5, tolerance = 1e-9)
  expect_equal(o$upper, 1.0, tolerance = 1e-9)
  expect_equal(o$argmin, c(0, 0.5), tolerance = 1e-9)
  expect_equal(o$argmax, c(1, 0), tolerance = 1e-9)
  expect_true(o$feasible && o$bounded && o$sharp)
})

test_that("LP bounds tighten as restrictions accumulate", {
  # adding an assumption can only shrink the identified set --
  # the monotonicity the MST framework runs on
  free <- morie_bnd_lp(c(1, 1, 1))
  eq <- morie_bnd_lp(c(1, 1, 1), A_eq = rbind(c(1, 1, 0)), b_eq = 0.8)
  both <- morie_bnd_lp(c(1, 1, 1), A_eq = rbind(c(1, 1, 0)), b_eq = 0.8,
                       A_ub = rbind(c(0, 0, 1)), b_ub = 0.3)
  expect_gte(free$width, eq$width)
  expect_gte(eq$width, both$width)
  expect_equal(both$upper, 1.1, tolerance = 1e-9)
})

test_that("LP infeasibility is a specification rejection", {
  o <- morie_bnd_lp(1, A_eq = rbind(1), b_eq = 2, bounds = list(c(0, 1)))
  expect_false(o$feasible)
  expect_true(is.na(o$lower) && is.na(o$upper))
  u <- morie_bnd_lp(1, bounds = list(c(0, NA)))
  expect_true(!u$bounded || is.infinite(u$upper))
  expect_error(morie_bnd_lp(c(1, 1), A_eq = rbind(1), b_eq = 1),
               "inconsistent")
  expect_error(morie_bnd_lp(c(1, 1), bounds = list(c(0, 1))), "bounds has")
})

test_that("morie_bnd_polya_tree matches morie.fn.bndpl", {
  z <- stats::qnorm(bnd_unif(600L, 99))
  o <- morie_bnd_polya_tree(z, grid = c(-1, 0, 1), tree_depth = 6L,
                            alpha = 1, lo = -4, hi = 4)
  expect_equal(o$density,
               c(0.29218940002767874, 0.38427770119125376,
                 0.148656898965039), tolerance = 1e-10)
})

test_that("the Polya tree is a density that tracks the sample", {
  z <- stats::qnorm(bnd_unif(1500L, 11))
  o <- morie_bnd_polya_tree(z, grid = seq(-4, 4, length.out = 400L),
                            tree_depth = 7L, lo = -5, hi = 5)
  expect_equal(o$mass, 1, tolerance = 0.02)
  expect_true(all(o$density >= 0))
  truth <- stats::dnorm(o$grid)
  expect_lt(mean(abs(o$density - truth)), 0.06)
})

test_that("alpha interpolates between base measure and histogram", {
  # large alpha smooths toward the uniform base; small alpha follows
  # the data. Kraft's alpha_m = alpha m^2 is what buys absolute
  # continuity, and the rule is recorded in the output.
  z <- 0.5 * stats::qnorm(bnd_unif(800L, 13))
  g <- seq(-2, 2, length.out = 200L)
  tight <- morie_bnd_polya_tree(z, grid = g, alpha = 0.01, lo = -3, hi = 3)
  loose <- morie_bnd_polya_tree(z, grid = g, alpha = 1e4, lo = -3, hi = 3)
  base <- 1 / 6
  expect_lt(max(abs(loose$density - base)), 0.02)
  expect_gt(tight$density[100L], 3 * base)
  expect_true(startsWith(tight$alpha_rule, "alpha_m"))
})

test_that("morie_bnd_polya_tree validates inputs", {
  z <- stats::qnorm(bnd_unif(50L, 17))
  expect_error(morie_bnd_polya_tree(z, tree_depth = 0L), "tree_depth")
  expect_error(morie_bnd_polya_tree(z, alpha = -1), "alpha must be positive")
  expect_error(morie_bnd_polya_tree(1), "at least 2")
})

test_that("the moment-inequality bootstrap does not leak the RNG stream", {
  z <- stats::qnorm(bnd_unif(800L, 5))
  d <- cbind(1 + 0.3 * z[1:400], 3 + 0.3 * z[401:800])
  g <- function(d, th) cbind(d[, 1] - th, th - d[, 2])
  set.seed(31337)
  before <- stats::runif(3L)
  set.seed(31337)
  invisible(morie_bnd_moment_inequality(d, g, 2, B = 50, seed = 1))
  expect_equal(stats::runif(3L), before, tolerance = 1e-12)
})
