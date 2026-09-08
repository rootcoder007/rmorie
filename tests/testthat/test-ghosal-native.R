# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the Bayesian nonparametrics shelf.
# Anchors from the Python modules at full double precision; LCG
# fixture so both languages see identical bits. Only the
# deterministic estimators are mirrored -- the Monte Carlo mixture
# fits (gh_c5_8, gh_c5_9, gh_c7_4, gh_c10_7) depend on each
# language's RNG stream and are covered by the Python suite.

gh_fix <- function(n = 200L, seed = 24680) {
  m <- 2L * n
  u <- numeric(m)
  s <- seed
  for (k in seq_len(m)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[k] <- (s + 0.5) / 4294967296
  }
  list(u = u, z = stats::qnorm(u[1:n]), n = n)
}

test_that("morie_dp_predictive matches morie.fn.gh_dp_post_ex", {
  f <- gh_fix()
  o <- morie_dp_predictive(f$z, alpha = 2)
  expect_equal(o$base_weight, 0.009900990099009901, tolerance = 1e-12)
  expect_equal(o$n_distinct, 200L)
  expect_equal(o$base_weight + o$atom_weight, 1)
  expect_equal(sum(o$atom_probs), o$atom_weight, tolerance = 1e-12)
  # a DP draw is a.s. discrete: this is NOT a density
  expect_false(o$is_density)
  expect_gt(morie_dp_predictive(f$z, alpha = 1e-6)$atom_weight, 0.999)
  expect_gt(morie_dp_predictive(f$z, alpha = 1e6)$base_weight, 0.999)
  expect_error(morie_dp_predictive(f$z, alpha = 0), "positive")
})

test_that("morie_predictive_recursion matches morie.fn.gh_c5_7", {
  f <- gh_fix()
  o <- morie_predictive_recursion(f$z, sigma = 1)
  expect_equal(o$f_mixing[101], 0.9941064473673016, tolerance = 1e-9)
  expect_equal(o$theta_grid[1], -4.280925817685364, tolerance = 1e-12)
  expect_equal(o$mixed_density[101], 0.3693317047529827, tolerance = 1e-9)
  # it is a density
  expect_equal(.morie_gh_trapz(o$theta_grid, o$f_mixing), 1, tolerance = 1e-6)
  # the section's own caveat, made checkable
  expect_true(o$order_dependent)
  expect_false(isTRUE(all.equal(
    o$f_mixing, morie_predictive_recursion(rev(f$z), sigma = 1)$f_mixing)))
  # w_1 must be strictly inside (0, 1): (i+1)^{-2/3} would be 1
  expect_error(morie_predictive_recursion(f$z, weights = rep(1, 200)),
               "strictly in")
})

test_that("morie_polya_tree_density matches morie.fn.gh_c7_6", {
  f <- gh_fix()
  o <- morie_polya_tree_density(f$z, levels = 4L)
  expect_equal(o$density[101], 0.36719632674615077, tolerance = 1e-9)
  expect_equal(o$mass, 1.003406035916731, tolerance = 1e-9)
  expect_true(o$absolutely_continuous_prior)
  expect_true(all(o$density >= 0))
  expect_error(morie_polya_tree_density(f$z, a_scale = 0), "positive")
})

test_that("morie_polya_tree_mixture smooths the partition artefacts", {
  f <- gh_fix()
  o <- morie_polya_tree_mixture(f$z, levels = 4L)
  expect_equal(o$density[101], 0.38253322785610316, tolerance = 1e-9)
  expect_equal(o$max_jump, 0.060641459152482924, tolerance = 1e-9)
  expect_equal(o$max_jump_single, 0.1749100435921437, tolerance = 1e-9)
  # the entire reason the section exists
  expect_true(o$smoother_than_single)
  expect_lt(o$max_jump, o$max_jump_single)
})

test_that("morie_polya_tree_rate pays exactly one log factor", {
  o <- morie_polya_tree_rate(numeric(5), s = 2, n = 10000)
  expect_equal(o$rate, 0.2313532900995187, tolerance = 1e-12)
  expect_equal(o$minimax_rate, 0.025118864315095794, tolerance = 1e-12)
  expect_equal(o$ratio_to_minimax, 9.210340371976184, tolerance = 1e-12)
  expect_equal(o$rate, o$minimax_rate * log(10000))
  expect_true(o$adaptive)
  expect_false(o$requires_knowing_s)
  expect_error(morie_polya_tree_rate(numeric(5), s = -1, n = 100), "positive")
})

test_that("morie_contraction_conditions balances entropy against prior mass", {
  o <- morie_contraction_conditions(numeric(9), eps = 0.1, n = 1000,
                                    entropy = 5, prior_mass = exp(-5))
  expect_equal(o$n_eps_squared, 10)
  expect_equal(o$prior_mass_budget, 4.5399929762484854e-05, tolerance = 1e-14)
  expect_true(o$entropy_ok)
  expect_true(o$prior_mass_ok)
  expect_true(o$all_conditions_checked)
  expect_equal(o$metric, "Hellinger")
  # the theorem needs them TOGETHER: one alone is not a rate
  partial <- morie_contraction_conditions(numeric(9), eps = 0.1, n = 1000,
                                          entropy = 5)
  expect_null(partial$prior_mass_ok)
  expect_false(partial$all_conditions_checked)
})

test_that("morie_gp_density_rate matches Python and diverges only asymptotically", {
  o <- morie_gp_density_rate(numeric(5), s = 1, n = 10000,
                             kernel = "rescaled_se")
  expect_equal(o$rate, 0.2039469054503718, tolerance = 1e-12)
  expect_equal(o$ratio_to_minimax, 4.393902880269336, tolerance = 1e-12)
  se <- morie_gp_density_rate(numeric(5), s = 1, n = 10000)
  ma <- morie_gp_density_rate(numeric(5), s = 1, n = 10000, kernel = "matern")
  expect_equal(se$rate_kind, "LOGARITHMIC")
  expect_true(ma$attains_minimax)
  # at n = 1e4 the LOGARITHMIC rate is numerically SMALLER than the
  # rescaled polynomial one -- they cross later, so the ratio to
  # minimax is the honest summary, not the rate value
  expect_lt(se$rate, o$rate)
  ratios <- vapply(c(1e4, 1e6, 1e10), function(m)
    morie_gp_density_rate(numeric(5), s = 1, n = m)$ratio_to_minimax,
    numeric(1))
  expect_true(all(diff(ratios) > 0))
  expect_gt(ratios[3], 50)
  expect_error(morie_gp_density_rate(numeric(5), kernel = "laplace"),
               "squared_exponential")
})

test_that("morie_dp_survival converges to Kaplan-Meier as alpha vanishes", {
  f <- gh_fix()
  tt <- abs(f$z) + 0.1
  ev <- ifelse(f$u[201:400] > 0.25, 1, 0)
  o <- morie_dp_survival(tt, ev, alpha = 1)
  expect_equal(o$survival_dp[11], 0.963754628332539, tolerance = 1e-10)
  expect_equal(o$survival_km[11], 0.9644387026585981, tolerance = 1e-10)
  expect_equal(o$max_abs_diff_to_km, 0.0010332471752781958, tolerance = 1e-10)
  # the section's whole claim, as a measurement
  far <- morie_dp_survival(tt, ev, alpha = 50)$max_abs_diff_to_km
  near <- morie_dp_survival(tt, ev, alpha = 0.01)$max_abs_diff_to_km
  expect_lt(near, far)
  expect_lt(near, 1e-2)
  expect_true(all(diff(o$survival_km) <= 1e-12))
  expect_error(morie_dp_survival(-tt, ev), "non-negative")
})

test_that("morie_empirical_bayes_dp tracks the cluster count", {
  o <- morie_empirical_bayes_dp(rep(1:5, each = 40))
  expect_equal(o$alpha_hat, 0.8119844993184013, tolerance = 1e-9)
  expect_equal(o$n_clusters, 5L)
  expect_true(o$understates_uncertainty)
  # the marginal depends on the data only through the partition
  expect_gt(morie_empirical_bayes_dp(as.numeric(1:120))$alpha_hat,
            morie_empirical_bayes_dp(rep(1:3, each = 40))$alpha_hat)
  expect_error(morie_empirical_bayes_dp(1:10, alpha_grid = c(-1, 1)),
               "positive")
})
