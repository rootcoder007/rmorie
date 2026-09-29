# Coverage for the Beta quantile-pyramid predictive (Hjort & Walker 2009):
# cell counts, the exact and substitute (multinomial) likelihoods, the
# log prior as a sum of Beta log densities with the c m^3 concentration
# schedule and null centring, pyramid draws regenerated from the counter
# generator, and the MCMC predictive checked for internal consistency
# (predictive quantiles invert the predictive CDF).

.q <- c(0, 0.1, 0.35, 0.6, 1)

test_that("counts assign each u to the first cell with u <= q_j", {
  u <- c(0, 0.05, 0.1, 0.2, 0.6, 0.61, 1)
  expect_identical(morie_bnppvl_counts(u, .q), as.integer(tabulate(vapply(u, function(v) which(v <= .q[-1])[1], 1), 4)))
  expect_identical(morie_bnppvl_counts(u, .q), c(3L, 1L, 1L, 2L))
})

test_that("exact and substitute likelihoods", {
  u <- c(0.05, 0.2, 0.3, 0.7, 0.9)
  n <- morie_bnppvl_counts(u, .q)
  expect_equal(morie_bnppvl_loglik(u, .q), sum(n * (-log(4) - log(diff(.q)))), tolerance = 1e-12)
  expect_equal(morie_bnppvl_loglik(u, .q, "substitute"), stats::dmultinom(n, prob = rep(0.25, 4), log = TRUE), tolerance = 1e-12)
  expect_error(morie_bnppvl_loglik(u, .q, "poisson"), "kind must be one of")
})

test_that("the log prior sums Beta log densities of the node splits", {
  lp <- morie_bnppvl_log_prior(.q, 2, c = 2)
  # level 1: node q_2 in (0, 1), a = 2 * 1^3 / 2 each; level 2: a = 2 * 8 / 2
  ref <- stats::dbeta(0.35, 1, 1, log = TRUE) +
    stats::dbeta(0.1 / 0.35, 8, 8, log = TRUE) - log(0.35) +
    stats::dbeta((0.6 - 0.35) / 0.65, 8, 8, log = TRUE) - log(0.65)
  # the package log-gamma is a series good to ~1e-12 relative, not lgamma()
  expect_equal(lp, ref, tolerance = 1e-10)
  lc <- morie_bnppvl_log_prior(.q, 2, c = 3, schedule = "constant")
  refc <- stats::dbeta(0.35, 1.5, 1.5, log = TRUE) + stats::dbeta(0.1 / 0.35, 1.5, 1.5, log = TRUE) - log(0.35) +
    stats::dbeta(0.25 / 0.65, 1.5, 1.5, log = TRUE) - log(0.65)
  expect_equal(lc, refc, tolerance = 1e-10)
  nq <- function(p) stats::qbeta(p, 2, 5)
  ln <- morie_bnppvl_log_prior(c(0, 0.35, 1), 1, c = 4, centring = "null", nullq = nq)
  mu <- nq(0.5)
  expect_equal(ln, stats::dbeta(0.35, 4 * mu, 4 * (1 - mu), log = TRUE), tolerance = 1e-10)
  expect_identical(morie_bnppvl_log_prior(c(0, 0.5, 0.4, 0.6, 1), 2), -Inf)
  expect_error(morie_bnppvl_log_prior(.q, 1, schedule = "linear"), "schedule must be one of")
  expect_error(morie_bnppvl_log_prior(.q, 1, c = 0), "must be positive")
})

test_that("pyramid draws fill the dyadic nodes level by level", {
  q <- morie_bnppvl_draw(.ghc_rng(4), 2, c = 2)
  e <- .ghc_rng(4)
  ref <- c(0, 0, 0, 0, 1)
  v <- .ghc_beta1(e, 1, 1)
  ref[3] <- v
  v <- .ghc_beta1(e, 8, 8)
  ref[2] <- ref[3] * v
  v <- .ghc_beta1(e, 8, 8)
  ref[4] <- ref[3] * (1 - v) + v
  expect_equal(q, ref, tolerance = 1e-12)
  expect_true(all(diff(q) > 0))
  expect_true(is.finite(morie_bnppvl_log_prior(q, 2, c = 2)))
  expect_error(morie_bnppvl_draw(.ghc_rng(1), 0), "at least one level")
})

test_that("the predictive quantiles invert the predictive CDF", {
  set.seed(2)
  x <- 10 + 5 * stats::rbeta(40, 2, 5)
  r <- morie_bnppvl(x, m = 3, lo = 10, hi = 15, sweeps = 120, burn = 20, thin = 5, seed = 1, init = "empirical")
  expect_identical(r$n_draws, 20L)
  expect_identical(c(r$quantile_mean[1], r$quantile_mean[9]), c(10, 15))
  expect_true(all(diff(r$quantile_mean) > 0))
  g <- morie_bnppvl(x, m = 3, lo = 10, hi = 15, sweeps = 120, burn = 20, thin = 5, seed = 1,
                    init = "empirical", grid = r$predictive_quantile)
  expect_equal(g$cdf, r$probs, tolerance = 1e-9)
  expect_true(all(diff(r$cdf) >= 0))
  expect_gt(r$accept_rate, 0)
  expect_equal(r$log_likelihood, morie_bnppvl_loglik((x - 10) / 5, (r$quantile_mean - 10) / 5), tolerance = 1e-12)
  s <- morie_bnppvl(x, m = 2, lo = 10, hi = 15, sweeps = 30, burn = 5, likelihood = "substitute", seed = 2)
  expect_identical(s$likelihood, "substitute")
  expect_match(morie_bnppvl_cheatsheet(), "quantile-pyramid")
  expect_error(morie_bnppvl(x, lo = 10, hi = 15, centring = "null"), "needs nullq")
  expect_error(morie_bnppvl(x), "inside \\[lo, hi\\]")
  expect_error(morie_bnppvl(x, lo = 10, hi = 15, sweeps = 10, burn = 10), "burn < sweeps")
  expect_error(morie_bnppvl(x, lo = 10, hi = 15, probs = 1), "strictly inside")
  expect_error(morie_bnppvl(x, lo = 10, hi = 15, init = "zero"), "init must be one of")
  expect_error(morie_bnppvl(numeric(0)), "at least one observation")
})
