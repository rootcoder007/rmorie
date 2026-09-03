# SPDX-License-Identifier: AGPL-3.0-or-later

.pars <- function() list(mu = 0.2, alpha = 0.5, beta = 1, sigma = 0.4)

test_that("intensity is background plus a positive, decaying trigger", {
  ev <- data.frame(t = c(0.1, 0.5), x = c(0, 1), y = c(0, 1))
  p <- .pars()
  # No past events -> exactly the background rate.
  expect_equal(morie_hawkes_st_intensity(ev, -1, 0, 0, p), p$mu)
  # Right on top of a recent event -> strictly above background.
  near <- morie_hawkes_st_intensity(ev, 0.51, 0, 0, p)
  far <- morie_hawkes_st_intensity(ev, 5.0, 0, 0, p)
  expect_gt(near, p$mu)
  expect_lt(far, near)            # temporal decay
  # Spatial decay: same time gap, farther away -> lower intensity.
  close <- morie_hawkes_st_intensity(ev, 0.6, 0, 0, p)
  distant <- morie_hawkes_st_intensity(ev, 0.6, 8, 8, p)
  expect_gt(close, distant)
})

test_that("log-likelihood is finite and peaks near the true parameters", {
  ev <- morie_hawkes_st_simulate(.pars(), end_time = 40,
                                 region = c(0, 10, 0, 10), seed = 3)
  ll_true <- morie_hawkes_st_loglik(ev, .pars(), end_time = 40, area = 100)
  expect_true(is.finite(ll_true))
  # A badly wrong alpha should not fit better than the truth.
  bad <- .pars()
  bad$alpha <- 0.95
  ll_bad <- morie_hawkes_st_loglik(ev, bad, end_time = 40, area = 100)
  expect_gt(ll_true, ll_bad)
})

test_that("simulation clusters: more offspring at higher branching ratio", {
  lo <- morie_hawkes_st_simulate(
    list(mu = 0.2, alpha = 0.1, beta = 1, sigma = 0.4),
    end_time = 50, region = c(0, 10, 0, 10), seed = 11)
  hi <- morie_hawkes_st_simulate(
    list(mu = 0.2, alpha = 0.7, beta = 1, sigma = 0.4),
    end_time = 50, region = c(0, 10, 0, 10), seed = 11)
  expect_gt(nrow(hi), nrow(lo))
  expect_true(all(hi$t < 50))
  expect_true(all(hi$gen >= 0))
  expect_true(any(hi$gen > 0))     # at least some triggered offspring
})

test_that("simulation rejects supercritical alpha", {
  expect_error(
    morie_hawkes_st_simulate(list(mu = 1, alpha = 1.2, beta = 1, sigma = 1),
                             end_time = 10, region = c(0, 1, 0, 1)),
    "subcritical"
  )
})

test_that("MLE returns a stable fit that maximises the in-sample likelihood", {
  truth <- list(mu = 0.3, alpha = 0.5, beta = 1.2, sigma = 0.5)
  ev <- morie_hawkes_st_simulate(truth, end_time = 40,
                                 region = c(0, 6, 0, 6), seed = 5)
  ll_truth <- morie_hawkes_st_loglik(ev, truth, end_time = 40, area = 36)
  fit <- morie_hawkes_st_fit(ev, end_time = 40, area = 36)
  expect_equal(fit$convergence, 0L)
  # The optimiser must do at least as well as the data-generating params
  # in-sample (spatiotemporal Hawkes MLE is weakly identified, so we assert
  # likelihood maximisation + stability rather than tight parameter recovery).
  expect_gte(fit$loglik, ll_truth - 1e-6)
  expect_gt(fit$params$alpha, 0)
  expect_lt(fit$params$alpha, 1)
  expect_gt(fit$params$mu, 0)
  expect_gt(fit$params$beta, 0)
  expect_gt(fit$params$sigma, 0)
})

test_that("parameter validation", {
  expect_error(morie_hawkes_st_loglik(data.frame(t = 1, x = 0, y = 0),
                                      list(mu = 0.1)), "mu, alpha, beta, sigma")
  expect_error(morie_hawkes_st_intensity(data.frame(t = 1, x = 0, y = 0),
                                         2, 0, 0,
                                         list(mu = -1, alpha = 0.5, beta = 1,
                                              sigma = 1)), "mu>=0")
})
