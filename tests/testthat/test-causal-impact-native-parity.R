# SPDX-License-Identifier: AGPL-3.0-or-later
# Native Bayesian structural time series behind morie_causal_impact().
#
# CausalImpact (and its Boom / bsts dependencies) cannot be cross-validated
# here, so these tests check the sampler against known truths: coverage of a
# simulated effect, a null post-period, and the Kalman-filter level of the
# matching frequentist local-level model.

mk_ci_data <- function(seed, effect, n = 100L, pre = 70L) {
  set.seed(seed)
  x <- 100 + as.numeric(arima.sim(list(ar = 0.8), n))
  y <- 1.2 * x + rnorm(n)
  y[(pre + 1L):n] <- y[(pre + 1L):n] + effect
  data.frame(y = y, x = x)
}

test_that("native causal impact returns the documented fields", {
  d <- mk_ci_data(1, 3)
  res <- morie_causal_impact(d, c(1, 70), c(71, 100),
                             model_args = list(niter = 300L, seed = 1L))
  expect_true(all(c("average_effect", "cumulative_effect", "ci_lower",
                    "ci_upper", "summary", "posterior_prob_causal",
                    "impact") %in% names(res)))
  expect_identical(rownames(res$summary), c("Average", "Cumulative"))
  expect_true(all(c("Actual", "Pred", "Pred.lower", "Pred.upper", "AbsEffect",
                    "AbsEffect.lower", "AbsEffect.upper", "RelEffect",
                    "RelEffect.lower", "RelEffect.upper", "alpha", "p") %in%
                    names(res$summary)))
  expect_s3_class(res$impact, "morie_causal_impact")
  expect_equal(nrow(res$impact$series), 100L)
  expect_equal(dim(res$impact$y_samples), c(270L, 30L))
  s <- res$summary
  expect_equal(s["Average", "AbsEffect"], s["Average", "Actual"] - s["Average", "Pred"])
  expect_equal(s["Cumulative", "Actual"], sum(d$y[71:100]))
  expect_equal(res$cumulative_effect, 30 * res$average_effect)
  expect_lt(res$ci_lower, res$ci_upper)
})

test_that("the effect interval covers a known effect and the null gives p > 0.05", {
  # A single 95 percent interval misses the truth 5 percent of the time
  # (observed coverage 0.97 over 100 seeds), so check the frequency over
  # replicates: under the null the one-sided tail probability p is roughly
  # uniform on (0, 0.5), so p > 0.05 in about 90 percent of replicates.
  cover <- cover0 <- null_ok <- detect <- logical(0)
  for (seed in 1:12) {
    r1 <- morie_causal_impact(mk_ci_data(seed, 5), c(1, 70), c(71, 100),
                              model_args = list(niter = 200L, seed = seed))
    cover <- c(cover, r1$ci_lower < 5 && r1$ci_upper > 5)
    detect <- c(detect, r1$posterior_prob_causal < 0.05)
    r0 <- morie_causal_impact(mk_ci_data(seed, 0), c(1, 70), c(71, 100),
                              model_args = list(niter = 200L, seed = seed))
    cover0 <- c(cover0, r0$ci_lower < 0 && r0$ci_upper > 0)
    null_ok <- c(null_ok, r0$posterior_prob_causal > 0.05)
  }
  expect_gte(mean(cover), 0.75)
  expect_gte(mean(cover0), 0.75)
  expect_gte(mean(null_ok), 0.75)
  expect_true(all(detect))
  # the seed-1 null case on its own
  r0 <- morie_causal_impact(mk_ci_data(1, 0), c(1, 70), c(71, 100),
                            model_args = list(niter = 500L, seed = 1L))
  expect_gt(r0$posterior_prob_causal, 0.05)
})

test_that("seeded runs are reproducible and leave the session RNG alone", {
  d <- mk_ci_data(7, 2)
  set.seed(99)
  before <- runif(1)
  set.seed(99)
  a <- morie_causal_impact(d, c(1, 70), c(71, 100), model_args = list(niter = 200L, seed = 3L))
  after <- runif(1)
  b <- morie_causal_impact(d, c(1, 70), c(71, 100), model_args = list(niter = 200L, seed = 3L))
  expect_identical(a$summary, b$summary)
  expect_identical(before, after)
  set.seed(5)
  c1 <- morie_causal_impact(d, c(1, 70), c(71, 100), model_args = list(niter = 200L))
  set.seed(5)
  c2 <- morie_causal_impact(d, c(1, 70), c(71, 100), model_args = list(niter = 200L))
  expect_identical(c1$summary, c2$summary)
})

test_that("local-level counterfactual agrees with the Kalman filter level", {
  set.seed(3)
  yy <- 10 + cumsum(rnorm(80, sd = 0.3)) + rnorm(80, sd = 0.5)
  r <- morie_causal_impact(data.frame(y = yy), c(1, 60), c(61, 80),
                           model_args = list(niter = 2000L, seed = 2L))
  s_obs <- stats::median(r$impact$model$sigma_obs)
  s_lev <- stats::median(r$impact$model$sigma_level)
  kf <- morie_kalman_filter(yy[1:60], Q = matrix(s_lev^2), R = matrix(s_obs^2),
                            x0 = yy[1], P0 = matrix(stats::var(yy[1:60])))
  # With no covariates the counterfactual mean is the filtered level at the
  # end of the pre-period carried forward. Posterior averaging over the
  # variances moves it slightly (observed difference ~0.01 here); allow half
  # the Kalman filter's own state sd.
  expect_lt(abs(mean(r$impact$series$point.pred[61:80]) - kf$state[60, 1]),
            0.5 * sqrt(kf$state_cov[60, 1, 1]))
})

test_that("unsupported CausalImpact options error clearly", {
  d <- mk_ci_data(1, 0)
  expect_error(morie_causal_impact(d, c(1, 70), c(71, 100), model_args = list(nseasons = 7)),
               "nseasons")
  expect_error(morie_causal_impact(d, c(1, 70), c(71, 100), model_args = list(dynamic.regression = TRUE)),
               "dynamic")
  expect_error(morie_causal_impact(d, c(1, 70), c(71, 100), model_args = list(foo = 1)),
               "not supported")
  expect_error(morie_causal_impact(d, c(1, 70), c(60, 100)), "after")
})
