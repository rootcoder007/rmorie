# SPDX-License-Identifier: AGPL-3.0-or-later
# The branches of R/causal_impact_native.R the parity tests leave untouched: the
# sigma cap in the rejection sampler, period arguments given as index values,
# missing pre-period responses, constant covariates, and the input guards.

mk_cov_ci <- function(seed = 2, n = 60L, pre = 40L, effect = 2) {
  set.seed(seed)
  x <- 50 + cumsum(stats::rnorm(n, sd = 0.5))
  y <- 0.8 * x + stats::rnorm(n, sd = 0.5)
  y[(pre + 1L):n] <- y[(pre + 1L):n] + effect
  data.frame(y = y, x = x)
}

test_that("the sigma^2 draw falls back to the cap when every draw exceeds the upper limit", {
  set.seed(1)
  # shape 1e6, rate 1: Gamma(1e6, 1) is about 1e6, so sigma^2 = 1/Gamma is about 1e-6 -> accepted
  s2 <- .ci_draw_sigma2(1e6, 1, upper = 1)
  expect_lt(sqrt(s2), 1)
  # a cap of 0 cannot be met by any positive draw: the capped fallback 0^2 = 0 is returned
  expect_identical(.ci_draw_sigma2(5, 5, upper = 0), 0)
  # a cap of 1e-9 with a draw around 1: all 100 rejections, fallback upper^2
  expect_identical(.ci_draw_sigma2(100, 100, upper = 1e-9), 1e-18)
})

test_that("the backward sampler handles a single observation and missing values", {
  set.seed(4)
  mu1 <- .ci_ffbs(z = 3, s2 = 1, q = 1, m0 = 0, C0 = 1)
  expect_length(mu1, 1L)
  # posterior mean of mu_1 given z = 3 with prior N(0, 1) and noise 1 is 1.5
  set.seed(4)
  draws <- replicate(4000, .ci_ffbs(z = 3, s2 = 1, q = 1, m0 = 0, C0 = 1))
  expect_lt(abs(mean(draws) - 1.5), 0.05)
  z <- c(1, NA, NA, 2)
  set.seed(5)
  mu <- .ci_ffbs(z, s2 = 0.01, q = 0.01, m0 = 1, C0 = 0.01)
  expect_length(mu, 4L)
  expect_true(all(is.finite(mu)))
})

test_that("periods may be given as index values and the post period may be a single row", {
  d <- mk_cov_ci()
  idx <- as.Date("2024-01-01") + seq_len(nrow(d)) - 1
  attr(d, "index") <- idx
  r <- morie_causal_impact(d, c(idx[1], idx[40]), c(idx[41], idx[60]),
                           model_args = list(niter = 100L, seed = 1L))
  expect_equal(r$impact$pre_period, c(1L, 40L))
  expect_equal(r$impact$post_period, c(41L, 60L))
  expect_error(morie_causal_impact(d, c(idx[1], idx[40]), c(idx[41], as.Date("2030-01-01")),
                                   model_args = list(niter = 50L)), "two increasing positions")
  attr(d, "index") <- NULL
  r1 <- morie_causal_impact(d, c(1, 40), c(41, 41), model_args = list(niter = 100L, seed = 1L))
  expect_equal(nrow(r1$impact$series), 41L)
  expect_equal(sum(r1$impact$series$post_period), 1L)
  expect_equal(r1$summary["Cumulative", "Actual"], d$y[41])
  # a window that starts after the first row shifts the time column
  r2 <- morie_causal_impact(d, c(5, 40), c(41, 50), model_args = list(niter = 100L, seed = 1L))
  expect_equal(r2$impact$series$time, 5:50)
})

test_that("missing pre-period responses are skipped and a constant covariate is dropped", {
  d <- mk_cov_ci()
  d$y[c(3, 10, 11)] <- NA
  d$k <- 7
  r <- morie_causal_impact(d, c(1, 40), c(41, 60), model_args = list(niter = 200L, seed = 2L))
  expect_identical(colnames(r$impact$model$coefficients), "x")
  expect_equal(nrow(r$impact$series), 60L)
  expect_true(is.na(r$impact$series$response[3]))
  # the pointwise effect is undefined where the response is missing, defined elsewhere
  expect_true(is.na(r$impact$series$point.effect.lower[3]))
  expect_true(all(is.finite(r$impact$series$point.effect.lower[-c(3, 10, 11)])))
  # the effect is recovered within the interval
  expect_gt(r$summary["Average", "AbsEffect.upper"], 0)
  # only constant covariates: the model falls back to a local level alone
  d2 <- d[, c("y", "k")]
  r2 <- morie_causal_impact(d2, c(1, 40), c(41, 60), model_args = list(niter = 100L, seed = 2L))
  expect_equal(ncol(r2$impact$model$coefficients), 0L)
  # no standardisation keeps the response on its own scale
  r3 <- morie_causal_impact(d[, c("y", "x")], c(1, 40), c(41, 60),
                            model_args = list(niter = 100L, seed = 2L, standardize.data = FALSE))
  expect_equal(r3$impact$model$standardization, list(y_mean = 0, y_sd = 1))
  # a numeric vector without covariates is accepted
  r4 <- morie_causal_impact(d$y, c(1, 40), c(41, 60), model_args = list(niter = 100L, seed = 2L))
  expect_equal(nrow(r4$impact$series), 60L)
})

test_that("the input guards name the problem", {
  d <- mk_cov_ci()
  # a seeded run in a session that has not touched the RNG yet leaves no .Random.seed behind
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  r0 <- morie_causal_impact(d, c(1, 40), c(41, 60), model_args = list(niter = 20L, seed = 9L))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  expect_equal(nrow(r0$impact$series), 60L)
  expect_error(morie_causal_impact(d, c(1, 40), c(41, 60), alpha = 1.5), "alpha")
  expect_error(morie_causal_impact(d, c(1, 40), c(41, 60), model_args = list(niter = 5L)), "at least 10")
  d_na <- d
  d_na$x[2] <- NA
  expect_error(morie_causal_impact(d_na, c(1, 40), c(41, 60)), "covariates must not")
  d_post <- d
  d_post$y[45] <- NA
  expect_error(morie_causal_impact(d_post, c(1, 40), c(41, 60), model_args = list(niter = 20L)),
               "post-period response")
  d_few <- d
  d_few$y[1:38] <- NA
  expect_error(morie_causal_impact(d_few, c(1, 40), c(41, 60)), "at least 3 observed")
  expect_error(morie_causal_impact(d, c(40, 1), c(41, 60)), "two increasing positions")
  # a constant pre-period response gets unit scale instead of a division by zero, and the
  # sampler's priors fall back to unit scale instead of producing NA draws
  d_c <- d
  d_c$y[1:40] <- 5
  r <- morie_causal_impact(d_c, c(1, 40), c(41, 60), model_args = list(niter = 50L, seed = 1L))
  expect_equal(r$impact$model$standardization$y_sd, 1)
  expect_true(all(is.finite(r$impact$model$sigma_obs)))
  expect_true(all(is.finite(unlist(r$summary))))
})
