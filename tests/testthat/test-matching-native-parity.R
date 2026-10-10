# SPDX-License-Identifier: AGPL-3.0-or-later
# Native gradient-boosted propensity scores (morie_matching_estimate_propensity
# model = "gbm") against gbm::gbm.

mk_gbm_df <- function(n, seed) {
  set.seed(seed)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = runif(n), x4 = rpois(n, 3))
  d$d <- rbinom(n, 1, plogis(0.5 * d$x1 - 0.7 * d$x2 + d$x1 * d$x3 + 0.2 * (d$x4 - 3)))
  d
}

test_that("native gbm propensities reproduce gbm::gbm with bag.fraction = 1", {
  skip_if_not_installed("gbm")
  for (cfg in list(c(200, 1), c(600, 2))) {
    d <- mk_gbm_df(cfg[1], cfg[2])
    cv <- c("x1", "x2", "x3", "x4")
    a <- morie_matching_estimate_propensity(d, "d", cv, model = "gbm")
    fit <- gbm::gbm(d ~ x1 + x2 + x3 + x4, data = d, distribution = "bernoulli",
                    n.trees = 100, interaction.depth = 3, shrinkage = 0.1,
                    bag.fraction = 1, verbose = FALSE)
    b <- gbm::predict.gbm(fit, newdata = d, n.trees = 100, type = "response")
    # Same deterministic algorithm (best-first trees, midpoint splits,
    # n.minobsinnode = 10, Newton leaves): observed max |diff| ~ 1e-15,
    # tolerance 1e-8 allows for summation order only.
    expect_equal(unname(a), as.numeric(b), tolerance = 1e-8)
    expect_gt(stats::cor(a, b), 0.99)
  }
})

test_that("native gbm propensities need no package and are deterministic", {
  d <- mk_gbm_df(150, 3)
  a1 <- morie_matching_estimate_propensity(d, "d", c("x1", "x2"), model = "gbm")
  a2 <- morie_matching_estimate_propensity(d, "d", c("x1", "x2"), model = "gbm")
  expect_identical(a1, a2)
  expect_length(a1, nrow(d))
  expect_true(all(a1 > 0 & a1 < 1))
  expect_identical(names(a1), rownames(d))
  # boosting on the deviance raises the likelihood above the intercept-only fit
  ll <- function(p) sum(d$d * log(p) + (1 - d$d) * log(1 - p))
  expect_gt(ll(a1), ll(rep(mean(d$d), nrow(d))))
})
