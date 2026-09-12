# Marginal structural model with ridge-penalised propensity weights
# (Setoguchi et al. 2008; Westreich, Lessler & Funk 2010; weights per
# Hernan & Robins 2020 Sec. 12.3).
#
# Anchors outside the module: with no penalty the propensity model is an
# ordinary logistic regression, so base R's glm fixes the weights; with a
# very large penalty the slopes are shrunk away and, because the penalty
# spares the intercept, the fitted probability must collapse to the sample
# proportion. Those two limits bracket the whole penalty path.

sim <- function(n = 400, seed = 1, confounded = TRUE) {
  set.seed(seed)
  L1 <- rnorm(n)
  A1 <- rbinom(n, 1, if (confounded) plogis(0.9 * L1) else rep(0.5, n))
  L2 <- rnorm(n, 0.5 * A1)
  A2 <- rbinom(n, 1, if (confounded) plogis(0.9 * L2) else rep(0.5, n))
  cum <- A1 + A2
  # the true effect of cumulative exposure is 2 per unit
  y <- 1 + 2 * cum + 1.5 * L1 + rnorm(n, 0, 0.5)
  list(y = y, A = list(A1, A2), L = list(L1, L2), cum = cum)
}

test_that("the penalty path is reported at its default grid", {
  d <- sim(200, 2)
  r <- shrinkage_msm(d$y, d$A, d$L)
  expect_length(r$path, 7L)
  lams <- vapply(r$path, function(z) z$lam, numeric(1))
  expect_equal(lams, c(0, 1e-4, 1e-2, 0.1, 1, 10, 1e3))
  # every row carries the quantities the diagnostic needs
  for (z in r$path) {
    expect_true(all(c("lam", "estimate", "se", "max_weight",
                      "effective_sample_size") %in% names(z)))
    expect_true(is.finite(z$estimate))
    expect_true(z$max_weight >= 1 - 1e-9 || z$max_weight > 0)
    expect_true(z$effective_sample_size > 0)
  }
  # an explicit grid is honoured
  g <- c(0, 0.5, 5)
  e <- shrinkage_msm(d$y, d$A, d$L, path = g)
  expect_length(e$path, 3L)
  expect_equal(vapply(e$path, function(z) z$lam, numeric(1)), g)
  # and the convenience wrapper is that path at no penalty
  expect_equal(penalty_path(d$y, d$A, d$L, path = g), e$path)
  expect_equal(r$n, 200L)
  expect_equal(r$n_times, 2L)
  expect_equal(r$contrast, "cumulative")
  expect_match(r$method, "Setoguchi")
})

test_that("no penalty is an ordinary logistic propensity model", {
  d <- sim(300, 3)
  r <- shrinkage_msm(d$y, d$A, d$L, lam = 0)
  # With no penalty each treatment model is an ordinary logistic
  # regression on the covariates at that time, so base R fixes the
  # propensities; the weight for a history is the product of the per-time
  # inverse probabilities, not their sum.
  g1 <- unname(fitted(glm(d$A[[1]] ~ d$L[[1]], family = binomial())))
  g2 <- unname(fitted(glm(d$A[[2]] ~ d$L[[2]], family = binomial())))
  den <- ifelse(d$A[[1]] > 0.5, g1, 1 - g1) *
    ifelse(d$A[[2]] > 0.5, g2, 1 - g2)
  expect_equal(r$weights, 1 / den, tolerance = 1e-6)
  expect_true(all(r$weights > 0))
  # summing instead of multiplying would give this, and must not
  expect_false(isTRUE(all.equal(
    r$weights,
    1 / ifelse(d$A[[1]] > 0.5, g1, 1 - g1) +
      1 / ifelse(d$A[[2]] > 0.5, g2, 1 - g2),
    tolerance = 1e-6)))
  # the reported summaries follow from the weights
  expect_equal(r$mean_weight, mean(r$weights))
  expect_equal(r$max_weight, max(r$weights))
  expect_equal(r$effective_sample_size,
               sum(r$weights)^2 / sum(r$weights^2))
  expect_equal(r$lam, 0)
  # the marginal structural model carries an intercept, so the exposure
  # coefficient is not forced to absorb it
  expect_true(is.finite(r$intercept))
})

test_that("a large penalty shrinks the propensity to the sample proportion", {
  d <- sim(300, 5)
  small <- shrinkage_msm(d$y, d$A, d$L, lam = 0)
  big <- shrinkage_msm(d$y, d$A, d$L, lam = 1e8)
  # The penalty spares the intercept, so shrinking the slopes away leaves
  # an intercept-only model. That is the same model fitted when there are
  # no covariates to condition on at all, so the two must agree exactly.
  none <- shrinkage_msm(d$y, d$A, list(NULL, NULL), lam = 0)
  expect_equal(big$weights, none$weights, tolerance = 1e-4)
  # the weights are then far less dispersed than the unpenalised ones
  expect_true(stats::sd(big$weights) < stats::sd(small$weights))
  expect_true(big$effective_sample_size > small$effective_sample_size)
  expect_true(big$max_weight < small$max_weight + 1e-9)
  # and the penalty path moves in that direction
  ess <- vapply(small$path, function(z) z$effective_sample_size,
                numeric(1))
  expect_true(ess[length(ess)] >= ess[1] - 1e-6)
})

test_that("weighting reduces confounding bias", {
  # with treatment assigned on a covariate that also drives the outcome,
  # the unweighted regression is biased for the true effect of two and the
  # weighted one should be closer
  d <- sim(1500, 7, confounded = TRUE)
  r <- shrinkage_msm(d$y, d$A, d$L, lam = 0)
  expect_true(abs(r$estimate - 2) < abs(r$unadjusted - 2))
  expect_equal(r$estimate, 2, tolerance = 0.35)
  # with assignment independent of the covariates there is nothing to
  # correct, so the two agree
  ind <- sim(1500, 7, confounded = FALSE)
  ri <- shrinkage_msm(ind$y, ind$A, ind$L, lam = 0)
  expect_equal(ri$estimate, ri$unadjusted, tolerance = 0.25)
  expect_equal(ri$estimate, 2, tolerance = 0.3)
})

test_that("a covariate history may be absent", {
  d <- sim(200, 11)
  # with nothing to condition on, the propensity at each time is the
  # marginal proportion, so the weight is the product of two marginal
  # inverse probabilities and takes only a few distinct values
  none <- shrinkage_msm(d$y, d$A, list(NULL, NULL), lam = 0)
  m1 <- mean(d$A[[1]])
  m2 <- mean(d$A[[2]])
  want <- (1 / ifelse(d$A[[1]] > 0.5, m1, 1 - m1)) *
    (1 / ifelse(d$A[[2]] > 0.5, m2, 1 - m2))
  expect_equal(none$weights, want, tolerance = 1e-8)
  expect_length(unique(round(none$weights, 8)), 4L)
  expect_equal(none$mean_weight, mean(want))
  expect_true(is.finite(none$estimate))
})

test_that("shrinkage_msm validates its arguments", {
  d <- sim(80, 13)
  expect_error(shrinkage_msm(d$y, d$A, d$L, lam = -1),
               "lam must be non-negative")
  expect_error(shrinkage_msm(d$y, d$A, list(d$L[[1]])),
               "treatment times but .* covariate blocks")
})
