test_that("counting and probability rules", {
  expect_equal(AtLeastOneOfIid(0.3, 5)$p_at_least_one, 1 - 0.7^5, tolerance = 1e-14)
  expect_equal(AtMostTwoSuitsProbability()$favorable,
               4 * choose(13, 5) + choose(4, 2) * (choose(26, 5) - 2 * choose(13, 5)))
  b <- BayesGeneral(c(0.5, 0.3, 0.2), c(0.1, 0.4, 0.8))
  expect_equal(b$posteriors, c(0.05, 0.12, 0.16) / 0.33, tolerance = 1e-15)
  expect_equal(ExactHalfHeads(3)$probability, dbinom(3, 6, 0.5), tolerance = 1e-15)
  expect_equal(MorinFactorial(10)$factorial, factorial(10))
  expect_equal(StarsAndBars(3, 4)$count, nrow(unique(t(apply(expand.grid(1:4, 1:4, 1:4), 1, sort)))))
  expect_equal(SuitFullHouseProbability()$probability, 4 * choose(13, 3) * 3 * choose(13, 2) / choose(52, 5))
  expect_equal(ProbOrGeneral(0.5, 0.4, 0.2)$p_or, 0.7, tolerance = 1e-15)
  expect_error(ProbOrExclusive(c(0.6, 0.7)))
})

test_that("moments, independence and correlation results", {
  v <- c(1, 2, 6)
  p <- c(0.2, 0.5, 0.3)
  mu <- sum(v * p)
  expect_equal(PmfSd(v, p)$sd, sqrt(sum(p * (v - mu)^2)), tolerance = 1e-15)
  d <- PmfSumConvolution(1:6, rep(1 / 6, 6), 1:6, rep(1 / 6, 6))
  expect_equal(d$probs, (6 - abs(2:12 - 7)) / 36, tolerance = 1e-15)
  expect_true(JointIndependent(outer(c(0.3, 0.7), c(0.4, 0.6)))$independent)
  x <- c(1, 2, 3, 5, 8)
  y <- c(2, 2.5, 4, 4.5, 9)
  expect_equal(SlopeFromCov(x, y)$slope, unname(coef(lm(y ~ x))[2]), tolerance = 1e-14)
  r <- cor(x, y)
  expect_equal(PredictionImprovement(r)$mse_fraction_remaining, sum(resid(lm(y ~ x))^2) / sum((y - mean(y))^2),
               tolerance = 1e-14)
  expect_equal(SdFairCoinAvg(12)$sd_avg, sqrt(0.25 / 12), tolerance = 1e-15)
  expect_equal(VarSumWithCov(2, 3, 0.5)$var_sum, 6)
  expect_equal(JointDensityFactorizes(c(0, 0.5, 1, 2), c(0.2, 0.6, 0.5, 0.1), c(0, 1, 3), c(0.3, 0.4, 0.1))$total_mass,
               (0.5 * 0.4 + 0.5 * 0.55 + 1 * 0.3) * (0.35 + 2 * 0.25), tolerance = 1e-14)
  t <- ExponentialCrossingTime()$t
  expect_equal(exp(-0.2 * t), exp(-0.05 * t) / 4, tolerance = 1e-15)
})
