# Tests for ProbabilityKit: probability and inference toolkit.

test_that("ProductVariance, WeibullMoments, FoldedNormal and Chisq1Cdf", {
  expect_equal(ProductVariance(1.3, 0.4, -2.2, 1.7), 0.4 * 1.7 + 1.3^2 * 1.7 + 2.2^2 * 0.4)
  m <- gamma(1 + 1 / 2.3) / 0.7^(1 / 2.3)
  expect_equal(WeibullMoments(0.7, 2.3)$var, gamma(1 + 2 / 2.3) / 0.7^(2 / 2.3) - m^2, tolerance = 1e-12)
  expect_equal(FoldedNormal(0.9, 0.7, 1.3)$cdf, pnorm(0.2 / 1.3) + pnorm(1.6 / 1.3) - 1, tolerance = 1e-15)
  expect_equal(FoldedNormal(1)$mean, sqrt(2 / pi), tolerance = 1e-15)
  expect_equal(Chisq1Cdf(3), pchisq(3, 1), tolerance = 1e-14)
})

test_that("HarmonicMeanEvidence and CrpsCdf closed forms", {
  ll <- c(-10.5, -12.25, -9.8)
  expect_equal(HarmonicMeanEvidence(ll)$log_evidence, log(1 / mean(exp(-ll))), tolerance = 1e-12)
  y <- 0.3
  expect_equal(CrpsCdf(pnorm, y), y * (2 * pnorm(y) - 1) + 2 * dnorm(y) - 1 / sqrt(pi), tolerance = 1e-12)
  expect_equal(CrpsCdf(function(z) min(max(z / 3, 0), 1), 2.2, breaks = c(0, 3)), (2.2^3 + 0.8^3) / 27, tolerance = 1e-12)
})

test_that("MaxEntropyDiscrete meets the moments", {
  G <- rbind(1:6, (1:6)^2)
  r <- MaxEntropyDiscrete(G, c(4.1, 19))
  expect_equal(as.numeric(G %*% r$p), c(4.1, 19), tolerance = 1e-10)
  expect_equal(log(r$p) - log(r$p[1]), as.numeric(crossprod(G - G[, 1], r$lambdas)), tolerance = 1e-10)
})

test_that("BvLogisticSimulate, DdmDrift and KingKinship", {
  r <- BvLogisticSimulate(25, 0.45, seed = 9)
  u <- .morie_random_uniform(25, seed = 9, stream = 0)
  mix <- .morie_random_uniform(25, seed = 9, stream = 1)
  z <- -log(.morie_random_uniform(25, seed = 9, stream = 2)) -
    ifelse(mix < 0.45, log(.morie_random_uniform(25, seed = 9, stream = 3)), 0)
  expect_equal(r$x, -log(z * u^0.45), tolerance = 1e-12)
  i <- 0:599
  e <- c(as.numeric((i[1:300] * 37) %% 10 < 1), as.numeric((i[301:600] * 37) %% 10 < 6))
  d <- DdmDrift(e)$drifts
  expect_true(d[1] >= 300 && d[1] < 400)
  g <- rbind(c(0, 1, 2, 1, 1, 0), c(0, 1, 2, 1, 1, 0), c(2, 1, 0, 1, 0, 2))
  K <- KingKinship(g)$kinship
  expect_equal(K[1, 2], 0.5)
  expect_equal(K[1, 3], (2 - 6) / 4 + 0.5 - 5 / 8)
})
