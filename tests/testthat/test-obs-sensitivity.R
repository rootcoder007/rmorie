# Tests for ObsSensitivity: Rosenbaum sensitivity analysis for matched observational studies.

test_that("amplification round trip", {
  lam <- c(1.8, 2.5, 10)
  dl <- AmplifyGamma(1.7, lam)
  expect_equal(GammaFromLambdaDelta(lam, dl)$gamma, rep(1.7, 3), tolerance = 1e-12)
  expect_equal(GammaFromLambdaDelta(2, 3)$abz_bounds[[1]], c(0.25, 0.75))
})

test_that("UStatisticSensitivity with (2, 2, 2) is the signed-rank normal bound", {
  d <- sin((0:29) * 1.7) * 2 + 0.6 + 0.1 * ((0:29) %% 4)
  sc <- (rank(abs(d)) - 1) / choose(30, 2)
  pr <- 1.3 / 2.3
  z <- (sum(sc[d > 0]) - pr * sum(sc)) / sqrt(sum(sc^2) * pr * (1 - pr))
  expect_equal(UStatisticSensitivity(d, 1.3)$p_value, 1 - pnorm(z), tolerance = 1e-14)
})

test_that("CrosscutTest and CrosscutDesignSensitivity", {
  j <- 0:119
  x <- (j * 37) %% 101 + 0.5 * sin(j)
  y <- 0.02 * x + cos(j * 2.1)
  r <- CrosscutTest(x, y, 0.25)
  tb <- r$table
  expect_equal(r$p_value, phyper(tb[1, 1] - 1, sum(tb[1, ]), sum(tb[2, ]), sum(tb[, 1]), lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(round(CrosscutDesignSensitivity(c(0.1, 0.3, 0.5), rep(0.25, 3)), 1), c(1.9, 7.7, 44.5))
})
