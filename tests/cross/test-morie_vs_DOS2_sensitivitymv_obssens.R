# Cross tests: U-statistic bounds and the crosscut test against DOS2; amplification against sensitivitymv.

test_that("UStatisticSensitivity equals DOS2::senU", {
  skip_if_not_installed("DOS2")
  i <- 0:39
  d <- sin(i * 1.7) * 2 + 0.6 + 0.1 * (i %% 4)
  d2 <- c(round(d, 1), 0, 0)
  dl <- sin((0:69) * 1.3) + 0.3
  for (g in c(1, 1.3, 2)) {
    expect_equal(UStatisticSensitivity(d, g, m = 8, m1 = 7, m2 = 8)$p_value, DOS2::senU(d, g, m = 8, m1 = 7, m2 = 8)$pval,
                 tolerance = 1e-12)
    expect_equal(UStatisticSensitivity(d2, g)$p_value, DOS2::senU(d2, g)$pval, tolerance = 1e-12)
    expect_equal(UStatisticSensitivity(dl, g, m = 5, m1 = 4, m2 = 5)$p_value,
                 DOS2::senU(dl, g, m = 5, m1 = 4, m2 = 5)$pval, tolerance = 1e-12)
    expect_equal(UStatisticSensitivity(d, g, m = 3, m1 = 2, m2 = 3, alternative = "twosided")$p_value,
                 DOS2::senU(d, g, m = 3, m1 = 2, m2 = 3, alternative = "twosided")$pval, tolerance = 1e-12)
  }
})

test_that("CrosscutTest equals DOS2::crosscut", {
  skip_if_not_installed("DOS2")
  skip_if_not_installed("sensitivity2x2xk")
  j <- 0:119
  x <- (j * 37) %% 101 + 0.5 * sin(j)
  y <- 0.02 * x + cos(j * 2.1)
  for (ct in c(0.2, 0.25, 0.5)) for (g in c(1, 1.5)) {
    ref <- DOS2::crosscut(x, y, ct = ct, gamma = g)
    got <- CrosscutTest(x, y, ct, g)
    expect_equal(as.numeric(got$table), as.numeric(ref$table))
    # BiasedUrn::dFNCHypergeo, used by sensitivity2x2xk::mh, works to a relative precision of 1e-7
    expect_equal(got$p_value, ref$output$pval, tolerance = 1e-7)
  }
})

test_that("AmplifyGamma equals sensitivitymv::amplify", {
  skip_if_not_installed("sensitivitymv")
  lam <- c(1.8, 2.5, 4, 10)
  expect_equal(AmplifyGamma(1.7, lam), as.numeric(sensitivitymv::amplify(1.7, lam)), tolerance = 1e-12)
})
