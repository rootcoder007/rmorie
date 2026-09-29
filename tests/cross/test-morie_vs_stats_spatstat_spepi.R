# Cross tests: Poisson ecological regression against glm; kernel densities against spatstat; buffer test against pbinom.

test_that("PoissonEcological equals glm(poisson) with offset", {
  i <- 0:29
  y <- c(3, 0, 5, 2, 8, 1, 4, 6, 0, 2, 7, 3, 1, 5, 9, 2, 4, 0, 3, 6, 2, 8, 1, 5, 3, 4, 7, 2, 0, 6)
  E <- 2 + 0.3 * (i %% 7)
  X <- cbind(sin(i), (i %% 3) * 0.5)
  ref <- stats::glm(y ~ X, family = stats::poisson(), offset = log(E), control = stats::glm.control(epsilon = 1e-14, maxit = 100))
  got <- PoissonEcological(y, E, X)
  expect_equal(got$beta, unname(coef(ref)), tolerance = 1e-9)
  expect_equal(got$se, unname(sqrt(diag(vcov(ref)))), tolerance = 1e-7)
  expect_equal(got$deviance, ref$deviance, tolerance = 1e-9)
})

test_that("KernelRelativeRisk densities equal spatstat density.ppp without edge correction", {
  skip_if_not_installed("spatstat.geom")
  skip_if_not_installed("spatstat.explore")
  cs <- cbind(sin(0:14) * 2, cos((0:14) * 1.3))
  ct <- cbind(sin((0:24) * 0.7) * 3, cos((0:24) * 0.4) * 2)
  pts <- rbind(c(0, 0), c(1, 1), c(-1, 0.5))
  w <- spatstat.geom::owin(c(-4, 4), c(-3, 3))
  d1 <- spatstat.explore::density.ppp(spatstat.geom::ppp(cs[, 1], cs[, 2], window = w), sigma = 0.8, at = "points",
                                      edge = FALSE, leaveoneout = FALSE)
  r <- KernelRelativeRisk(cs, ct, cs, 0.8)
  expect_equal(r$f_cases * nrow(cs), as.numeric(d1), tolerance = 1e-10)
})

test_that("BufferRateRatio p-value equals the binomial upper tail", {
  r <- BufferRateRatio(c(6, 2, 2, 3), c(100, 100, 150, 120), cbind(c(0, 5, 9, 1), 0), rbind(c(0, 0)), 1.5)
  expect_equal(r$p_value, stats::pbinom(8, 13, 220 / 470, lower.tail = FALSE), tolerance = 1e-12)
})
