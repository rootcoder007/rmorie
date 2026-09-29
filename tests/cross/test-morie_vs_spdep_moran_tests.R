skip_if_not_installed("spdep")

set.seed(11)
n <- 30
xy <- cbind(stats::runif(n), stats::runif(n))
nb <- spdep::knn2nb(spdep::knearneigh(xy, k = 4))
lw <- spdep::nb2listw(nb, style = "W")
W <- spdep::listw2mat(lw)
X <- cbind(1, stats::rnorm(n), stats::runif(n))
y <- as.vector(X %*% c(1, 2, -1) + stats::rnorm(n))

test_that("miorig and mirand match spdep::moran.test", {
  a <- spdep::moran.test(y, lw, randomisation = FALSE)
  b <- spdep::moran.test(y, lw, randomisation = TRUE)
  expect_equal(miorig(y, W)$statistic, unname(a$estimate[1]), tolerance = 1e-12)
  expect_equal(miorig(y, W)$variance, unname(a$estimate[3]), tolerance = 1e-12)
  expect_equal(miorig(y, W)$p_value, a$p.value, tolerance = 1e-10)
  expect_equal(mirand(y, W)$variance, unname(b$estimate[3]), tolerance = 1e-12)
  expect_equal(mirand(y, W)$p_value, b$p.value, tolerance = 1e-10)
  S <- spdep::spweights.constants(lw)
  expect_equal(minorm(unname(a$estimate[1]), n, S$S0, S$S1, S$S2)$statistic, unname(a$statistic), tolerance = 1e-10)
})

test_that("miols matches spdep::lm.morantest", {
  f <- stats::lm(y ~ X[, 2] + X[, 3])
  a <- spdep::lm.morantest(f, lw)
  r <- miols(stats::residuals(f), W, X)
  expect_equal(r$statistic, unname(a$estimate[1]), tolerance = 1e-12)
  expect_equal(r$expected, unname(a$estimate[2]), tolerance = 1e-12)
  expect_equal(r$variance, unname(a$estimate[3]), tolerance = 1e-12)
})

test_that("mimc statistic matches spdep::moran.mc", {
  a <- spdep::moran.mc(y, lw, nsim = 99)
  expect_equal(mimc(y, W, nsim = 19)$statistic, unname(a$statistic), tolerance = 1e-12)
})
