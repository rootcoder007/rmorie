# Tests for SpaceTimeModels: GTWR, multiscale GWR backfitting and Gaussian hidden Markov models.

n <- 30
i <- 0:29
xy <- cbind((i * 7) %% 10 + 0.3 * sin(i), (i * 3) %% 8 + 0.2 * cos(i))
tt <- i %% 5
X <- cbind(sin(i * 0.7) + 0.1 * xy[, 1], cos(i * 1.1))
y <- 1 + (0.5 + 0.1 * xy[, 1]) * X[, 1] - 0.3 * X[, 2] + 0.2 * sin(i * 2.3)

test_that("GtwrFit is local weighted least squares", {
  r <- GtwrFit(y, X, xy, tt, 6, lam = 1, mu = 0.5)
  d <- sqrt((xy[, 1] - xy[8, 1])^2 + (xy[, 2] - xy[8, 2])^2 + 0.5 * (tt - tt[8])^2)
  w <- ifelse(d < 6, (1 - (d / 6)^2)^2, 0)
  expect_equal(r$beta[8, ], as.numeric(coef(lm(y ~ X, weights = w))), tolerance = 1e-10)
})

test_that("MgwrBackfit with huge bandwidths is OLS", {
  r <- MgwrBackfit(y, X, xy, c(1e9, 1e9, 1e9), center = FALSE)
  expect_equal(r$beta[1, ], as.numeric(coef(lm(y ~ X))), tolerance = 1e-8)
})

test_that("GaussianHmm decodes two regimes", {
  x <- c(0.1, 1.9, 2.2, -0.3, 0.4, 2.5, 2.1)
  r <- GaussianHmm(x, 2, max_iter = 50)
  expect_equal(r$states[[1]], c(0L, 1L, 1L, 0L, 0L, 1L, 1L))
  expect_gte(r$loglik, GaussianHmm(x, 2, max_iter = 1)$loglik - 1e-12)
})
