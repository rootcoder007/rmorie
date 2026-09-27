gm_w <- function(n = 8) {
  W <- 1 * (abs(outer(seq_len(n), seq_len(n), "-")) == 1)
  W / rowSums(W)
}
gm_y <- c(1, 2.2, 1.4, 3.1, .9, 2, 2.6, 1.1)
gm_x <- cbind(1, c(.1, .6, .2, .9, .3, .5, .8, .4))

test_that("KPMoments follows the Kelejian-Prucha definitions", {
  u <- c(.3, -.2, .5, -.1, .4, -.6, .2, -.5)
  W <- gm_w()
  m <- KPMoments(u, W)
  wu <- as.vector(W %*% u)
  expect_equal(m$g, c(sum(u^2), sum(wu^2), sum(u * wu)) / 8, tolerance = 1e-15)
  expect_equal(m$G[, 3], c(1, sum(W^2) / 8, 0), tolerance = 1e-15)
})

test_that("GMErrorSAR solves the moment criterion and fits FGLS", {
  W <- gm_w()
  r <- GMErrorSAR(gm_y, gm_x, W)
  lam <- r$lambda
  expect_equal(round(lam, 6), -0.042117)
  e <- as.vector(stats::lm.fit(gm_x, gm_y)$residuals)
  m <- KPMoments(e, W)
  c3 <- m$G[, 3]
  rr <- m$g - m$G[, 1] * lam - m$G[, 2] * lam^2
  d1 <- -m$G[, 1] - 2 * m$G[, 2] * lam
  expect_lt(abs(2 * sum(rr * d1) - 2 * sum(c3 * rr) * sum(c3 * d1) / sum(c3^2)), 1e-12)
  B <- gm_x - lam * W %*% gm_x
  expect_equal(r$coefficients, unname(stats::lm.fit(B, gm_y - lam * as.vector(W %*% gm_y))$coefficients),
               tolerance = 1e-12)
  expect_gt(r$lambda_se, 0)
  sr <- GMErrorSAR(gm_y, gm_x, W, lambda_se_method = "spatialreg")
  expect_identical(sr$coefficients, r$coefficients)
  expect_false(isTRUE(all.equal(sr$lambda_se, r$lambda_se)))
})

test_that("GS2SLSSAC matches its documented value and needs a regressor", {
  r <- GS2SLSSAC(gm_y, gm_x, gm_w())
  expect_equal(round(r$rho, 6), -0.561259)
  expect_equal(r$coefficients[1], r$rho)
  expect_error(GS2SLSSAC(gm_y, matrix(1, 8, 1), gm_w()))
})
