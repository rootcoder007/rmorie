test_that("MSM weights and fit recompute", {
  i <- 0:39
  L <- cbind(sin(i), cos(0.7 * i))
  A <- cbind(as.numeric(L[, 1] + 1.5 * sin(3.3 * i + 1) > 0), as.numeric(L[, 2] + 1.5 * cos(2.1 * i) > 0))
  y <- 1 + 0.5 * rowSums(A) + L[, 1] + 0.2 * sin(5 * i)
  r <- marginal_structural_model(y, A, L)
  lf <- function(f) stats::fitted(stats::glm(f, family = stats::binomial(), control = stats::glm.control(epsilon = 1e-14)))
  a0 <- A[, 1]
  a1 <- A[, 2]
  d0 <- lf(a0 ~ L[, 1])
  n1 <- lf(a1 ~ a0)
  d1 <- lf(a1 ~ a0 + L[, 1] + L[, 2])
  sw <- ifelse(a0 == 1, mean(a0), 1 - mean(a0)) / ifelse(a0 == 1, d0, 1 - d0) *
    ifelse(a1 == 1, n1, 1 - n1) / ifelse(a1 == 1, d1, 1 - d1)
  expect_equal(unname(r$weights), unname(sw), tolerance = 1e-8)
  expect_equal(r$estimate, unname(stats::coef(stats::lm(y ~ rowSums(A), weights = sw))[2]), tolerance = 1e-8)
})

test_that("spatial_dbscan wraps DbscanClusters", {
  P <- rbind(c(0, 0), c(0, 1), c(1, 0), c(9, 9), c(9, 8), c(8, 9), c(5, 5))
  r <- spatial_dbscan(P, eps = 1.5, min_pts = 3)
  expect_equal(r$cluster, c(1, 1, 1, 2, 2, 2, 0))
  expect_equal(r$statistic, 2)
})
