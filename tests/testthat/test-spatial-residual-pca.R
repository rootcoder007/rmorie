# Moran's I on OLS residuals against spdep::lm.morantest, and MULTISPATI
# axes against eigen() on H = Z'((W + W')/2)Z/n, on a 5 x 4 rook grid.

.sp_grid <- function() {
  nr <- 5; nc <- 4; n <- nr * nc
  W <- matrix(0, n, n)
  for (i in 0:(n - 1)) for (j in 0:(n - 1)) {
    if (abs(i %/% nc - j %/% nc) + abs(i %% nc - j %% nc) == 1) W[i + 1, j + 1] <- 1
  }
  i <- 0:(n - 1)
  x1 <- (i * 5) %% 9
  x2 <- cos(i)
  list(
    W = W, n = n, x1 = x1, x2 = x2,
    z = ((i * 37) %% 17) / 3 + 0.5 * (i %/% nc),
    y = 1 + 0.8 * x1 - 0.5 * x2 + ((i * 13) %% 7 - 3) / 2 + 0.3 * (i %/% nc)
  )
}

test_that("MoranRes projects the response onto the residual space", {
  g <- .sp_grid()
  X <- cbind(1, g$x1, g$x2)
  r <- MoranRes(g$y, g$W, X)
  # spdep 1.3 lm.morantest(lm(y ~ x1 + x2), style "B")
  expect_equal(r$i, 0.12677553905685854, tolerance = 1e-12)
  expect_equal(r$expectation, -0.040517768878611776, tolerance = 1e-12)
  expect_equal(r$variance, 0.03036576122101374, tolerance = 1e-12)
  e <- as.numeric(stats::lm.fit(X, g$y)$residuals)
  expect_equal(MoranRes(e, g$W, X)$i, r$i, tolerance = 1e-12)
})

test_that("SpatialPca orders axes from the most positive eigenvalue down", {
  g <- .sp_grid()
  D <- cbind(g$z, g$x1, g$x2, g$y)
  Z <- scale(D, scale = FALSE)
  Z <- sweep(Z, 2, sqrt(colSums(Z^2) / g$n), "/")
  H <- t(Z) %*% ((g$W + t(g$W)) / 2) %*% Z / g$n
  ref <- eigen(H, symmetric = TRUE)$values
  r <- SpatialPca(D, g$W, 4L)
  expect_equal(r$eigenvalues, ref, tolerance = 1e-12)
  expect_equal(SpatialPca(D, g$W, 1L)$eigenvalues, max(ref), tolerance = 1e-12)
  expect_true(ref[1] > 0 && ref[4] < 0 && abs(ref[4]) > abs(ref[1]))
})
