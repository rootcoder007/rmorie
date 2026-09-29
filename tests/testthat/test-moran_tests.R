n <- 10
W <- 1 * (abs(outer(1:n, 1:n, "-")) == 1)
y <- c(2, 3.5, 3, 5, 4.5, 6, 5.5, 8, 7, 9.5)
s0 <- sum(W)
s1 <- 0.5 * sum((W + t(W))^2)
s2 <- sum((rowSums(W) + colSums(W))^2)
z <- y - mean(y)
I <- n / s0 * sum(z * (W %*% z)) / sum(z^2)

test_that("moments and z-score recompute", {
  expect_equal(miexp(7)$statistic, -1 / 6)
  vn <- (n^2 * s1 - n * s2 + 3 * s0^2) / (s0^2 * (n^2 - 1)) - 1 / (n - 1)^2
  expect_equal(mivar(n, s0, s1, s2)$statistic, vn, tolerance = 1e-14)
  b2 <- n * sum(z^4) / sum(z^2)^2
  num <- n * ((n^2 - 3 * n + 3) * s1 - n * s2 + 3 * s0^2) - b2 * ((n^2 - n) * s1 - 2 * n * s2 + 6 * s0^2)
  vr <- num / ((n - 1) * (n - 2) * (n - 3) * s0^2) - 1 / (n - 1)^2
  expect_equal(mivar(n, s0, s1, s2, b2)$statistic, vr, tolerance = 1e-14)
  zz <- (I + 1 / (n - 1)) / sqrt(vn)
  expect_equal(mizval(I, -1 / (n - 1), vn)$statistic, zz, tolerance = 1e-14)
  expect_equal(minorm(I, n, s0, s1, s2)$p_value, pnorm(zz, lower.tail = FALSE), tolerance = 1e-14)
  expect_equal(mizval(I, -1 / (n - 1), vn, "two.sided")$p_value, 2 * pnorm(-abs(zz)), tolerance = 1e-14)
  expect_equal(miorig(y, W)$statistic, I, tolerance = 1e-14)
  expect_equal(miorig(y, W)$variance, vn, tolerance = 1e-14)
  expect_equal(mirand(y, W)$variance, vr, tolerance = 1e-14)
  expect_equal(miml(z, W)$statistic, I, tolerance = 1e-14)
})

test_that("permutation, OLS-residual and AK tests recompute", {
  r <- mimc(y, W, nsim = 49, seed = 3)
  expect_equal(r$statistic, I, tolerance = 1e-14)
  expect_equal(r$p_value, (1 + sum(r$simulated >= r$statistic)) / 50)
  X <- cbind(1, 0:9, (((0:9) * 7) %% 5) / 2)
  M <- diag(n) - X %*% solve(crossprod(X), t(X))
  e <- as.vector(M %*% y)
  MW <- M %*% W
  E <- n / s0 * sum(diag(MW)) / (n - 3)
  expect_equal(miols(e, W, X)$expected, E, tolerance = 1e-12)
  expect_equal(miols(e, W, X)$statistic, n / s0 * sum(e * (W %*% e)) / sum(e^2), tolerance = 1e-12)
  Z <- cbind(1, (((0:9) * 3) %% 7) / 2)
  H <- cbind(Z, ((0:9)^2 %% 5) / 3)
  ak <- miiv(e, W, Z, H)
  V <- solve(t(Z) %*% H %*% solve(t(H) %*% H) %*% t(H) %*% Z)
  g <- t(Z) %*% t(W) %*% e
  phi2 <- (sum(diag((t(W) + W) %*% W)) + 4 / (sum(e^2) / n) * as.numeric(t(g) %*% V %*% g)) / ((s0 / n)^2 * n)
  mi <- n / s0 * sum(e * (W %*% e)) / sum(e^2)
  expect_equal(ak$statistic, n * mi^2 / phi2, tolerance = 1e-12)
})
