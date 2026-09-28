# Tests for SpatialRegExtra: spatial regression extras and the Satorra-Bentler scaled chi-square.

g <- expand.grid(c = 0:5, r = 0:3)
A <- outer(seq_len(24), seq_len(24), function(i, j) as.numeric(abs(g$c[i] - g$c[j]) + abs(g$r[i] - g$r[j]) == 1))
W <- A / rowSums(A)
i <- 0:23
X <- cbind(sin(i * 0.7) + 0.1 * (i %% 5), cos(i * 1.3) * (1 + 0.02 * i))
y <- 1 + 0.8 * X[, 1] - 0.5 * X[, 2] + 0.4 * sin(i * 2.1) + 0.3 * cos(i * i * 0.1)

test_that("MoranPermutationTest statistic", {
  z <- y - mean(y)
  r <- MoranPermutationTest(y, A, nsim = 9, seed = 2)
  expect_equal(r$statistic, 24 / sum(A) * sum(z * (A %*% z)) / sum(z^2), tolerance = 1e-12)
  expect_equal(r$p_value, (1 + sum(r$simulated >= r$statistic)) / 10)
})

test_that("SpautolmFit and S2slsLag", {
  r <- SpautolmFit(y, X, W)
  o <- SpautolmFit(y, X, W, bounds = c(r$lambda_ + 1e-4, r$lambda_ + 1e-4 + 1e-12))
  expect_lte(o$loglik, r$loglik)
  s <- S2slsLag(y, X, W)
  expect_equal(s$residuals, y - as.numeric(cbind(1, X, W %*% y) %*% s$coefficients), tolerance = 1e-12)
})

test_that("GmErrorHet, SpatialJTest and SatorraBentler", {
  h <- GmErrorHet(y, X, W)
  expect_true(abs(h$rho) < 0.9 && all(h$se > 0))
  B <- outer(seq_len(24), seq_len(24), function(i, j) as.numeric(abs(g$c[i] - g$c[j]) + abs(g$r[i] - g$r[j]) == 2))
  j <- SpatialJTest(y, X, W, X, B / rowSums(B))
  expect_equal(j$statistic, j$coefficients[5] / j$se[5])
  Xd <- cbind(sin(0:29), cos((0:29) * 1.7) + 0.2 * (0:29))
  expect_equal(SatorraBentler(Xd, rbind(c(1, 0.3), c(0.3, 2)), diag(3), 1, 1)$scaling, 0, tolerance = 1e-12)
})
