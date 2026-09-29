# Tests for MiscKit: Crime Severity Index, U-learner, repeated k-fold, FastICA, rStress MDS.

test_that("CrimeSeverityIndex and RepeatedKfoldIndices", {
  w <- CsiWeights(c(0.5, 0.9, 0.1), c(30, 400, 10))
  cnt <- rbind(c(10, 2, 5), c(8, 3, 7), c(12, 1, 4))
  r <- CrimeSeverityIndex(cnt, w, c(1000, 1100, 1050), base = 1)
  rates <- as.numeric(cnt %*% w) / c(1000, 1100, 1050)
  expect_equal(r$index, 100 * rates / rates[2])
  f <- RepeatedKfoldIndices(23, 5, 4, seed = 1)
  expect_true(all(vapply(f, function(v) diff(range(tabulate(v + 1, 5))) <= 1, TRUE)))
})

test_that("ULearnerCate, FastIca and RstressMds", {
  i <- 0:79
  X <- cbind(sin(i * 0.37), cos(i * 0.91))
  d <- as.numeric(sin(i * 1.7) > 0)
  y <- (1.5 + 0.8 * X[, 1]) * d + X[, 2]
  r <- ULearnerCate(y, d, X, l2 = 1e-8)
  expect_lt(abs(r$coefficients[1] - 1.5), 0.2)
  k <- 0:199
  S <- cbind(sin(k * 0.13), ((k * 7) %% 11) / 5 - 1, cos(k * 0.029)^3)
  ica <- FastIca(S %*% rbind(c(1, 0.3, 0.6), c(0.5, -1, 0.2), c(0.2, 0.4, -1)), 3, tol = 1e-10, max_iter = 500)
  expect_equal(crossprod(ica$S) / 200, diag(3), tolerance = 1e-8)
  P <- rbind(c(0, 0), c(1, 0), c(0, 2), c(1.5, 1))
  expect_lt(RstressMds(as.matrix(dist(P)), r = 1)$stress, 1e-6)
})
