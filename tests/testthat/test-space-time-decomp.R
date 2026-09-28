# Tests for SpaceTimeDecomp: tensor decompositions, matrix factorisation, Haar wavelets, harmonic trends.

a1 <- c(1, 2, -1, 0.5)
b1 <- c(0.3, 1, 2)
c1 <- c(1, -1, 0.5, 2, 1.5)
a2 <- c(0.5, -1, 1, 2)
b2 <- c(1, 0.2, -0.7)
c2 <- c(0.4, 1, 1, -0.5, 0.3)
T3 <- 3 * outer(outer(a1, b1), c1) + outer(outer(a2, b2), c2)

test_that("CpAls recovers an exact rank-two tensor and TuckerHooi is exact at full rank", {
  r <- CpAls(T3, 2, max_iter = 2000, tol = 1e-15)
  fit <- r$weights[1] * outer(outer(r$A[, 1], r$B[, 1]), r$C[, 1]) + r$weights[2] * outer(outer(r$A[, 2], r$B[, 2]), r$C[, 2])
  expect_equal(fit, T3, tolerance = 1e-6)
  tk <- TuckerHooi(T3, c(4, 3, 5), max_iter = 2)
  expect_equal(crossprod(tk$factors[[1]]), diag(4), tolerance = 1e-10)
  expect_equal(tk$fit, 1, tolerance = 1e-6)
})

test_that("MatrixFactorizationAls completes a rank-one field", {
  X <- outer(c(1, 2, -1, 0.5, 3), c(0.5, -1, 2, 1))
  Xm <- X
  Xm[2, 3] <- NA
  r <- MatrixFactorizationAls(Xm, 1, l2 = 0, max_iter = 5000, tol = 1e-15)
  expect_equal(r$fitted[2, 3], X[2, 3], tolerance = 1e-5)
})

test_that("Haar transforms preserve energy and the MRA is exact", {
  x <- sin((0:15) * 0.4) * 3 + 0.1 * (0:15)^2
  m <- HaarMra(x, 3)
  expect_equal(Reduce(`+`, m$details) + m$smooth, x, tolerance = 1e-12)
  F <- outer(0:7, 0:7, function(i, j) sin(i * 0.3 + j * 0.8) + 0.05 * i * j)
  m2 <- HaarMra2d(F, 2)
  tot <- m2$smooth
  for (lv in m2$details) tot <- tot + lv$h + lv$v + lv$d
  expect_equal(tot, F, tolerance = 1e-12)
  r <- HarmonicRegression(2 + 1.5 * cos(2 * pi * (0:35) / 12 - 0.4), 0:35, 12, 1, trend = FALSE)
  expect_equal(c(r$amplitude, r$phase), c(1.5, 0.4), tolerance = 1e-10)
})
