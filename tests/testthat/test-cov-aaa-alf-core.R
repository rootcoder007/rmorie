# Coverage tests for R/aaa_alf_core.R (AlphaFold 2 building blocks:
# activations, layer norm, rigid transforms, quaternion rotations).

test_that("activations, softmax, dot products, linear layer, layer norm", {
  x <- c(-800, -2, 0, 1.5, 800)
  expect_equal(alfSigm(x), plogis(x), tolerance = 1e-12)
  expect_false(anyNA(alfSigm(x)))
  expect_equal(alfRelu(x), pmax(x, 0))
  v <- c(1, 2, 3, 1000)
  expect_equal(alfSmax(v), exp(v - 1000) / sum(exp(v - 1000)), tolerance = 1e-12)
  expect_equal(sum(alfSmax(c(0.1, 0.2))), 1)
  expect_equal(alfVdot(1:3, 4:6), 32)
  expect_equal(alfVn2(c(3, 4)), 25)
  W <- matrix(1:6, 2, 3)
  expect_equal(alfLin(c(1, 0, -1), W), as.numeric(W %*% c(1, 0, -1)))
  expect_equal(alfLin(c(1, 0, -1), W, b = c(0.5, -0.5)), as.numeric(W %*% c(1, 0, -1)) + c(0.5, -0.5))
  z <- c(2, 4, 4, 5, 7)
  ln <- alfLnorm(z, g = c(1, 2, 1, 2, 1), b = rep(0.1, 5))
  zs <- (z - mean(z)) / sqrt(mean((z - mean(z))^2) + 1e-5)
  expect_equal(ln, zs * c(1, 2, 1, 2, 1) + 0.1, tolerance = 1e-12)
  expect_equal(alfLnorm(z, eps = 0), (z - mean(z)) / sqrt(mean((z - mean(z))^2)), tolerance = 1e-12)
})

test_that("quaternion rotation matrix is the Hamilton-product rotation", {
  R <- alfQ2rot(0.3, -0.5, 0.2)
  expect_equal(crossprod(R), diag(3), tolerance = 1e-12)
  expect_equal(det(R), 1, tolerance = 1e-12)
  hp <- function(p, q) c(p[1] * q[1] - sum(p[2:4] * q[2:4]),
    p[1] * q[2:4] + q[1] * p[2:4] + c(p[3] * q[4] - p[4] * q[3], p[4] * q[2] - p[2] * q[4], p[2] * q[3] - p[3] * q[2]))
  q <- c(1, 0.3, -0.5, 0.2) / sqrt(1 + 0.09 + 0.25 + 0.04)
  v <- c(0.7, -1.1, 2.3)
  rot <- hp(hp(q, c(0, v)), c(q[1], -q[2:4]))[2:4]
  expect_equal(as.numeric(R %*% v), rot, tolerance = 1e-12)
  expect_equal(alfQ2rot(0, 0, 0), diag(3))
})

test_that("rigid transforms: apply, inverse, compose, identity", {
  A <- list(R = alfQ2rot(0.2, 0.1, -0.4), t = c(1, -2, 0.5))
  B <- list(R = alfQ2rot(-0.3, 0.6, 0.1), t = c(0.3, 0.4, -1))
  x <- c(0.5, 1.5, -0.7)
  expect_equal(alfRap(A, x), as.numeric(A$R %*% x) + A$t, tolerance = 1e-12)
  expect_equal(alfRinvap(A, alfRap(A, x)), x, tolerance = 1e-12)
  Ai <- alfRinv(A)
  expect_equal(alfRap(Ai, alfRap(A, x)), x, tolerance = 1e-12)
  C <- alfRcomp(A, B)
  expect_equal(alfRap(C, x), alfRap(A, alfRap(B, x)), tolerance = 1e-12)
  I <- alfIdent()
  expect_equal(alfRap(I, x), x)
  expect_equal(alfRcomp(A, alfRinv(A))$t, c(0, 0, 0), tolerance = 1e-12)
})

test_that("one-hot binning and cross-entropy", {
  bins <- c(2.3125, 3.6, 5, 7.5, 21.6875)
  expect_equal(alfOnehot(4.2, bins), c(0, 1, 0, 0, 0))
  expect_equal(alfOnehot(4.4, bins), c(0, 0, 1, 0, 0))
  expect_equal(alfOnehot(100, bins), c(0, 0, 0, 0, 1))
  y <- c(0, 1, 0)
  p <- c(0.2, 0.7, 0.1)
  expect_equal(alfXent(y, p), -log(0.7), tolerance = 1e-12)
  expect_equal(alfXent(c(1, 0), c(0, 1)), -log(1e-12), tolerance = 1e-12)
  expect_equal(alfXent(c(0.5, 0.5), c(0.5, 0.5)), log(2), tolerance = 1e-12)
})
