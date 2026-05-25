# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 28 -- directly unit-tests the optimiser objectives that
# were refactored out of their parent functions' closures. The parent
# optimisers (nlminb / optimize) are box-constrained, so they never
# probe the out-of-domain points the guard branches defend against;
# calling the extracted .<fn>_negll helpers directly with crafted
# arguments is the only way those guard lines execute.

test_that("ARCH-in-mean objective: domain guard + valid path", {
  y <- rnorm(40)
  expect_equal(morie:::.archm_negll(c(0, 0, -1, 0.2), y, 40L), 1e10) # omega<=0
  expect_equal(morie:::.archm_negll(c(0, 0, 1, 1.5), y, 40L), 1e10) # alpha>=1
  expect_true(is.finite(morie:::.archm_negll(c(0, 0, 1, 0.2), y, 40L)))
})

test_that("EGARCH objective: stationarity guard + valid path", {
  r <- rnorm(40)
  EZ <- sqrt(2 / pi)
  expect_equal(morie:::.egrch_negll(c(0, 0.1, 0, 1.5), r, 40L, EZ), 1e10)
  expect_true(is.finite(morie:::.egrch_negll(c(0, 0.1, 0, 0.5), r, 40L, EZ)))
})

test_that("MIDAS SSE objective: theta guard + non-finite guard + valid", {
  set.seed(28)
  X <- matrix(rnorm(20), 5, 4)
  Y <- rnorm(5)
  expect_equal(morie:::.midas_sse(c(0, 1, -1, 2), X, Y, 4L), 1e10) # t1<=0
  expect_equal(morie:::.midas_sse(c(1e308, 1e308, 1.5, 2), X, Y, 4L), 1e10)
  expect_true(is.finite(morie:::.midas_sse(c(0, 1, 1.5, 2), X, Y, 4L)))
})

test_that("SAR-lag objective: negative-det + singular guards + valid path", {
  e0 <- rnorm(4)
  e1 <- rnorm(4)
  I4 <- diag(4)
  # W = diag(2,0,0,0), rho = 1  ->  A = diag(-1,1,1,1), det = -1 (sign -1)
  expect_equal(
    morie:::.sarla_negll(1, e0, e1, 4L, I4, diag(c(2, 0, 0, 0))), 1e12
  )
  # W = I, rho = 1  ->  A = 0, singular: determinant() reports sign +1 but
  # modulus -Inf -- the !is.finite(modulus) guard must catch it.
  expect_equal(morie:::.sarla_negll(1, e0, e1, 4L, I4, I4), 1e12)
  expect_true(is.finite(
    morie:::.sarla_negll(0.2, e0, e1, 4L, I4, diag(c(0.5, 0.5, 0.5, 0.5)))
  ))
})

test_that("SAR-error objective: null-beta, det-sign and zero-variance guards", {
  I4 <- diag(4)
  X <- matrix(rnorm(8), 4, 2)
  y <- rnorm(4)
  # A = 0  ->  crossprod(AX) singular  ->  solve() errors  ->  beta NULL
  expect_equal(morie:::.sarre_negll(1, I4, I4, X, y, 4L), 1e12)
  # A = diag(-1,1,1,1): full rank, det = -1  ->  the det-sign guard
  expect_equal(
    morie:::.sarre_negll(1, I4, diag(c(2, 0, 0, 0)), X, y, 4L), 1e12
  )
  # lambda = 0 -> A = I; y = 2 * X exactly. The normal equations give
  # beta = 28/14 = 2 (an exact IEEE division), so the residual is the
  # exact zero vector and sigma2 == 0 -- the zero-variance guard.
  expect_equal(
    morie:::.sarre_negll(
      0, diag(3), diag(3),
      matrix(c(1, 2, 3), 3, 1), c(2, 4, 6), 3L
    ), 1e12
  )
})

test_that("spatial-GLM objective: non-PD covariance guard + valid path", {
  # a distance matrix with a large negative entry -> exp(-D/phi) not PD
  Dbad <- matrix(c(0, -100, -100, 0), 2, 2)
  expect_equal(
    morie:::.sglm_negll(0, Dbad, 2L, matrix(1, 2, 1), c(1, 2)),
    1e12
  )
  Dok <- as.matrix(dist(1:6))
  expect_true(is.finite(
    morie:::.sglm_negll(0, Dok, 6L, cbind(1, 1:6), as.numeric(1:6) + rnorm(6))
  ))
})

test_that("spatial-mixed REML objective: covariance + information guards", {
  Dbad <- matrix(c(0, -100, -100, 0), 2, 2)
  # non-PD Sigma -> chol(Sigma) fails
  expect_equal(morie:::.smixd_negreml(
    c(0, 0), Dbad, 2L, matrix(1, 2, 1),
    c(1, 2), 1L
  ), 1e12)
  Dok <- as.matrix(dist(1:6))
  # a zero design column -> crossprod(Xw) singular -> chol(XtSiX) fails
  expect_equal(morie:::.smixd_negreml(
    c(0, 0), Dok, 6L, cbind(1:6, 0),
    rnorm(6), 2L
  ), 1e12)
  # single-column X with y = 2 * X: whitening is linear, so yw = 2 * Xw
  # exactly and the residual is the exact zero vector -> sigma2 == 0.
  expect_equal(
    morie:::.smixd_negreml(
      c(0, 0), as.matrix(dist(c(0, 1, 2))), 3L,
      matrix(c(1, 2, 3), 3, 1), c(2, 4, 6), 1L
    ), 1e12
  )
})

test_that("DCC objective: parameter guard + indefinite-correlation guard", {
  set.seed(28)
  Z <- matrix(rnorm(2 * 3), 2, 3)
  expect_equal(morie:::.dccmd_negll(c(-1, 0.5), crossprod(Z) / 2, 2L, Z), 1e10)
  # Q_bar = [[1,2],[2,1]] -> running correlation has determinant -3 (sign -1)
  Z2 <- matrix(rnorm(2), 1, 2)
  expect_equal(
    morie:::.dccmd_negll(c(0.02, 0.95), matrix(c(1, 2, 2, 1), 2, 2), 1L, Z2),
    1e10
  )
  # Q_bar = all-ones is rank-1 -> running correlation is singular (det 0);
  # the !is.finite(modulus) guard must catch it before solve(R) errors.
  expect_equal(
    morie:::.dccmd_negll(c(0.02, 0.95), matrix(1, 2, 2), 1L, Z2), 1e10
  )
})

test_that("Horowitz objectives: zero-norm guard + valid path", {
  set.seed(28)
  X <- matrix(rnorm(20), 10, 2)
  ys <- rep(c(-1, 1), 5)
  y <- rnorm(10)
  expect_equal(morie:::.hrzb2_loss(c(0, 0), X, ys, 0.5), 1e12)
  expect_true(is.finite(morie:::.hrzb2_loss(c(1, 1), X, ys, 0.5)))
  expect_equal(morie:::.hrzi1_obj(c(0, 0), X, y, 0.5), 1e12)
  expect_true(is.finite(morie:::.hrzi1_obj(c(1, 1), X, y, 0.5)))
})

# ---- Horowitz parent functions: the sign-flip lines -----------------------
# `if (beta0[1] < 0) beta0 <- -beta0` only executes its assignment when
# the first lm.fit coefficient is negative; data with y anti-correlated
# to the first covariate guarantees that.

test_that("Horowitz estimators run the negative-coefficient sign flip", {
  set.seed(282)
  n <- 60L
  X <- matrix(rnorm(n * 2), n, 2)
  y_neg <- as.numeric(-2 * X[, 1] + 0.3 * X[, 2] + rnorm(n)) # coef1 < 0
  yb_neg <- rbinom(n, 1, plogis(-2 * X[, 1]))
  expect_true(is.list(morie:::hrzb1(X, yb_neg)))
  expect_true(is.list(morie:::hrzb2(X, yb_neg)))
  expect_true(is.list(morie:::hrzi1(X, y_neg)))
  expect_true(is.list(morie:::hrzc1(X, abs(y_neg) + 0.1)))
})
