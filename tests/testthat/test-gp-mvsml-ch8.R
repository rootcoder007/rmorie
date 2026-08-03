# MVSML chapter 8: RKHS and kernels.

X <- matrix(c(0,0, 1,0, 0,1, 1,1, 0.5,0.5), ncol = 2, byrow = TRUE)
Y <- c(1, 2, 2, 3, 2)
XA <- matrix(c(1,0, 0,1, 1,1, -1,0.5), ncol = 2, byrow = TRUE)

test_that("kernels are symmetric and positive semi-definite", {
  for (k in c("linear", "gaussian", "polynomial", "exponential")) {
    K <- morie:::morie_mvsml_kernel_matrix(X, kernel = k)
    expect_equal(K, t(K), tolerance = 1e-12)
    expect_true(morie:::morie_mvsml_is_psd(K)$psd)
  }
  bad <- matrix(c(1, 2, 2, 1), 2)
  expect_false(morie:::morie_mvsml_is_psd(bad)$psd)
})

test_that("the linear kernel is the inner product (p.255)", {
  K <- morie:::morie_mvsml_kernel_matrix(X, kernel = "linear")
  expect_equal(K, X %*% t(X), tolerance = 1e-12)
})

test_that("eq (8.2) representer form matches the definition", {
  K <- morie:::morie_mvsml_kernel_matrix(X, kernel = "linear")
  beta <- c(0.1, -0.2, 0.3, 0, 0.4)
  pred <- morie:::morie_mvsml_rkhs_predict(K, beta, 0.5)
  expect_equal(pred, as.numeric(0.5 + K %*% beta), tolerance = 1e-12)
  expect_equal(morie:::morie_mvsml_rkhs_norm(beta, K),
               sum(outer(beta, beta) * K), tolerance = 1e-12)
})

test_that("eq (8.3) fit shrinks with lambda", {
  K <- morie:::morie_mvsml_kernel_matrix(X, kernel = "gaussian", gamma = 0.5)
  small <- morie:::morie_mvsml_rkhs_fit(K, Y, lambda = 0.01)
  large <- morie:::morie_mvsml_rkhs_fit(K, Y, lambda = 10)
  expect_lt(morie:::morie_mvsml_rkhs_norm(large$beta, K),
            morie:::morie_mvsml_rkhs_norm(small$beta, K))
  expect_gt(small$penalty, 0)
})

test_that("eq (8.4) arc-cosine kernel properties (p.265)", {
  K <- morie:::morie_mvsml_arccos_kernel(XA)
  expect_equal(diag(K), rowSums(XA^2), tolerance = 1e-12)
  expect_equal(morie:::morie_mvsml_arccos_kernel(matrix(c(1, 2), 1),
                                         Z = matrix(c(-1, -2), 1))[1],
               0, tolerance = 1e-12)
  # orthogonal unit inputs: theta = pi/2, J = 1, so AK = 1/pi
  expect_equal(morie:::morie_mvsml_arccos_kernel(matrix(c(1, 0), 1),
                                         Z = matrix(c(0, 1), 1))[1],
               1 / pi, tolerance = 1e-12)
  expect_true(morie:::morie_mvsml_is_psd(K)$psd)
  # heterogeneous diagonal, unlike the Gaussian kernel
  expect_gt(diff(range(diag(K))), 1e-6)
  G <- morie:::morie_mvsml_kernel_matrix(XA, kernel = "gaussian")
  expect_lt(diff(range(diag(G))), 1e-12)
})

test_that("eq (8.5) deep recursion reduces to (8.4) at depth 1", {
  a <- morie:::morie_mvsml_arccos_kernel(XA, depth = 1L)
  b <- morie:::morie_mvsml_arccos_kernel(XA, depth = 2L)
  expect_true(morie:::morie_mvsml_is_psd(b)$psd)
  expect_false(isTRUE(all.equal(a, b)))
  expect_equal(morie:::morie_mvsml_arccos_kernel(XA, depth = 1L), a,
               tolerance = 1e-12)
  # median normalization as in the book's R code
  K <- morie:::morie_mvsml_arccos_kernel(XA, normalize_median = TRUE)
  expect_equal(K[1, 1], a[1, 1] / median(a), tolerance = 1e-12)
})
