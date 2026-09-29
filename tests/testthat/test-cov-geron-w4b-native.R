# Coverage tests for the Gram-matrix helpers of R/geron_w4b_native.R:
# centring, classical-MDS double centring, pairwise distances and kernel
# PCA against prcomp on a linear kernel.

gw_X <- rbind(c(1, 2, 0.5), c(-0.5, 1, 2), c(2, -1, 1), c(0, 0.3, -1), c(1.5, 1.5, 1.5), c(-2, 0.5, 0))

test_that("Gram centring, double centring and distances", {
  K <- gw_X %*% t(gw_X)
  H <- diag(6) - 1 / 6
  expect_equal(morie_geron_center_gram(K), H %*% K %*% H, tolerance = 1e-12)
  D <- as.matrix(dist(gw_X))
  expect_equal(morie_geron_pairwise_distances(gw_X), D, ignore_attr = TRUE, tolerance = 1e-12)
  B <- morie_geron_double_center(D)
  Xc <- scale(gw_X, scale = FALSE)
  # classical MDS: -J D^2 J / 2 is the Gram matrix of the centred points
  expect_equal(B, Xc %*% t(Xc), ignore_attr = TRUE, tolerance = 1e-10)
})

test_that("kernel PCA on a linear kernel reproduces principal component scores", {
  K <- gw_X %*% t(gw_X)
  kp <- morie_geron_kernel_pca_from_gram(K, 2)
  pc <- prcomp(gw_X)
  expect_equal(abs(kp$projection), abs(unname(pc$x[, 1:2])), tolerance = 1e-10)
  expect_equal(kp$eigenvalues, pc$sdev[1:2]^2 * 5, tolerance = 1e-10)
  expect_equal(kp$alphas %*% diag(kp$eigenvalues), kp$projection, tolerance = 1e-10)
  full <- morie_geron_kernel_pca_from_gram(K, 6)
  # the centred Gram matrix has rank 3; the null directions stay zero
  expect_equal(full$projection[, 4:6], matrix(0, 6, 3))
  expect_equal(full$projection %*% t(full$projection), full$K_centered, tolerance = 1e-10)
  expect_error(morie_geron_kernel_pca_from_gram(K, 0), "n_components out of range")
  expect_error(morie_geron_kernel_pca_from_gram(matrix(1, 3, 3), 1), "no positive eigenvalue")
})
