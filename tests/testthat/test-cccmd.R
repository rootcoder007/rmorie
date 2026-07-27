
test_that("morie_ccc_multivariate_garch returns a valid correlation matrix", {
  set.seed(42)
  x <- matrix(rnorm(300), 100, 3)
  r <- morie_ccc_multivariate_garch(x)
  expect_equal(dim(r$R), c(3L, 3L))
  expect_equal(diag(r$R), rep(1, 3), tolerance = 1e-10)
  expect_equal(r$R, t(r$R))
  # H_t = D_t R D_t is a covariance matrix only if R is positive definite.
  expect_true(all(eigen(r$R, symmetric = TRUE, only.values = TRUE)$values > 0))
})

test_that("morie_ccc_multivariate_garch recovers a known correlation", {
  set.seed(7)
  n <- 400
  z1 <- rnorm(n)
  z2 <- 0.7 * z1 + sqrt(1 - 0.7^2) * rnorm(n)
  r <- morie_ccc_multivariate_garch(cbind(z1, z2))
  expect_equal(r$R[1, 2], 0.7, tolerance = 0.12)
})

test_that("morie_ccc_multivariate_garch gives positive variances and finite loglik", {
  set.seed(1)
  r <- morie_ccc_multivariate_garch(matrix(rnorm(200), 100, 2))
  expect_true(all(r$conditional_variance > 0))
  expect_equal(r$sigmas, sqrt(r$conditional_variance))
  expect_true(is.finite(r$loglik))
})

test_that("morie_ccc_multivariate_garch rejects a single series", {
  expect_error(morie_ccc_multivariate_garch(matrix(rnorm(100), 100, 1)),
               "Need n>=30, k>=2")
})
