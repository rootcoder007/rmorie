# MVSML chapter 8c: Bayesian kernel BLUP.

test_that("eq (8.8) conditional mode equals Henderson's BLUP", {
  set.seed(5)
  n <- 8
  M <- matrix(sample(0:2, n * 20, TRUE), nrow = n)
  G <- morie:::morie_mvsml_grm(M) + diag(0.4, n)
  set.seed(4); y <- rnorm(n, 5, 1)
  s2u <- 0.7; s2e <- 1.3
  fit <- morie:::morie_mvsml_blue_blup_v(matrix(1, n, 1), diag(n), y,
                                 s2u * G, diag(s2e, n))
  r <- morie:::morie_mvsml_bayesian_kernel_blup(y, G, s2u, s2e,
                                        mu = fit$blue[1])
  expect_equal(r$u, fit$blup, tolerance = 1e-8)     # p.282
})

test_that("eq (8.9) covariance is Z K Z'", {
  K <- matrix(c(1, 0.3, 0.3, 1), 2)
  Z <- matrix(c(1,0, 1,0, 0,1), nrow = 3, byrow = TRUE)
  Ks <- morie:::morie_mvsml_kernel_blup_replicated(Z, K)
  expect_equal(Ks[1, 1], 1, tolerance = 1e-12)
  expect_equal(Ks[1, 3], 0.3, tolerance = 1e-12)
  expect_true(morie:::morie_mvsml_is_psd(Ks)$psd)
  expect_equal(morie:::morie_mvsml_kernel_blup_replicated(Z, K, 2)[1, 1], 2,
               tolerance = 1e-12)
})

test_that("eq (8.10) interaction kernel is a Hadamard product", {
  K <- matrix(c(1, 0.3, 0.3, 1), 2)
  Z_u1 <- matrix(c(1,0, 0,1, 1,0, 0,1), nrow = 4, byrow = TRUE)
  Z_E <- matrix(c(1,0, 1,0, 0,1, 0,1), nrow = 4, byrow = TRUE)
  r <- morie:::morie_mvsml_kernel_blup_gxe(Z_u1, K, Z_E)
  expect_equal(r$K2, r$K1 * r$K_env, tolerance = 1e-12)  # p.285
  expect_equal(r$K_env[1, 2], 1, tolerance = 1e-12)
  expect_equal(r$K_env[1, 3], 0, tolerance = 1e-12)
  expect_equal(r$K2[1, 3], 0, tolerance = 1e-12)
  expect_true(morie:::morie_mvsml_is_psd(r$K1)$psd)
  expect_true(morie:::morie_mvsml_is_psd(r$K2)$psd)
})

test_that("the Hadamard product matches its definition", {
  A <- matrix(c(1, 3, 2, 4), 2); B <- matrix(c(5, 7, 6, 8), 2)
  expect_equal(morie:::morie_mvsml_hadamard(A, B), A * B)
})
