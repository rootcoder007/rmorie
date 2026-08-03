# MVSML chapter 8d/8e: compression and RKHS estimating equations.

set.seed(5)
Xm <- matrix(sample(0:2, 10 * 25, TRUE), nrow = 10)

test_that("eq (8.11) design satisfies P P' = K (p.289)", {
  K <- morie_mvsml_kernel_matrix(Xm, "linear")
  r <- morie_mvsml_kernel_eigen_design(K)
  expect_equal(r$P %*% t(r$P), K, tolerance = 1e-7)
  expect_lte(r$rank, nrow(K))
})

test_that("Nystrom is exact when all lines are landmarks (p.290)", {
  p <- ncol(Xm)
  K <- morie_mvsml_kernel_matrix(Xm, "linear") / p
  ny <- morie_mvsml_nystrom(Xm, seq_len(nrow(Xm)))
  expect_equal(ny$Q, K, tolerance = 1e-6)
})

test_that("eq (8.12) design satisfies P P' = Q (p.291)", {
  r <- morie_mvsml_sparse_kernel_design(Xm, c(1, 4, 6, 8))
  expect_equal(r$P %*% t(r$P), r$Q, tolerance = 1e-6)
  expect_lte(ncol(r$P), 4L)
})

test_that("eq (8.6) and (8.7) give the same solution (p.276)", {
  set.seed(7)
  n <- 9
  M <- matrix(sample(0:2, n * 20, TRUE), nrow = n)
  K <- morie_mvsml_grm(M) + diag(0.5, n)
  C <- matrix(1, n, 1)
  y <- rnorm(n, 5, 1)
  a <- morie_mvsml_rkhs_mixed_equations(C, K, y, 0.7, 1.3,
                                        "direct")
  b <- morie_mvsml_rkhs_mixed_equations(C, K, y, 0.7, 1.3,
                                        "reduced")
  expect_equal(a$theta, b$theta, tolerance = 1e-7)
  expect_equal(a$beta, b$beta, tolerance = 1e-7)
  # reparameterization II: u = K beta
  expect_equal(a$u, as.numeric(K %*% a$beta), tolerance = 1e-9)
  expect_equal(a$sigma2_beta, 1 / 0.7, tolerance = 1e-12)
})
