# MVSML chapter 6: R tests mirroring the Python checks.

test_that("BRR hyperparameters match the BGLR defaults (p.175)", {
  set.seed(1); y <- rnorm(40, 5, 2)
  hp <- morie_brr_hyper(y, R2 = 0.5, nu = 5, nu_beta = 5)
  expect_equal(hp$S, var(y) * 0.5 * 7, tolerance = 1e-12)
  expect_equal(hp$S_beta, var(y) * 0.5 * 7, tolerance = 1e-12)
  hp2 <- morie_brr_hyper(y, sum_var_x = 4)
  expect_equal(hp2$S_beta, hp$S_beta / 4, tolerance = 1e-12)
})

test_that("the BRR Gibbs sampler recovers a known signal", {
  set.seed(7)
  X <- matrix(rnorm(40 * 4), nrow = 40)
  truth <- c(2, 0, -1.5, 0)
  y <- as.numeric(X %*% truth) + rnorm(40, 0, 0.3)
  r <- morie_brr_gibbs(y, X, n_iter = 1500L, burn_in = 400L)
  expect_lt(abs(r$beta[1] - 2), 0.3)
  expect_lt(abs(r$beta[3] + 1.5), 0.3)
  expect_lt(abs(r$beta[2]), 0.25)
  expect_equal(r$n_kept, 1100L)
})

test_that("GBLUP is the BRR on the Cholesky factor (p.177)", {
  set.seed(11)
  n <- 12
  A <- matrix(rnorm(n * 6), nrow = n)
  G <- morie_grm(A) + diag(0.3, n)
  y <- rnorm(n, 5, 1)
  L <- morie_chol_lower(G)
  direct <- morie_brr_gibbs(y, L, n_iter = 600L,
                                  burn_in = 200L, seed = 3L)
  r <- morie_bayes_gblup(y, G, n_iter = 600L,
                               burn_in = 200L, seed = 3L)
  expect_equal(r$mu, direct$mu, tolerance = 1e-12)
  expect_equal(r$g, as.numeric(L %*% direct$beta),
               tolerance = 1e-12)
  expect_equal(L %*% t(L), G, tolerance = 1e-9)
})

test_that("eq (6.5)/(6.7) covariances are Z G Z'", {
  Z <- matrix(c(1,0, 1,0, 0,1, 0,1), nrow = 4, byrow = TRUE)
  G <- matrix(c(1, 0.4, 0.4, 1), nrow = 2)
  K <- morie_rkhs_cov(Z, G)$K_L
  expect_equal(K[1, 1], 1, tolerance = 1e-12)
  expect_equal(K[1, 3], 0.4, tolerance = 1e-12)
  full <- morie_rkhs_cov(Z, G, Z_LE = diag(4),
                               I_env = diag(2))
  expect_equal(dim(full$K_LE), c(4L, 4L))
})

test_that("the extended predictor stacks blocks in eq (6.6) order", {
  n <- 6
  X_E <- rbind(matrix(rep(c(1, 0), 3), nrow = 3, byrow = TRUE),
               matrix(rep(c(0, 1), 3), nrow = 3, byrow = TRUE))
  Xm <- matrix(rep(c(0.5, -0.5, 1), 6), nrow = 6, byrow = TRUE)
  r <- morie_extended_predictor(n, X_E = X_E, X = Xm)
  expect_equal(unname(r$widths), c(1L, 2L, 3L))
  expect_equal(r$n_columns, 6L)
  expect_equal(as.numeric(r$design[1, 1:3]), c(1, 1, 0))
})

test_that("the scaled inverse chi-square has mean S/(nu-2)", {
  set.seed(2)
  d <- morie_scaled_inv_chisq(20, 40, n = 4000L)
  expect_equal(mean(d), 40 / 18, tolerance = 0.3)
})
