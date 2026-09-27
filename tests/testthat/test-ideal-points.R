em_votes <- function() {
  U <- .morie_random_uniform(4000, seed = 11, stream = 0)
  Z <- .morie_random_normal(4000, seed = 11, stream = 1)
  V <- matrix(0, 30, 20)
  for (i in 0:29) {
    for (j in 0:19) {
      m <- 0.3 * Z[101 + j] + (1 + Z[201 + j]) * Z[i + 1] + 0.5 * Z[301 + j] * Z[401 + i]
      V[i + 1, j + 1] <- if (U[2001 + i * 20 + j] < 0.05) NA else as.numeric(U[i * 20 + j + 1] < stats::pnorm(m))
    }
  }
  V
}

test_that("morie_spatial_voting_em_irt reproduces emIRT::binIRT", {
  V <- em_votes()
  r <- morie_spatial_voting_em_irt(V)
  # emIRT::binIRT with the same starts, makePriors and default control: 83 iterations
  expect_identical(r$iterations, 83L)
  expect_true(r$converged)
  t <- morie_spatial_voting_em_irt(V, max_iter = 20000L, tol = 1e-13, conv = "abs")
  expect_lt(abs(t$ideal_points[1, 1] + 0.5391190086), 1e-9)
  expect_lt(max(abs(t$difficulty[1:2] - c(0.6150891743, -0.4241460078))), 1e-9)
  expect_lt(abs(t$discrimination[1, 1] + 6.5721230364), 1e-9)
  expect_lt(abs(t$log_lik + 267.3624873117), 1e-9)
})

test_that("two-dimensional EM fixed point is a posterior mode", {
  V <- em_votes()
  r <- morie_spatial_voting_em_irt(V, n_dims = 2L, max_iter = 20000L, tol = 1e-12, conv = "abs")
  lp <- function(a, b, x) {
    M <- matrix(a, nrow(V), ncol(V), byrow = TRUE) + x %*% t(b)
    sum(stats::pnorm(ifelse(V == 1, M, -M), log.p = TRUE)[!is.na(V)]) -
      0.5 * sum(x^2) - 0.5 * (sum(a^2) + sum(b^2)) / 25
  }
  h <- 1e-5
  e <- matrix(0, 30, 2)
  e[1, 2] <- h
  expect_lt(abs((lp(r$difficulty, r$discrimination, r$ideal_points + e) -
                 lp(r$difficulty, r$discrimination, r$ideal_points - e)) / (2 * h)), 1e-5)
  f <- matrix(0, 20, 2)
  f[4, 2] <- h
  expect_lt(abs((lp(r$difficulty, r$discrimination + f, r$ideal_points) -
                 lp(r$difficulty, r$discrimination - f, r$ideal_points)) / (2 * h)), 1e-5)
  g <- numeric(20)
  g[6] <- h
  expect_lt(abs((lp(r$difficulty + g, r$discrimination, r$ideal_points) -
                 lp(r$difficulty - g, r$discrimination, r$ideal_points)) / (2 * h)), 1e-5)
})

test_that("morie_spatial_voting_cjr_irt gives the Python arm's Philox chain", {
  V <- em_votes()
  f <- morie_spatial_voting_cjr_irt(V, n_samples = 200L, burn_in = 50L, seed = 5)
  got <- c(f$ideal_point_mean[1:3, 1], f$alpha_mean[1:2], f$beta_mean[1, 1], f$ideal_point_sd[1, 1])
  ref <- c(-0.577500384336, -0.678132861519, -0.176501975313, -1.12013879797, 0.540891603986,
           -9.235856442311, 0.297621796656)
  expect_lt(max(abs(got - ref)), 1e-9)
  expect_identical(dim(f$ideal_point_chain), c(200L, 30L, 1L))
  expect_lt(abs(.cjr_log_phi_far(-50) - stats::pnorm(-50, log.p = TRUE)), 1e-9)
  expect_lt(abs(.cjr_log_tail_quantile(log(0.3) + stats::pnorm(-50, log.p = TRUE)) -
                  stats::qnorm(log(0.3) + stats::pnorm(-50, log.p = TRUE), log.p = TRUE)), 1e-9)
})
