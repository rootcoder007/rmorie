# MVSML chapter 7b: multinomial and Poisson.

test_that("eq (7.6) probabilities normalize and use the baseline", {
  X <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  P <- morie_mvsml_multinomial_probs(X, c(0.5, -0.5),
                                     matrix(c(1, 0, 0, 1), 2,
                                            byrow = TRUE))
  expect_equal(ncol(P), 3L)
  expect_equal(rowSums(P), c(1, 1), tolerance = 1e-12)
  d <- exp(1.5) + exp(-0.5) + 1
  expect_equal(P[1, 1], exp(1.5) / d, tolerance = 1e-12)
  expect_equal(P[1, 3], 1 / d, tolerance = 1e-12)
})

test_that("eq (7.8) log-likelihood matches the probabilities", {
  X <- matrix(c(1,0, 0,1, 1,1), nrow = 3, byrow = TRUE)
  y <- c(0, 1, 2)
  b0 <- c(0.5, -0.5); B <- matrix(c(1,0, 0,1), 2, byrow = TRUE)
  P <- morie_mvsml_multinomial_probs(X, b0, B)
  hand <- sum(log(P[cbind(1:3, y + 1)]))
  expect_equal(morie_mvsml_multinomial_loglik(X, y, b0, B), hand,
               tolerance = 1e-12)
})

test_that("eq (7.7) and (7.10) penalties are quadratic and L1", {
  X <- matrix(c(1,0, 0,1, 1,1), nrow = 3, byrow = TRUE)
  y <- c(0, 1, 2)
  b0 <- c(0.5, -0.5); B <- matrix(c(1,-2, 0,1), 2, byrow = TRUE)
  ridge <- morie_mvsml_penalized_multinomial(X, y, b0, B, 2)
  lasso <- morie_mvsml_penalized_multinomial(X, y, b0, B, 2,
                                             penalty = "lasso")
  expect_equal(ridge$penalty, 12, tolerance = 1e-12)
  expect_equal(lasso$penalty, 8, tolerance = 1e-12)
  expect_equal(ridge$loglik, lasso$loglik, tolerance = 1e-12)
})

test_that("eq (7.9) block update improves the objective", {
  set.seed(3)
  n <- 60
  X <- matrix(rnorm(n), ncol = 1)
  y <- ifelse(X[, 1] < -0.3, 0L, ifelse(X[, 1] < 0.4, 1L, 2L))
  b0 <- c(0, 0); B <- matrix(0, 2, 1)
  before <- morie_mvsml_penalized_multinomial(X, y, b0, B, 0.1)
  u <- morie_mvsml_multinomial_block(X, y, b0, B, 0.1, 0L)
  after <- morie_mvsml_penalized_multinomial(
    X, y, c(u$beta0, b0[2]), rbind(u$beta, B[2, ]), 0.1)
  expect_gt(after$penalized_loglik, before$penalized_loglik)
  expect_true(all(u$weights > 0 & u$weights <= 0.25 + 1e-12))
})

test_that("eq (7.11) Poisson pmf and penalized fit", {
  expect_equal(morie_mvsml_poisson_pmf(2, 3), exp(-3) * 9 / 2,
               tolerance = 1e-12)
  expect_equal(sum(morie_mvsml_poisson_pmf(0:60, 2.5)), 1,
               tolerance = 1e-10)
  set.seed(8)
  n <- 300
  X <- matrix(runif(n, -1, 1), ncol = 1)
  y <- rpois(n, exp(0.7 + 1.2 * X[, 1]))
  r <- morie_mvsml_penalized_poisson(X, y, lambda = 0)
  expect_equal(r$beta[1], 0.7, tolerance = 0.3)
  expect_equal(r$beta[2], 1.2, tolerance = 0.3)
  pen <- morie_mvsml_penalized_poisson(X, y, lambda = 200)
  expect_lt(abs(pen$beta[2]), abs(r$beta[2]))
  expect_lt(pen$penalized_loglik, pen$loglik)
})
