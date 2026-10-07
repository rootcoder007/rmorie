# Native Bayesian Aldrich-McKelvey (Hare et al. 2015) and Bakker-Poole
# Bayesian MDS / unfolding.

test_that("the truncated normal draw stays in bounds and has the right mean", {
  set.seed(1)
  x <- rmorie:::.morie_rtnorm(rep(0.3, 20000L), 1.2, -0.5, 2)
  expect_true(all(x >= -0.5 & x <= 2))
  al <- (-0.5 - 0.3) / 1.2
  be <- (2 - 0.3) / 1.2
  m <- 0.3 + 1.2 * (stats::dnorm(al) - stats::dnorm(be)) /
    (stats::pnorm(be) - stats::pnorm(al))
  expect_lt(abs(mean(x) - m), 4 * 1.2 / sqrt(20000))
})

test_that("tau is drawn from its gamma conditional truncated at 10", {
  set.seed(2)
  big <- replicate(4000L, rmorie:::.morie_bp_tau(200, 400))
  # Gamma(101, 200): mean 0.505, far from the bound
  expect_lt(abs(mean(big) - 101 / 200), 4 * sqrt(101) / 200 / sqrt(4000))
  small <- replicate(500L, rmorie:::.morie_bp_tau(200, 1))
  expect_true(all(small <= 10 & small > 0))
})

test_that("Bayesian Aldrich-McKelvey recovers standardised stimulus positions", {
  skip_on_cran()
  set.seed(11)
  zt <- c(-1.4, -0.6, 0, 0.5, 1.5)
  N <- 120
  Z <- stats::rnorm(N, 0, 0.8) + outer(stats::runif(N, 0.4, 1.6), zt) +
    matrix(stats::rnorm(N * 5, 0, 0.4), N, 5)
  Z[sample(length(Z), 30)] <- NA
  fit <- morie_spatial_voting_bayesian_am(Z, n_samples = 300L,
                                          burn_in = 200L, seed = 2L)
  target <- (zt - mean(zt)) / stats::sd(zt)
  expect_lt(max(abs(fit$zeta_mean - target)), 0.06)
  # every draw is standardised and the polarity stimulus is on the left
  expect_equal(rowMeans(fit$draws), rep(0, 300L), tolerance = 1e-12)
  expect_equal(apply(fit$draws, 1L, stats::sd), rep(1, 300L),
               tolerance = 1e-12)
  expect_true(all(fit$draws[, 1] < 0))
  expect_true(all(fit$zeta_interval[1, ] <= fit$zeta_mean &
                    fit$zeta_mean <= fit$zeta_interval[2, ]))
})

test_that("Bayesian Aldrich-McKelvey refuses a single stimulus", {
  expect_error(morie_spatial_voting_bayesian_am(matrix(c(1, NA, 3), 3, 1)),
               "at least two stimuli")
  expect_error(morie_spatial_voting_bayesian_am(cbind(NA, 1:3, 2:4)),
               "polarity")
})

test_that("Bayesian MDS recovers the distances and aligns its draws", {
  skip_on_cran()
  set.seed(4)
  Xt <- matrix(stats::rnorm(20), 10, 2)
  Dt <- as.matrix(stats::dist(Xt))
  f <- morie_spatial_voting_bayesian_mds(Dt, n_samples = 200L,
                                         burn_in = 200L, seed = 1L)
  ut <- upper.tri(Dt)
  expect_gt(stats::cor(f$distance_mean[ut], Dt[ut]), 0.99)
  expect_true(f$tau > 0 && f$tau <= 10)
  expect_equal(dim(f$draws), c(200L, 10L, 2L))
  expect_error(morie_spatial_voting_bayesian_mds(matrix(1, 3, 4)), "square")
})

test_that("Bayesian unfolding recovers the respondent-stimulus geometry", {
  skip_on_cran()
  set.seed(31)
  S <- matrix(stats::runif(12, -0.55, 0.55), 6, 2)
  R <- matrix(stats::runif(120, -0.55, 0.55), 60, 2)
  D <- sqrt(rmorie:::.morie_sqdist(R, S)) *
    exp(matrix(stats::rnorm(360, 0, 0.1), 60, 6))
  f <- morie_spatial_voting_bayesian_unfolding(D, n_samples = 200L,
                                               burn_in = 300L, seed = 1L)
  expect_gt(stats::cor(as.numeric(f$distance_mean),
                       as.numeric(sqrt(rmorie:::.morie_sqdist(R, S)))), 0.97)
  al <- rmorie:::.morie_rigid_align(f$stimuli, S)
  expect_lt(sqrt(mean((al(f$stimuli) - S)^2)), 0.08)
})

test_that("rigid alignment is a translation plus a rotation", {
  set.seed(9)
  T0 <- matrix(stats::rnorm(10), 5, 2)
  th <- 0.7
  Q <- matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2)
  X <- sweep(T0 %*% Q, 2L, c(3, -1), "+")
  expect_equal(rmorie:::.morie_rigid_align(X, T0)(X), T0, tolerance = 1e-12)
})
