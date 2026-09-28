test_that("Wishart moments reduce to the scaled chi-square", {
  w <- WishartMoments(matrix(1.7), 6.5)
  expect_equal(w$mean_logdet, log(2 * 1.7) + digamma(3.25), tolerance = 1e-13)
  expect_equal(w$mean_inverse[1, 1], 1 / (1.7 * 4.5), tolerance = 1e-14)
  iw <- WishartMoments(matrix(1.7), 6.5, TRUE)
  expect_equal(iw$mean[1, 1], 1.7 / 4.5, tolerance = 1e-14)
  expect_equal(iw$mean_logdet, log(1.7 / 2) - digamma(3.25), tolerance = 1e-13)
})

test_that("partitioned covariance, prewhitening, Carroll and convolution identities", {
  S <- matrix(c(3, 1, 0.5, 1, 2, 0.4, 0.5, 0.4, 1), 3)
  r <- PartitionedCovariance(S, 1, x2 = c(1, -2), mean = c(0.5, 0, 1))
  expect_equal(r$reconstructed, S, tolerance = 1e-12)
  tau <- solve(S[2:3, 2:3], S[2:3, 1])
  expect_equal(r$cond_mean, 0.5 + sum(tau * c(1, -3)), tolerance = 1e-12)
  R <- 0.7^abs(outer(1:4, 1:4, "-"))
  M <- SeparablePrewhiten(matrix(1:4, 1), R)$rho_t_inv_sqrt
  expect_equal(M %*% R %*% M, diag(4), tolerance = 1e-12)
  cr <- CarrollStCorrelation(rbind(c(0, 0), c(2, 0)), c(0, 1), c(-0.3, 0.1, 0.02), c(-0.1, -0.2, 0))
  rho <- exp(-0.3) * exp(-0.18)^2
  expect_equal(cr$correlation[1, 2], rho, tolerance = 1e-13)
  expect_equal(cr$min_eigenvalue, 1 - rho, tolerance = 1e-12)
  pts <- rbind(c(0, 0), c(1, 0.5), c(0.3, -1))
  cc <- ProcessConvolutionCovariance(pts, diag(2), normalize = TRUE)$covariance
  expect_equal(cc, exp(-as.matrix(stats::dist(pts))^2 / 4), tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("normal-gamma DLM variance recursion and ARMA closed forms", {
  y <- .morie_random_normal(3, seed = 3)
  s <- NormalGammaDlm(y, 1, matrix(1), matrix(0.5), 0, matrix(1), 2, 1.5)
  expect_equal(s$S, (3 + cumsum((y - s$f)^2 / s$Q)) / (3:5), tolerance = 1e-12)
  expect_equal(ArmaAcf(ma = 0.6, lag_max = 3), c(1, 0.6 / 1.36, 0, 0), tolerance = 1e-15)
  expect_equal(ArmaAcf(ar = 0.8, lag_max = 5), 0.8^(0:5), tolerance = 1e-13)
  expect_error(ArmaAcf(), "empty")
})
