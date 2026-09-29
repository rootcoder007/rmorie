# Coverage tests for R/dp_native2.R (Dwork and Roth 2014; Mironov 2017;
# Balle et al. 2018): private covariance and PCA, min/max quantiles,
# amplification, Renyi composition, calibration and empirical audits.

dp_X <- cbind(c(0.2, 0.9, -0.4, 1.5, 0.3, -0.8, 0.6, 1.1), c(1, 0.1, 0.5, -0.3, 0.8, 0.2, -1.2, 0.4))

test_that("Gaussian-mechanism covariance: clipping, symmetric noise, PSD projection", {
  r <- morie_dp_covariance(dp_X, C = 1, epsilon = 2, delta = 1e-5, seed = 11, project_psd = FALSE)
  nr <- sqrt(rowSums(dp_X^2))
  Xc <- dp_X * pmin(1, 1 / nr)
  S <- crossprod(Xc) / 8
  sig <- (1 / 8) * sqrt(2 * log(1.25 / 1e-5)) / 2
  expect_equal(r$sigma, sig, tolerance = 1e-12)
  set.seed(11)
  N <- matrix(rnorm(4, 0, sig), 2, 2)
  N[2, 1] <- N[1, 2]
  expect_equal(r$raw, S + N, tolerance = 1e-12)
  expect_equal(r$release, r$raw)
  expect_equal(r$clipped_fraction, mean(nr > 1))
  p <- morie_dp_covariance(dp_X, C = 1, epsilon = 0.01, seed = 3)
  expect_true(all(eigen(p$release, symmetric = TRUE)$values >= -1e-12))
  if (p$n_negative_eigenvalues > 0) {
    e <- eigen(p$raw, symmetric = TRUE)
    expect_equal(p$release, e$vectors %*% diag(pmax(e$values, 0)) %*% t(e$vectors), tolerance = 1e-12)
  }
  expect_error(morie_dp_covariance(dp_X, C = 0), "C must be positive")
  expect_error(morie_dp_covariance(dp_X, epsilon = -1), "epsilon")
})

test_that("private PCA uses the eigenvectors of the private covariance", {
  r <- morie_dp_pca(dp_X, k = 1, epsilon = 5, seed = 4)
  cv <- morie_dp_covariance(dp_X, C = 1, epsilon = 5, seed = 4)
  e <- eigen(cv$release, symmetric = TRUE)
  expect_equal(abs(r$components[, 1]), abs(e$vectors[, 1]), tolerance = 1e-12)
  expect_equal(r$eigengap, e$values[1] - e$values[2], tolerance = 1e-12)
  expect_equal(r$scores, dp_X %*% r$components, tolerance = 1e-12)
  expect_equal(r$explained_variance_ratio, max(e$values[1], 0) / sum(pmax(e$values, 0)), tolerance = 1e-12)
  expect_error(morie_dp_pca(dp_X, k = 3), "between 1 and 2")
})

test_that("private min/max are two half-budget quantile releases", {
  x <- c(3, 7, 1, 9, 4, 6, 2, 8, 5, 10)
  r <- morie_dp_minmax(x, epsilon = 2, a = 0, b = 12, alpha = 0.1, seed = 5)
  expect_equal(r$lower, morie_dp_quantile(x, 0.1, 1, 0, 12, seed = 5)$release)
  expect_equal(r$upper, morie_dp_quantile(x, 0.9, 1, 0, 12, seed = 6)$release)
  expect_equal(c(r$true_min, r$true_max), c(1, 10))
  expect_error(morie_dp_minmax(x, alpha = 0.5), "alpha")
})

test_that("amplification by subsampling and Renyi composition", {
  a <- morie_privacy_amplification(1, 0.1, delta = 1e-6)
  expect_equal(a$epsilon_amplified, log(1 + 0.1 * (exp(1) - 1)), tolerance = 1e-12)
  expect_equal(a$delta_amplified, 1e-7)
  expect_lt(a$epsilon_amplified, 1)
  expect_equal(morie_privacy_amplification(0.7, 1)$epsilon_amplified, 0.7, tolerance = 1e-12)
  expect_error(morie_privacy_amplification(1, 0), "q must be")
  rd <- morie_renyi_dp_composition(c(0.1, 0.2, 0.05), alpha = 8, delta = 1e-6)
  expect_equal(rd$rdp_total, 0.35, tolerance = 1e-12)
  expect_equal(rd$epsilon, 0.35 + log(1e6) / 7, tolerance = 1e-12)
  expect_error(morie_renyi_dp_composition(1, alpha = 1), "greater than 1")
  expect_error(morie_renyi_dp_composition(-1), "non-negative")
})

test_that("Laplace calibration between error and epsilon", {
  c1 <- morie_dp_release_calibration(sensitivity = 2, target_error = 0.5, confidence = 0.9, n = 10)
  expect_equal(c1$epsilon, 2 * log(10) / (10 * 0.5), tolerance = 1e-12)
  # P(|Lap(b)| > w) = exp(-w / b) = 1 - confidence
  expect_equal(exp(-c1$half_width / c1$noise_scale), 0.1, tolerance = 1e-12)
  c2 <- morie_dp_release_calibration(1, epsilon = c1$epsilon * 5, confidence = 0.9, n = 2)
  expect_equal(c2$half_width, log(10) / (2 * c1$epsilon * 5), tolerance = 1e-12)
  expect_equal(c2$noise_sd, sqrt(2) * c2$noise_scale, tolerance = 1e-12)
  expect_length(morie_dp_release_calibration(1, epsilon = 20)$warnings, 1L)
  expect_error(morie_dp_release_calibration(1), "exactly one")
  expect_error(morie_dp_release_calibration(1, epsilon = 1, confidence = 1), "confidence")
})

test_that("empirical epsilon and delta audits of a Laplace mechanism", {
  mech <- function(D) sum(D) + (if (runif(1) < 0.5) -1 else 1) * rexp(1, 1)
  D <- c(1, 0, 1)
  Dp <- c(1, 0, 0)
  r <- morie_epsilon_dp(mech, D, Dp, n_samples = 4000, bins = 20, seed = 9)
  set.seed(9)
  a <- vapply(1:4000, function(i) mech(D), 0)
  b <- vapply(1:4000, function(i) mech(Dp), 0)
  e <- seq(min(a, b), max(a, b), length.out = 21)
  ca <- tabulate(pmin(pmax(findInterval(a, e), 1), 20), 20)
  cb <- tabulate(pmin(pmax(findInterval(b, e), 1), 20), 20)
  ok <- ca >= 10 & cb >= 10
  expect_equal(r$epsilon_empirical, max(abs(log((ca[ok] / 4000) / (cb[ok] / 4000)))), tolerance = 1e-12)
  # the Laplace mechanism with sensitivity 1 and scale 1 is 1-DP; bin
  # noise with 4000 draws keeps the audit near 1
  expect_lt(r$epsilon_empirical, 1.5)
  d <- morie_approx_dp(mech, D, Dp, epsilon = 1, n_samples = 4000, bins = 20, seed = 9)
  pa <- ca / 4000
  pb <- cb / 4000
  expect_equal(d$delta_empirical, sum(pmax(pa - exp(1) * pb, 0)), tolerance = 1e-12)
  expect_equal(morie_epsilon_dp(function(D) sum(D), D, Dp, n_samples = 10)$epsilon_empirical, Inf)
  expect_equal(morie_approx_dp(function(D) 1, D, Dp, n_samples = 10)$delta_empirical, 1)
})

test_that("unit of privacy: contributions per unit", {
  r <- morie_dp_unit_definition(c("a", "b", "a", "c", "a", "b"), unit = "b")
  expect_equal(r$contributions, c(3L, 2L, 1L))
  expect_equal(r$sensitivity_multiplier, 3L)
  expect_equal(r$unit_contribution, 2L)
  expect_length(r$warnings, 1L)
  expect_length(morie_dp_unit_definition(1:4)$warnings, 0L)
  expect_error(morie_dp_unit_definition(character(0)), "non-empty")
})
