# Parity for the maximum-likelihood spatial models and friends.
# Anchors come from spatialreg::lagsarlm / errorsarlm.

W25 <- local({
  k <- 5; n <- k * k
  idx <- function(r, c) (r - 1) * k + c
  W <- matrix(0, n, n)
  for (r in 1:k) for (cc in 1:k)
    for (d in list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))) {
      rr <- r + d[1]; c2 <- cc + d[2]
      if (rr >= 1 && rr <= k && c2 >= 1 && c2 <= k)
        W[idx(r, cc), idx(rr, c2)] <- 1
    }
  W / rowSums(W)
})

fx <- local({
  n <- 25
  x1 <- sapply(0:(n - 1), function(i) ((i * 7) %% 11) + 0.5 * ((i * 3) %% 5))
  x2 <- sapply(0:(n - 1), function(i) ((i * 5) %% 7) - 0.25 * ((i * 2) %% 3))
  e  <- sapply(0:(n - 1), function(i) ((i * 13) %% 17) / 17 - 0.5)
  list(y = as.numeric(solve(diag(n) - 0.4 * W25) %*%
                        (2 + 1.5 * x1 - 0.8 * x2 + e)),
       X = cbind(x1, x2))
})

test_that("spatial lag ML matches spatialreg::lagsarlm", {
  m <- morie_spatial_lag_model(fx$y, fx$X, W25)
  expect_equal(m$rho, 0.391406232872727, tolerance = 1e-6)
  expect_equal(m$beta[1], 1.94007428339988, tolerance = 1e-6)
  expect_equal(m$beta[2], 1.50683946841365, tolerance = 1e-6)
  expect_equal(m$beta[3], -0.762066735722032, tolerance = 1e-6)
  expect_equal(m$sigma2, 0.0785475951546229, tolerance = 1e-6)
})

test_that("spatial error ML matches spatialreg::errorsarlm", {
  m <- morie_spatial_error_model(fx$y, fx$X, W25)
  expect_equal(m$lambda, 0.824444050787294, tolerance = 1e-5)
  expect_equal(m$beta[1], 7.94062292250759, tolerance = 1e-5)
  expect_equal(m$beta[2], 1.34203947781064, tolerance = 1e-6)
  expect_equal(m$beta[3], -0.508793220452759, tolerance = 1e-6)
})

test_that("the lag model recovers the generating rho", {
  m <- morie_spatial_lag_model(fx$y, fx$X, W25)
  expect_lt(abs(m$rho - 0.4), 0.05)
  expect_lt(abs(m$rho - morie_spatial_2sls(fx$y, fx$X, W25)$rho), 0.01)
})

test_that("Ripley K tracks pi r^2 under a regular pattern", {
  pts <- as.matrix(expand.grid(0:9, 0:9))
  out <- morie_ripley_k(pts, c(1.5, 2, 2.5))
  expect_true(all(abs(out$K - pi * out$r^2) / (pi * out$r^2) < 0.35))
  expect_true(all(diff(out$K) >= -1e-9))
  expect_equal(out$L, sqrt(out$K / pi), tolerance = 1e-12)
})

test_that("edge correction raises K", {
  pts <- as.matrix(expand.grid(0:5, 0:5))
  expect_gt(morie_ripley_k(pts, 2)$K,
            morie_ripley_k(pts, 2, edge_correction = FALSE)$K)
})

test_that("co-kriging reduces to ordinary kriging without cross structure", {
  pts <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1))
  out <- morie_cokriging(pts, c(1, 2, 3, 4), rep(9, 4), c(0.5, 0.5),
                         cross_vario = function(h) 0 * h)
  expect_equal(sum(out$lambda), 1, tolerance = 1e-9)
  expect_lt(max(abs(out$mu)), 1e-9)
  expect_equal(out$prediction, 2.5, tolerance = 1e-6)
})

test_that("randomised response satisfies the privacy ratio", {
  for (eps in c(0.5, 1, 2)) for (k in c(2, 4, 10)) {
    r <- morie_local_dp_randomised_response(rep(0, 5), k, eps)
    expect_equal(r$p_keep / r$p_flip, exp(eps), tolerance = 1e-12)
    expect_equal(r$p_keep + (k - 1) * r$p_flip, 1, tolerance = 1e-12)
  }
})

test_that("randomised response debiasing recovers the distribution", {
  truth <- c(rep(0, 6000), rep(1, 3000), rep(2, 1000))
  r <- morie_local_dp_randomised_response(truth, 3, 2, seed = 11)
  expect_equal(as.numeric(r$estimate), c(0.6, 0.3, 0.1), tolerance = 0.05)
  expect_equal(sum(r$estimate), 1, tolerance = 1e-9)
})
