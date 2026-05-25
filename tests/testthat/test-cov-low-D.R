# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch D: rgdfa, lstmc, svmge, grucl, kalmn.

# ==== rgdfa.R ====
test_that("rgdfa runs on default Gaussian series", {
  set.seed(1)
  out <- rgdfa(rnorm(500))
  expect_type(out, "list")
  expect_true(is.numeric(out$alpha))
  expect_true(is.finite(out$alpha))
})

test_that("rgdfa errors when series shorter than 32 samples", {
  set.seed(1)
  expect_error(rgdfa(rnorm(20)), "at least 32")
})

test_that("rgdfa accepts user-supplied scales and order", {
  set.seed(1)
  x <- cumsum(rnorm(400))
  out <- rgdfa(x, scales = c(8L, 16L, 32L, 64L), order = 2L)
  expect_equal(out$scales, c(8L, 16L, 32L, 64L))
})

# ==== lstmc.R ====
test_that("morie_lstmc_lstm_cell runs on defaults", {
  set.seed(1)
  out <- morie_lstmc_lstm_cell(x = rnorm(8))
  expect_type(out, "list")
  expect_named(out, c("h", "c", "estimate", "i", "f", "g", "o", "method"))
  expect_length(out$h, 8L)
  expect_equal(out$method, "LSTM cell forward")
  expect_true(all(out$i >= 0 & out$i <= 1))
  expect_true(all(out$f >= 0 & out$f <= 1))
})

test_that("morie_lstmc_lstm_cell honours hidden_size and prior states", {
  set.seed(1)
  H <- 4L
  out <- morie_lstmc_lstm_cell(
    x = rnorm(6), h_prev = rep(0.1, H), c_prev = rep(-0.2, H),
    hidden_size = H, seed = 42L
  )
  expect_length(out$h, H)
  expect_length(out$c, H)
  expect_equal(out$estimate, out$h)
})

# ==== svmge.R ====
test_that("morie_svm_genomic runs with x = NULL (markers only)", {
  set.seed(1)
  M <- matrix(sample(0:2, 200, TRUE), 50, 4)
  y <- rnorm(50)
  out <- morie_svm_genomic(x = NULL, y = y, markers = M)
  expect_type(out, "list")
  expect_length(out$y_hat, 50L)
  expect_equal(out$n, 50L)
})

test_that("morie_svm_genomic drops zero-variance columns without error", {
  set.seed(1)
  M <- matrix(rnorm(100), 25, 4)
  M[, 2] <- 0
  y <- rnorm(25)
  out <- morie_svm_genomic(x = NULL, y = y, markers = M)
  expect_length(out$y_hat, 25L)
})

# ==== grucl.R ====
test_that("morie_grucl_gru_cell runs on defaults", {
  set.seed(1)
  out <- morie_grucl_gru_cell(x = rnorm(8))
  expect_type(out, "list")
  expect_named(out, c("h", "estimate", "z", "r", "n", "method"))
  expect_length(out$h, 8L)
  expect_equal(out$method, "GRU cell forward")
  expect_true(all(out$z >= 0 & out$z <= 1))
  expect_true(all(out$r >= 0 & out$r <= 1))
})

test_that("morie_grucl_gru_cell respects explicit hidden_size and h_prev", {
  set.seed(1)
  H <- 5L
  out <- morie_grucl_gru_cell(
    x = rnorm(4), h_prev = rep(0.05, H),
    hidden_size = H, seed = 3L
  )
  expect_length(out$h, H)
  expect_equal(out$estimate, out$h)
})

# ==== kalmn.R ====
test_that("morie_kalman_filter runs on a univariate default series", {
  set.seed(1)
  out <- morie_kalman_filter(x = rnorm(50))
  expect_type(out, "list")
  expect_named(out, c(
    "state", "state_cov", "innovations", "innovation_variance",
    "loglik", "n", "method"
  ))
  expect_equal(out$n, 50L)
  expect_equal(nrow(out$state), 50L)
  expect_true(is.finite(out$loglik))
  expect_match(out$method, "Kalman")
})

test_that("morie_kalman_filter accepts user-supplied transition/H/Q/R", {
  set.seed(1)
  n <- 40L
  m <- 2L
  Y <- matrix(rnorm(n * m), n, m)
  trans <- diag(m) * 0.9
  Hmat <- diag(m)
  Q <- diag(0.1, m)
  R <- diag(0.5, m)
  out <- morie_kalman_filter(
    x = Y, transition = trans, H = Hmat, Q = Q, R = R,
    x0 = rep(0, m), P0 = diag(1, m)
  )
  expect_equal(dim(out$state), c(n, m))
  expect_equal(dim(out$innovations), c(n, m))
})
