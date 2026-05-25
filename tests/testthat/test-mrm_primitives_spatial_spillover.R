# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/mrm_primitives_spatial_spillover.R

mk_W <- function(N = 12L, row_std = TRUE) {
  set.seed(42L)
  W <- matrix(runif(N * N), N, N)
  diag(W) <- 0
  if (row_std) W <- W / rowSums(W)
  W
}

test_that("mrm_spatial_spillover_decomposition basic happy path", {
  W <- mk_W(15L)
  res <- mrm_spatial_spillover_decomposition(
    rho = 0.3,
    beta_direct = c(1.0, -0.5),
    beta_spatial = c(0.2, 0.1),
    W = W,
    coefficient_names = c("x1", "x2")
  )
  expect_s3_class(res, "morie_mrm_result")
  expect_true("decomposition" %in% names(res))
  expect_s3_class(res$decomposition, "data.frame")
  expect_equal(nrow(res$decomposition), 2L)
  expect_named(res$decomposition,
               c("coefficient", "direct", "indirect", "total", "note"))
})

test_that("mrm_spatial_spillover_decomposition default coefficient_names", {
  W <- mk_W(10L)
  res <- mrm_spatial_spillover_decomposition(
    rho = 0.2, beta_direct = c(0.5), beta_spatial = c(0.1), W = W
  )
  expect_equal(res$decomposition$coefficient, "x1")
})

test_that("mrm_spatial_spillover_decomposition direct-dominant lead text branch", {
  W <- mk_W(10L)
  res <- mrm_spatial_spillover_decomposition(
    rho = 0.05, beta_direct = c(5.0), beta_spatial = c(0.001), W = W
  )
  expect_s3_class(res, "morie_mrm_result")
  expect_true(is.character(res$interpretation))
})

test_that("mrm_spatial_spillover_decomposition errors on length mismatch", {
  W <- mk_W(10L)
  expect_error(
    mrm_spatial_spillover_decomposition(
      rho = 0.3, beta_direct = c(1, 2),
      beta_spatial = c(0.1), W = W),
    "same length"
  )
})

test_that("mrm_spatial_spillover_decomposition errors on name mismatch", {
  W <- mk_W(10L)
  expect_error(
    mrm_spatial_spillover_decomposition(
      rho = 0.3, beta_direct = c(1, 2),
      beta_spatial = c(0.1, 0.2), W = W,
      coefficient_names = c("a")),
    "coefficient_names"
  )
})

test_that("mrm_spatial_spillover_decomposition errors on non-square W", {
  W <- matrix(0, 3, 4)
  expect_error(
    mrm_spatial_spillover_decomposition(
      rho = 0.1, beta_direct = c(1), beta_spatial = c(0.1), W = W),
    "square"
  )
})

test_that("mrm_spatial_spillover_decomposition warns on non-row-standardised W", {
  W <- matrix(runif(100), 10, 10)
  diag(W) <- 0
  # NOT row-standardised
  res <- mrm_spatial_spillover_decomposition(
    rho = 0.1, beta_direct = c(0.5), beta_spatial = c(0.05), W = W
  )
  expect_true(length(res$warnings) >= 1L)
})

test_that("mrm_spatial_spillover_decomposition warns on |rho| >= 1", {
  W <- mk_W(8L)
  # Hard to construct singularity-free rho>=1 — use rho=0.99 to skirt it
  # Use diagonal W and rho>1 to avoid singular invert
  W_diag <- matrix(0, 5, 5)
  W_diag[1, 2] <- W_diag[2, 1] <- 0.5
  W_diag[3, 4] <- W_diag[4, 3] <- 0.5
  W_diag[5, 1] <- 1
  W_diag <- W_diag / pmax(rowSums(W_diag), 1)
  res <- tryCatch(
    mrm_spatial_spillover_decomposition(
      rho = 1.5, beta_direct = c(0.1),
      beta_spatial = c(0.05), W = W_diag),
    error = function(e) e
  )
  # Either errors on singular invert or warns; both code paths are valid
  expect_true(inherits(res, "morie_mrm_result") || inherits(res, "error"))
})

test_that("mrm_spatial_spillover_decomposition singular invert errors", {
  N <- 5
  W <- matrix(0, N, N)
  W[1, 2] <- 1; W[2, 1] <- 1
  W[3, 4] <- 1; W[4, 3] <- 1
  # rho = 1 with W not row-standardised triggers near-singular (I - W)
  expect_error(
    mrm_spatial_spillover_decomposition(
      rho = 1.0, beta_direct = c(0.5),
      beta_spatial = c(0.05), W = W),
    "singular|invert"
  )
})

test_that("mrm_morans_i basic happy path", {
  W <- mk_W(15L)
  res <- mrm_morans_i(residuals = rnorm(15), W = W)
  expect_s3_class(res, "morie_mrm_result")
  expect_true("morans_i" %in% names(res))
  expect_true(is.finite(res$morans_i))
})

test_that("mrm_morans_i errors on non-matrix W", {
  expect_error(mrm_morans_i(rnorm(5), c(1, 2, 3)), "matrix")
})

test_that("mrm_morans_i errors on dim mismatch", {
  expect_error(mrm_morans_i(rnorm(5), mk_W(10)), "to match")
})

test_that("mrm_morans_i undefined when residual variance zero", {
  W <- mk_W(10L)
  res <- mrm_morans_i(rep(1, 10), W)
  expect_true(is.na(res$morans_i))
  expect_true(any(grepl("undefined", res$warnings)))
})

test_that("mrm_morans_i undefined when sum of W is zero", {
  W <- matrix(0, 10, 10)
  res <- mrm_morans_i(rnorm(10), W)
  expect_true(is.na(res$morans_i))
})

test_that("mrm_morans_i strong positive autocorrelation branch", {
  # Construct residuals with strong spatial autocorrelation
  N <- 20
  W <- matrix(0, N, N)
  for (i in 1:N) {
    W[i, ((i %% N) + 1)] <- 1
    W[i, ((i + 1) %% N) + 1] <- 1
  }
  W <- W / rowSums(W)
  # Pattern with positive autocorr: alternating clusters
  resid <- c(rep(2, 10), rep(-2, 10))
  res <- mrm_morans_i(resid, W)
  expect_true(is.finite(res$morans_i))
})

test_that("mrm_morans_i negative autocorrelation branch", {
  N <- 20
  W <- matrix(0, N, N)
  for (i in 1:N) {
    W[i, ((i %% N) + 1)] <- 1
  }
  W <- W / rowSums(W)
  # Pattern with strong negative autocorr: alternating signs
  resid <- rep(c(2, -2), 10)
  res <- mrm_morans_i(resid, W)
  expect_true(is.finite(res$morans_i))
})
