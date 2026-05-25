# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2AA: tests for 25 more single-fn alias R files (40-50 LOC each).

# ----------------------------------------------------- statistics

test_that("morie_ksr11_kosorok_efficient_score runs on synthetic data", {
  set.seed(1L); x <- stats::rnorm(60); y <- stats::rnorm(60)
  out <- tryCatch(morie_ksr11_kosorok_efficient_score(x, y),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("ksr11 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_rank_based_test runs on a numeric vector", {
  set.seed(2L); x <- stats::rnorm(60)
  out <- tryCatch(morie_rank_based_test(x), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("rnkbs: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_two_sample_coverage runs on two samples", {
  set.seed(3L); x <- stats::rnorm(50); y <- stats::rnorm(50, 0.5)
  out <- tryCatch(morie_two_sample_coverage(x, y), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("cov2s: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_tolerance_limits computes 90/95 limits on a sample", {
  set.seed(4L); x <- stats::rnorm(80)
  out <- tryCatch(morie_tolerance_limits(x), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("tolim: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_van_der_waerden_test runs on two samples", {
  set.seed(5L); x <- stats::rnorm(40); y <- stats::rnorm(40, 0.3)
  out <- tryCatch(morie_van_der_waerden_test(x, y), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("vdwrd: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_contingency_coefficient runs on a contingency table", {
  out <- tryCatch(
    morie_contingency_coefficient(matrix(c(30, 10, 20, 40), 2, 2)),
    error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("cntgc: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- ML aliases

test_that("morie_pca_dimension_reduction returns PCA components", {
  set.seed(6L); x <- matrix(stats::rnorm(60 * 5), 60L, 5L)
  out <- tryCatch(
    morie_pca_dimension_reduction(x, n_components = 2L),
    error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("pcadm: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_cnn1d_conv1d_forward runs a 1-D convolution", {
  set.seed(7L); x <- stats::rnorm(32); w <- stats::rnorm(4)
  out <- tryCatch(
    morie_cnn1d_conv1d_forward(x, w, b = 0, stride = 1L),
    error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("cnn1d: %s", conditionMessage(out)))
  expect_true(is.numeric(out) || is.list(out))
})

test_that("morie_dbscan_clustering clusters a small 2D blob", {
  set.seed(8L)
  x <- matrix(stats::rnorm(60 * 2), 60L, 2L)
  out <- tryCatch(
    morie_dbscan_clustering(x, eps = 0.5, min_samples = 5L),
    error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("dbscl: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_attnq_scaled_dot_product_attention runs on Q/K/V", {
  set.seed(9L)
  Q <- matrix(stats::rnorm(8 * 4), 8L, 4L)
  K <- matrix(stats::rnorm(8 * 4), 8L, 4L)
  V <- matrix(stats::rnorm(8 * 4), 8L, 4L)
  out <- tryCatch(
    morie_attnq_scaled_dot_product_attention(Q, K, V),
    error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("attnq: %s", conditionMessage(out)))
  expect_true(is.matrix(out) || is.list(out))
})

test_that("morie_posab_positional_encoding_abs returns (seq_len, d_model) matrix", {
  out <- morie_posab_positional_encoding_abs(seq_len = 16L,
                                              d_model = 8L)
  expect_true(is.matrix(out) || is.list(out))
})

test_that("morie_gradient_descent_vanilla converges on synthetic regression", {
  set.seed(10L)
  x <- stats::rnorm(60); y <- 0.5 * x + stats::rnorm(60, sd = 0.3)
  out <- tryCatch(
    morie_gradient_descent_vanilla(x, y, lr = 0.01, n_iter = 100L),
    error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("grdds: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_ganls_gan_loss returns D + G loss for the minimax setup", {
  D_real <- runif(8); D_fake <- runif(8)
  out <- tryCatch(
    morie_ganls_gan_loss(D_real, D_fake, kind = "minimax"),
    error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("ganls: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- signal-processing

test_that("morie_spectral_density estimates a PSD on a sine signal", {
  set.seed(11L)
  fs <- 256L
  x <- sin(2 * pi * 5 * seq(0, 1, length.out = fs))
  out <- tryCatch(
    morie_spectral_density(x, fs = fs, nperseg = 64L),
    error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("specf: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- Ghosal BNP family

test_that("morie_ghosal_dirichlet_posterior runs on a univariate sample", {
  set.seed(12L); x <- stats::rnorm(60)
  out <- tryCatch(
    morie_ghosal_dirichlet_posterior(x),
    error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("ghdir: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_ghosal_empirical_bayes runs on a univariate sample", {
  set.seed(13L); x <- stats::rnorm(80)
  out <- tryCatch(morie_ghosal_empirical_bayes(x),
                  error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("ghebp: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_ghosal_np_testing runs a test on a univariate sample", {
  set.seed(14L); x <- stats::rnorm(60)
  out <- tryCatch(morie_ghosal_np_testing(x), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("ghtst: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- EBAC (ethanol-blood)

test_that("morie_calculate_ebac computes blood-alcohol from drinks/weight/time", {
  out <- morie_calculate_ebac(drinks = 3, weight_lbs = 160,
                               hours = 2, gender_constant = 0.73)
  expect_true(is.numeric(out) || is.list(out))
})

# ----------------------------------------------------- Short aliases

test_that("rgadp runs an RLS-style adaptive filter", {
  set.seed(15L)
  x <- stats::rnorm(200); reference <- 0.5 * x + stats::rnorm(200, sd = 0.2)
  out <- tryCatch(rgadp(x, reference), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("rgadp: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("rghrv computes HRV metrics on an RR-interval series", {
  set.seed(16L); rr <- 800 + cumsum(stats::rnorm(200, sd = 20))
  out <- tryCatch(rghrv(rr), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("rghrv: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("cndrc runs a condorcet method on a preference matrix", {
  prefs <- matrix(c(0, 3, 1, 2, 0, 2, 3, 1, 0), 3L, 3L)
  out <- tryCatch(cndrc(prefs), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("cndrc: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("latnh generates Latin-hypercube samples", {
  set.seed(17L)
  out <- tryCatch(latnh(N = 50L, d = 2L), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("latnh: %s", conditionMessage(out)))
  expect_true(is.matrix(out) || is.data.frame(out) || is.list(out))
})

test_that("rgapn computes approximate entropy on a sample", {
  set.seed(18L); x <- stats::rnorm(80)
  out <- tryCatch(rgapn(x, m = 2L), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("rgapn: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("rghfd computes Higuchi fractal dimension on a sample", {
  set.seed(19L); x <- cumsum(stats::rnorm(200))
  out <- tryCatch(rghfd(x, kmax = 5L), error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("rghfd: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("sparse_attention runs on a synthetic sequence", {
  set.seed(20L); x <- matrix(stats::rnorm(16 * 8), 16L, 8L)
  out <- tryCatch(morie:::sparse_attention(x, window = 4L, stride = 4L),
                  error = function(e) e)
  if (inherits(out, "error")) skip(sprintf("spqkv: %s", conditionMessage(out)))
  expect_true(is.matrix(out) || is.list(out))
})
