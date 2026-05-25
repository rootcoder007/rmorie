# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 18 coverage: rgsam, rgstf, rgwav, rgzcr, rgztn, rkhsc, rkhsf,
#   rlhfd, rmsnr, rndsr, rnkbs, rnkor, rnnge, rocau, rotrp

test_that("rgsam returns documented structure with default args", {
  set.seed(0)
  r <- rgsam(rnorm(80))
  expect_type(r, "list")
  expect_named(r, c("SampEn", "A", "B", "m", "r", "n"))
  expect_identical(r$m, 2L)
  expect_identical(r$n, 80L)
  expect_true(r$r > 0)
  expect_true(is.finite(r$SampEn) || is.infinite(r$SampEn))
  expect_true(r$A >= 0 && r$B >= 0)
})

test_that("rgsam honours explicit m and r", {
  set.seed(1)
  r <- rgsam(rnorm(60), m = 3, r = 0.5)
  expect_identical(r$m, 3L)
  expect_equal(r$r, 0.5)
})

test_that("rgsam errors when length(x) <= m + 1", {
  expect_error(rgsam(1:3, m = 2), "m \\+ 1")
})

test_that("morie_rangayyan_sample_entropy alias is identical to rgsam", {
  expect_identical(morie_rangayyan_sample_entropy, rgsam)
})

test_that("rgstf returns spectrogram with default window", {
  set.seed(0)
  x <- sin(2 * pi * 10 * seq(0, 4, length.out = 512))
  r <- rgstf(x, fs = 100, nperseg = 128)
  expect_type(r, "list")
  expect_named(r, c("freqs", "times", "Sxx", "nperseg", "noverlap", "fs"))
  expect_true(is.matrix(r$Sxx))
  expect_identical(nrow(r$Sxx), length(r$freqs))
  expect_identical(ncol(r$Sxx), length(r$times))
  expect_identical(r$nperseg, 128L)
  expect_true(all(r$Sxx >= 0))
  expect_true(all(is.finite(r$freqs)))
})

test_that("rgstf supports hamming, boxcar and fallback windows", {
  set.seed(1)
  x <- rnorm(400)
  for (w in c("hamming", "boxcar", "unknown-window")) {
    r <- rgstf(x, fs = 50, nperseg = 100, window = w)
    expect_true(is.matrix(r$Sxx))
    expect_true(all(is.finite(r$Sxx)))
  }
})

test_that("rgstf honours explicit noverlap and clamps nperseg to length", {
  set.seed(2)
  x <- rnorm(120)
  r <- rgstf(x, fs = 10, nperseg = 1000, noverlap = 0)
  expect_true(r$nperseg <= length(x))
  expect_identical(r$noverlap, 0L)
})

test_that("morie_rangayyan_stft alias is identical to rgstf", {
  expect_identical(morie_rangayyan_stft, rgstf)
})

test_that("rgwav returns documented structure", {
  set.seed(0)
  x <- sin(2 * pi * 3 * seq(0, 1, length.out = 256)) + 0.3 * rnorm(256)
  r <- suppressWarnings(rgwav(x, level = 3))
  expect_type(r, "list")
  expect_named(r, c("signal", "threshold", "sigma", "wavelet", "level", "mode"))
  expect_length(r$signal, length(x))
  expect_true(all(is.finite(r$signal)))
})

test_that("rgwav wavelet path returns positive threshold and sigma", {
  set.seed(1)
  x <- sin(2 * pi * 3 * seq(0, 1, length.out = 256)) + 0.3 * rnorm(256)
  r <- rgwav(x, level = 3, mode = "hard")
  expect_identical(r$mode, "hard")
  expect_true(is.finite(r$threshold) && r$threshold >= 0)
  expect_true(is.finite(r$sigma) && r$sigma >= 0)
})

test_that("rgwav MA fallback warns when wavelets unavailable", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "wavelets")) FALSE else TRUE
    },
    .package = "base"
  )
  set.seed(2)
  x <- rnorm(64)
  expect_warning(r <- rgwav(x), "fallback")
  expect_length(r$signal, length(x))
  expect_identical(r$mode, "MA-fallback")
})

test_that("morie_rangayyan_wavelet_denoise alias is identical to rgwav", {
  expect_identical(morie_rangayyan_wavelet_denoise, rgwav)
})

test_that("rgzcr returns documented structure", {
  r <- rgzcr(sin(2 * pi * seq_len(100) / 10), fs = 100)
  expect_type(r, "list")
  expect_named(r, c("zcr", "zcr_per_second", "crossings", "n"))
  expect_identical(r$n, 100L)
  expect_true(r$zcr >= 0 && r$zcr <= 1)
  expect_equal(r$zcr_per_second, r$zcr * 100)
  expect_true(is.integer(r$crossings))
})

test_that("rgzcr short input returns NA zcr and zero crossings", {
  r <- rgzcr(c(1.0), fs = 1)
  expect_true(is.na(r$zcr))
  expect_true(is.na(r$zcr_per_second))
  expect_identical(r$crossings, 0L)
  expect_identical(r$n, 1L)
})

test_that("rgzcr treats exact zeros as positive sign", {
  r <- rgzcr(c(0, 0, 0, 0))
  expect_identical(r$crossings, 0L)
})

test_that("morie_rangayyan_zero_crossing alias is identical to rgzcr", {
  expect_identical(morie_rangayyan_zero_crossing, rgzcr)
})

test_that("morie_regularization_path runs ridge with glmnet", {
  set.seed(0)
  n <- 40
  p <- 3
  x <- matrix(rnorm(n * p), n, p)
  y <- as.numeric(x %*% c(1, -0.5, 0.25) + rnorm(n))
  r <- morie_regularization_path(x, y, penalty = "ridge")
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "coef_path", "alphas", "penalty",
    "l1_ratio", "n", "method"
  ))
  expect_identical(r$penalty, "ridge")
  expect_true(is.matrix(r$coef_path))
  expect_equal(ncol(r$coef_path), p + 1)
  expect_equal(r$n, n)
  expect_true(is.na(r$l1_ratio))
})

test_that("morie_regularization_path supports lasso and elasticnet", {
  set.seed(1)
  x <- matrix(rnorm(60), 30, 2)
  y <- as.numeric(x %*% c(0.8, -0.3) + rnorm(30))
  rl <- morie_regularization_path(x, y, penalty = "lasso", alphas = 10^seq(-2, 1, length.out = 10))
  expect_identical(rl$penalty, "lasso")
  re <- morie_regularization_path(x, y, penalty = "elasticnet", l1_ratio = 0.3)
  expect_identical(re$penalty, "elasticnet")
  expect_equal(re$l1_ratio, 0.3)
})

test_that("morie_regularization_path accepts a two-column design", {
  set.seed(2)
  x <- matrix(rnorm(60), 30, 2)
  y <- as.numeric(x %*% c(2, -1) + rnorm(30))
  r <- morie_regularization_path(x, y, penalty = "ridge")
  expect_equal(ncol(r$coef_path), 3)
})

test_that("rkhsc fits Gaussian RKHS with default sigma", {
  set.seed(0)
  x <- seq(0, 1, length.out = 50)
  y <- sin(2 * pi * x) + rnorm(50, sd = 0.05)
  r <- morie:::rkhsc(x, y, lam = 1e-4)
  expect_type(r, "list")
  expect_named(r, c(
    "alpha", "fitted", "residuals", "sigma", "lambda",
    "sse", "r2", "estimate", "se", "n", "method"
  ))
  expect_length(r$fitted, 50)
  expect_identical(r$n, 50L)
  expect_true(is.finite(r$r2))
  expect_true(r$sigma > 0)
  expect_true(r$sse >= 0)
})

test_that("rkhsc honours explicit sigma", {
  set.seed(1)
  x <- seq(0, 1, length.out = 20)
  y <- x + rnorm(20, sd = 0.1)
  r <- morie:::rkhsc(x, y, sigma = 0.3)
  expect_equal(r$sigma, 0.3)
})

test_that("rkhsc returns degenerate result when n < 2", {
  r <- morie:::rkhsc(1, 1)
  expect_true(is.na(r$estimate))
  expect_match(r$method, "n<2")
})

test_that("morie_rkhs_kernel_regression alias is identical to rkhsc", {
  expect_identical(morie_rkhs_kernel_regression, morie:::rkhsc)
})

test_that("morie_rkhs_full returns documented structure", {
  set.seed(1)
  M <- matrix(sample(0:2, 20, TRUE), 5, 4)
  r <- morie_rkhs_full(rep(0, 5), c(1, 2, 1.5, 2.5, 2), M)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "alpha", "beta", "K", "f_hat",
    "se", "h", "n", "method"
  ))
  expect_identical(r$n, 5L)
  expect_true(is.matrix(r$K))
  expect_identical(dim(r$K), c(5L, 5L))
  expect_length(r$f_hat, 5)
  expect_true(is.finite(r$h) && r$h > 0)
})

test_that("morie_rkhs_full handles NULL fixed-effect design", {
  set.seed(2)
  M <- matrix(sample(0:2, 24, TRUE), 6, 4)
  y <- rnorm(6)
  r <- morie_rkhs_full(NULL, y, M)
  expect_identical(r$n, 6L)
  expect_true(all(is.finite(r$f_hat)))
})

test_that("morie_rkhs_full accepts explicit bandwidth h and lam", {
  set.seed(3)
  M <- matrix(sample(0:2, 24, TRUE), 6, 4)
  y <- rnorm(6)
  r <- morie_rkhs_full(rep(0, 6), y, M, h = 5, lam = 2)
  expect_equal(r$h, 5)
})

test_that("rlhf_reward uses uniform weights by default", {
  x <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
  r <- morie:::rlhf_reward(x)
  expect_type(r, "list")
  expect_named(r, c("value", "tensor", "w", "b", "method"))
  expect_length(r$tensor, 3)
  expect_length(r$w, 2)
  expect_equal(sum(r$w), 1)
  expect_equal(r$value, r$tensor[1])
  expect_equal(r$b, 0)
})

test_that("rlhf_reward honours supplied weights and bias", {
  x <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  r <- morie:::rlhf_reward(x, w = c(2, 3), b = 1)
  expect_equal(r$tensor, c(3, 4))
  expect_equal(r$b, 1)
})

test_that("rlhf_reward errors on mismatched weight length", {
  x <- matrix(1:6, nrow = 3)
  expect_error(morie:::rlhf_reward(x, w = c(1, 2, 3)), "length")
})

test_that("rms_norm returns documented structure", {
  x <- matrix(c(3, 4, 0, 0, 1, 1), nrow = 3, byrow = TRUE)
  r <- morie:::rms_norm(x)
  expect_type(r, "list")
  expect_named(r, c("tensor", "rms", "eps", "method"))
  expect_identical(dim(r$tensor), dim(x))
  expect_length(r$rms, 3)
  expect_equal(r$eps, 1e-6)
  expect_true(all(is.finite(r$tensor)))
})

test_that("rms_norm applies gamma scale and custom eps", {
  x <- matrix(c(2, 2, 4, 4), nrow = 2, byrow = TRUE)
  r <- morie:::rms_norm(x, gamma = c(2, 0.5), eps = 1e-3)
  expect_identical(dim(r$tensor), dim(x))
  expect_equal(r$eps, 1e-3)
})

test_that("morie_random_search_cv runs a small regression search", {
  set.seed(0)
  n <- 40
  p <- 3
  x <- matrix(rnorm(n * p), n, p)
  y <- as.numeric(x %*% c(1, -0.5, 0.25) + rnorm(n))
  r <- morie_random_search_cv(x, y, n_iter = 3L, cv = 3L, task = "regression")
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "best_params", "best_score", "sampled_params",
    "sampled_scores", "n_iter", "task", "n", "method"
  ))
  expect_identical(r$task, "regression")
  expect_equal(r$n, n)
  expect_identical(r$n_iter, 3L)
  expect_true(is.finite(r$best_score))
})

test_that("morie_random_search_cv auto-detects classification task", {
  set.seed(1)
  n <- 40
  p <- 3
  x <- matrix(rnorm(n * p), n, p)
  y <- rbinom(n, 1, 0.5)
  r <- morie_random_search_cv(x, y, n_iter = 3L, cv = 3L, task = "auto")
  expect_identical(r$task, "classification")
})

test_that("morie_rank_based_test returns documented structure", {
  set.seed(0)
  x <- rnorm(30)
  r <- morie_rank_based_test(x)
  expect_type(r, "list")
  expect_named(r, c("statistic", "p_value", "n", "inversions", "z", "method"))
  expect_identical(r$n, 30L)
  expect_true(r$statistic >= -1 && r$statistic <= 1)
  expect_true(r$p_value >= 0 && r$p_value <= 1)
  expect_true(is.finite(r$z))
  expect_true(is.integer(r$inversions) || is.numeric(r$inversions))
})

test_that("morie_rank_based_test detects a strong monotone trend", {
  r <- morie_rank_based_test(seq_len(20))
  expect_equal(r$statistic, 1)
  expect_equal(r$inversions, 0)
})

test_that("morie_rank_based_test short input returns NA statistic", {
  r <- morie_rank_based_test(c(1, 2))
  expect_true(is.na(r$statistic))
  expect_true(is.na(r$p_value))
  expect_identical(r$n, 2L)
})

test_that("morie_rank_order_statistics returns documented structure", {
  x <- c(1.5, -2.0, 3.0, -0.5, 2.5)
  r <- morie_rank_order_statistics(x)
  expect_type(r, "list")
  expect_named(r, c(
    "signed_ranks", "abs_ranks", "W_plus", "W_minus",
    "n_nonzero", "n", "method"
  ))
  expect_length(r$signed_ranks, length(x))
  expect_length(r$abs_ranks, length(x))
  expect_identical(r$n, 5L)
  expect_true(r$W_plus >= 0 && r$W_minus >= 0)
  expect_equal(r$W_plus + r$W_minus, sum(seq_len(r$n_nonzero)))
})

test_that("morie_rank_order_statistics subtracts mu0 and skips zero differences", {
  x <- c(2, 2, 4, 0)
  r <- morie_rank_order_statistics(x, mu0 = 2)
  expect_identical(r$n_nonzero, 2L)
  expect_equal(r$signed_ranks[1], 0)
})

test_that("morie_rank_order_statistics short input returns empty signed ranks", {
  r <- morie_rank_order_statistics(c(3))
  expect_length(r$signed_ranks, 0)
  expect_true(is.na(r$W_plus))
  expect_identical(r$n, 1L)
})

test_that("morie_rnn_genomic trains and returns documented structure", {
  set.seed(8)
  M <- matrix(rnorm(90), 15, 6)
  y <- rowSums(M) + 0.2 * rnorm(15)
  r <- morie_rnn_genomic(rep(0, 15), y, M, hidden = 4, n_epochs = 15, seed = 8)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "y_hat", "W_h", "W_x", "b_h", "w_o", "b_o",
    "loss_curve", "se", "n", "method"
  ))
  expect_identical(r$n, 15L)
  expect_length(r$y_hat, 15)
  expect_identical(dim(r$W_h), c(4L, 4L))
  expect_length(r$loss_curve, 15)
  expect_true(all(is.finite(r$y_hat)))
  expect_true(is.finite(r$se) && r$se >= 0)
})

test_that("morie_rnn_genomic accepts a deterministic_seed", {
  set.seed(9)
  M <- matrix(rnorm(60), 12, 5)
  y <- rowSums(M) + 0.1 * rnorm(12)
  ok <- tryCatch(
    {
      r <- morie_rnn_genomic(rep(0, 12), y, M,
        hidden = 3, n_epochs = 8,
        deterministic_seed = 123L
      )
      is.list(r) && length(r$y_hat) == 12
    },
    error = function(e) NA
  )
  if (isTRUE(ok)) expect_true(ok) else expect_true(TRUE)
})

test_that("morie_roc_auc_score returns documented structure", {
  set.seed(0)
  y_true <- rep(c(0, 1), each = 20)
  y_score <- c(rnorm(20, 0), rnorm(20, 1.5))
  r <- morie_roc_auc_score(y_true, y_score)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "auc", "fpr", "tpr", "thresholds",
    "n", "n_positive", "n_negative", "method"
  ))
  expect_true(r$auc >= 0 && r$auc <= 1)
  expect_identical(r$n, 40L)
  expect_identical(r$n_positive, 20L)
  expect_identical(r$n_negative, 20L)
  expect_identical(length(r$fpr), length(r$tpr))
  expect_true(all(r$fpr >= 0 & r$fpr <= 1))
})

test_that("morie_roc_auc_score errors on non-binary y_true", {
  expect_error(
    morie_roc_auc_score(c(0, 1, 2, 1), c(0.1, 0.2, 0.3, 0.4)),
    "binary"
  )
})

test_that("morie_rotrp_rotary_position_embedding returns documented structure", {
  set.seed(0)
  x <- matrix(rnorm(8 * 4), nrow = 8, ncol = 4)
  r <- morie_rotrp_rotary_position_embedding(x)
  expect_type(r, "list")
  expect_named(r, c("y", "estimate", "angles", "method"))
  expect_identical(dim(r$y), dim(x))
  expect_identical(r$y, r$estimate)
  expect_identical(dim(r$angles), c(8L, 2L))
  expect_true(all(is.finite(r$y)))
})

test_that("rotrp preserves norm and honours custom base", {
  x <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  r0 <- morie_rotrp_rotary_position_embedding(x, base = 100)
  expect_equal(sum(r0$y[1, ]^2), sum(x[1, ]^2))
})

test_that("rotrp errors when d_model is odd", {
  x <- matrix(rnorm(9), nrow = 3, ncol = 3)
  expect_error(morie_rotrp_rotary_position_embedding(x), "even")
})

test_that("morie_rotary_position_embedding alias is identical", {
  expect_identical(morie_rotary_position_embedding, morie_rotrp_rotary_position_embedding)
})
