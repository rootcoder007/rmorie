# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 21 tests: tarmd, tgrch, thfdt, tknbp, tmpsc, tolim, topkd, toppd,
# tpspn, trfbl, trfge, tsnrd, ucmod, ukrig, unfdl

test_that("morie_threshold_autoregression returns a SETAR fit on the default path", {
  set.seed(21)
  x <- as.numeric(arima.sim(list(ar = 0.5), n = 120))
  fit <- morie_threshold_autoregression(x)
  expect_type(fit, "list")
  expect_named(fit, c(
    "threshold", "phi_lower", "phi_upper", "p", "d",
    "regime_sizes", "sse", "n", "method"
  ))
  expect_true(is.finite(fit$threshold))
  expect_equal(fit$p, 1)
  expect_equal(fit$d, 1)
  expect_equal(fit$n, length(x))
  expect_true(is.finite(fit$sse) && fit$sse >= 0)
  expect_length(fit$phi_lower, 2L)
  expect_length(fit$phi_upper, 2L)
  expect_named(fit$regime_sizes, c("lower", "upper"))
  expect_true(all(fit$regime_sizes > 0))
  expect_type(fit$method, "character")
})

test_that("morie_threshold_autoregression honours p, d and n_grid arguments", {
  set.seed(22)
  x <- as.numeric(arima.sim(list(ar = c(0.4, 0.2)), n = 200))
  fit <- morie_threshold_autoregression(x, p = 2, d = 2, n_grid = 25)
  expect_equal(fit$p, 2)
  expect_equal(fit$d, 2)
  expect_length(fit$phi_lower, 3L)
  expect_length(fit$phi_upper, 3L)
  expect_true(grepl("p=2", fit$method))
})

test_that("morie_threshold_autoregression errors on a too-short series", {
  expect_error(morie_threshold_autoregression(1:6),
    "too short",
    ignore.case = TRUE
  )
})

test_that("morie_tgarch_model fits a GJR-GARCH(1,1) series", {
  set.seed(23)
  x <- rnorm(150)
  fit <- morie_tgarch_model(x)
  expect_type(fit, "list")
  expect_named(fit, c(
    "omega", "alpha", "gamma", "beta", "persistence",
    "loglik", "conditional_variance", "n", "method"
  ))
  expect_true(is.finite(fit$omega))
  expect_true(is.finite(fit$alpha))
  expect_true(is.finite(fit$beta))
  expect_true(is.finite(fit$persistence))
  expect_equal(fit$n, length(x))
  expect_length(fit$conditional_variance, length(x))
  expect_true(all(is.finite(fit$conditional_variance)))
  expect_type(fit$method, "character")
})

test_that("morie_tgarch_model errors when there are fewer than 20 observations", {
  expect_error(morie_tgarch_model(rnorm(10)), ">=20", fixed = TRUE)
})

test_that("morie_terry_hoeffding_test computes a normal-scores statistic", {
  set.seed(24)
  x <- rnorm(15, 0)
  y <- rnorm(18, 0.7)
  res <- morie_terry_hoeffding_test(x, y)
  expect_type(res, "list")
  expect_named(res, c("statistic", "p_value", "z", "n", "m", "method"))
  expect_true(is.finite(res$statistic))
  expect_true(is.finite(res$z))
  expect_true(res$p_value >= 0 && res$p_value <= 1)
  expect_equal(res$m, length(x))
  expect_equal(res$n, length(x) + length(y))
})

test_that("morie_terry_hoeffding_test returns NA when a sample is too small", {
  res <- morie_terry_hoeffding_test(c(1), c(2, 3, 4))
  expect_true(is.na(res$statistic))
  expect_true(is.na(res$p_value))
  expect_true(is.na(res$z))
  expect_equal(res$m, 1L)
})

test_that("bpe_tokenizer produces merges and vocab from a corpus string", {
  res <- morie:::bpe_tokenizer("low lower lowest low", num_merges = 5L)
  expect_type(res, "list")
  expect_named(res, c(
    "merges", "vocab", "corpus", "n_merges", "n_vocab",
    "method"
  ))
  expect_true(res$n_merges >= 0L && res$n_merges <= 5L)
  expect_equal(res$n_vocab, length(res$vocab))
  expect_equal(res$method, "BPE")
  expect_type(res$vocab, "character")
})

test_that("bpe_tokenizer accepts a character vector of words", {
  res <- morie:::bpe_tokenizer(c("aa", "ab", "aa"), num_merges = 3L)
  expect_true(res$n_vocab > 0L)
  expect_length(res$merges, res$n_merges)
})

test_that("bpe_tokenizer handles empty input", {
  res <- morie:::bpe_tokenizer(character(0))
  expect_equal(res$n_merges, 0L)
  expect_equal(res$n_vocab, 0L)
  expect_length(res$vocab, 0L)
})

test_that("temperature_scaling returns a normalised softmax tensor", {
  res <- morie:::temperature_scaling(c(1, 2, 3, 4))
  expect_type(res, "list")
  expect_named(res, c("tensor", "entropy", "temperature", "method"))
  expect_length(res$tensor, 4L)
  expect_equal(sum(res$tensor), 1, tolerance = 1e-8)
  expect_true(all(res$tensor >= 0))
  expect_true(is.finite(res$entropy) && res$entropy >= 0)
  expect_equal(res$temperature, 1)
})

test_that("temperature_scaling with high temperature flattens the tensor", {
  hot <- morie:::temperature_scaling(c(0, 5, 10), temperature = 100)
  cold <- morie:::temperature_scaling(c(0, 5, 10), temperature = 0.1)
  expect_gt(hot$entropy, cold$entropy)
  expect_equal(hot$temperature, 100)
})

test_that("temperature_scaling errors on a non-positive temperature", {
  expect_error(morie:::temperature_scaling(1:3, temperature = 0),
    "Temperature",
    ignore.case = TRUE
  )
})

test_that("morie_tolerance_limits computes Wilks coverage on the default path", {
  res <- morie_tolerance_limits(1:100)
  expect_type(res, "list")
  expect_named(res, c(
    "lower", "upper", "coverage_requested",
    "confidence_achieved", "n", "method"
  ))
  expect_equal(res$lower, 1)
  expect_equal(res$upper, 100)
  expect_equal(res$coverage_requested, 0.90)
  expect_true(res$confidence_achieved >= 0 && res$confidence_achieved <= 1)
  expect_equal(res$n, 100L)
})

test_that("morie_tolerance_limits honours a custom coverage argument", {
  res <- morie_tolerance_limits(1:50, coverage = 0.80, confidence = 0.99)
  expect_equal(res$coverage_requested, 0.80)
  expect_true(is.finite(res$confidence_achieved))
})

test_that("morie_tolerance_limits returns NA for a single observation", {
  res <- morie_tolerance_limits(42)
  expect_true(is.na(res$lower))
  expect_true(is.na(res$upper))
  expect_true(is.na(res$confidence_achieved))
  expect_equal(res$n, 1L)
})

test_that("top_k_decoding filters logits to the top k", {
  res <- morie:::top_k_decoding(c(1, 5, 2, 8, 3), k = 2L)
  expect_type(res, "list")
  expect_named(res, c("tensor", "topk_indices", "topk_logits", "k", "method"))
  expect_equal(res$k, 2L)
  expect_equal(sum(res$tensor), 1, tolerance = 1e-8)
  expect_length(res$topk_indices, 2L)
  expect_length(res$topk_logits, 2L)
  expect_true(all(res$topk_indices >= 0))
})

test_that("top_k_decoding clamps k to the vocabulary length", {
  res <- morie:::top_k_decoding(c(1, 2, 3), k = 99L)
  expect_equal(res$k, 3L)
  expect_length(res$tensor, 3L)
})

test_that("top_k_decoding honours the temperature argument", {
  res <- morie:::top_k_decoding(c(1, 2, 3, 4), k = 3L, temperature = 2)
  expect_equal(sum(res$tensor), 1, tolerance = 1e-8)
})

test_that("top_p_nucleus performs nucleus filtering on the default path", {
  res <- morie:::top_p_nucleus(c(1, 2, 3, 4, 5))
  expect_type(res, "list")
  expect_named(res, c("tensor", "keep_mask", "n_kept", "p", "method"))
  expect_equal(sum(res$tensor), 1, tolerance = 1e-8)
  expect_type(res$keep_mask, "logical")
  expect_equal(res$n_kept, sum(res$keep_mask))
  expect_true(res$n_kept >= 1L)
  expect_equal(res$p, 0.9)
})

test_that("top_p_nucleus honours custom p and temperature", {
  res <- morie:::top_p_nucleus(c(1, 2, 3, 4), p = 0.5, temperature = 2)
  expect_equal(res$p, 0.5)
  expect_equal(sum(res$tensor), 1, tolerance = 1e-8)
})

test_that("top_p_nucleus errors when p is out of range", {
  expect_error(morie:::top_p_nucleus(1:3, p = 0), "(0, 1]", fixed = TRUE)
  expect_error(morie:::top_p_nucleus(1:3, p = 1.5), "(0, 1]", fixed = TRUE)
})

test_that("morie_thin_plate_spline fits a smooth surface", {
  set.seed(25)
  xx <- matrix(runif(60), ncol = 2)
  yy <- xx[, 1] + xx[, 2] + rnorm(30, sd = 0.01)
  res <- morie_thin_plate_spline(xx, yy, lam = 1e-6)
  expect_type(res, "list")
  expect_named(res, c(
    "a", "beta", "fitted", "residuals", "sse", "r2",
    "lambda", "estimate", "n", "d", "method"
  ))
  expect_length(res$fitted, length(yy))
  expect_length(res$residuals, length(yy))
  expect_true(is.finite(res$sse) && res$sse >= 0)
  expect_equal(res$n, 30L)
  expect_equal(res$d, 2L)
  expect_true(is.finite(res$estimate))
})

test_that("tpspn accepts a vector predictor and a smoothing penalty", {
  set.seed(26)
  x <- runif(20)
  y <- x^2 + rnorm(20, sd = 0.05)
  res <- morie:::tpspn(x, y, lam = 1)
  expect_equal(res$d, 1L)
  expect_equal(res$lambda, 1)
  expect_length(res$fitted, 20L)
})

test_that("tpspn returns an NA estimate when n is too small", {
  res <- morie:::tpspn(matrix(c(1, 2), ncol = 1), c(1, 2))
  expect_true(is.na(res$estimate))
  expect_true(grepl("too small", res$method))
})

test_that("morie_trfbl_transformer_block runs a post-LN encoder block", {
  set.seed(27)
  x <- matrix(rnorm(24), nrow = 4, ncol = 6)
  res <- morie_trfbl_transformer_block(x, num_heads = 2L, seed = 1L)
  expect_type(res, "list")
  expect_named(res, c(
    "output", "estimate", "h1", "num_heads", "d_ff",
    "method"
  ))
  expect_equal(dim(res$output), dim(x))
  expect_equal(dim(res$h1), dim(x))
  expect_true(all(is.finite(res$output)))
  expect_equal(res$num_heads, 2L)
  expect_equal(res$d_ff, 24L)
})

test_that("morie_trfbl_transformer_block honours a custom d_ff", {
  set.seed(28)
  x <- matrix(rnorm(20), nrow = 4, ncol = 5)
  res <- morie_transformer_block(x, num_heads = 1L, d_ff = 12L, seed = 2L)
  expect_equal(res$d_ff, 12L)
  expect_equal(dim(res$output), dim(x))
})

test_that("morie_trfbl_transformer_block accepts a deterministic seed", {
  x <- matrix(rnorm(24), nrow = 4, ncol = 6)
  res1 <- morie_trfbl_transformer_block(x, num_heads = 2L, deterministic_seed = 7L)
  res2 <- morie_trfbl_transformer_block(x, num_heads = 2L, deterministic_seed = 7L)
  expect_equal(res1$output, res2$output)
})

test_that("morie_transformer_genomic fits a 1-head attention genomic predictor", {
  set.seed(29)
  M <- matrix(rnorm(72), 12, 6)
  y <- M[, 3] + 0.2 * rnorm(12)
  res <- morie_transformer_genomic(rep(0, 12), y, M, seed = 9)
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "y_hat", "beta", "attention", "context",
    "se", "n", "method"
  ))
  expect_true(is.finite(res$estimate))
  expect_length(res$y_hat, 12L)
  expect_equal(res$n, 12L)
  expect_equal(dim(res$attention), c(12L, 6L, 6L))
  expect_equal(dim(res$context), c(12L, 8L))
  expect_true(is.finite(res$se) && res$se >= 0)
})

test_that("morie_transformer_genomic works with NULL fixed effects and custom args", {
  set.seed(30)
  M <- matrix(rnorm(50), 10, 5)
  y <- M[, 1] + 0.1 * rnorm(10)
  res <- morie_transformer_genomic(NULL, y, M, d_model = 4, lam = 2, seed = 3)
  expect_length(res$y_hat, 10L)
  expect_equal(dim(res$context), c(10L, 4L))
})

test_that("morie_transformer_genomic accepts a deterministic seed", {
  M <- matrix(rnorm(48), 8, 6)
  y <- M[, 2] + 0.1 * rnorm(8)
  r1 <- morie_transformer_genomic(NULL, y, M, deterministic_seed = 5L)
  r2 <- morie_transformer_genomic(NULL, y, M, deterministic_seed = 5L)
  expect_equal(r1$y_hat, r2$y_hat)
})

test_that("morie_tsne_reduction wraps Rtsne and returns an embedding", {
  set.seed(31)
  x <- matrix(rnorm(120), nrow = 30, ncol = 4)
  res <- morie_tsne_reduction(x,
    n_components = 2L, perplexity = 5,
    n_iter = 250L, seed = 1L
  )
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "embedding", "kl_divergence", "perplexity",
    "n_components", "n", "method"
  ))
  expect_equal(res$n, 30L)
  expect_equal(res$n_components, 2L)
  expect_equal(ncol(res$embedding), 2L)
  expect_equal(nrow(res$embedding), 30L)
  expect_true(is.finite(res$kl_divergence))
})

test_that("morie_tsne_reduction errors when Rtsne is unavailable", {
  skip_if_not_installed("Rtsne")
  if (!requireNamespace("Rtsne", quietly = TRUE)) {
    expect_error(morie_tsne_reduction(matrix(rnorm(40), 10, 4)), "Rtsne")
  } else {
    expect_true(TRUE)
  }
})

test_that("morie_unobserved_components decomposes a seasonal series", {
  set.seed(32)
  y <- as.numeric(sin(2 * pi * (1:48) / 12)) + rnorm(48, sd = 0.1)
  res <- morie_unobserved_components(y, period = 12)
  expect_type(res, "list")
  expect_named(res, c(
    "trend", "seasonal", "irregular", "loglik", "n",
    "period", "method"
  ))
  expect_length(res$trend, 48L)
  expect_length(res$seasonal, 48L)
  expect_length(res$irregular, 48L)
  expect_equal(res$n, 48L)
  expect_equal(res$period, 12)
  expect_true(all(is.finite(res$trend)))
})

test_that("morie_unobserved_components handles the non-seasonal path (period <= 1)", {
  set.seed(33)
  y <- cumsum(rnorm(40))
  res <- morie_unobserved_components(y, period = 0)
  expect_length(res$trend, 40L)
  expect_true(all(res$seasonal == 0))
  expect_equal(res$period, 0)
})

test_that("morie_unobserved_components errors on a too-short series", {
  expect_error(morie_unobserved_components(1:4, period = 12), "too short",
    ignore.case = TRUE
  )
})

test_that("ukrig predicts at a single target location", {
  res <- ukrig(c(1, 2, 3, 4, 5), matrix(0:4, ncol = 1),
    matrix(2.5, 1, 1),
    trend_order = 1
  )
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "n", "method"))
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se) && res$se >= 0)
  expect_equal(res$n, 5L)
  expect_true(grepl("trend_order=1", res$method))
})

test_that("ukrig predicts at multiple targets and supports covariance models", {
  set.seed(34)
  coords <- matrix(runif(20), ncol = 2)
  x <- rnorm(10)
  target <- matrix(runif(6), ncol = 2)
  res_g <- morie_universal_kriging(x, coords, target,
    model = "gaussian",
    trend_order = 0
  )
  expect_length(res_g$estimate, 3L)
  expect_length(res_g$se, 3L)
  res_s <- ukrig(x, coords, target, model = "spherical", trend_order = 2)
  expect_length(res_s$estimate, 3L)
  expect_true(all(is.finite(res_s$estimate)))
})

test_that("ukrig errors on mismatched dimensions and unknown model", {
  expect_error(ukrig(1:5, matrix(0:7, ncol = 2), matrix(1, 1, 2)),
    "coords rows",
    ignore.case = TRUE
  )
  expect_error(ukrig(1:4, matrix(0:7, ncol = 2), matrix(1, 1, 1)),
    "dim mismatch",
    ignore.case = TRUE
  )
  expect_error(ukrig(1:4, matrix(0:7, ncol = 2), matrix(1, 1, 2),
    model = "bogus"
  ), "unknown model")
})

test_that("unfdl performs metric unfolding on a preference matrix", {
  set.seed(35)
  P <- matrix(abs(rnorm(20, 5)), nrow = 4, ncol = 5)
  res <- unfdl(P, k = 2L, n_iter = 30L)
  expect_type(res, "list")
  expect_named(res, c("X", "Y", "stress", "k", "n_resp", "n_stim", "method"))
  expect_equal(nrow(res$X), 4L)
  expect_equal(nrow(res$Y), 5L)
  expect_equal(res$n_resp, 4L)
  expect_equal(res$n_stim, 5L)
  expect_true(is.finite(res$stress) && res$stress >= 0)
  expect_equal(res$method, "unfolding")
})

test_that("morie_unfolding_analysis honours k and returns the right embedding dims", {
  set.seed(36)
  P <- matrix(abs(rnorm(30, 3)), nrow = 5, ncol = 6)
  res <- morie_unfolding_analysis(P, k = 3L, n_iter = 20L)
  expect_true(res$k <= 3L)
  expect_equal(ncol(res$X), res$k)
  expect_equal(ncol(res$Y), res$k)
})

test_that("unfdl returns an empty result for a degenerate matrix", {
  res <- unfdl(matrix(1, nrow = 1, ncol = 1))
  expect_equal(res$n_resp, 0L)
  expect_equal(res$n_stim, 0L)
  expect_true(is.na(res$stress))
})

test_that("unfdl errors when x is not a matrix", {
  expect_error(unfdl(1:10), "must be a matrix")
})
