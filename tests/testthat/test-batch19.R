# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 19: rptpn, rslnk, sampling, sarla, sarre, sglm, sgnpw, signal,
#           smixd, sobls, spblk, spcrs, specf, spqkv, sptag

test_that("repetition_penalty: alpha == 1 short-circuits with no penalised idx", {
  z <- c(-1, 0.5, 2, -3)
  res <- morie:::repetition_penalty(z, generated = c(0L, 2L), alpha = 1)
  expect_type(res, "list")
  expect_named(res, c("tensor", "penalised_idx", "alpha", "method"))
  expect_equal(res$tensor, z)
  expect_length(res$penalised_idx, 0L)
  expect_equal(res$alpha, 1)
  expect_identical(res$method, "rep-penalty")
})

test_that("repetition_penalty: positive logits divided, negative multiplied", {
  z <- c(2, -2, 0.5, -1)
  res <- morie:::repetition_penalty(z, generated = c(0L, 1L), alpha = 1.5)
  expect_named(res, c("tensor", "penalised_idx", "alpha", "method"))
  expect_equal(res$tensor[1], 2 / 1.5)
  expect_equal(res$tensor[2], -2 * 1.5)
  expect_true(all(is.finite(res$tensor)))
  expect_equal(sort(res$penalised_idx), c(0L, 1L))
})

test_that("repetition_penalty: out-of-range / duplicate generated ids dropped", {
  z <- c(1, 2, 3)
  res <- morie:::repetition_penalty(z,
    generated = c(0L, 0L, 99L, -5L),
    alpha = 1.2
  )
  expect_equal(res$penalised_idx, 0L)
  expect_length(res$tensor, 3L)
})

test_that("morie_rslnk_residual_connection: identity branch doubles input", {
  x <- array(1:8, dim = c(2, 4))
  res <- morie_rslnk_residual_connection(x)
  expect_type(res, "list")
  expect_named(res, c("y", "estimate", "Fx", "method"))
  expect_equal(res$y, x + x)
  expect_equal(res$estimate, res$y)
  expect_identical(res$method, "Residual identity shortcut")
})

test_that("morie_rslnk_residual_connection: custom residual branch applied", {
  x <- c(1, 2, 3, 4)
  res <- morie_rslnk_residual_connection(x, f = function(z) z * 2)
  expect_equal(as.numeric(res$y), x * 2 + x)
  expect_true(all(is.finite(res$y)))
})

test_that("morie_rslnk_residual_connection: shape mismatch errors", {
  expect_error(
    morie_rslnk_residual_connection(c(1, 2, 3), f = function(z) c(1, 2)),
    "shape"
  )
})

test_that("morie_residual_connection alias is identical", {
  expect_identical(morie_residual_connection, morie_rslnk_residual_connection)
})

test_that("morie_simple_random_sample: WOR returns n rows and weight column", {
  df <- data.frame(x = 1:100)
  out <- morie_simple_random_sample(df, 20)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 20L)
  expect_true(".weight" %in% names(out))
  expect_equal(unique(out$.weight), 100 / 20)
})

test_that("morie_simple_random_sample: with replacement gives unit weights", {
  df <- data.frame(x = 1:10)
  out <- morie_simple_random_sample(df, 25, replace = TRUE, seed = 1L)
  expect_equal(nrow(out), 25L)
  expect_equal(unique(out$.weight), 1)
})

test_that("morie_simple_random_sample: n exceeding population WOR errors", {
  expect_error(morie_simple_random_sample(data.frame(x = 1:5), 10), "exceeds")
})

test_that("morie_stratified_sample: equal allocation per stratum", {
  set.seed(1)
  df <- data.frame(g = c(rep("A", 60), rep("B", 40)), x = rnorm(100))
  out <- morie_stratified_sample(df, "g", n_per_stratum = 10)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 20L)
  expect_true(all(is.finite(out$.weight)))
})

test_that("morie_stratified_sample: proportional allocation uses total n", {
  set.seed(1)
  df <- data.frame(g = c(rep("A", 60), rep("B", 40)), x = rnorm(100))
  out <- morie_stratified_sample(df, "g", n_per_stratum = 20, proportional = TRUE)
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) >= 1L)
  expect_true(all(out$.weight > 0))
})

test_that("morie_stratified_sample: proportional with vector n_per_stratum errors", {
  df <- data.frame(g = c("A", "A", "B"), x = 1:3)
  expect_error(
    morie_stratified_sample(df, "g",
      n_per_stratum = c(A = 1, B = 1),
      proportional = TRUE
    ),
    "proportional"
  )
})

test_that("morie_stratified_sample: named vector allocation honoured", {
  set.seed(2)
  df <- data.frame(g = c(rep("A", 30), rep("B", 30)), x = rnorm(60))
  out <- morie_stratified_sample(df, "g", n_per_stratum = c(A = 5L, B = 8L))
  expect_equal(nrow(out), 13L)
})

test_that("morie_cluster_sample: selects whole clusters with cluster weights", {
  df <- data.frame(cl = rep(1:10, each = 5), x = 1:50)
  out <- morie_cluster_sample(df, "cl", n_clusters = 4)
  expect_s3_class(out, "data.frame")
  expect_equal(length(unique(out$cl)), 4L)
  expect_equal(unique(out$.weight), 10 / 4)
})

test_that("morie_cluster_sample: too many clusters errors", {
  df <- data.frame(cl = rep(1:3, each = 2), x = 1:6)
  expect_error(morie_cluster_sample(df, "cl", n_clusters = 5), "exceeds")
})

test_that("morie_pps_sample: returns n rows with Hansen-Hurwitz weights", {
  df <- data.frame(s = c(1, 2, 3, 4, 5), x = 1:5)
  # Hansen-Hurwitz is with-replacement; pass replace=TRUE explicitly
  # (default switched to WoR for Python parity 2026-05-22).
  out <- morie_pps_sample(df, "s", n = 10, replace = TRUE)
  expect_equal(nrow(out), 10L)
  expect_true(all(out$.weight > 0))
  expect_true(all(is.finite(out$.weight)))
})

test_that("morie_pps_sample: non-positive size errors", {
  df <- data.frame(s = c(1, 0, 2), x = 1:3)
  expect_error(morie_pps_sample(df, "s", n = 2), "positive")
})

test_that("morie_bootstrap_sample: returns estimate, se, CI and distribution", {
  set.seed(1)
  df <- data.frame(x = rnorm(40))
  res <- morie_bootstrap_sample(df,
    statistic = function(d) mean(d$x),
    n_bootstrap = 50L
  )
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "se", "ci_lower", "ci_upper",
    "distribution"
  ))
  expect_true(is.finite(res$estimate))
  expect_true(res$se >= 0)
  expect_true(res$ci_lower <= res$ci_upper)
  expect_length(res$distribution, 50L)
})

test_that("morie_jackknife_estimate: returns estimate, se and bias", {
  df <- data.frame(x = c(2, 4, 6, 8, 10))
  res <- morie_jackknife_estimate(df, statistic = function(d) mean(d$x))
  expect_named(res, c("estimate", "se", "bias"))
  expect_equal(res$estimate, 6)
  expect_true(res$se >= 0)
  expect_true(is.finite(res$bias))
})

test_that("morie_effective_sample_size: equal weights give n", {
  expect_equal(morie_effective_sample_size(rep(1, 20)), 20)
})

test_that("morie_effective_sample_size: drops NA and non-positive weights", {
  ess <- morie_effective_sample_size(c(1, 2, NA, -1, 3))
  expect_true(is.finite(ess))
  expect_true(ess > 0)
})

test_that("morie_design_effect: equal weights give DEFF of 1", {
  expect_equal(morie_design_effect(rep(2, 15)), 1)
})

test_that("morie_design_effect: unequal weights give DEFF >= 1", {
  expect_true(morie_design_effect(c(1, 1, 5, 10)) >= 1)
})

test_that("morie_compute_design_weights: inverse-probability weights", {
  df <- data.frame(g = c(rep("A", 4), rep("B", 6)))
  w <- morie_compute_design_weights(df, "g",
    population_sizes = c(A = 100L, B = 300L)
  )
  expect_type(w, "double")
  expect_length(w, 10L)
  expect_true(all(w > 0))
})

test_that("morie_calibration_weights: default unit start, converges to totals", {
  df <- data.frame(g = c(rep("f", 5), rep("m", 5)))
  w <- morie_calibration_weights(df,
    aux_vars = "g",
    population_totals = list(g_f = 200, g_m = 100)
  )
  expect_length(w, 10L)
  expect_equal(sum(w[df$g == "f"]), 200)
  expect_equal(sum(w[df$g == "m"]), 100)
})

test_that("morie_calibration_weights: honours supplied initial weights", {
  df <- data.frame(g = c("a", "a", "b", "b"))
  w <- morie_calibration_weights(df,
    aux_vars = "g",
    population_totals = list(g_a = 10),
    initial_weights = c(1, 1, 2, 2),
    max_iter = 5L
  )
  expect_length(w, 4L)
  expect_equal(sum(w[df$g == "a"]), 10)
})

.b19_path_W <- function(n) {
  W <- matrix(0, n, n)
  for (i in seq_len(n)) {
    nb <- intersect(c(i - 1L, i + 1L), seq_len(n))
    W[i, nb] <- 1 / length(nb)
  }
  W
}

test_that("sarla: returns coefficient list on a small path graph", {
  set.seed(11)
  n <- 12
  W <- .b19_path_W(n)
  X <- cbind(1, seq_len(n))
  y <- 1 + 0.5 * seq_len(n) + rnorm(n, sd = 0.3)
  res <- sarla(X, y, W)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "rho", "sigma2", "n", "method"))
  expect_length(res$estimate, 2L)
  expect_true(all(is.finite(res$estimate)))
  expect_true(all(res$se >= 0))
  expect_true(res$rho > -1 && res$rho < 1)
  expect_true(res$sigma2 >= 0)
  expect_equal(res$n, n)
})

test_that("sarla: shape mismatch errors", {
  W <- .b19_path_W(5)
  expect_error(sarla(cbind(1, 1:4), 1:4, W), "shape mismatch")
})

test_that("morie_spatial_ar_lag alias is identical to sarla", {
  expect_identical(morie_spatial_ar_lag, sarla)
})

test_that("sarre: returns coefficient list on a small path graph", {
  set.seed(12)
  n <- 12
  W <- .b19_path_W(n)
  X <- cbind(1, seq_len(n))
  y <- 2 - 0.4 * seq_len(n) + rnorm(n, sd = 0.3)
  res <- sarre(X, y, W)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "lambda", "sigma2", "n", "method"))
  expect_length(res$estimate, 2L)
  expect_true(all(is.finite(res$estimate)))
  expect_true(all(res$se >= 0))
  expect_true(res$lambda > -1 && res$lambda < 1)
  expect_true(res$sigma2 >= 0)
  expect_equal(res$n, n)
})

test_that("sarre: shape mismatch errors", {
  W <- .b19_path_W(6)
  expect_error(sarre(cbind(1, 1:5), 1:5, W), "shape mismatch")
})

test_that("morie_spatial_ar_error alias is identical to sarre", {
  expect_identical(morie_spatial_ar_error, sarre)
})

test_that("sglm: Gaussian fit recovers a coefficient vector", {
  set.seed(13)
  n <- 10
  coords <- matrix(seq_len(n), ncol = 1)
  X <- cbind(1, seq_len(n))
  y <- 1 + 2 * seq_len(n) + rnorm(n, sd = 0.2)
  res <- sglm(X, y, coords)
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "se", "sigma2", "phi", "tau2", "n",
    "method"
  ))
  expect_length(res$estimate, 2L)
  expect_true(all(is.finite(res$estimate)))
  expect_true(all(res$se >= 0))
  expect_true(res$phi > 0)
  expect_equal(res$tau2, 0)
  expect_equal(res$n, n)
})

test_that("sglm: non-gaussian family errors", {
  expect_error(
    sglm(cbind(1, 1:5), 1:5, matrix(1:5, ncol = 1), family = "poisson"),
    "family"
  )
})

test_that("sglm: shape mismatch errors", {
  expect_error(
    sglm(cbind(1, 1:5), 1:5, matrix(1:3, ncol = 1)),
    "shape mismatch"
  )
})

test_that("sglm: accepts list-form coords", {
  set.seed(14)
  n <- 8
  X <- cbind(1, seq_len(n))
  y <- 1 + seq_len(n) + rnorm(n, sd = 0.2)
  res <- sglm(X, y, coords = as.list(seq_len(n)))
  expect_length(res$estimate, 2L)
  expect_true(all(is.finite(res$estimate)))
})

test_that("morie_spatial_glm alias is identical to sglm", {
  expect_identical(morie_spatial_glm, sglm)
})

test_that("morie_sign_test_power: default arguments give a valid power", {
  set.seed(15)
  x <- rnorm(30, mean = 0.5)
  res <- morie_sign_test_power(x)
  expect_type(res, "list")
  expect_true(is.finite(res$statistic))
  expect_true(res$statistic >= 0 && res$statistic <= 1)
  expect_true(res$size <= res$alpha + 1e-9)
  expect_true(res$k_lower <= res$k_upper)
  expect_equal(res$p_alt, 0.7)
})

test_that("morie_sign_test_power: empty effective n returns NA statistic", {
  res <- morie_sign_test_power(rep(0, 5), mu0 = 0)
  expect_true(is.na(res$statistic))
  expect_equal(res$n, 0)
  expect_identical(res$method, "Sign-test power")
})

test_that("morie_sign_test_power: invalid p_alt returns NA statistic", {
  res <- morie_sign_test_power(rnorm(10), p_alt = 1.5)
  expect_true(is.na(res$statistic))
})

test_that("morie_sign_test_power: tiny n with strict alpha has no rejection region", {
  res <- morie_sign_test_power(c(1, -1), mu0 = 0, alpha = 0.001)
  expect_equal(res$statistic, 0)
  expect_equal(res$size, 0)
  expect_true("warnings" %in% names(res))
})

test_that("morie_sign_test_power: optional alpha widens rejection region", {
  set.seed(16)
  x <- rnorm(25, mean = 0.6)
  res <- morie_sign_test_power(x, mu0 = 0, p_alt = 0.8, alpha = 0.10)
  expect_true(is.finite(res$statistic))
  expect_true(res$size <= 0.10 + 1e-9)
})

test_that("buttlp: lowpass filter preserves length", {
  set.seed(1)
  t <- seq(0, 1, length.out = 200)
  x <- sin(2 * pi * 5 * t) + 0.5 * sin(2 * pi * 60 * t)
  res <- buttlp(x, fs = 200, cutoff = 20)
  expect_type(res, "list")
  expect_named(res, c("filtered", "fs", "order", "name"))
  expect_length(res$filtered, length(x))
  expect_true(all(is.finite(res$filtered)))
  expect_identical(res$name, "butter_lowpass")
})

test_that("butthp: highpass filter preserves length", {
  set.seed(1)
  t <- seq(0, 1, length.out = 200)
  x <- 5 * t + sin(2 * pi * 10 * t)
  res <- butthp(x, fs = 200, cutoff = 1)
  expect_length(res$filtered, length(x))
  expect_identical(res$name, "butter_highpass")
})

test_that("buttbp: bandpass filter preserves length", {
  set.seed(1)
  t <- seq(0, 1, length.out = 300)
  x <- sin(2 * pi * 2 * t) + sin(2 * pi * 10 * t)
  res <- buttbp(x, fs = 300, low = 5, high = 20)
  expect_length(res$filtered, length(x))
  expect_identical(res$name, "butter_bandpass")
})

test_that("buttbs: bandstop filter with default cutoffs preserves length", {
  set.seed(1)
  t <- seq(0, 1, length.out = 300)
  x <- sin(2 * pi * 10 * t) + sin(2 * pi * 60 * t)
  res <- buttbs(x, fs = 300)
  expect_length(res$filtered, length(x))
  expect_identical(res$name, "butter_bandstop")
})

test_that("morie_sgolay_smooth: default window/polyorder preserves length", {
  set.seed(1)
  x <- sin(2 * pi * 3 * seq(0, 1, length.out = 120)) + rnorm(120, sd = 0.2)
  res <- morie_sgolay_smooth(x)
  expect_type(res, "list")
  expect_named(res, c("filtered", "name"))
  expect_length(res$filtered, length(x))
  expect_identical(res$name, "savitzky_golay")
})

test_that("morie_hurst_r: returns H and interpretation", {
  set.seed(1)
  x <- cumsum(rnorm(512))
  res <- morie_hurst_r(x)
  expect_type(res, "list")
  expect_named(res, c("H", "interpretation"))
  expect_true(res$interpretation %in%
    c("persistent", "anti-persistent", "random"))
})

test_that("hfd: Python-bridge path is not exercised offline", {
  expect_true(is.function(hfd))
})

test_that("morie_pcg_filter: convenience preset preserves length", {
  set.seed(1)
  x <- rnorm(600)
  res <- morie_pcg_filter(x)
  expect_length(res$filtered, length(x))
  expect_identical(res$name, "butter_bandpass")
})

test_that("smixd: REML fit returns coefficient list", {
  set.seed(17)
  n <- 10
  coords <- matrix(seq_len(n), ncol = 1)
  X <- cbind(1, seq_len(n))
  y <- 1 + 2 * seq_len(n) + rnorm(n, sd = 0.3)
  res <- smixd(X, y, coords)
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "se", "sigma2", "tau2", "phi", "n",
    "method"
  ))
  expect_length(res$estimate, 2L)
  expect_true(all(is.finite(res$estimate)))
  expect_true(all(res$se >= 0))
  expect_true(res$sigma2 >= 0)
  expect_true(res$phi > 0)
  expect_equal(res$n, n)
})

test_that("smixd: accepts list-form coords", {
  set.seed(18)
  n <- 8
  X <- cbind(1, seq_len(n))
  y <- 1 + seq_len(n) + rnorm(n, sd = 0.3)
  res <- smixd(X, y, coords = as.list(seq_len(n)))
  expect_length(res$estimate, 2L)
  expect_true(all(is.finite(res$estimate)))
})

test_that("morie_spatial_mixed_model alias is identical to smixd", {
  expect_identical(morie_spatial_mixed_model, smixd)
})

test_that("sobls: default sample is N-by-d in the unit cube", {
  res <- morie:::sobls(N = 64L, d = 2L)
  expect_type(res, "list")
  expect_true(is.matrix(res$sample))
  expect_equal(dim(res$sample), c(64L, 2L))
  expect_true(all(res$sample >= 0 & res$sample <= 1))
  expect_equal(res$N, 64L)
  expect_equal(res$d, 2L)
  expect_identical(res$method, "Sobol QMC (Sobol 1967)")
})

test_that("sobls: integrand path adds estimate and se", {
  res <- morie:::sobls(
    N = 128L, d = 2L,
    f = function(u) u[1] * u[2], seed = 0L
  )
  expect_true(is.finite(res$estimate))
  expect_true(res$se >= 0)
  expect_true(abs(res$estimate - 0.25) < 0.1)
})

test_that("sobls: no scramble path returns valid sample", {
  res <- morie:::sobls(N = 32L, d = 1L, scramble = FALSE)
  expect_equal(dim(res$sample), c(32L, 1L))
  expect_true(all(is.finite(res$sample)))
})

test_that("morie_sobol_sequence alias is identical to sobls", {
  expect_identical(morie:::morie_sobol_sequence, morie:::sobls)
})

test_that("spblk: box-form 1-D block returns estimate and se", {
  set.seed(19)
  coords <- matrix(seq(0, 10, length.out = 8), ncol = 1)
  x <- 2 + 0.5 * coords[, 1] + rnorm(8, sd = 0.2)
  blocks <- list(matrix(c(2, 4), ncol = 1))
  res <- spblk(x, coords, blocks, n_quad = 9)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "n", "method"))
  expect_length(res$estimate, 1L)
  expect_true(is.finite(res$estimate))
  expect_true(res$se >= 0)
  expect_equal(res$n, 8L)
})

test_that("spblk: explicit quadrature-point block works", {
  set.seed(20)
  coords <- matrix(seq(0, 5, length.out = 6), ncol = 1)
  x <- coords[, 1] + rnorm(6, sd = 0.1)
  blocks <- list(matrix(c(1, 1.5, 2, 2.5), ncol = 1))
  res <- spblk(x, coords, blocks)
  expect_length(res$estimate, 1L)
  expect_true(is.finite(res$se))
})

test_that("spblk: multiple blocks give vector outputs", {
  set.seed(21)
  coords <- matrix(seq(0, 9, length.out = 7), ncol = 1)
  x <- coords[, 1] + rnorm(7, sd = 0.1)
  blocks <- list(matrix(c(1, 2), ncol = 1), matrix(c(5, 6), ncol = 1))
  res <- spblk(x, coords, blocks, n_quad = 9)
  expect_length(res$estimate, 2L)
  expect_length(res$se, 2L)
})

test_that("spblk: 2-D box block quadratured", {
  set.seed(22)
  coords <- as.matrix(expand.grid(0:2, 0:2))
  x <- rowSums(coords) + rnorm(nrow(coords), sd = 0.1)
  blocks <- list(matrix(c(0.5, 0.5, 1.5, 1.5), nrow = 2, byrow = TRUE))
  res <- spblk(x, coords, blocks, n_quad = 9)
  expect_length(res$estimate, 1L)
  expect_true(is.finite(res$estimate))
})

test_that("morie_spatial_block_kriging alias is identical to spblk", {
  expect_identical(morie_spatial_block_kriging, spblk)
})

test_that("spcrs: LOO cross-validation returns MSPE diagnostics", {
  set.seed(23)
  coords <- matrix(seq(0, 10, length.out = 9), ncol = 1)
  x <- 1 + 0.5 * coords[, 1] + rnorm(9, sd = 0.2)
  res <- spcrs(x, coords)
  expect_type(res, "list")
  expect_named(res, c("estimate", "n", "method"))
  expect_named(res$estimate, c("MSPE", "RMSPE", "MAE", "residuals"))
  expect_true(res$estimate$MSPE >= 0)
  expect_equal(res$estimate$RMSPE, sqrt(res$estimate$MSPE))
  expect_true(res$estimate$MAE >= 0)
  expect_length(res$estimate$residuals, 9L)
  expect_equal(res$n, 9L)
})

test_that("spcrs: nugget/sill/range arguments accepted", {
  set.seed(24)
  coords <- matrix(seq(0, 8, length.out = 7), ncol = 1)
  x <- coords[, 1] + rnorm(7, sd = 0.1)
  res <- spcrs(x, coords, nugget = 0.1, sill = 2, range_ = 3)
  expect_true(is.finite(res$estimate$MSPE))
  expect_true(all(is.finite(res$estimate$residuals)))
})

test_that("spcrs: accepts list-form coords", {
  set.seed(25)
  x <- (1:6) + rnorm(6, sd = 0.1)
  res <- spcrs(x, coords = as.list(1:6))
  expect_equal(res$n, 6L)
})

test_that("morie_spatial_cross_validation alias is identical to spcrs", {
  expect_identical(morie_spatial_cross_validation, spcrs)
})

test_that("morie_spectral_density: default arguments give Welch PSD", {
  set.seed(26)
  x <- sin(2 * pi * 0.1 * seq_len(128)) + rnorm(128, sd = 0.3)
  res <- morie_spectral_density(x)
  expect_type(res, "list")
  expect_named(res, c(
    "frequencies", "psd", "n_segments", "nperseg",
    "fs", "n", "method"
  ))
  expect_equal(length(res$frequencies), length(res$psd))
  expect_true(all(res$psd >= 0))
  expect_true(all(is.finite(res$frequencies)))
  expect_true(res$n_segments >= 1L)
  expect_equal(res$n, 128L)
})

test_that("morie_spectral_density: custom fs and nperseg honoured", {
  set.seed(27)
  x <- rnorm(100)
  res <- morie_spectral_density(x, fs = 50, nperseg = 20)
  expect_equal(res$fs, 50)
  expect_equal(res$nperseg, 20L)
  expect_true(max(res$frequencies) <= 25 + 1e-9)
})

test_that("morie_spectral_density: too-short input errors", {
  expect_error(morie_spectral_density(1:5), ">=8")
})

test_that("sparse_attention: scalar N gives N-by-N mask", {
  res <- morie:::sparse_attention(10L, window = 2L, stride = 4L)
  expect_type(res, "list")
  expect_named(res, c("tensor", "boolean", "density", "method"))
  expect_equal(dim(res$boolean), c(10L, 10L))
  expect_true(is.logical(res$boolean))
  expect_true(res$density > 0 && res$density <= 1)
  expect_true(all(res$tensor[res$boolean] == 0))
  expect_true(all(is.infinite(res$tensor[!res$boolean])))
  expect_identical(res$method, "sparse-attention")
})

test_that("sparse_attention: random links increase density", {
  d0 <- morie:::sparse_attention(20L,
    window = 1L, stride = 50L,
    n_random = 0L
  )$density
  d1 <- morie:::sparse_attention(20L,
    window = 1L, stride = 50L,
    n_random = 5L, seed = 1L
  )$density
  expect_true(d1 >= d0)
})

test_that("sparse_attention: vector input uses its length as N", {
  res <- morie:::sparse_attention(rep(0, 6))
  expect_equal(dim(res$boolean), c(6L, 6L))
})

test_that("sptag: pairwise vote agreement matrix is symmetric", {
  M <- matrix(c(
    1, 1, 0,
    1, 0, 0,
    0, 1, 1,
    1, 1, 1
  ), nrow = 4, byrow = TRUE)
  res <- sptag(M)
  expect_type(res, "list")
  expect_named(res, c("agreement", "mean_agreement", "n", "m", "method"))
  expect_equal(dim(res$agreement), c(4L, 4L))
  expect_equal(res$agreement, t(res$agreement))
  expect_true(all(diag(res$agreement) == 1))
  expect_true(res$mean_agreement >= 0 && res$mean_agreement <= 1)
  expect_equal(res$n, 4L)
  expect_equal(res$m, 3L)
})

test_that("sptag: single-row input returns NA mean agreement", {
  res <- sptag(matrix(c(1, 0, 1), nrow = 1))
  expect_true(is.na(res$mean_agreement))
  expect_equal(res$n, 1L)
})

test_that("sptag: mutually-absent pair yields NA cell", {
  M <- matrix(c(
    1, NA,
    NA, 0,
    1, 1
  ), nrow = 3, byrow = TRUE)
  res <- sptag(M)
  expect_true(is.na(res$agreement[1, 2]))
  expect_true(is.finite(res$mean_agreement))
})

test_that("sptag: non-matrix vector input coerced to one column", {
  res <- sptag(c(1, 0, 1, 1))
  expect_equal(res$m, 1L)
  expect_equal(res$n, 4L)
})

test_that("morie_spatial_agreement alias is identical to sptag", {
  expect_identical(morie_spatial_agreement, sptag)
})
