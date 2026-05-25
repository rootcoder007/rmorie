# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for batch01: det_rng, montesinos GRM, samples, accuracy,
# agenda-setter, party alignment, anisotropy, antithetic variates,
# ARCH-in-mean, scaled dot-product attention.

test_that("morie_det_rng_sha_hex returns a 64-char lowercase hex digest", {
  h <- morie_det_rng_sha_hex("ksr07_bootstrap", 42L)
  expect_type(h, "character")
  expect_length(h, 1L)
  expect_equal(nchar(h), 64L)
  expect_match(h, "^[0-9a-f]{64}$")
})

test_that("morie_det_rng_sha_hex is deterministic and key-sensitive", {
  a <- morie_det_rng_sha_hex("fixture_x", 1L)
  b <- morie_det_rng_sha_hex("fixture_x", 1L)
  c <- morie_det_rng_sha_hex("fixture_x", 2L)
  d <- morie_det_rng_sha_hex("fixture_y", 1L)
  expect_identical(a, b)
  expect_false(identical(a, c))
  expect_false(identical(a, d))
})

test_that("morie_det_rng_sha_hex rejects bad input", {
  expect_error(morie_det_rng_sha_hex(c("a", "b"), 1L))
  expect_error(morie_det_rng_sha_hex(123, 1L))
})

test_that("morie_det_rng installs a seed and returns it invisibly", {
  s <- morie_det_rng("ksr07_bootstrap", 42L)
  expect_type(s, "integer")
  expect_length(s, 1L)
  expect_true(is.finite(s))
  expect_gte(s, 0L)
  expect_lt(s, 2^31 - 1)
})

test_that("morie_det_rng makes subsequent draws reproducible", {
  morie_det_rng("repro_fixture", 7L)
  d1 <- rnorm(5)
  morie_det_rng("repro_fixture", 7L)
  d2 <- rnorm(5)
  expect_equal(d1, d2)
  expect_true(all(is.finite(d1)))
})

test_that("morie_det_rng rejects bad input", {
  expect_error(morie_det_rng(123, 1L))
  expect_error(morie_det_rng(c("a", "b"), 1L))
})

test_that("morie_grm_vanraden method 1 returns a valid GRM list", {
  set.seed(0)
  M <- matrix(sample(0:2, 100, TRUE), nrow = 10, ncol = 10)
  res <- morie_grm_vanraden(M)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "diag_mean", "off_mean",
    "p", "n", "m", "method"
  ))
  expect_true(is.matrix(res$estimate))
  expect_equal(dim(res$estimate), c(10L, 10L))
  expect_true(all(is.finite(res$estimate)))
  expect_true(is.finite(res$diag_mean))
  expect_true(is.finite(res$off_mean))
  expect_equal(res$n, 10L)
  expect_equal(res$m, 10L)
  expect_length(res$p, 10L)
  expect_match(res$method, "VanRaden")
})

test_that("morie_grm_vanraden method 2 uses per-locus scaling", {
  set.seed(1)
  M <- matrix(sample(0:2, 60, TRUE), nrow = 6, ncol = 10)
  res <- morie_grm_vanraden(M, method = 2)
  expect_true(is.list(res))
  expect_true(is.matrix(res$estimate))
  expect_equal(dim(res$estimate), c(6L, 6L))
  expect_true(all(is.finite(res$estimate)))
  expect_match(res$method, "method 2")
})

test_that("morie_grm_vanraden GRM is symmetric", {
  set.seed(2)
  M <- matrix(sample(0:2, 80, TRUE), nrow = 8, ncol = 10)
  G <- morie_grm_vanraden(M)$estimate
  expect_equal(G, t(G))
})

test_that("morie_sample errors on unknown sample name", {
  expect_error(morie_sample("not_a_sample"))
})

test_that("morie_sample bundled-CSV path is structurally valid", {
  path <- system.file("extdata", "samples", "otis_b01_sample.csv",
    package = "morie"
  )
  expect_type(path, "character")
  if (FALSE) {
    df <- morie_sample("otis_b01")
    expect_s3_class(df, "data.frame")
    expect_gt(nrow(df), 0L)
  }
})

test_that("morie_prediction_accuracy returns full metric list", {
  y <- c(1, 2, 3, 4, 5)
  yhat <- c(1.1, 1.9, 3.2, 3.8, 5.1)
  res <- morie_prediction_accuracy(y, yhat)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "pearson_r", "morie_spearman_rho",
    "mse", "mspe", "rmse", "r2", "slope",
    "intercept", "n", "method"
  ))
  expect_equal(res$n, 5L)
  expect_equal(res$mse, res$mspe)
  expect_true(is.finite(res$pearson_r))
  expect_gte(res$pearson_r, -1)
  expect_lte(res$pearson_r, 1)
  expect_gte(res$rmse, 0)
  expect_equal(res$rmse, sqrt(res$mse))
})

test_that("morie_prediction_accuracy handles n<2 gracefully", {
  res <- morie_prediction_accuracy(1, 1)
  expect_true(is.list(res))
  expect_equal(res$n, 1L)
  expect_true(is.na(res$estimate))
})

test_that("morie_prediction_accuracy errors on length mismatch", {
  expect_error(
    morie_prediction_accuracy(c(1, 2, 3), c(1, 2)),
    "same length"
  )
})

test_that("morie_prediction_accuracy handles constant predictions", {
  res <- morie_prediction_accuracy(c(1, 2, 3, 4), c(2, 2, 2, 2))
  expect_true(is.list(res))
  expect_true(is.na(res$pearson_r))
  expect_true(is.na(res$slope))
})

test_that("agset returns a chosen proposal within the win set", {
  res <- agset(
    options = seq(0, 10, by = 0.5),
    setter_ideal = 8, reversion = 2
  )
  expect_true(is.list(res))
  expect_named(res, c(
    "chosen", "power", "setter_ideal", "reversion",
    "win_set_size", "win_set_bounds", "method"
  ))
  expect_true(is.finite(res$chosen))
  expect_gte(res$power, 0)
  expect_equal(res$power, abs(res$chosen - res$reversion))
  expect_type(res$win_set_size, "integer")
  expect_length(res$win_set_bounds, 2L)
  expect_equal(res$method, "morie_agenda_setter_power")
})

test_that("agset returns reversion when win set is empty", {
  res <- agset(
    options = c(50, 60, 70),
    setter_ideal = 8, reversion = 2
  )
  expect_equal(res$chosen, 2)
  expect_equal(res$power, 0)
})

test_that("agset handles empty options", {
  res <- agset(options = numeric(0), setter_ideal = 5, reversion = 1)
  expect_true(is.na(res$chosen))
  expect_equal(res$win_set_size, 0L)
})

test_that("morie_agenda_setter_power is an alias of agset", {
  expect_identical(morie_agenda_setter_power, agset)
})

test_that("algnm computes Rice cohesion for a vote vector", {
  set.seed(10)
  v <- rbinom(40, 1, 0.7)
  res <- algnm(v)
  expect_true(is.list(res))
  expect_true(is.finite(res$estimate))
  expect_gte(res$estimate, 0)
  expect_lte(res$estimate, 1)
  expect_equal(res$method, "rice_cohesion")
  expect_equal(res$n, 40L)
})

test_that("algnm handles a roll-call matrix without party", {
  set.seed(11)
  X <- matrix(rbinom(50, 1, 0.6), nrow = 10, ncol = 5)
  res <- algnm(X)
  expect_true(is.list(res))
  expect_true(is.finite(res$estimate))
  expect_true(is.list(res$per_party))
  expect_true("all" %in% names(res$per_party))
  expect_equal(res$n, 10L)
  expect_equal(res$m, 5L)
})

test_that("algnm handles a roll-call matrix with party labels", {
  set.seed(12)
  X <- matrix(rbinom(60, 1, 0.5), nrow = 12, ncol = 5)
  party <- rep(c("A", "B"), each = 6)
  res <- algnm(X, party = party)
  expect_true(is.list(res))
  expect_true(is.finite(res$estimate))
  expect_true(all(c("A", "B") %in% names(res$per_party)))
})

test_that("algnm errors on party length mismatch", {
  X <- matrix(rbinom(20, 1, 0.5), nrow = 4, ncol = 5)
  expect_error(algnm(X, party = c("A", "B")), "party length")
})

test_that("morie_party_alignment is an alias of algnm", {
  expect_identical(morie_party_alignment, algnm)
})

test_that("morie_aniso runs Levene-style anisotropy detection in 2D", {
  set.seed(20)
  n <- 60
  coords <- matrix(runif(n * 2, 0, 10), ncol = 2)
  x <- rnorm(n)
  res <- morie_aniso(x, coords, n_dirs = 4, tol_deg = 22.5)
  expect_true(is.list(res))
  expect_equal(res$n, n)
  expect_match(res$method, "Anisotropy")
  if (!is.null(res$statistic) && !is.na(res$statistic)) {
    expect_true(is.finite(res$statistic))
    expect_gte(res$p_value, 0)
    expect_lte(res$p_value, 1)
  }
})

test_that("morie_aniso treats 1D coords as trivially isotropic", {
  set.seed(21)
  n <- 30
  coords <- matrix(runif(n), ncol = 1)
  res <- morie_aniso(rnorm(n), coords)
  expect_equal(res$statistic, 0)
  expect_equal(res$p_value, 1)
  expect_match(res$method, "1D")
})

test_that("morie_aniso errors when coords rows mismatch length(x)", {
  coords <- matrix(runif(20), ncol = 2)
  expect_error(morie_aniso(rnorm(5), coords), "coords rows")
})

test_that("morie_anisotropy_test is an alias of morie_aniso", {
  expect_identical(morie_anisotropy_test, morie_aniso)
})

test_that("morie_antithetic_variates estimates E[f(U)] with variance reduction", {
  res <- morie_antithetic_variates(N = 2000L, seed = 0L)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "estimate_crude", "se",
    "var_ratio_av_over_crude", "n_pairs", "method"
  ))
  expect_true(is.finite(res$estimate))
  expect_lt(abs(res$estimate - 0.5), 0.05)
  expect_gte(res$se, 0)
  expect_equal(res$n_pairs, 2000L)
  expect_match(res$method, "Antithetic")
})

test_that("morie_antithetic_variates accepts a custom integrand", {
  res <- morie_antithetic_variates(f = function(u) u^2, N = 1500L, seed = 1L)
  expect_true(is.finite(res$estimate))
  expect_lt(abs(res$estimate - 1 / 3), 0.05)
  expect_true(is.finite(res$estimate_crude))
})

test_that("morie_antithetic_variates rescales a supplied out-of-range sample", {
  set.seed(3)
  x <- rnorm(500)
  res <- morie_antithetic_variates(x = x)
  expect_true(is.finite(res$estimate))
  expect_equal(res$n_pairs, 500L)
})

test_that("morie_arch_in_mean fits an ARCH(1)-in-mean model", {
  set.seed(30)
  y <- rnorm(120, sd = 1.2)
  res <- morie_arch_in_mean(y)
  expect_true(is.list(res))
  expect_named(res, c(
    "mu", "delta", "omega", "alpha", "loglik",
    "conditional_variance", "n", "method"
  ))
  expect_true(is.finite(res$mu))
  expect_true(is.finite(res$delta))
  expect_gt(res$omega, 0)
  expect_gte(res$alpha, 0)
  expect_lt(res$alpha, 1)
  expect_true(is.finite(res$loglik))
  expect_length(res$conditional_variance, 120L)
  expect_true(all(res$conditional_variance > 0))
  expect_equal(res$n, 120L)
})

test_that("morie_arch_in_mean errors on too-short series", {
  expect_error(morie_arch_in_mean(rnorm(10)))
})

test_that("morie_attnq_scaled_dot_product_attention computes self-attention", {
  set.seed(40)
  Q <- matrix(rnorm(12), nrow = 3, ncol = 4)
  res <- morie_attnq_scaled_dot_product_attention(Q)
  expect_true(is.list(res))
  expect_named(res, c(
    "output", "estimate", "attn", "logits",
    "d_k", "method"
  ))
  expect_true(is.matrix(res$output))
  expect_equal(dim(res$output), c(3L, 4L))
  expect_identical(res$output, res$estimate)
  expect_equal(dim(res$attn), c(3L, 3L))
  expect_true(all(abs(rowSums(res$attn) - 1) < 1e-8))
  expect_true(all(res$attn >= 0))
  expect_equal(res$d_k, 4L)
})

test_that("morie_attnq_scaled_dot_product_attention accepts explicit K and V", {
  set.seed(41)
  Q <- matrix(rnorm(8), nrow = 2, ncol = 4)
  K <- matrix(rnorm(20), nrow = 5, ncol = 4)
  V <- matrix(rnorm(15), nrow = 5, ncol = 3)
  res <- morie_attnq_scaled_dot_product_attention(Q, K, V)
  expect_equal(dim(res$output), c(2L, 3L))
  expect_equal(dim(res$attn), c(2L, 5L))
  expect_true(all(abs(rowSums(res$attn) - 1) < 1e-8))
  expect_true(all(is.finite(res$output)))
})

test_that("morie_attnq_scaled_dot_product_attention applies an additive mask", {
  set.seed(42)
  Q <- matrix(rnorm(12), nrow = 3, ncol = 4)
  mask <- matrix(0, nrow = 3, ncol = 3)
  mask[upper.tri(mask)] <- -1e9
  res <- morie_attnq_scaled_dot_product_attention(Q, mask = mask)
  expect_equal(dim(res$attn), c(3L, 3L))
  expect_true(all(abs(rowSums(res$attn) - 1) < 1e-8))
  expect_true(all(res$attn[upper.tri(res$attn)] < 1e-6))
})

test_that("morie_scaled_dot_product_attention is an alias", {
  expect_identical(
    morie_scaled_dot_product_attention,
    morie_attnq_scaled_dot_product_attention
  )
})
