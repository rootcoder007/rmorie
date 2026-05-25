# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 20 coverage tests: sptau, sptrn, ssmod, stacv, stkrg, strat,
# study_core, study_reporting, stvar, sukht, svmge, svmhg, svmkr, swigl, synthetic

test_that("sptau computes Moran's I on a simple linear weight chain", {
  x <- c(1, 2, 3, 4, 5)
  n <- 5L
  W <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    W[i, i + 1] <- 1
    W[i + 1, i] <- 1
  }
  res <- sptau(x, W)
  expect_type(res, "list")
  expect_named(res, c(
    "statistic", "p_value", "expectation", "variance",
    "z_score", "n", "method"
  ))
  expect_equal(res$n, n)
  expect_true(is.finite(res$statistic))
  expect_true(is.character(res$method))
  expect_equal(res$expectation, -1 / (n - 1))
})

test_that("sptau errors on non-conformable weight matrix", {
  expect_error(sptau(c(1, 2, 3), matrix(0, 2, 2)), "n-by-n")
})

test_that("sptau returns NA list for n < 3", {
  W <- matrix(c(0, 1, 1, 0), 2, 2)
  res <- sptau(c(1, 2), W)
  expect_true(is.na(res$statistic))
  expect_equal(res$n, 2L)
})

test_that("sptau returns NA list when weights or variance vanish", {
  n <- 5L
  W0 <- matrix(0, n, n)
  res <- sptau(1:5, W0)
  expect_true(is.na(res$statistic))
  W <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    W[i, i + 1] <- 1
    W[i + 1, i] <- 1
  }
  res2 <- sptau(rep(2, n), W)
  expect_true(is.na(res2$statistic))
})

test_that("morie_spatial_autocorrelation is an alias of sptau", {
  expect_identical(morie_spatial_autocorrelation, sptau)
})

test_that("sptrn fits a 1-D linear trend surface", {
  res <- sptrn(c(1, 2, 3, 4, 5), matrix(0:4, ncol = 1), order = 1)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "r2", "order", "n", "method"))
  expect_length(res$estimate, 2L)
  expect_equal(res$order, 1L)
  expect_true(all(is.finite(res$se)))
  expect_true(res$r2 >= 0 && res$r2 <= 1 + 1e-8)
})

test_that("sptrn handles 2-D coordinates at various orders", {
  set.seed(1)
  n <- 30L
  coords <- cbind(runif(n), runif(n))
  y <- 1 + coords[, 1] + rnorm(n, sd = 0.1)
  for (ord in 0:3) {
    res <- sptrn(y, coords, order = ord)
    expect_equal(res$n, n)
    expect_true(is.finite(res$r2))
    expect_true(length(res$estimate) >= 1L)
  }
})

test_that("sptrn errors when order exceeds 3 or n < p", {
  set.seed(2)
  coords <- cbind(runif(20), runif(20))
  expect_error(sptrn(rnorm(20), coords, order = 4), "trend_order")
  expect_error(
    sptrn(c(1, 2), matrix(c(0, 1, 0, 1), ncol = 2), order = 2),
    "need n"
  )
})

test_that("morie_spatial_trend_surface is an alias of sptrn", {
  expect_identical(morie_spatial_trend_surface, sptrn)
})

test_that("morie_state_space_model runs the base-R Kalman path", {
  set.seed(3)
  y <- cumsum(rnorm(40))
  res <- morie_state_space_model(y)
  expect_type(res, "list")
  expect_named(res, c(
    "filtered_state", "filtered_state_variance",
    "smoothed_state", "loglik", "Q", "R", "n", "method"
  ))
  expect_equal(res$n, 40L)
  expect_length(res$filtered_state, 40L)
  expect_length(res$smoothed_state, 40L)
  expect_true(is.finite(res$loglik))
  expect_true(res$Q >= 0 && res$R >= 0)
})

test_that("morie_state_space_model errors on short series", {
  expect_error(morie_state_space_model(c(1, 2, 3)), ">=4")
})

test_that("stacv computes empirical spatiotemporal autocovariance", {
  set.seed(4)
  n <- 24L
  coords <- cbind(runif(n), runif(n))
  times <- sample(1:6, n, replace = TRUE)
  x <- rnorm(n)
  res <- stacv(x, coords, times, n_spatial_bins = 4, n_temporal_bins = 3)
  expect_type(res, "list")
  expect_named(res, c("estimate", "n", "method"))
  expect_equal(res$n, n)
  expect_named(res$estimate, c("C", "spatial_bins", "temporal_bins", "counts"))
  expect_equal(dim(res$estimate$C), c(4L, 3L))
  expect_equal(dim(res$estimate$counts), c(4L, 3L))
  expect_length(res$estimate$spatial_bins, 4L)
})

test_that("stacv honours explicit max cutoffs", {
  set.seed(5)
  n <- 20L
  coords <- cbind(runif(n), runif(n))
  times <- runif(n, 0, 10)
  res <- stacv(rnorm(n), coords, times, max_spatial = 0.5, max_temporal = 5)
  expect_true(is.matrix(res$estimate$C))
})

test_that("stacv errors on shape mismatch", {
  coords <- cbind(runif(10), runif(10))
  expect_error(stacv(rnorm(8), coords, runif(10)), "mismatch")
})

test_that("morie_spatiotemporal_autocovariance is an alias of stacv", {
  expect_identical(morie_spatiotemporal_autocovariance, stacv)
})

test_that("stkrg predicts at a single target location", {
  set.seed(6)
  n <- 16L
  coords <- cbind(runif(n), runif(n))
  times <- sample(1:4, n, replace = TRUE)
  x <- rnorm(n)
  target <- list(s0 = matrix(c(0.5, 0.5), nrow = 1), t0 = 2)
  res <- stkrg(x, coords, times, target)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "n", "method"))
  expect_length(res$estimate, 1L)
  expect_true(is.finite(res$estimate))
  expect_true(res$se >= 0)
})

test_that("stkrg predicts at multiple targets with custom variogram", {
  set.seed(7)
  n <- 18L
  coords <- cbind(runif(n), runif(n))
  times <- sample(1:5, n, replace = TRUE)
  x <- rnorm(n)
  target <- list(s0 = cbind(runif(3), runif(3)), t0 = c(1, 2, 3))
  res <- stkrg(x, coords, times, target,
    sill = 2, nugget = 0.3, range_s = 0.5, range_t = 2
  )
  expect_length(res$estimate, 3L)
  expect_length(res$se, 3L)
  expect_true(all(res$se >= 0))
})

test_that("stkrg errors on shape and dimension mismatches", {
  coords <- cbind(runif(10), runif(10))
  expect_error(
    stkrg(rnorm(8), coords, runif(10),
      target = list(s0 = matrix(c(0, 0), 1), t0 = 1)
    ),
    "mismatch"
  )
  expect_error(
    stkrg(rnorm(10), coords, runif(10),
      target = list(s0 = matrix(0, 1, 3), t0 = 1)
    ),
    "dim mismatch"
  )
  expect_error(
    stkrg(rnorm(10), coords, runif(10),
      target = list(s0 = cbind(c(0, 1), c(0, 1)), t0 = 1)
    ),
    "align"
  )
})

test_that("morie_spatiotemporal_kriging is an alias of stkrg", {
  expect_identical(morie_spatiotemporal_kriging, stkrg)
})

test_that("strat computes stratified mean with proportional weights", {
  df <- data.frame(
    y = c(1, 2, 3, 10, 11, 12),
    stratum = c("a", "a", "a", "b", "b", "b")
  )
  res <- strat(df, "y", "stratum")
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "se", "ci_lower", "ci_upper", "weights",
    "strata_means", "n_strata", "method"
  ))
  expect_equal(res$estimate, 6.5, tolerance = 1e-9)
  expect_equal(res$n_strata, 2L)
  expect_true(res$ci_lower <= res$estimate && res$estimate <= res$ci_upper)
})

test_that("strat accepts explicit population sizes", {
  df <- data.frame(
    y = c(1, 2, 3, 10, 11, 12),
    stratum = c("a", "a", "a", "b", "b", "b")
  )
  res <- strat(df, "y", "stratum", pop_sizes = c(a = 300, b = 100))
  expect_true(is.finite(res$estimate))
  expect_equal(res$estimate, 0.75 * 2 + 0.25 * 11, tolerance = 1e-9)
})

test_that("morie_stratified_sampling is an exported alias of strat", {
  expect_identical(morie_stratified_sampling, strat)
})

test_that("study_core .safe_divide handles zero and NA denominators", {
  expect_true(is.na(morie:::.safe_divide(1, 0)))
  expect_true(is.na(morie:::.safe_divide(1, NA)))
  expect_equal(morie:::.safe_divide(6, 3), 2)
})

test_that("study_core .wald_ci returns a symmetric interval", {
  ci <- morie:::.wald_ci(0.5, 0.1)
  expect_length(ci, 2L)
  expect_true(ci[1] < ci[2])
  expect_equal(mean(ci), 0.5, tolerance = 1e-9)
})

test_that("study_core .binary_ci returns proportion, se and clipped CI", {
  res <- morie:::.binary_ci(5, 20)
  expect_named(res, c("p", "se", "ci"))
  expect_equal(res$p, 0.25)
  expect_true(all(res$ci >= 0 & res$ci <= 1))
})

test_that("study_core .weighted_binary_estimate handles empty and valid input", {
  empty <- morie:::.weighted_binary_estimate(numeric(0), numeric(0))
  expect_equal(empty$n, 0L)
  expect_true(is.na(empty$p))
  set.seed(8)
  x <- rbinom(50, 1, 0.4)
  w <- runif(50, 0.5, 2)
  res <- morie:::.weighted_binary_estimate(x, w)
  expect_true(res$p >= 0 && res$p <= 1)
  expect_equal(res$n, 50L)
  expect_true(is.finite(res$n_eff))
})

test_that("study_core .clip_exp bounds extreme exponents", {
  expect_true(is.finite(morie:::.clip_exp(5000)))
  expect_true(is.finite(morie:::.clip_exp(-5000)))
  expect_equal(morie:::.clip_exp(0), 1)
})

test_that("study_core .na_omit_cols drops incomplete rows", {
  df <- data.frame(a = c(1, NA, 3), b = c(4, 5, 6))
  out <- morie:::.na_omit_cols(df, c("a", "b"))
  expect_equal(nrow(out), 2L)
})

test_that("study_reporting .binary_power_required_n behaves on effects", {
  n <- morie:::.binary_power_required_n(0.3, 0.5)
  expect_true(is.finite(n) && n > 0)
  expect_true(is.na(morie:::.binary_power_required_n(0.4, 0.4)))
})

test_that("study_reporting .continuous_power_required_n behaves on effects", {
  n <- morie:::.continuous_power_required_n(1, 2, 1.5)
  expect_true(is.finite(n) && n > 0)
  expect_true(is.na(morie:::.continuous_power_required_n(1, 1, 1)))
})

test_that("study_reporting .block_schedule returns empty frame on no strata", {
  out <- morie:::.block_schedule("endpoint", NA_real_, character(0))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("study_reporting .block_schedule builds a randomization schedule", {
  out <- morie:::.block_schedule("hd", 40, c("Female", "Male"))
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true(all(out$block_size == 4L))
  expect_true(all(out$assignment %in% c("Control", "Treatment")))
})

test_that("stvar computes empirical spatiotemporal semivariogram", {
  set.seed(9)
  n <- 24L
  coords <- cbind(runif(n), runif(n))
  times <- sample(1:6, n, replace = TRUE)
  res <- stvar(rnorm(n), coords, times, n_spatial_bins = 4, n_temporal_bins = 3)
  expect_type(res, "list")
  expect_named(res, c("estimate", "n", "method"))
  expect_equal(res$n, n)
  expect_named(res$estimate, c(
    "gamma", "spatial_bins", "temporal_bins",
    "counts"
  ))
  expect_equal(dim(res$estimate$gamma), c(4L, 3L))
  expect_true(all(res$estimate$gamma[is.finite(res$estimate$gamma)] >= 0))
})

test_that("stvar honours explicit cutoffs and errors on shape mismatch", {
  set.seed(10)
  n <- 20L
  coords <- cbind(runif(n), runif(n))
  times <- runif(n, 0, 8)
  res <- stvar(rnorm(n), coords, times, max_spatial = 0.4, max_temporal = 4)
  expect_true(is.matrix(res$estimate$gamma))
  expect_error(stvar(rnorm(8), coords, times), "mismatch")
})

test_that("morie_spatiotemporal_variogram is an alias of stvar", {
  expect_identical(morie_spatiotemporal_variogram, stvar)
})

test_that("morie_sukhatme_test compares two-sample scales", {
  set.seed(11)
  x <- rnorm(20, sd = 1)
  y <- rnorm(25, sd = 3)
  res <- morie_sukhatme_test(x, y)
  expect_type(res, "list")
  expect_named(res, c("statistic", "p_value", "U", "n", "m", "method"))
  expect_equal(res$n, 45L)
  expect_equal(res$m, 20L)
  expect_true(is.finite(res$statistic))
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})

test_that("morie_sukhatme_test returns NA list for short samples", {
  res <- morie_sukhatme_test(c(1), c(2, 3, 4))
  expect_true(is.na(res$statistic))
  expect_true(is.na(res$U))
  expect_equal(res$n, 4L)
})

test_that("morie_svm_genomic runs (e1071 path or kernel-ridge fallback)", {
  set.seed(12)
  M <- matrix(rnorm(100), 25, 4)
  y <- sin(M[, 1]) + 0.2 * rnorm(25)
  res <- morie_svm_genomic(rep(0, 25), y, M)
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "y_hat", "alpha", "support_indices",
    "intercept", "se", "n", "method"
  ))
  expect_equal(res$n, 25L)
  expect_length(res$y_hat, 25L)
  expect_true(is.finite(res$estimate))
  expect_true(res$se >= 0)
})

test_that("morie_svm_genomic accepts NULL fixed effects and numeric gamma", {
  set.seed(13)
  M <- matrix(rnorm(80), 20, 4)
  y <- M[, 1] + 0.1 * rnorm(20)
  res <- morie_svm_genomic(NULL, y, M, C = 2, epsilon = 0.05, gamma = 0.5)
  expect_equal(res$n, 20L)
  expect_length(res$y_hat, 20L)
  expect_true(is.character(res$method))
})

test_that("morie_svm_hinge_primal fits a linear SVM when e1071 is available", {
  set.seed(14)
  x <- rbind(matrix(rnorm(40, 1), 20, 2), matrix(rnorm(40, -1), 20, 2))
  y <- rep(c(1L, 0L), each = 20)
  res <- morie_svm_hinge_primal(x, y, C = 1)
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "intercept", "weights", "train_accuracy",
    "C", "classes", "n", "method"
  ))
  expect_equal(res$n, 40L)
  expect_length(res$classes, 2L)
  expect_true(res$train_accuracy >= 0 && res$train_accuracy <= 1)
})

test_that("morie_svm_hinge_primal errors on non-binary y", {
  x <- matrix(rnorm(30), 15, 2)
  y <- rep(c(1L, 2L, 3L), each = 5)
  expect_error(morie_svm_hinge_primal(x, y), "binary")
})

test_that("morie_svm_hinge_primal coerces a vector predictor to a 1-column matrix", {
  set.seed(15)
  x <- c(rnorm(15, 2), rnorm(15, -2))
  y <- rep(c(1L, 0L), each = 15)
  res <- morie_svm_hinge_primal(x, y)
  expect_equal(res$n, 30L)
})

test_that("morie_svm_kernel_trick fits each supported kernel", {
  set.seed(16)
  x <- rbind(matrix(rnorm(60, 1), 30, 2), matrix(rnorm(60, -1), 30, 2))
  y <- rep(c(1L, 0L), each = 30)
  for (k in c("rbf", "poly", "sigmoid", "linear")) {
    res <- morie_svm_kernel_trick(x, y, kernel = k)
    expect_type(res, "list")
    expect_named(res, c(
      "estimate", "train_accuracy", "n_support", "kernel",
      "C", "gamma", "degree", "n", "method"
    ))
    expect_equal(res$kernel, k)
    expect_equal(res$n, 60L)
    expect_true(res$train_accuracy >= 0 && res$train_accuracy <= 1)
  }
})

test_that("morie_svm_kernel_trick honours gamma 'auto' and numeric gamma", {
  set.seed(17)
  x <- rbind(matrix(rnorm(40, 1), 20, 2), matrix(rnorm(40, -1), 20, 2))
  y <- rep(c(1L, 0L), each = 20)
  res_auto <- morie_svm_kernel_trick(x, y, gamma = "auto")
  expect_equal(res_auto$gamma, "auto")
  res_num <- morie_svm_kernel_trick(x, y, gamma = 0.25, degree = 2L)
  expect_equal(res_num$degree, 2L)
})

test_that("swiglu_activation runs with default identity projections", {
  set.seed(18)
  x <- matrix(rnorm(12), 4, 3)
  res <- morie:::swiglu_activation(x)
  expect_type(res, "list")
  expect_named(res, c("tensor", "gate", "up", "method"))
  expect_equal(dim(res$tensor), c(4L, 3L))
  expect_true(all(is.finite(res$tensor)))
})

test_that("swiglu_activation accepts explicit weights and biases", {
  set.seed(19)
  x <- matrix(rnorm(8), 2, 4)
  W <- matrix(rnorm(12), 4, 3)
  V <- matrix(rnorm(12), 4, 3)
  res <- morie:::swiglu_activation(x,
    W = W, V = V, b = rep(0.1, 3),
    c = rep(-0.1, 3)
  )
  expect_equal(dim(res$tensor), c(2L, 3L))
})

test_that("swiglu_activation errors when only one of W/V is supplied", {
  x <- matrix(rnorm(8), 2, 4)
  W <- diag(4)
  expect_error(morie:::swiglu_activation(x, W = W), "both W and V")
})

test_that("morie_default_synthetic_name_map returns the canonical key set", {
  m_generic <- morie_default_synthetic_name_map("generic")
  expect_type(m_generic, "character")
  expect_true(all(morie:::synthetic_required_keys() %in% names(m_generic)))
  m_legacy <- morie_default_synthetic_name_map("morie_legacy")
  expect_true(all(morie:::synthetic_required_keys() %in% names(m_legacy)))
  expect_equal(unname(m_legacy[["id"]]), "seqid")
})

test_that("morie_generate_synthetic_data builds a reproducible synthetic frame", {
  d1 <- morie_generate_synthetic_data(n = 150L, seed = 1L)
  expect_s3_class(d1, "data.frame")
  expect_equal(nrow(d1), 150L)
  expect_true(isTRUE(attr(d1, "synthetic")))
  d2 <- morie_generate_synthetic_data(n = 150L, seed = 1L)
  expect_identical(d1, d2)
})

test_that("morie_generate_synthetic_data respects the legacy naming profile", {
  d <- morie_generate_synthetic_data(n = 120L, seed = 2L, profile = "morie_legacy")
  expect_true("seqid" %in% names(d))
  expect_equal(attr(d, "synthetic_profile"), "morie_legacy")
})

test_that("morie_generate_synthetic_data validates n and special_code_rate", {
  expect_error(morie_generate_synthetic_data(n = 10L), "integer >= 100")
  expect_error(
    morie_generate_synthetic_data(n = 150L, special_code_rate = 0.5),
    "0, 0.2"
  )
})

test_that("morie_generate_synthetic_data accepts a custom name map", {
  base <- morie_default_synthetic_name_map("generic")
  custom <- base
  custom[["id"]] <- "record_id"
  d <- morie_generate_synthetic_data(n = 120L, seed = 3L, name_map = custom)
  expect_true("record_id" %in% names(d))
})

test_that("resolve_synthetic_name_map rejects malformed maps", {
  bad_missing <- c(id = "id")
  expect_error(
    morie:::resolve_synthetic_name_map(bad_missing, "generic"),
    "missing required keys"
  )
  full <- morie_default_synthetic_name_map("generic")
  dup <- full
  dup[["weight"]] <- dup[["id"]]
  expect_error(morie:::resolve_synthetic_name_map(dup, "generic"), "unique")
  expect_error(
    morie:::resolve_synthetic_name_map(123, "generic"),
    "named character vector"
  )
})

test_that("synthetic helpers inv_logit and inject_special_codes behave", {
  expect_equal(morie:::inv_logit(0), 0.5)
  expect_true(all(morie:::inv_logit(c(-10, 0, 10)) >= 0))
  set.seed(4)
  x <- rep(1L, 200)
  out <- morie:::inject_special_codes(x, rate = 0)
  expect_identical(out, x)
  out2 <- morie:::inject_special_codes(rep(1L, 500), rate = 0.1)
  expect_length(out2, 500L)
})

test_that("morie_write_synthetic_data writes a CSV and guards existing files", {
  tmp <- tempfile(fileext = ".csv")
  p <- morie_write_synthetic_data(tmp, n = 110L, seed = 5L)
  expect_true(file.exists(p))
  back <- utils::read.csv(p)
  expect_equal(nrow(back), 110L)
  expect_error(
    morie_write_synthetic_data(tmp, n = 110L, seed = 5L),
    "already exists"
  )
  p2 <- morie_write_synthetic_data(tmp, n = 110L, seed = 5L, overwrite = TRUE)
  expect_true(file.exists(p2))
  unlink(tmp)
})

test_that("morie_write_synthetic_data errors on an empty path", {
  expect_error(morie_write_synthetic_data(""), "non-empty")
})

test_that("study_core data-file module paths are not exercised offline", {
  if (FALSE) {
    morie:::.run_data_wrangling_module_internal(data.frame(), cpads_csv = "x")
  }
  expect_true(TRUE)
})
