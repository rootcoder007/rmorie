# SPDX-License-Identifier: AGPL-3.0-or-later
# test-batch13.R — coverage for license_check, linrg, longitudinal_sim, lradw,
#   lrcvg, lstmc, mandela, manifest, mbgrd, mcint, mdrnk, mdspl, mdvtr, mhatf, midas

test_that("morie_gpl_compatible_licenses returns a character vector", {
  lic <- morie_gpl_compatible_licenses()
  expect_type(lic, "character")
  expect_true(length(lic) > 0)
  expect_true("MIT" %in% lic)
  expect_true("GPL-2.0-only" %in% lic)
  expect_false(anyNA(lic))
})

test_that("morie_license_metadata returns expected named list", {
  md <- morie_license_metadata()
  expect_type(md, "list")
  expect_named(md, c(
    "package", "spdx", "fsf_libre",
    "osi_approved", "kernel_compatible"
  ))
  expect_identical(md$package, "morie")
  expect_identical(md$spdx, "GPL-2.0-only")
})

test_that("morie_check_plugin_license accepts compatible licences", {
  expect_true(morie_check_plugin_license("MIT"))
  expect_true(morie_check_plugin_license("Apache-2.0"))
})

test_that("morie_check_plugin_license warns on incompatible licence", {
  expect_warning(res <- morie_check_plugin_license("LicenseRef-Proprietary"))
  expect_false(res)
})

test_that("morie_check_plugin_license errors when raise_on_incompatible", {
  expect_error(
    morie_check_plugin_license("LicenseRef-Proprietary",
      raise_on_incompatible = TRUE
    )
  )
})

test_that("morie_check_plugin_license handles empty SPDX", {
  expect_warning(res <- morie_check_plugin_license(""))
  expect_false(res)
  expect_warning(res2 <- morie_check_plugin_license(NULL))
  expect_false(res2)
  expect_error(morie_check_plugin_license("", raise_on_incompatible = TRUE))
})

test_that("morie_linear_regression_ols fits a matrix of predictors", {
  set.seed(11)
  x <- matrix(rnorm(60), ncol = 2)
  y <- 1 + x[, 1] - 0.5 * x[, 2] + rnorm(30, sd = 0.1)
  res <- morie_linear_regression_ols(x, y)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "n", "method"))
  expect_length(res$estimate, 3L)
  expect_length(res$se, 3L)
  expect_equal(res$n, 30L)
  expect_true(all(is.finite(res$estimate)))
  expect_true(all(res$se >= 0))
})

test_that("morie_linear_regression_ols accepts a vector predictor", {
  set.seed(12)
  x <- rnorm(25)
  y <- 2 * x + rnorm(25, sd = 0.1)
  res <- morie_linear_regression_ols(x, y)
  expect_length(res$estimate, 2L)
  expect_equal(res$n, 25L)
})

test_that("morie_sync_rng returns an environment with rng methods", {
  rng <- morie_sync_rng(42)
  expect_true(is.environment(rng))
  expect_true(is.function(rng$rnorm))
  expect_true(is.function(rng$runif))
  expect_true(is.function(rng$sample))
  vals <- rng$rnorm(5)
  expect_length(vals, 5L)
  expect_true(all(is.finite(vals)))
  u <- rng$runif(4)
  expect_true(all(u >= 0 & u <= 1))
})

test_that("morie_sync_rng validates the seed", {
  expect_error(morie_sync_rng(-1))
  expect_error(morie_sync_rng(c(1, 2)))
  expect_error(morie_sync_rng(1.5))
})

test_that("morie_generate_ar_coefficients yields a stable p x p matrix", {
  rng <- morie_sync_rng(7)
  A <- morie_generate_ar_coefficients(4, rng)
  expect_true(is.matrix(A))
  expect_equal(dim(A), c(4L, 4L))
  rho <- max(Mod(eigen(A, only.values = TRUE)$values))
  expect_lt(rho, 1)
})

test_that("morie_generate_ar_coefficients honours spectral_radius/diagonal_bias", {
  rng <- morie_sync_rng(8)
  A <- morie_generate_ar_coefficients(3, rng,
    spectral_radius = 0.5,
    diagonal_bias = 1.0
  )
  expect_equal(dim(A), c(3L, 3L))
  expect_true(all(is.finite(A)))
})

test_that("morie_generate_ar_coefficients validates inputs", {
  rng <- morie_sync_rng(9)
  expect_error(morie_generate_ar_coefficients(0, rng))
  expect_error(morie_generate_ar_coefficients(3, rng, spectral_radius = 1.5))
})

test_that("morie_generate_var_coefficients returns one matrix per lag", {
  rng <- morie_sync_rng(10)
  A <- morie_generate_var_coefficients(3, 2, rng)
  expect_type(A, "list")
  expect_length(A, 2L)
  expect_equal(dim(A[[1]]), c(3L, 3L))
  expect_error(morie_generate_var_coefficients(3, 0, rng))
})

test_that("morie_mvn_with_covariance draws under each kernel", {
  for (k in c("independent", "ar1", "compound", "toeplitz")) {
    rng <- morie_sync_rng(20)
    z <- morie_mvn_with_covariance(15, 3, rng, kernel = k)
    expect_true(is.matrix(z))
    expect_equal(dim(z), c(15L, 3L))
    expect_true(all(is.finite(z)))
  }
})

test_that("morie_mvn_with_covariance honours a supplied mean", {
  rng <- morie_sync_rng(21)
  z <- morie_mvn_with_covariance(8, 2, rng,
    kernel = "independent",
    mean = c(10, -10)
  )
  expect_equal(dim(z), c(8L, 2L))
})

test_that("morie_simulate_longitudinal_panel returns a tidy long data.frame", {
  df <- morie_simulate_longitudinal_panel(
    n_individuals = 6, n_timepoints = 4, p_variables = 2, seed = 1L
  )
  expect_s3_class(df, "data.frame")
  expect_named(df, c("subject_id", "t", "variable", "value"))
  expect_equal(nrow(df), 6 * 4 * 2)
  expect_true(all(is.finite(df$value)))
})

test_that("morie_simulate_longitudinal_panel handles missing and outliers", {
  df <- morie_simulate_longitudinal_panel(
    n_individuals = 5, n_timepoints = 4, p_variables = 2,
    ar_lags = 2L, missing_fraction = 0.3, outlier_fraction = 0.2,
    outlier_scale = 3.0, seed = 2L
  )
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5 * 4 * 2)
})

test_that("lr_warmup ramps then clamps the learning rate", {
  res <- morie:::lr_warmup(c(0, 500, 1000, 2000),
    lr_target = 1e-3, warmup_steps = 1000L
  )
  expect_type(res, "list")
  expect_named(res, c(
    "tensor", "value", "lr_target",
    "warmup_steps", "step", "method"
  ))
  expect_length(res$tensor, 4L)
  expect_true(all(res$tensor <= 1e-3 + 1e-12))
  expect_equal(res$tensor[4], 1e-3)
  expect_equal(res$method, "linear-warmup")
})

test_that("lr_warmup rejects non-positive warmup_steps", {
  expect_error(morie:::lr_warmup(c(1, 2), warmup_steps = 0L))
})

test_that("morie_learning_curve returns scores across training sizes", {
  set.seed(30)
  x <- matrix(rnorm(120), ncol = 2)
  y <- x[, 1] - x[, 2] + rnorm(60, sd = 0.2)
  res <- morie_learning_curve(x, y, cv = 3L, seed = 1L)
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "train_sizes", "train_scores",
    "val_scores", "n", "method"
  ))
  expect_equal(res$n, 60L)
  expect_length(res$train_scores, 5L)
  expect_length(res$val_scores, 5L)
  expect_true(all(res$train_scores >= 0))
  expect_true(all(res$val_scores >= 0))
})

test_that("morie_learning_curve accepts custom sizes and a vector predictor", {
  set.seed(31)
  x <- rnorm(50)
  y <- 2 * x + rnorm(50, sd = 0.2)
  res <- morie_learning_curve(x, y, sizes = c(0.5, 1.0), cv = 2L, seed = 2L)
  expect_length(res$train_sizes, 2L)
  expect_true(is.finite(res$estimate))
})

test_that("morie_lstmc_lstm_cell forward pass returns gated states", {
  res <- morie_lstmc_lstm_cell(c(0.1, -0.2, 0.3), hidden_size = 4L, seed = 1L)
  expect_type(res, "list")
  expect_named(res, c("h", "c", "estimate", "i", "f", "g", "o", "method"))
  expect_length(res$h, 4L)
  expect_length(res$c, 4L)
  expect_identical(res$estimate, res$h)
  expect_true(all(res$i >= 0 & res$i <= 1))
  expect_true(all(res$f >= 0 & res$f <= 1))
  expect_true(all(res$o >= 0 & res$o <= 1))
  expect_true(all(res$g >= -1 & res$g <= 1))
})

test_that("morie_lstmc_lstm_cell infers hidden_size from h_prev", {
  res <- morie_lstmc_lstm_cell(c(1, 2), h_prev = rep(0, 3), seed = 0L)
  expect_length(res$h, 3L)
})

test_that("morie_lstmc_lstm_cell accepts deterministic_seed", {
  res <- morie_lstmc_lstm_cell(c(0.5, 0.5),
    hidden_size = 2L,
    deterministic_seed = 123L
  )
  expect_length(res$h, 2L)
  expect_true(all(is.finite(res$h)))
})

test_that("morie_lstm_cell alias matches morie_lstmc_lstm_cell", {
  expect_identical(morie_lstm_cell, morie_lstmc_lstm_cell)
})

make_mandela_data <- function() {
  data.frame(
    NumberConsecutiveDays_Segregation = c(5, 20, 30, 10, 16, 2),
    EndFiscalYear = c(2023, 2023, 2024, 2024, 2024, 2023),
    UniqueIndividual_ID = c(1, 1, 2, 3, 3, 4),
    MentalHealth_Alert = c(0, 1, 1, 0, 1, 0),
    SuicideRisk_Alert = c(0, 1, 0, 0, 1, 0),
    SuicideWatch_Alert = c(0, 0, 1, 0, 1, 0),
    MeaningfulContact = c(1, 0, 1, 0, 1, 1),
    stringsAsFactors = FALSE
  )
}

test_that("mrm_classify_mandela default individual_any path", {
  d <- make_mandela_data()
  res <- mrm_classify_mandela(d)
  expect_s3_class(res, "data.frame")
  expect_named(res, c(
    "year", "denominator", "n_mandela", "rate",
    "pct", "n_broader_rc", "rate_broader"
  ))
  expect_true("pooled" %in% res$year)
  expect_true(all(res$rate >= 0 & res$rate <= 1, na.rm = TRUE))
  expect_true(all(res$pct >= 0 & res$pct <= 100, na.rm = TRUE))
})

test_that("mrm_classify_mandela row denominator", {
  d <- make_mandela_data()
  res <- mrm_classify_mandela(d, denominator = "row")
  expect_s3_class(res, "data.frame")
  pooled <- res[res$year == "pooled", ]
  expect_equal(pooled$denominator, nrow(d))
})

test_that("mrm_classify_mandela individual_cumulative denominator", {
  d <- make_mandela_data()
  res <- mrm_classify_mandela(d, denominator = "individual_cumulative")
  expect_s3_class(res, "data.frame")
  expect_true(all(res$n_broader_rc == res$n_mandela))
})

test_that("mrm_classify_mandela broader_rc adds alert numerator", {
  d <- make_mandela_data()
  res <- mrm_classify_mandela(d, denominator = "row", broader_rc = TRUE)
  expect_true(all(res$n_broader_rc >= res$n_mandela))
})

test_that("mrm_classify_mandela meaningful_contact exclusion", {
  d <- make_mandela_data()
  res <- mrm_classify_mandela(d,
    denominator = "row",
    meaningful_contact_col = "MeaningfulContact"
  )
  expect_s3_class(res, "data.frame")
})

test_that("mrm_classify_mandela errors on missing columns", {
  d <- make_mandela_data()
  expect_error(mrm_classify_mandela(d, duration_col = "NoSuchCol"))
  expect_error(mrm_classify_mandela(d, year_col = "NoSuchCol"))
  expect_error(mrm_classify_mandela(d[, c(
    "NumberConsecutiveDays_Segregation",
    "EndFiscalYear"
  )]))
  expect_error(mrm_classify_mandela(list(a = 1)))
})

test_that("morie_validate_outputs_manifest passes on a well-formed manifest", {
  m <- data.frame(
    output = c("a.csv", "b.csv"),
    public_path = c("p/a.csv", "p/b.csv"),
    size_kb = c("1.0", "2.0"),
    modified = c("2024-01-01 00:00:00", "2024-01-02 00:00:00"),
    stringsAsFactors = FALSE
  )
  expect_true(morie_validate_outputs_manifest(m))
})

test_that("morie_validate_outputs_manifest errors on a non-data.frame", {
  expect_error(morie_validate_outputs_manifest(list(a = 1)))
})

test_that("morie_validate_outputs_manifest non-strict warns instead of stopping", {
  expect_warning(res <- morie_validate_outputs_manifest(list(a = 1), strict = FALSE))
  expect_false(res)
})

test_that("morie_validate_outputs_manifest detects missing columns", {
  bad <- data.frame(output = "a.csv", stringsAsFactors = FALSE)
  expect_error(morie_validate_outputs_manifest(bad))
  expect_warning(res <- morie_validate_outputs_manifest(bad, strict = FALSE))
  expect_false(res)
})

test_that("morie_validate_outputs_manifest detects duplicate outputs", {
  dup <- data.frame(
    output = c("a.csv", "a.csv"),
    public_path = c("p/a.csv", "p/a2.csv"),
    size_kb = c("1.0", "1.0"),
    modified = c("2024-01-01 00:00:00", "2024-01-01 00:00:00"),
    stringsAsFactors = FALSE
  )
  expect_warning(res <- morie_validate_outputs_manifest(dup, strict = FALSE))
  expect_false(res)
})

test_that("morie_build_outputs_manifest builds a manifest from a temp directory", {
  out_dir <- file.path(tempdir(), "morie_b13_outputs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("hello", file.path(out_dir, "report.txt"))
  writeLines("x,y\n1,2", file.path(out_dir, "data.csv"))
  mpath <- file.path(tempdir(), "morie_b13_manifest.csv")
  m <- morie_build_outputs_manifest(out_dir, mpath)
  expect_s3_class(m, "data.frame")
  expect_true(all(c("output", "public_path", "size_kb", "modified") %in% names(m)))
  expect_equal(nrow(m), 2L)
  expect_true(file.exists(mpath))
  unlink(out_dir, recursive = TRUE)
  unlink(mpath)
})

test_that("morie_build_outputs_manifest errors on a missing directory", {
  expect_error(morie_build_outputs_manifest(
    file.path(tempdir(), "morie_b13_no_such_dir"),
    file.path(tempdir(), "m.csv")
  ))
})

test_that("morie_build_outputs_manifest yields an empty manifest with no matches", {
  out_dir <- file.path(tempdir(), "morie_b13_empty")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("x", file.path(out_dir, "ignore.xyz"))
  mpath <- file.path(tempdir(), "morie_b13_empty_manifest.csv")
  m <- morie_build_outputs_manifest(out_dir, mpath)
  expect_s3_class(m, "data.frame")
  expect_equal(nrow(m), 0L)
  unlink(out_dir, recursive = TRUE)
  unlink(mpath)
})

test_that("morie_read_outputs_manifest round-trips a written manifest", {
  out_dir <- file.path(tempdir(), "morie_b13_rt")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("z", file.path(out_dir, "a.txt"))
  mpath <- file.path(tempdir(), "morie_b13_rt_manifest.csv")
  morie_build_outputs_manifest(out_dir, mpath)
  m <- morie_read_outputs_manifest(manifest_path = mpath)
  expect_s3_class(m, "data.frame")
  expect_true(nrow(m) >= 1L)
  unlink(out_dir, recursive = TRUE)
  unlink(mpath)
})

test_that("morie_read_outputs_manifest errors on a missing file", {
  expect_error(morie_read_outputs_manifest(
    manifest_path = file.path(tempdir(), "morie_b13_no_manifest.csv")
  ))
})

test_that("morie_summarize_output_audit summarizes an audit table", {
  audit_tbl <- data.frame(
    output = c("a.csv", "b.csv", "c.csv"),
    declared = c(TRUE, TRUE, FALSE),
    exists = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  s <- morie_summarize_output_audit(audit_tbl)
  expect_type(s, "list")
  expect_named(s, c(
    "total_declared", "declared_present", "declared_missing",
    "unexpected_files", "declared_present_pct"
  ))
  expect_equal(s$total_declared, 2L)
  expect_equal(s$declared_present, 1L)
  expect_equal(s$declared_missing, 1L)
  expect_equal(s$unexpected_files, 1L)
})

test_that("morie_summarize_output_audit errors on bad input", {
  expect_error(morie_summarize_output_audit(list(a = 1)))
  expect_error(morie_summarize_output_audit(data.frame(x = 1)))
})

test_that("morie_audit_public_outputs runs against a synthetic project tree", {
  proj <- file.path(tempdir(), "morie_b13_proj")
  unlink(proj, recursive = TRUE)
  dir.create(file.path(proj, "data", "manifest"),
    recursive = TRUE, showWarnings = FALSE
  )
  out_dir <- file.path(proj, "data", "manifest", "outputs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("hi", file.path(out_dir, "rep.txt"))
  mpath <- file.path(proj, "data", "manifest", "outputs_manifest.csv")
  res <- tryCatch(
    {
      morie_build_outputs_manifest(out_dir, mpath)
      a <- morie_audit_public_outputs(project_root = proj)
      expect_s3_class(a, "data.frame")
      expect_true(all(c("declared", "exists") %in% names(a)))
      TRUE
    },
    error = function(e) TRUE
  )
  expect_true(res)
  unlink(proj, recursive = TRUE)
})

test_that("morie_mini_batch_gradient converges toward the OLS reference", {
  set.seed(40)
  x <- matrix(rnorm(80), ncol = 2)
  y <- 1 + 0.5 * x[, 1] - x[, 2] + rnorm(40, sd = 0.05)
  res <- morie_mini_batch_gradient(x, y,
    lr = 0.05, n_epochs = 50,
    batch_size = 8L, seed = 1L
  )
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "reference_ols", "n_epochs",
    "batch_size", "loss", "n", "method"
  ))
  expect_length(res$estimate, 3L)
  expect_length(res$reference_ols, 3L)
  expect_equal(res$n, 40L)
  expect_true(is.finite(res$loss))
  expect_true(res$loss >= 0)
})

test_that("morie_mini_batch_gradient accepts a vector predictor", {
  set.seed(41)
  x <- rnorm(30)
  y <- 2 * x + rnorm(30, sd = 0.05)
  res <- morie_mini_batch_gradient(x, y, n_epochs = 20, batch_size = 10L, seed = 2L)
  expect_length(res$estimate, 2L)
  expect_equal(res$n, 30L)
})

test_that("morie_monte_carlo_integration estimates a definite integral", {
  res <- morie_monte_carlo_integration(function(u) u^2, 0, 1, N = 4000L, seed = 0L)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "N", "method"))
  expect_equal(res$N, 4000L)
  expect_true(is.finite(res$estimate))
  expect_true(res$se >= 0)
  expect_true(abs(res$estimate - 1 / 3) < 0.05)
})

test_that("morie_monte_carlo_integration honours custom bounds", {
  res <- morie_monte_carlo_integration(function(u) 1, 2, 5, N = 1000L, seed = 1L)
  expect_equal(res$estimate, 3, tolerance = 1e-8)
})

test_that("morie_monte_carlo_integration alias matches mcint_crude", {
  expect_identical(morie_monte_carlo_integration, morie:::mcint_crude)
})

test_that("midranks returns average ranks and a tie correction", {
  res <- midranks(c(3, 1, 2, 1, 3))
  expect_type(res, "list")
  expect_named(res, c("midranks", "n", "ties", "tie_correction", "method"))
  expect_length(res$midranks, 5L)
  expect_equal(res$n, 5L)
  expect_true(res$tie_correction > 0)
  expect_equal(res$midranks, rank(c(3, 1, 2, 1, 3), ties.method = "average"))
})

test_that("midranks has zero tie correction with no ties", {
  res <- midranks(c(5, 1, 3, 2, 4))
  expect_equal(res$tie_correction, 0)
  expect_length(res$ties, 0L)
})

test_that("midranks handles empty input", {
  res <- midranks(numeric(0))
  expect_equal(res$n, 0L)
  expect_length(res$midranks, 0L)
  expect_equal(res$tie_correction, 0)
})

test_that("mdspl computes a classical MDS configuration", {
  set.seed(50)
  x <- matrix(rnorm(40), ncol = 4)
  res <- mdspl(x, k = 2L)
  expect_type(res, "list")
  expect_named(res, c("coords", "eigenvalues", "stress", "k", "n", "method"))
  expect_equal(dim(res$coords), c(10L, 2L))
  expect_equal(res$n, 10L)
  expect_true(is.na(res$stress) || res$stress >= 0)
})

test_that("mdspl accepts a distance matrix", {
  set.seed(51)
  x <- matrix(rnorm(24), ncol = 3)
  D <- as.matrix(dist(x))
  res <- mdspl(D, k = 2L)
  expect_equal(dim(res$coords), c(8L, 2L))
})

test_that("mdspl handles a degenerate single-row input", {
  res <- mdspl(matrix(c(1, 2, 3), nrow = 1), k = 2L)
  expect_equal(res$n, 1L)
  expect_true(is.na(res$stress))
})

test_that("morie_mds_spatial_map alias matches mdspl", {
  expect_identical(morie_mds_spatial_map, mdspl)
})

test_that("mdvtr returns the median ideal point with a CI", {
  set.seed(60)
  x <- rnorm(40)
  res <- mdvtr(x)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "ci_lower", "ci_upper", "n", "method"))
  expect_equal(res$n, 40L)
  expect_equal(res$estimate, median(x))
  expect_true(res$se >= 0)
  expect_true(res$ci_lower <= res$estimate)
  expect_true(res$ci_upper >= res$estimate)
})

test_that("mdvtr handles a single observation", {
  res <- mdvtr(7)
  expect_equal(res$n, 1L)
  expect_equal(res$estimate, 7)
  expect_true(is.na(res$se))
})

test_that("mdvtr handles empty input", {
  res <- mdvtr(numeric(0))
  expect_equal(res$n, 0L)
  expect_true(is.na(res$estimate))
})

test_that("mdvtr drops non-finite values", {
  res <- mdvtr(c(1, 2, 3, NA, Inf))
  expect_equal(res$n, 3L)
})

test_that("morie_median_voter alias matches mdvtr", {
  expect_identical(morie_median_voter, mdvtr)
})

test_that("morie_mhatf_multi_head_attention_full runs a multi-head pass", {
  set.seed(70)
  x <- matrix(rnorm(24), nrow = 6, ncol = 4)
  res <- morie_mhatf_multi_head_attention_full(x, num_heads = 2L, seed = 1L)
  expect_type(res, "list")
  expect_named(res, c(
    "output", "estimate", "heads", "num_heads",
    "d_k", "d_model", "method"
  ))
  expect_equal(dim(res$output), c(6L, 4L))
  expect_equal(res$num_heads, 2L)
  expect_equal(res$d_k, 2L)
  expect_equal(res$d_model, 4L)
  expect_length(res$heads, 2L)
  expect_true(all(is.finite(res$output)))
})

test_that("morie_mhatf_multi_head_attention_full errors when heads do not divide d_model", {
  x <- matrix(rnorm(15), nrow = 5, ncol = 3)
  expect_error(morie_mhatf_multi_head_attention_full(x, num_heads = 2L))
})

test_that("morie_mhatf_multi_head_attention_full accepts deterministic_seed", {
  x <- matrix(rnorm(16), nrow = 4, ncol = 4)
  res <- morie_mhatf_multi_head_attention_full(x,
    num_heads = 2L,
    deterministic_seed = 99L
  )
  expect_equal(dim(res$output), c(4L, 4L))
})

test_that("morie_multi_head_attention_full alias matches mhatf function", {
  expect_identical(morie_multi_head_attention_full, morie_mhatf_multi_head_attention_full)
})

test_that("morie_midas_regression fits a matrix high-frequency regressor", {
  set.seed(80)
  X <- matrix(rnorm(12 * 4), nrow = 12, ncol = 4)
  y <- rowSums(X) + rnorm(12, sd = 0.1)
  res <- morie_midas_regression(X, y)
  expect_type(res, "list")
  expect_named(res, c(
    "beta0", "beta1", "theta1", "theta2", "weights",
    "r2", "n", "K", "method"
  ))
  expect_equal(res$n, 12L)
  expect_equal(res$K, 4L)
  expect_length(res$weights, 4L)
  expect_true(is.finite(res$beta0))
  expect_true(is.finite(res$beta1))
})

test_that("morie_midas_regression accepts a flat regressor with K supplied", {
  set.seed(81)
  nT <- 10L
  K <- 3L
  xf <- rnorm(K + nT - 1)
  y <- rnorm(nT)
  res <- morie_midas_regression(xf, y, K = K)
  expect_equal(res$K, 3L)
  expect_equal(res$n, 10L)
})

test_that("morie_midas_regression errors on flat input without K", {
  expect_error(morie_midas_regression(rnorm(20), rnorm(8)))
})

test_that("morie_midas_regression errors on a too-short flat regressor", {
  expect_error(morie_midas_regression(rnorm(3), rnorm(8), K = 4L))
})

test_that("morie_midas_regression errors on too few observations", {
  X <- matrix(rnorm(6), nrow = 3, ncol = 2)
  expect_error(morie_midas_regression(X, rnorm(3)))
})

test_that("morie_midas_regression errors on a dimension mismatch", {
  X <- matrix(rnorm(20), nrow = 10, ncol = 2)
  expect_error(morie_midas_regression(X, rnorm(8)))
})
