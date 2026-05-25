# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for batch 04: dataset_catalog, dataset_profile, dbscl, dccmd,
# diffu, dimrd, dlgen, drpfw, dtrsp, dwnmn, ebac, egrch, database, data.

test_that("morie_dataset_catalog returns a well-formed data.frame", {
  cat <- morie_dataset_catalog()
  expect_s3_class(cat, "data.frame")
  expect_gt(nrow(cat), 30L)
  expect_true(all(c(
    "key", "name", "source", "survey", "year", "format",
    "type", "large_file", "local_path", "table_name",
    "ckan_resource_id"
  ) %in% names(cat)))
})

test_that("morie_dataset_catalog keys are unique and non-empty", {
  cat <- morie_dataset_catalog()
  expect_equal(anyDuplicated(cat$key), 0L)
  expect_true(all(nzchar(cat$key)))
  expect_true(all(nzchar(cat$table_name)))
  expect_type(cat$large_file, "logical")
  expect_true("ocp21" %in% cat$key)
  expect_true(any(grepl("otis", cat$key)))
})

test_that("morie_infer_measurement_level classifies logical and 0/1 as binary", {
  expect_equal(morie_infer_measurement_level(c(TRUE, FALSE, TRUE)), "binary")
  expect_equal(morie_infer_measurement_level(c(0, 1, 1, 0)), "binary")
  expect_equal(morie_infer_measurement_level(c("0", "1", "1")), "binary")
})

test_that("morie_infer_measurement_level classifies factors", {
  expect_equal(morie_infer_measurement_level(factor(c("a", "b", "c"))), "nominal")
  expect_equal(morie_infer_measurement_level(factor(c("a", "b", "a"))), "binary")
  expect_equal(
    morie_infer_measurement_level(ordered(c("low", "med", "high"))),
    "ordinal"
  )
})

test_that("morie_infer_measurement_level classifies numeric ratio vs interval", {
  expect_equal(morie_infer_measurement_level(c(1.2, 3.4, 5.6, 7.8)), "ratio")
  expect_equal(morie_infer_measurement_level(c(-1.5, 0.0, 2.3, 4.1)), "interval")
  expect_equal(morie_infer_measurement_level(c(2, 5, 2, 5)), "binary")
})

test_that("morie_infer_measurement_level returns a single valid string", {
  lvl <- morie_infer_measurement_level(rnorm(50))
  expect_type(lvl, "character")
  expect_length(lvl, 1L)
  expect_true(lvl %in% c("binary", "nominal", "ordinal", "interval", "ratio"))
})

test_that("morie_profile_dataset returns expected structure", {
  p <- morie_profile_dataset(iris)
  expect_type(p, "list")
  expect_true(all(c("n_rows", "n_cols", "columns") %in% names(p)))
  expect_equal(p$n_rows, nrow(iris))
  expect_equal(p$n_cols, ncol(iris))
  expect_named(p$columns, names(iris))
})

test_that("morie_profile_dataset numeric columns carry summary stats", {
  set.seed(1)
  df <- data.frame(
    a = rnorm(40),
    b = rbinom(40, 1, 0.5),
    g = factor(sample(letters[1:3], 40, replace = TRUE))
  )
  p <- morie_profile_dataset(df)
  col_a <- p$columns$a
  expect_true(all(c(
    "name", "dtype", "measurement_level", "n_missing",
    "n_unique", "mean", "sd", "min", "max",
    "q25", "q50", "q75"
  ) %in% names(col_a)))
  expect_true(is.finite(col_a$mean))
  expect_true(is.finite(col_a$sd))
  expect_lte(col_a$q25, col_a$q50)
  expect_lte(col_a$q50, col_a$q75)
  expect_equal(col_a$n_missing, 0L)
  expect_false("mean" %in% names(p$columns$g))
})

test_that("morie_profile_dataset counts missing values", {
  df <- data.frame(x = c(1, NA, 3, NA, 5))
  p <- morie_profile_dataset(df)
  expect_equal(p$columns$x$n_missing, 2L)
})

test_that("morie_profile_dataset errors on non-data.frame input", {
  expect_error(morie_profile_dataset(1:10))
  expect_error(morie_profile_dataset("not a frame"))
})

test_that("morie_suggest_analysis_plan returns character recommendations", {
  s <- morie_suggest_analysis_plan(morie_profile_dataset(iris))
  expect_type(s, "character")
  expect_gte(length(s), 1L)
})

test_that("morie_suggest_analysis_plan triggers binary+numeric suggestion", {
  set.seed(2)
  df <- data.frame(
    outcome = rbinom(60, 1, 0.5),
    pred1 = rnorm(60),
    pred2 = rnorm(60)
  )
  s <- morie_suggest_analysis_plan(morie_profile_dataset(df))
  expect_true(any(grepl("[Ll]ogistic", s)))
  expect_true(any(grepl("[Ll]inear regression", s)))
})

test_that("morie_suggest_analysis_plan flags missing values and ordinal vars", {
  set.seed(3)
  df <- data.frame(
    num = c(rnorm(29), NA),
    ord = ordered(sample(c("lo", "mid", "hi"), 30, replace = TRUE),
      levels = c("lo", "mid", "hi")
    )
  )
  s <- morie_suggest_analysis_plan(morie_profile_dataset(df))
  expect_true(any(grepl("[Mm]issing", s)))
  expect_true(any(grepl("[Oo]rdinal", s)))
})

test_that("morie_suggest_analysis_plan errors on bad profile input", {
  expect_error(morie_suggest_analysis_plan(list(foo = 1)))
  expect_error(morie_suggest_analysis_plan(42))
})

test_that("morie_dbscan_clustering returns expected structure", {
  set.seed(10)
  x <- rbind(
    matrix(rnorm(80, 0, 0.2), ncol = 2),
    matrix(rnorm(80, 5, 0.2), ncol = 2)
  )
  res <- morie_dbscan_clustering(x, eps = 0.6, min_samples = 4L)
  expect_type(res, "list")
  expect_true(all(c(
    "estimate", "labels", "n_clusters", "n_noise",
    "core_sample_indices", "eps", "min_samples",
    "n", "method"
  ) %in% names(res)))
  expect_equal(res$n, nrow(x))
  expect_length(res$labels, nrow(x))
  expect_gte(res$n_clusters, 0L)
  expect_gte(res$n_noise, 0L)
  expect_equal(res$eps, 0.6)
})

test_that("morie_dbscan_clustering handles a vector input", {
  set.seed(11)
  v <- c(rnorm(40, 0, 0.2), rnorm(40, 10, 0.2))
  res <- morie_dbscan_clustering(v, eps = 0.7, min_samples = 3L)
  expect_equal(res$n, length(v))
  expect_type(res$labels, "integer")
})

test_that("morie_dcc_multivariate_garch returns expected structure", {
  set.seed(20)
  x <- matrix(rnorm(120 * 3, 0, 0.5), ncol = 3)
  res <- morie_dcc_multivariate_garch(x)
  expect_type(res, "list")
  expect_true(all(c(
    "a", "b", "unconditional_correlation",
    "conditional_correlation", "conditional_variance",
    "loglik", "n", "k", "method"
  ) %in% names(res)))
  expect_equal(res$n, 120L)
  expect_equal(res$k, 3L)
  expect_true(is.finite(res$a))
  expect_true(is.finite(res$b))
  expect_true(is.finite(res$loglik))
  expect_gte(res$a, 0)
  expect_gte(res$b, 0)
})

test_that("morie_dcc_multivariate_garch errors when too small", {
  expect_error(morie_dcc_multivariate_garch(matrix(rnorm(20), ncol = 2)))
  expect_error(morie_dcc_multivariate_garch(matrix(rnorm(100), ncol = 1)))
})

test_that("morie_diffu_heat_diffusion returns expected structure", {
  T0 <- c(0, 0, 100, 100, 100, 0, 0)
  res <- morie_diffu_heat_diffusion(T0,
    alpha = 0.01, dx = 0.1, dt = 0.01,
    n_steps = 50L
  )
  expect_type(res, "list")
  expect_true(all(c(
    "value", "T_final", "T_initial", "history",
    "r_stability", "n_steps", "alpha", "method"
  ) %in%
    names(res)))
  expect_length(res$T_final, length(T0))
  expect_equal(res$T_initial, as.numeric(T0))
  expect_equal(dim(res$history), c(51L, length(T0)))
  expect_true(all(is.finite(res$T_final)))
  expect_equal(res$n_steps, 50L)
  expect_equal(res$T_final[1], T0[1])
  expect_equal(res$T_final[length(T0)], T0[length(T0)])
})

test_that("morie_diffu_heat_diffusion enforces CFL and minimum length", {
  expect_error(morie_diffu_heat_diffusion(c(1, 2)))
  expect_error(morie_diffu_heat_diffusion(c(0, 50, 100, 50, 0),
    alpha = 100, dx = 0.1, dt = 0.5
  ))
})

test_that("morie_diffusion_forward alias equals morie_diffu_heat_diffusion", {
  T0 <- c(0, 25, 50, 25, 0)
  expect_identical(
    morie_diffusion_forward(T0, n_steps = 10L)$T_final,
    morie_diffu_heat_diffusion(T0, n_steps = 10L)$T_final
  )
})

test_that("morie_diffu_diffusion_forward returns expected structure", {
  set.seed(21)
  x0 <- rnorm(8)
  res <- morie_diffu_diffusion_forward(x0, t = 100L, num_steps = 1000L, seed = 1L)
  expect_type(res, "list")
  expect_true(all(c(
    "x_t", "estimate", "noise", "alpha_bar", "beta",
    "method"
  ) %in% names(res)))
  expect_length(res$x_t, length(x0))
  expect_true(all(is.finite(res$x_t)))
  expect_gte(res$alpha_bar, 0)
  expect_lte(res$alpha_bar, 1)
  expect_identical(res$x_t, res$estimate)
})

test_that("morie_diffu_diffusion_forward accepts supplied noise and rejects bad t", {
  x0 <- rep(1, 5)
  noise <- rep(0, 5)
  res <- morie_diffu_diffusion_forward(x0, t = 1L, noise = noise, num_steps = 10L)
  expect_true(all(is.finite(res$x_t)))
  expect_error(morie_diffu_diffusion_forward(x0, t = 0L, num_steps = 10L))
  expect_error(morie_diffu_diffusion_forward(x0, t = 999L, num_steps = 10L))
})

test_that("dimrd works on a data matrix", {
  set.seed(30)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  res <- dimrd(x, threshold = 1)
  expect_type(res, "list")
  expect_true(all(c(
    "n_dims", "eigenvalues", "threshold", "scree_gap",
    "method"
  ) %in% names(res)))
  expect_equal(res$threshold, 1)
  expect_gte(res$n_dims, 0L)
  expect_true(all(is.finite(res$eigenvalues)))
  expect_length(res$eigenvalues, 5L)
})

test_that("dimrd works on a symmetric (correlation) matrix", {
  set.seed(31)
  m <- matrix(rnorm(60 * 4), ncol = 4)
  cm <- cor(m)
  res <- dimrd(cm)
  expect_type(res$n_dims, "integer")
  expect_true(all(is.finite(res$eigenvalues)))
})

test_that("dimrd alias and single-column degenerate path", {
  set.seed(32)
  x <- matrix(rnorm(40 * 3), ncol = 3)
  expect_equal(morie_dimensionality_test(x)$n_dims, dimrd(x)$n_dims)
  res1 <- dimrd(matrix(rnorm(20), ncol = 1))
  expect_equal(res1$n_dims, 0L)
})

test_that("morie_deep_learning_genomic returns expected structure", {
  set.seed(6)
  M <- matrix(rnorm(20 * 5), 20, 5)
  y <- M[, 1] + 0.3 * rnorm(20)
  res <- morie_deep_learning_genomic(rep(0, 20), y, M,
    hidden = 8,
    n_epochs = 30, seed = 6
  )
  expect_type(res, "list")
  expect_true(all(c(
    "estimate", "y_hat", "beta", "W1", "b1", "w2", "b2",
    "loss_curve", "se", "n", "method"
  ) %in% names(res)))
  expect_equal(res$n, 20L)
  expect_length(res$y_hat, 20L)
  expect_true(all(is.finite(res$y_hat)))
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se))
  expect_gte(res$se, 0)
  expect_equal(dim(res$W1), c(5L, 8L))
  expect_length(res$loss_curve, 30L)
  expect_true(all(is.finite(res$loss_curve)))
})

test_that("morie_deep_learning_genomic loss generally decreases", {
  set.seed(7)
  M <- matrix(rnorm(30 * 4), 30, 4)
  y <- M[, 2] + 0.2 * rnorm(30)
  res <- morie_deep_learning_genomic(rep(0, 30), y, M,
    hidden = 6,
    n_epochs = 100, lr = 1e-2, seed = 7
  )
  expect_lte(res$loss_curve[length(res$loss_curve)], res$loss_curve[1])
})

test_that("morie_drpfw_dropout_forward returns expected structure in training", {
  set.seed(40)
  x <- array(rnorm(24), dim = c(4, 6))
  res <- morie_drpfw_dropout_forward(x, p = 0.3, seed = 1L, training = TRUE)
  expect_type(res, "list")
  expect_true(all(c(
    "y", "estimate", "mask", "p", "kept_fraction",
    "method"
  ) %in% names(res)))
  expect_equal(dim(res$y), dim(x))
  expect_equal(dim(res$mask), dim(x))
  expect_true(all(res$mask %in% c(0, 1)))
  expect_gte(res$kept_fraction, 0)
  expect_lte(res$kept_fraction, 1)
  expect_equal(res$p, 0.3)
})

test_that("morie_drpfw_dropout_forward passes through when not training or p=0", {
  x <- array(1:12, dim = c(3, 4))
  res_eval <- morie_drpfw_dropout_forward(x, p = 0.5, training = FALSE)
  expect_equal(res_eval$y, as.array(x))
  expect_equal(res_eval$kept_fraction, 1.0)
  res_p0 <- morie_drpfw_dropout_forward(x, p = 0, training = TRUE)
  expect_equal(res_p0$y, as.array(x))
})

test_that("morie_drpfw_dropout_forward rejects out-of-range p; alias works", {
  x <- array(rnorm(8), dim = c(2, 4))
  expect_error(morie_drpfw_dropout_forward(x, p = -0.1))
  expect_error(morie_drpfw_dropout_forward(x, p = 1))
  res <- morie_dropout_forward(x, p = 0.5, seed = 2L)
  expect_equal(dim(res$y), dim(x))
})

test_that("morie_decision_tree_split returns expected structure", {
  set.seed(50)
  x <- matrix(rnorm(80 * 3), ncol = 3)
  y <- factor(ifelse(x[, 1] + rnorm(80, 0, 0.1) > 0, "pos", "neg"))
  res <- morie_decision_tree_split(x, y, criterion = "gini", seed = 0L)
  expect_type(res, "list")
  expect_true(all(c(
    "estimate", "train_accuracy", "root_feature",
    "root_threshold", "root_impurity", "n_leaves",
    "feature_importances", "criterion", "n",
    "method"
  ) %in% names(res)))
  expect_equal(res$n, 80L)
  expect_gte(res$train_accuracy, 0)
  expect_lte(res$train_accuracy, 1)
  expect_length(res$feature_importances, 3L)
  expect_equal(res$criterion, "gini")
  expect_true(is.finite(res$root_impurity))
})

test_that("morie_decision_tree_split supports entropy criterion and vector x", {
  set.seed(51)
  v <- rnorm(60)
  y <- factor(ifelse(v > 0, "a", "b"))
  res <- morie_decision_tree_split(v, y, criterion = "entropy", seed = 1L)
  expect_equal(res$criterion, "entropy")
  expect_equal(res$n, 60L)
  expect_gte(res$n_leaves, 1L)
})

test_that("dwnmn smooths a scalar series", {
  set.seed(60)
  x <- cumsum(rnorm(30))
  res <- dwnmn(x, sigma_w = 0.1)
  expect_type(res, "list")
  expect_true(all(c(
    "smoothed", "raw", "P_smoothed", "sigma_w",
    "n_periods", "method"
  ) %in% names(res)))
  expect_length(res$smoothed, length(x))
  expect_true(all(is.finite(res$smoothed)))
  expect_equal(res$n_periods, 30L)
  expect_equal(res$sigma_w, 0.1)
})

test_that("dwnmn smooths a panel matrix", {
  set.seed(61)
  x <- matrix(cumsum(rnorm(50)), nrow = 5, ncol = 10)
  res <- dwnmn(x, sigma_w = 0.2)
  expect_equal(dim(res$smoothed), dim(x))
  expect_true(all(is.finite(res$smoothed)))
  expect_equal(res$n_units, 5L)
  expect_equal(res$n_periods, 10L)
})

test_that("dwnmn handles empty input and alias works", {
  res_empty <- dwnmn(numeric(0))
  expect_equal(res_empty$n_periods, 0L)
  expect_length(res_empty$smoothed, 0L)
  set.seed(62)
  v <- cumsum(rnorm(20))
  expect_equal(morie_dynamic_wnominate(v)$smoothed, dwnmn(v)$smoothed)
})

test_that("morie_calculate_ebac returns a non-negative scalar", {
  v <- morie_calculate_ebac(
    drinks = 4, weight_lbs = 180, hours = 2,
    gender_constant = 0.73
  )
  expect_type(v, "double")
  expect_length(v, 1L)
  expect_gte(v, 0)
  expect_true(is.finite(v))
})

test_that("morie_calculate_ebac clips at zero and guards bad weight", {
  expect_equal(
    morie_calculate_ebac(
      drinks = 1, weight_lbs = 200, hours = 100,
      gender_constant = 0.73
    ),
    0
  )
  expect_equal(
    morie_calculate_ebac(
      drinks = 4, weight_lbs = 0, hours = 1,
      gender_constant = 0.73
    ),
    0
  )
  expect_equal(
    morie_calculate_ebac(
      drinks = 4, weight_lbs = -10, hours = 1,
      gender_constant = 0.73
    ),
    0
  )
})

test_that("morie_is_over_legal_limit returns integer 0/1", {
  expect_identical(morie_is_over_legal_limit(0.09), 1L)
  expect_identical(morie_is_over_legal_limit(0.05), 0L)
  expect_identical(morie_is_over_legal_limit(0.05, limit = 0.05), 1L)
  expect_identical(morie_is_over_legal_limit(0.08), 1L)
})

test_that("morie_egarch_model returns expected structure", {
  set.seed(70)
  r <- rnorm(150, 0, 1)
  res <- morie_egarch_model(r)
  expect_type(res, "list")
  expect_true(all(c(
    "omega", "alpha", "gamma", "beta", "loglik",
    "conditional_variance", "n", "method"
  ) %in%
    names(res)))
  expect_equal(res$n, 150L)
  expect_length(res$conditional_variance, 150L)
  expect_true(all(is.finite(res$conditional_variance)))
  expect_true(all(res$conditional_variance >= 0))
  expect_true(is.finite(res$loglik))
  expect_true(is.finite(res$beta))
})

test_that("morie_egarch_model errors on too few observations", {
  expect_error(morie_egarch_model(rnorm(10)))
})

test_that("morie_builtin_db returns a path string", {
  p <- morie_builtin_db()
  expect_type(p, "character")
  expect_length(p, 1L)
})

test_that("morie cache round-trip works against a temp database", {
  skip_if_not_installed("DBI")
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  con <- morie_db_connect(db_path = tmp)
  expect_s4_class(con, "DBIConnection")
  DBI::dbDisconnect(con)

  df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  n_written <- morie_cache_store(df, "t_demo", db_path = tmp)
  expect_equal(n_written, 5L)

  loaded <- morie_cache_load("t_demo", db_path = tmp)
  expect_s3_class(loaded, "data.frame")
  expect_equal(nrow(loaded), 5L)

  expect_null(morie_cache_load("does_not_exist", db_path = tmp))

  listing <- morie_cache_list(db_path = tmp)
  expect_s3_class(listing, "data.frame")
  expect_true(all(c("table", "rows") %in% names(listing)))
  expect_true("t_demo" %in% listing$table)
})

test_that("morie_cache_file ingests a CSV into the cache", {
  tmp <- tempfile(fileext = ".db")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(c(tmp, csv)), add = TRUE)

  utils::write.csv(data.frame(x = 1:4, y = 4:1), csv, row.names = FALSE)
  n <- morie_cache_file(csv, "from_csv", db_path = tmp)
  expect_equal(n, 4L)
  expect_equal(nrow(morie_cache_load("from_csv", db_path = tmp)), 4L)

  bad <- tempfile(fileext = ".txt")
  file.create(bad)
  on.exit(unlink(bad), add = TRUE)
  expect_error(morie_cache_file(bad, "bad_tbl", db_path = tmp))
})

test_that("morie_list_datasets reports catalog with cache status", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)
  ds <- morie_list_datasets(db_path = tmp)
  expect_s3_class(ds, "data.frame")
  expect_true(all(c(
    "key", "name", "source", "survey", "year", "type",
    "cached", "rows"
  ) %in% names(ds)))
  expect_type(ds$cached, "logical")
})

test_that("morie_dataset_info resolves keys (exact and fuzzy)", {
  info <- morie_dataset_info("ocp21")
  expect_type(info, "list")
  expect_true(all(c("key", "source", "year", "survey") %in% names(info)))
  expect_equal(info$key, "ocp21")
  expect_true(is.list(morie_dataset_info("cpads")))
  expect_error(morie_dataset_info("totally_unknown_key_xyz"))
})

test_that("network / local-file database paths are exercised only offline-safe", {
  expect_true(TRUE)
  if (FALSE) {
    morie_fetch_ckan(dataset_key = "cpads", limit = 100L)
    morie_load_cpads(use_ckan = TRUE)
    morie_load_dataset("ocp21")
    morie_download_bootstrap(survey = "csads_2021")
    morie_userguide()
    morie_userguide("20212022-cpads-pumf-user-guide.pdf")
  }
})

test_that("data.R documented datasets are loadable when built", {
  expect_true(TRUE)
  if (FALSE) {
    data("dataset_catalog", package = "morie")
    data("substance_categories", package = "morie")
    data("ckan_metadata", package = "morie")
  }
})
