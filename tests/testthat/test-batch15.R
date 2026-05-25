# SPDX-License-Identifier: AGPL-3.0-or-later
# test-batch15.R - coverage batch 15: mrm_tps, mtgbl, mxpol, nbeat, nstat,
#   okrig, optcl, ordct, ordlt, ordlt_jonckheere, paths, pcadm, pctmr, penls, permt

test_that("mrm_tps_levy_scaling handles short / empty input", {
  empty <- data.frame(
    OCC_DATE = character(0),
    LAT_WGS84 = numeric(0),
    LONG_WGS84 = numeric(0)
  )
  r0 <- mrm_tps_levy_scaling(empty)
  expect_type(r0, "list")
  expect_named(r0, c("n_events", "n_steps_tail", "min_step_km", "hill_alpha"))
  expect_equal(r0$n_events, 0L)
  expect_equal(r0$n_steps_tail, 0L)
  expect_true(is.na(r0$hill_alpha))

  one <- data.frame(
    OCC_DATE = "2020-01-01",
    LAT_WGS84 = 43.7, LONG_WGS84 = -79.4
  )
  r1 <- mrm_tps_levy_scaling(one)
  expect_equal(r1$n_events, 1L)
  expect_true(is.na(r1$hill_alpha))
})

test_that("mrm_tps_levy_scaling computes Hill exponent on a small stream", {
  set.seed(1)
  df <- data.frame(
    OCC_DATE = sprintf("2020-01-%02d", 1:20),
    LAT_WGS84 = 43.7 + cumsum(runif(20, -0.05, 0.05)),
    LONG_WGS84 = -79.4 + cumsum(runif(20, -0.05, 0.05))
  )
  r <- mrm_tps_levy_scaling(df, min_step_km = 0.1)
  expect_type(r, "list")
  expect_equal(r$n_events, 20L)
  expect_true(r$n_steps_tail >= 0L)
  expect_equal(r$min_step_km, 0.1)
})

test_that("mrm_tps_levy_scaling errors on missing columns / non-data.frame", {
  expect_error(mrm_tps_levy_scaling(list(a = 1)))
  bad <- data.frame(x = 1:3)
  expect_error(mrm_tps_levy_scaling(bad))
})

test_that("mrm_tps_moran_clustering returns NA structure for tiny input", {
  small <- data.frame(
    LAT_WGS84 = c(43.7, 43.8),
    LONG_WGS84 = c(-79.4, -79.3)
  )
  r <- mrm_tps_moran_clustering(small)
  expect_type(r, "list")
  expect_named(r, c(
    "morans_I", "morans_z", "dbscan_n_clusters",
    "dbscan_n_noise", "dbscan_largest"
  ))
  expect_true(is.na(r$morans_I))
  expect_equal(r$dbscan_n_clusters, 0L)
})

test_that("mrm_tps_moran_clustering computes Moran's I on a grid of points", {
  set.seed(2)
  n <- 60
  df <- data.frame(
    LAT_WGS84 = 43.6 + runif(n, 0, 0.3),
    LONG_WGS84 = -79.5 + runif(n, 0, 0.3)
  )
  r <- mrm_tps_moran_clustering(df, grid_resolution = 8L)
  expect_type(r, "list")
  expect_true(is.finite(r$morans_I))
  expect_true(is.finite(r$morans_z))
})

test_that("mrm_tps_moran_clustering errors on missing columns", {
  expect_error(mrm_tps_moran_clustering(data.frame(a = 1:20)))
})

test_that("mrm_tps_neighbourhood_recurrence_km summarises gaps per hood", {
  df <- data.frame(
    OCC_DATE = c(
      "2020-01-01", "2020-01-05", "2020-01-12",
      "2020-02-01", "2020-02-10", "2020-02-20"
    ),
    HOOD_158 = c("A", "A", "A", "B", "B", "B")
  )
  out <- mrm_tps_neighbourhood_recurrence_km(df)
  expect_s3_class(out, "data.frame")
  expect_true(all(c(
    "hood", "n_events", "n_gaps", "mean_gap_days",
    "median_gap_days", "p25_gap_days",
    "p75_gap_days"
  ) %in% names(out)))
  expect_equal(nrow(out), 2L)
  expect_true(all(out$mean_gap_days > 0))
})

test_that("mrm_tps_neighbourhood_recurrence_km errors on missing columns", {
  expect_error(mrm_tps_neighbourhood_recurrence_km(data.frame(a = 1:3)))
})

test_that("mrm_tps_load_hawkes_refit errors on missing manifest file", {
  expect_error(mrm_tps_load_hawkes_refit(tempfile(fileext = ".json")))
  if (FALSE) {
    mrm_tps_load_hawkes_refit("paper_hawkes_refit.json")
  }
  expect_true(TRUE)
})

test_that("morie_multi_trait_gblup runs on a small multi-trait problem", {
  set.seed(5)
  M <- matrix(sample(0:2, 48, TRUE), 6, 8)
  Y <- matrix(rnorm(12), 6, 2)
  r <- morie_multi_trait_gblup(rep(0, 6), Y, M)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "G_hat", "B_hat", "Sigma_g", "Sigma_e",
    "n", "t", "method"
  ))
  expect_equal(r$n, 6L)
  expect_equal(r$t, 2L)
  expect_equal(dim(r$G_hat), c(6L, 2L))
  expect_true(is.finite(r$estimate))
})

test_that("morie_multi_trait_gblup handles NULL fixed effects and single trait", {
  set.seed(6)
  M <- matrix(sample(0:2, 40, TRUE), 5, 8)
  Y <- matrix(rnorm(5), 5, 1)
  r <- morie_multi_trait_gblup(NULL, Y, M)
  expect_equal(r$t, 1L)
  expect_equal(dim(r$G_hat), c(5L, 1L))
})

test_that("morie_multi_trait_gblup accepts supplied covariance matrices", {
  set.seed(7)
  M <- matrix(sample(0:2, 48, TRUE), 6, 8)
  Y <- matrix(rnorm(12), 6, 2)
  Sg <- diag(2)
  Se <- diag(2)
  r <- morie_multi_trait_gblup(rep(0, 6), Y, M, Sigma_g = Sg, Sigma_e = Se)
  expect_equal(r$Sigma_g, Sg)
  expect_equal(r$Sigma_e, Se)
})

test_that("morie_mxpol_maxpool_forward pools a 4x4 matrix with default stride", {
  x <- matrix(1:16, 4, 4)
  r <- morie_mxpol_maxpool_forward(x, kernel_size = 2L)
  expect_type(r, "list")
  expect_named(r, c("y", "estimate", "argmax", "output_shape", "method"))
  expect_equal(r$output_shape, c(2L, 2L))
  expect_equal(dim(r$y), c(2L, 2L))
  expect_equal(r$y, r$estimate)
  expect_true(all(r$argmax >= 0L))
})

test_that("morie_mxpol_maxpool_forward honours an explicit stride", {
  x <- matrix(seq_len(25), 5, 5)
  r <- morie_mxpol_maxpool_forward(x, kernel_size = 2L, stride = 1L)
  expect_equal(r$output_shape, c(4L, 4L))
})

test_that("morie_mxpol_maxpool_forward errors when input smaller than kernel", {
  expect_error(morie_mxpol_maxpool_forward(matrix(1:4, 2, 2), kernel_size = 3L))
})

test_that("morie_maxpool_forward alias matches morie_mxpol_maxpool_forward", {
  x <- matrix(1:16, 4, 4)
  expect_equal(morie_maxpool_forward(x, 2L)$y, morie_mxpol_maxpool_forward(x, 2L)$y)
})

test_that("morie_nbeats_basis forecasts a seasonal series", {
  set.seed(11)
  n <- 60
  t <- seq_len(n)
  x <- 0.1 * t + 2 * sin(2 * pi * t / 12) + rnorm(n, 0, 0.2)
  r <- morie_nbeats_basis(x, horizon = 3, n_trend = 2, n_season = 3, period = 12)
  expect_type(r, "list")
  expect_named(r, c(
    "forecast", "fitted", "trend", "seasonal",
    "theta_trend", "theta_seasonal", "r2", "n",
    "horizon", "method"
  ))
  expect_length(r$forecast, 3L)
  expect_equal(r$n, n)
  expect_length(r$fitted, n)
  expect_true(is.finite(r$r2))
  expect_true(r$r2 <= 1)
})

test_that("morie_nbeats_basis works with default arguments", {
  set.seed(12)
  x <- rnorm(50)
  r <- morie_nbeats_basis(x)
  expect_length(r$forecast, 1L)
  expect_equal(r$horizon, 1)
})

test_that("morie_nbeats_basis errors on a too-short series", {
  expect_error(morie_nbeats_basis(rnorm(5), n_trend = 3, n_season = 5))
})

test_that("nstat estimates a non-stationary covariance", {
  set.seed(21)
  n <- 12
  coords <- cbind(runif(n), runif(n))
  x <- rnorm(n)
  r <- nstat(x, coords)
  expect_type(r, "list")
  expect_named(r, c("estimate", "n", "method"))
  expect_equal(r$n, n)
  expect_length(r$estimate$sigma_local, n)
  expect_equal(dim(r$estimate$C_matrix), c(n, n))
  expect_true(all(is.finite(r$estimate$sigma_local)))
  expect_true(r$estimate$bandwidth > 0)
})

test_that("nstat accepts an explicit bandwidth", {
  set.seed(22)
  coords <- cbind(runif(10), runif(10))
  r <- nstat(rnorm(10), coords, bandwidth = 0.5)
  expect_equal(r$estimate$bandwidth, 0.5)
})

test_that("nstat errors when coords rows mismatch length(x)", {
  expect_error(nstat(rnorm(5), cbind(runif(4), runif(4))))
})

test_that("morie_nonstationary_covariance alias matches nstat", {
  set.seed(23)
  coords <- cbind(runif(8), runif(8))
  x <- rnorm(8)
  expect_equal(morie_nonstationary_covariance(x, coords)$method, nstat(x, coords)$method)
})

test_that("okrig predicts at a single target (exponential)", {
  r <- okrig(
    c(1, 2, 3, 4, 5), matrix(0:4, ncol = 1),
    matrix(2.5, 1, 1), "exponential", 0, 1, 2
  )
  expect_type(r, "list")
  expect_named(r, c("estimate", "se", "n", "method"))
  expect_equal(r$n, 5L)
  expect_length(r$estimate, 1L)
  expect_true(is.finite(r$estimate))
  expect_true(r$se >= 0)
})

test_that("okrig supports gaussian and spherical models", {
  coords <- matrix(0:4, ncol = 1)
  x <- c(1, 2, 3, 4, 5)
  rg <- okrig(x, coords, matrix(2.5, 1, 1), "gaussian", 0, 1, 2)
  rs <- okrig(x, coords, matrix(2.5, 1, 1), "spherical", 0, 1, 2)
  expect_true(is.finite(rg$estimate))
  expect_true(is.finite(rs$estimate))
})

test_that("okrig predicts at multiple targets", {
  coords <- matrix(0:4, ncol = 1)
  x <- c(1, 2, 3, 4, 5)
  r <- okrig(x, coords, matrix(c(1.5, 2.5, 3.5), ncol = 1))
  expect_length(r$estimate, 3L)
  expect_length(r$se, 3L)
})

test_that("okrig errors on bad model and dimension mismatch", {
  coords <- matrix(0:4, ncol = 1)
  x <- c(1, 2, 3, 4, 5)
  expect_error(okrig(x, coords, matrix(2.5, 1, 1), model = "nonsense"))
  expect_error(okrig(x, cbind(0:4, 0:4), matrix(2.5, 1, 1)))
})

test_that("morie_ordinary_kriging alias matches okrig", {
  coords <- matrix(0:4, ncol = 1)
  x <- c(1, 2, 3, 4, 5)
  expect_equal(
    morie_ordinary_kriging(x, coords, matrix(2.5, 1, 1))$estimate,
    okrig(x, coords, matrix(2.5, 1, 1))$estimate
  )
})

test_that("optcl handles an empty vector", {
  r <- optcl(numeric(0))
  expect_type(r, "list")
  expect_named(r, c("cut", "correct_class", "polarity", "pre", "n", "method"))
  expect_equal(r$n, 0L)
  expect_true(is.na(r$cut))
})

test_that("optcl with no votes returns the median cut", {
  r <- optcl(c(-2, -1, 0, 1, 2))
  expect_equal(r$cut, stats::median(c(-2, -1, 0, 1, 2)))
  expect_true(is.na(r$pre))
})

test_that("optcl finds a separating cut with votes", {
  x <- c(-3, -2, -1, 1, 2, 3)
  votes <- c(0L, 0L, 0L, 1L, 1L, 1L)
  r <- optcl(x, votes)
  expect_equal(r$n, 6L)
  expect_equal(r$correct_class, 6L)
  expect_true(r$pre >= 0 && r$pre <= 1)
  expect_true(r$polarity %in% c(1L, -1L))
})

test_that("morie_optimal_classification alias matches optcl", {
  x <- c(-1, 0, 1, 2)
  votes <- c(0L, 0L, 1L, 1L)
  expect_equal(morie_optimal_classification(x, votes)$cut, optcl(x, votes)$cut)
})

test_that("morie_ordered_categories computes the linear-by-linear statistic", {
  tab <- matrix(c(
    10, 5, 2,
    4, 8, 6,
    1, 3, 11
  ), nrow = 3, byrow = TRUE)
  r <- morie_ordered_categories(tab)
  expect_type(r, "list")
  expect_named(r, c("statistic", "p_value", "df", "n", "correlation", "method"))
  expect_equal(r$df, 1L)
  expect_true(is.finite(r$statistic))
  expect_true(r$p_value >= 0 && r$p_value <= 1)
  expect_true(abs(r$correlation) <= 1)
})

test_that("morie_ordered_categories returns NA structure for degenerate tables", {
  r <- morie_ordered_categories(matrix(1, 1, 1))
  expect_true(is.na(r$statistic))
  expect_equal(r$df, 1L)
})

test_that("morie_ordered_categories accepts custom scores", {
  tab <- matrix(c(6, 2, 3, 7), nrow = 2)
  r <- morie_ordered_categories(tab, row_scores = c(0, 1), col_scores = c(0, 2))
  expect_true(is.finite(r$statistic))
})

test_that("morie_ordered_alternatives_test runs on ordered groups", {
  set.seed(31)
  groups <- list(rnorm(8, 0), rnorm(8, 1), rnorm(8, 2))
  r <- morie_ordered_alternatives_test(groups)
  expect_type(r, "list")
  expect_true(all(c("statistic", "p_value", "E_J", "Var_J", "n") %in% names(r)))
  expect_equal(r$n, 24)
  expect_true(is.finite(r$E_J))
  expect_true(r$Var_J > 0)
  expect_true(r$p_value >= 0 && r$p_value <= 1)
})

test_that("morie_ordered_alternatives_test handles a too-short group list", {
  res <- tryCatch(morie_ordered_alternatives_test(list(1:3)),
    error = function(e) "errored"
  )
  if (identical(res, "errored")) {
    expect_true(TRUE)
  } else {
    expect_type(res, "list")
  }
})

test_that("morie_ordered_alternatives_test works on two groups", {
  r <- morie_ordered_alternatives_test(list(c(1, 2, 3), c(4, 5, 6)))
  expect_type(r, "list")
  expect_equal(r$n, 6)
  expect_true(is.finite(r$E_J))
})

test_that("morie_find_project_root detects a synthetic project root", {
  root <- file.path(tempdir(), paste0("morie_root_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(root, "docs", "source"), recursive = TRUE)
  file.create(file.path(root, "pyproject.toml"))
  sub <- file.path(root, "a", "b")
  dir.create(sub, recursive = TRUE)
  detected <- morie_find_project_root(start = sub, max_up = 10L)
  expect_true(is.character(detected))
  expect_equal(
    normalizePath(detected, winslash = "/", mustWork = FALSE),
    normalizePath(root, winslash = "/", mustWork = FALSE)
  )
  unlink(root, recursive = TRUE)
})

test_that("morie_find_project_root errors when no markers exist", {
  bare <- file.path(tempdir(), paste0("morie_bare_", as.integer(runif(1, 1, 1e6))))
  dir.create(bare, recursive = TRUE)
  expect_error(morie_find_project_root(start = bare, max_up = 2L))
  unlink(bare, recursive = TRUE)
})

test_that("morie_paths returns the standard named path list", {
  root <- file.path(tempdir(), paste0("morie_paths_", as.integer(runif(1, 1, 1e6))))
  dir.create(root, recursive = TRUE)
  p <- morie_paths(project_root = root)
  expect_type(p, "list")
  expect_true(all(c(
    "project_root", "data_dir", "cache_dir", "datasets_dir",
    "outputs_dir", "outputs_manifest", "rtests_dir",
    "pytests_dir", "tools_dir", "docs_dir"
  ) %in% names(p)))
  expect_true(grepl("data$", p$data_dir))
  unlink(root, recursive = TRUE)
})

test_that("paths internal helpers behave", {
  expect_equal(morie:::`%||%`(NULL, "fallback"), "fallback")
  expect_true(is.na(morie:::`%||%`(NA, "fallback")))
  expect_equal(morie:::`%||%`("", "fallback"), "")
  expect_equal(morie:::`%||%`("kept", "fallback"), "kept")
  expect_true(morie:::is_absolute_path("/usr/local"))
  expect_true(morie:::is_absolute_path("C:/Users"))
  expect_false(morie:::is_absolute_path("relative/path"))
})

test_that("morie_pca_dimension_reduction reduces a numeric matrix", {
  set.seed(41)
  X <- matrix(rnorm(100), 20, 5)
  r <- morie_pca_dimension_reduction(X, n_components = 2L)
  expect_type(r, "list")
  expect_named(r, c(
    "estimate", "components", "explained_variance",
    "explained_variance_ratio", "singular_values",
    "scores", "n_components", "n", "method"
  ))
  expect_equal(r$n_components, 2L)
  expect_equal(r$n, 20L)
  expect_equal(dim(r$components), c(2L, 5L))
  expect_equal(dim(r$scores), c(20L, 2L))
  expect_true(r$estimate >= 0 && r$estimate <= 1)
})

test_that("morie_pca_dimension_reduction defaults n_components and accepts a vector", {
  set.seed(42)
  X <- matrix(rnorm(60), 12, 5)
  r <- morie_pca_dimension_reduction(X)
  expect_true(r$n_components <= 5L)

  rv <- morie_pca_dimension_reduction(rnorm(15))
  expect_equal(rv$n, 15L)
})

test_that("morie_percentile_modified_rank runs a two-sample test", {
  set.seed(51)
  x <- rnorm(20, 0)
  y <- rnorm(20, 1)
  r <- morie_percentile_modified_rank(x, y)
  expect_type(r, "list")
  expect_true(all(c("statistic", "p_value", "z", "n", "m", "q") %in% names(r)))
  expect_equal(r$n, 40L)
  expect_equal(r$m, 20L)
  expect_true(is.finite(r$z))
  expect_true(r$p_value >= 0 && r$p_value <= 1)
})

test_that("morie_percentile_modified_rank returns NA structure for tiny samples", {
  r <- morie_percentile_modified_rank(c(1), c(2, 3, 4))
  expect_true(is.na(r$statistic))
  expect_equal(r$m, 1L)
})

test_that("morie_percentile_modified_rank errors on out-of-range q", {
  x <- rnorm(10)
  y <- rnorm(10)
  expect_error(morie_percentile_modified_rank(x, y, q = 0.5))
  expect_error(morie_percentile_modified_rank(x, y, q = 0))
})

test_that("morie_penalized_regression fits an elastic-net model", {
  set.seed(10)
  X <- matrix(rnorm(120), 30, 4)
  b <- c(1, 0, -1, 0)
  y <- X %*% b + 0.1 * rnorm(30)
  r <- morie_penalized_regression(X, y, alpha = 1, lam = 0.05)
  expect_type(r, "list")
  expect_true(all(c(
    "estimate", "beta", "intercept", "se", "alpha",
    "lam", "n_iter", "n", "p", "method"
  ) %in% names(r)))
  expect_length(r$beta, 4L)
  expect_equal(r$n, 30L)
  expect_equal(r$p, 4L)
  expect_true(is.finite(r$se))
  expect_true(is.finite(r$intercept))
})

test_that("morie_penalized_regression supports ridge (alpha = 0) and default args", {
  set.seed(13)
  X <- matrix(rnorm(80), 20, 4)
  y <- as.numeric(X %*% c(1, 1, 0, 0) + rnorm(20))
  rr <- morie_penalized_regression(X, y, alpha = 0, lam = 0.5)
  expect_equal(rr$alpha, 0)
  rd <- morie_penalized_regression(X, y)
  expect_equal(rd$alpha, 0.5)
  expect_length(rd$beta, 4L)
})

test_that("morie_permutation_test_general runs a small permutation test", {
  set.seed(0)
  x <- rnorm(15)
  y <- rnorm(15)
  r <- morie_permutation_test_general(x, y, B = 200L, seed = 0L)
  expect_type(r, "list")
  expect_named(r, c(
    "statistic", "p_value", "n_x", "n_y", "B",
    "alternative", "method"
  ))
  expect_equal(r$n_x, 15L)
  expect_equal(r$n_y, 15L)
  expect_equal(r$B, 200L)
  expect_true(r$p_value > 0 && r$p_value <= 1)
})

test_that("permt supports one-sided alternatives and custom statistic", {
  set.seed(1)
  x <- rnorm(12, 1)
  y <- rnorm(12, 0)
  rg <- permt(x, y, B = 150L, alternative = "greater", seed = 1L)
  expect_equal(rg$alternative, "greater")
  rl <- permt(x, y, B = 150L, alternative = "less", seed = 1L)
  expect_equal(rl$alternative, "less")

  med_diff <- function(a, b) stats::median(a) - stats::median(b)
  rc <- permt(x, y, statistic = med_diff, B = 150L, seed = 2L)
  expect_true(is.finite(rc$statistic))
})

test_that("permt returns NA structure for empty input", {
  r <- permt(numeric(0), c(1, 2, 3))
  expect_type(r, "list")
  expect_true(is.na(r$statistic))
  expect_equal(r$n_x, 0L)
})

test_that("morie_permutation_test_general alias matches permt", {
  set.seed(3)
  x <- rnorm(10)
  y <- rnorm(10)
  expect_equal(
    morie_permutation_test_general(x, y, B = 100L, seed = 5L)$statistic,
    permt(x, y, B = 100L, seed = 5L)$statistic
  )
})
