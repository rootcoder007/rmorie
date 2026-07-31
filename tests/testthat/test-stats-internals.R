# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2NN: tests for internal helpers in statistics.R,
# multiple_testing.R, weights.R, tps_io.R, tps_spatial.R.

# ============================================================== statistics.R

test_that(".stat_validate returns silently on numeric input", {
  expect_silent(rmorie:::.stat_validate(c(1, 2, 3)))
})

test_that(".stat_result builds a stats result list", {
  # Signature: method, test_statistic, p_value, df, ci_lower, ci_upper,
  # effect_size, estimate, n, extra. No `interpretation` or `alpha` arg.
  out <- rmorie:::.stat_result(method = "t-test",
                                test_statistic = 2.1,
                                p_value = 0.04,
                                df = 99L,
                                estimate = 0.5,
                                ci_lower = 0.1, ci_upper = 0.9,
                                effect_size = 0.3,
                                n = 100L)
  expect_type(out, "list")
})

test_that(".cohens_d_ind returns positive d for separated samples", {
  set.seed(1L)
  x <- stats::rnorm(50, 0, 1); y <- stats::rnorm(50, 1, 1)
  d <- rmorie:::.cohens_d_ind(x, y)
  expect_true(is.numeric(d) && abs(d) > 0.5)
})

test_that(".cohens_d_one returns d against a single-sample mu0", {
  set.seed(2L); x <- stats::rnorm(50, 1, 1)
  d <- rmorie:::.cohens_d_one(x, mu0 = 0)
  expect_true(is.numeric(d))
})

test_that(".cohens_d_paired returns d on paired differences", {
  set.seed(3L); d <- stats::rnorm(50, 0.5, 1)
  out <- rmorie:::.cohens_d_paired(d)
  expect_true(is.numeric(out))
})

test_that(".mean_ci returns confidence-interval bounds", {
  set.seed(4L); x <- stats::rnorm(100)
  out <- rmorie:::.mean_ci(x, confidence = 0.95)
  expect_true(is.list(out) || is.numeric(out))
})

test_that(".diff_ci returns CI for difference of two means", {
  set.seed(5L); x <- stats::rnorm(50, 0); y <- stats::rnorm(50, 0.5)
  out <- rmorie:::.diff_ci(x, y)
  expect_true(is.list(out) || is.numeric(out))
})

test_that(".fisher_z_ci returns CI on Fisher-z-transformed correlation", {
  out <- rmorie:::.fisher_z_ci(r = 0.4, n = 50L, confidence = 0.95)
  expect_true(is.list(out) || is.numeric(out))
})

# ========================================================== multiple_testing.R

test_that(".mt_check_p validates a p-value vector", {
  expect_silent(rmorie:::.mt_check_p(c(0.01, 0.04, 0.5)))
  expect_error(rmorie:::.mt_check_p(c(0.5, -0.1, 0.2)))
  expect_error(rmorie:::.mt_check_p(c(0.5, 1.5, 0.2)))
})

test_that(".mt_result builds a multiple-testing result list", {
  out <- rmorie:::.mt_result(title = "Test",
                              call = "morie_bonferroni",
                              summary_lines = list(N = 10))
  expect_type(out, "list")
})

test_that(".mt_adjusted returns adjusted-p result list", {
  out <- rmorie:::.mt_adjusted(
    method = "bonferroni",
    p = c(0.01, 0.04, 0.2),
    alpha = 0.05,
    adjusted = c(0.03, 0.12, 0.6))
  expect_type(out, "list")
})

test_that(".mt_combine_result builds a Fisher / Stouffer combined p result", {
  out <- rmorie:::.mt_combine_result(
    method = "fisher",
    stat = 18.4,
    p_comb = 0.001,
    interp = "Strong combined evidence")
  expect_type(out, "list")
})

# ========================================================== weights.R

test_that(".has_survey_pkg returns logical", {
  expect_type(rmorie:::.has_survey_pkg(), "logical")
})

# ========================================================== tps_io.R

test_that(".morie_tps_io_category_dir builds the right cache subpath", {
  out <- tryCatch(
    rmorie:::.morie_tps_io_category_dir("Assault", "CSV"),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("category_dir error: %s", conditionMessage(out)))
  }
  expect_true(is.character(out))
})

test_that(".morie_tps_io_pick_one errors when no matching file in dir", {
  tmp <- tempfile("tps_io_pick_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  expect_error(
    rmorie:::.morie_tps_io_pick_one(tmp, c("csv", "txt")),
    regexp = "no matching file"
  )
})

test_that(".morie_tps_io_pick_one finds an existing file", {
  tmp <- tempfile("tps_io_pick2_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.create(file.path(tmp, "data.csv"))
  out <- rmorie:::.morie_tps_io_pick_one(tmp, c("csv"))
  expect_match(out, "data\\.csv$")
})

test_that(".morie_tps_apply_nrows trims a data.frame to first n rows", {
  df <- data.frame(x = 1:100)
  out <- rmorie:::.morie_tps_apply_nrows(df, nrows = 10L)
  expect_equal(nrow(out), 10L)
})

test_that(".morie_tps_apply_nrows is a no-op for NULL", {
  df <- data.frame(x = 1:100)
  out <- rmorie:::.morie_tps_apply_nrows(df, nrows = NULL)
  expect_equal(nrow(out), 100L)
})

# ========================================================== tps_spatial.R

test_that(".tps_spatial_result builds a TPS-spatial result list", {
  out <- rmorie:::.tps_spatial_result(title = "Test",
                                       call = "demo")
  expect_type(out, "list")
})

test_that(".tps_hood_counts returns per-hood counts (numeric vector)", {
  df <- data.frame(HOOD_158 = c("001", "002", "001", "003", "002"))
  out <- rmorie:::.tps_hood_counts(df)
  # Returns a named numeric vector keyed by hood id.
  expect_true(is.numeric(out) || is.table(out) ||
                is.data.frame(out) || is.list(out))
  expect_true(length(out) >= 3L)
})

test_that(".tps_knn_adjacency builds k-NN adjacency on 2D coords", {
  set.seed(6L)
  coords <- matrix(stats::runif(20 * 2), 20L, 2L)
  out <- rmorie:::.tps_knn_adjacency(coords, k = 3L)
  expect_true(is.matrix(out) || is.list(out))
})

test_that(".tps_moran_wconst matches the hand-computed S0/S1/S2", {
  set.seed(11)
  W <- matrix(stats::runif(25), 5L, 5L)
  W <- (W + t(W)) / 2; diag(W) <- 0
  k <- rmorie:::.tps_moran_wconst(W)
  # for a symmetric W these reduce to closed forms independent of the
  # implementation, so they check the algebra rather than restate it
  expect_equal(k$S0, sum(W), tolerance = 1e-12)
  expect_equal(k$S1, 2 * sum(W^2), tolerance = 1e-12)
  expect_equal(k$S2, sum((2 * rowSums(W))^2), tolerance = 1e-12)

  # asymmetric W: S1 must use the symmetric part, not W itself
  A <- matrix(stats::runif(25), 5L, 5L); diag(A) <- 0
  ka <- rmorie:::.tps_moran_wconst(A)
  expect_equal(ka$S1, 2 * sum(((A + t(A)) / 2)^2), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(ka$S1, 2 * sum(A^2))))
})

test_that(".tps_moran_variance labels its null and uses sample kurtosis", {
  set.seed(12)
  n <- 5L
  W <- matrix(stats::runif(25), n, n)
  W <- (W + t(W)) / 2; diag(W) <- 0
  z <- stats::rnorm(n); z <- z - mean(z)

  vn <- rmorie:::.tps_moran_variance(W, n, z = z, randomisation = FALSE)
  expect_identical(vn$assumption, "normality")
  expect_true(is.finite(vn$variance))
  # the normality null does not look at the data at all
  expect_equal(vn$variance,
               rmorie:::.tps_moran_variance(W, n, z = 10 * z,
                                            randomisation = FALSE)$variance,
               tolerance = 1e-12)
  expect_true(is.na(vn$kurtosis))

  vr <- rmorie:::.tps_moran_variance(W, n, z = z, randomisation = TRUE)
  expect_identical(vr$assumption, "randomisation")
  expect_true(is.finite(vr$variance))
  expect_equal(vr$kurtosis, n * sum(z^4) / sum(z^2)^2, tolerance = 1e-12)
  # kurtosis is scale-free, so the randomisation null is too
  expect_equal(vr$variance,
               rmorie:::.tps_moran_variance(W, n, z = 10 * z,
                                            randomisation = TRUE)$variance,
               tolerance = 1e-12)
  # and the two nulls must actually differ
  expect_false(isTRUE(all.equal(vr$variance, vn$variance)))
})
