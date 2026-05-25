# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2YY: tests for internal helpers across tps_io.R (file readers),
# study_core.R, study_reporting.R, otis_all_analyze.R, causal.R, and
# tps_render.R.

# ============================================================== tps_io.R readers

.with_fake_tps_cache <- function(name, fmt_subdir, ext, csv_text) {
  cache <- tempfile("morie_tps_cache_")
  dir.create(file.path(cache, name, fmt_subdir), recursive = TRUE)
  fpath <- file.path(cache, name, fmt_subdir, paste0("data.", ext))
  writeLines(csv_text, fpath)
  old <- Sys.getenv("MORIE_TPS_DATA_DIR", unset = NA_character_)
  Sys.setenv(MORIE_TPS_DATA_DIR = cache)
  list(cache = cache, old = old)
}

.restore_tps_cache <- function(state) {
  unlink(state$cache, recursive = TRUE)
  if (is.na(state$old)) {
    Sys.unsetenv("MORIE_TPS_DATA_DIR")
  } else {
    Sys.setenv(MORIE_TPS_DATA_DIR = state$old)
  }
}

test_that(".morie_tps_read_csv reads a bundled CSV under cache layout", {
  state <- .with_fake_tps_cache("Assault", "CSV", "csv",
                                  "a,b\n1,x\n2,y\n3,z")
  on.exit(.restore_tps_cache(state), add = TRUE)
  df <- morie:::.morie_tps_read_csv("Assault", NULL)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3L)
})

test_that(".morie_tps_read_csv honours nrows truncation", {
  state <- .with_fake_tps_cache("Assault", "CSV", "csv",
                                  "a,b\n1,x\n2,y\n3,z\n4,w")
  on.exit(.restore_tps_cache(state), add = TRUE)
  df <- morie:::.morie_tps_read_csv("Assault", nrows = 2L)
  expect_equal(nrow(df), 2L)
})

test_that(".morie_tps_read_excel requires readxl + reads xlsx via cache", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  if (!requireNamespace("readxl", quietly = TRUE)) {
    skip("readxl not installed")
  }
  if (!requireNamespace("writexl", quietly = TRUE)) {
    skip("writexl not installed; cannot synth xlsx fixture")
  }
  cache <- tempfile("morie_tps_xlsx_cache_")
  dir.create(file.path(cache, "Assault", "Excel"), recursive = TRUE)
  fpath <- file.path(cache, "Assault", "Excel", "data.xlsx")
  writexl::write_xlsx(data.frame(a = 1:3, b = c("x", "y", "z")), fpath)
  old <- Sys.getenv("MORIE_TPS_DATA_DIR", unset = NA_character_)
  Sys.setenv(MORIE_TPS_DATA_DIR = cache)
  on.exit({
    unlink(cache, recursive = TRUE)
    if (is.na(old)) Sys.unsetenv("MORIE_TPS_DATA_DIR")
    else Sys.setenv(MORIE_TPS_DATA_DIR = old)
  }, add = TRUE)
  df <- morie:::.morie_tps_read_excel("Assault", NULL)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3L)
})

test_that(".morie_tps_read_geojson reads a GeoJSON via sf", {
  skip_if_not_installed("sf")
  if (!requireNamespace("sf", quietly = TRUE)) {
    skip("sf not installed")
  }
  cache <- tempfile("morie_tps_geo_cache_")
  dir.create(file.path(cache, "Assault", "GeoJSON"), recursive = TRUE)
  fpath <- file.path(cache, "Assault", "GeoJSON", "data.geojson")
  # Minimal FeatureCollection.
  writeLines('{"type":"FeatureCollection","features":[
    {"type":"Feature","properties":{"a":1},"geometry":{"type":"Point","coordinates":[0,0]}},
    {"type":"Feature","properties":{"a":2},"geometry":{"type":"Point","coordinates":[1,1]}}
  ]}', fpath)
  old <- Sys.getenv("MORIE_TPS_DATA_DIR", unset = NA_character_)
  Sys.setenv(MORIE_TPS_DATA_DIR = cache)
  on.exit({
    unlink(cache, recursive = TRUE)
    if (is.na(old)) Sys.unsetenv("MORIE_TPS_DATA_DIR")
    else Sys.setenv(MORIE_TPS_DATA_DIR = old)
  }, add = TRUE)
  df <- morie:::.morie_tps_read_geojson("Assault", NULL)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
})

# ====================================================================== study_core.R

test_that(".or_table returns OR/SE/p table on a logistic fit", {
  set.seed(1L); n <- 100L
  y <- stats::rbinom(n, 1L, 0.5)
  x <- stats::rnorm(n)
  fit <- suppressWarnings(stats::glm(y ~ x, family = stats::binomial()))
  out <- morie:::.or_table(fit)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("term", "log_odds", "OR", "p_value") %in% names(out)))
  # OR == exp(log_odds) up to clip_exp clipping.
  expect_equal(out$OR, exp(out$log_odds), tolerance = 1e-8)
})

test_that(".or_table prepends `model` when given a model label", {
  set.seed(2L); n <- 80L
  y <- stats::rbinom(n, 1L, 0.5); x <- stats::rnorm(n)
  fit <- suppressWarnings(stats::glm(y ~ x, family = stats::binomial()))
  out <- morie:::.or_table(fit, model = "M1")
  expect_true("model" %in% names(out))
  expect_true(all(out$model == "M1"))
})

test_that(".linear_coef_table returns coef/CI/p on a linear fit", {
  set.seed(3L); n <- 100L
  x <- stats::rnorm(n); y <- 1 + 2 * x + stats::rnorm(n, sd = 0.3)
  fit <- stats::lm(y ~ x)
  out <- morie:::.linear_coef_table(fit, model = "demo")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("estimate", "se", "ci_lower95", "ci_upper95",
                    "p_value") %in% names(out)))
})

test_that(".cpads_labeled_data labels gender/age/region/health", {
  df <- data.frame(
    gender = c(1L, 2L, 3L),
    age_group = c(1L, 2L, 3L),
    province_region = c(1L, 2L, 3L),
    mental_health = c(1L, 2L, 3L),
    physical_health = c(1L, 2L, 3L),
    alcohol_past12m = c(1, 0, 1),
    ebac_tot = c(0.05, NA, 0.02))
  out <- morie:::.cpads_labeled_data(df)
  expect_true("gender_label" %in% names(out))
  expect_true(is.factor(out$gender_label))
  expect_equal(levels(out$gender_label),
               c("Female", "Male", "Non-binary"))
  expect_equal(out$ebac_observed, c(1L, 0L, 1L))
})

# =============================================================== study_reporting.R

test_that(".read_existing_output returns fallback when file is absent", {
  out <- morie:::.read_existing_output(tempfile("noout_"),
                                         "data.csv",
                                         fallback = list(default = TRUE))
  expect_equal(out, list(default = TRUE))
})

test_that(".read_existing_output reads CSV when file exists", {
  d <- tempfile("read_exist_")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  utils::write.csv(data.frame(a = 1:3, b = c("x", "y", "z")),
                   file.path(d, "data.csv"), row.names = FALSE)
  out <- morie:::.read_existing_output(d, "data.csv")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
})

test_that(".copy_legacy_artifacts no-ops when src files are absent", {
  out_dir <- tempfile("copy_legacy_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  copied <- morie:::.copy_legacy_artifacts(
    relative_paths = c("missing.csv"),
    output_dir = out_dir,
    root = tempfile("noroot_"))
  expect_equal(copied, character(0))
})

test_that(".copy_legacy_artifacts copies present files + returns names", {
  root <- tempfile("legacy_root_")
  dir.create(root, recursive = TRUE)
  writeLines("a,b\n1,2", file.path(root, "report.csv"))
  out_dir <- tempfile("copy_target_")
  dir.create(out_dir, recursive = TRUE)
  on.exit({ unlink(root, recursive = TRUE)
            unlink(out_dir, recursive = TRUE) }, add = TRUE)
  copied <- morie:::.copy_legacy_artifacts(
    relative_paths = c("report.csv"),
    output_dir = out_dir,
    root = root)
  expect_equal(copied, "report.csv")
  expect_true(file.exists(file.path(out_dir, "report.csv")))
})

# =============================================================== otis_all_analyze.R

test_that(".otis_classify_bin maps known bins to Solitary / Torture / Unknown", {
  # Bins are defined as character labels in .otis_mandela_solitary_bins
  # and .otis_mandela_torture_bins; an unknown label falls to "Unknown".
  solitary <- morie:::.otis_mandela_solitary_bins
  torture  <- morie:::.otis_mandela_torture_bins
  expect_true(length(solitary) > 0L)
  expect_true(length(torture) > 0L)
  out <- morie:::.otis_classify_bin(c(solitary[1], torture[1],
                                        "DefinitelyNotAKnownBin"))
  expect_equal(out, c("Solitary Confinement (<=15d)",
                      "Torture (>=16d)", "Unknown"))
})

# ====================================================================== causal.R

test_that(".dml_xfit_ridge_predict returns length-n_te numeric vector", {
  set.seed(4L)
  X_tr <- matrix(stats::rnorm(80L), 40L, 2L)
  y_tr <- X_tr[, 1L] - 0.5 * X_tr[, 2L] + stats::rnorm(40L, sd = 0.3)
  X_te <- matrix(stats::rnorm(20L), 10L, 2L)
  pred <- morie:::.dml_xfit_ridge_predict(X_tr, y_tr, X_te,
                                            lambda = 1.0)
  expect_length(pred, 10L)
  expect_true(all(is.finite(pred)))
})

# ================================================================== tps_render.R

test_that(".tps_draw_compass invisibly returns NULL on base graphics", {
  pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot(0:10, 0:10, type = "n")
  out <- morie:::.tps_draw_compass(5, 5, size = 1.0, use_gg = FALSE)
  expect_null(out)
})

test_that(".tps_draw_scalebar invisibly returns NULL on base graphics", {
  pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot(0:10, 0:10, type = "n")
  out <- morie:::.tps_draw_scalebar(2, 2, length_km = 3,
                                       use_gg = FALSE)
  expect_null(out)
})

test_that(".tps_draw_compass returns a list of ggplot annotates on gg path", {
  skip_if_not_installed("ggplot2")
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    skip("ggplot2 not installed")
  }
  out <- morie:::.tps_draw_compass(5, 5, size = 1.0, use_gg = TRUE)
  expect_type(out, "list")
})

test_that(".tps_draw_scalebar returns a list of ggplot annotates on gg path", {
  skip_if_not_installed("ggplot2")
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    skip("ggplot2 not installed")
  }
  out <- morie:::.tps_draw_scalebar(2, 2, length_km = 3, use_gg = TRUE)
  expect_type(out, "list")
})
