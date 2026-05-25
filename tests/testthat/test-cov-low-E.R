# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch E: entheo_data, okrig, wavts, database, tarmd.

# ==== entheo_data.R ====
test_that("load_dmt_imaging returns synthetic when root missing", {
  set.seed(1)
  out <- morie:::load_dmt_imaging(subject_id = 1L, root = tempfile("no_such_root_"))
  expect_type(out, "list")
  expect_true(out$synthetic)
})

test_that("load_dmt_imaging null subject_id falls back to default list", {
  set.seed(1)
  out <- morie:::load_dmt_imaging(subject_id = NULL, root = tempfile("missing_"))
  expect_true(out$synthetic)
  expect_true(length(out$subject_ids) >= 1L)
})

test_that(".entheo_list_subjects returns empty when fMRI dir missing", {
  td <- tempfile("dmt_root_")
  dir.create(td)
  withr::defer(unlink(td, recursive = TRUE))
  expect_equal(morie:::.entheo_list_subjects(td), character(0))
})

test_that(".entheo_list_subjects picks up .mat filenames", {
  td <- tempfile("dmt_root_")
  dir.create(file.path(td, "fMRI"), recursive = TRUE)
  withr::defer(unlink(td, recursive = TRUE))
  file.create(file.path(td, "fMRI", "LongS04DMT.mat"))
  file.create(file.path(td, "fMRI", "LongS04PCB.mat"))
  file.create(file.path(td, "fMRI", "LongS09DMT.mat"))
  file.create(file.path(td, "fMRI", "LongS09PCB.mat"))
  ids <- morie:::.entheo_list_subjects(td)
  expect_equal(ids, c("04", "09"))
})

# ==== okrig.R ====
test_that("okrig runs on defaults (exponential)", {
  set.seed(1)
  out <- okrig(
    x = rnorm(20),
    coords = matrix(runif(40), 20, 2),
    target = matrix(c(0.5, 0.5), 1, 2)
  )
  expect_named(out, c("estimate", "se", "n", "method"))
  expect_length(out$estimate, 1L)
  expect_true(out$se >= 0)
  expect_equal(out$n, 20L)
  expect_match(out$method, "exponential")
})

test_that("okrig gaussian and spherical models run", {
  set.seed(1)
  coords <- matrix(runif(40), 20, 2)
  x <- rnorm(20)
  target <- matrix(runif(6), 3, 2)
  g <- okrig(x, coords, target, model = "gaussian", range_ = 0.5)
  s <- okrig(x, coords, target, model = "spherical", range_ = 0.5)
  expect_length(g$estimate, 3L)
  expect_length(s$estimate, 3L)
  expect_match(g$method, "gaussian")
  expect_match(s$method, "spherical")
})

test_that("okrig errors on dim mismatch", {
  set.seed(1)
  expect_error(
    okrig(rnorm(10), matrix(runif(20), 10, 2), matrix(rnorm(3), 1, 3)),
    "target dim"
  )
})

# ==== wavts.R ====
test_that("morie_wavelet_time_series runs on defaults", {
  set.seed(1)
  out <- morie_wavelet_time_series(rnorm(64))
  expect_named(out, c(
    "approximation", "details", "energies", "level",
    "n", "wavelet", "method"
  ))
  expect_equal(out$n, 64L)
  expect_true(out$level >= 1L)
  expect_length(out$energies, out$level + 1L)
})

test_that("morie_wavelet_time_series errors on too-short input", {
  set.seed(1)
  expect_error(morie_wavelet_time_series(c(1, 2, 3)), ">=4")
})

test_that("morie_wavelet_time_series caps level at max_lv", {
  set.seed(1)
  out <- morie_wavelet_time_series(rnorm(8), level = 99)
  expect_equal(out$level, 3L) # floor(log2(8)) = 3
})

# ==== database.R ====
test_that("morie_cache_dir respects MORIE_CACHE_DIR", {
  # v0.9.5 CRAN Policy fix: cache override env var is MORIE_CACHE_DIR.
  withr::local_envvar(c(MORIE_CACHE_DIR = file.path(tempdir(), "morie-low-E")))
  expect_equal(
    morie:::morie_cache_dir(),
    file.path(tempdir(), "morie-low-E")
  )
})

test_that("morie_db_connect opens a SQLite handle on .db extension", {
  skip_if_not_installed("DBI")
  tmp <- tempfile(fileext = ".db")
  withr::defer({
    try(DBI::dbDisconnect(con), silent = TRUE)
    unlink(tmp)
  })
  con <- morie_db_connect(db_path = tmp)
  expect_s4_class(con, "DBIConnection")
})

test_that("morie_cache_store + load + list round-trip via con", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(1)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  df <- data.frame(x = rnorm(10), y = rnorm(10))
  n <- morie_cache_store(df, "demo_tbl", con = con)
  expect_equal(n, 10L)
  back <- morie_cache_load("demo_tbl", con = con)
  expect_equal(nrow(back), 10L)
  lst <- morie_cache_list(con = con)
  expect_true("demo_tbl" %in% lst$table)
  expect_equal(lst$rows[lst$table == "demo_tbl"], 10L)
})

test_that("morie_cache_load returns NULL for missing table", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  expect_null(morie_cache_load("no_such_table", con = con))
})

test_that(".morie_db_handle rejects non-DBI con", {
  expect_error(
    morie:::.morie_db_handle(con = list(fake = TRUE)),
    "DBIConnection"
  )
})

test_that("morie_cache_file errors on unsupported format", {
  f <- tempfile(fileext = ".txt")
  file.create(f)
  withr::defer(unlink(f))
  expect_error(morie_cache_file(f, "bad"), "Unsupported")
})

test_that("morie_dataset_info errors on unknown key", {
  expect_error(
    morie_dataset_info("not_a_real_key_zzz"),
    "Unknown dataset key"
  )
})

test_that("morie_load_dataset errors on unknown key", {
  expect_error(
    morie_load_dataset("not_a_real_key_zzz"),
    "Unknown dataset key"
  )
})

test_that("morie_download_bootstrap errors on unknown survey", {
  expect_error(
    morie_download_bootstrap(survey = "not_a_survey_zzz"),
    "Unknown survey"
  )
})

# ==== tarmd.R ====
test_that("morie_threshold_autoregression runs on defaults", {
  set.seed(1)
  x <- as.numeric(arima.sim(list(ar = 0.5), n = 200))
  out <- morie_threshold_autoregression(x)
  expect_named(out, c(
    "threshold", "phi_lower", "phi_upper", "p", "d",
    "regime_sizes", "sse", "n", "method"
  ))
  expect_equal(out$p, 1L)
  expect_equal(out$d, 1L)
  expect_equal(out$n, 200L)
  expect_true(out$sse > 0)
})

test_that("morie_threshold_autoregression errors on too-short series", {
  set.seed(1)
  expect_error(morie_threshold_autoregression(rnorm(5)), "too short")
})
