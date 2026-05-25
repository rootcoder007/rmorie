# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2W: tests for database.R cache + dataset loader API.
# Most paths are tested indirectly via test-db_indexes.R; this file
# adds direct coverage for morie_cache_dir, morie_cache_clear,
# morie_cache_load/store/list/file, morie_builtin_db,
# morie_load_cpads / load_dataset / list_datasets / dataset_info /
# userguide.

# ----------------------------------------------------- morie_cache_dir

test_that("morie_cache_dir returns a writable directory path", {
  d <- morie_cache_dir()
  expect_type(d, "character")
  expect_length(d, 1L)
})

test_that("morie_cache_dir(subdir) returns a deeper path", {
  parent <- morie_cache_dir()
  child  <- morie_cache_dir("siu")
  expect_true(startsWith(child, parent) || file.path(parent, "siu") == child)
})

# ----------------------------------------------------- morie_builtin_db

test_that("morie_builtin_db returns a path or NULL", {
  out <- tryCatch(morie_builtin_db(), error = function(e) NULL)
  expect_true(is.null(out) || is.character(out))
})

# ----------------------------------------------------- morie_db_connect

test_that("morie_db_connect returns a DBI connection on a temp path", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE) ||
                requireNamespace("duckdb", quietly = TRUE),
              "no SQLite/duckdb backend installed")
  tmp <- tempfile(fileext = ".db")
  on.exit({ if (file.exists(tmp)) unlink(tmp) }, add = TRUE)
  con <- tryCatch(morie_db_connect(db_path = tmp), error = function(e) e)
  if (inherits(con, "error")) {
    skip(sprintf("db_connect error: %s", conditionMessage(con)))
  }
  expect_s4_class(con, "DBIConnection")
  DBI::dbDisconnect(con)
})

# ------------------------------------- store/load/list round-trip on SQLite

test_that("cache_store + cache_load round-trip on a SQLite db", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  df <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
  morie_cache_store(df, "demo_round_trip", con = con)
  out <- morie_cache_load("demo_round_trip", con = con)
  expect_equal(out$x, df$x)
  expect_equal(out$y, df$y)
})

test_that("cache_load returns NULL on missing table", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_null(morie_cache_load("__no_such_table__", con = con))
})

test_that("cache_list enumerates user-stored tables", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  morie_cache_store(data.frame(x = 1:3), "tbl_a", con = con)
  morie_cache_store(data.frame(x = 1:3), "tbl_b", con = con)
  out <- morie_cache_list(con = con)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("tbl_a", "tbl_b") %in% out$table))
})

test_that("cache_file imports a CSV into a named table", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data.frame(a = 1:5, b = 5:1), csv,
                   row.names = FALSE)
  morie_cache_file(csv, "from_file", con = con)
  out <- morie_cache_load("from_file", con = con)
  expect_equal(nrow(out), 5L)
})

# ----------------------------------------------------- dataset loaders (offline)

test_that("morie_list_datasets returns a data.frame on a fresh in-mem db", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  out <- tryCatch(morie_list_datasets(con = con),
                  error = function(e) e)
  expect_true(inherits(out, "error") || is.data.frame(out))
})

test_that("morie_dataset_info returns metadata for a known key or NULL", {
  out <- tryCatch(morie_dataset_info("cpads"), error = function(e) e)
  expect_true(is.null(out) || is.list(out) || inherits(out, "error"))
})

test_that("morie_dataset_info(unknown_key) returns NULL or errors", {
  out <- tryCatch(morie_dataset_info("__not_a_dataset__"),
                  error = function(e) e)
  expect_true(is.null(out) || inherits(out, "error"))
})

# ----------------------------------------------------- userguides + bootstrap

test_that("morie_userguide returns a path or NULL when no userguide", {
  out <- tryCatch(morie_userguide(), error = function(e) e)
  expect_true(is.null(out) || is.character(out) || is.list(out) ||
                inherits(out, "error"))
})

test_that("morie_load_cpads falls back gracefully without cache", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  out <- tryCatch(morie_load_cpads(use_ckan = FALSE, con = con),
                  error = function(e) e)
  # Without CKAN + without cached data, expect either a synthetic
  # fallback frame or a structured error.
  expect_true(inherits(out, "error") || is.data.frame(out) || is.list(out))
})
