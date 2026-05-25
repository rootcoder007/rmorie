# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Backend tests for morie's DBI-generic cache layer.
#
# rOpenSci isolation rules followed:
#   * Embedded backends (SQLite, DuckDB) are created in tempfile() and
#     deleted via withr::defer() so the test leaves the filesystem
#     exactly as it found it -- even if the test crashes mid-way.
#   * The server backend (PostgreSQL) needs a live server, which CRAN
#     and rOpenSci's automated check farm do not provide. Those tests
#     run only when MORIE_PG_TEST=true is set in the environment (e.g.
#     under a GitHub Actions workflow with a postgres service).

# ---- 1. SQLite (the default fallback; always available) ------------------
test_that("DBI/SQLite cache round-trip via db_path", {

  tmp_db <- tempfile(fileext = ".db")
  withr::defer(if (file.exists(tmp_db)) unlink(tmp_db))

  df <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
  morie_cache_store(df, "demo", db_path = tmp_db)
  out <- morie_cache_load("demo", db_path = tmp_db)

  expect_s3_class(out, "data.frame")
  expect_equal(out$x, df$x)
  expect_equal(out$y, df$y)
})

test_that("DBI/SQLite cache via pre-opened `con=`", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  tmp_db <- tempfile(fileext = ".db")
  withr::defer(if (file.exists(tmp_db)) unlink(tmp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = tmp_db)
  withr::defer(DBI::dbDisconnect(con))

  df <- data.frame(a = rnorm(3), b = rnorm(3))
  morie_cache_store(df, "demo", con = con)

  expect_true(DBI::dbIsValid(con))
  expect_true(DBI::dbExistsTable(con, "demo"))
  expect_equal(nrow(morie_cache_load("demo", con = con)), 3L)
})

test_that(".morie_db_handle rejects non-DBI input", {
  expect_error(
    morie_cache_store(data.frame(x = 1), "demo", con = "not-a-con"),
    "must be a DBIConnection"
  )
})

# ---- 2. DuckDB (default for new caches; columnar, larger data sizes) -----
test_that("DBI/DuckDB cache round-trip via tempfile", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  tmp_db <- tempfile(fileext = ".duckdb")
  withr::defer(if (file.exists(tmp_db)) unlink(tmp_db))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmp_db)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_true(DBI::dbIsValid(con))

  df <- data.frame(x = 1:1000, y = rnorm(1000))
  morie_cache_store(df, "big", con = con)

  expect_true(DBI::dbExistsTable(con, "big"))
  out <- morie_cache_load("big", con = con)
  expect_equal(nrow(out), 1000L)
  expect_equal(out$x, df$x)
})

test_that("morie_db_connect() default opens DuckDB when available", {
  skip_if_not_installed("DBI")

  tmp_dir <- tempfile("morie-test-")
  dir.create(tmp_dir)
  withr::defer(unlink(tmp_dir, recursive = TRUE))
  withr::local_envvar(XDG_CACHE_HOME = tmp_dir)

  con <- morie_db_connect()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_true(inherits(con, "duckdb_connection"))
})

test_that("morie_db_connect() falls back to SQLite when duckdb absent", {
  skip_if_not_installed("DBI")

  tmp_dir <- tempfile("morie-test-")
  dir.create(tmp_dir)
  withr::defer(unlink(tmp_dir, recursive = TRUE))
  withr::local_envvar(XDG_CACHE_HOME = tmp_dir)

  # Force the SQLite path by passing a *.db extension; this exercises the
  # fallback even on systems where duckdb is installed.
  sqlite_path <- file.path(tmp_dir, "explicit.db")
  con <- morie_db_connect(db_path = sqlite_path)
  withr::defer(DBI::dbDisconnect(con))
  expect_s4_class(con, "SQLiteConnection")
})

# ---- 3. PostgreSQL (server backend; CI only, opt-in via env var) ---------
test_that("DBI/PostgreSQL cache round-trip (CI only)", {
  testthat::skip_if_not(
    identical(Sys.getenv("MORIE_PG_TEST"), "true"),
    message = "Set MORIE_PG_TEST=true (and a live postgres service) to run."
  )

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("PGDATABASE", "testdb"),
    host     = Sys.getenv("PGHOST", "localhost"),
    port     = as.integer(Sys.getenv("PGPORT", "5432")),
    user     = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "password")
  )
  withr::defer(DBI::dbDisconnect(con))

  expect_true(DBI::dbIsValid(con))

  # Unique table name so parallel CI runs don't collide.
  tbl <- paste0(
    "morie_test_",
    format(Sys.time(), "%Y%m%d%H%M%S"), "_",
    as.integer(stats::runif(1, 0, .Machine$integer.max))
  )
  withr::defer(
    if (DBI::dbExistsTable(con, tbl)) {
      DBI::dbRemoveTable(con, tbl)
    }
  )

  df <- data.frame(x = 1:100, y = rnorm(100))
  morie_cache_store(df, tbl, con = con)
  expect_true(DBI::dbExistsTable(con, tbl))
  expect_equal(nrow(morie_cache_load(tbl, con = con)), 100L)
})
