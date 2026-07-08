# SPDX-License-Identifier: AGPL-3.0-or-later
# Filesystem cache backends (parquet default + rds fallback). These must work
# with NO SQL backend (DuckDB/RSQLite) installed -- that is the whole point of
# Phase 1: the r-universe build no longer compiles RSQLite's vendored boost.

.with_backend <- function(be, code) {
  old <- Sys.getenv("MORIE_CACHE_BACKEND", unset = NA)
  Sys.setenv(MORIE_CACHE_BACKEND = be)
  on.exit(if (is.na(old)) Sys.unsetenv("MORIE_CACHE_BACKEND") else
    Sys.setenv(MORIE_CACHE_BACKEND = old))
  force(code)
}

test_that("RDS cache backend round-trips with no SQL backend", {
  .with_backend("rds", {
    d <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
    expect_equal(morie_cache_store(d, "fs_demo_rds"), 5L)
    lst <- morie_cache_list()
    expect_true("fs_demo_rds" %in% lst$table)
    expect_equal(lst$rows[lst$table == "fs_demo_rds"], 5L)
    got <- morie_cache_load("fs_demo_rds")
    expect_equal(got$x, 1:5)
    expect_equal(got$y, letters[1:5])
    expect_null(morie_cache_load("fs_missing_rds"))
  })
})

test_that("Parquet cache backend round-trips (cross-language format)", {
  skip_if_not_installed("nanoparquet")
  .with_backend("parquet", {
    d <- data.frame(a = 1:3, b = c("p", "q", "r"), stringsAsFactors = FALSE)
    expect_equal(morie_cache_store(d, "fs_demo_pq"), 3L)
    lst <- morie_cache_list()
    expect_equal(lst$rows[lst$table == "fs_demo_pq"], 3L)
    got <- morie_cache_load("fs_demo_pq")
    expect_equal(got$a, 1:3)
    expect_equal(got$b, c("p", "q", "r"))
    expect_null(morie_cache_load("fs_missing_pq"))
  })
})

test_that("file cache rejects unsafe table names (path traversal)", {
  .with_backend("rds", {
    expect_error(morie_cache_store(data.frame(x = 1), "../evil"),
      "Invalid table_name")
    expect_error(morie_cache_store(data.frame(x = 1), "a/b"),
      "Invalid table_name")
  })
})
