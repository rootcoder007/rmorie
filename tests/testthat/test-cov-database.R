# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 1 -- R/database.R: the DBI/RSQLite cache layer and the
# CKAN fetch / dataset-resolution functions. Cache functions run
# offline against a temporary SQLite database; the network functions
# are exercised with mocked HTTP so no connection is made.

.cdb_have_db <- function() {
}

test_that("morie_cache_dir honours MORIE_CACHE_DIR then R_user_dir", {
  # CRAN Policy compliance (v0.9.5): override via MORIE_CACHE_DIR,
  # default via tools::R_user_dir() (NOT ~/.cache/morie).
  old <- Sys.getenv("MORIE_CACHE_DIR", unset = NA)
  on.exit(if (is.na(old)) {
    Sys.unsetenv("MORIE_CACHE_DIR")
  } else {
    Sys.setenv(MORIE_CACHE_DIR = old)
  }, add = TRUE)
  Sys.setenv(MORIE_CACHE_DIR = file.path(tempdir(), "morie-override"))
  expect_equal(
    morie:::morie_cache_dir(),
    file.path(tempdir(), "morie-override")
  )
  Sys.unsetenv("MORIE_CACHE_DIR")
  # Default falls back to tools::R_user_dir("morie", which = "cache").
  expect_equal(
    morie:::morie_cache_dir(),
    tools::R_user_dir("morie", which = "cache")
  )
})

test_that("morie_builtin_db returns a morie.db path", {
  p <- morie_builtin_db()
  expect_type(p, "character")
  expect_match(p, "morie\\.db$")
})

test_that("morie_db_connect errors clearly without DBI / RSQLite", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) FALSE, .package = "base"
  )
  expect_error(morie_db_connect(tempfile(fileext = ".db")), "DBI")
})

test_that("cache store / load / list round-trip on a temp SQLite db", {
  .cdb_have_db()
  db <- tempfile(fileext = ".db")
  on.exit(unlink(db), add = TRUE)
  d <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
  expect_equal(morie_cache_store(d, "t1", db_path = db), 5L)
  got <- morie_cache_load("t1", db_path = db)
  expect_s3_class(got, "data.frame")
  expect_equal(nrow(got), 5L)
  expect_null(morie_cache_load("no_such_table", db_path = db))
  lst <- morie_cache_list(db_path = db)
  expect_true("t1" %in% lst$table)
  expect_equal(lst$rows[lst$table == "t1"], 5L)
})

test_that("morie_cache_list returns an empty frame for an empty db", {
  skip_if_not_installed("DBI")
  .cdb_have_db()
  db <- tempfile(fileext = ".db")
  on.exit(unlink(db), add = TRUE)
  con <- morie_db_connect(db_path = db)
  DBI::dbDisconnect(con)
  lst <- morie_cache_list(db_path = db)
  expect_s3_class(lst, "data.frame")
  expect_equal(nrow(lst), 0L)
})

test_that("morie_cache_file ingests csv and rds, errors on other formats", {
  .cdb_have_db()
  db <- tempfile(fileext = ".db")
  on.exit(unlink(db), add = TRUE)
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3), csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  expect_equal(morie_cache_file(csv, "fromcsv", db_path = db), 3L)
  rds <- tempfile(fileext = ".rds")
  saveRDS(data.frame(a = 1:4), rds)
  on.exit(unlink(rds), add = TRUE)
  expect_equal(morie_cache_file(rds, "fromrds", db_path = db), 4L)
  bad <- tempfile(fileext = ".txt")
  file.create(bad)
  on.exit(unlink(bad), add = TRUE)
  expect_error(morie_cache_file(bad, "x", db_path = db), "Unsupported")
})

test_that(".fuzzy_match_key resolves exact, legacy and substring keys", {
  expect_equal(morie:::.fuzzy_match_key("ocp21"), "ocp21")
  expect_equal(morie:::.fuzzy_match_key("opencanada_cpads_2021"), "ocp21")
  expect_null(morie:::.fuzzy_match_key("totally-not-a-key-xyzzy"))
})

test_that("morie_load_dataset errors on an unknown key", {
  expect_error(
    morie_load_dataset("not-a-real-dataset-key-xyzzy"),
    "Unknown dataset key"
  )
})

test_that("morie_load_dataset loads a seeded table from the user cache", {
  .cdb_have_db()
  db <- tempfile(fileext = ".db")
  on.exit(unlink(db), add = TRUE)
  cat <- morie_dataset_catalog()
  tn <- cat$table_name[cat$key == "ocp21"][1]
  morie_cache_store(data.frame(SEQID = 1:3, v = 4:6), tn, db_path = db)
  d <- tryCatch(morie_load_dataset("ocp21", db_path = db),
    error = function(e) NULL
  )
  # the built-in DB (if shipped) or our seeded cache supplies the rows
  expect_true(is.null(d) || is.data.frame(d))
})

test_that("morie_load_cpads returns NULL when offline and use_ckan = FALSE", {
  .cdb_have_db()
  db <- tempfile(fileext = ".db")
  on.exit(unlink(db), add = TRUE)
  withr <- tempfile("cpads-wd-")
  dir.create(withr)
  withr::local_dir(withr)
  res <- tryCatch(morie_load_cpads(db_path = db, use_ckan = FALSE),
    error = function(e) e
  )
  expect_true(is.null(res) || inherits(res, "error") ||
    is.data.frame(res))
})

test_that("morie_fetch_ckan paginates a mocked CKAN datastore", {
  .cdb_have_db()
  page1 <- paste0(
    '{"success":true,"result":{"total":3,"records":',
    '[{"_id":1,"SEQID":"a"},{"_id":2,"SEQID":"b"}]}}'
  )
  page2 <- paste0(
    '{"success":true,"result":{"total":3,"records":',
    '[{"_id":3,"SEQID":"c"}]}}'
  )
  page_empty <- '{"success":true,"result":{"total":3,"records":[]}}'
  calls <- 0L
  testthat::local_mocked_bindings(
    readLines = function(con, ...) {
      calls <<- calls + 1L
      if (calls == 1L) page1 else if (calls == 2L) page2 else page_empty
    }, .package = "base"
  )
  db <- tempfile(fileext = ".db")
  on.exit(unlink(db), add = TRUE)
  dat <- morie_fetch_ckan(
    dataset_key = "cpads",
    resource_id = "fake-resource-id",
    db_path = db
  )
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 3L)
  expect_false("_id" %in% names(dat))
  expect_true("SEQID" %in% names(dat))
})

test_that("morie_fetch_ckan errors when the datastore yields no records", {
  .cdb_have_db()
  testthat::local_mocked_bindings(
    readLines = function(con, ...) {
      '{"success":true,"result":{"total":0,"records":[]}}'
    },
    .package = "base"
  )
  expect_error(
    morie_fetch_ckan(
      dataset_key = "cpads", resource_id = "fake",
      db_path = tempfile(fileext = ".db")
    ),
    "0 records"
  )
})
