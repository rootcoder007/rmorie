# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 20 -- R/database.R resolution tiers: the built-in-DB
# tier and the local-file tier of morie_load_dataset(), and the
# local-file / cache branches of morie_load_cpads().

.cw20_db <- function() {
}

test_that("morie_load_dataset reads from the built-in database tier", {
  skip_if_not_installed("DBI")
  .cw20_db()
  bdb <- tempfile(fileext = ".db")
  on.exit(unlink(bdb), add = TRUE)
  con <- morie_db_connect(db_path = bdb)
  cat <- morie_dataset_catalog()
  tn <- cat$table_name[cat$key == "ocp21"][1]
  DBI::dbWriteTable(con, tn, data.frame(SEQID = 1:4), overwrite = TRUE)
  DBI::dbDisconnect(con)
  testthat::local_mocked_bindings(
    morie_builtin_db = function() bdb,
    .package = "morie"
  )
  d <- morie_load_dataset("ocp21", db_path = tempfile(fileext = ".db"))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 4L)
})

test_that("morie_load_dataset ingests csv and rds local files", {
  .cw20_db()
  # an empty built-in DB so the local-file tier is reached
  testthat::local_mocked_bindings(
    morie_builtin_db = function() tempfile(fileext = ".db"),
    .package = "morie"
  )
  wd <- tempfile("ld-")
  dir.create(wd)
  withr::local_dir(wd)
  cat <- morie_dataset_catalog()

  lp <- cat$local_path[cat$key == "ocp21"][1]
  dir.create(dirname(lp), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(SEQID = 1:6), lp, row.names = FALSE)
  dcsv <- morie_load_dataset("ocp21", db_path = tempfile(fileext = ".db"))
  expect_equal(nrow(dcsv), 6L)

  lp2 <- cat$local_path[cat$key == "otisexp"][1]
  dir.create(dirname(lp2), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data.frame(a = 1:3), lp2)
  drds <- morie_load_dataset("otisexp", db_path = tempfile(fileext = ".db"))
  expect_equal(nrow(drds), 3L)
})

test_that("morie_load_cpads resolves local file then the SQLite cache", {
  .cw20_db()
  wd <- tempfile("cp-")
  dir.create(wd)
  withr::local_dir(wd)
  db <- tempfile(fileext = ".db")
  lp <- "data/datasets/oc/CPADS/2021-2022/cpads-2021-2022-pumf2.csv"
  dir.create(dirname(lp), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(SEQID = 1:5, x = 6:10), lp,
    row.names = FALSE
  )
  d <- suppressMessages(morie_load_cpads(db_path = db, use_ckan = FALSE))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 5L)
  # remove the local file -> the second call resolves from the cache
  file.remove(lp)
  d2 <- suppressMessages(morie_load_cpads(db_path = db, use_ckan = FALSE))
  expect_s3_class(d2, "data.frame")
  expect_equal(nrow(d2), 5L)
})
