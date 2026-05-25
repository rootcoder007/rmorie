# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2N: tests for the empirical-cardinality-driven per-dataset
# index registry in R/db_indexes.R, plus the auto-create hook in
# morie_cache_store.

.list_indexes_sqlite <- function(con, table_name) {
  # SQLite-portable index lookup. Returns a character vector of
  # index names attached to `table_name`.
  out <- tryCatch(
    DBI::dbGetQuery(
      con,
      "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name=?",
      params = list(table_name)),
    error = function(e) data.frame(name = character(0))
  )
  out$name
}

# ---------------------------- registry sanity ----------------------------

test_that(".morie_db_index_registry contains all expected dataset keys", {
  reg <- morie:::.morie_db_index_registry()
  expected <- c("SIU", "a01", "b01", "b02", "b03", "b04", "b05",
                "b06", "b07", "b08", "b09",
                "c01", "c02", "c03", "c04", "c05", "c06", "c07",
                "c08", "c09", "c10", "c11", "c12",
                "d01", "d02", "d03", "d04", "d05", "d06", "d07",
                "uof_main_records", "uof_individual_records",
                "uof_probe_cycle_records", "uof_weapon_records")
  for (k in expected) expect_true(k %in% names(reg),
                                   info = paste("missing key:", k))
})

test_that(".morie_db_indexes_for matches OTIS short names directly", {
  expect_true(length(morie:::.morie_db_indexes_for("b01")) > 0L)
  expect_true(length(morie:::.morie_db_indexes_for("c01")) > 0L)
  expect_true(length(morie:::.morie_db_indexes_for("d01")) > 0L)
})

test_that(".morie_db_indexes_for resolves prefixed table names", {
  # "OTIS_b01" -> "b01"
  expect_true(length(morie:::.morie_db_indexes_for("OTIS_b01")) > 0L)
  expect_true(length(morie:::.morie_db_indexes_for("ARSAU_uof_main_records")) > 0L)
})

test_that(".morie_db_indexes_for dispatches TPS crime-table family by name", {
  specs <- morie:::.morie_db_indexes_for("assault")
  expect_true(length(specs) >= 4L)
  cols_indexed <- unlist(lapply(specs, function(s) s$cols))
  expect_true("OBJECTID" %in% cols_indexed)
  expect_true("HOOD_158" %in% cols_indexed)
})

test_that(".morie_db_indexes_for returns empty list for unknown tables", {
  expect_length(morie:::.morie_db_indexes_for("__unknown_table__"), 0L)
})

# ---------------------------- end-to-end create -------------------------

test_that("morie_db_create_indexes builds SIU indexes against a real SQLite db", {
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  siu <- data.frame(
    drid = 1:50,
    case_number = sprintf("23-OFD-%03d", 1:50),
    date_of_incident_iso = sprintf("2023-%02d-15", 1 + (0:49) %% 12),
    police_service = sample(c("Toronto", "OPP", "Halton"),
                             50L, replace = TRUE),
    stringsAsFactors = FALSE
  )
  n <- morie_db_create_indexes(con, "SIU_xxxxx_unknown_table")
  expect_equal(n, 0L)  # table doesn't exist yet
  morie_cache_store(siu, "SIU", con = con)
  idx <- .list_indexes_sqlite(con, "SIU")
  expect_true("idx_siu_drid" %in% idx)
  expect_true("idx_siu_case_number" %in% idx)
  expect_true("idx_siu_date_iso" %in% idx)
  expect_true("idx_siu_date_x_service" %in% idx)
})

test_that("morie_db_create_indexes silently skips specs whose cols are missing", {
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # b01 spec wants UniqueIndividual_ID + EndFiscalYear+Region_AtTimeOfPlacement
  # Supply only UniqueIndividual_ID — the year+region composite should silently skip.
  partial <- data.frame(
    UniqueIndividual_ID = sprintf("syn%04d", 1:100),
    stringsAsFactors = FALSE
  )
  morie_cache_store(partial, "b01", con = con)
  idx <- .list_indexes_sqlite(con, "b01")
  expect_true("idx_b01_uid" %in% idx)
  expect_false("idx_b01_year_x_region" %in% idx)
})

test_that("morie_db_create_indexes creates the TPS crime-table family indexes", {
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tps <- data.frame(
    OBJECTID = 1:100,
    EVENT_UNIQUE_ID = sprintf("ev-%04d", 1:100),
    HOOD_158 = sprintf("%03d", sample(1:158, 100, replace = TRUE)),
    REPORT_YEAR = sample(2014:2025, 100, replace = TRUE),
    DIVISION = sample(c("D11", "D12", "D13"), 100, replace = TRUE),
    OCC_DATE = sprintf("2023-%02d-15", 1 + (0:99) %% 12),
    LOCATION_TYPE = sample(c("Apartment", "House", "Outside"),
                            100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  morie_cache_store(tps, "assault", con = con)
  idx <- .list_indexes_sqlite(con, "assault")
  expect_true("idx_assault_objectid"  %in% idx)
  expect_true("idx_assault_event_uid" %in% idx)
  expect_true("idx_assault_hood158"   %in% idx)
  expect_true("idx_assault_year_div"  %in% idx)
  expect_true("idx_assault_occ_date"  %in% idx)
})

test_that("morie_cache_store works on tables with no registered indexes (no-op)", {
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  morie_cache_store(data.frame(x = 1:10, y = letters[1:10]),
                    "my_arbitrary_table", con = con)
  expect_equal(.list_indexes_sqlite(con, "my_arbitrary_table"),
               character(0))
  expect_equal(DBI::dbReadTable(con, "my_arbitrary_table")$x, 1:10)
})

test_that("morie_db_create_indexes is idempotent (CREATE INDEX IF NOT EXISTS)", {
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE),
              "RSQLite not installed")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  df <- data.frame(
    UniqueIndividual_ID = sprintf("syn%04d", 1:50),
    EndFiscalYear = sample(2018:2024, 50, replace = TRUE),
    Region_AtTimeOfPlacement = sample(c("Central", "Eastern", "Toronto"),
                                       50, replace = TRUE),
    stringsAsFactors = FALSE
  )
  morie_cache_store(df, "b01", con = con)
  before <- .list_indexes_sqlite(con, "b01")
  # second call should not fail nor duplicate indexes
  morie_db_create_indexes(con, "b01")
  after <- .list_indexes_sqlite(con, "b01")
  expect_equal(sort(after), sort(before))
})
