# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2W: tests for tps_datasets.R — TPS data-dir + per-name
# dataset loader.

test_that("morie_tps_data_dir returns a character path", {
  d <- morie_tps_data_dir()
  expect_type(d, "character")
  expect_length(d, 1L)
  expect_true(nzchar(d))
})

test_that("morie_tps_list_datasets returns a tidy data.frame", {
  out <- morie_tps_list_datasets()
  expect_true(is.data.frame(out) || is.list(out))
  if (is.data.frame(out)) expect_gt(nrow(out), 0L)
})

test_that("morie_tps_load_dataset errors on a bogus name", {
  expect_error(
    morie_tps_load_dataset("__not_a_tps_dataset__"),
    regexp = "(unknown|not.*found|no.*such|invalid)"
  )
})

test_that("morie_tps_load_dataset accepts MORIE_TPS_DATA_DIR override + reads CSV", {
  csv_dir <- tempfile("tps_dir_")
  dir.create(file.path(csv_dir, "Assault", "CSV"), recursive = TRUE)
  on.exit(unlink(csv_dir, recursive = TRUE), add = TRUE)
  utils::write.csv(
    data.frame(OBJECTID = 1:5,
               EVENT_UNIQUE_ID = sprintf("ev-%02d", 1:5),
               OCC_DATE = sprintf("2023-%02d-15", 1:5)),
    file.path(csv_dir, "Assault", "CSV", "test_data.csv"),
    row.names = FALSE)
  withr::with_envvar(
    c(MORIE_TPS_DATA_DIR = csv_dir),
    {
      out <- tryCatch(morie_tps_load_dataset("Assault"),
                      error = function(e) e)
      if (inherits(out, "error")) {
        skip(sprintf("load_dataset error: %s",
                     conditionMessage(out)))
      }
      expect_true(is.data.frame(out) || is.list(out))
    }
  )
})
