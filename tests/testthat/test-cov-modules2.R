# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 7 -- R/modules.R: CPADS-CSV resolution, canonicalization,
# module-output writing, and the morie_run_morie_module(s) dispatch.

test_that(".cpads_default_csv returns a CPADS csv path", {
  p <- morie:::.cpads_default_csv()
  expect_type(p, "character")
  expect_match(p, "cpads", ignore.case = TRUE)
})

test_that(".resolve_cpads_csv resolves an existing file, errors otherwise", {
  f <- tempfile(fileext = ".csv")
  file.create(f)
  on.exit(unlink(f), add = TRUE)
  expect_equal(morie:::.resolve_cpads_csv(f), normalizePath(f))
  expect_error(
    morie:::.resolve_cpads_csv("no-such-cpads-xyzzy.csv"),
    "not found"
  )
})

test_that("morie_canonicalize_cpads_data passes through already-canonical data", {
  out <- morie_canonicalize_cpads_data(make_canonical_cpads(n = 120L))
  expect_s3_class(out, "data.frame")
  expect_true("ebac_tot" %in% names(out))
})

test_that("morie_canonicalize_cpads_data maps raw CPADS columns", {
  out <- morie_canonicalize_cpads_data(make_raw_cpads(n = 300L))
  expect_true(all(c(
    "weight", "alcohol_past12m", "cannabis_any_use",
    "heavy_drinking_30d"
  ) %in% names(out)))
})

test_that("morie_load_cpads_data reads and canonicalizes a raw CPADS csv", {
  csv <- tempfile("rawcpads-", fileext = ".csv")
  utils::write.csv(make_raw_cpads(n = 400L), csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  d <- morie_load_cpads_data(csv)
  expect_s3_class(d, "data.frame")
  expect_true("heavy_drinking_30d" %in% names(d))
})

test_that(".write_module_outputs: no dir returns as-is; a dir writes files", {
  outs <- list(
    tbl = data.frame(a = 1:2),
    note = "a one-line note",
    `pre.csv` = data.frame(z = 1)
  )
  expect_identical(morie:::.write_module_outputs(outs, NULL), outs)
  od <- tempfile("mods-")
  morie:::.write_module_outputs(outs, od)
  expect_true(file.exists(file.path(od, "tbl.csv")))
  expect_true(file.exists(file.path(od, "note.txt")))
  expect_true(file.exists(file.path(od, "pre.csv")))
})

test_that("morie_run_morie_module errors on an unknown module name", {
  csv <- tempfile("rawcpads-", fileext = ".csv")
  utils::write.csv(make_raw_cpads(n = 200L), csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  expect_error(
    suppressWarnings(morie_run_morie_module("not-a-real-module", cpads_csv = csv)),
    "Unknown module"
  )
})

test_that("morie_run_morie_modules runs a set of in-memory-safe modules", {
  csv <- tempfile("rawcpads-", fileext = ".csv")
  utils::write.csv(make_raw_cpads(n = 600L), csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)
  res <- suppressWarnings(morie_run_morie_modules(
    modules = c("descriptive-statistics", "distribution-tests"),
    cpads_csv = csv
  ))
  expect_named(res, c("descriptive-statistics", "distribution-tests"))
  expect_true(is.list(res))
})
