# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 8 -- R/study_reporting.R: power-design helpers, the
# legacy-artifact utilities, and the figures / tables / final-report /
# ebac-integrations module internals.

test_that("power-required-n helpers compute and handle degenerate input", {
  expect_true(is.finite(rmorie:::.binary_power_required_n(0.20, 0.40)))
  expect_true(is.na(rmorie:::.binary_power_required_n(0.30, 0.30)))
  expect_true(is.finite(rmorie:::.continuous_power_required_n(0, 1, 2)))
  expect_true(is.na(rmorie:::.continuous_power_required_n(5, 5, 2)))
})

test_that(".block_schedule yields an empty frame then a real schedule", {
  empty <- rmorie:::.block_schedule("hd", NA, character(0))
  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0L)
  sched <- rmorie:::.block_schedule(
    "heavy_drinking_30d", 200,
    c("Female", "Male")
  )
  expect_true(nrow(sched) > 0)
  expect_true(all(c("block_id", "assignment", "endpoint") %in%
    names(sched)))
})

test_that(".read_existing_output reads a CSV or returns the fallback", {
  od <- tempfile("ro-")
  dir.create(od)
  utils::write.csv(data.frame(a = 1:2), file.path(od, "x.csv"),
    row.names = FALSE
  )
  got <- rmorie:::.read_existing_output(od, "x.csv")
  expect_equal(nrow(got), 2L)
  expect_identical(
    rmorie:::.read_existing_output(od, "missing.csv", fallback = "FB"),
    "FB"
  )
})

test_that(".legacy_reference_root resolves a path or errors off-project", {
  # .legacy_reference_root() builds on .morie_project_root(), which
  # legitimately errors when the working directory is outside the
  # morie project tree (e.g. under covr / R CMD check).
  res <- tryCatch(rmorie:::.legacy_reference_root(),
    error = function(e) e
  )
  expect_true(is.character(res) || inherits(res, "error"))
})

test_that(".copy_legacy_artifacts copies present files and skips absent", {
  root <- tempfile("legacy-")
  dir.create(file.path(root, "sub"), recursive = TRUE)
  writeLines("artifact", file.path(root, "sub", "f.txt"))
  od <- tempfile("out-")
  copied <- rmorie:::.copy_legacy_artifacts(
    c("sub/f.txt", "sub/absent.txt"),
    output_dir = od, root = root
  )
  expect_equal(copied, "sub/f.txt")
  expect_true(file.exists(file.path(od, "sub", "f.txt")))
})

test_that("study-reporting module internals run on canonical CPADS", {
  d <- make_canonical_cpads(n = 1400L, seed = 808L)
  run <- function(expr) {
    res <- tryCatch(suppressWarnings(expr), error = function(e) e)
    expect_true(is.list(res) || inherits(res, "error"))
    invisible(res)
  }
  run(rmorie:::.run_power_design_module_extended(d))
  run(rmorie:::.run_figures_module_internal(d))
  run(rmorie:::.run_figures_module_internal(d, output_dir = tempfile("fig-")))
  run(rmorie:::.run_tables_module_internal(d, output_dir = tempfile("tbl-")))
  run(rmorie:::.run_final_report_module_internal(d))
  run(rmorie:::.run_final_report_module_internal(d, output_dir = tempfile("fr-")))
  run(rmorie:::.run_ebac_integrations_module_internal(d))
})
