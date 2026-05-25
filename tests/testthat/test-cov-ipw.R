# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 4 -- R/ipw.R: the CPADS contract, validation, and the
# propensity / eBAC selection-adjusted IPW workflows. Uses the shared
# make_canonical_cpads() fixture (helper-cpads.R).

test_that("morie_cpads_contract describes the 11-variable open-data contract", {
  ct <- morie_cpads_contract()
  expect_type(ct, "list")
  expect_length(ct$required_variables, 11L)
  # 3MMM.2: CPADS PUMF is open data on open.canada.ca CKAN, not
  # local-private. source_kind reflects that.
  expect_equal(ct$source_kind, "open_data_pumf")
  expect_true(nzchar(ct$note))
})

test_that("morie_validate_cpads_data flags missing variables and respects strict", {
  d <- make_canonical_cpads(n = 200L)
  expect_length(morie_validate_cpads_data(d, strict = TRUE), 0L)
  broken <- d[, setdiff(names(d), c("ebac_tot", "gender")), drop = FALSE]
  miss <- morie_validate_cpads_data(broken, strict = FALSE)
  expect_true(all(c("ebac_tot", "gender") %in% miss))
  expect_error(
    morie_validate_cpads_data(broken, strict = TRUE),
    "missing required variables"
  )
})

test_that(".weighted_prop and .ess compute on simple inputs", {
  expect_equal(morie:::.weighted_prop(c(1, 0, 1), c(2, 1, 1)), 0.75)
  expect_equal(morie:::.ess(rep(1, 10)), 10)
  expect_lt(morie:::.ess(c(rep(1, 9), 100)), 10)
})

test_that("morie_run_propensity_ipw_analysis returns IPW tables, writes CSVs", {
  d <- make_canonical_cpads(n = 1400L, seed = 303L)
  od <- tempfile("ipw-")
  res <- suppressWarnings(morie_run_propensity_ipw_analysis(d, output_dir = od))
  expect_named(res, c("analysis_frame", "ipw_results", "diagnostics"))
  expect_equal(res$ipw_results$estimand, "ATE")
  expect_equal(nrow(res$diagnostics), 6L)
  expect_true(file.exists(file.path(od, "ipw_results.csv")))
  expect_true(file.exists(file.path(od, "ipw_diagnostics.csv")))
})

test_that("morie_run_ebac_selection_ipw_analysis errors without the survey pkg", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "survey")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_error(
    morie_run_ebac_selection_ipw_analysis(make_canonical_cpads(n = 200L)),
    "survey"
  )
})

test_that("morie_run_ebac_selection_ipw_analysis runs the selection-adjusted IPW", {
  d <- make_canonical_cpads(n = 1600L, seed = 404L)
  od <- tempfile("ebac-")
  res <- suppressWarnings(
    morie_run_ebac_selection_ipw_analysis(d, output_dir = od)
  )
  expect_named(res, c(
    "analysis_frame", "ebac_final_ipw_diagnostics",
    "ebac_final_ipw_or", "ebac_final_ipw_linear",
    "ebac_final_ipw_comparison"
  ))
  expect_equal(nrow(res$ebac_final_ipw_diagnostics), 8L)
  expect_equal(res$ebac_final_ipw_or$model, "selection_adjusted_ipw")
  expect_equal(nrow(res$ebac_final_ipw_comparison), 2L)
  expect_true(file.exists(file.path(od, "ebac_final_ipw_or.csv")))
  expect_true(file.exists(file.path(od, "ebac_final_ipw_comparison.csv")))
})
