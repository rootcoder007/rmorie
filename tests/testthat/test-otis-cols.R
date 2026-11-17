# SPDX-License-Identifier: AGPL-3.0-or-later
# Ontario renamed every OTIS column on 2026-10-30. These tests build small
# tables in both spellings and check the analyses read either one the same.

respell <- function(d, table) {
  map <- .otis_colmap()
  map <- map[map$table == table, ]
  hit <- match(names(d), map$old)
  names(d)[!is.na(hit)] <- map$new[hit[!is.na(hit)]]
  d
}

b09_old <- function() {
  data.frame(
    EndFiscalYear = rep(2025:2026, each = 4),
    NumberPlacements_Segregation = rep(c("1", "2", "3 to 5", "Greater than 10"), 2),
    Gender = rep(c("Male", "Female"), 4),
    NumberIndividuals_Segregation = c(50, 20, 10, 3, 60, 25, 12, 4))
}

test_that("the helper resolves exact, case/underscore and renamed columns", {
  d <- data.frame(end_fiscal_year = 2026, number_of_segregation_placements = "1",
                  GENDER = "Male", NumberIndividuals_Segregation = 3)
  out <- .otis_cols(d, table = "b09")
  expect_named(out, c("EndFiscalYear", "NumberPlacements_Segregation", "Gender",
                      "NumberIndividuals_Segregation"))
  # the same new name means a different old one in b03: the expected name decides
  b03 <- .otis_cols(data.frame(number_of_segregation_placements = 1),
                    need = "Number_SegregationPlacements")
  expect_named(b03, "Number_SegregationPlacements")
  # names the table does not list are left alone
  expect_named(.otis_cols(data.frame(x = 1), table = "b09"), "x")
})

test_that("two columns for one name are refused and a missing one is named", {
  d <- data.frame(EndFiscalYear = 2025, end_fiscal_year = 2026)
  expect_error(.otis_cols(d, table = "b09"),
               "EndFiscalYear and end_fiscal_year all resolve to EndFiscalYear")
  expect_error(.otis_cols(data.frame(year = 1, n = 2), need = "EndFiscalYear", table = "b09"),
               "missing column\\(s\\): EndFiscalYear; the table has: year, n")
  expect_error(.otis_cols(list(a = 1)), "must be a data frame")
})

test_that("per-table analyzers give the same result on either spelling", {
  old <- b09_old()
  expect_identical(morie_otis_analyze_b09(respell(old, "b09")),
                   morie_otis_analyze_b09(old))
  d07 <- data.frame(Year = 2025, Alert_Type = c("Mental Health", "None"),
                    Housing_Type = c("Segregation", "General Population"),
                    Number_CustodialDeaths = c(2, 5))
  r <- morie_otis_analyze_d07(respell(d07, "d07"))
  expect_identical(r, morie_otis_analyze_d07(d07))
  expect_length(r$tables, 1L)
  # none of the columns it reads: an error, not rows and columns only
  expect_error(morie_otis_analyze_b09(data.frame(a = 1, b = 2)),
               "the OTIS b09 table: missing column\\(s\\): EndFiscalYear")
})

test_that("Mandela classification and churn read Ontario's spelling", {
  c11 <- data.frame(EndFiscalYear = rep(2026, 3),
                    Aggregate_Duration = c("1 to 15 days", "16 to 30 days", "Greater than 60 days"),
                    NumberIndividuals_RestrictiveConfinement = c(100, 20, 5),
                    NumberIndividuals_Segregation = c(80, 10, 2))
  expect_identical(morie_otis_analyze_c11_mandela_classification(respell(c11, "c11")),
                   morie_otis_analyze_c11_mandela_classification(c11))
  b01 <- morie_synth_otis("b01", n = 60L, seed = 3L)
  expect_identical(morie_otis_within_year_placement_count(respell(b01, "b01")),
                   morie_otis_within_year_placement_count(b01))
})

test_that("mrm callables read Ontario's spelling", {
  old <- b09_old()
  expect_equal(mrm_otis_placement_concentration(respell(old, "b09")),
               mrm_otis_placement_concentration(old))
  b01 <- morie_synth_otis("b01", n = 80L, seed = 4L)
  expect_equal(mrm_otis_region_locality(respell(b01, "b01")), mrm_otis_region_locality(b01))
  expect_error(mrm_otis_seg_duration_km(data.frame(x = 1)),
               "mrm_otis_seg_duration_km: missing column\\(s\\): NumberConsecutiveDays_Segregation")
})

test_that("the a01 primitives read either spelling and name the year they use", {
  a01 <- morie_synth_otis("a01", n = 120L, seed = 5L)
  a01$EndFiscalYear <- rep(c(2025L, 2026L), length.out = nrow(a01))
  expect_identical(morie_otis_rctrnd(a01), morie_otis_rctrnd(.otis_snake_names(a01)))
  expect_message(morie_otis_all_analyses(respell(a01, "a01")),
                 "fiscal year 2026 \\(the latest; the data hold 2025, 2026\\)")
  pa <- morie_otis_make_pair_a(a01)
  expect_identical(morie_otis_make_pair_a(respell(a01, "a01")), pa)
})

test_that("the CSI context uses the years both inputs hold", {
  local_mocked_bindings(morie_otis_analyze_a01 = function(data, ...)
    .otis_wrap("a01", list(), interpretation = "", payload = NULL))
  a01 <- morie_synth_otis("a01", n = 60L, seed = 6L)
  a01$EndFiscalYear <- rep(2024:2026, length.out = nrow(a01))
  cats <- MORIE_TPS_CSI_CATEGORIES()
  tps <- stats::setNames(lapply(seq_along(cats), function(i)
    data.frame(OCC_YEAR = rep(2022:2025, times = 5 * i))), cats)
  r <- morie_otis_analyze_a01_with_csi_context(respell(a01, "a01"), tps_data = tps)
  expect_identical(r$summary_lines[["-- Years in both (shown)"]], "2024, 2025")
  expect_identical(vapply(r$tables[[1]]$rows, `[`, "", 1L), c("2024", "2025"))
})
