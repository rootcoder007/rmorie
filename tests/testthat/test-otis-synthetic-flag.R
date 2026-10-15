# SPDX-License-Identifier: AGPL-3.0-or-later
# An OTIS analysis on synthetic rows, or on a treatment with an empty arm, must say its
# numbers are not findings and withhold the canned interpretation.

test_that("a01 with no cached table warns and flags the 5-row synthetic sample", {
  withr::local_envvar(MORIE_CACHE_DIR = withr::local_tempdir())
  expect_warning(
    r <- morie_otis_analyze_a01_ruhela_formulations(),
    "5-row synthetic sample: its numbers are not findings"
  )
  expect_true(any(grepl("^synthetic input", r$warnings)))
  # the sample has no treated row: the empty arm is named, and no SMD is invented
  expect_true(any(grepl("0 treated and 5 control rows", r$warnings, fixed = TRUE)))
  expect_identical(r$summary_lines$`Pre-balance (covariate SMD)`,
                   "n/a (an SMD needs treated and control units)")
  expect_match(r$interpretation, "^No interpretation: synthetic input")
})

test_that("synthetic data passed explicitly is flagged in the result, without a warning", {
  a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
  r <- suppressWarnings(morie_otis_analyze_a01_ruhela_formulations(a01))
  expect_true(any(grepl("^synthetic input", r$warnings)))
  expect_match(r$interpretation, "^No interpretation:")
  expect_match(r$summary_lines$`Pre-balance (covariate SMD)`, "^max \\|SMD\\| = [0-9.]+$")
})

test_that("rows that are not synthetic keep the interpretation", {
  a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
  a01$UniqueIndividual_ID <- sub("^syn-", "id-", a01$UniqueIndividual_ID)
  r <- suppressWarnings(morie_otis_analyze_a01_ruhela_formulations(a01))
  expect_length(r$warnings, 0L)
  expect_match(r$interpretation, "^Ruhela formulation on the canonical OTIS-RC dataset \\(a01\\):")
})

test_that("the printout carries the estimate tables, not just the header", {
  a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
  r <- suppressWarnings(morie_otis_analyze_a01_ruhela_formulations(a01))
  out <- utils::capture.output(print(r))
  # every row of every table is printed: the first estimator, the SE comparison, the naive arm
  for (tab in r$tables) {
    expect_true(any(grepl(tab$headers[1L], out, fixed = TRUE)))
    for (row in tab$rows) expect_true(any(grepl(row[[1L]], out, fixed = TRUE)), info = row[[1L]])
  }
  expect_true(any(grepl("1. IPW (Hajek)", out, fixed = TRUE)))
  # the .txt that out_dir receives is the same printout
  dir <- withr::local_tempdir()
  suppressWarnings(morie_otis_analyze_a01_ruhela_formulations(a01, out_dir = dir))
  expect_true(any(grepl("IRM-DML SE comparison", readLines(file.path(dir, "otis_a01.txt")), fixed = TRUE)))
})

test_that("no result text claims a finding the data did not produce", {
  a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
  a01$UniqueIndividual_ID <- sub("^syn-", "id-", a01$UniqueIndividual_ID)
  r <- suppressWarnings(morie_otis_analyze_a01_ruhela_formulations(a01))
  txt <- c(r$interpretation, vapply(r$tables, function(t) t$title, ""))
  expect_false(any(grepl("published|notez1a|res_pool|smaller magnitude|concordance with", txt)))
})

test_that("a duration table whose bins the classifier does not know says so", {
  b05 <- data.frame(EndFiscalYear = c(2024, 2024), Consecutive_Duration = c("1 to 7 days", "Over 60 days"),
                    Number_SegregationPlacements = c(10, 3))
  r <- morie_otis_analyze_b05_mandela_classification(b05)
  expect_match(r$warnings, "no Consecutive_Duration value matches a known OTIS duration bin")
  expect_match(r$warnings, "1 to 7 days, Over 60 days", fixed = TRUE)
  b05$Consecutive_Duration[1] <- "1 day"
  r <- morie_otis_analyze_b05_mandela_classification(b05)
  expect_match(r$warnings, "values outside the known OTIS duration bins were left out: Over 60 days", fixed = TRUE)
})
