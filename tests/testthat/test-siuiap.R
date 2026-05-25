# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tests for R/siuiap.R --- FEDERAL Structured Intervention Unit
# Implementation Advisory Panel metadata + citations.

# ---------------------------------------------------------------------------
# 1. Registry shape -- all four exported lists are non-empty named lists.
# ---------------------------------------------------------------------------

test_that("MORIE_SIUIAP_PANEL_MEMBERS lists Sapers, Doob, Sprott", {
  set.seed(1)
  m <- MORIE_SIUIAP_PANEL_MEMBERS
  expect_type(m, "list")
  expect_length(m, 3L)
  names_v <- vapply(m, function(x) x$name, character(1))
  expect_true("Howard Sapers"   %in% names_v)
  expect_true("Anthony N. Doob" %in% names_v)
  expect_true("Jane B. Sprott"  %in% names_v)
})

test_that("MORIE_SIUIAP_REPORTS has the documented entries", {
  set.seed(1)
  r <- MORIE_SIUIAP_REPORTS
  expect_type(r, "list")
  expect_true("final_2024" %in% names(r))
  expect_true("annual_2023_2024" %in% names(r))
  expect_true("preliminary_observations" %in% names(r))
  # Every entry has title + year + type + publisher.
  for (id in names(r)) {
    entry <- r[[id]]
    expect_true(!is.null(entry$title), info = id)
    expect_true(!is.null(entry$year),  info = id)
    expect_true(!is.null(entry$type),  info = id)
    expect_true(!is.null(entry$publisher), info = id)
  }
})

test_that("MORIE_SIUIAP_CRIMSL_REPORTS contains Sprott-Doob torture-solitary 2021", {
  set.seed(1)
  r <- MORIE_SIUIAP_CRIMSL_REPORTS
  expect_true("sprott_doob_torture_solitary_2021" %in% names(r))
  entry <- r$sprott_doob_torture_solitary_2021
  expect_equal(entry$year, 2021)
  expect_true(grepl("Solitary Confinement", entry$title, fixed = TRUE))
  expect_match(entry$url, "TortureSolitarySIUs")
})

test_that("MORIE_SIUIAP_AFFIDAVITS contains the T-539-20 Doob affidavit", {
  set.seed(1)
  a <- MORIE_SIUIAP_AFFIDAVITS
  expect_true("doob_t_539_20_2020" %in% names(a))
  entry <- a$doob_t_539_20_2020
  expect_equal(entry$file_no, "T-539-20")
  expect_equal(entry$court, "Federal Court of Canada")
})

test_that("MORIE_SIUIAP_ORIGINAL_PANEL_2019_2020 documents the earlier panel", {
  set.seed(1)
  p <- MORIE_SIUIAP_ORIGINAL_PANEL_2019_2020
  expect_equal(p$chair, "Anthony N. Doob")
  expect_equal(p$established, "2019")
  expect_match(p$dissolved, "2020")
})

# ---------------------------------------------------------------------------
# 2. cite()
# ---------------------------------------------------------------------------

test_that("cite('final_2024') returns a one-line authors-year-title-publisher string", {
  set.seed(1)
  s <- morie_siuiap_cite("final_2024")
  expect_type(s, "character")
  expect_length(s, 1L)
  expect_match(s, "SIU IAP")
  expect_match(s, "2024", fixed = TRUE)
  expect_match(s, "SIU IAP Final Report", fixed = TRUE)
  expect_match(s, "Public Safety Canada", fixed = TRUE)
})

test_that("cite() falls through REPORTS -> CRIMSL -> AFFIDAVITS", {
  set.seed(1)
  s_crimsl <- morie_siuiap_cite("sprott_doob_torture_solitary_2021")
  expect_match(s_crimsl, "Sprott")
  expect_match(s_crimsl, "Doob")
  expect_match(s_crimsl, "2021", fixed = TRUE)

  s_aff <- morie_siuiap_cite("doob_t_539_20_2020")
  expect_match(s_aff, "Doob")
  # Affidavit lacks a publisher -> court name surfaces instead.
  expect_match(s_aff, "Federal Court of Canada", fixed = TRUE)
})

test_that("cite() errors on an unknown report_id", {
  set.seed(1)
  expect_error(morie_siuiap_cite("totally_made_up_2099"),
               "unknown report_id",
               fixed = TRUE)
})

test_that("cite() validates input type", {
  set.seed(1)
  expect_error(morie_siuiap_cite(42))
  expect_error(morie_siuiap_cite(c("final_2024", "annual_2023_2024")))
})

# ---------------------------------------------------------------------------
# 3. panel_summary()
# ---------------------------------------------------------------------------

test_that("panel_summary names chair + members + URL + mandate dates", {
  set.seed(1)
  s <- morie_siuiap_panel_summary()
  expect_type(s, "character")
  expect_length(s, 1L)
  expect_match(s, "Howard Sapers")
  expect_match(s, "Anthony N. Doob")
  expect_match(s, "Jane B. Sprott")
  expect_match(s, "Chair")
  expect_match(s, "Member")
  expect_match(s, "2021-04", fixed = TRUE)
  expect_match(s, "2024-12", fixed = TRUE)
  expect_match(s, "publicsafety\\.gc\\.ca")
})

# ---------------------------------------------------------------------------
# 4. Exported scalars
# ---------------------------------------------------------------------------

test_that("MORIE_SIUIAP_URL is an https URL", {
  set.seed(1)
  expect_match(MORIE_SIUIAP_URL, "^https://www\\.publicsafety\\.gc\\.ca/")
})

test_that("MORIE_SIUIAP_PANEL_MANDATE mentions the 2024-12 end date", {
  set.seed(1)
  expect_match(MORIE_SIUIAP_PANEL_MANDATE, "December 31, 2024", fixed = TRUE)
})
