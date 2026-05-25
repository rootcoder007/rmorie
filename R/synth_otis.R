# SPDX-License-Identifier: AGPL-3.0-or-later
#
# *** AUTO-GENERATED FILE -- do not hand-edit. ***
#
# Regenerate by running:
#     python3 data-raw/gen-synth-otis.py
# (the historical generator at data-raw/gen-helper-otis.py wrote the
# same content into tests/testthat/helper-otis.R; this file is the
# package-level home so morie_synth_otis() is part of the public API.)
#
# Source of truth: inst/extdata/otis_dictionary.json (extracted from
# the Ontario MCSCS XLSX dictionary
# od-restrictiveconfinement-segregation-deaths-dd20251103-datadictionary.xlsx).
#
# Per-dataset OTIS synthetic-fixture dispatcher. Each OTIS publication
# id has its own column schema + categorical-level vocabulary; feeding
# a one-size-fits-all panel to every analyzer triggered silent
# tryCatch+skip bail-outs and left ~50% of otis_all_analyze.R unreached.
# This helper draws every column name + every categorical level from
# the authoritative dictionary.

.morie_otis_a01_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    UniqueIndividual_ID = sprintf("syn-%05d", sample.int(max(2L, n %/% 4L), n, replace = TRUE)),
    Region_AtTimeOfPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Region_MostRecentPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    Age_Category = sample(c("18 to 24", "25 to 49", "50+"), n, replace = TRUE),
    MentalHealth_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    SuicideRisk_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    SuicideWatch_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    Number_Of_Placements = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_b01_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    UniqueIndividual_ID = sprintf("syn-%05d", sample.int(max(2L, n %/% 4L), n, replace = TRUE)),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    Region_AtTimeOfPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Region_MostRecentPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Age_Category = sample(c("18 to 24", "25 to 49", "50+"), n, replace = TRUE),
    NumberConsecutiveDays_Segregation = sample(0:80, n, replace = TRUE),
    SegReason_SecurityOfInstitution_SafetyOfOthers = sample(c("Yes", "No"), n, replace = TRUE),
    SegReason_InmateNeedsProtection = sample(c("Yes", "No"), n, replace = TRUE),
    SegReason_InmateNeedsProtection_Medical = sample(c("Yes", "No"), n, replace = TRUE),
    SegReason_SecurityOfInstitution_SafetyOfOthers_Medical = sample(c("Yes", "No"), n, replace = TRUE),
    SegReason_Disciplinary_Segregation = sample(c("Yes", "No"), n, replace = TRUE),
    SegReason_InmateRefuseSearch_Scan = sample(c("Yes", "No"), n, replace = TRUE),
    MentalHealth_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    SuicideRisk_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    SuicideWatch_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    SegReason_Other = sample(c("Yes", "No"), n, replace = TRUE),
    Number_Of_Placements = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_b02_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    UniqueIndividual_ID = sprintf("syn-%05d", sample.int(max(2L, n %/% 4L), n, replace = TRUE)),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    Region_MostRecentPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Age_Category = sample(c("18 to 24", "25 to 49", "50+"), n, replace = TRUE),
    TotalAggregatedDays_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_b03_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_AtTimeOfPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Institution_AtTimeOfPlacement = sample(c("Algoma Treatment and Remand Centre", "Brockville Jail", "Central East Correctional Centre", "Central North Correctional Centre", "Elgin-Middlesex Detention Centre", "Fort Frances Jail", "Hamilton Wentworth Detention Centre", "Kenora Jail", "Maplehurst Correctional Complex", "Monteith Correctional Complex", "Niagara Detention Centre", "North Bay Jail", "Ontario Correctional Institute", "Ottawa-Carleton Detention Centre", "Quinte Detention Centre", "Sarnia Jail", "South West Detention Centre", "St. Lawrence Valley Correctional and Treatment Centre", "Stratford Jail", "Sudbury Jail", "Thunder Bay Correctional Centre", "Thunder Bay Jail", "Toronto East Detention Centre", "Toronto South Detention Centre", "Vanier Centre for Women"), n, replace = TRUE),
    Alert_Type = sample(c("Immigration Hold Flag", "Mental Health Alert", "Serious Mental Illness Alert", "Suicide Risk Alert", "Suicide Watch Alert", "Transgender Alert"), n, replace = TRUE),
    Alert_Presence = sample(c("Yes", "No"), n, replace = TRUE),
    Number_SegregationPlacements = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_b04_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_AtTimeOfPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Gender = sample(c("Female", "Male", "All"), n, replace = TRUE),
    Measure = sample(c("Maximum", "Median", "Mode"), n, replace = TRUE),
    NumberConsecutiveDays_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_b05_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Consecutive_Duration = sample(c("1 day", "2 days", "3 days", "4 days", "5 days", "6 to 10 days", "11 to 15 days", "16 to 20 days", "21 to 25 days", "26 to 30 days", "Greater than 30 days"), n, replace = TRUE),
    Number_SegregationPlacements = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_b06_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_AtTimeOfPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Institution_AtTimeOfPlacement = sample(c("Algoma Treatment and Remand Centre", "Brockville Jail", "Central East Correctional Centre", "Central North Correctional Centre", "Elgin-Middlesex Detention Centre", "Fort Frances Jail", "Hamilton Wentworth Detention Centre", "Kenora Jail", "Maplehurst Correctional Complex", "Monteith Correctional Complex", "Niagara Detention Centre", "North Bay Jail", "Ontario Correctional Institute", "Ottawa-Carleton Detention Centre", "Quinte Detention Centre", "Sarnia Jail", "South West Detention Centre", "St. Lawrence Valley Correctional and Treatment Centre", "Stratford Jail", "Sudbury Jail", "Thunder Bay Correctional Centre", "Thunder Bay Jail", "Toronto East Detention Centre", "Toronto South Detention Centre", "Vanier Centre for Women"), n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    Reason = sample(c("Close Confinement (Disciplinary Segregation)", "Inmate Needs Protection", "Inmate Needs Protection: Medical", "Inmate Refused Search/Scan", "Security of Institution/Safety of Others", "Security of Institution/Safety of Others: Medical", "Other"), n, replace = TRUE),
    Number_SegregationPlacements = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_b07_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Alert_Type = sample(c("Immigration Hold Flag", "Mental Health Alert", "Serious Mental Illness Alert", "Suicide Risk Alert", "Suicide Watch Alert", "Transgender Alert"), n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    Number_Segregation_Placements_Without_Alert = sample(0:80, n, replace = TRUE),
    Number_Segregation_Placements_With_Alert = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_b08_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_AtTimeOfPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Institution_AtTimeOfPlacement = sample(c("Algoma Treatment and Remand Centre", "Brockville Jail", "Central East Correctional Centre", "Central North Correctional Centre", "Elgin-Middlesex Detention Centre", "Fort Frances Jail", "Hamilton Wentworth Detention Centre", "Kenora Jail", "Maplehurst Correctional Complex", "Monteith Correctional Complex", "Niagara Detention Centre", "North Bay Jail", "Ontario Correctional Institute", "Ottawa-Carleton Detention Centre", "Quinte Detention Centre", "Sarnia Jail", "South West Detention Centre", "St. Lawrence Valley Correctional and Treatment Centre", "Stratford Jail", "Sudbury Jail", "Thunder Bay Correctional Centre", "Thunder Bay Jail", "Toronto East Detention Centre", "Toronto South Detention Centre", "Vanier Centre for Women"), n, replace = TRUE),
    Gender = sample(c("Female", "Male", "All"), n, replace = TRUE),
    Measure = sample(c("Median", "Mode"), n, replace = TRUE),
    NumberConsecutiveDays_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_b09_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    NumberPlacements_Segregation = sample(c("1 placement", "2 placements", "3 placements", "4 placements", "5 placements", "6 to 10 placements", "11 to 15 placements", "16 to 20 placements", "21 to 25 placements", "26 to 30 placements", "31 to 35 placements", "36 to 40 placements", "more than 40 placements"), n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c01_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    NumberIndividuals_InCustody = sample(0:80, n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c02_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_MostRecentPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Institution_MostRecentPlacement = sample(c("Algoma Treatment and Remand Centre", "Brockville Jail", "Central East Correctional Centre", "Central North Correctional Centre", "Elgin-Middlesex Detention Centre", "Fort Frances Jail", "Hamilton Wentworth Detention Centre", "Kenora Jail", "Maplehurst Correctional Complex", "Monteith Correctional Complex", "Niagara Detention Centre", "North Bay Jail", "Ontario Correctional Institute", "Ottawa-Carleton Detention Centre", "Quinte Detention Centre", "Sarnia Jail", "South West Detention Centre", "St. Lawrence Valley Correctional and Treatment Centre", "Stratford Jail", "Sudbury Jail", "Thunder Bay Correctional Centre", "Thunder Bay Jail", "Toronto East Detention Centre", "Toronto South Detention Centre", "Vanier Centre for Women"), n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c03_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Race = sample(c("Another Race Category", "Black", "East Asian", "Indigenous", "Latino", "Middle Eastern", "More Than One Reported Race Category", "South Asian", "Unknown Or Not Reported", "White"), n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    NumberIndividuals_InCustody = sample(0:80, n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c04_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_MostRecentPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Race = sample(c("Another Race Category", "Black", "East Asian", "Indigenous", "Latino", "Middle Eastern", "More Than One Reported Race Category", "South Asian", "Unknown Or Not Reported", "White"), n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c05_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_MostRecentPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Religion = sample(c("Another Religion/Spiritual Affiliation", "Buddhist", "Christian", "Hindu", "Indigenous Spirituality", "Jewish", "More Than One Reported Religion or Spiritual Affiliation", "Muslim", "No Religion", "Sikh", "Unknown Or Not Reported"), n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c06_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_MostRecentPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Age_Category = sample(c("18 to 24", "25 to 49", "50+"), n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c07_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Alert_Type = sample(c("Immigration Hold Flag", "Mental Health Alert", "Serious Mental Illness Alert", "Suicide Risk Alert", "Suicide Watch Alert", "Transgender Alert"), n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    NumberIndividuals_InCustody = sample(0:80, n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c08_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Religion = sample(c("Another Religion/Spiritual Affiliation", "Buddhist", "Christian", "Hindu", "Indigenous Spirituality", "Jewish", "More Than One Reported Religion or Spiritual Affiliation", "Muslim", "No Religion", "Sikh", "Unknown Or Not Reported"), n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    NumberIndividuals_InCustody = sample(0:80, n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c09_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Age_Category = sample(c("18 to 24", "25 to 49", "50+"), n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    NumberIndividuals_InCustody = sample(0:80, n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c10_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_MostRecentPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Institution_MostRecentPlacement = sample(c("Algoma Treatment and Remand Centre", "Brockville Jail", "Central East Correctional Centre", "Central North Correctional Centre", "Elgin-Middlesex Detention Centre", "Fort Frances Jail", "Hamilton Wentworth Detention Centre", "Kenora Jail", "Maplehurst Correctional Complex", "Monteith Correctional Complex", "Niagara Detention Centre", "North Bay Jail", "Ontario Correctional Institute", "Ottawa-Carleton Detention Centre", "Quinte Detention Centre", "Sarnia Jail", "South West Detention Centre", "St. Lawrence Valley Correctional and Treatment Centre", "Stratford Jail", "Sudbury Jail", "Thunder Bay Correctional Centre", "Thunder Bay Jail", "Toronto East Detention Centre", "Toronto South Detention Centre", "Vanier Centre for Women"), n, replace = TRUE),
    Gender = sample(c("Female", "Male", "All"), n, replace = TRUE),
    Measure = sample(c("Maximum", "Median", "Mode"), n, replace = TRUE),
    TotalAggregatedDays_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    TotalAggregatedDays_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c11_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Aggregate_Duration = sample(c("1 day", "2 days", "3 days", "4 days", "5 days", "6 to 10 days", "11 to 15 days", "16 to 20 days", "21 to 25 days", "26 to 30 days", "Greater than 30 days"), n, replace = TRUE),
    NumberIndividuals_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    NumberIndividuals_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_c12_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    EndFiscalYear = sample(2018:2024, n, replace = TRUE),
    Region_MostRecentPlacement = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    Gender = sample(c("Female", "Male", "All"), n, replace = TRUE),
    Measure = sample(c("Maximum", "Median", "Mode"), n, replace = TRUE),
    TotalAggregatedDays_RestrictiveConfinement = sample(0:80, n, replace = TRUE),
    TotalAggregatedDays_Segregation = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_d01_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    Year = sample(2018:2024, n, replace = TRUE),
    UniqueIndividual_ID = sprintf("syn-%05d", sample.int(max(2L, n %/% 4L), n, replace = TRUE)),
    Region_AtTimeOfDeath = sample(c("Central", "Eastern", "Northern", "Toronto", "Western"), n, replace = TRUE),
    HousingUnit_Type = sample(c("General Population", "Segregation", "Specialized Care", "Protective Custody Outside of a Facility"), n, replace = TRUE),
    MedicalCauseOfDeath = sample(c("Drug Toxicity", "Natural Causes", "Other", "Unknown / To Be Determined"), n, replace = TRUE),
    MeansOfDeath = sample(c("Accident", "Homicide", "Natural Causes", "Suicide", "Undetermined", "To Be Determined"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_d02_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    Year = sample(2018:2024, n, replace = TRUE),
    Gender = sample(c("Female", "Male"), n, replace = TRUE),
    Number_CustodialDeaths = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_d03_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    Year = sample(2018:2024, n, replace = TRUE),
    Race = sample(c("Another Race Category Black", "East Asian", "Indigenous", "Latino", "Middle Eastern", "More Than One Reported Race Category", "South Asian", "Unknown Or Not Reported", "White"), n, replace = TRUE),
    Number_CustodialDeaths = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_d04_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    Year = sample(2018:2024, n, replace = TRUE),
    Religion = sample(c("Another Religion/Spiritual Affiliation", "Buddhist", "Christian", "Hindu", "Indigenous Spirituality", "Jewish", "Muslim", "More Than One Reported Religion or Spiritual Affiliation", "No Religion", "Sikh", "Unknown Or Not Reported"), n, replace = TRUE),
    Number_CustodialDeaths = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_d05_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    Year = sample(2018:2024, n, replace = TRUE),
    Age_Category = sample(c("18 to 24", "25 to 49", "50+"), n, replace = TRUE),
    Number_CustodialDeaths = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_d06_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    Year = sample(2018:2024, n, replace = TRUE),
    Alert_Type = sample(c("Mental Health Alert", "Suicide Risk Alert", "Suicide Watch Alert"), n, replace = TRUE),
    MedicalCauseOfDeath = sample(c("Drug Toxicity", "Natural Causes", "Other", "Unknown / To Be Determined"), n, replace = TRUE),
    Number_CustodialDeaths = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.morie_otis_d07_panel <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    Year = sample(2018:2024, n, replace = TRUE),
    Alert_Type = sample(c("Mental Health Alert", "Suicide Risk Alert", "Suicide Watch Alert"), n, replace = TRUE),
    HousingUnit_Type = sample(c("General Population / Protective Custody Unit", "Outside of a Facility", "Segregation Conditions", "Specialized Care Unit"), n, replace = TRUE),
    Number_CustodialDeaths = sample(0:80, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# a01 mirrors b01 (person-level placements) per Python parity. The
# dictionary lists a01 separately, but if the analyzer expects
# b01-style columns, the dispatcher falls through to b01 below when
# the requested id has no dedicated panel.

#' Build a synthetic OTIS data.frame for a given publication id.
#'
#' Returns a data.frame mirroring the column shape + categorical level
#' set of the published OTIS dataset for the given \code{id} (a01,
#' b01..b09, c01..c12, or d01..d07). Schema is derived from the
#' Ontario MCSCS XLSX data dictionary that ships at
#' \code{inst/extdata/otis_dictionary.json}. Values are randomly drawn
#' from the dictionary categorical levels (or 0..80 for count
#' columns); used only for offline-no-fixture fallback in tests and
#' demos. NOT a substitute for the real OTIS data published at
#' \url{https://data.ontario.ca/dataset/data-on-inmates-in-ontario}.
#'
#' @param id Character; one of \code{"a01"}, \code{"b01"}..\code{"b09"},
#'   \code{"c01"}..\code{"c12"}, or \code{"d01"}..\code{"d07"}.
#'   Unknown ids fall through to the b01 person-level panel.
#' @param n Integer; number of synthetic rows. Default 200.
#' @param seed Integer; RNG seed for reproducibility. Default 1.
#' @return A \code{data.frame} with the dictionary-derived schema.
#' @seealso \code{\link{morie_synth_otis_all}} for the full 29-dataset
#'   list; \code{\link{morie_datasets_otis_a01}} and friends for the
#'   real bundled+live loaders.
#' @export
#' @examples
#' df <- morie_synth_otis("c11", n = 50)
#' head(df)
morie_synth_otis <- function(id, n = 200L, seed = 1L) {
  helper_name <- paste0(".morie_otis_", id, "_panel")
  ns <- asNamespace("morie")
  if (!exists(helper_name, envir = ns, inherits = FALSE)) {
    return(.morie_otis_b01_panel(n = n, seed = seed))
  }
  fn <- get(helper_name, envir = ns, inherits = FALSE)
  fn(n = n, seed = seed)
}

#' Build the full 29-dataset OTIS synthetic list.
#'
#' Returns a named list of synthetic data.frames keyed by OTIS
#' publication id (a01, b01..b09, c01..c12, d01..d07). Each frame is
#' built by \code{\link{morie_synth_otis}} with a per-id seed offset
#' for reproducibility.
#'
#' @param n Integer; rows per dataset. Default 80.
#' @param seed Integer; base RNG seed. Default 2.
#' @return Named list of 29 data.frames.
#' @seealso \code{\link{morie_synth_otis}}.
#' @export
#' @examples
#' all_otis <- morie_synth_otis_all(n = 30)
#' names(all_otis)
morie_synth_otis_all <- function(n = 80L, seed = 2L) {
  ids <- c("a01",
           paste0("b0", 1:9),
           paste0("c0", 1:9), "c10", "c11", "c12",
           paste0("d0", 1:7))
  stats::setNames(
    lapply(seq_along(ids), function(i)
      morie_synth_otis(ids[i], n = n, seed = seed + i)),
    ids
  )
}
