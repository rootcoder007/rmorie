# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3JJ: OTIS b01-b09 Segregation family + c01-c12 Individuals-in-
# Segregation+RC family (21 lookup-pending wrappers).

# =================================================== b01-b09 offline fixtures

test_that("OTIS b01 segregation-detailed loads canonical 18-col schema", {
  df <- morie_datasets_otis_b01_segregation_detailed(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 18L)
  for (col in c("EndFiscalYear", "UniqueIndividual_ID", "Gender",
                "Region_AtTimeOfPlacement",
                "NumberConsecutiveDays_Segregation",
                "MentalHealth_Alert", "SuicideRisk_Alert",
                "Number_Of_Placements"))
    expect_true(col %in% names(df))
})

test_that("OTIS b02 segregation-total-days loads 6-col schema", {
  df <- morie_datasets_otis_b02_segregation_total_days(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 6L)
  expect_true("TotalAggregatedDays_Segregation" %in% names(df))
})

test_that("OTIS b03 alerts-by-institution loads 6-col schema", {
  df <- morie_datasets_otis_b03_seg_alerts_by_institution(offline = TRUE)
  expect_equal(ncol(df), 6L)
  for (col in c("Institution_AtTimeOfPlacement", "Alert_Type",
                "Alert_Presence", "Number_SegregationPlacements"))
    expect_true(col %in% names(df))
})

test_that("OTIS b04 consecutive-by-region loads 5-col schema", {
  df <- morie_datasets_otis_b04_seg_consecutive_by_region(offline = TRUE)
  expect_equal(ncol(df), 5L)
  expect_true("Measure" %in% names(df))
})

test_that("OTIS b05 consecutive-lengths loads 3-col schema", {
  df <- morie_datasets_otis_b05_seg_consecutive_lengths(offline = TRUE)
  expect_equal(ncol(df), 3L)
  expect_true("Consecutive_Duration" %in% names(df))
})

test_that("OTIS b06 reason-by-institution loads 6-col schema", {
  df <- morie_datasets_otis_b06_seg_reason_by_institution(offline = TRUE)
  expect_equal(ncol(df), 6L)
  expect_true("Reason" %in% names(df))
})

test_that("OTIS b07 alerts-by-gender loads 5-col schema", {
  df <- morie_datasets_otis_b07_seg_alerts_by_gender(offline = TRUE)
  expect_equal(ncol(df), 5L)
  for (col in c("Number_Segregation_Placements_Without_Alert",
                "Number_Segregation_Placements_With_Alert"))
    expect_true(col %in% names(df))
})

test_that("OTIS b08 consecutive-by-institution loads 6-col schema", {
  df <- morie_datasets_otis_b08_seg_consecutive_by_institution(offline = TRUE)
  expect_equal(ncol(df), 6L)
  expect_true("Institution_AtTimeOfPlacement" %in% names(df))
})

test_that("OTIS b09 number-of-times loads 4-col schema", {
  df <- morie_datasets_otis_b09_seg_n_times(offline = TRUE)
  expect_equal(ncol(df), 4L)
  expect_true("NumberPlacements_Segregation" %in% names(df))
})

# =================================================== c01-c12 offline fixtures

test_that("OTIS c01 individuals-total loads 5-col schema with all-three counts", {
  df <- morie_datasets_otis_c01_individuals_total(offline = TRUE)
  expect_equal(ncol(df), 5L)
  for (col in c("NumberIndividuals_InCustody",
                "NumberIndividuals_RestrictiveConfinement",
                "NumberIndividuals_Segregation"))
    expect_true(col %in% names(df))
})

test_that("OTIS c02 by-institution loads 6-col schema", {
  df <- morie_datasets_otis_c02_individuals_by_institution(offline = TRUE)
  expect_equal(ncol(df), 6L)
  expect_true("Institution_MostRecentPlacement" %in% names(df))
})

test_that("OTIS c03 race-by-gender loads 6-col schema", {
  df <- morie_datasets_otis_c03_individuals_race_by_gender(offline = TRUE)
  expect_equal(ncol(df), 6L)
  for (col in c("Race", "Gender")) expect_true(col %in% names(df))
})

test_that("OTIS c04 race-by-region loads 5-col schema", {
  df <- morie_datasets_otis_c04_individuals_race_by_region(offline = TRUE)
  expect_equal(ncol(df), 5L)
})

test_that("OTIS c05 religion-by-region loads 5-col schema", {
  df <- morie_datasets_otis_c05_individuals_religion_by_region(offline = TRUE)
  expect_equal(ncol(df), 5L)
  expect_true("Religion" %in% names(df))
})

test_that("OTIS c06 age-by-region loads 5-col schema", {
  df <- morie_datasets_otis_c06_individuals_age_by_region(offline = TRUE)
  expect_equal(ncol(df), 5L)
  expect_true("Age_Category" %in% names(df))
})

test_that("OTIS c07 alerts loads 6-col schema", {
  df <- morie_datasets_otis_c07_individuals_alerts(offline = TRUE)
  expect_equal(ncol(df), 6L)
  expect_true("Alert_Type" %in% names(df))
})

test_that("OTIS c08 religion-by-gender loads 6-col schema", {
  df <- morie_datasets_otis_c08_individuals_religion_by_gender(offline = TRUE)
  expect_equal(ncol(df), 6L)
})

test_that("OTIS c09 age-by-gender loads 6-col schema", {
  df <- morie_datasets_otis_c09_individuals_age_by_gender(offline = TRUE)
  expect_equal(ncol(df), 6L)
})

test_that("OTIS c10 aggregate-by-institution loads 7-col schema", {
  df <- morie_datasets_otis_c10_aggregate_durations_by_institution(
    offline = TRUE)
  expect_equal(ncol(df), 7L)
  for (col in c("TotalAggregatedDays_RestrictiveConfinement",
                "TotalAggregatedDays_Segregation"))
    expect_true(col %in% names(df))
})

test_that("OTIS c11 aggregate-lengths loads 4-col schema", {
  df <- morie_datasets_otis_c11_aggregate_lengths(offline = TRUE)
  expect_equal(ncol(df), 4L)
  expect_true("Aggregate_Duration" %in% names(df))
})

test_that("OTIS c12 aggregate-by-region loads 6-col schema", {
  df <- morie_datasets_otis_c12_aggregate_durations_by_region(offline = TRUE)
  expect_equal(ncol(df), 6L)
})

# =================================================== auto-resolve via registry (post-3KK)

test_that("every b/c wrapper auto-resolves canonical resource_id from the registry", {
  # Map: wrapper -> expected wired CKAN resource_id.
  expected <- list(
    list(fn = morie_datasets_otis_b01_segregation_detailed,
         rid = "406e6d90-d568-4553-8ca7-bc9f90e133b9"),
    list(fn = morie_datasets_otis_b02_segregation_total_days,
         rid = "84161f23-ee75-48b4-97df-3b19b8bbd745"),
    list(fn = morie_datasets_otis_b03_seg_alerts_by_institution,
         rid = "ef45902a-b946-49fe-8c2f-f778e8357e1f"),
    list(fn = morie_datasets_otis_b04_seg_consecutive_by_region,
         rid = "d76d8f65-6318-4a45-b4c7-5b9a2d985408"),
    list(fn = morie_datasets_otis_b05_seg_consecutive_lengths,
         rid = "754e8cde-5c74-4c0a-9782-79767a2b26b0"),
    list(fn = morie_datasets_otis_b06_seg_reason_by_institution,
         rid = "af633c35-2f98-4ca6-8629-02aa8acd237a"),
    list(fn = morie_datasets_otis_b07_seg_alerts_by_gender,
         rid = "38090dad-9f73-4a0b-8a7b-ca2477fc0030"),
    list(fn = morie_datasets_otis_b08_seg_consecutive_by_institution,
         rid = "73c77cf2-faeb-4136-a897-ed4d4c19e240"),
    list(fn = morie_datasets_otis_b09_seg_n_times,
         rid = "df24e943-d52b-43a8-a10e-a3cc906e26bb"),
    list(fn = morie_datasets_otis_c01_individuals_total,
         rid = "81bc03cc-b3f6-4983-b717-11f85fa90330"),
    list(fn = morie_datasets_otis_c02_individuals_by_institution,
         rid = "cb4ed82f-c67a-430a-9cb5-5e5698a06ddf"),
    list(fn = morie_datasets_otis_c03_individuals_race_by_gender,
         rid = "0532a199-3a4f-45b4-b79b-6db7920ff7f2"),
    list(fn = morie_datasets_otis_c04_individuals_race_by_region,
         rid = "b38f754f-9141-4ea3-a10c-a071473ed00a"),
    list(fn = morie_datasets_otis_c05_individuals_religion_by_region,
         rid = "c899bf66-bd7b-4305-8ccc-ae031e041df8"),
    list(fn = morie_datasets_otis_c06_individuals_age_by_region,
         rid = "4e4c91e9-ae29-4ab6-864e-bcda896c7882"),
    list(fn = morie_datasets_otis_c07_individuals_alerts,
         rid = "879cf325-a7a7-4d48-bc69-ea050d8a4d4e"),
    list(fn = morie_datasets_otis_c08_individuals_religion_by_gender,
         rid = "8fc09ce2-5097-4a94-af29-abcdcd5aa015"),
    list(fn = morie_datasets_otis_c09_individuals_age_by_gender,
         rid = "0e990da6-5427-453e-91ba-b6f24dee2ef2"),
    list(fn = morie_datasets_otis_c10_aggregate_durations_by_institution,
         rid = "eaf6d52a-210a-48ef-822a-294e9346c45c"),
    list(fn = morie_datasets_otis_c11_aggregate_lengths,
         rid = "9c7b74a5-53ad-4ef0-a7a6-97772cd01c55"),
    list(fn = morie_datasets_otis_c12_aggregate_durations_by_region,
         rid = "d7080653-69fc-4f38-8d83-709fe16ae465"))
  for (R in expected) {
    out <- testthat::with_mocked_bindings(
      .morie_ontario_ckan_dump_csv = function(resource_id,
                                                limit = 200000L) {
        expect_equal(resource_id, R$rid)
        data.frame(EndFiscalYear = 2024L)
      },
      .package = "morie",
      code = R$fn(offline = FALSE))
    expect_s3_class(out, "data.frame")
  }
})

# =================================================== mocked live dispatch (3 reps)

test_that("OTIS b01 dispatches via mock when resource_id is supplied", {
  out <- testthat::with_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id,
                                              limit = 200000L) {
      expect_equal(resource_id, "future-b01-id")
      data.frame(EndFiscalYear = 2024L,
                  UniqueIndividual_ID = "LIVE-B01-1")
    },
    .package = "morie",
    code = morie_datasets_otis_b01_segregation_detailed(
      offline = FALSE, resource_id = "future-b01-id"))
  expect_equal(out$UniqueIndividual_ID, "LIVE-B01-1")
})

test_that("OTIS c01 dispatches via mock when resource_id is supplied", {
  out <- testthat::with_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id,
                                              limit = 200000L) {
      expect_equal(resource_id, "future-c01-id")
      data.frame(EndFiscalYear = 2024L,
                  NumberIndividuals_InCustody = 7777L)
    },
    .package = "morie",
    code = morie_datasets_otis_c01_individuals_total(
      offline = FALSE, resource_id = "future-c01-id"))
  expect_equal(out$NumberIndividuals_InCustody, 7777L)
})

test_that("OTIS c11 dispatches via mock when resource_id is supplied", {
  out <- testthat::with_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id,
                                              limit = 200000L) {
      expect_equal(resource_id, "future-c11-id")
      data.frame(EndFiscalYear = 2024L,
                  Aggregate_Duration = "Over 90 days",
                  NumberIndividuals_RestrictiveConfinement = 9L,
                  NumberIndividuals_Segregation = 5L)
    },
    .package = "morie",
    code = morie_datasets_otis_c11_aggregate_lengths(
      offline = FALSE, resource_id = "future-c11-id"))
  expect_equal(out$NumberIndividuals_Segregation, 5L)
})

# =================================================== registry integration

test_that("morie_datasets_ontario_ckan_layers includes all 21 b/c entries with wired resource_ids", {
  reg <- morie_datasets_ontario_ckan_layers()
  expect_true(nrow(reg) >= 38L)
  b_keys <- sprintf("otis_b%02d_%s", 1:9,
                     c("segregation_detailed",
                       "segregation_total_days",
                       "seg_alerts_by_institution",
                       "seg_consecutive_by_region",
                       "seg_consecutive_lengths",
                       "seg_reason_by_institution",
                       "seg_alerts_by_gender",
                       "seg_consecutive_by_institution",
                       "seg_n_times"))
  c_keys <- sprintf("otis_c%02d_%s", 1:12,
                     c("individuals_total",
                       "individuals_by_institution",
                       "individuals_race_by_gender",
                       "individuals_race_by_region",
                       "individuals_religion_by_region",
                       "individuals_age_by_region",
                       "individuals_alerts",
                       "individuals_religion_by_gender",
                       "individuals_age_by_gender",
                       "aggregate_durations_by_institution",
                       "aggregate_lengths",
                       "aggregate_durations_by_region"))
  for (k in c(b_keys, c_keys))
    expect_true(k %in% reg$dataset_key)
  # Post-3KK: every b/c entry has a wired CKAN UUID resource_id.
  subset <- reg[reg$dataset_key %in% c(b_keys, c_keys), ]
  expect_false(any(is.na(subset$resource_id)))
  expect_true(all(grepl("^[0-9a-f]{8}-[0-9a-f]{4}",
                         subset$resource_id)))
  # And the FULL Ontario CKAN registry now has zero NA resource_ids
  # (all 38 dataset_keys are wired live-mode ready).
  expect_false(any(is.na(reg$resource_id)))
})

test_that("morie_datasets_ontario_ckan_by_key('otis_c03_individuals_race_by_gender', offline=TRUE) reads bundled fixture", {
  df <- morie_datasets_ontario_ckan_by_key(
    "otis_c03_individuals_race_by_gender", offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Race", "Gender") %in% names(df)))
})
