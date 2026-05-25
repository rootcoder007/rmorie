# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3DD + 3EE-prep: Toronto neighbourhood boundary-version
# awareness, exercised against the REAL Open Toronto attribute
# fixtures (158 + 140 + NIA + 158<->140 crosswalk) bundled under
# inst/extdata/.
#
# Network paths are exercised via testthat::local_mocked_bindings,
# so the suite never reaches the wire.

# ===================================================== morie_to_neighbourhoods()

test_that("morie_to_neighbourhoods('158', offline=TRUE) returns the full 158-row canonical layer", {
  df <- morie_to_neighbourhoods("158", offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 158L)
  for (col in c("AREA_ID", "AREA_SHORT_CODE", "AREA_NAME",
                "CLASSIFICATION", "OBJECTID"))
    expect_true(col %in% names(df))
  # Names that are stable post-2022 158-scheme (none were split or
  # renamed -- the 1:1 cohort). Pick one from the high-recall set.
  expect_true("Annex" %in% df$AREA_NAME)
  # "Niagara" (the historical 140-82 name) was SPLIT in 158 into
  # Fort York-Liberty Village + West Queen West; assert the new names
  # are there and the old single-name is not.
  expect_true("Fort York-Liberty Village" %in% df$AREA_NAME)
  expect_true("West Queen West"           %in% df$AREA_NAME)
  expect_false("Niagara" %in% df$AREA_NAME)
})

test_that("morie_to_neighbourhoods('140', offline=TRUE) returns the full 140-row historical layer", {
  df <- morie_to_neighbourhoods("140", offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 140L)
  # Historical names carry the "(NN)" suffix per City convention.
  expect_true(any(grepl("\\(\\d+\\)$", df$AREA_NAME)))
  # 140 has Niagara (82) -- still present in the historical scheme.
  expect_true(any(grepl("^Niagara \\(82\\)$", df$AREA_NAME)))
})

test_that("morie_to_neighbourhoods('nia', offline=TRUE) returns NIA schema with DATE_EFFECTIVE", {
  df <- morie_to_neighbourhoods("nia", offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0L)
  for (col in c("DATE_EFFECTIVE", "DATE_EXPIRY", "AREA_TYPE",
                "AREA_NAME"))
    expect_true(col %in% names(df))
})

test_that("morie_to_neighbourhoods errors on unknown version arg", {
  expect_error(morie_to_neighbourhoods("xyz", offline = TRUE),
               regexp = "should be one of")
})

# Live-mode dispatch (mocked CKAN).

test_that("morie_to_neighbourhoods(offline=FALSE) dispatches via mocked CKAN helper", {
  stub_df <- data.frame(
    `_id` = c(1L, 2L),
    AREA_ID = c(2502366L, 2502367L),
    AREA_SHORT_CODE = c("174", "082"),
    AREA_NAME = c("South Eglinton-Davisville", "Niagara"),
    check.names = FALSE)
  testthat::local_mocked_bindings(
    .morie_to_ckan_dump_csv = function(resource_id, limit = 100000L) {
      expect_match(resource_id,
                   "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}")
      stub_df
    },
    .package = "morie")
  out <- morie_to_neighbourhoods("158", offline = FALSE)
  expect_equal(nrow(out), 2L)
  expect_equal(out$AREA_NAME[2], "Niagara")
})

test_that("morie_to_neighbourhoods(offline=FALSE, resource_id=X) honours override", {
  testthat::local_mocked_bindings(
    .morie_to_ckan_dump_csv = function(resource_id, limit = 100000L) {
      expect_equal(resource_id, "custom-resource-id-xyz")
      data.frame(AREA_NAME = "OverrideHit")
    },
    .package = "morie")
  out <- morie_to_neighbourhoods("nia", offline = FALSE,
                                   resource_id = "custom-resource-id-xyz")
  expect_equal(out$AREA_NAME, "OverrideHit")
})

# =================================================== morie_tps_resolve_hood_col()

test_that("morie_tps_resolve_hood_col picks HOOD_158 when both present + prefer=158", {
  df <- data.frame(HOOD_158 = "82", HOOD_140 = "82")
  expect_equal(morie_tps_resolve_hood_col(df, prefer = "158"),
               "HOOD_158")
  expect_equal(morie_tps_resolve_hood_col(df, prefer = "140"),
               "HOOD_140")
})

test_that("morie_tps_resolve_hood_col falls back with a warning", {
  df <- data.frame(HOOD_140 = "82")
  expect_warning(
    out <- morie_tps_resolve_hood_col(df, prefer = "158",
                                        fallback = TRUE),
    regexp = "falling back")
  expect_equal(out, "HOOD_140")
})

test_that("morie_tps_resolve_hood_col returns NULL without fallback", {
  df <- data.frame(HOOD_140 = "82")
  expect_warning(
    out <- morie_tps_resolve_hood_col(df, prefer = "158",
                                        fallback = FALSE),
    regexp = "no HOOD_158")
  expect_null(out)
})

test_that("morie_tps_resolve_hood_col accepts lowercase column variants", {
  df <- data.frame(hood_158 = "82")
  expect_equal(morie_tps_resolve_hood_col(df, prefer = "158"),
               "hood_158")
})

# ================================================ morie_tps_assert_hood_version()

test_that("morie_tps_assert_hood_version returns TRUE invisibly when expected schema is present", {
  df <- data.frame(HOOD_158 = "82")
  expect_true(morie_tps_assert_hood_version(df, expected = "158"))
})

test_that("morie_tps_assert_hood_version errors when expected schema is absent", {
  df <- data.frame(HOOD_140 = "82")
  expect_error(morie_tps_assert_hood_version(df, expected = "158"),
               regexp = "expected HOOD_158")
})

test_that("morie_tps_assert_hood_version warns when BOTH schemas present", {
  df <- data.frame(HOOD_158 = "82", HOOD_140 = "82")
  expect_warning(morie_tps_assert_hood_version(df, expected = "158"),
                 regexp = "BOTH HOOD_158 .* and HOOD_140")
})

# =================================================== morie_tps_year_to_hood_version()

test_that("morie_tps_year_to_hood_version maps year -> recommended scheme", {
  expect_equal(morie_tps_year_to_hood_version(2014L), "140")
  expect_equal(morie_tps_year_to_hood_version(2021L), "140")
  expect_equal(morie_tps_year_to_hood_version(2022L), "158")
  expect_equal(morie_tps_year_to_hood_version(2025L), "158")
  expect_equal(
    morie_tps_year_to_hood_version(c(2020L, 2022L, 2024L)),
    c("140", "158", "158"))
})

test_that("morie_tps_year_to_hood_version returns NA on non-numeric input", {
  expect_true(is.na(morie_tps_year_to_hood_version("not-a-year")))
})

# ============================================= morie_to_hood_crosswalk() (REAL)

test_that("morie_to_hood_crosswalk returns the bundled REAL 158<->140 mapping", {
  cw <- morie_to_hood_crosswalk()
  expect_s3_class(cw, "data.frame")
  for (col in c("hood_140", "name_140", "hood_158", "name_158",
                "pct_140_in_158", "pct_158_in_140", "relation"))
    expect_true(col %in% names(cw))
  # Per the polygon-intersection build: 159 rows covering all 140
  # historical hoods + all 158 current hoods.
  expect_equal(nrow(cw), 159L)
  expect_equal(length(unique(cw$hood_140)), 140L)
  expect_equal(length(unique(cw$hood_158)), 158L)
})

test_that("morie_to_hood_crosswalk hood codes are 3-char zero-padded strings", {
  cw <- morie_to_hood_crosswalk()
  expect_type(cw$hood_140, "character")
  expect_type(cw$hood_158, "character")
  expect_true(all(nchar(cw$hood_140) == 3L))
  expect_true(all(nchar(cw$hood_158) == 3L))
})

test_that("morie_to_hood_crosswalk has bidirectional percent columns", {
  cw <- morie_to_hood_crosswalk()
  expect_true("pct_140_in_158" %in% names(cw))
  expect_true("pct_158_in_140" %in% names(cw))
  # Per-140 forward sum = 100.
  by_fwd <- aggregate(pct_140_in_158 ~ hood_140, cw, sum)
  expect_true(all(abs(by_fwd$pct_140_in_158 - 100) < 0.01))
  # Per-158 reverse sum = 100.
  by_rev <- aggregate(pct_158_in_140 ~ hood_158, cw, sum)
  expect_true(all(abs(by_rev$pct_158_in_140 - 100) < 0.01))
})

test_that("morie_to_hood_crosswalk split children have pct_158_in_140 == 100 (clean cake-cut)", {
  cw <- morie_to_hood_crosswalk()
  splits <- cw[cw$relation == "split", ]
  # All 34 split rows are clean cake-cuts (each 158 child fully
  # inside its 140 parent).
  expect_equal(nrow(splits), 34L)
  expect_true(all(splits$pct_158_in_140 == 100))
})

test_that("morie_to_hood_crosswalk relation distribution matches the OT data", {
  cw <- morie_to_hood_crosswalk()
  rel <- table(cw$relation)
  expect_equal(unname(rel["1:1"]),         123L)
  expect_equal(unname(rel["split"]),        34L)
  expect_equal(unname(rel["merge"]),         1L)
  expect_equal(unname(rel["split+merge"]),   1L)
})

test_that("morie_to_hood_crosswalk: 140-75 Church-Yonge Corridor splits into 158-167 + 158-168", {
  cw <- morie_to_hood_crosswalk()
  row75 <- cw[cw$hood_140 == "075", ]
  expect_true(nrow(row75) >= 2L)
  expect_true(all(row75$relation %in% c("split", "split+merge")))
  expect_setequal(row75$hood_158, c("167", "168"))
})

test_that("morie_to_hood_crosswalk: 140-82 Niagara splits into Fort York-Liberty Village + West Queen West", {
  cw <- morie_to_hood_crosswalk()
  row82 <- cw[cw$hood_140 == "082", ]
  expect_equal(nrow(row82), 2L)
  expect_setequal(row82$name_158,
                  c("Fort York-Liberty Village", "West Queen West"))
})

# ====================================== morie_tps_add_hood_158_from_140() / 140-from-158

test_that("morie_tps_add_hood_158_from_140 maps a 1:1 hood unchanged", {
  df <- data.frame(EVENT_ID = 1:2, HOOD_140 = c("001", "095"))
  out <- morie_tps_add_hood_158_from_140(df)
  expect_true("HOOD_158_equiv" %in% names(out))
  # 001 (West Humber-Clairville) and 095 (Annex) are both 1:1.
  expect_equal(out$HOOD_158_equiv, c("001", "095"))
})

test_that("morie_tps_add_hood_158_from_140 maps split hoods to PRIMARY-overlap 158", {
  # 140-75 (Church-Yonge Corridor) splits into 158-167 (40.76%)
  # + 158-168 (59.24%); primary is 158-168 (Downtown Yonge East).
  df <- data.frame(HOOD_140 = "075")
  out <- morie_tps_add_hood_158_from_140(df)
  expect_equal(out$HOOD_158_equiv, "168")
})

test_that("morie_tps_add_hood_158_from_140 normalises unpadded hood codes", {
  # TPS feeds publish HOOD_140 as integer-string "82" (no padding);
  # the crosswalk keys are "082". The normaliser should bridge.
  df <- data.frame(HOOD_140 = c("82", 1L, "1"))
  out <- morie_tps_add_hood_158_from_140(df)
  # 82 -> primary 158 is 163 (Fort York-Liberty Village, 72.18%).
  # 1  -> 001 (1:1).
  expect_equal(out$HOOD_158_equiv, c("163", "001", "001"))
})

test_that("morie_tps_add_hood_158_from_140 errors when no HOOD_140 column present", {
  df <- data.frame(HOOD_158 = "82")
  expect_error(morie_tps_add_hood_158_from_140(df),
               regexp = "no HOOD_140")
})

test_that("morie_tps_add_hood_140_from_158 maps a 1:1 hood unchanged", {
  df <- data.frame(EVENT_ID = 1:2, HOOD_158 = c("001", "095"))
  out <- morie_tps_add_hood_140_from_158(df)
  expect_true("HOOD_140_equiv" %in% names(out))
  expect_equal(out$HOOD_140_equiv, c("001", "095"))
})

test_that("morie_tps_add_hood_140_from_158 maps a child-of-split back to its historical parent", {
  # 158-168 (Downtown Yonge East) is a child of 140-75 (Church-Yonge
  # Corridor) -- the only 140 it overlaps. So 168 -> 075.
  df <- data.frame(HOOD_158 = c("168", "163"))
  out <- morie_tps_add_hood_140_from_158(df)
  # 168 -> 075. 163 (Fort York-Liberty Village) -> 082 (Niagara).
  expect_equal(out$HOOD_140_equiv, c("075", "082"))
})

test_that("morie_tps_add_hood_140_from_158 honours col_in + col_out overrides", {
  df <- data.frame(my_hood = c("001", "095"))
  out <- morie_tps_add_hood_140_from_158(df, col_in = "my_hood",
                                           col_out = "old_hood")
  expect_true("old_hood" %in% names(out))
  expect_equal(out$old_hood, c("001", "095"))
})

# ===================== morie_tps_disaggregate_140_to_158() (cake-cut forward)

test_that("morie_tps_disaggregate_140_to_158 passes 1:1 hoods through unchanged", {
  df <- data.frame(HOOD_140 = c("001", "095"),
                   incidents = c(100, 50))
  out <- morie_tps_disaggregate_140_to_158(df)
  expect_setequal(out$hood_158, c("001", "095"))
  # 1:1 -> count unchanged.
  one_001 <- out$incidents[out$hood_140 == "001"]
  one_095 <- out$incidents[out$hood_140 == "095"]
  expect_equal(one_001, 100)
  expect_equal(one_095, 50)
  # pct_140_in_158 column carries the weight.
  expect_true("pct_140_in_158" %in% names(out))
})

test_that("morie_tps_disaggregate_140_to_158 splits 140-75 into 158-167 + 158-168 by cake-cut weight", {
  df <- data.frame(HOOD_140 = "075", incidents = 100)
  out <- morie_tps_disaggregate_140_to_158(df)
  expect_equal(nrow(out), 2L)
  out <- out[order(out$hood_158), ]
  # 158-167 gets 40.7567% = 40.7567 incidents.
  # 158-168 gets 59.2433% = 59.2433 incidents.
  expect_equal(out$incidents[out$hood_158 == "167"], 40.7567,
               tolerance = 1e-4)
  expect_equal(out$incidents[out$hood_158 == "168"], 59.2433,
               tolerance = 1e-4)
  # Per-140 the disaggregated total equals the input.
  expect_equal(sum(out$incidents), 100, tolerance = 1e-4)
})

test_that("morie_tps_disaggregate_140_to_158 handles multiple count cols + multiple rows", {
  df <- data.frame(HOOD_140 = c("001", "075", "082"),
                   assault = c(10, 20, 30),
                   robbery = c(1, 2, 3))
  out <- morie_tps_disaggregate_140_to_158(df)
  expect_true(all(c("assault", "robbery") %in% names(out)))
  # 1:1 row 001 unchanged.
  one <- out[out$hood_140 == "001", ]
  expect_equal(nrow(one), 1L)
  expect_equal(one$assault, 10); expect_equal(one$robbery, 1)
  # Split row 075 -> 2 rows summing to (20, 2).
  s75 <- out[out$hood_140 == "075", ]
  expect_equal(nrow(s75), 2L)
  expect_equal(sum(s75$assault), 20, tolerance = 1e-3)
  expect_equal(sum(s75$robbery), 2,  tolerance = 1e-3)
})

test_that("morie_tps_disaggregate_140_to_158 errors when no numeric count col present", {
  df <- data.frame(HOOD_140 = "001", label = "x",
                   stringsAsFactors = FALSE)
  expect_error(morie_tps_disaggregate_140_to_158(df),
               regexp = "no numeric count columns")
})

# ===================== morie_tps_aggregate_158_to_140() (cake-cut reverse, EXACT)

test_that("morie_tps_aggregate_158_to_140 sums 158-children back to 140-parent EXACTLY for clean cake-cuts", {
  # 140-75 Church-Yonge Corridor split into 158-167 + 158-168.
  # Suppose post-split 158 counts are 40 + 60. Aggregation should
  # recover 100 in 140-75 exactly (no uniform-density assumption).
  df <- data.frame(HOOD_158 = c("167", "168"),
                   incidents = c(40, 60))
  out <- morie_tps_aggregate_158_to_140(df)
  expect_equal(out$hood_140, "075")
  expect_equal(out$incidents, 100)
})

test_that("morie_tps_aggregate_158_to_140 passes 1:1 hoods through unchanged", {
  df <- data.frame(HOOD_158 = c("001", "095"),
                   incidents = c(42, 17))
  out <- morie_tps_aggregate_158_to_140(df)
  expect_setequal(out$hood_140, c("001", "095"))
  out <- out[order(out$hood_140), ]
  expect_equal(out$incidents, c(42, 17))
})

test_that("morie_tps_aggregate_158_to_140 handles multiple count cols", {
  # 140-82 Niagara split into 158-162 + 158-163.
  df <- data.frame(HOOD_158 = c("162", "163"),
                   assault = c(7, 13),
                   robbery = c(1, 2))
  out <- morie_tps_aggregate_158_to_140(df)
  expect_equal(out$hood_140, "082")
  expect_equal(out$assault, 20)
  expect_equal(out$robbery,  3)
})

test_that("morie_tps_aggregate_158_to_140 + disaggregate_140_to_158 round-trip on 1:1 cohort", {
  # For a 1:1 hood the forward+reverse cake-cut is identity.
  df <- data.frame(HOOD_140 = c("001", "002", "095"),
                   incidents = c(10, 20, 30))
  forward <- morie_tps_disaggregate_140_to_158(df)
  back <- morie_tps_aggregate_158_to_140(
    data.frame(HOOD_158 = forward$hood_158,
               incidents = forward$incidents))
  back <- back[order(back$hood_140), ]
  expect_equal(back$hood_140, c("001", "002", "095"))
  expect_equal(back$incidents, c(10, 20, 30))
})

test_that("morie_tps_aggregate_158_to_140 + disaggregate_140_to_158 round-trip on the 140-75 split", {
  # Disaggregate 100 from 140-75; re-aggregate back to 140-75 = 100.
  df <- data.frame(HOOD_140 = "075", incidents = 100)
  forward <- morie_tps_disaggregate_140_to_158(df)
  back <- morie_tps_aggregate_158_to_140(
    data.frame(HOOD_158 = forward$hood_158,
               incidents = forward$incidents))
  expect_equal(back$hood_140, "075")
  expect_equal(back$incidents, 100, tolerance = 1e-4)
})

test_that("morie_tps_aggregate_158_to_140 errors when 158-hood col is missing", {
  df <- data.frame(other_col = c("001"), incidents = 10)
  expect_error(morie_tps_aggregate_158_to_140(df),
               regexp = "hood_158_col 'HOOD_158' not in df")
})
