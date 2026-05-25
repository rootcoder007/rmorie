# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3CCC3: TPS Hub resolved-joins analyzer + police divisions
# loader. Mirrors the Chicago + NYC resolved-joins patterns from
# 3VV+ and 3AAA-3CCC1.

# =================================================== police divisions

test_that("morie_datasets_tps_police_divisions(offline=TRUE) reads 16-row fixture", {
  df <- morie_datasets_tps_police_divisions(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 16L)
  for (col in c("DIV", "UNIT_NAME", "ADDRESS", "CITY", "AREA_SQKM"))
    expect_true(col %in% names(df),
                info = sprintf("missing %s", col))
  expect_type(df$DIV, "character")
  # Post-amalgamation TPS divisions.
  expect_setequal(df$DIV,
                  c("D11", "D12", "D13", "D14",
                    "D22", "D23",
                    "D31", "D32", "D33",
                    "D41", "D42", "D43",
                    "D51", "D52", "D53", "D55"))
})

test_that("max_features cap is honoured on divisions loader", {
  df <- morie_datasets_tps_police_divisions(offline = TRUE,
                                              max_features = 3L)
  expect_equal(nrow(df), 3L)
})

# =================================================== resolved-joins analyzer

test_that("morie_datasets_tps_psdp_resolved('assault', offline=TRUE) adds all resolver cols", {
  df <- morie_datasets_tps_psdp_resolved("assault", offline = TRUE)
  expect_s3_class(df, "data.frame")
  # Division
  for (col in c("division_UNIT_NAME", "division_AREA_SQKM",
                "division_ADDRESS"))
    expect_true(col %in% names(df),
                 info = sprintf("missing %s", col))
  # Hood158 / Hood140
  expect_true("hood158_AREA_NAME" %in% names(df))
  expect_true("hood140_AREA_NAME" %in% names(df))
  # NIA flag
  expect_true("nia_is_nia" %in% names(df))
  expect_type(df$nia_is_nia, "logical")
  # PSDP class
  expect_true("psdp_class_label" %in% names(df))
  expect_true("psdp_class_hub_id" %in% names(df))
})

test_that("morie_datasets_tps_psdp_resolved row count is preserved", {
  base <- morie_datasets_tps_assault(offline = TRUE)
  resolved <- morie_datasets_tps_psdp_resolved("assault",
                                                  offline = TRUE)
  expect_equal(nrow(resolved), nrow(base))
})

test_that("morie_datasets_tps_psdp_resolved single-resolver mode skips others", {
  d <- morie_datasets_tps_psdp_resolved("assault",
                                           offline = TRUE,
                                           resolvers = "division")
  expect_true("division_UNIT_NAME" %in% names(d))
  expect_false("hood158_AREA_NAME" %in% names(d))
  expect_false("nia_is_nia" %in% names(d))
  # psdp_class only joined when in resolvers list.
  expect_false("psdp_class_label" %in% names(d))

  h <- morie_datasets_tps_psdp_resolved("assault",
                                           offline = TRUE,
                                           resolvers = "hood158")
  expect_true("hood158_AREA_NAME" %in% names(h))
  expect_false("division_UNIT_NAME" %in% names(h))
})

test_that("morie_datasets_tps_psdp_resolved works across all 11 PSDP keys", {
  reg <- morie_tps_psdp_layers()
  expect_true(nrow(reg) >= 11L)
  for (k in reg$layer_key) {
    df <- morie_datasets_tps_psdp_resolved(k, offline = TRUE,
                                              resolvers = "psdp_class")
    expect_s3_class(df, "data.frame")
    expect_true("psdp_class_label" %in% names(df),
                 info = sprintf("layer_key=%s", k))
    if (nrow(df) > 0L) {
      expect_true(all(df$psdp_class_key == k))
    }
  }
})

test_that("morie_datasets_tps_psdp_resolved rejects unknown resolver names", {
  expect_error(
    morie_datasets_tps_psdp_resolved("assault",
                                        offline = TRUE,
                                        resolvers = "alien"))
})

test_that("DIVISION join produces real UNIT_NAME values for assault fixture", {
  df <- morie_datasets_tps_psdp_resolved("assault",
                                            offline = TRUE,
                                            resolvers = "division")
  # All synthetic assault rows have DIVISION values that hit a real
  # division -- so UNIT_NAME should be non-NA for every row that
  # had a DIVISION.
  has_div <- !is.na(df$DIVISION) & nzchar(df$DIVISION)
  expect_true(all(!is.na(df$division_UNIT_NAME[has_div])) ||
                 sum(has_div) == 0L)
})
