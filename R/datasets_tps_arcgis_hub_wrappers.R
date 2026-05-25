# SPDX-License-Identifier: AGPL-3.0-or-later
#
# THIS FILE IS GENERATED. Do not edit by hand.
#
# Regenerate with:
#   Rscript data-raw/generate_tps_hub_wrappers.R
#
# Source: inst/extdata/tps_arcgis_hub_catalog.csv (71 TPS Hub items,
# canonical count verified live 2026-05-24).
#
# Each wrapper is a thin dispatch to morie_datasets_tps_arcgis_hub_by_id
# with the hub item_id hard-coded. Skipped catalog entries whose
# slug collides with an existing TPS export at generation time:
#   morie_datasets_tps_assault, morie_datasets_tps_auto_theft, morie_datasets_tps_break_and_enter, morie_datasets_tps_hate_crimes, morie_datasets_tps_homicides, morie_datasets_tps_intimate_partner_and_family_violence, morie_datasets_tps_mental_health_act_apprehensions, morie_datasets_tps_robbery, morie_datasets_tps_shooting_and_firearm_discharges, morie_datasets_tps_theft_from_motor_vehicle, morie_datasets_tps_theft_over


#' 2008 FIRS
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `b8e3ef826ea84cbcb85951d051afc2fa`.
#'
#'   2008 Field Information Requests (FIRS)
#'
#' Tags: FIRS; Field Information Requests
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_2008_firs <- function(format = "json",
                                             where = "1=1",
                                             max_features = NULL,
                                             layer_idx = 0L,
                                             offline = TRUE,
                                             dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("b8e3ef826ea84cbcb85951d051afc2fa",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' 2009 FIRS
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `de8ba3b4899b48bc8fbf4421f4945ed6`.
#'
#'   2009 Field Information Requests (FIRS)
#'
#' Tags: FIRS; Freedom of Information Requests
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_2009_firs <- function(format = "json",
                                             where = "1=1",
                                             max_features = NULL,
                                             layer_idx = 0L,
                                             offline = TRUE,
                                             dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("de8ba3b4899b48bc8fbf4421f4945ed6",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' 2010 FIRS
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `9a5c8a4fdfa54e7f97236a769b196f16`.
#'
#'   2010 Field Information Requests (FIRS)
#'
#' Tags: FIRS; Field Information Requests
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_2010_firs <- function(format = "json",
                                             where = "1=1",
                                             max_features = NULL,
                                             layer_idx = 0L,
                                             offline = TRUE,
                                             dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("9a5c8a4fdfa54e7f97236a769b196f16",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' 2011 FIRS
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `78361ed81cca40aebd1032a26ef52e5b`.
#'
#'   2011 Field Information Requests (FIRS)
#'
#' Tags: FIRS; Field Information Requests
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_2011_firs <- function(format = "json",
                                             where = "1=1",
                                             max_features = NULL,
                                             layer_idx = 0L,
                                             offline = TRUE,
                                             dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("78361ed81cca40aebd1032a26ef52e5b",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' 2012 FIRS
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `7a690ba1d7714063983ed78024d5b2af`.
#'
#'   2012 Field Information Requests (FIRS)
#'
#' Tags: FIRS; Freedom of Information Requests
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_2012_firs <- function(format = "json",
                                             where = "1=1",
                                             max_features = NULL,
                                             layer_idx = 0L,
                                             offline = TRUE,
                                             dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("7a690ba1d7714063983ed78024d5b2af",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' 2013 FIRS
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `95a29d4453894944be7a79f537a432b1`.
#'
#'   2013 Field Information Requests (FIRS)
#'
#' Tags: FIRS; Field Information Requests
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_2013_firs <- function(format = "json",
                                             where = "1=1",
                                             max_features = NULL,
                                             layer_idx = 0L,
                                             offline = TRUE,
                                             dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("95a29d4453894944be7a79f537a432b1",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Administrative (ASR-AD-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `135607d9250b4e5ea930e7ea39780a77`.
#'
#'   This dataset provides a breakdown of administrative information.
#'   This data is compiled and provided by several units of the
#'   Toronto Police Service.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Administrative
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_administrative <- function(format = "json",
                                                  where = "1=1",
                                                  max_features = NULL,
                                                  layer_idx = 0L,
                                                  offline = TRUE,
                                                  dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("135607d9250b4e5ea930e7ea39780a77",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Arrested and Charged Persons (ASR-ENF-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `4702e79fd2404f7d93dd9866f45d7ec2`.
#'
#'   This dataset provides an aggregate count of persons who have
#'   been arrested and charged. The data is aggregated by division,
#'   neighbourhood, sex, age, crime category, and crime subtype.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Arrests
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_arrested_and_charged_persons <- function(format = "json",
                                                                where = "1=1",
                                                                max_features = NULL,
                                                                layer_idx = 0L,
                                                                offline = TRUE,
                                                                dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("4702e79fd2404f7d93dd9866f45d7ec2",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Arrests and Strip Searches (RBDC-ARR-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `899f1b3b047c47659a9843e9c5858269`.
#'
#'   This dataset includes information related to all arrests and
#'   strip searches.
#'
#' Tags: RBDC; race; race based data
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_arrests_and_strip_searches <- function(format = "json",
                                                              where = "1=1",
                                                              max_features = NULL,
                                                              layer_idx = 0L,
                                                              offline = TRUE,
                                                              dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("899f1b3b047c47659a9843e9c5858269",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Automobile KSI
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `9a21cd6f550748c3a25ac89a108ca5c5`.
#'
#'   Automobile-related KSI collisions (2006 - 2023).
#'
#' Tags: Killed or Seriously Injured; KSI; Automobile; Traffic; Collision; Traffic Collision
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_automobile_ksi <- function(format = "json",
                                                  where = "1=1",
                                                  max_features = NULL,
                                                  layer_idx = 0L,
                                                  offline = TRUE,
                                                  dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("9a21cd6f550748c3a25ac89a108ca5c5",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Bicycle Thefts Open Data
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `a89d10d5e28444ceb0c8d1d4c0ee39cc`.
#'
#'   Bicycle Theft occurrences by reported date.
#'
#' Tags: Bike; Bicycle; Thefts; Crime; Toronto; TPS; Toronto Police
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_bicycle_thefts <- function(format = "json",
                                                  where = "1=1",
                                                  max_features = NULL,
                                                  layer_idx = 0L,
                                                  offline = TRUE,
                                                  dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("a89d10d5e28444ceb0c8d1d4c0ee39cc",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Budget_2020
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `daca9df799ea4a54a29955ce7fb972d4`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: Budget; Toronto Police Service; TPS
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_budget_2020 <- function(format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               offline = TRUE,
                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("daca9df799ea4a54a29955ce7fb972d4",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Budget_2021
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `b511c476865b4f0a993cb7bb1c6be7cf`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: Budget; Toronto Police Service; TPS
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_budget_2021 <- function(format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               offline = TRUE,
                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("b511c476865b4f0a993cb7bb1c6be7cf",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Budget_2022
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `25c20a7f15e44579acb947510405ab24`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: Budget; Toronto Police Service; TPS
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_budget_2022 <- function(format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               offline = TRUE,
                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("25c20a7f15e44579acb947510405ab24",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Budget_2023
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `6ac1f56513ab481091ce16f435c390b7`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: Budget; Toronto Police Service; TPS
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_budget_2023 <- function(format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               offline = TRUE,
                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("6ac1f56513ab481091ce16f435c390b7",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Budget_2024
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `584b12967d214bb9a673505d97295eea`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: Budget; Toronto Police Service; TPS
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_budget_2024 <- function(format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               offline = TRUE,
                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("584b12967d214bb9a673505d97295eea",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Budget_2025
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `cae4c92e80f84e949de156b3ac0d4fef`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: Budget; Toronto Police Service; TPS
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_budget_2025 <- function(format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               offline = TRUE,
                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("cae4c92e80f84e949de156b3ac0d4fef",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Budget_2026
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `d80f9e0b3cc74f649e5e4593cdda207e`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: Budget; Toronto Police Service; TPS
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_budget_2026 <- function(format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               offline = TRUE,
                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("d80f9e0b3cc74f649e5e4593cdda207e",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Budget_by_Command
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `3dca9164b32e4ca7b9c23f41efc9904b`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: Budget; Toronto Police Service; TPS
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_budget_by_command <- function(format = "json",
                                                     where = "1=1",
                                                     max_features = NULL,
                                                     layer_idx = 0L,
                                                     offline = TRUE,
                                                     dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("3dca9164b32e4ca7b9c23f41efc9904b",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Calls for Service Attended (ASR-CS-TBL-003)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `46c7581a136445c78831acb657a4fb0d`.
#'
#'   This dataset provides a count of calls for service attended
#'   aggregated by division and neighbourhood.
#'
#' Tags: ASR; TPS; Annual Statistical Report; Toronto Police; Calls for Service
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_calls_for_service_attended <- function(format = "json",
                                                              where = "1=1",
                                                              max_features = NULL,
                                                              layer_idx = 0L,
                                                              offline = TRUE,
                                                              dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("46c7581a136445c78831acb657a4fb0d",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Community Safety Indicators Open Data
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `0a239a5563a344a3bbf8452504ed8d68`.
#'
#'   Community Safety Indicators (CSI) occurrences by reported date.
#'
#' Tags: Community Safety Indicators; CSI; Crime; Toronto; TPS; Toronto Police
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_community_safety_indicators <- function(format = "json",
                                                               where = "1=1",
                                                               max_features = NULL,
                                                               layer_idx = 0L,
                                                               offline = TRUE,
                                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("0a239a5563a344a3bbf8452504ed8d68",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Complaint Dispositions (ASR-PCF-TBL-003)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `8f3cbe34f3724f93b3aa321b3e957092`.
#'
#'   This dataset provides a breakdown of the total investigated
#'   complaints by disposition of the complaint submitted.
#'
#' Tags: ASR; TPS; Toronto Police; Open Data; Annual Statistical Report; Complaints
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_complaint_dispositions <- function(format = "json",
                                                          where = "1=1",
                                                          max_features = NULL,
                                                          layer_idx = 0L,
                                                          offline = TRUE,
                                                          dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("8f3cbe34f3724f93b3aa321b3e957092",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Cyclist KSI
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `b38c2524696943bb86398d314a06a42a`.
#'
#'   Cyclist-related KSI collisions (2006 - 2023).
#'
#' Tags: Killed or Seriously Injured; KSI; Cyclist; Traffic; Collision; Traffic Collision
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_cyclist_ksi <- function(format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               offline = TRUE,
                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("b38c2524696943bb86398d314a06a42a",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Dispatched Calls by Division (ASR-CS-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `9cfcd6fe0d374f65afda69c4b9bdc60a`.
#'
#'   This dataset provides a count of the dispatched calls by
#'   division, including some specific units such as PRIME, Parking
#'   and “Other”. This data includes the command level at the time of
#'   reporting.
#'
#' Tags: ASR; TPS; Annual Statistical Report; Toronto Police; Dispatched Calls
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_dispatched_calls_by_division <- function(format = "json",
                                                                where = "1=1",
                                                                max_features = NULL,
                                                                layer_idx = 0L,
                                                                offline = TRUE,
                                                                dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("9cfcd6fe0d374f65afda69c4b9bdc60a",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Facilities
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `6288c8314c594bc9a384df2cf17f8474`.
#'
#'   Police stations and other TPS facilities.
#'
#' Tags: Police Facilities; TPS Facilities; Facilities
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_facilities <- function(format = "json",
                                              where = "1=1",
                                              max_features = NULL,
                                              layer_idx = 0L,
                                              offline = TRUE,
                                              dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("6288c8314c594bc9a384df2cf17f8474",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Fatals KSI
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `317e768682d14fad94de83eaefbf5954`.
#'
#'   Fatal-related KSI collisions (2006 - 2023).
#'
#' Tags: Killed or Seriously Injured; KSI; Fatal; Traffic; Collision; Traffic Collision
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_fatals_ksi <- function(format = "json",
                                              where = "1=1",
                                              max_features = NULL,
                                              layer_idx = 0L,
                                              offline = TRUE,
                                              dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("317e768682d14fad94de83eaefbf5954",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Firearms Top Calibres (ASR-F-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `9b1b38ed56764b968c25ce6b74e5dc0d`.
#'
#'   The dataset provides a list of the most common types of pistols,
#'   revolvers, rifles and shotguns that comprise the crime guns
#'   seized by the Toronto Police Service.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Firearms
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_firearms_top_calibres <- function(format = "json",
                                                         where = "1=1",
                                                         max_features = NULL,
                                                         layer_idx = 0L,
                                                         offline = TRUE,
                                                         dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("9b1b38ed56764b968c25ce6b74e5dc0d",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Gross Expenditures by Division (ASR-PB-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `6cb7e76c7d5b4bf5bce0c533ca7fdf40`.
#'
#'   This dataset provides a breakdown of the Gross Expenditures for
#'   each division. This data includes the command level at the time
#'   of reporting.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Budget
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_gross_expenditures_by_division <- function(format = "json",
                                                                  where = "1=1",
                                                                  max_features = NULL,
                                                                  layer_idx = 0L,
                                                                  offline = TRUE,
                                                                  dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("6cb7e76c7d5b4bf5bce0c533ca7fdf40",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Gross Operating Budget (ASR-PB-TBL-005)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `8e95b932cd2d404b9d9d26c2ecc8ebec`.
#'
#'   This dataset provides the Gross Operating Budget incurred by the
#'   Toronto Police Service.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Budget
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_gross_operating_budget <- function(format = "json",
                                                          where = "1=1",
                                                          max_features = NULL,
                                                          layer_idx = 0L,
                                                          offline = TRUE,
                                                          dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("8e95b932cd2d404b9d9d26c2ecc8ebec",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Investigated Alleged Misconduct (ASR-PCF-TBL-002)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `aaea16d94ae64da8a790d9649788de4c`.
#'
#'   This dataset provides a breakdown of the total investigated
#'   complaints by type of complaint submitted.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Misconduct
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_investigated_alleged_misconduct <- function(format = "json",
                                                                   where = "1=1",
                                                                   max_features = NULL,
                                                                   layer_idx = 0L,
                                                                   offline = TRUE,
                                                                   dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("aaea16d94ae64da8a790d9649788de4c",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Killed and Seriously Injured
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `0a1ee9d9436546dcbdc0ee9301e45e83`.
#'
#'   Killed or Seriously Injured (KSI) - related collisions (2006 -
#'   2023).
#'
#' Tags: Killed or Seriously Injured; KSI; MVC; Traffic; Collision; Traffic Collision
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_killed_and_seriously_injured <- function(format = "json",
                                                                where = "1=1",
                                                                max_features = NULL,
                                                                layer_idx = 0L,
                                                                offline = TRUE,
                                                                dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("0a1ee9d9436546dcbdc0ee9301e45e83",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Miscellaneous Calls for Service (ASR-CS-TBL-002)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `542374a83ea64b3ba222c41309445b8e`.
#'
#'   This dataset includes the following categories of data:
#'   Languages, Calls Received, and Alarm Calls
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Calls for Service
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_miscellaneous_calls_for_service <- function(format = "json",
                                                                   where = "1=1",
                                                                   max_features = NULL,
                                                                   layer_idx = 0L,
                                                                   offline = TRUE,
                                                                   dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("542374a83ea64b3ba222c41309445b8e",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Miscellaneous Data (ASR-MISC-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `bc229f576f174e24946bd1649c98aa43`.
#'
#'   This dataset contains information pertaining to intimate partner
#'   violence, hate crimes and budget.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Hate Crime; Budget; IPV
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_miscellaneous_data <- function(format = "json",
                                                      where = "1=1",
                                                      max_features = NULL,
                                                      layer_idx = 0L,
                                                      offline = TRUE,
                                                      dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("bc229f576f174e24946bd1649c98aa43",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Miscellaneous Firearms (ASR-F-TBL-003)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `7f9e4439a6e749dea32dab9e1704b58a`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Firearms
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_miscellaneous_firearms <- function(format = "json",
                                                          where = "1=1",
                                                          max_features = NULL,
                                                          layer_idx = 0L,
                                                          offline = TRUE,
                                                          dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("7f9e4439a6e749dea32dab9e1704b58a",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Motorcylist KSI
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `d691a9391c2a4c6d85bb761530d33310`.
#'
#'   Motorcyclist-related KSI collisions (2006 - 2023).
#'
#' Tags: Killed or Seriously Injured; KSI; Motorcyclist; Traffic; Collision; Traffic Collision
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_motorcylist_ksi <- function(format = "json",
                                                   where = "1=1",
                                                   max_features = NULL,
                                                   layer_idx = 0L,
                                                   offline = TRUE,
                                                   dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("d691a9391c2a4c6d85bb761530d33310",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Neighbourhood Crime Rates Open Data
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `ea0cfecdb1de416884e6b0bf08a9e195`.
#'
#'   Neighbourhood crime rates per 100,000.
#'
#' Tags: Neighbourhood; Crime; Rate; Crime Rates; Community Safety Indicators; CSI; Toronto; TPS; Toronto Police
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_neighbourhood_crime_rates <- function(format = "json",
                                                             where = "1=1",
                                                             max_features = NULL,
                                                             layer_idx = 0L,
                                                             offline = TRUE,
                                                             dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("ea0cfecdb1de416884e6b0bf08a9e195",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Passenger KSI
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `e4e28a899191479d8e53754414894870`.
#'
#'   Passenger-related KSI collisions (2006 - 2023).
#'
#' Tags: Killed or Seriously Injured; KSI; Passenger; Traffic; Collision; Traffic Collision
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_passenger_ksi <- function(format = "json",
                                                 where = "1=1",
                                                 max_features = NULL,
                                                 layer_idx = 0L,
                                                 offline = TRUE,
                                                 dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("e4e28a899191479d8e53754414894870",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Patrol Zone
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `4a02ac3ed83d478c914d62c3064d6bc4`.
#'
#'   Police patrol zones.
#'
#' Tags: Patrol Zone; Boundary Files; Patrol Zones
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_patrol_zone <- function(format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               offline = TRUE,
                                               dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("4a02ac3ed83d478c914d62c3064d6bc4",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Pedestrian KSI
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `a96252bf61b84fc68c3926bb7485970e`.
#'
#'   Pedestrian-related KSI collisions (2006 - 2023).
#'
#' Tags: Killed or Seriously Injured; KSI; Pedestrian; Traffic; Collision; Traffic Collision
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_pedestrian_ksi <- function(format = "json",
                                                  where = "1=1",
                                                  max_features = NULL,
                                                  layer_idx = 0L,
                                                  offline = TRUE,
                                                  dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("a96252bf61b84fc68c3926bb7485970e",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Personnel by Command (ASR-PB-TBL-004)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `1f58109772e2484fba0f509c1ab49fe8`.
#'
#'   This dataset provides a count of personnel broken down by
#'   command level.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Personnel
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_personnel_by_command <- function(format = "json",
                                                        where = "1=1",
                                                        max_features = NULL,
                                                        layer_idx = 0L,
                                                        offline = TRUE,
                                                        dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("1f58109772e2484fba0f509c1ab49fe8",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Personnel by Rank (ASR-PB-TBL-002)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `62016275c866412d8de5db539dc0cb8a`.
#'
#'   This dataset provides a count of personnel broken down by rank
#'   classification for Uniform, Civilian, and Other Staff.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Personnel
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_personnel_by_rank <- function(format = "json",
                                                     where = "1=1",
                                                     max_features = NULL,
                                                     layer_idx = 0L,
                                                     offline = TRUE,
                                                     dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("62016275c866412d8de5db539dc0cb8a",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Personnel by Rank by Division (ASR-PB-TBL-003)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `e29b8d05c4754b3b8fc234324811a897`.
#'
#'   This dataset provides a count of personnel broken down by rank
#'   classification for Uniform & Civilian staff by division. This
#'   data includes the command level at the time of reporting.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Personnel
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_personnel_by_rank_by_division <- function(format = "json",
                                                                 where = "1=1",
                                                                 max_features = NULL,
                                                                 layer_idx = 0L,
                                                                 offline = TRUE,
                                                                 dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("e29b8d05c4754b3b8fc234324811a897",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Persons in Crisis Calls for Service Attended Open Data
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `79c8e950bfe54ce39334ba108e1b325f`.
#'
#'   Persons in crisis calls for service attended.
#'
#' Tags: Persons in Crisis; PIC; Crisis; Apprehensions; MHA; Calls; Calls for Service; Toronto; TPS; Toronto Police
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_persons_in_crisis_calls_for_service_attended <- function(format = "json",
                                                                                where = "1=1",
                                                                                max_features = NULL,
                                                                                layer_idx = 0L,
                                                                                offline = TRUE,
                                                                                dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("79c8e950bfe54ce39334ba108e1b325f",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Police Divisions
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `fda21b25213c4c07b08c5162cba5081f`.
#'
#'   Police divisions (post D54/D55 amalgamation).
#'
#' Tags: City of Toronto; Toronto; Open Data; Feature Class; Update; Data Load; Divisions; Police Divisions
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_police_divisions <- function(format = "json",
                                                    where = "1=1",
                                                    max_features = NULL,
                                                    layer_idx = 0L,
                                                    offline = TRUE,
                                                    dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("fda21b25213c4c07b08c5162cba5081f",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Regulated Interactions (ASR-RI-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `1cd5d478ef79424a8a6d5319a44edb0a`.
#'
#'   The data provided count describes situations involving regulated
#'   interactions.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Regulated Interactions
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_regulated_interactions <- function(format = "json",
                                                          where = "1=1",
                                                          max_features = NULL,
                                                          layer_idx = 0L,
                                                          offline = TRUE,
                                                          dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("1cd5d478ef79424a8a6d5319a44edb0a",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Reported Crimes (ASR-RC-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `fe2e40a464e64cb3a0e69ac3ccd17dfa`.
#'
#'   This dataset includes all reported crime offences by reported
#'   date aggregated by division.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Reported Crimes
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_reported_crimes <- function(format = "json",
                                                   where = "1=1",
                                                   max_features = NULL,
                                                   layer_idx = 0L,
                                                   offline = TRUE,
                                                   dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("fe2e40a464e64cb3a0e69ac3ccd17dfa",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Search of Persons (ASR-SP-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `8ee1697ce6af44a78640686a1feeeefb`.
#'
#'   This dataset includes all Level 3 and Level 4 searches that were
#'   conducted.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Search of Person
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_search_of_persons <- function(format = "json",
                                                     where = "1=1",
                                                     max_features = NULL,
                                                     layer_idx = 0L,
                                                     offline = TRUE,
                                                     dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("8ee1697ce6af44a78640686a1feeeefb",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Staffing_by_Command
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `9d97ef7e8b494095be4abc0a628d7ce3`.
#'
#'   Toronto Police Service dataset.
#'
#' Tags: Staffing; Budget; Toronto Police Service; TPS
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_staffing_by_command <- function(format = "json",
                                                       where = "1=1",
                                                       max_features = NULL,
                                                       layer_idx = 0L,
                                                       offline = TRUE,
                                                       dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("9d97ef7e8b494095be4abc0a628d7ce3",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Tickets Issued (ASR-ENF-TBL-002)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `5069c21b5b364194807bf1958556b1ff`.
#'
#'   This dataset provides an aggregated count of tickets issued by
#'   year, ticket type, offence, age group, division, and
#'   neighbourhood.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Tickets
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_tickets_issued <- function(format = "json",
                                                  where = "1=1",
                                                  max_features = NULL,
                                                  layer_idx = 0L,
                                                  offline = TRUE,
                                                  dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("5069c21b5b364194807bf1958556b1ff",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Top 20 Offences of Firearm Seizures (ASR-F-TBL-002)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `a83aa604fed240acaf2dfe64e1b323f8`.
#'
#'   This dataset provides a list of top 20 offences ranked by
#'   volume, for occurrences linked to a firearm seizure.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Firearms
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_top_20_offences_of_firearm_seizures <- function(format = "json",
                                                                       where = "1=1",
                                                                       max_features = NULL,
                                                                       layer_idx = 0L,
                                                                       offline = TRUE,
                                                                       dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("a83aa604fed240acaf2dfe64e1b323f8",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Total Public Complaints (ASR-PCF-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `a16edf4bc9484e94ad7e00bc22727544`.
#'
#'   This dataset provides a breakdown of the total number of public
#'   complaints from the Law Enforcement Complaints Agency (L.E.C.A.)
#'   broken down by complaints that were investigated and not
#'   investigated.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Complaints
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_total_public_complaints <- function(format = "json",
                                                           where = "1=1",
                                                           max_features = NULL,
                                                           layer_idx = 0L,
                                                           offline = TRUE,
                                                           dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("a16edf4bc9484e94ad7e00bc22727544",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Traffic Collisions Open Data (ASR-T-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `bc4c72a793014a55a674984ef175a6f3`.
#'
#'   Collision occurrences by occurrence date.
#'
#' Tags: Traffic; Collision; Traffic Collisions; Motor Vehicle Collisions; Toronto; TPS; Toronto Police
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_traffic_collisions <- function(format = "json",
                                                      where = "1=1",
                                                      max_features = NULL,
                                                      layer_idx = 0L,
                                                      offline = TRUE,
                                                      dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("bc4c72a793014a55a674984ef175a6f3",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Use of Force: Call for Service Types (RBDC-UOF-TBL-004)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `04633ebdaba941efaa82f2cdaaa00bb8`.
#'
#'   This table provides information about the types of calls for
#'   service which resulted in an enforcement action and/or reported
#'   use of force.
#'
#' Tags: race; race based data; RBDC
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_use_of_force_call_for_service_types <- function(format = "json",
                                                                       where = "1=1",
                                                                       max_features = NULL,
                                                                       layer_idx = 0L,
                                                                       offline = TRUE,
                                                                       dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("04633ebdaba941efaa82f2cdaaa00bb8",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Use of Force: Call Sources by Month (RBDC-UOF-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `98d88b18c0364c8d86e6a7c690037b85`.
#'
#'   This table provides monthly counts of incidents which officers
#'   responded to from different call sources and which resulted in
#'   an enforcement action and/or reported use of force.
#'
#' Tags: RBDC; race based data; race
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_use_of_force_call_sources_by_month <- function(format = "json",
                                                                      where = "1=1",
                                                                      max_features = NULL,
                                                                      layer_idx = 0L,
                                                                      offline = TRUE,
                                                                      dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("98d88b18c0364c8d86e6a7c690037b85",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Use of Force: Gender Composition (RBDC-UOF-TBL-006)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `de9284945c3e479e938c4b77586535b1`.
#'
#'   This table provides information about the genders of people
#'   involved in enforcement action incidents, including those that
#'   may be associated with a reported use of force.
#'
#' Tags: race; race based data; rbdc
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_use_of_force_gender_composition <- function(format = "json",
                                                                   where = "1=1",
                                                                   max_features = NULL,
                                                                   layer_idx = 0L,
                                                                   offline = TRUE,
                                                                   dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("de9284945c3e479e938c4b77586535b1",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Use of Force: Location of Occurrences (RBDC-UOF-TBL-003)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `0e7f95cb45704c8e8c9a05973422211c`.
#'
#'   This table provides location information, aggregated to the
#'   division level.
#'
#' Tags: RBDC; race; race based data
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_use_of_force_location_of_occurrences <- function(format = "json",
                                                                        where = "1=1",
                                                                        max_features = NULL,
                                                                        layer_idx = 0L,
                                                                        offline = TRUE,
                                                                        dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("0e7f95cb45704c8e8c9a05973422211c",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Use of Force: Occurrence Category (RBDC-UOF-TBL-005)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `a9b6bef1d34b44eea814e1869fdcda62`.
#'
#'   This table provides information about the nature of the incident
#'   or the most serious offence associated with the incident, after
#'   officers arrive to the scene.
#'
#' Tags: rbdc; race; race based data
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_use_of_force_occurrence_category <- function(format = "json",
                                                                    where = "1=1",
                                                                    max_features = NULL,
                                                                    layer_idx = 0L,
                                                                    offline = TRUE,
                                                                    dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("a9b6bef1d34b44eea814e1869fdcda62",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Use of Force: Time of Day Trends (RBDC-UOF-TBL-002)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `b2bd6427e19a4706a4727338824a82b6`.
#'
#'   This table provides information on the time periods of the day
#'   in which incidents took place and which resulted in an
#'   enforcement action and/or reported use of force.
#'
#' Tags: RBDC; race; race based data
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_use_of_force_time_of_day_trends <- function(format = "json",
                                                                   where = "1=1",
                                                                   max_features = NULL,
                                                                   layer_idx = 0L,
                                                                   offline = TRUE,
                                                                   dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("b2bd6427e19a4706a4727338824a82b6",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Use of Force: Use of Force Types and Perceived Weapons (RBDC-UOF-TBL-007)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `9388798a44cd4ee5bc175669d8b6fb13`.
#'
#'   This table provides information about reported use of force
#'   incidents and the highest type of force used as well as whether
#'   officers perceived weapons were carried by people involved.
#'
#' Tags: race; race based data; rbdc
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_use_of_force_use_of_force_types_and_perceived_weapons <- function(format = "json",
                                                                                         where = "1=1",
                                                                                         max_features = NULL,
                                                                                         layer_idx = 0L,
                                                                                         offline = TRUE,
                                                                                         dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("9388798a44cd4ee5bc175669d8b6fb13",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}

#' Victims of Crime (ASR-VC-TBL-001)
#'
#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to
#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical
#' hub item_id `6afabfd5109847a2bbba3eaeb0275e35`.
#'
#'   This dataset includes all identified victims of crimes against
#'   the person, including, but not limited to, those that may have
#'   been deemed unfounded after investigation, those that may have
#'   occurred outside the City of Toronto limits, or have no verified
#'   location.
#'
#' Tags: ASR; TPS; Toronto Police; Annual Statistical Report; Victims
#'
#' @inheritParams morie_datasets_tps_arcgis_hub_by_id
#' @return A data.frame / GeoJSON list / file path; see
#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.
#' @export
morie_datasets_tps_victims_of_crime <- function(format = "json",
                                                    where = "1=1",
                                                    max_features = NULL,
                                                    layer_idx = 0L,
                                                    offline = TRUE,
                                                    dest = NULL) {
  morie_datasets_tps_arcgis_hub_by_id("6afabfd5109847a2bbba3eaeb0275e35",
                                       format = format,
                                       where = where,
                                       max_features = max_features,
                                       layer_idx = layer_idx,
                                       offline = offline,
                                       dest = dest)
}
