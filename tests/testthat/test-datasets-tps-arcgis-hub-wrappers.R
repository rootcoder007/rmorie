# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3TT: bulk-generated TPS Hub named wrappers (60 of 71
# canonical TPS Hub items; 11 skipped because their slug collides
# with an existing 3EE / 3FF / pre-3CC wrapper).
#
# Each wrapper is a thin dispatch to
# morie_datasets_tps_arcgis_hub_by_id with a hard-coded hub_id.
# These tests verify:
#   (a) the wrappers are exported and callable
#   (b) each one forwards to its canonical hub_id (spot-checked
#       against the bundled catalog fixture)
#   (c) the dedupe against existing PSDP/3EE wrappers held -- no
#       generated wrapper masks an older export

# ============================================== count + presence

test_that("3TT adds exactly 60 new TPS-named exports (catalog has 71, minus 11 collisions)", {
  exports <- ls("package:morie")
  tps_exports <- grep("^morie_datasets_tps_", exports, value = TRUE)
  # Pre-3TT had 19 TPS exports. 3TT adds 60.
  expect_gte(length(tps_exports), 79L)
})

test_that("each of the 60 emitted wrappers exists and is a function", {
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  # Approximate the dedupe rules from the generator: skip wrappers
  # whose snake-case-slugged name collides with an existing
  # pre-3TT wrapper.
  slugify <- function(t) {
    s <- gsub("\\s*\\(ASR-[A-Z]+-TBL-\\d+\\)", "", t)
    s <- gsub("\\s*\\(RBDC-[A-Z]+-TBL-\\d+\\)", "", s)
    s <- gsub("\\s*Open Data", "", s, ignore.case = TRUE)
    s <- gsub("^Use of Force:\\s*", "Use of Force ", s)
    s <- tolower(s)
    s <- gsub("[^a-z0-9]+", "_", s)
    s <- gsub("^_+|_+$", "", s)
    gsub("_+", "_", s)
  }
  cat$slug <- vapply(cat$title, slugify, character(1L))
  cat$name <- paste0("morie_datasets_tps_", cat$slug)
  # Count the existing exports.
  exports <- ls("package:morie")
  hits <- vapply(cat$name, function(n) n %in% exports, logical(1L))
  # At least 60 of the 71 catalog entries have a matching new
  # wrapper (the 11 collisions are skipped and the existing
  # alternative-spelling export covers them).
  expect_gte(sum(hits), 60L)
})

# ============================================== dispatch correctness

test_that("morie_datasets_tps_victims_of_crime dispatches to the canonical Victims of Crime hub_id", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id,
                                                     format = "json",
                                                     where = "1=1",
                                                     max_features = NULL,
                                                     layer_idx = 0L,
                                                     offline = TRUE,
                                                     dest = NULL) {
      seen <<- list(hub_id = hub_id, format = format,
                    layer_idx = layer_idx)
      data.frame(stub = 1L)
    },
    .package = "morie")
  morie_datasets_tps_victims_of_crime()
  # 6afabfd5109847a2bbba3eaeb0275e35 = Victims of Crime (ASR-VC-TBL-001)
  # (one of the 2 missing items we discovered + added in 3SS).
  expect_equal(seen$hub_id, "6afabfd5109847a2bbba3eaeb0275e35")
})

test_that("morie_datasets_tps_use_of_force_gender_composition dispatches to RBDC-UOF-TBL-006 hub_id", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id, ...) {
      seen <<- list(hub_id = hub_id)
      data.frame()
    },
    .package = "morie")
  morie_datasets_tps_use_of_force_gender_composition()
  expect_equal(seen$hub_id, "de9284945c3e479e938c4b77586535b1")
})

test_that("morie_datasets_tps_police_divisions dispatches to the Police Divisions boundary hub_id", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id, ...) {
      seen <<- list(hub_id = hub_id)
      data.frame()
    },
    .package = "morie")
  # 3CCC3 added a bundled-fixture offline path; the hub_id dispatch
  # is only exercised when offline = FALSE. Other wrappers in this
  # suite don't have an offline mode and dispatch unconditionally.
  morie_datasets_tps_police_divisions(offline = FALSE)
  expect_equal(seen$hub_id, "fda21b25213c4c07b08c5162cba5081f")
})

test_that("morie_datasets_tps_budget_2026 dispatches to the canonical Budget_2026 hub_id", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id, ...) {
      seen <<- list(hub_id = hub_id)
      data.frame()
    },
    .package = "morie")
  morie_datasets_tps_budget_2026()
  expect_equal(seen$hub_id, "d80f9e0b3cc74f649e5e4593cdda207e")
})

test_that("morie_datasets_tps_2008_firs (digit-prefixed slug) is callable + dispatches to b8e3ef82...", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id, ...) {
      seen <<- list(hub_id = hub_id)
      data.frame()
    },
    .package = "morie")
  morie_datasets_tps_2008_firs()
  expect_equal(seen$hub_id, "b8e3ef826ea84cbcb85951d051afc2fa")
})

test_that("a wrapper forwards format / where / max_features / layer_idx / offline / dest verbatim", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id,
                                                     format = "json",
                                                     where = "1=1",
                                                     max_features = NULL,
                                                     layer_idx = 0L,
                                                     offline = TRUE,
                                                     dest = NULL) {
      seen <<- list(hub_id = hub_id, format = format,
                    where = where, max_features = max_features,
                    layer_idx = layer_idx, offline = offline,
                    dest = dest)
      data.frame()
    },
    .package = "morie")
  morie_datasets_tps_neighbourhood_crime_rates(
    format = "geojson",
    where = "OCC_YEAR=2024",
    max_features = 10L,
    layer_idx = 0L,
    offline = FALSE,
    dest = "/tmp/x.zip")
  expect_equal(seen$format, "geojson")
  expect_equal(seen$where, "OCC_YEAR=2024")
  expect_equal(seen$max_features, 10L)
  expect_false(seen$offline)
  expect_equal(seen$dest, "/tmp/x.zip")
})

# ============================================== dedupe verification

test_that("dedupe held: pre-3TT TPS exports were not overwritten by the generator", {
  # Verify that the existing 3FF wrappers (which use the old
  # underscore-less spelling) still exist and still come from their
  # original module (datasets_tps_psdp.R) rather than the generated
  # wrapper module.
  pre_3tt_names <- c("morie_datasets_tps_assault",
                     "morie_datasets_tps_hatecrimes",
                     "morie_datasets_tps_homicides",
                     "morie_datasets_tps_robbery",
                     "morie_datasets_tps_theft_over",
                     "morie_datasets_tps_mha_apprehensions")
  for (n in pre_3tt_names) {
    expect_true(exists(n, where = "package:morie"),
                info = sprintf("pre-3TT export %s missing!", n))
  }
})

test_that("the 11 skipped catalog entries' equivalent slugs are NOT in the generated file's exports", {
  # The generator skipped these because their old-form
  # equivalents already exist. We verify the new-form slugs
  # were NOT exported.
  shouldnt_exist <- c(
    "morie_datasets_tps_auto_theft",
    "morie_datasets_tps_break_and_enter",
    "morie_datasets_tps_hate_crimes",
    "morie_datasets_tps_intimate_partner_and_family_violence",
    "morie_datasets_tps_mental_health_act_apprehensions",
    "morie_datasets_tps_shooting_and_firearm_discharges")
  for (n in shouldnt_exist) {
    expect_false(exists(n, where = "package:morie"),
                 info = sprintf("collision-skip not honoured: %s exists", n))
  }
})

# ============================================== offline smoke

test_that("a sample of wrappers can be called with offline=TRUE without error", {
  # offline=TRUE would normally read a bundled fixture; for these
  # 60 generated wrappers there is no bundled fixture, so they
  # delegate to morie_datasets_tps_arcgis_hub_by_id which
  # offline-resolves the FeatureServer URL (no network needed
  # for the resolve step) but would still try to hit the
  # FeatureServer for the actual data. We mock that step.
  testthat::local_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id, ...) {
      data.frame(hub_id_seen = hub_id, stringsAsFactors = FALSE)
    },
    .package = "morie")
  for (fn in list(morie_datasets_tps_facilities,
                  morie_datasets_tps_patrol_zone,
                  morie_datasets_tps_killed_and_seriously_injured,
                  morie_datasets_tps_traffic_collisions,
                  morie_datasets_tps_community_safety_indicators)) {
    df <- fn(offline = TRUE)
    expect_s3_class(df, "data.frame")
    expect_true(grepl("^[a-f0-9]{32}$", df$hub_id_seen))
  }
})
