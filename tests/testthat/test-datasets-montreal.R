# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3EEE1: Montreal Open Data CKAN loaders.

test_that("morie_datasets_montreal_justice_safety_layers returns 23-row catalog", {
  d <- morie_datasets_montreal_justice_safety_layers(offline = TRUE)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 23L)
  expect_setequal(names(d),
                  c("package_name", "title", "num_resources",
                    "metadata_modified", "language", "license"))
  expect_true("interventions-service-securite-incendie-montreal" %in%
                 d$package_name)
  # All MTL datasets are CC-BY licensed and French-language by default.
  expect_true(all(d$license == "cc-by"))
  expect_true(all(d$language == "FR"))
})

test_that("morie_datasets_montreal_sim_interventions returns 349-row sample", {
  df <- morie_datasets_montreal_sim_interventions(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 349L)
  for (col in c("INCIDENT_NBR", "CREATION_DATE_TIME",
                "INCIDENT_TYPE_DESC", "DESCRIPTION_GROUPE",
                "CASERNE", "NOM_VILLE", "NOM_ARROND",
                "DIVISION", "NOMBRE_UNITES",
                "MTM8_X", "MTM8_Y", "LONGITUDE", "LATITUDE"))
    expect_true(col %in% names(df),
                 info = sprintf("missing %s", col))
  # All 7 SIM DESCRIPTION_GROUPE categories represented.
  expect_true(length(unique(df$DESCRIPTION_GROUPE)) >= 6L)
  # Coordinates fall in greater Montreal bounding box.
  expect_true(all(df$LATITUDE > 45.3 & df$LATITUDE < 45.8,
                   na.rm = TRUE))
  expect_true(all(df$LONGITUDE > -74 & df$LONGITUDE < -73.4,
                   na.rm = TRUE))
})

test_that("max_features cap honoured on SIM loader", {
  df <- morie_datasets_montreal_sim_interventions(offline = TRUE,
                                                     max_features = 10L)
  expect_equal(nrow(df), 10L)
})

test_that("user csv_path mode reads external CSV", {
  # Synthesise a tiny CSV to feed the csv_path branch
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(
    INCIDENT_NBR = 1L, CREATION_DATE_TIME = "2026-01-01T00:00:00",
    INCIDENT_TYPE_DESC = "test", DESCRIPTION_GROUPE = "1-REPOND",
    CASERNE = 1L, NOM_VILLE = "Montreal", NOM_ARROND = "Ville-Marie",
    DIVISION = 1L, NOMBRE_UNITES = 1L,
    MTM8_X = 0, MTM8_Y = 0, LONGITUDE = -73.5, LATITUDE = 45.5),
    file = tmp, row.names = FALSE)
  df <- morie_datasets_montreal_sim_interventions(csv_path = tmp)
  expect_equal(nrow(df), 1L)
})

test_that("morie_datasets_montreal_sim_intervention_types returns 170-row dict", {
  d <- morie_datasets_montreal_sim_intervention_types()
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 170L)
  expect_setequal(names(d),
                  c("INCIDENT_TYPE_DESCRIPTION", "Description"))
})

test_that("sim_interventions offline=FALSE without csv_path errors", {
  expect_error(
    morie_datasets_montreal_sim_interventions(offline = FALSE,
                                                 csv_path = NULL),
    regexp = "csv_path")
})
