# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3GGG1-5: bulk catalog snapshots from 6 portals.

test_that("NYC OpenData bulk catalog reads 2851 entities", {
  d <- morie_datasets_nyc_opendata_bulk_layers(offline = TRUE)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 2851L)
  expect_true("soda_id" %in% names(d))
  expect_true("type" %in% names(d))
  # Confirm presence of canonical NYPD soda_id.
  expect_true("uip8-fykc" %in% d$soda_id)
})

test_that("NYC bulk includes the 294 maps Vee called out", {
  d <- morie_datasets_nyc_opendata_bulk_layers(offline = TRUE)
  expect_true(sum(d$type == "map") >= 290L)
})

test_that("Chicago Open Data bulk catalog reads 1856 entities", {
  d <- morie_datasets_chicago_opendata_bulk_layers(offline = TRUE)
  expect_equal(nrow(d), 1856L)
  expect_true("soda_id" %in% names(d))
  expect_true("ijzp-q8t2" %in% d$soda_id)  # Chicago Crimes
})

test_that("Toronto Open Data bulk catalog reads 540 packages", {
  d <- morie_datasets_toronto_opendata_bulk_layers(offline = TRUE)
  expect_equal(nrow(d), 540L)
  expect_true("package_name" %in% names(d))
  expect_true("ambulance-station-locations" %in% d$package_name)
})

test_that("Calgary Open Data bulk catalog reads 933 entities", {
  d <- morie_datasets_calgary_opendata_bulk_layers(offline = TRUE)
  expect_equal(nrow(d), 933L)
  expect_true("78gh-n26t" %in% d$soda_id)  # Community Crime Statistics
})

test_that("Edmonton Open Data bulk catalog reads 2027 entities", {
  d <- morie_datasets_edmonton_opendata_bulk_layers(offline = TRUE)
  expect_equal(nrow(d), 2027L)
  expect_true("e7aq-scxv" %in% d$soda_id)  # Police Stations
})

test_that("Ottawa Open Data Hub bulk catalog reads 287 datasets", {
  d <- morie_datasets_ottawa_opendata_bulk_layers(offline = TRUE)
  expect_equal(nrow(d), 287L)
  expect_true("hub_id" %in% names(d))
  expect_true(all(d$source == "City of Ottawa"))
})

test_that("live mode errors on all eight bulk loaders", {
  for (fn in list(morie_datasets_nyc_opendata_bulk_layers,
                   morie_datasets_chicago_opendata_bulk_layers,
                   morie_datasets_toronto_opendata_bulk_layers,
                   morie_datasets_calgary_opendata_bulk_layers,
                   morie_datasets_edmonton_opendata_bulk_layers,
                   morie_datasets_ottawa_opendata_bulk_layers,
                   morie_datasets_montreal_opendata_bulk_layers,
                   morie_datasets_vancouver_opendata_bulk_layers)) {
    expect_error(fn(offline = FALSE), regexp = "Live re-harvest")
  }
})

# ========================================== 3HHH1-2

test_that("Montreal Open Data BULK reads 401 packages", {
  d <- morie_datasets_montreal_opendata_bulk_layers(offline = TRUE)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 401L)
  expect_true("package_name" %in% names(d))
  expect_true("interventions-service-securite-incendie-montreal" %in%
                 d$package_name)
})

test_that("Vancouver Open Data BULK reads 190 datasets (full Opendatasoft)", {
  d <- morie_datasets_vancouver_opendata_bulk_layers(offline = TRUE)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 190L)
  expect_true("dataset_id" %in% names(d))
  # Verify it carries the enriched fields beyond the 3CCC4 fixture.
  expect_true("records_count" %in% names(d))
})

test_that("Catalog totals reflect bulk fixtures (>= 8800 rows)", {
  d <- morie_dataset_portal_catalog()
  expect_true(nrow(d) >= 8800L)
})

test_that("NYC Socrata-by-id wrapper signature accepts soda_id + limit", {
  args <- formals(morie_datasets_nyc_socrata_by_id)
  expect_true("soda_id" %in% names(args))
  expect_true("limit" %in% names(args))
})

test_that("Chicago Socrata-by-id wrapper signature accepts soda_id + limit", {
  args <- formals(morie_datasets_chicago_socrata_by_id)
  expect_true("soda_id" %in% names(args))
  expect_true("limit" %in% names(args))
})
