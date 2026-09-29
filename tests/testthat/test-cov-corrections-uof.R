# Coverage for the Ontario correctional use-of-force loaders. The offline
# path must return exactly the bundled CKAN slice (re-read here with
# utils::read.csv), the synthetic path must reproduce its schema with
# values drawn from the slice's own ranges and levels, and the live CKAN
# path (network) is not exercised.

.uof_loaders <- list(
  incidents = morie_datasets_corrections_uof_incidents,
  inmate_incident = morie_datasets_corrections_uof_inmate_incident,
  staff_incident = morie_datasets_corrections_uof_staff_incident,
  incident_type = morie_datasets_corrections_uof_incident_type,
  institution_summary = morie_datasets_corrections_uof_institution_summary,
  location_summary = morie_datasets_corrections_uof_location_summary,
  select_incident_summary = morie_datasets_corrections_uof_select_incident_summary,
  inmate_participant = morie_datasets_corrections_uof_inmate_participant,
  indigenous = morie_datasets_corrections_uof_indigenous,
  ethnic_origin = morie_datasets_corrections_uof_ethnic_origin,
  race = morie_datasets_corrections_uof_race,
  religion = morie_datasets_corrections_uof_religion
)

.uof_csv <- function(key) {
  pkg <- environmentName(environment(morie_synth_corrections_uof))
  system.file("extdata", sprintf("corrections_uof_%s_sample.csv", key), package = pkg)
}

test_that("resource ids cover the twelve CKAN resources as UUIDs", {
  ids <- morie_corrections_uof_resource_ids()
  expect_setequal(names(ids), names(.uof_loaders))
  expect_true(all(grepl("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$", unlist(ids))))
  expect_identical(anyDuplicated(unlist(ids)), 0L)
})

test_that("each offline loader returns the bundled CKAN slice verbatim", {
  for (key in names(.uof_loaders)) {
    path <- .uof_csv(key)
    expect_true(file.exists(path))
    ref <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    got <- .uof_loaders[[key]]()
    expect_identical(got, ref, info = key)
    expect_identical(.uof_loaders[[key]](source = "bundled"), ref, info = key)
  }
})

test_that("the synthetic generator keeps the slice's schema, ranges and levels", {
  for (key in c("incidents", "race", "institution_summary")) {
    path <- .uof_csv(key)
    expect_true(file.exists(path))
    head5 <- utils::read.csv(path, nrows = 5L, check.names = FALSE, stringsAsFactors = FALSE)
    s <- morie_synth_corrections_uof(key, n = 40L, seed = 3L)
    expect_identical(names(s), names(head5))
    expect_identical(nrow(s), 40L)
    for (col in names(head5)) {
      real <- head5[[col]]
      if (is.numeric(real) && any(!is.na(real))) {
        lo <- floor(min(real, na.rm = TRUE))
        expect_true(all(s[[col]] >= lo & s[[col]] <= max(lo + 1, ceiling(max(real, na.rm = TRUE)))), info = col)
      } else if (any(!is.na(real))) {
        expect_true(all(s[[col]] %in% unique(stats::na.omit(as.character(real)))), info = col)
      }
    }
    expect_identical(s, morie_synth_corrections_uof(key, n = 40L, seed = 3L))
  }
  sy <- morie_datasets_corrections_uof_race(source = "synthetic")
  expect_identical(sy, morie_synth_corrections_uof("race", n = 30L))
  expect_error(morie_synth_corrections_uof("nope"), "no bundled sample")
})
