# Coverage tests for R/datasets_tps_arcgis_hub_wrappers.R. The live
# fetch needs the network, so the dispatcher is mocked: each wrapper
# must forward every argument unchanged and pass the hub_id whose
# catalog title slugifies to the wrapper's own name.

tps_wrapper_suffixes <- c(
  "2009_firs", "2010_firs", "2011_firs", "2012_firs", "2013_firs",
  "administrative", "arrested_and_charged_persons",
  "arrests_and_strip_searches", "automobile_ksi", "bicycle_thefts",
  "budget_2020", "budget_2021", "budget_2022", "budget_2023", "budget_2024",
  "budget_2025", "budget_by_command", "calls_for_service_attended",
  "complaint_dispositions", "cyclist_ksi", "dispatched_calls_by_division",
  "fatals_ksi", "firearms_top_calibres", "gross_expenditures_by_division",
  "gross_operating_budget", "investigated_alleged_misconduct",
  "miscellaneous_calls_for_service", "miscellaneous_data",
  "miscellaneous_firearms", "motorcylist_ksi", "passenger_ksi",
  "pedestrian_ksi", "personnel_by_command", "personnel_by_rank",
  "personnel_by_rank_by_division",
  "persons_in_crisis_calls_for_service_attended", "regulated_interactions",
  "reported_crimes", "search_of_persons", "staffing_by_command",
  "tickets_issued", "top_20_offences_of_firearm_seizures",
  "total_public_complaints", "use_of_force_call_for_service_types",
  "use_of_force_call_sources_by_month",
  "use_of_force_location_of_occurrences", "use_of_force_occurrence_category",
  "use_of_force_time_of_day_trends",
  "use_of_force_use_of_force_types_and_perceived_weapons"
)

test_that("every wrapper forwards its arguments to the hub dispatcher", {
  # local_mocked_bindings() cannot rebind the covr-instrumented namespace
  testthat::skip_on_covr()
  seen <- NULL
  local_mocked_bindings(.package = if (isNamespaceLoaded("rmorie")) "rmorie" else "morie", 
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id, format = "json",
                                                   where = "1=1",
                                                   max_features = NULL,
                                                   layer_idx = 0L,
                                                   offline = TRUE,
                                                   dest = NULL) {
      seen <<- list(
        hub_id = hub_id, format = format, where = where,
        max_features = max_features, layer_idx = layer_idx,
        offline = offline, dest = dest
      )
      data.frame(ok = 1L)
    }
  )
  ids <- character(0)
  for (s in tps_wrapper_suffixes) {
    fn <- get(paste0("morie_datasets_tps_", s))
    seen <- NULL
    out <- fn(
      format = "csv", where = "YEAR=2020", max_features = 7L,
      layer_idx = 2L, offline = FALSE, dest = "x.zip"
    )
    expect_identical(out, data.frame(ok = 1L), info = s)
    expect_identical(
      seen[-1],
      list(
        format = "csv", where = "YEAR=2020", max_features = 7L,
        layer_idx = 2L, offline = FALSE, dest = "x.zip"
      ),
      info = s
    )
    expect_match(seen$hub_id, "^[0-9a-f]{32}$", info = s)
    ids[s] <- seen$hub_id
    fn()
    expect_identical(seen[-1], list(
      format = "json", where = "1=1", max_features = NULL,
      layer_idx = 0L, offline = TRUE, dest = NULL
    ), info = s)
  }
  expect_equal(anyDuplicated(ids), 0L)
})

test_that("each hub_id is the catalog entry whose title slugifies to the name", {
  # local_mocked_bindings() cannot rebind the covr-instrumented namespace
  testthat::skip_on_covr()
  skip_if_not_installed("rmoriedata")
  cat <- suppressWarnings(morie_datasets_tps_arcgis_hub_layers(offline = TRUE))
  skip_if(nrow(cat) == 0L, "catalog fixture not bundled")
  slugify <- function(t) {
    s <- gsub("\\s*\\(ASR-[A-Z]+-TBL-\\d+\\)", "", t)
    s <- gsub("\\s*\\(RBDC-[A-Z]+-TBL-\\d+\\)", "", s)
    s <- gsub("\\s*Open Data", "", s, ignore.case = TRUE)
    s <- gsub("^Use of Force:\\s*", "Use of Force ", s)
    s <- gsub("[^a-z0-9]+", "_", tolower(s))
    gsub("_+", "_", gsub("^_+|_+$", "", s))
  }
  seen <- NULL
  local_mocked_bindings(.package = if (isNamespaceLoaded("rmorie")) "rmorie" else "morie", 
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id, ...) {
      seen <<- hub_id
      NULL
    }
  )
  for (s in tps_wrapper_suffixes) {
    get(paste0("morie_datasets_tps_", s))()
    row <- match(seen, cat$hub_id)
    expect_false(is.na(row), info = s)
    expect_identical(slugify(cat$title[row]), s, info = s)
  }
})

test_that("an unknown format is rejected before any network access", {
  expect_error(morie_datasets_tps_budget_2020(format = "xlsx"), "arg")
  expect_error(morie_datasets_tps_reported_crimes(format = "pdf"), "arg")
})
