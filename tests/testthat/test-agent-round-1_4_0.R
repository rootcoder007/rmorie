# The findings of the 1.4.0 fresh-user test agents (l14, 2026-10-03), each with its fix.
.pkg <- utils::packageName(environment(morie_cli))

.cap <- function(...) {
  buf <- character()
  status <- withCallingHandlers(
    morie_cli(c(...), out = function(x) buf <<- c(buf, x)),
    message = function(m) invokeRestart("muffleMessage")
  )
  list(status = status, text = paste(buf, collapse = ""))
}

test_that("the dataset catalog has the 71 keys of the Python package, NAPS and CCHS included", {
  cat_df <- morie_dataset_catalog()
  expect_equal(nrow(cat_df), 71L)
  expect_true(all(c("naps-no2-on-2023", "naps-pm25-ca-2023", "naps-co-on-2023", "cchs22", "siumanifest") %in% cat_df$key))
  naps <- cat_df[cat_df$source == "naps", ]
  expect_equal(nrow(naps), 24L)
  expect_true(all(naps$fetcher == "morie_fetch_naps"))
  args <- .morie_parse_fetcher_args(naps$fetcher_args[naps$key == "naps-no2-on-2023"])
  expect_equal(args, list(pollutant = "no2", year = 2023L, province = "ON"))
  expect_equal(nrow(morie_list_datasets()), 71L + sum(morie_list_datasets()$type == "hosted"))
})

test_that("a NAPS hourly file parses to long rows and -999 hours are dropped", {
  lines <- c(
    "Hourly data // Donnees horaires",
    "",
    "Pollutant//Polluant,NAPS ID//Identifiant SNPA,City//Ville,Province/Territory//Province/Territoire,Latitude//Latitude,Longitude//Longitude,Date//Date,H01//H01,H02//H02,H03//H03",
    "NO2,60101,Ottawa,ON,45.4,-75.7,2023-01-01,12,-999,14.5",
    "NO2,70119,Montreal,QC,45.5,-73.6,2023-01-01,-999,3,"
  )
  out <- .morie_parse_naps_hourly(lines, "no2")
  expect_equal(nrow(out), 3L)
  expect_equal(out$value[out$station_id == "60101"], c(12, 14.5))
  expect_equal(out$datetime_local[out$station_id == "60101"], c("2023-01-01 01:00", "2023-01-01 03:00"))
  expect_equal(unique(out$unit), "ppb")
  expect_equal(out$province[out$station_id == "70119"], "QC")
  expect_error(.morie_parse_naps_hourly(c("x", "y"), "no2"), "Pollutant//Polluant")
})

test_that("morie_fetch_naps() downloads one file, parses it and filters a province", {
  lines <- c(
    "Pollutant//Polluant,NAPS ID//Identifiant SNPA,City//Ville,Province/Territory//Province/Territoire,Latitude//Latitude,Longitude//Longitude,Date//Date,H01//H01",
    "PM25,60101,Ottawa,ON,45.4,-75.7,2023-06-01,8",
    "PM25,70119,Montreal,QC,45.5,-73.6,2023-06-01,9"
  )
  testthat::local_mocked_bindings(
    .morie_dl = function(url, dest, headers = NULL, label = basename(dest), ...) {
      expect_match(url, "PM25_2023.csv")
      expect_true(nzchar(headers[["User-Agent"]]))
      writeLines(lines, dest)
      dest
    },
    .package = .pkg
  )
  on <- morie_fetch_naps(2023, "pm25", province = "ON")
  expect_equal(on$station_id, "60101")
  expect_equal(on$unit, "ug/m3")
  all_prov <- morie_fetch_naps(2023, "pm25")
  expect_equal(nrow(all_prov), 2L)
  expect_error(morie_fetch_naps(2023, "lead"), "pollutant must be one of")
})

test_that("rmorie list-datasets names a route for every key and ends with the footer", {
  r <- .cap("list-datasets")
  expect_equal(r$status, 0L)
  expect_match(r$text, "naps-no2-on-2023 .*ECCC NAPS")
  expect_match(r$text, "cchs22 .*Statistics Canada")
  expect_match(r$text, "mapq .*own file: data/datasets/vsr/TKARONTOMAPQ.xlsx")
  expect_match(r$text, "siumanifest .*rmoriedata \\(CRAN\\)")
  expect_match(r$text, "hibsa .*health-infobase.canada.ca \\(or data.rmorie.com\\)")
  expect_match(r$text, "71 keys: 70 download from their portal, rmoriedata or data.rmorie.com on first use; 1 is your own research file")
  expect_match(r$text, "Curated tables at data.rmorie.com")
})

test_that("inspect, verify and pull without a path print usage and exit 2", {
  for (verb in c("inspect", "verify", "pull")) {
    r <- .cap(verb)
    expect_equal(r$status, 2L, info = verb)
    expect_match(r$text, paste0("usage: rmorie ", verb))
  }
})

test_that("emissions refuses Inf and verify-pollution refuses non-numeric flags", {
  r <- .cap("emissions", "--seconds", "Inf")
  expect_equal(r$status, 2L)
  expect_match(r$text, "positive number, not 'Inf'")
  r <- .cap("verify-pollution", "--pollutant", "no2", "--exposure-mean", "abc", "--exposure-prevalence", "0.5")
  expect_equal(r$status, 2L)
  expect_match(r$text, "--exposure-mean must be a number")
})

test_that("verify-pollution uses one reference concentration and counts avoided deaths over the exposed share", {
  r <- morie_verify_pollution(pollutant = "no2", exposure_mean = 20, exposure_prevalence = 0.5,
                              reference = 5.8, baseline_rate = 500, population = 1e6)
  rr <- exp(log(1.02) * (20 - 5.8) / 10)
  paf <- 0.5 * (rr - 1) / (0.5 * (rr - 1) + 1)
  expect_equal(r$pipeline$burden$reference_conc, 5.8)
  expect_equal(r$pipeline$burden$attributable_cases, paf * 500 / 1e5 * 1e6, tolerance = 1e-9)
  expect_equal(r$pipeline$displaced$deaths_displaced, 500 / 1e5 * 1e6 * 0.5 * (1 - 1 / rr), tolerance = 1e-9)
})

test_that("generate-template takes a positional module, fills the description and never clobbers", {
  d <- withr::local_tempdir()
  r <- .cap("generate-template", "power-design", "--out", file.path(d, "p.md"))
  expect_equal(r$status, 0L)
  txt <- paste(readLines(file.path(d, "p.md")), collapse = "\n")
  expect_false(grepl("[REPLACE_WITH_MODULE_DESCRIPTION]", txt, fixed = TRUE))
  r2 <- .cap("generate-template", "power-design", "--out", file.path(d, "p.md"))
  expect_equal(r2$status, 1L)
  expect_match(r2$text, "already exists")
  r3 <- .cap("generate-template", "--module", "nosuchmodule", "--out", file.path(d, "q.md"))
  expect_equal(r3$status, 1L)
  expect_match(r3$text, "unknown module: nosuchmodule")
  expect_false(file.exists(file.path(d, "q.md")))
})

test_that("exec sees the package's functions without a prefix", {
  r <- .cap("exec", "nrow(morie_list_morie_modules())")
  expect_equal(r$status, 0L)
  expect_match(r$text, "23")
})

test_that("the launcher pins its library with .libPaths() so ~/.Renviron cannot replace it", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  target <- suppressMessages(install_cli(dir = dir))
  txt <- paste(readLines(target), collapse = "\n")
  expect_match(txt, ".libPaths(c(", fixed = TRUE)
  expect_false(grepl("export R_LIBS", txt, fixed = TRUE))
})

test_that("the synthetic-CPADS notice is given once per session", {
  skip_if_not_installed("rmoriedata")
  withr::local_options(morie.cpads_synthetic_noticed = NULL)
  m1 <- capture.output(invisible(morie_load_cpads_data()), type = "message")
  m2 <- capture.output(invisible(morie_load_cpads_data()), type = "message")
  expect_true(any(grepl("synthetic frame", m1)))
  expect_false(any(grepl("synthetic frame", m2)))
})

test_that("edit refuses a terminal editor when stdin is not a terminal", {
  skip_if(isatty(stdin()))
  withr::local_envvar(VISUAL = "", EDITOR = "nano")
  d <- withr::local_tempdir()
  r <- .cap("edit", file.path(d, "f.txt"))
  expect_equal(r$status, 1L)
  expect_match(r$text, "terminal editor")
})
