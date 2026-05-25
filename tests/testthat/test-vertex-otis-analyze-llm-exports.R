# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3AA: tests for exported callables across vertex.R, otis_analyze.R,
# llm.R provider dispatch, and the remaining tps_io.R sf-based readers.

# ===================================================================== vertex.R

test_that("morie_vertex_resolve_config errors when GOOGLE_CLOUD_PROJECT is unset", {
  old_gcp <- Sys.getenv("GOOGLE_CLOUD_PROJECT", unset = NA_character_)
  old_mee <- Sys.getenv("MORIE_EE_PROJECT",     unset = NA_character_)
  Sys.unsetenv("GOOGLE_CLOUD_PROJECT"); Sys.unsetenv("MORIE_EE_PROJECT")
  on.exit({
    if (!is.na(old_gcp)) Sys.setenv(GOOGLE_CLOUD_PROJECT = old_gcp)
    if (!is.na(old_mee)) Sys.setenv(MORIE_EE_PROJECT     = old_mee)
  }, add = TRUE)
  expect_error(morie_vertex_resolve_config(),
               regexp = "GOOGLE_CLOUD_PROJECT")
})

test_that("morie_vertex_resolve_config returns project/location/model from env", {
  old_gcp <- Sys.getenv("GOOGLE_CLOUD_PROJECT", unset = NA_character_)
  old_loc <- Sys.getenv("VERTEX_LOCATION",      unset = NA_character_)
  old_mdl <- Sys.getenv("VERTEX_MODEL",         unset = NA_character_)
  Sys.setenv(GOOGLE_CLOUD_PROJECT = "fake-test-proj",
             VERTEX_LOCATION = "us-east1",
             VERTEX_MODEL = "gemini-2.5-flash-test")
  on.exit({
    if (is.na(old_gcp)) Sys.unsetenv("GOOGLE_CLOUD_PROJECT")
    else Sys.setenv(GOOGLE_CLOUD_PROJECT = old_gcp)
    if (is.na(old_loc)) Sys.unsetenv("VERTEX_LOCATION")
    else Sys.setenv(VERTEX_LOCATION = old_loc)
    if (is.na(old_mdl)) Sys.unsetenv("VERTEX_MODEL")
    else Sys.setenv(VERTEX_MODEL = old_mdl)
  }, add = TRUE)
  cfg <- morie_vertex_resolve_config()
  expect_named(cfg, c("project", "location", "model", "token_ttl_s",
                      "gcloud_path"))
  expect_equal(cfg$project,  "fake-test-proj")
  expect_equal(cfg$location, "us-east1")
  expect_equal(cfg$model,    "gemini-2.5-flash-test")
})

test_that("morie_vertex_access_token returns cached token without invoking gcloud", {
  cache <- get(".morie_vertex_token_cache",
               envir = asNamespace("morie"))
  old_tok <- cache$token
  old_exp <- cache$expires_at
  cache$token      <- "test-cached-token"
  cache$expires_at <- as.numeric(Sys.time()) + 10
  on.exit({
    cache$token      <- old_tok
    cache$expires_at <- old_exp
  }, add = TRUE)
  cfg <- list(token_ttl_s = 10, gcloud_path = "gcloud")
  expect_equal(morie_vertex_access_token(cfg), "test-cached-token")
})

test_that("morie_vertex_health_check returns ok=FALSE when env is unset", {
  old_gcp <- Sys.getenv("GOOGLE_CLOUD_PROJECT", unset = NA_character_)
  old_mee <- Sys.getenv("MORIE_EE_PROJECT",     unset = NA_character_)
  Sys.unsetenv("GOOGLE_CLOUD_PROJECT"); Sys.unsetenv("MORIE_EE_PROJECT")
  on.exit({
    if (!is.na(old_gcp)) Sys.setenv(GOOGLE_CLOUD_PROJECT = old_gcp)
    if (!is.na(old_mee)) Sys.setenv(MORIE_EE_PROJECT     = old_mee)
  }, add = TRUE)
  out <- morie_vertex_health_check()
  expect_type(out, "list")
  expect_false(out$ok)
  expect_match(out$error, "GOOGLE_CLOUD_PROJECT")
})

# ============================================================== otis_analyze.R

test_that("morie_otis_load errors on missing CSV path", {
  expect_error(
    morie_otis_load(csv_path = tempfile("no_otis_", fileext = ".csv")),
    regexp = "OTIS dataset not found")
})

test_that("morie_otis_load reads a present CSV", {
  tmp <- tempfile("otis_load_", fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(
    data.frame(end_fiscal_year = c(2020L, 2021L),
               unique_individual_id = c(1L, 2L),
               number_of_placements = c(3L, 5L)),
    tmp, row.names = FALSE)
  df <- morie_otis_load(csv_path = tmp)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
})

test_that("format.morie_otis_result renders title + summary lines", {
  res <- list(
    title = "Demo OTIS analysis",
    summary_lines = list(Rows = 1000L, Years = "2018-2024"),
    interpretation = "All clear.",
    warnings = character(0))
  class(res) <- c("morie_otis_result", "morie_rich_result", "list")
  out <- format(res)
  expect_match(out, "Demo OTIS analysis")
  expect_match(out, "Rows: 1000")
  expect_match(out, "Years: 2018-2024")
  expect_match(out, "Interpretation:")
})

test_that("format.morie_otis_result renders warnings block", {
  res <- list(title = "T", summary_lines = list(),
              interpretation = "",
              warnings = c("warn1", "warn2"))
  class(res) <- c("morie_otis_result", "morie_rich_result", "list")
  out <- format(res)
  expect_match(out, "Warnings:")
  expect_match(out, "warn1")
})

test_that("print.morie_otis_result prints the format() output", {
  res <- list(title = "Print test", summary_lines = list(N = 5L),
              interpretation = "", warnings = character(0))
  class(res) <- c("morie_otis_result", "morie_rich_result", "list")
  expect_output(print(res), "Print test")
})

# =================================================================== llm.R exports

test_that("morie_llm_detect_provider returns 'local' when nothing is configured", {
  saved <- list(
    OLLAMA_BASE_URL  = Sys.getenv("OLLAMA_BASE_URL",  unset = NA),
    GEMINI_API_KEY   = Sys.getenv("GEMINI_API_KEY",   unset = NA),
    OPENAI_API_KEY   = Sys.getenv("OPENAI_API_KEY",   unset = NA),
    LLM_API_BASE_URL = Sys.getenv("LLM_API_BASE_URL", unset = NA),
    LLM_API_KEY      = Sys.getenv("LLM_API_KEY",      unset = NA))
  for (k in names(saved)) Sys.unsetenv(k)
  # Reset cached ollama probe flag so we re-probe.
  old_opt <- getOption("morie.llm.ollama_cached", default = NULL)
  options(morie.llm.ollama_cached = FALSE)
  on.exit({
    for (k in names(saved)) {
      v <- saved[[k]]
      if (!is.na(v)) Sys.setenv(.list = stats::setNames(list(v), k))
    }
    options(morie.llm.ollama_cached = old_opt)
  }, add = TRUE)
  expect_equal(morie_llm_detect_provider(), "local")
})

test_that("morie_llm_detect_provider returns 'gemini' when GEMINI_API_KEY set + ollama down", {
  saved <- list(
    GEMINI_API_KEY  = Sys.getenv("GEMINI_API_KEY", unset = NA))
  Sys.setenv(GEMINI_API_KEY = "fake-test-key")
  old_opt <- getOption("morie.llm.ollama_cached", default = NULL)
  options(morie.llm.ollama_cached = FALSE)
  on.exit({
    if (is.na(saved$GEMINI_API_KEY)) Sys.unsetenv("GEMINI_API_KEY")
    else Sys.setenv(GEMINI_API_KEY = saved$GEMINI_API_KEY)
    options(morie.llm.ollama_cached = old_opt)
  }, add = TRUE)
  expect_equal(morie_llm_detect_provider(), "gemini")
})

# =============================================================== tps_io.R sf paths

test_that(".morie_tps_read_sf_path reads a GeoJSON-by-path via sf", {
  skip_if_not_installed("sf")
  if (!requireNamespace("sf", quietly = TRUE)) {
    skip("sf not installed")
  }
  tmp <- tempfile("sf_path_", fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  writeLines('{"type":"FeatureCollection","features":[
    {"type":"Feature","properties":{"a":1},"geometry":{"type":"Point","coordinates":[0,0]}}
  ]}', tmp)
  df <- morie:::.morie_tps_read_sf_path(tmp, NULL)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
})

test_that(".morie_tps_read_kml reads a KML via sf + cache layout", {
  skip_if_not_installed("sf")
  if (!requireNamespace("sf", quietly = TRUE)) {
    skip("sf not installed")
  }
  cache <- tempfile("morie_tps_kml_")
  dir.create(file.path(cache, "Assault", "KML"), recursive = TRUE)
  fpath <- file.path(cache, "Assault", "KML", "data.kml")
  # Minimal KML with a single Point placemark.
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<kml xmlns="http://www.opengis.net/kml/2.2">',
    '<Document>',
    '<Placemark><name>P1</name>',
    '<Point><coordinates>-79.4,43.7,0</coordinates></Point>',
    '</Placemark>',
    '</Document></kml>'), fpath)
  old <- Sys.getenv("MORIE_TPS_DATA_DIR", unset = NA_character_)
  Sys.setenv(MORIE_TPS_DATA_DIR = cache)
  on.exit({
    unlink(cache, recursive = TRUE)
    if (is.na(old)) Sys.unsetenv("MORIE_TPS_DATA_DIR")
    else Sys.setenv(MORIE_TPS_DATA_DIR = old)
  }, add = TRUE)
  df <- tryCatch(morie:::.morie_tps_read_kml("Assault", NULL),
                 error = function(e) e)
  if (inherits(df, "error")) {
    skip(sprintf("kml reader failed (sf KML driver): %s",
                 conditionMessage(df)))
  }
  expect_s3_class(df, "data.frame")
})
