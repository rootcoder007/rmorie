# The 1.4.0 branches codecov/patch reported without a test: error messages, usage lines and fallbacks.

.pc_cap <- function(...) {
  buf <- character()
  status <- withCallingHandlers(
    morie_cli(c(...), out = function(x) buf <<- c(buf, x)),
    message = function(m) invokeRestart("muffleMessage")
  )
  list(status = status, text = paste(buf, collapse = ""))
}

.pc_out <- function() {
  buf <- character()
  list(out = function(x) buf <<- c(buf, x), text = function() paste(buf, collapse = ""))
}

.pc_entry <- function(...) {
  row <- morie_dataset_catalog()[1L, ]
  for (col in intersect(c("ckan_resource_id", "download_url", "arcgis_url", "fetcher", "fetcher_args",
                          "rmoriedata", "hosted_key", "hosted_file", "zip_member"), names(row))) {
    row[[col]] <- ""
  }
  row$key <- "zzcov"
  row$table_name <- "zzcov_patch_test"
  row$local_path <- file.path("zzcov-not-here", "none.csv")
  set <- list(...)
  for (nm in names(set)) row[[nm]] <- set[[nm]]
  row
}

.pc_mock_catalog <- function(entry, env = parent.frame()) {
  force(entry)
  testthat::local_mocked_bindings(
    .fuzzy_match_key = function(key) "zzcov",
    morie_dataset_catalog = function() entry,
    morie_builtin_db = function() NULL,
    morie_cache_load = function(...) NULL,
    morie_cache_store = function(...) invisible(TRUE),
    .env = env
  )
}

test_that("agent() names the missing launcher instead of failing", {
  withr::local_envvar(PATH = withr::local_tempdir())
  expect_false(agent_available())
  expect_match(agent("compute the mean of 1:10"), "launcher is not on PATH")
})

test_that("morie_bricklayer() points at install_cli() when the launcher is absent", {
  withr::local_envvar(PATH = withr::local_tempdir())
  txt <- paste(utils::capture.output(morie_bricklayer()), collapse = "\n")
  expect_match(txt, "install_cli()", fixed = TRUE)
  expect_match(txt, "Python not found")
})

test_that("morie_e_value() refuses a risk ratio that is not positive", {
  expect_error(morie_e_value(-1), "single positive risk ratio")
  expect_error(morie_e_value(c(2, 3)), "single positive risk ratio")
  expect_error(morie_e_value(2, rr_lower = 0), "rr_lower must be a positive")
})

test_that("a one-parameter BATS model is fitted with Brent's method", {
  y <- 10 + cumsum(c(0.5, -0.3, 0.8, -0.1, 0.4, -0.6, 0.2, 0.7, -0.4, 0.3, 0.1, -0.2, 0.6, -0.5, 0.2, 0.4))
  fit <- morie_bats(y, use_box_cox = FALSE, use_trend = FALSE)
  expect_true(is.finite(fit$aic))
  expect_true(fit$alpha >= 0 && fit$alpha <= 1)
})

test_that("the launcher's old --args separator is dropped before the verb", {
  r <- .pc_cap("--args", "help")
  expect_equal(r$status, 0L)
  expect_match(r$text, "ask")
})

test_that("ask with only the local fallback prints why no model answered and exits 1", {
  testthat::local_mocked_bindings(
    morie_llm_detect_provider = function(...) "local",
    .morie_llm_local_fallback = function(prompt) "local fallback text",
    .cli_llm_fallback_cause = function(model = NULL) "no backend: the cause line\n"
  )
  r <- .pc_cap("ask", "hello")
  expect_equal(r$status, 1L)
  expect_match(r$text, "the cause line")
})

test_that("run-module says when a module wrote nothing and exits 1", {
  testthat::local_mocked_bindings(morie_run_morie_module = function(...) list())
  od <- withr::local_tempdir()
  r <- .pc_cap("run-module", "figures", "--output-dir", file.path(od, "empty"))
  expect_equal(r$status, 1L)
  expect_match(r$text, "figures wrote nothing")
})

test_that("pull muffles connection warnings, writes the CSV, and names a path it cannot write", {
  testthat::local_mocked_bindings(morie_load_dataset = function(key, ...) {
    warning("cannot open URL 'https://example.invalid/x.csv'")
    data.frame(a = 1:3, b = c("x", "y", "z"))
  })
  d <- withr::local_tempdir()
  dest <- file.path(d, "out.csv")
  expect_no_warning(r <- .pc_cap("pull", "zzcov", "--out", dest))
  expect_equal(r$status, 0L)
  expect_match(r$text, "3 rows, 2 cols")
  expect_equal(nrow(utils::read.csv(dest)), 3L)
  r2 <- .pc_cap("pull", "zzcov", "--out", d)  # a directory is not a writable file
  expect_equal(r2$status, 1L)
  expect_match(r2$text, "cannot write")
})

test_that("inspect --help and verify --help print their usage", {
  expect_match(.pc_cap("inspect", "--help")$text, "usage: rmorie inspect")
  expect_match(.pc_cap("verify", "--help")$text, "usage: rmorie verify")
})

test_that("analyze exits 1 when every analysis in the subject failed", {
  testthat::local_mocked_bindings(cli_main = function(subject, json = "{}") {
    '{"one": {"title": "first (failed)"}, "two": {"title": "second (failed)"}}'
  })
  r <- .pc_cap("analyze", "otis")
  expect_equal(r$status, 1L)
  expect_match(r$text, "every analysis in this subject failed")
})

test_that(".cli_analyze_all_failed reads lists, status fields and non-list entries", {
  expect_false(.cli_analyze_all_failed(list()))
  expect_false(.cli_analyze_all_failed("not json"))
  expect_true(.cli_analyze_all_failed(list(status = "error")))
  expect_false(.cli_analyze_all_failed(list(status = "ok")))
  expect_false(.cli_analyze_all_failed(list(a = 1, b = list(title = "t (failed)"))))
  expect_true(.cli_analyze_all_failed(list(b = list(title = "t (failed)"))))
})

test_that("list-datasets routes hosted tables to data.rmorie.com and says how to reach them", {
  base <- data.frame(key = "ocp21", type = "catalog", cached = FALSE, rows = NA_integer_, stringsAsFactors = FALSE)
  hub <- data.frame(key = "hib/demo", type = "hosted", cached = FALSE, rows = NA_integer_, stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(morie_list_datasets = function(...) rbind(base, hub))
  o <- .pc_out()
  expect_equal(.cli_list_datasets(o$out), 0L)
  expect_match(o$text(), "data.rmorie.com (your MORIE key)", fixed = TRUE)
  expect_match(o$text(), "1 curated tables at data.rmorie.com")
  testthat::local_mocked_bindings(morie_list_datasets = function(...) base)
  o2 <- .pc_out()
  .cli_list_datasets(o2$out)
  expect_match(o2$text(), "appear here after `rmorie login`", fixed = TRUE)
})

test_that("inspect and sample print usage, and inspect names an unknown module", {
  no_flag <- function(name) NULL
  o <- .pc_out()
  expect_equal(.cli_inspect(character(), no_flag, o$out), 2L)
  expect_match(o$text(), "usage: rmorie inspect")
  o2 <- .pc_out()
  expect_equal(.cli_inspect("x.csv", function(name) if (identical(name, "--module")) "no-such-module" else NULL, o2$out), 1L)
  expect_match(o2$text(), "unknown module: no-such-module")
  o3 <- .pc_out()
  expect_equal(.cli_sample(character(), no_flag, function(name) FALSE, o3$out), 2L)
  expect_match(o3$text(), "usage: rmorie sample")
})

test_that("the no-answer cause line names the Gemini key, the model, or the default", {
  withr::local_envvar(GEMINI_API_KEY = NA, GOOGLE_API_KEY = NA)
  testthat::local_mocked_bindings(.morie_llm_api_base = function() NULL, .morie_llm_hosted_key = function() NULL)
  expect_match(.cli_llm_fallback_cause(), "no LLM backend answered")
  expect_match(.cli_llm_fallback_cause("llama3"), "no provider answered for model 'llama3'.*models lists")
  withr::local_envvar(GEMINI_API_KEY = "test-key")
  expect_match(.cli_llm_fallback_cause("llama3"), "GEMINI_API_KEY")
})

test_that("a failed read with no Wayback snapshot names the URL and the cause", {
  if (requireNamespace("rmoriebricklayer", quietly = TRUE)) {
    testthat::local_mocked_bindings(wayback_snapshot_url = function(...) NULL, .package = "rmoriebricklayer")
  }
  url <- paste0("file://", file.path(withr::local_tempdir(), "missing.json"))
  expect_error(.morie_read_text(url), "Wayback Machine has no snapshot")
})

test_that("morie_fetch_arcgis draws its progress line when not quiet", {
  testthat::local_mocked_bindings(
    .morie_dl_quiet = function() FALSE,
    .morie_read_text = function(url) {
      if (grepl("returnCountOnly", url)) '{"count": 2}'
      else '{"features": [{"attributes": {"a": 1}}, {"attributes": {"a": 2}}]}'
    }
  )
  progress <- utils::capture.output(res <- morie_fetch_arcgis("https://example.invalid/layer/0"), type = "message")
  expect_equal(res$a, c(1L, 2L))
  expect_true(any(grepl("rows", progress)))
})

test_that("a hosted R environment file opens as an environment", {
  withr::local_envvar(MORIE_DATA_DIR = withr::local_tempdir())
  .pc_mock_catalog(.pc_entry(hosted_file = "zz/env.rda", local_path = "zz/env.rda"))
  testthat::local_mocked_bindings(.morie_data_get = function(path, dest, ...) {
    alpha <- 1:3
    save(alpha, file = dest)
  })
  e <- suppressMessages(morie_load_dataset("zzcov"))
  expect_true(is.environment(e))
  expect_equal(get("alpha", envir = e), 1:3)
})

test_that("a catalogued rmoriedata table loads from that package", {
  .pc_mock_catalog(.pc_entry(rmoriedata = "siu_manifest"))
  if (requireNamespace("rmoriedata", quietly = TRUE)) {
    testthat::local_mocked_bindings(morie_data_load = function(slug, ...) data.frame(slug = slug), .package = "rmoriedata")
    expect_equal(morie_load_dataset("zzcov")$slug, "siu_manifest")
  } else {
    expect_error(morie_load_dataset("zzcov"), "ships in the rmoriedata package")
  }
})

test_that("a catalogued fetcher is called with its parsed arguments", {
  .pc_mock_catalog(.pc_entry(fetcher = "morie_fetch_naps", fetcher_args = "year=2023;pollutant=no2;scale=1.5"))
  testthat::local_mocked_bindings(morie_fetch_naps = function(...) {
    a <- list(...)
    data.frame(year = a$year, pollutant = a$pollutant, scale = a$scale)
  })
  d <- suppressMessages(morie_load_dataset("zzcov"))
  expect_identical(d$year, 2023L)
  expect_equal(d$scale, 1.5)
  expect_equal(d$pollutant, "no2")
})

test_that("a failed portal download without a hosted copy re-raises the portal error", {
  .pc_mock_catalog(.pc_entry(download_url = "https://example.invalid/data.csv"))
  testthat::local_mocked_bindings(morie_fetch = function(...) stop("portal down for maintenance"))
  expect_error(suppressMessages(morie_load_dataset("zzcov")), "portal down for maintenance")
})

test_that("the data directory falls back to the per-user R data directory", {
  withr::local_envvar(MORIE_DATA_DIR = NA)
  expect_identical(.morie_data_root(), tools::R_user_dir("morie", which = "data"))
})

test_that("the NAPS parser handles an empty file, missing columns and all-missing hours", {
  hdr <- "Pollutant//Polluant,Method Code//M,NAPS ID//N,City//V,Province/Territory//P,Latitude//L,Longitude//L,Date//D,H01//H01,H02//H02"
  expect_equal(nrow(.morie_parse_naps_hourly(c("preamble", hdr), "no2")), 0L)
  expect_error(.morie_parse_naps_hourly(c("Pollutant//Polluant,Date//D,H01//H01", "NO2,2023-01-01,5"), "no2"), "lacks column")
  allmiss <- "NO2,1,10101,Ottawa,ON,45.4,-75.7,2023-01-01,-999,-999"
  expect_equal(nrow(.morie_parse_naps_hourly(c(hdr, allmiss), "no2")), 0L)
})

test_that("morie_fetch_naps says when the year has no rows for the pollutant", {
  testthat::local_mocked_bindings(.morie_dl = function(url, dest, ...) {
    writeLines("Pollutant//Polluant,NAPS ID//N,City//V,Province/Territory//P,Latitude//L,Longitude//L,Date//D,H01//H01", dest)
  })
  expect_error(morie_fetch_naps(2023, "no2"), "NAPS has no NO2 hourly file for 2023")
})

test_that("fetcher arguments parse integers, decimals, text and an empty spec", {
  expect_identical(.morie_parse_fetcher_args(""), list())
  expect_identical(.morie_parse_fetcher_args(NULL), list())
  a <- .morie_parse_fetcher_args("year=2023; scale=0.5; province=ON")
  expect_identical(a, list(year = 2023L, scale = 0.5, province = "ON"))
})

test_that("the cheat sheet lists the issue tracker", {
  txt <- paste(utils::capture.output(body <- .explain_cheatsheet()), collapse = "\n")
  expect_match(body, "Issues:", fixed = TRUE)
  expect_match(txt, "github.com/rootcoder007/", fixed = TRUE)
})

test_that("a CKAN resource of unknown type is read as CSV", {
  skip_if_not_installed("readr")
  p <- withr::local_tempfile(fileext = ".dat")
  writeLines(c("a,b", "1,x", "2,y"), p)
  d <- .morie_ckan_read_path(p, "dat")
  expect_equal(nrow(d), 2L)
  expect_equal(d$b, c("x", "y"))
})

test_that("an endpoint that refuses the connection probes as unreachable", {
  skip_if_not_installed("httr2")
  testthat::local_mocked_bindings(
    .morie_llm_api_base = function() "http://127.0.0.1:9/v1",
    .morie_llm_api_key = function() "sk-test",
    .morie_llm_no_net = function() FALSE
  )
  expect_false(.morie_llm_probe_api(timeout = 2))
})

test_that("the device sign-in polls and reports a service error", {
  testthat::local_mocked_bindings(.morie_llm_hosted_auth = function() "https://auth.example.invalid")
  testthat::local_mocked_bindings(  # the device code is issued, then the token endpoint answers 503
    .morie_llm_http = function(url, body = NULL, headers = character(), timeout = 30) {
      if (grepl("/device/code$", url)) {
        return(list(status = 200L, body = '{"verification_uri":"https://auth.example.invalid/device","user_code":"ABCD","device_code":"dc","interval":0.01}'))
      }
      list(status = 503L, body = "{}")
    }
  )
  expect_error(suppressMessages(morie_llm_login(open_browser = FALSE, poll_max_seconds = 5)), "answered 503")
})

test_that("glm-based separation check reports none when large fitted values come without separation", {
  x <- cbind(z = c(-1000, -2, -1, 0, 1, 2, 1000))
  y <- c(0, 1, 0, 1, 0, 1, 1)
  r <- morie_logit_separation(y, x, method = "glm")
  expect_equal(r$separation, "none")
})

test_that("the OTIS and MAPQ modules announce their synthetic data", {
  testthat::local_mocked_bindings(
    morie_load_cpads_data = function(...) NULL,
    .run_otis_analysis_module_internal = function(...) list(otis = TRUE),
    .run_mapq_psychometrics_module_internal = function(...) list(mapq = TRUE)
  )
  expect_message(r1 <- morie_run_morie_module("otis-analysis", cpads_csv = "unused.csv"), "synthetic OTIS frame")
  expect_message(r2 <- morie_run_morie_module("mapq-psychometrics", cpads_csv = "unused.csv"), "synthetic MAPQII panel")
  expect_true(length(r1) >= 1L && length(r2) >= 1L)
})

test_that("a zero or invalid numeric entity decodes to nothing", {
  expect_identical(.siu_decode_numeric_entities("a&#0;b&#x41;"), "abA")
})

test_that("the hosted SIU provider builds a chat request and rejects empty text", {
  withr::local_envvar(MORIE_HOSTED_MODEL = NA)
  testthat::local_mocked_bindings(
    .morie_llm_hosted_base = function() "https://llm.example.invalid/",
    morie_llm_hosted_models = function(...) structure(c("m1", "m2"), default = "m1")
  )
  p <- .siu_llm_providers()$hosted
  req <- p$build(list(MORIE_HOSTED_KEY_OR_LOGIN = "sk-test"), "summarise")
  expect_equal(req$url, "https://llm.example.invalid/v1/chat/completions")
  expect_equal(req$headers$authorization, "Bearer sk-test")
  expect_equal(req$body$model, "m1")
  expect_equal(p$extract(list(choices = list(list(message = list(content = "ok"))))), "ok")
  expect_error(p$extract(list(choices = list(list(message = list())))), "empty text")
})
