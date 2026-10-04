# The in-package command line: verb dispatch, exit codes, the launcher and
# install_cli(). Network verbs are mocked; nothing leaves the machine.

.pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"

.capture <- function(...) {
  buf <- character()
  status <- morie_cli(c(...), out = function(s) buf <<- c(buf, s))
  list(text = paste(buf, collapse = ""), status = status)
}

test_that("version, help and unknown verbs behave", {
  v <- .capture("version")
  expect_equal(v$status, 0L)
  expect_match(v$text, as.character(utils::packageVersion(.pkg)), fixed = TRUE)
  h <- .capture("help")
  expect_match(h$text, "login \\[--email ADDRESS\\]")
  expect_match(.capture()$text, "usage: rmorie")
  u <- .capture("frobnicate")
  expect_equal(u$status, 1L)
  expect_match(u$text, "unknown verb 'frobnicate'")
})

test_that("login forwards the flags to morie_llm_login and logout forgets the key", {
  withr::local_envvar(XDG_CONFIG_HOME = withr::local_tempdir())
  seen <- NULL
  testthat::local_mocked_bindings(
    .package = .pkg,
    morie_llm_login = function(open_browser = TRUE, poll_max_seconds = 600, email = NULL, code = NULL,
                               token = NULL, to_email = FALSE) {
      seen <<- list(email = email, code = code, open_browser = open_browser, token = token, to_email = to_email)
      invisible(if (isTRUE(to_email)) "" else "sk-x")
    })
  r <- .capture("login", "--email", "vee@example.com", "--code", "123456", "--no-browser")
  expect_equal(r$status, 0L)
  expect_equal(seen$email, "vee@example.com")
  expect_equal(seen$code, "123456")
  expect_false(seen$open_browser)
  expect_match(r$text, "Logged in to https://llm.rmorie.com")
  expect_equal(.capture("login", "--email")$status, 2L)  # a flag without its value is a usage error
  r2 <- .capture("login", "--email", "vee@example.com", "--code", "1", "--to-email")
  expect_equal(r2$status, 0L)
  expect_true(seen$to_email)
  expect_false(grepl("Logged in", r2$text, fixed = TRUE))
  testthat::local_mocked_bindings(.package = .pkg, .morie_llm_probe_token = function(token) identical(token, "sk-pasted"))
  testthat::local_mocked_bindings(.package = .pkg, .morie_llm_probe_token = function(token) identical(token, "sk-pasted"))
  expect_equal(.capture("login", "--token", "sk-pasted")$status, 0L)
  expect_equal(seen$token, "sk-pasted")
  expect_equal(.capture("login", "--token", "sk-rejected")$status, 1L)
  expect_equal(seen$token, "sk-pasted")  # a rejected key is never stored
  expect_equal(.capture("login", "--token", "sk-rejected")$status, 1L)
  expect_equal(seen$token, "sk-pasted")  # a rejected key is never stored
  .morie_llm_write_credentials(list(hosted_key = "sk-x"))
  expect_equal(suppressMessages(.capture("logout"))$status, 0L)
  expect_null(.morie_llm_hosted_key())
})

test_that("doctor lists the providers and ask relays the reply", {
  withr::local_envvar(XDG_CONFIG_HOME = withr::local_tempdir(), GEMINI_API_KEY = NA,
                      LLM_API_BASE_URL = NA, LLM_API_KEY = NA, OPENAI_API_KEY = NA)
  .morie_llm_cache$ollama_cached <- FALSE
  .morie_llm_cache$hosted_cached <- NULL
  withr::defer({ .morie_llm_cache$ollama_cached <- NULL; .morie_llm_cache$hosted_cached <- NULL })
  d <- .capture("doctor")
  expect_equal(d$status, 0L)
  expect_match(d$text, "Ollama \\(local\\) +not reachable")
  expect_match(d$text, "not logged in")
  expect_match(d$text, "active provider: local")
  testthat::local_mocked_bindings(.package = .pkg, morie_llm_ask = function(prompt, ...) paste("echo:", prompt),
                                  morie_llm_detect_provider = function(...) "hosted")
  a <- .capture("ask", "what", "is", "MORIE")
  expect_equal(a$text, "echo: what is MORIE\n")
  expect_match(.capture("ask", "--help")$text, "usage: rmorie ask")
})

test_that("models lists the hosted and local models and ask --model names one", {
  withr::local_envvar(XDG_CONFIG_HOME = withr::local_tempdir())
  .morie_llm_cache$ollama_cached <- FALSE
  .morie_llm_cache$hosted_cached <- NULL
  withr::defer({ .morie_llm_cache$ollama_cached <- NULL; .morie_llm_cache$hosted_cached <- NULL
                 .morie_llm_cache$hosted_models <- NULL })
  expect_match(.capture("models")$text, "Hosted LLM: not logged in")
  testthat::local_mocked_bindings(.package = .pkg,
    .morie_llm_hosted_key = function() "sk-test",
    morie_llm_probe_hosted = function(...) { .morie_llm_cache$hosted_models <- c("a:cloud", "b:cloud"); TRUE },
    .morie_llm_hosted_model = function() "b:cloud")
  m <- .capture("models")
  expect_equal(m$status, 0L)
  expect_match(m$text, "Hosted LLM \\(https://llm.rmorie.com\\); default marked \\*:")
  expect_match(m$text, "\n    a:cloud\n  \\* b:cloud\n")
  expect_match(m$text, "Local Ollama: not reachable")
  hm <- morie_llm_hosted_models()
  expect_equal(as.character(hm), c("a:cloud", "b:cloud"))
  expect_equal(attr(hm, "default"), "b:cloud")
  expect_match(.capture("doctor")$text, "models: a:cloud, b:cloud \\(default b:cloud\\)")
  seen <- NULL
  testthat::local_mocked_bindings(.package = .pkg,
    morie_llm_ask = function(prompt, model = NULL, ...) { seen <<- model; paste("echo:", prompt) },
    morie_llm_detect_provider = function(...) "hosted")
  expect_equal(.capture("ask", "--model", "a:cloud", "hi", "there")$text, "echo: hi there\n")
  expect_equal(seen, "a:cloud")
  expect_equal(.capture("ask", "hi")$text, "echo: hi\n")
  expect_null(seen)
  expect_match(.capture("ask", "--help")$text, "usage: rmorie ask \\[--model NAME\\]")
})

test_that("the launcher ships and install_cli links it", {
  src <- system.file("bin", "rmorie", package = .pkg)
  expect_true(nzchar(src))
  expect_match(readLines(src)[1], "^#!/bin/sh")
  expect_true(any(grepl("morie_cli", readLines(src), fixed = TRUE)))
  dir <- withr::local_tempdir()
  target <- suppressMessages(install_cli(dir = dir))
  expect_true(file.exists(target))
  if (.Platform$OS.type != "windows") {
    expect_true(file.access(target, 1L) == 0L)  # executable
    expect_match(paste(readLines(target), collapse = "\n"), "morie_cli", fixed = TRUE)
  }
})

test_that("list-modules, cheatsheet, pull and run-module verbs work", {
  expect_match(.capture("list-modules")$text, "power-design")
  expect_equal(length(morie_module_names()), 23L)
  cs <- .capture("cheatsheet")
  expect_equal(cs$status, 0L)
  expect_match(cs$text, "rmorie login")
  expect_match(cs$text, "run-module power-design")
  expect_match(.capture("pull")$text, "usage: rmorie pull KEY")
  expect_match(.capture("run-module")$text, "usage: rmorie run-module NAME")
  testthat::local_mocked_bindings(.package = .pkg,
    morie_load_dataset = function(key, ...) data.frame(k = key, n = 1:3))
  dest <- tempfile(fileext = ".csv")
  p <- .capture("pull", "ocp21", "--out", dest)
  expect_equal(p$status, 0L)
  expect_match(p$text, "3 rows, 2 cols")
  expect_equal(nrow(utils::read.csv(dest)), 3L)
  testthat::local_mocked_bindings(.package = .pkg,
    morie_run_morie_module = function(module_name, cpads_csv, output_dir = NULL) {
      utils::write.csv(data.frame(x = 1), file.path(output_dir, "a.csv"), row.names = FALSE)  # it wrote a table
      list(a = 1, b = 2)
    })
  r <- .capture("run-module", "power-design", "--output-dir", withr::local_tempdir())
  expect_equal(r$status, 0L)
  expect_match(r$text, "Generated tables: a, b")
})

test_that("provider set/show/unset store an endpoint the chain reads", {
  withr::local_envvar(XDG_CONFIG_HOME = withr::local_tempdir(),
                      LLM_API_BASE_URL = NA, LLM_API_KEY = NA, MORIE_API_MODEL = NA)
  expect_null(.morie_llm_api_base())
  r <- .capture("provider", "set", "--base-url", "https://api.example.org/v1/", "--key", "sk-test-1234567", "--model", "demo-model")
  expect_equal(r$status, 0L)
  expect_equal(.morie_llm_api_base(), "https://api.example.org/v1")
  expect_equal(.morie_llm_api_key(), "sk-test-1234567")
  expect_equal(.morie_llm_api_model(), "demo-model")
  expect_true(file.exists(.morie_llm_credentials_path()))
  expect_equal(.capture("provider", "show")$status, 0L)
  expect_equal(.capture("provider", "unset")$status, 0L)
  expect_null(.morie_llm_api_base())
  expect_match(.capture("provider", "bogus")$text, "usage: rmorie provider")
  expect_equal(.capture("provider", "set", "--key", "x")$status, 2L)  # --base-url missing: usage
})


test_that("output verbs: explain, inspect and verify", {
  expect_match(.capture("explain", "power_two_proportion_gender.csv")$text, "group1, group2")
  d <- withr::local_tempdir()
  f <- file.path(d, "t.csv")
  utils::write.csv(data.frame(statistic = c(1.5, 2), p_value = c(0.05, 0.01)), f, row.names = FALSE)
  r <- .capture("inspect", f)
  expect_equal(r$status, 0L)
  expect_match(r$text, "rows: 2")
  expect_equal(.capture("inspect", file.path(d, "nope.csv"))$status, 1L)
  expect_match(.capture("inspect", d)$text, "t.csv")
  v <- .capture("verify", f)
  expect_true(v$status %in% c(0L, 1L))
  expect_match(v$text, "PASS|FAIL")
})

test_that("profile-dataset and sample work on a CSV", {
  d <- withr::local_tempdir()
  f <- file.path(d, "d.csv")
  set.seed(1)
  utils::write.csv(data.frame(treated = rep(0:1, 50), y = rnorm(100), g = rep(c("a", "b"), each = 50)),
                   f, row.names = FALSE)
  p <- .capture("profile-dataset", f, "--treatment", "treated", "--outcome", "y", "--suggest")
  expect_equal(p$status, 0L)
  expect_match(p$text, "Dataset Profile")
  expect_match(p$text, "Suggested Analysis Plan")
  expect_equal(.capture("profile-dataset")$status, 2L)
  s <- .capture("sample", f, "--n", "7", "--output", file.path(d, "s.csv"))
  expect_equal(s$status, 0L)
  expect_equal(nrow(utils::read.csv(file.path(d, "s.csv"))), 7L)
  expect_match(.capture("sample", f, "--n", "3", "--method", "stratified")$text, "strata-col")
  st <- .capture("sample", f, "--n", "2", "--method", "stratified", "--strata-col", "g")
  expect_match(st$text, "Sampled 2 rows")   # --n is the total; the strata share it
  each <- .capture("sample", f, "--n", "2", "--method", "stratified", "--strata-col", "g", "--per-stratum")
  expect_match(each$text, "Sampled 4 rows")
})

test_that("run-modules and pipeline run through the module runner", {
  calls <- list()
  testthat::local_mocked_bindings(.package = .pkg,
    morie_run_morie_modules = function(modules, ...) {
      calls[[length(calls) + 1L]] <<- modules
      stats::setNames(lapply(modules, function(m) list(a = 1)), modules)
    })
  r <- .capture("run-modules", "--modules", "power-design,descriptive-statistics")
  expect_equal(r$status, 0L)
  seen <- NULL
  testthat::local_mocked_bindings(.package = .pkg,
    morie_load_dataset = function(key, ...) data.frame(k = key),
    morie_run_morie_module = function(module_name, cpads_csv = NULL, output_dir = NULL, ...) {
      seen <<- cpads_csv
      if (!is.null(output_dir)) {  # a module that writes nothing is reported as a failure by the verb
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        utils::write.csv(data.frame(a = 1), file.path(output_dir, "a.csv"), row.names = FALSE)
      }
      list(a = 1)
    })
  od <- withr::local_tempdir()
  expect_equal(.capture("run-module", "power-design", "--dataset", "ocp21", "--output-dir", od)$status, 0L)
  testthat::local_mocked_bindings(.package = .pkg,
    morie_list_datasets = function(...) data.frame(key = c("ocp21", "bad1")),
    morie_load_dataset = function(key, ...) if (key == "bad1") stop("offline") else data.frame(k = key))
  od <- withr::local_tempdir()
  a <- .capture("pull", "--all", "--out", od)
  expect_equal(a$status, 0L)
  expect_true(file.exists(file.path(od, "ocp21.csv")))
  expect_match(a$text, "bad1 +FAILED: offline")
  expect_match(seen, "dataset-ocp21\\.csv$")
  expect_equal(utils::read.csv(seen)$k, "ocp21")
  expect_match(r$text, "Completed modules: power-design, descriptive-statistics")
  expect_match(.capture("pipeline")$text, "usage: rmorie pipeline")
  withr::local_envvar(MORIE_EMISSIONS_OFFLINE = "1")
  d <- withr::local_tempdir()
  p <- .capture("pipeline", "--modules", "power-design", "--output-dir", d)
  expect_equal(p$status, 0L)
  expect_match(p$text, "Pipeline CO2 emissions")
  expect_true(file.exists(file.path(d, "emissions", "emissions.csv")))
  expect_true(file.exists(file.path(d, "emissions", "emissions_manifest.json")))
})

test_that("percy, agent and chat go through morie_llm_ask", {
  testthat::local_mocked_bindings(.package = .pkg,
    morie_llm_ask = function(prompt, context = NULL, model = NULL, ...) paste0("[", model %||% "default", "] ", prompt),
    morie_llm_detect_provider = function() "hosted")
  expect_match(.capture("percy", "what", "is", "a", "PAF")$text, "\\[default\\] what is a PAF")
  expect_match(.capture("agent", "--model", "m1", "hi")$text, "\\[m1\\] hi")
  expect_match(.capture("percy", "--context", "ctx", "q")$text, "Context:\nctx")
  expect_equal(.capture("percy")$status, 2L)
  answers <- c("what is a PAF", "/quit")
  testthat::local_mocked_bindings(.package = .pkg,
    .cli_readline = function(prompt) {
      a <- answers[[1L]]
      answers <<- answers[-1L]
      a
    })
  r <- .capture("chat")
  expect_match(r$text, "percy> \\[default\\] what is a PAF")
  expect_equal(r$status, 0L)
  expect_match(r$text, "Bye!")
})

test_that("tutorial --dry-run, generate-template, exec and verify-earth-engine", {
  t <- .capture("tutorial", "--dry-run")
  expect_equal(t$status, 0L)
  expect_match(t$text, "STEP 3")
  expect_match(t$text, "rmorie run-module power-design")
  d <- withr::local_tempdir()
  g <- .capture("generate-template", "--module", "hawkes", "--out", file.path(d, "p.md"))
  expect_equal(g$status, 0L)
  expect_match(paste(readLines(file.path(d, "p.md")), collapse = "\n"), "hawkes")
  e <- .capture("exec", "1 + 41")
  expect_equal(e$status, 0L)
  expect_match(e$text, "42")
  expect_equal(.capture("exec")$status, 2L)
  expect_equal(.capture("verify-earth-engine")$status, 2L)
})

test_that("crypto keygen/encrypt/decrypt round-trip through files and the keystore", {
  skip_if_not(isTRUE(tryCatch(morie_crypto_liboqs_available(), error = function(e) FALSE)) &&
                isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE)),
              "ML-KEM needs liboqs and ChaCha20 needs libsodium")
  expect_equal(.capture("crypto")$status, 2L)
  d <- withr::local_tempdir()
  withr::local_envvar(HOME = d, MORIE_KEYSTORE_PASSWORD = "pw-test")
  k <- .capture("crypto", "keygen", "--name", "alice", "--output", file.path(d, "keys"))
  expect_equal(k$status, 0L)
  expect_true(file.exists(file.path(d, "keys", "alice.moriepk")))
  f <- file.path(d, "secret.txt")
  writeLines("hello capsule", f)
  e <- .capture("crypto", "encrypt", f, "--to", file.path(d, "keys", "alice.moriepk"))
  expect_equal(e$status, 0L)
  expect_true(file.exists(paste0(f, ".morieenc")))
  expect_equal(.capture("crypto", "encrypt", file.path(d, "missing"), "--recipient", "x")$status, 1L)
  kk <- .capture("crypto", "keygen", "--name", "bob")
  expect_equal(kk$status, 0L)
  g <- file.path(d, "s2.txt")
  writeLines("second", g)
  expect_equal(.capture("crypto", "encrypt", g, "--recipient", "bob")$status, 0L)
  unlink(g)
  expect_equal(.capture("crypto", "decrypt", paste0(g, ".morieenc"), "--key", "bob")$status, 0L)
  expect_equal(readLines(g), "second")
})

test_that("ingest dispatches to the portal functions", {
  testthat::local_mocked_bindings(.package = .pkg,
    morie_ingest_tps_layers = function() data.frame(name = "mci", url = "u"),
    morie_ingest_ckan_search_packages = function(portal, query, rows = 50L, ...) data.frame(portal = portal, q = query, rows = rows),
    morie_ingest_a2aj_coverage = function(doc_type = "cases", ...) data.frame(doc_type = doc_type))
  expect_match(.capture("ingest", "tps", "--list")$text, "mci")
  r <- .capture("ingest", "ckan", "--portal", "https://p", "--search", "crime", "--rows", "3")
  expect_match(r$text, "crime")
  expect_match(.capture("ingest", "a2aj", "coverage", "--doc-type", "laws")$text, "laws")
  expect_equal(.capture("ingest", "nope")$status, 2L)
  expect_equal(.capture("ingest", "ckan")$status, 2L)
})

test_that("download-bootstrap, percysuits and update report honestly", {
  testthat::local_mocked_bindings(.package = .pkg,
    morie_load_dataset = function(key, ...) data.frame(w = seq_len(3)))
  r <- .capture("download-bootstrap", "--survey", "csads_2021")
  expect_equal(r$status, 0L)
  expect_match(r$text, "OK: 3 rows cached")
  expect_equal(.capture("download-bootstrap", "--survey", "zzz_1999")$status, 1L)
  testthat::local_mocked_bindings(.package = .pkg, morie_llm_probe_ollama = function(...) FALSE)
  expect_equal(.capture("percysuits")$status, 1L)
  testthat::local_mocked_bindings(.package = .pkg,
    .cli_latest_version = function(pkg) list(version = "99.0.0", source = "test"))
  u <- .capture("update")
  expect_equal(u$status, 0L)
  expect_match(u$text, "Latest:    99.0.0")
  expect_match(u$text, "Update with")
})

test_that("verify-pollution and emissions verbs", {
  r <- .capture("verify-pollution", "--pollutant", "no2", "--demo")
  expect_equal(r$status, 0L)
  expect_match(r$text, "STATUS: ok")
  expect_match(r$text, "source:   Huangfu & Atkinson")
  f <- .capture("verify-pollution", "--pollutant", "pm25", "--exposure-mean", "3", "--exposure-prevalence", "0.5")
  expect_equal(f$status, 1L)
  expect_match(f$text, "assumption_failure")
  j <- .capture("verify-pollution", "--pollutant", "no2", "--exposure-mean", "25", "--exposure-prevalence", "0.9", "--json")
  expect_equal(j$status, 0L)
  expect_match(j$text, "\"paf\"")
  expect_equal(.capture("verify-pollution")$status, 2L)
  withr::local_envvar(MORIE_EMISSIONS_OFFLINE = "1")
  d <- withr::local_tempdir()
  e <- .capture("emissions", "--seconds", "0.3", "--output-dir", d, "--country", "CAN")
  expect_equal(e$status, 0L)
  expect_match(e$text, "kg CO2eq")
  expect_match(e$text, "Capsule:")
  expect_true(file.exists(file.path(d, "emissions.csv")))
})

test_that("selftest runs every subsystem", {
  withr::local_envvar(MORIE_EMISSIONS_OFFLINE = "1")
  r <- .capture("selftest")
  expect_match(r$text, "module registry")
  expect_match(r$text, "emissions tracker")
  expect_match(r$text, "All tests passed")
  expect_equal(r$status, 0L, info = r$text)
})

test_that("data.rmorie.com tables: key handling, manifest cache, download cached in the store, pull", {
  withr::local_envvar(XDG_CONFIG_HOME = withr::local_tempdir(), MORIE_HOSTED_KEY = NA,
                      R_USER_CACHE_DIR = withr::local_tempdir(), MORIE_DATA_URL = "https://data.example.test")
  expect_error(morie_hosted_manifest(), "rmorie login")
  withr::local_envvar(MORIE_HOSTED_KEY = "sk-good")
  calls <- character()
  manifest <- '{"generated_utc":"2026-10-01T00:00:00Z","datasets":[{"db":"chicago_crime","table":"incidents","key":"chicago_crime/incidents","rows":3,"columns":["id","type"],"bytes_gz":10,"sha256":"x","source":"bigquery-public-data.chicago_crime.crime","meta":{"description":"Chicago Police incidents"}}]}'
  testthat::local_mocked_bindings(.package = .pkg,
    .morie_data_get = function(path, dest, timeout = 600) {
      calls <<- c(calls, path)
      if (path == "/manifest.json") writeLines(manifest, dest)
      else if (path == "/chicago_crime/incidents.csv.gz") {
        con <- gzfile(dest, "w")
        writeLines(c("id,type", "1,THEFT", "2,BATTERY", "3,THEFT"), con)
        close(con)
      } else stop("404")
      invisible(dest)
    })
  m <- morie_hosted_manifest()
  expect_equal(m$datasets[[1]]$key, "chicago_crime/incidents")
  morie_hosted_manifest()
  expect_equal(sum(calls == "/manifest.json"), 1L)
  d <- morie_hosted_datasets()
  expect_equal(d$key, "chicago_crime/incidents")
  expect_equal(d$name, "Chicago Police incidents")
  db <- file.path(withr::local_tempdir(), "cache.sqlite")
  df <- morie_load_dataset("chicago_crime/incidents", db_path = db)
  expect_equal(names(df), c("id", "type"))
  expect_equal(nrow(df), 3L)
  morie_load_dataset("chicago_crime/incidents", db_path = db)
  expect_equal(sum(calls == "/chicago_crime/incidents.csv.gz"), 1L)
  l <- morie_list_datasets(db_path = db)
  row <- l[l$key == "chicago_crime/incidents", ]
  expect_equal(nrow(row), 1L)
  expect_true(row$cached)
  expect_equal(row$rows, 3L)
  expect_true("ocp21" %in% l$key)
  expect_error(morie_load_dataset("no-such-dataset"), "data.rmorie.com")
  out <- withr::local_tempfile(fileext = ".csv")
  r <- .capture("pull", "chicago_crime/incidents", "--out", out)
  expect_equal(r$status, 0L)
  expect_equal(nrow(utils::read.csv(out)), 3L)
})
