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
  expect_equal(.capture("login", "--email")$status, 1L)
  r2 <- .capture("login", "--email", "vee@example.com", "--code", "1", "--to-email")
  expect_equal(r2$status, 0L)
  expect_true(seen$to_email)
  expect_false(grepl("Logged in", r2$text, fixed = TRUE))
  expect_equal(.capture("login", "--token", "sk-pasted")$status, 0L)
  expect_equal(seen$token, "sk-pasted")
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
  testthat::local_mocked_bindings(.package = .pkg, morie_llm_ask = function(prompt, ...) paste("echo:", prompt))
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
    morie_llm_ask = function(prompt, model = NULL, ...) { seen <<- model; paste("echo:", prompt) })
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
    morie_run_morie_module = function(module_name, cpads_csv, output_dir = NULL) list(a = 1, b = 2))
  r <- .capture("run-module", "power-design", "--output-dir", tempdir())
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
  expect_equal(.capture("provider", "set", "--key", "x")$status, 1L)
})
