# SPDX-License-Identifier: AGPL-3.0-or-later
#
# The saved language-model settings (morie_llm_config / `rmorie config`) and
# the route `ask` takes: an Ollama server with nothing pulled must not stand
# in front of a logged-in hosted tier. Every HTTP call goes through a mocked
# .morie_llm_http; the settings live in a temporary XDG_CONFIG_HOME, so
# nothing touches the network or $HOME.

.pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"

.cfg_sandbox <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  vars <- c("MORIE_LLM_ROUTE", "MORIE_LLM_BASE_URL", "MORIE_LLM_API_KEY", "MORIE_LLM_MODEL",
            "OLLAMA_HOST", "OLLAMA_BASE_URL", "OLLAMA_MODEL", "OLLAMA_API_KEY",
            "MORIE_HOSTED_BASE_URL", "MORIE_HOSTED_MODEL", "MORIE_HOSTED_KEY", "MORIE_HOSTED_AUTH_URL",
            "LLM_API_BASE_URL", "LLM_API_KEY", "MORIE_API_MODEL",
            "GEMINI_API_KEY", "GOOGLE_API_KEY", "OPENAI_API_KEY")
  withr::local_envvar(c(XDG_CONFIG_HOME = dir, stats::setNames(rep(NA_character_, length(vars)), vars)),
                      .local_envir = env)
  # probes are mocked below, so they may run even under R CMD check
  withr::local_options(morie.llm.allow_net_probe = TRUE, .local_envir = env)
  .morie_llm_reset_probes()
  withr::defer(.morie_llm_reset_probes(), envir = env)
  dir
}

# A fake network: an Ollama server serving `ollama` (NULL: nothing answers) and
# a hosted gateway serving `hosted` to the key "sk-good".
.fake_net <- function(ollama = character(), hosted = c("minimax-m3:cloud", "gpt-oss-120b:cf"),
                      env = parent.frame()) {
  seen <- new.env()
  seen$urls <- character()
  testthat::local_mocked_bindings(
    .package = .pkg,
    .morie_llm_services = function() NULL,
    .morie_llm_http = function(url, body = NULL, headers = character(), timeout = 30) {
      seen$urls <- c(seen$urls, url)
      seen$headers <- headers
      if (grepl("/api/tags$", url)) {
        if (is.null(ollama)) return(list(status = 0L, body = "connection refused"))
        return(list(status = 200L, body = .morie_to_json(
          list(models = lapply(ollama, function(m) list(name = m))), auto_unbox = TRUE)))
      }
      if (grepl("/v1/models$", url)) {
        if (!any(grepl("sk-good", headers, fixed = TRUE))) return(list(status = 401L, body = "{}"))
        return(list(status = 200L, body = .morie_to_json(
          list(data = lapply(hosted, function(m) list(id = m))), auto_unbox = TRUE)))
      }
      list(status = 404L, body = "{}")
    },
    .env = env)
  seen
}

.cap <- function(...) {
  txt <- character()
  st <- morie_cli(c(...), out = function(s) txt <<- c(txt, s))
  list(status = st, text = paste(txt, collapse = ""))
}

test_that("reading the settings writes nothing; every key has its environment variable", {
  dir <- .cfg_sandbox()
  tab <- morie_llm_config()
  expect_s3_class(tab, "data.frame")
  expect_equal(tab$key, c("route", "own.url", "own.key", "own.model", "ollama.url", "ollama.model",
                          "ollama.key", "hosted.url", "hosted.model", "hosted.key"))
  expect_equal(tab$env[tab$key == "route"], "MORIE_LLM_ROUTE")
  expect_equal(tab$value[tab$key == "route"], "auto")
  expect_true(all(tab$source == "default"))
  expect_false(file.exists(file.path(dir, "morie", "llm.json")))
  expect_equal(.morie_llm_config_path(), file.path(dir, "morie", "llm.json"))
})

test_that("settings are saved privately, read back, overridden by the environment and removed", {
  dir <- .cfg_sandbox()
  morie_llm_config(route = "Hosted", hosted.model = "gpt-oss-120b:cf",
                   ollama.url = "192.168.1.20:11434", ollama.model = "qwen3:8b",
                   own.url = "http://localhost:1234/v1/", own.model = "m1", own.key = "sk-own-123456")
  p <- file.path(dir, "morie", "llm.json")
  expect_true(file.exists(p))
  if (.Platform$OS.type != "windows") expect_equal(as.character(file.info(p)$mode), "600")
  expect_equal(.morie_llm_route(), "hosted")
  expect_equal(.morie_llm_hosted_model(), "gpt-oss-120b:cf")
  expect_equal(.morie_llm_ollama_base(), "http://192.168.1.20:11434")
  expect_equal(.morie_llm_ollama_default_model(), "qwen3:8b")
  expect_equal(.morie_llm_api_base(), "http://localhost:1234/v1")
  expect_equal(.morie_llm_api_model(), "m1")
  expect_equal(.morie_llm_api_key(), "sk-own-123456")
  tab <- morie_llm_config()
  expect_equal(tab$source[tab$key == "route"], "saved")
  expect_equal(tab$value[tab$key == "own.key"], "set")  # no character of a key is ever shown
  # an environment variable that is set wins, and saving under it says so
  withr::local_envvar(OLLAMA_MODEL = "llama3.2", MORIE_LLM_ROUTE = "ollama")
  expect_equal(.morie_llm_ollama_default_model(), "llama3.2")
  expect_equal(.morie_llm_route(), "ollama")
  expect_warning(morie_llm_config(ollama.model = "x"), "OLLAMA_MODEL is set in the environment")
  withr::local_envvar(OLLAMA_MODEL = NA, MORIE_LLM_ROUTE = NA)
  expect_equal(.morie_llm_ollama_default_model(), "x")
  # NULL removes a setting; the file goes when nothing is left
  morie_llm_config(route = NULL, hosted.model = NULL, ollama.url = NULL, ollama.model = NULL,
                   own.url = NULL, own.model = NULL, own.key = "")
  expect_equal(.morie_llm_route(), "auto")
  expect_false(file.exists(p))
  expect_error(morie_llm_config(rout = "hosted"), "unknown setting 'rout'")
  expect_error(morie_llm_config(route = "cloud"), "route must be one of")
  expect_error(morie_llm_config(own.url = "localhost:1234"), "http\\(s\\)://")
  expect_error(morie_llm_config(hosted.url = "http://example.org"), "https://")
  expect_error(morie_llm_config("hosted"), "key = value")
})

test_that("a running Ollama with no model pulled does not stand in front of the hosted tier", {
  .cfg_sandbox()
  seen <- .fake_net(ollama = character())
  .morie_llm_write_credentials(list(hosted_key = "sk-good"))
  expect_true(morie_llm_probe_ollama())  # it answers ...
  expect_false(.morie_llm_ollama_usable())  # ... but has nothing to answer with
  expect_equal(morie_llm_detect_provider(), "hosted")
  asked <- NULL
  testthat::local_mocked_bindings(.package = .pkg,
    morie_llm_request_completion = function(base_url, model, messages, api_key = NULL, timeout = 120) {
      asked <<- list(base = base_url, model = model, key = api_key)
      list(choices = list(list(message = list(content = "from the hosted tier"))))
    })
  expect_equal(morie_llm_ask("hi"), "from the hosted tier")
  expect_equal(asked$base, "https://llm.rmorie.com")
  expect_equal(asked$key, "sk-good")
  expect_equal(asked$model, "minimax-m3:cloud")
  expect_equal(morie_llm_ask_multi(list(list(role = "user", content = "hi"))), "from the hosted tier")
  # the CLI says the same, before asking
  d <- .cap("doctor")
  expect_match(d$text, "reachable, no model pulled \\(skipped\\)")
  expect_match(d$text, "ask uses: hosted MORIE tier \\(https://llm.rmorie.com\\), model minimax-m3:cloud")
  expect_equal(.cap("ask", "hi")$text, "from the hosted tier\n")
  # naming a model for Ollama makes it usable again
  withr::local_envvar(OLLAMA_MODEL = "qwen3:8b")
  expect_equal(morie_llm_detect_provider(), "ollama")
})

test_that("forcing the Ollama route on an empty server says what to do", {
  .cfg_sandbox()
  .fake_net(ollama = character())
  .morie_llm_write_credentials(list(hosted_key = "sk-good"))
  expect_equal(morie_llm_detect_provider(route = "ollama"), "ollama")
  expect_error(morie_llm_ask("hi", route = "ollama"), "has no model to use.*config set route hosted")
  r <- .cap("ask", "--route", "ollama", "hi")
  expect_equal(r$status, 1L)
  expect_match(r$text, "ollama pull NAME")
})

test_that("an Ollama server with a model is used, and a saved route overrides the order", {
  .cfg_sandbox()
  .fake_net(ollama = c("qwen3:8b", "llama3.2"))
  .morie_llm_write_credentials(list(hosted_key = "sk-good"))
  expect_equal(morie_llm_detect_provider(), "ollama")
  expect_equal(.morie_llm_ollama_default_model(), "qwen3:8b")
  asked <- character()
  testthat::local_mocked_bindings(.package = .pkg,
    morie_llm_request_completion = function(base_url, model, messages, api_key = NULL, timeout = 120) {
      asked <<- c(asked, paste(base_url, model))
      list(choices = list(list(message = list(content = paste("answer from", model)))))
    })
  expect_equal(morie_llm_ask("hi"), "answer from qwen3:8b")
  expect_match(.cap("doctor")$text, "ask uses: local Ollama \\(http://localhost:11434\\), model qwen3:8b")
  # the hosted route, saved or for one call, skips Ollama altogether
  expect_equal(morie_llm_ask("hi", route = "hosted"), "answer from minimax-m3:cloud")
  expect_equal(.cap("ask", "--route", "hosted", "--model", "gpt-oss-120b:cf", "hi")$text,
               "answer from gpt-oss-120b:cf\n")
  expect_equal(.cap("config", "set", "route", "hosted")$status, 0L)
  expect_equal(morie_llm_detect_provider(), "hosted")
  expect_match(.cap("doctor")$text, "ask uses: hosted MORIE tier .*\\(route = hosted\\)")
  expect_equal(.cap("ask", "hi")$text, "answer from minimax-m3:cloud\n")
  withr::local_envvar(MORIE_LLM_ROUTE = "auto")  # the environment wins over the saved route
  expect_equal(morie_llm_detect_provider(), "ollama")
  withr::local_envvar(MORIE_LLM_ROUTE = NA)
  expect_equal(.cap("config", "unset", "route")$text, "route = auto\n")
  # a forced route that is not set up names what is missing
  .morie_llm_write_credentials(list())
  .morie_llm_reset_probes()
  r <- .cap("ask", "--route", "hosted", "hi")
  expect_equal(r$status, 1L)
  expect_match(r$text, "not logged in: `rmorie login`")
  expect_equal(.cap("ask", "--route", "cloud", "hi")$status, 2L)
})

test_that("Ollama can be switched off and keys reach a server that wants one", {
  .cfg_sandbox()
  seen <- .fake_net(ollama = c("qwen3:8b"))
  morie_llm_config(ollama.url = "off")
  expect_true(.morie_llm_ollama_off())
  expect_false(morie_llm_probe_ollama())
  expect_false(any(grepl("/api/tags", seen$urls)))
  expect_equal(morie_llm_config()$value[1:5][5], "off")
  morie_llm_config(ollama.url = "http://gpu.lan:11434", ollama.key = "sk-ollama-key")
  expect_true(morie_llm_probe_ollama())
  expect_true(any(grepl("Bearer sk-ollama-key", seen$headers, fixed = TRUE)))
})

test_that("your own endpoint is read from the settings and addressed once at /v1", {
  .cfg_sandbox()
  testthat::local_mocked_bindings(.package = .pkg, .morie_llm_services = function() NULL)
  morie_llm_config(ollama.url = "off")  # nothing is probed: the request below is the only one
  expect_equal(.morie_llm_v1("http://localhost:1234/v1"), "http://localhost:1234/v1")
  expect_equal(.morie_llm_v1("https://llm.rmorie.com/"), "https://llm.rmorie.com/v1")
  expect_equal(.morie_llm_v1(GEMINI_BASE_URL), GEMINI_BASE_URL)
  morie_llm_config(own.url = "http://localhost:1234/v1", own.model = "local-model")
  expect_true(.morie_llm_api_usable())  # a server on this machine needs no key
  expect_equal(morie_llm_detect_provider(), "api")
  expect_equal(morie_llm_detect_provider(route = "own"), "api")
  posted <- NULL
  testthat::local_mocked_bindings(.package = .pkg,
    .morie_http_post_with_status = function(url, body, content_type = "application/json", timeout_s = 60L,
                                            headers = character(), ...) {
      posted <<- list(url = url, body = body)
      list(status_code = 200L, body = '{"choices":[{"message":{"content":"ok"}}]}')
    })
  expect_equal(morie_llm_ask("hi", route = "own"), "ok")
  expect_equal(posted$url, "http://localhost:1234/v1/chat/completions")
  expect_match(posted$body, "local-model")
  # a remote endpoint still needs a key
  morie_llm_config(own.url = "https://api.example.org/v1")
  expect_false(.morie_llm_api_usable())
  withr::local_envvar(MORIE_LLM_API_KEY = "sk-remote")
  expect_true(.morie_llm_api_usable())
})

test_that("rmorie config shows, explains, gets, sets and unsets the settings", {
  dir <- .cfg_sandbox()
  .fake_net(ollama = NULL)
  s <- .cap("config")
  expect_equal(s$status, 0L)
  expect_match(s$text, "route +auto")
  expect_match(s$text, "rmorie config setup")
  h <- .cap("config", "help")
  expect_match(h$text, "ollama.url +your Ollama server")
  expect_match(h$text, "environment variable MORIE_HOSTED_MODEL")
  expect_equal(.cap("help", "config")$text, h$text)
  expect_equal(.cap("config", "path")$text, paste0(file.path(dir, "morie", "llm.json"), "\n"))
  expect_equal(.cap("config", "set", "ollama.model", "qwen3:8b")$text, "ollama.model = qwen3:8b\n")
  expect_equal(.cap("config", "get", "ollama.model")$text, "qwen3:8b\n")
  expect_match(.cap("config")$text, "ollama.model +qwen3:8b +\\(saved\\)")
  expect_equal(.cap("config", "unset", "ollama.model")$text, "ollama.model = (the first one pulled)\n")
  expect_equal(.cap("config", "set", "nope", "x")$status, 2L)
  expect_equal(.cap("config", "set", "route")$status, 2L)
  expect_equal(.cap("config", "bogus")$status, 2L)
  expect_match(.cap("config", "set", "route", "cloud")$text, "route must be one of")
  # a secret typed at the prompt stays out of the shell history
  testthat::local_mocked_bindings(.package = .pkg, .cli_readline = function(prompt) "sk-typed-key-123")
  expect_equal(.cap("config", "set", "own.key")$text, "own.key = set\n")
  expect_match(.cap("config", "--help")$text, "usage: rmorie config")
})

test_that("a hosted key given to config is checked with the gateway before it is stored", {
  .cfg_sandbox()
  .fake_net(ollama = NULL)
  expect_error(morie_llm_config(hosted.key = "sk-bad"), "did not accept")
  expect_null(.morie_llm_hosted_key())
  suppressMessages(morie_llm_config(hosted.key = "sk-good"))
  expect_equal(.morie_llm_hosted_key(), "sk-good")
  expect_false(file.exists(.morie_llm_config_path()))  # the key goes to credentials.json only
  suppressMessages(morie_llm_config(hosted.key = NULL))
  expect_null(.morie_llm_hosted_key())
})

test_that("config setup walks through the routes", {
  .cfg_sandbox()
  .fake_net(ollama = character())
  answers <- c("hosted", "sk-good", "gpt-oss-120b:cf")
  testthat::local_mocked_bindings(.package = .pkg,
    .cli_stdin_closed = function() FALSE,
    .cli_readline = function(prompt) { a <- answers[[1L]]; answers <<- answers[-1L]; a })
  r <- suppressMessages(.cap("config", "setup"))
  expect_equal(r$status, 0L)
  expect_equal(.morie_llm_route(), "hosted")
  expect_equal(.morie_llm_hosted_key(), "sk-good")
  expect_equal(.morie_llm_hosted_model(), "gpt-oss-120b:cf")
  expect_match(r$text, "ask uses: hosted MORIE tier .*model gpt-oss-120b:cf")
  testthat::local_mocked_bindings(.package = .pkg, .cli_stdin_closed = function() TRUE)
  expect_equal(.cap("config", "setup")$status, 2L)
})

test_that("help has a guide per topic and every verb, config included", {
  .cfg_sandbox()
  v <- .cap("help")
  expect_match(v$text, "config \\[show \\| help")
  expect_match(v$text, "rmorie help llm")
  expect_match(.cap("help", "start")$text, "Getting started")
  expect_match(.cap("help", "llm")$text, "rmorie config set route hosted")
  expect_match(.cap("help", "r")$text, "morie_llm_config\\(route = \"hosted\"")
  expect_match(.cap("help", "ask")$text, "usage: rmorie ask \\[--model NAME\\] \\[--route")
  expect_match(.cap("ask", "--help")$text, "--route")
  expect_equal(.cap("help", "no-such-topic")$status, 2L)
})
