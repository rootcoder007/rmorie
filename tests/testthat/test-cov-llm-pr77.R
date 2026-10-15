# R/llm.R, the lines PR #77 added that no other test reached: the Ollama endpoint when switched off or
# given without a scheme, the model the probe learned, the OpenAI-style API root, and a character
# context handed to the model. No network: the libcurl primitive is a recorder.

.ll <- function(name) get(name, envir = environment(morie_llm_ask))

# No language-model setting from the session, the settings file in a temporary directory, and no probe
# result carried in from another test
.ll_sandbox <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(OLLAMA_HOST = NA, OLLAMA_BASE_URL = NA, OLLAMA_MODEL = NA, OLLAMA_API_KEY = NA,
                      XDG_CONFIG_HOME = dir, "_R_CHECK_PACKAGE_NAME_" = NA, .local_envir = env)
  withr::local_options(morie.llm.allow_net_probe = NULL, .local_envir = env)
  cache <- .ll(".morie_llm_cache")
  cache$ollama_cached <- NULL
  withr::defer(cache$ollama_cached <- NULL, envir = env)
  cache
}

# The libcurl GET, answering with `body` and counting its calls
.ll_http <- function(body, status = 200L, env = parent.frame()) {
  calls <- new.env()
  calls$urls <- character()
  testthat::local_mocked_bindings(
    .morie_http_get_with_status = function(url, timeout_s = 60L, headers = character(), user_agent = "",
                                           follow_redirects = TRUE) {
      calls$urls <- c(calls$urls, url)
      list(status_code = status, body = body)
    },
    .package = "rmorie", .env = env)
  calls
}

test_that("the Ollama endpoint: switched off shows the default, a bare host gets http://", {
  .ll_sandbox()
  base <- .ll(".morie_llm_ollama_base")
  withr::local_envvar(OLLAMA_HOST = "off")
  expect_identical(base(), "http://localhost:11434")
  calls <- .ll_http("{}")
  empty <- morie_llm_ollama_models()
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), c("name", "size_gb", "family", "parameter_size", "quantization"))
  expect_length(calls$urls, 0L)   # switched off: never asked
  withr::local_envvar(OLLAMA_HOST = "box.lan:11434/")
  expect_identical(base(), "http://box.lan:11434")
})

test_that("the default model is the first the probe saw, or the server's first, or none", {
  cache <- .ll_sandbox()
  dm <- .ll(".morie_llm_ollama_default_model")
  cache$ollama_cached <- structure(TRUE, models = c("m-one", "m-two"))
  expect_identical(dm(), "m-one")
  cache$ollama_cached <- structure(TRUE, models = character())
  expect_identical(dm(), NA_character_)
  cache$ollama_cached <- NULL
  withr::local_envvar(OLLAMA_HOST = "http://127.0.0.1:9", "_R_CHECK_PACKAGE_NAME_" = "rmorie")
  calls <- .ll_http("{\"models\":[{\"name\":\"llama-x\"}]}")
  expect_identical(dm(), NA_character_)   # under R CMD check: no probe
  expect_length(calls$urls, 0L)
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = NA)
  expect_identical(dm(), "llama-x")
  expect_identical(calls$urls, "http://127.0.0.1:9/api/tags")
  .ll_http("{\"models\":[{\"name\":\"\"}]}")
  expect_identical(dm(), NA_character_)
})

test_that("the probe keeps the names the server serves, and none when the list is malformed", {
  cache <- .ll_sandbox()
  withr::local_envvar(OLLAMA_HOST = "http://127.0.0.1:9")
  calls <- .ll_http("{\"models\":[{\"name\":\"a\"},{\"model\":\"b\"},{\"name\":\"\"}]}")
  expect_true(morie_llm_probe_ollama())
  expect_identical(attr(cache$ollama_cached, "models"), c("a", "b"))
  expect_length(calls$urls, 1L)
  cache$ollama_cached <- NULL
  .ll_http("{\"models\":[{\"name\":[\"x\",\"y\"]}]}")   # a name that is not one string
  expect_true(morie_llm_probe_ollama())
  expect_identical(attr(cache$ollama_cached, "models"), character())
  expect_identical(.ll(".morie_llm_ollama_default_model")(), NA_character_)
})

test_that("an endpoint that names its API version keeps it; a bare host gets /v1", {
  v1 <- .ll(".morie_llm_v1")
  expect_identical(v1("https://api.example.org/v1/"), "https://api.example.org/v1")
  expect_identical(v1("https://g.example.com/v1beta/openai"), "https://g.example.com/v1beta/openai")
  expect_identical(v1("http://box.lan:11434"), "http://box.lan:11434/v1")
  expect_identical(v1("https://gw.example.com/v1/chat"), "https://gw.example.com/v1/chat/v1")
})

test_that("a character context is appended to the question one element per line", {
  .ll_sandbox()
  seen <- NULL
  testthat::local_mocked_bindings(
    bricklayer_llm_ask = function(prompt, model = NULL, timeout = 120, system_prompt = NULL, route = NULL,
                                  docs = TRUE, packages = NULL) {
      seen <<- prompt
      "ok"
    },
    .package = "rmoriebricklayer")
  expect_identical(morie_llm_ask("Which test?", context = c("n = 40", "two groups"), route = "anthropic"), "ok")
  expect_identical(seen, "Which test?\n\nContext:\nn = 40\ntwo groups")
})
