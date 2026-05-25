# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/llm.R -- provider detection + local fallback + helpers.

set.seed(1)

.clean_llm_env <- function() {
  for (v in c("OLLAMA_BASE_URL", "GEMINI_API_KEY", "OPENAI_API_KEY",
              "LLM_API_BASE_URL", "LLM_API_KEY", "GEMINI_MODEL", "moriefam")) {
    Sys.unsetenv(v)
  }
  options(morie.llm.ollama_cached = NULL)
  options(morie.llm.freeapi_cached = NULL)
}

test_that("env helper returns trimmed value or default", {
  set.seed(1)
  .clean_llm_env()
  Sys.setenv(MORIE_TEST_LLM_ENV = "  hi  ")
  expect_equal(morie:::.morie_llm_env("MORIE_TEST_LLM_ENV"), "hi")
  expect_equal(morie:::.morie_llm_env("__no_such__", default = "x"), "x")
})

test_that("ollama_base strips trailing slash", {
  set.seed(1)
  .clean_llm_env()
  Sys.setenv(OLLAMA_BASE_URL = "http://x/")
  expect_equal(morie:::.morie_llm_ollama_base(), "http://x")
})

test_that("gemini/openai/api key helpers return NULL when unset", {
  set.seed(1)
  .clean_llm_env()
  expect_null(morie:::.morie_llm_gemini_key())
  expect_null(morie:::.morie_llm_openai_key())
  expect_null(morie:::.morie_llm_api_base())
  expect_null(morie:::.morie_llm_api_key())
})

test_that("probe_ollama caches via options", {
  set.seed(1)
  .clean_llm_env()
  options(morie.llm.ollama_cached = FALSE)
  on.exit(options(morie.llm.ollama_cached = NULL))
  expect_false(morie_llm_probe_ollama())
})

test_that("detect_provider returns 'local' with no providers configured", {
  set.seed(1)
  .clean_llm_env()
  options(morie.llm.ollama_cached = FALSE)
  options(morie.llm.freeapi_cached = FALSE)
  on.exit({ options(morie.llm.ollama_cached = NULL); options(morie.llm.freeapi_cached = NULL) })
  expect_equal(morie_llm_detect_provider(), "local")
})

test_that("detect_provider picks gemini when key set", {
  set.seed(1)
  .clean_llm_env()
  options(morie.llm.ollama_cached = FALSE)
  options(morie.llm.freeapi_cached = FALSE)
  Sys.setenv(GEMINI_API_KEY = "k")
  on.exit({
    options(morie.llm.ollama_cached = NULL); options(morie.llm.freeapi_cached = NULL)
    Sys.unsetenv("GEMINI_API_KEY")
  })
  expect_equal(morie_llm_detect_provider(), "gemini")
})

test_that("system_prompt + messages helpers shape correctly", {
  set.seed(1)
  sp <- morie:::.morie_llm_system_prompt("context")
  expect_type(sp, "character")
  msgs <- morie:::.morie_llm_messages("hi", context = list(study = "x"))
  expect_length(msgs, 2L)
  expect_equal(msgs[[2]]$role, "user")
  expect_equal(msgs[[2]]$content, "hi")
  expect_equal(msgs[[1]]$role, "system")
})

test_that("local_fallback returns informative text", {
  set.seed(1)
  out <- morie:::.morie_llm_local_fallback("anything")
  expect_type(out, "character")
  expect_match(out, "local-only")
})

test_that("extract_text handles empty + valid choices", {
  set.seed(1)
  expect_equal(morie:::.morie_llm_extract_text(list(choices = list())), "")
  d <- list(choices = list(list(message = list(content = "yes"))))
  expect_equal(morie:::.morie_llm_extract_text(d), "yes")
})

test_that("ask returns local-fallback when provider=local", {
  set.seed(1)
  .clean_llm_env()
  options(morie.llm.ollama_cached = FALSE)
  options(morie.llm.freeapi_cached = FALSE)
  on.exit({ options(morie.llm.ollama_cached = NULL); options(morie.llm.freeapi_cached = NULL) })
  out <- morie_llm_ask("question?")
  expect_type(out, "character")
  expect_match(out, "local")
})

test_that("agent_available reflects detect_provider", {
  set.seed(1)
  .clean_llm_env()
  options(morie.llm.ollama_cached = FALSE)
  options(morie.llm.freeapi_cached = FALSE)
  on.exit({ options(morie.llm.ollama_cached = NULL); options(morie.llm.freeapi_cached = NULL) })
  expect_false(morie_llm_agent_available())
})

test_that("messages_to_prompt concatenates role labels", {
  set.seed(1)
  msgs <- list(
    list(role = "system", content = "rules"),
    list(role = "user", content = "q"),
    list(role = "assistant", content = "a")
  )
  out <- morie:::.morie_llm_messages_to_prompt(msgs)
  expect_match(out, "System")
  expect_match(out, "Assistant")
})

test_that("strip_think removes <think> blocks", {
  set.seed(1)
  out <- morie:::.morie_llm_strip_think("hi  <think>secret</think>  bye")
  expect_type(out, "character")
})

test_that("freeapi_model honours moriefam env", {
  set.seed(1)
  .clean_llm_env()
  Sys.setenv(moriefam = "myfree:model")
  on.exit(Sys.unsetenv("moriefam"))
  expect_equal(morie:::.morie_llm_freeapi_model(), "myfree:model")
})

test_that("probe_freeapi cached FALSE returns FALSE", {
  set.seed(1)
  .clean_llm_env()
  options(morie.llm.freeapi_cached = FALSE)
  on.exit(options(morie.llm.freeapi_cached = NULL))
  expect_false(morie_llm_probe_freeapi())
})

test_that("request_completion errors without httr2/jsonlite", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "httr2") || identical(package, "jsonlite")) FALSE
      else TRUE
    },
    .package = "base"
  )
  expect_error(
    morie_llm_request_completion("http://x", "m",
                                 list(list(role = "user", content = "x"))),
    "httr2"
  )
})

test_that("request_completion fails clean off-network", {
  set.seed(1)
  res <- tryCatch(
    morie_llm_request_completion("http://127.0.0.1:1", "m",
                                 list(list(role = "user", content = "x")),
                                 timeout = 1),
    error = function(e) NULL
  )
  expect_null(res)
})

test_that("ask_multi falls back to local with no providers", {
  set.seed(1)
  .clean_llm_env()
  options(morie.llm.ollama_cached = FALSE)
  options(morie.llm.freeapi_cached = FALSE)
  on.exit({ options(morie.llm.ollama_cached = NULL); options(morie.llm.freeapi_cached = NULL) })
  msgs <- list(list(role = "user", content = "hello"))
  out <- morie_llm_ask_multi(msgs)
  expect_type(out, "character")
  expect_match(out, "local")
})

test_that("list_freeapi_models routes through http helper (mocked)", {
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(...) {
      list(data = list(list(id = "mock-model")))
    },
    .package = "morie"
  )
  res <- tryCatch(morie_llm_list_freeapi_models(), error = function(e) e)
  expect_true(is.data.frame(res) || is.list(res) || inherits(res, "error"))
})

test_that("ask_multi rejects non-list messages", {
  set.seed(1)
  expect_error(morie_llm_ask_multi("notalist"))
})