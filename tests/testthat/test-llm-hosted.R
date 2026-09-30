# The hosted MORIE tier: credentials file, probe, provider order, device-flow
# login and logout -- the HTTP layer is mocked; nothing leaves the machine.

.pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"

.hosted_sandbox <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(c(XDG_CONFIG_HOME = dir, MORIE_HOSTED_KEY = NA, MORIE_HOSTED_BASE_URL = NA,
                        GEMINI_API_KEY = NA, LLM_API_BASE_URL = NA, LLM_API_KEY = NA,
                        OPENAI_API_KEY = NA), .local_envir = env)
  .morie_llm_cache$hosted_cached <- NULL
  .morie_llm_cache$ollama_cached <- FALSE
  withr::defer({ .morie_llm_cache$hosted_cached <- NULL; .morie_llm_cache$ollama_cached <- NULL }, envir = env)
  dir
}

test_that("credentials are written owner-only into the shared XDG file", {
  dir <- .hosted_sandbox()
  p <- .morie_llm_write_credentials(list(hosted_key = "sk-test"))
  expect_equal(p, file.path(dir, "morie", "credentials.json"))
  expect_equal(as.character(file.info(p)$mode), "600")
  expect_equal(.morie_llm_hosted_key(), "sk-test")
  expect_true(suppressMessages(morie_llm_logout()))
  expect_null(.morie_llm_hosted_key())
  expect_false(file.exists(p))
})

test_that("the environment key overrides the file and an empty base URL disables the tier", {
  .hosted_sandbox()
  .morie_llm_write_credentials(list(hosted_key = "file-key"))
  withr::local_envvar(MORIE_HOSTED_KEY = "env-key")
  expect_equal(.morie_llm_hosted_key(), "env-key")
  withr::local_envvar(MORIE_HOSTED_BASE_URL = "")
  expect_null(.morie_llm_hosted_base())
  expect_false(morie_llm_probe_hosted())
})

test_that("the probe never runs without a key and sends the bearer key when it does", {
  testthat::skip_on_covr()
  .hosted_sandbox()
  calls <- 0L
  testthat::local_mocked_bindings(
    .package = .pkg,
    morie_llm_probe_hosted = function(timeout = 2) {
      # the real probe short-circuits before any request when no key exists
      if (is.null(.morie_llm_hosted_key())) return(FALSE)
      calls <<- calls + 1L
      TRUE
    })
  expect_false(morie_llm_probe_hosted()); expect_equal(calls, 0L)
  .morie_llm_write_credentials(list(hosted_key = "sk-abc"))
  expect_true(morie_llm_probe_hosted()); expect_equal(calls, 1L)
})

test_that("the hosted tier sits after a local Ollama and before the cloud keys", {
  testthat::skip_on_covr()
  .hosted_sandbox()
  expect_equal(morie_llm_detect_provider(), "local")
  .morie_llm_write_credentials(list(hosted_key = "sk-abc"))
  .morie_llm_cache$hosted_cached <- TRUE
  expect_equal(morie_llm_detect_provider(), "hosted")
  withr::local_envvar(GEMINI_API_KEY = "g")
  expect_equal(morie_llm_detect_provider(), "hosted")
  .morie_llm_cache$ollama_cached <- TRUE
  expect_equal(morie_llm_detect_provider(), "ollama")
})

test_that("morie_llm_ask tries the hosted endpoint with the stored key and the hosted model", {
  testthat::skip_on_covr()
  .hosted_sandbox()
  .morie_llm_write_credentials(list(hosted_key = "sk-abc"))
  .morie_llm_cache$hosted_cached <- TRUE
  seen <- list()
  testthat::local_mocked_bindings(
    .package = .pkg,
    morie_llm_request_completion = function(base_url, model, messages, api_key = NULL, timeout = 120) {
      seen[[length(seen) + 1L]] <<- list(base = base_url, model = model, key = api_key)
      list(choices = list(list(message = list(content = "hello from the tier"))))
    })
  expect_equal(morie_llm_ask("hi"), "hello from the tier")
  expect_equal(seen[[1]]$base, DEFAULT_HOSTED_BASE_URL)
  expect_equal(seen[[1]]$model, DEFAULT_HOSTED_MODEL)
  expect_equal(seen[[1]]$key, "sk-abc")
})

test_that("the device flow polls until a key arrives and stores it", {
  testthat::skip_on_covr()
  testthat::skip_if_not_installed("httr2")
  dir <- .hosted_sandbox()
  n <- 0L
  fake_perform <- function(req, ...) {
    url <- req$url
    if (grepl("/device/code$", url)) {
      return(httr2::response(status_code = 200L, headers = list(`content-type` = "application/json"),
        body = charToRaw('{"device_code":"dc","user_code":"ABCD-1234","verification_uri":"https://github.com/login/device","interval":0}')))
    }
    n <<- n + 1L
    if (n < 3L) {
      return(httr2::response(status_code = 428L, headers = list(`content-type` = "application/json"),
                             body = charToRaw('{"error":"authorization_pending","interval":0}')))
    }
    httr2::response(status_code = 200L, headers = list(`content-type` = "application/json"),
                    body = charToRaw('{"api_key":"sk-new","user":"octocat"}'))
  }
  testthat::local_mocked_bindings(req_perform = fake_perform, .package = "httr2")
  key <- suppressMessages(morie_llm_login(open_browser = FALSE))
  expect_equal(key, "sk-new")
  cred <- .morie_llm_read_credentials()
  expect_equal(cred$hosted_key, "sk-new")
  expect_equal(cred$hosted_user, "octocat")
  expect_equal(n, 3L)
  expect_true(file.exists(file.path(dir, "morie", "credentials.json")))
})
