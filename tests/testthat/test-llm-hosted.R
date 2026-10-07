# The hosted MORIE tier: credentials file, probe, provider order, device-flow
# login and logout -- the HTTP layer is mocked; nothing leaves the machine.

.pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"

.hosted_sandbox <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(c(XDG_CONFIG_HOME = dir, MORIE_HOSTED_KEY = NA, MORIE_HOSTED_BASE_URL = NA,
                        GEMINI_API_KEY = NA, LLM_API_BASE_URL = NA, LLM_API_KEY = NA,
                        OPENAI_API_KEY = NA), .local_envir = env)
  .morie_llm_cache$hosted_cached <- NULL
  .morie_llm_cache$hosted_models <- NULL
  .morie_llm_cache$ollama_cached <- FALSE
  withr::defer({
    .morie_llm_cache$hosted_cached <- NULL
    .morie_llm_cache$hosted_models <- NULL
    .morie_llm_cache$ollama_cached <- NULL
  }, envir = env)
  dir
}

test_that("credentials are written owner-only into the shared XDG file", {
  dir <- .hosted_sandbox()
  p <- .morie_llm_write_credentials(list(hosted_key = "sk-test"))
  expect_equal(p, file.path(dir, "morie", "credentials.json"))
  if (.Platform$OS.type != "windows") expect_equal(as.character(file.info(p)$mode), "600")
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
  withr::local_envvar(MORIE_HOSTED_BASE_URL = "off")
  expect_null(.morie_llm_hosted_base())
  expect_false(morie_llm_probe_hosted())
  if (.Platform$OS.type != "windows") {  # Windows drops a variable set to ""
    withr::local_envvar(MORIE_HOSTED_BASE_URL = "")
    expect_null(.morie_llm_hosted_base())
  }
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

test_that("the hosted tier is the last resort: after a local Ollama and after every cloud key", {
  testthat::skip_on_covr()
  .hosted_sandbox()
  expect_equal(morie_llm_detect_provider(), "local")
  .morie_llm_write_credentials(list(hosted_key = "sk-abc"))
  .morie_llm_cache$hosted_cached <- TRUE
  expect_equal(morie_llm_detect_provider(), "hosted")
  withr::local_envvar(GEMINI_API_KEY = "g")
  expect_equal(morie_llm_detect_provider(), "gemini")
  withr::local_envvar(GEMINI_API_KEY = NA, OPENAI_API_KEY = "o")
  expect_equal(morie_llm_detect_provider(), "openai")
  withr::local_envvar(OPENAI_API_KEY = NA, LLM_API_BASE_URL = "https://api.example.org", LLM_API_KEY = "k")
  expect_equal(morie_llm_detect_provider(), "api")
  .morie_llm_cache$ollama_cached <- TRUE
  expect_equal(morie_llm_detect_provider(), "ollama")
  # the hosted address comes from the services document when bricklayer has it
  expect_true(is.null(.morie_llm_services()) || is.list(.morie_llm_services()))
  expect_match(.morie_llm_access_hint(), "rmorie.com/access")
  expect_match(.morie_llm_hosted_base(), "^https://")
  expect_match(.morie_llm_hosted_auth(), "^https://")
  expect_true(nzchar(.morie_llm_hosted_model()))
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
  dir <- .hosted_sandbox()
  n <- 0L
  # the package's own HTTP layer (libcurl), mocked: nothing leaves the machine
  fake_http <- function(url, body = NULL, headers = character(), timeout = 30) {
    if (grepl("/device/code$", url)) {
      return(list(status = 200L, body = '{"device_code":"dc","user_code":"ABCD-1234","verification_uri":"https://github.com/login/device","interval":0}'))
    }
    n <<- n + 1L
    if (n < 3L) return(list(status = 428L, body = '{"error":"authorization_pending","interval":0}'))
    list(status = 200L, body = '{"api_key":"sk-new","user":"octocat"}')
  }
  testthat::local_mocked_bindings(.morie_llm_http = fake_http, .package = .pkg)
  key <- suppressMessages(morie_llm_login(open_browser = FALSE))
  expect_equal(key, "sk-new")
  cred <- .morie_llm_read_credentials()
  expect_equal(cred$hosted_key, "sk-new")
  expect_equal(cred$hosted_user, "octocat")
  expect_equal(n, 3L)
  expect_true(file.exists(file.path(dir, "morie", "credentials.json")))
})

test_that("the email sign-in requests a code and exchanges it for a key", {
  testthat::skip_on_covr()
  dir <- .hosted_sandbox()
  seen <- list()
  fake_http <- function(url, body = NULL, headers = character(), timeout = 30) {
    seen[[length(seen) + 1L]] <<- list(url = url, body = .morie_from_json(body, simplifyVector = FALSE))
    if (grepl("/email/code$", url)) return(list(status = 200L, body = '{"sent":true,"expires_in":600}'))
    list(status = 200L, body = '{"api_key":"sk-mail","user":"mail:abc"}')
  }
  testthat::local_mocked_bindings(.morie_llm_http = fake_http, .package = .pkg)
  key <- suppressMessages(morie_llm_login(email = " vee@example.com", code = "123456"))
  expect_equal(key, "sk-mail")
  expect_length(seen, 1L)  # a supplied code skips the request step
  expect_equal(seen[[1]]$body$email, "vee@example.com")
  expect_equal(seen[[1]]$body$code, "123456")
  expect_equal(.morie_llm_read_credentials()$hosted_key, "sk-mail")
  expect_error(morie_llm_login(email = "nope"), "email address")
})

test_that("a pasted token is stored and probed, and the mailed-key path stores nothing", {
  testthat::skip_on_covr()
  .hosted_sandbox()
  testthat::local_mocked_bindings(.package = .pkg, morie_llm_probe_hosted = function(...) TRUE)
  expect_message(tok <- morie_llm_login(token = "  sk-pasted "), "accepts it")
  expect_equal(tok, "sk-pasted")
  expect_equal(.morie_llm_hosted_key(), "sk-pasted")
  expect_error(morie_llm_login(token = " "), "empty token")
  suppressMessages(morie_llm_logout())
  seen <- list()
  testthat::local_mocked_bindings(
    .package = .pkg,
    .morie_llm_http = function(url, body = NULL, headers = character(), timeout = 30) {
      seen[[length(seen) + 1L]] <<- .morie_from_json(body, simplifyVector = FALSE)
      list(status = 200L, body = '{"sent":true,"user":"mail:abc"}')
    })
  expect_message(out <- morie_llm_login(email = "vee@example.com", code = "123456", to_email = TRUE),
                 "rmorie login --token")
  expect_equal(out, "")
  expect_equal(seen[[1]]$deliver, "email")
  expect_null(.morie_llm_hosted_key())
})

test_that("the hosted model falls back to what the gateway lists for this key", {
  testthat::skip_on_covr()
  .hosted_sandbox()
  withr::local_options(morie.llm.allow_net_probe = TRUE)  # R CMD check sets _R_CHECK_PACKAGE_NAME_, which parks the probe
  .morie_llm_write_credentials(list(hosted_key = "sk-abc"))
  listed <- '{"data":[{"id":"gemma4:31b-cloud"},{"id":"minimax-m3:cloud"}]}'
  testthat::local_mocked_bindings(
    .package = .pkg,
    .morie_llm_http = function(url, body = NULL, headers = character(), timeout = 30) list(status = 200L, body = listed))
  withr::local_envvar(MORIE_HOSTED_MODEL = "qwen3.5:397b-cloud")  # retired upstream
  expect_equal(.morie_llm_hosted_model(), "qwen3.5:397b-cloud")
  expect_equal(.morie_llm_hosted_model_available(), "gemma4:31b-cloud")
  withr::local_envvar(MORIE_HOSTED_MODEL = "minimax-m3:cloud")
  expect_equal(.morie_llm_hosted_model_available(), "minimax-m3:cloud")
  .morie_llm_cache$hosted_cached <- NULL
  .morie_llm_cache$hosted_models <- NULL
  testthat::local_mocked_bindings(.package = .pkg,  # no answer: status 0, as for a refused connection
    .morie_llm_http = function(url, body = NULL, headers = character(), timeout = 30) list(status = 0L, body = ""))
  withr::local_envvar(MORIE_HOSTED_MODEL = "anything:cloud")
  expect_equal(.morie_llm_hosted_model_available(), "anything:cloud")  # no list known: keep the name
})

test_that("only an https address of a public host is handed to the browser, and server text never carries the key", {
  ok <- function(u) .morie_llm_browsable(u)
  expect_true(ok("https://github.com/login/device"))
  expect_true(ok("https://llm.rmorie.com:8443/auth/x"))
  for (u in c("http://github.com/login/device", "https://127.0.0.1/", "https://localhost/", "https://10.0.0.5/x",
              "https://192.168.1.9/", "https://172.20.0.1/", "https://169.254.169.254/latest", "https://100.64.0.1/",
              "https://metadata.google.internal/", "https://box.lan/", "https://metadata/", "file:///etc/passwd",
              "javascript:alert(1)", NA_character_, "")) {
    expect_false(ok(u), info = u)
  }
  expect_false(ok(c("https://a.example.org", "https://b.example.org")))  # one address, not a vector
  r <- .morie_llm_redact
  expect_identical(r("token sk-LEAKLEAK1234 rejected", "sk-LEAKLEAK1234"), "token <key> rejected")
  expect_identical(r("header was Authorization: Bearer abcdefgh12345678 at the gateway"), "header was Authorization: Bearer <key> at the gateway")
  expect_identical(r("Invalid key. Received API key = sk-abc, key hash = xyz"), "Invalid key")
  expect_identical(r("quota exceeded"), "quota exceeded")
  expect_identical(r("the key 'my secret 42' is unknown", "my secret 42"), "the key '<key>' is unknown")
})
