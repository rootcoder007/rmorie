# R/siu.R, the LLM side: every provider's request builder and reply reader, the single call
# with its credential rules and HTTP outcomes, the fail-over chain, the drid resolver and the
# extraction driver. No network: the HTTP primitives are replaced by recorders.

.sl_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.sl <- function(name) get(name, envir = environment(morie_siu_llm_extract))

test_that("every provider builds its request from the prompt and the credential and reads the reply", {
  P <- .sl(".siu_llm_providers")()
  expect_setequal(names(P), c("gemini", "claude", "vertex", "ollama", "openai", "openai_compatible"))
  g <- P$gemini$build(list(GOOGLE_API_KEY = "k1"), "hello")
  expect_match(g$url, "models/gemini-2.5-flash:generateContent\\?key=k1$")
  expect_equal(g$body$contents[[1]]$parts[[1]]$text, "hello")
  expect_equal(g$body$generationConfig$response_mime_type, "application/json")
  expect_equal(P$gemini$extract(list(candidates = list(list(content = list(parts = list(list(text = "T"))))))), "T")
  expect_error(P$gemini$extract(list()), "Gemini returned empty text")
  expect_error(P$gemini$extract(list(candidates = list())), "Gemini returned empty text")
  cl <- P$claude$build(list(ANTHROPIC_API_KEY = "k2"), "hello")
  expect_equal(cl$url, "https://api.anthropic.com/v1/messages")
  expect_equal(cl$headers[["x-api-key"]], "k2")
  expect_equal(cl$body$model, "claude-sonnet-4-6")
  expect_equal(cl$body$messages[[1]]$content, "hello")
  expect_equal(P$claude$extract(list(content = list(list(text = "C")))), "C")
  expect_error(P$claude$extract(list(content = list())), "Claude returned empty text")
  # Vertex insists on the caller's own project and defaults the location and model
  withr::local_envvar(GCP_PROJECT = NA, GCP_LOCATION = NA, VERTEX_MODEL = NA)
  expect_error(P$vertex$build(list(VERTEX_ACCESS_TOKEN = "t"), "p"), "requires your own Google Cloud project")
  withr::local_envvar(GCP_PROJECT = "proj")
  v <- P$vertex$build(list(VERTEX_ACCESS_TOKEN = "tok"), "p")
  expect_equal(v$url, paste0("https://us-central1-aiplatform.googleapis.com/v1/projects/proj/locations/us-central1/",
                             "publishers/google/models/gemini-2.5-flash:generateContent"))
  expect_equal(v$headers[["authorization"]], "Bearer tok")
  expect_equal(v$body$contents[[1]]$parts[[1]]$text, "p")
  withr::local_envvar(GCP_LOCATION = "europe-west1", VERTEX_MODEL = "gemini-9")
  expect_match(P$vertex$build(list(VERTEX_ACCESS_TOKEN = "tok"), "p")$url,
               "^https://europe-west1-aiplatform.*locations/europe-west1/publishers/google/models/gemini-9:")
  expect_equal(P$vertex$extract(list(candidates = list(list(content = list(parts = list(list(text = "V"))))))), "V")
  expect_error(P$vertex$extract(list()), "Vertex returned empty text")
  # Ollama: the host, an optional bearer key, the served model, the keep-alive
  testthat::local_mocked_bindings(.morie_llm_ollama_key = function() "", .morie_llm_ollama_default_model = function(base) "m1",
                                  .package = .sl_pkg)
  withr::local_envvar(OLLAMA_KEEP_ALIVE = NA)
  o <- P$ollama$build(list(OLLAMA_HOST_OR_DEFAULT = "http://h:1/"), "p")
  expect_equal(o$url, "http://h:1/api/generate")
  expect_null(o$headers[["authorization"]])
  expect_equal(o$body$model, "m1")
  expect_equal(o$body$keep_alive, "30m")
  expect_equal(o$body$format, "json")
  expect_false(o$body$stream)
  testthat::local_mocked_bindings(.morie_llm_ollama_key = function() "sek", .package = .sl_pkg)
  withr::local_envvar(OLLAMA_KEEP_ALIVE = "5m")
  o2 <- P$ollama$build(list(OLLAMA_HOST_OR_DEFAULT = "http://h:1"), "p")
  expect_equal(o2$headers[["authorization"]], "Bearer sek")
  expect_equal(o2$body$keep_alive, "5m")
  testthat::local_mocked_bindings(.morie_llm_ollama_default_model = function(base) NA_character_, .package = .sl_pkg)
  expect_error(P$ollama$build(list(OLLAMA_HOST_OR_DEFAULT = "http://h:1"), "p"), "No usable Ollama model")
  expect_equal(P$ollama$extract(list(response = "R")), "R")
  expect_error(P$ollama$extract(list()), "Ollama returned empty response")
  # OpenAI
  withr::local_envvar(OPENAI_MODEL = NA)
  oa <- P$openai$build(list(OPENAI_API_KEY = "k3"), "p")
  expect_equal(oa$url, "https://api.openai.com/v1/chat/completions")
  expect_equal(oa$headers[["authorization"]], "Bearer k3")
  expect_equal(oa$body$model, "gpt-4o-mini")
  expect_equal(oa$body$response_format$type, "json_object")
  withr::local_envvar(OPENAI_MODEL = "g-9")
  expect_equal(P$openai$build(list(OPENAI_API_KEY = "k3"), "p")$body$model, "g-9")
  expect_equal(P$openai$extract(list(choices = list(list(message = list(content = "O"))))), "O")
  expect_error(P$openai$extract(list(choices = list())), "OpenAI returned empty text")
  # any OpenAI-compatible endpoint: a key only when configured
  testthat::local_mocked_bindings(
    .morie_llm_setting = function(key, ...) switch(key, own.key = "kk", own.model = "mm", NULL), .package = .sl_pkg)
  oc <- P$openai_compatible$build(list(MORIE_LLM_BASE_URL = "http://x/v1/"), "p")
  expect_equal(oc$url, "http://x/v1/chat/completions")
  expect_equal(oc$headers[["authorization"]], "Bearer kk")
  expect_equal(oc$body$model, "mm")
  testthat::local_mocked_bindings(.morie_llm_setting = function(key, ...) NULL, .package = .sl_pkg)
  oc2 <- P$openai_compatible$build(list(MORIE_LLM_BASE_URL = "http://x/v1"), "p")
  expect_null(oc2$headers[["authorization"]])
  expect_equal(oc2$body$model, "default")
  expect_equal(P$openai_compatible$extract(list(choices = list(list(message = list(content = "X"))))), "X")
  expect_error(P$openai_compatible$extract(list(choices = list())), "OpenAI-compatible endpoint returned empty text")
})

test_that("one LLM call resolves the provider's credential and turns each HTTP outcome into words", {
  call_one <- .sl(".siu_llm_call_one")
  rec <- new.env()
  reply <- function(code, body) {
    testthat::local_mocked_bindings(
      .morie_http_post_with_status = function(url, body_json, content_type = "application/json", timeout_s = 60L,
                                              headers = character(), ...) {
        rec$url <- url
        rec$body <- body_json
        rec$headers <- headers
        rec$timeout <- timeout_s
        list(status_code = code, body = body)
      },
      .package = .sl_pkg, .env = parent.frame())
  }
  expect_error(call_one("nope", "p"), "Unknown LLM model: 'nope'. Available: gemini, claude, vertex, ollama, openai, openai_compatible")
  withr::local_envvar(GOOGLE_API_KEY = NA, GEMINI_API_KEY = NA, ANTHROPIC_API_KEY = NA)
  expect_error(call_one("gemini", "p"), "Env var 'GEMINI_API_KEY' \\(or 'GOOGLE_API_KEY'\\) is not set; cannot call gemini")
  expect_error(call_one("claude", "p"), "Env var 'ANTHROPIC_API_KEY' is not set; cannot call claude")
  withr::local_envvar(GEMINI_API_KEY = "g2")
  reply(200L, '{"candidates":[{"content":{"parts":[{"text":"hi"}]}}]}')
  expect_equal(call_one("gemini", "ask me", timeout_s = 7L), "hi")
  expect_match(rec$url, "key=g2$")
  expect_equal(rec$headers, "content-type: application/json")
  expect_equal(rec$timeout, 7L)
  expect_match(rec$body, "\"text\":\"ask me\"")
  reply(0L, "")
  expect_error(call_one("gemini", "p"), "gemini could not be reached")
  reply(401L, '{"error":{"message":"bad key"}}')
  expect_error(call_one("gemini", "p"), "gemini rejected GEMINI_API_KEY \\(or GOOGLE_API_KEY\\) \\(HTTP 401: bad key\\)")
  withr::local_envvar(ANTHROPIC_API_KEY = "a1")
  reply(500L, "boom")
  expect_error(call_one("claude", "p"), "claude answered HTTP 500: boom")
  reply(403L, '{"error":"nope"}')
  expect_error(call_one("claude", "p"), "claude rejected ANTHROPIC_API_KEY \\(HTTP 403: nope\\)")
  reply(200L, '{"content":[{"text":"C"}]}')
  expect_equal(call_one("claude", "p"), "C")
  expect_equal(rec$headers[1], "x-api-key: a1")
  # the three providers with their own credential rules
  testthat::local_mocked_bindings(.morie_llm_setting = function(key, ...) NULL, .package = .sl_pkg)
  expect_error(call_one("openai_compatible", "p"), "no OpenAI-compatible endpoint is set")
  testthat::local_mocked_bindings(.morie_llm_setting = function(key, ...) if (key == "own.url") "http://x" else NULL,
                                  .package = .sl_pkg)
  reply(200L, '{"choices":[{"message":{"content":"X"}}]}')
  expect_equal(call_one("openai_compatible", "p"), "X")
  expect_equal(rec$url, "http://x/chat/completions")
  # "route" is the model you set up, through rmoriebricklayer
  testthat::local_mocked_bindings(bricklayer_llm_chat = function(messages, model = NULL, route = NULL, timeout = 120) {
    paste("route", route %||% "auto", messages[[1L]]$content)
  }, .package = "rmoriebricklayer")
  expect_equal(call_one("route", "p"), "route auto p")
  expect_error(call_one("hosted", "p"), "Unknown LLM model: 'hosted'")
  testthat::local_mocked_bindings(
    .morie_llm_ollama_base = function() "http://o:1", .morie_llm_ollama_key = function() "",
    .morie_llm_ollama_default_model = function(base) "m", .package = .sl_pkg)
  reply(200L, '{"response":"r"}')
  expect_equal(call_one("ollama", "p"), "r")
  expect_equal(rec$url, "http://o:1/api/generate")
  # the default timeout comes from the environment, floored at one second
  dt <- .sl(".siu_llm_default_timeout")
  withr::local_envvar(MORIE_LLM_TIMEOUT_S = NA)
  expect_equal(dt(), 600L)
  withr::local_envvar(MORIE_LLM_TIMEOUT_S = "abc")
  expect_equal(dt(), 600L)
  withr::local_envvar(MORIE_LLM_TIMEOUT_S = "0")
  expect_equal(dt(), 600L)
  withr::local_envvar(MORIE_LLM_TIMEOUT_S = "5")
  expect_equal(dt(), 5L)
})

test_that("the fail-over chain takes the first provider that answers, and only JSON when asked", {
  chain <- .sl(".siu_llm_call")
  expect_equal(chain("gemini", "p", mock_response_text = "canned"), "canned")
  expect_error(chain(character(), "p"), "must be a non-empty character vector")
  testthat::local_mocked_bindings(
    .siu_llm_call_one = function(model, prompt, timeout_s = 600L) {
      switch(model, a = stop("a down"), b = "not json", c = "{\"x\":1}", d = "```json\n{\"y\":2}\n```")
    },
    .package = .sl_pkg)
  expect_equal(chain(c("a", "c"), "p"), "{\"x\":1}")
  expect_equal(chain("b", "p"), "not json")
  expect_equal(chain(c("a", "b", "c"), "p", expect_json = TRUE), "{\"x\":1}")
  expect_equal(chain("d", "p", expect_json = TRUE), "```json\n{\"y\":2}\n```")
  err <- tryCatch(chain(c("a", "b"), "p", expect_json = TRUE), error = function(e) conditionMessage(e))
  expect_match(err, "^All LLM providers failed:")
  expect_match(err, "\\[a\\] a down")
  expect_match(err, "\\[b\\] provider returned non-JSON output")
  expect_match(err, "no provider in the chain \\(a, b\\) answered")
})

test_that("the drid resolver reads the manifest, then pages the live index and remembers it", {
  resolve <- .sl(".siu_resolve_drid")
  state <- .sl(".morie_siu_state")
  reset <- function() for (k in c("live_index", "live_index_complete", "live_index_count", "live_index_total")) {
    if (exists(k, envir = state, inherits = FALSE)) rm(list = k, envir = state)
  }
  reset()
  withr::defer(reset())
  testthat::local_mocked_bindings(
    .siu_load_manifest = function() data.frame(drid = c(10L, 11L, 5L), case_number = c("A", "B", "K"),
                                               `_language` = c("en", "fr", "unknown"), check.names = FALSE),
    .siu_fetch_http_get = function(url, ...) stop("no network"),
    .package = .sl_pkg)
  expect_equal(resolve("A"), 10L)
  expect_equal(resolve("K"), 5L)
  # a case the manifest only has in French goes live; under R CMD check that is a plain miss
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = "rmorie", RMORIE_NETWORK_TESTS = NA)
  withr::local_options(morie.siu.allow_fetch = NULL)
  expect_equal(resolve("B"), NA_integer_)
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = NA)
  # an index that cannot be fetched is a miss too
  expect_equal(resolve("B"), NA_integer_)
  # the live index: a first page with the total, then more rows per page
  pages <- list(
    index = "<input id=\"total_drs\" value=\"3\"><tr class=\"dr-item\">X|drid=100</tr>",
    "lastCount=1" = "<tr>B|drid=11</tr>",
    "lastCount=2" = "<tr>Y|drid=200</tr>")
  fetched <- character()
  testthat::local_mocked_bindings(
    .siu_fetch_http_get = function(url, ...) {
      fetched <<- c(fetched, url)
      key <- if (grepl("get_more_drs", url)) sub(".*&", "", url) else "index"
      if (is.null(pages[[key]])) "" else pages[[key]]
    },
    .siu_fetch_extract_links = function(chunk, base_url) {
      m <- regmatches(chunk, gregexpr("[A-Z]+\\|drid=[0-9]+", chunk))[[1]]
      if (!length(m)) return(matrix(character(), ncol = 2, dimnames = list(NULL, c("case_number", "url"))))
      cbind(case_number = sub("\\|.*", "", m), url = paste0(base_url, "?", sub(".*\\|", "", m)))
    },
    .package = .sl_pkg)
  testthat::local_mocked_bindings(Sys.sleep = function(time) invisible(NULL), .package = "base")
  expect_equal(resolve("B"), 11L)
  expect_length(fetched, 2)
  expect_match(fetched[1], "directors_reports\\.php$")
  expect_match(fetched[2], "get_more_drs\\.php\\?lang=en&lastCount=1$")
  # what was paged is remembered: no request for a case already seen
  fetched <- character()
  expect_equal(resolve("X"), 100L)
  expect_length(fetched, 0)
  # an exhaustive miss pages to the end once, then every later miss is immediate
  expect_equal(resolve("Z"), NA_integer_)
  expect_equal(fetched, paste0(sub("/en/directors_reports\\.php$", "/ssi/get_more_drs.php", morie_siu_index_url()),
                               "?lang=en&lastCount=2"))
  expect_true(isTRUE(state$live_index_complete))
  fetched <- character()
  expect_equal(resolve("Q"), NA_integer_)
  expect_length(fetched, 0)
  expect_equal(resolve("Y"), 200L)
  # the manifest's older column name, and no manifest at all
  reset()
  testthat::local_mocked_bindings(
    .siu_load_manifest = function() data.frame(drid = 7L, case_number = "M", X_language = "en"), .package = .sl_pkg)
  expect_equal(resolve("M"), 7L)
  testthat::local_mocked_bindings(.siu_load_manifest = function() stop("no manifest"), .package = .sl_pkg)
  expect_equal(resolve("X"), 100L)
})

test_that("the extraction driver turns the model's JSON into the 64-column row with the known bookkeeping", {
  fields <- .sl(".siu_field_list")()
  dir <- withr::local_tempdir()
  utils::write.csv(data.frame(case_number = "24-TST-001", drid = 7L, nrid = 9L, police_service = "",
                              stringsAsFactors = FALSE), file.path(dir, "SIU.csv"), row.names = FALSE)
  dir.create(file.path(dir, "html"))
  .sl(".siu_write_html_cache")(file.path(dir, "html"), "drid_7.html.gz",
                               "<html><body><h1>Report</h1><p>Toronto Police Service   officers</p></body></html>")
  seen <- NULL
  testthat::local_mocked_bindings(
    .siu_llm_call = function(model, prompt, timeout_s = 600L, mock_response_text = NULL, expect_json = FALSE) {
      seen <<- list(model = model, prompt = prompt, mock = mock_response_text, expect_json = expect_json)
      "```json\n{\"police_service\":\"Toronto Police Service\",\"date_of_incident_iso\":\"2024-01-02\",\"age_affected\":null,\"extra\":\"x\"}\n```"
    },
    .package = .sl_pkg)
  out <- morie_siu_llm_extract("24-TST-001", cache_dir = dir, mock_response_text = "unused by the mock")
  expect_equal(names(out), fields)
  expect_equal(nrow(out), 1L)
  expect_equal(out$police_service, "Toronto Police Service")
  expect_equal(out$date_of_incident_iso, "2024-01-02")
  expect_equal(out$age_affected, "")
  expect_equal(out$case_number, "24-TST-001")
  expect_equal(out$drid, "7")
  expect_equal(out$nrid, "9")
  expect_equal(out$source_url_report, "https://www.siu.on.ca/en/directors_report_details.php?drid=7")
  expect_equal(out$parser_version, "llm-ollama+route+gemini")
  expect_match(out$scraped_at_utc, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
  expect_true(seen$expect_json)
  expect_equal(seen$mock, "unused by the mock")
  expect_match(seen$prompt, "Keys:\n case_number, drid, nrid")
  expect_match(seen$prompt, "Report text:\n  Report Toronto Police Service officers $")
  # the report text is capped
  morie_siu_llm_extract("24-TST-001", model = "claude", cache_dir = dir, max_html_chars = 12L, mock_response_text = "m")
  expect_match(seen$prompt, "Report text:\n  Report Toro$")
  expect_equal(seen$model, "claude")
  # no cache anywhere and no resolvable drid: an error that says so
  empty <- withr::local_tempdir()
  testthat::local_mocked_bindings(.siu_resolve_drid = function(case_number) NA_integer_, .package = .sl_pkg)
  expect_error(morie_siu_llm_extract("25-TST-009", cache_dir = empty, mock_response_text = "m"),
               "No HTML available for '25-TST-009'")
  # the self-serve route: the manifest resolves the drid and the page is fetched directly
  testthat::local_mocked_bindings(
    .siu_resolve_drid = function(case_number) 42L,
    .siu_fetch_http_get = function(url, ...) if (grepl("drid=42$", url)) "<p>Self-serve report</p>" else "",
    .package = .sl_pkg)
  out2 <- morie_siu_llm_extract("25-TST-009", cache_dir = empty, mock_response_text = "m")
  expect_equal(out2$case_number, "25-TST-009")
  expect_equal(out2$drid, "42")
  expect_equal(out2$nrid, "")
  expect_equal(out2$source_url_report, "https://www.siu.on.ca/en/directors_report_details.php?drid=42")
  expect_match(seen$prompt, "Report text:\n  Self-serve report $")
  # the same route with a scraper CSV that lacks the HTML cache but knows the nrid
  utils::write.csv(data.frame(case_number = "25-TST-009", drid = 42L, nrid = 77L, stringsAsFactors = FALSE),
                   file.path(empty, "SIU.csv"), row.names = FALSE)
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = "rmorie")
  withr::local_options(morie.siu.allow_fetch = NULL)
  out3 <- morie_siu_llm_extract("25-TST-009", cache_dir = empty, mock_response_text = "m")
  expect_equal(out3$drid, "42")
  expect_equal(out3$nrid, "77")
})
