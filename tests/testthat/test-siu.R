# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for the all-C/C++ Ontario SIU parser (src/siu_parser.cpp) and
# its R orchestration (R/siu.R). The HTML parsers are tested offline
# against synthetic fixtures that mirror the real SIU page structure;
# the libcurl transport and the end-to-end run are network-gated.

# A synthetic director's-report page with the real section skeleton.
.fake_siu_report <- function() {
  paste0(
    "<html><body>",
    "<h2 id=\"section_1\">Mandate of the SIU</h2><p>boilerplate text</p>",
    "<h2 id=\"section_4\">The Investigation</h2>",
    "<p>Notification of the SIU On January 5, 2024, at 9:00 a.m., the ",
    "Waterloo Regional Police Service (WRPS) contacted the SIU. ",
    "On January 6, 2024, officers responded. SO #1 attended with WO #1 ",
    "and WO #2. CW #1 was interviewed. The Waterloo Regional Police ",
    "Service confirmed the arrest of a 34-year-old man.</p>",
    "<h2 id=\"section_6\">Incident Narrative</h2>",
    "<p>On January 6, 2024, the man was arrested by Waterloo Regional ",
    "Police Service officers. He was injured. His arrest followed a ",
    "call. The man did not resist further.</p>",
    "<h2 id=\"section_8\">Analysis and Director's Decision</h2>",
    "<p>The Complainant was injured on January 6, 2024. On my ",
    "assessment of the evidence, there are no reasonable grounds to ",
    "believe that an officer committed a criminal offence.</p>",
    "<p>Director's Report for Case # 24-TCI-099.</p>",
    "<p>Date: March 3, 2024 Electronically approved by Jane Doe ",
    "Director Special Investigations Unit</p>",
    "<p>News Releases for this Case: ",
    "<a href=\"/en/news_template.php?nrid=999\">No Charges in Waterloo ",
    "Arrest</a></p></body></html>"
  )
}

# A synthetic news-release page with the real dateline structure.
.fake_siu_news <- function() {
  paste0(
    "<html><body><h1>News Release</h1>",
    "<h4>No Charges in Waterloo Arrest</h4>",
    "<strong>Kitchener, ON</strong> (3 March, 2024) --- ",
    "A 34-year-old man was injured during an arrest by police. ",
    "Full Director's Report follows.</body></html>"
  )
}

test_that(".siu_parse_report extracts the 64-column schema", {
  r <- morie:::.siu_parse_report(
    .fake_siu_report(), 4242L,
    "http://x/drid=4242"
  )
  expect_length(r, 64L)
  expect_equal(r[["case_number"]], "24-TCI-099")
  expect_equal(r[["drid"]], "4242")
  expect_equal(r[["_language"]], "en")
  expect_equal(r[["police_service"]], "Waterloo Regional Police Service")
  expect_equal(r[["date_siu_notified_iso"]], "2024-01-05")
  expect_equal(r[["date_of_incident_iso"]], "2024-01-06")
  expect_equal(r[["date_of_director_decision_iso"]], "2024-03-03")
  expect_equal(r[["directors_name"]], "Jane Doe")
  expect_equal(r[["number_of_subject_officials"]], "1")
  expect_equal(r[["number_of_witness_officials"]], "2")
  expect_equal(r[["number_of_civilian_witnesses"]], "1")
  expect_equal(r[["age_affected"]], "34")
  expect_equal(r[["sex_gender_affected"]], "Male")
  expect_equal(r[["directors_decision_reasonable"]], "No")
  expect_equal(r[["nrid"]], "999")
  expect_true(nzchar(r[["narrative_summary"]]))
  expect_match(r[["parser_version"]], "^[0-9]")
})

test_that(".siu_parse_report handles an empty / non-existent drid page", {
  r <- morie:::.siu_parse_report(
    "<html><body></body></html>", 7L,
    "http://x/drid=7"
  )
  expect_length(r, 64L)
  expect_equal(r[["drid"]], "7")
  expect_equal(r[["case_number"]], "")
})

test_that(".siu_parse_news extracts title, date and summary", {
  n <- morie:::.siu_parse_news(
    .fake_siu_news(), 999L,
    "http://x/nrid=999"
  )
  expect_equal(n[["nrid"]], "999")
  expect_equal(n[["news_release_title"]], "No Charges in Waterloo Arrest")
  expect_equal(n[["news_release_date_iso"]], "2024-03-03")
  expect_equal(n[["news_release_date_raw"]], "3 March, 2024")
  expect_true(grepl("34-year-old", n[["news_release_summary"]]))
})

test_that(".siu_parse_report recognises a non-binary affected person", {
  html <- sub("34-year-old man", "34-year-old non-binary person",
    .fake_siu_report(),
    fixed = TRUE
  )
  r <- morie:::.siu_parse_report(html, 1L, "http://x")
  expect_equal(r[["sex_gender_affected"]], "Non-binary")
})

test_that(".siu_discover_max_drid parses the index and adds a margin", {
  testthat::local_mocked_bindings(
    .siu_http_get = function(url, ...) {
      "<tr class=\"dr-item\" id=\"5090\"><tr class=\"dr-item\" id=\"5094\">"
    },
    .package = "morie"
  )
  expect_equal(morie:::.siu_discover_max_drid(margin = 10L), 5104L)
})

test_that("morie_fetch_siu assembles one row per case (offline, mocked)", {
  # covr instruments R source without loading the C++ .so, so the
  # .Call to .siu_http_get_many fails under covr even with mocked
  # bindings. The mock chain itself is exercised by the smaller
  # offline tests below; skip this end-to-end one under covr.
  skip_if(
    !is.null(getOption("covr.flags")),
    "skipped under covr (compiled .so not loaded)"
  )
  testthat::local_mocked_bindings(
    .siu_http_get_many = function(urls, ...) {
      vapply(urls, function(u) {
        if (grepl("nrid=999", u, fixed = TRUE)) {
          .fake_siu_news()
        } else if (grepl("drid=1$", u)) {
          .fake_siu_report()
        } else {
          ""
        }
      }, character(1), USE.NAMES = FALSE)
    },
    .package = "morie"
  )
  out <- morie_fetch_siu(
    cache_dir = tempfile("siu-"), overwrite = TRUE,
    max_drid = 3L, use_manifest = FALSE, progress = FALSE
  )
  df <- utils::read.csv(out, colClasses = "character", check.names = FALSE)
  expect_equal(ncol(df), 64L)
  expect_equal(nrow(df), 1L)
  expect_equal(df$case_number, "24-TCI-099")
  expect_equal(df$nrid, "999")
  # news fields joined from the (mocked) news page
  expect_equal(df$news_release_date_iso, "2024-03-03")
  expect_true(grepl("Waterloo", df$news_release_title))
})

test_that("morie_fetch_siu returns the cached path without re-fetching", {
  dir <- tempfile("siu-")
  dir.create(dir)
  writeLines("case_number\n24-X", file.path(dir, "SIU.csv"))
  expect_equal(
    normalizePath(morie_fetch_siu(
      cache_dir = dir,
      progress = FALSE
    )),
    normalizePath(file.path(dir, "SIU.csv"))
  )
})

test_that(".siu_curl_version reports a libcurl build string", {
  v <- morie:::.siu_curl_version()
  expect_type(v, "character")
  expect_match(v, "libcurl", fixed = TRUE)
})

test_that(".siu_http_get / .siu_http_get_many fetch over the network", {
  testthat::skip_if_offline("www.siu.on.ca")
  one <- tryCatch(
    morie:::.siu_http_get(
      "https://www.siu.on.ca/en/directors_report_details.php?drid=5080"
    ),
    error = function(e) ""
  )
  # Debug line: surface body size when CI surprises us. Distinguishes
  # WAF interstitial (~500B) vs rate-limit page (~200B) vs 5xx HTML
  # template (~1-2 kB) vs healthy report (~60 kB). Visible in the
  # testthat output regardless of pass/fail.
  cat(sprintf(
    "\n[test-siu.R] .siu_http_get drid=5080 body_bytes=%d\n",
    nchar(one)
  ))
  # Degraded responses (empty, error pages, 5xx HTML stubs, redirects) all
  # fall well under 1 kB. Skip on any such response — the test only
  # validates a healthy endpoint, transient flakiness shouldn't fail CI.
  skip_if(nchar(one) < 1000, "SIU site unreachable or degraded")
  expect_true(nchar(one) > 1000)
  many <- tryCatch(
    morie:::.siu_http_get_many(sprintf(
      "https://www.siu.on.ca/en/directors_report_details.php?drid=%d",
      5080:5083
    ), 4L),
    error = function(e) character(0)
  )
  skip_if(length(many) != 4L, "SIU site unreachable for batch fetch")
  expect_length(many, 4L)
  expect_true(all(nchar(many) > 0))
})

test_that("morie_fetch_siu runs end-to-end, one row per case (network)", {
  testthat::skip_if_offline("www.siu.on.ca")
  out <- tryCatch(
    morie_fetch_siu(
      cache_dir = tempfile("siu-"), overwrite = TRUE,
      max_drid = 120L, concurrency = 4L, rate_rps = 4.0,
      use_manifest = FALSE, progress = FALSE
    ),
    error = function(e) NULL
  )
  skip_if(is.null(out), "SIU site unreachable")
  df <- utils::read.csv(out, colClasses = "character", check.names = FALSE)
  expect_equal(ncol(df), 64L)
  expect_true(all(c("case_number", "drid", "nrid") %in% names(df)))
  expect_equal(sum(duplicated(df$case_number)), 0L)
  expect_true(all(nzchar(df$case_number)))
})

test_that(".siu_http_get_many_with_status returns parallel slots", {
  # Offline-safe: a length-zero URL vector should still return the
  # three-slot list shape, exercising the C++ early-return path.
  res <- morie:::.siu_http_get_many_with_status(character(0))
  expect_named(res, c("body", "http_code", "attempts"))
  expect_length(res$body, 0L)
  expect_length(res$http_code, 0L)
  expect_length(res$attempts, 0L)
})

test_that(".siu_http_get_many rate-limit gate spaces requests (network)", {
  testthat::skip_if_offline("www.siu.on.ca")
  # 8 requests at 4 rps should take >= ~1.5s of pure gating overhead
  # (gap between starts = 250 ms; 8 - 1 = 7 gaps -> 1.75s floor before
  # any request latency). Measure to confirm the throttle activates.
  urls <- sprintf(
    "https://www.siu.on.ca/en/directors_report_details.php?drid=%d",
    5070:5077
  )
  t0 <- Sys.time()
  res <- tryCatch(
    morie:::.siu_http_get_many_with_status(
      urls,
      concurrency = 8L, timeout_s = 30L,
      rate_rps = 4.0, max_retries = 1L
    ),
    error = function(e) NULL
  )
  skip_if(is.null(res), "SIU site unreachable")
  skip_if(any(nchar(res$body) < 1000), "SIU served degraded responses")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_true(elapsed >= 1.5,
    info = sprintf(
      "elapsed=%.2fs < 1.5s -> throttle not gating",
      elapsed
    )
  )
  expect_true(all(res$http_code == 200L))
})

test_that("html_to_text handles pathological input without segfault", {
  # The pre-fix std::regex html_to_text could blow the C stack on
  # unclosed <script> tags or megabyte-scale bodies. These three
  # adversarial inputs all crashed the parser pre-fix; they must now
  # return cleanly (any string, including "").
  expect_silent(morie:::.siu_parse_report(
    paste0(
      "<html><body><script>", strrep("x", 200000L),
      " // unclosed script"
    ),
    9991L, "test"
  ))
  expect_silent(morie:::.siu_parse_report(
    paste0(strrep("<a>", 10000L), "case # 99-TST-001"),
    9992L, "test"
  ))
  expect_silent(morie:::.siu_parse_report(
    paste0("<html><body>", strrep("plain text ", 600000L), "</body></html>"),
    9993L, "test"
  ))
})

test_that("morie_siu_audit_case errors cleanly without a SIU.csv", {
  d <- tempfile("siu-")
  dir.create(d)
  expect_error(
    morie_siu_audit_case("17-OVI-201", cache_dir = d),
    "No SIU.csv"
  )
})

test_that("morie_siu_audit_case reads from cached HTML", {
  # Build a minimal fake cache: one SIU.csv row + one cached drid HTML.
  d <- tempfile("siu-")
  dir.create(file.path(d, "html"), recursive = TRUE)
  hdr <- paste(names(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  )), collapse = ",")
  vals <- as.character(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  ))
  vals <- gsub("\"", "'", vals, fixed = TRUE) # avoid CSV-quote pain
  writeLines(
    c(hdr, paste(sprintf("\"%s\"", vals), collapse = ",")),
    file.path(d, "SIU.csv")
  )
  # Cache the HTML at drid_1.html.gz.
  con <- gzfile(file.path(d, "html", "drid_1.html.gz"), "w")
  writeChar(.fake_siu_report(), con, eos = NULL)
  close(con)

  case_no <- vals[1L]
  a <- morie_siu_audit_case(case_no,
    cache_dir = d,
    fetch_if_missing = FALSE
  )
  expect_equal(a$drid, 1L)
  expect_true(nzchar(a$report_html))
  expect_true(nzchar(a$report_text))
  expect_false(grepl("<", a$report_text, fixed = TRUE)) # stripped
})

test_that("LLM providers table has the four documented backends", {
  ps <- morie:::.siu_llm_providers()
  expect_setequal(names(ps), c("gemini", "claude", "vertex", "ollama"))
  expect_equal(ps$gemini$env_required, "GOOGLE_API_KEY")
  expect_equal(ps$claude$env_required, "ANTHROPIC_API_KEY")
  expect_equal(ps$vertex$env_required, "VERTEX_ACCESS_TOKEN")
  # Ollama uses a sentinel that triggers the localhost-default
  # branch in the dispatcher, so OLLAMA_HOST being unset still
  # works out of the box on a freshly-installed local daemon.
  expect_equal(ps$ollama$env_required, "OLLAMA_HOST_OR_DEFAULT")
})

test_that("morie_siu_llm_extract returns a 64-col row from mocked JSON", {
  d <- tempfile("siu-")
  dir.create(file.path(d, "html"), recursive = TRUE)
  hdr <- paste(names(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  )), collapse = ",")
  vals <- as.character(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  ))
  vals <- gsub("\"", "'", vals, fixed = TRUE)
  writeLines(
    c(hdr, paste(sprintf("\"%s\"", vals), collapse = ",")),
    file.path(d, "SIU.csv")
  )
  con <- gzfile(file.path(d, "html", "drid_1.html.gz"), "w")
  writeChar(.fake_siu_report(), con, eos = NULL)
  close(con)

  mock <- '{"case_number":"24-TCI-099","police_service":"Waterloo Regional Police Service","number_of_officers_involved":"2","charges_recommended":"false"}'
  case_no <- vals[1L]
  r <- morie_siu_llm_extract(case_no,
    cache_dir = d,
    mock_response_text = mock
  )
  expect_s3_class(r, "data.frame")
  expect_equal(nrow(r), 1L)
  expect_equal(ncol(r), 64L)
  expect_equal(r$police_service, "Waterloo Regional Police Service")
  expect_equal(r$number_of_officers_involved, "2")
  # parser_version overwritten to identify the LLM run
  expect_match(r$parser_version, "^llm-")
})

test_that("morie_siu_anomaly_check returns per-field verdicts", {
  d <- tempfile("siu-")
  dir.create(file.path(d, "html"), recursive = TRUE)
  hdr <- paste(names(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  )), collapse = ",")
  vals <- as.character(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  ))
  vals <- gsub("\"", "'", vals, fixed = TRUE)
  writeLines(
    c(hdr, paste(sprintf("\"%s\"", vals), collapse = ",")),
    file.path(d, "SIU.csv")
  )
  con <- gzfile(file.path(d, "html", "drid_1.html.gz"), "w")
  writeChar(.fake_siu_report(), con, eos = NULL)
  close(con)

  # Mock LLM response: a JSON array of {field, verdict, reason}
  mock <- paste0(
    '[{"field":"police_service","verdict":"agree",',
    '"reason":"named in notification"},',
    '{"field":"charges_recommended","verdict":"agree",',
    '"reason":"no reasonable grounds"}]'
  )
  case_no <- vals[1L]
  a <- morie_siu_anomaly_check(case_no,
    cache_dir = d,
    mock_response_text = mock
  )
  expect_s3_class(a, "data.frame")
  expect_named(a, c("field", "parser_value", "verdict", "reason"))
  expect_true(all(a$verdict %in% c("agree", "disagree", "unclear")))
})

test_that(".siu_llm_call fails fast when no env vars are set", {
  withr::with_envvar(
    c(
      GOOGLE_API_KEY = "", ANTHROPIC_API_KEY = "",
      OLLAMA_HOST = "", VERTEX_ACCESS_TOKEN = ""
    ),
    expect_error(
      morie:::.siu_llm_call("gemini", "test prompt"),
      "GOOGLE_API_KEY"
    )
  )
})

test_that("morie_siu_compare lines up parser vs external table", {
  # Build a tiny fake SIU.csv + HTML cache, then compare against a
  # user-supplied external table. No network, no LLM.
  d <- tempfile("siu-")
  dir.create(file.path(d, "html"), recursive = TRUE)
  hdr <- paste(names(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  )), collapse = ",")
  vals <- as.character(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  ))
  vals <- gsub("\"", "'", vals, fixed = TRUE)
  writeLines(
    c(hdr, paste(sprintf("\"%s\"", vals), collapse = ",")),
    file.path(d, "SIU.csv")
  )
  con <- gzfile(file.path(d, "html", "drid_1.html.gz"), "w")
  writeChar(.fake_siu_report(), con, eos = NULL)
  close(con)
  case_no <- vals[1L]

  # External "human-coded" table: agrees on case_number, disagrees
  # on number_of_subject_officials (calls it Officials, not the parsed value)
  ext <- data.frame(
    case_id = case_no,
    officers = "12-disagreement", # deliberate disagreement
    stringsAsFactors = FALSE
  )
  out <- morie_siu_compare(
    case_no,
    external = ext,
    field_map = list(officers = "number_of_subject_officials"),
    external_case_col = "case_id",
    cache_dir = d
  )
  expect_s3_class(out, "data.frame")
  expect_named(out, c(
    "field", "parser_value", "external_value",
    "agree", "html_excerpt"
  ))
  expect_equal(nrow(out), 1L)
  expect_equal(out$field, "number_of_subject_officials")
  expect_equal(out$external_value, "12-disagreement")
  expect_false(out$agree)
})

test_that("morie_siu_translate routes through provider with target lang", {
  # Verify the dispatch path: mocked LLM returns a translated JSON;
  # check the override gets written with target_lang in the note.
  d <- tempfile("siu-")
  dir.create(file.path(d, "html"), recursive = TRUE)
  hdr <- paste(names(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  )), collapse = ",")
  vals <- as.character(morie:::.siu_parse_report(
    .fake_siu_report(),
    1L, "x"
  ))
  vals <- gsub("\"", "'", vals, fixed = TRUE)
  writeLines(
    c(hdr, paste(sprintf("\"%s\"", vals), collapse = ",")),
    file.path(d, "SIU.csv")
  )
  con <- gzfile(file.path(d, "html", "drid_1.html.gz"), "w")
  writeChar(.fake_siu_report(), con, eos = NULL)
  close(con)
  case_no <- vals[1L]

  # Force the row's _language to differ from target so translation is
  # selected. The fake report's narrative_summary will become the
  # "translated" text via the mock.
  mock <- paste0(
    '{"narrative_summary":"Translated narrative.",',
    '"news_release_title":"Translated title."}'
  )
  res <- testthat::local_mocked_bindings(
    .siu_llm_call = function(model, prompt, timeout_s = NULL,
                             mock_response_text = NULL) {
      mock
    },
    .package = "morie"
  )
  # Force translate even though _language might match
  out <- morie_siu_translate(
    target_lang = "hi", # Hindi — anything ≠ "en"
    case_numbers = case_no,
    model = "ollama",
    cache_dir = d, progress = FALSE
  )
  expect_s3_class(out, "data.frame")
  expect_true("narrative_summary" %in% out$field)
  expect_true(any(out$verified_value == "Translated narrative."))
})

test_that(".siu_apply_canonical_overrides overwrites named cells", {
  df <- data.frame(
    case_number = c("17-OVI-201", "20-OCI-040"),
    drid = c("46", "1995"),
    location_of_call = c("Clair Road", "Hwy 401"),
    mental_health_or_race_indications = c("", ""),
    stringsAsFactors = FALSE
  )
  overrides <- data.frame(
    case_number = c("17-OVI-201", "20-OCI-040", "99-XXX-999"),
    field = c(
      "location_of_call",
      "mental_health_or_race_indications",
      "location_of_call"
    ), # third one: case not in df, no-op
    verified_value = c("Clair Road East, City of Guelph", "Black", "ignored"),
    stringsAsFactors = FALSE
  )
  out <- morie:::.siu_apply_canonical_overrides(df, overrides)
  expect_equal(out$location_of_call[1L], "Clair Road East, City of Guelph")
  expect_equal(out$mental_health_or_race_indications[2L], "Black")
  # Untouched cells stay
  expect_equal(out$location_of_call[2L], "Hwy 401")
  # Phantom case_number doesn't add a row
  expect_equal(nrow(out), 2L)
})

test_that("morie_siu_record_correction round-trips through cache_dir", {
  d <- tempfile("siu-")
  morie_siu_record_correction(
    case_number = "17-OVI-201",
    field = "location_of_call",
    verified_value = "Clair Road East, City of Guelph",
    note = "HTML excerpt confirms",
    cache_dir = d
  )
  # File created
  p <- file.path(d, "canonical_overrides.csv")
  expect_true(file.exists(p))
  # Loader reads it back
  loaded <- morie:::.siu_load_canonical_overrides(user_cache_dir = d)
  expect_equal(loaded$verified_value[
    loaded$case_number == "17-OVI-201" &
      loaded$field == "location_of_call"
  ], "Clair Road East, City of Guelph")
  # Subsequent write for the SAME (case, field) overwrites, doesn't dupe
  morie_siu_record_correction(
    case_number = "17-OVI-201",
    field = "location_of_call",
    verified_value = "(updated)",
    cache_dir = d
  )
  reread <- utils::read.csv(p, colClasses = "character")
  expect_equal(sum(reread$case_number == "17-OVI-201" &
    reread$field == "location_of_call"), 1L)
  expect_equal(reread$verified_value[
    reread$case_number == "17-OVI-201" &
      reread$field == "location_of_call"
  ], "(updated)")
})

test_that("morie_siu_record_correction rejects unknown field names", {
  d <- tempfile("siu-")
  expect_error(
    morie_siu_record_correction("17-OVI-201", "not_a_real_field",
      "x",
      cache_dir = d
    ),
    "field"
  )
})

test_that("morie_siu_sanity_check flags malformed cells", {
  # Hand-crafted "parsed" rows: some clean, some deliberately bad.
  df <- data.frame(
    case_number = c("24-TCI-099", "24-TCI-100", "BAD-FORMAT"),
    drid = c("1", "2", "3"),
    date_of_incident_iso = c("2024-03-03", "March 3, 2024", ""),
    date_of_injury_iso = c("", "", ""),
    date_siu_notified_iso = c("", "", ""),
    date_of_director_decision_iso = c("", "", ""),
    number_of_affected_persons = c("1", "1", ""),
    number_of_civilian_witnesses = c("3", "abc", ""),
    number_of_subject_officials = c("1", "1", ""),
    number_of_witness_officials = c("2", "2", ""),
    age_affected = c("34", "", "thirty"),
    number_of_officers_involved = c("1 SO 2 WO", "10 officers", "1 SO"),
    charges_recommended = c("No", "Yes", "maybe"),
    directors_decision_reasonable = c("No", "Yes", "Maybe"),
    sex_gender_affected = c("Male", "Female", "Other"),
    police_service = c("Toronto Police", "", "Hamilton Police"),
    narrative_summary = c(
      paste(
        "On January 5, 2024 at approximately 09:30 hours the",
        "complainant was arrested by Subject Officer #1 of the",
        "Toronto Police Service following a call for service",
        "from a member of the public reporting suspicious",
        "activity in the area of Dundas and Kipling. Officers",
        "responded and conducted an investigation."
      ),
      "",
      "First Nations, Inuit and Métis Liaison Program"
    ),
    supplemental_materials = c("", "", "https://twitter.com/SIUOntario"),
    mental_health_or_race_indications = c(
      "", "",
      # The literal page-chrome phrase that escaped sectioning bug:
      "First Nations, Inuit and Métis Liaison Program"
    ),
    stringsAsFactors = FALSE
  )

  out <- morie_siu_sanity_check(df)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_true(all(c("case_number", "drid", "issues_count", "issues")
  %in% names(out)))

  # Row 1 (24-TCI-099) is mostly clean -- 0 issues
  r1 <- out[out$case_number == "24-TCI-099", , drop = FALSE]
  expect_equal(r1$issues_count, 0L)

  # Row 2 (24-TCI-100) has malformed iso date + non-int CW count +
  # bad officer-count format + empty police_service + empty narrative
  r2 <- out[out$case_number == "24-TCI-100", , drop = FALSE]
  expect_gte(r2$issues_count, 4L)
  expect_match(r2$issues, "date_of_incident_iso:bad-iso")
  expect_match(r2$issues, "number_of_civilian_witnesses:not-int")
  expect_match(r2$issues, "number_of_officers_involved:bad-format")
  expect_match(r2$issues, "police_service:empty-when-expected")

  # Row 3 (BAD-FORMAT) has the most issues (bad case_number, bad age,
  # bad charges, bad gender, page-chrome leak)
  r3 <- out[out$case_number == "BAD-FORMAT", , drop = FALSE]
  expect_gte(r3$issues_count, 5L)
  expect_match(r3$issues, "case_number:bad-format")
  expect_match(r3$issues, "narrative_summary:page-chrome-leak")
  expect_match(r3$issues, "mental_health_or_race_indications:page-chrome-leak")

  # Output is sorted worst-first
  expect_equal(out$issues_count, sort(out$issues_count, decreasing = TRUE))
})

test_that("morie_siu_audit_columns errors when no cases succeed", {
  d <- tempfile("siu-")
  dir.create(d, recursive = TRUE)
  # No SIU.csv -> anomaly_check errors -> audit_columns has no usable data
  withr::with_envvar(
    c(GOOGLE_API_KEY = "", OLLAMA_HOST = ""),
    expect_error(
      morie_siu_audit_columns("99-XXX-999",
        model = "gemini",
        cache_dir = d, progress = FALSE
      ),
      "All audit attempts failed"
    )
  )
})

test_that(".siu_llm_call chain failover surfaces all provider errors", {
  # NB: OLLAMA_HOST defaults to localhost:11434 when unset, so to
  # force a failure on the ollama provider here we have to either
  # point it somewhere unreachable OR rely on no daemon being up.
  # We point it at an unbindable port to make the failure
  # deterministic regardless of whether a local Ollama daemon is
  # running on the test machine.
  withr::with_envvar(
    c(
      GOOGLE_API_KEY = "", ANTHROPIC_API_KEY = "",
      VERTEX_ACCESS_TOKEN = "",
      OLLAMA_HOST = "http://127.0.0.1:1"
    ),
    expect_error(
      morie:::.siu_llm_call(c("gemini", "ollama"), "test prompt",
        timeout_s = 2L
      ),
      "All LLM providers failed"
    )
  )
})

test_that(".siu_load_manifest returns NULL when no manifest is shipped", {
  # Until the shipped manifest lands at inst/extdata/, the loader must
  # gracefully return NULL so the harvester degrades to a full sweep.
  # When the manifest does ship, this test will exercise the parse
  # path instead (still expects a tibble-like data.frame).
  m <- morie:::.siu_load_manifest()
  if (is.null(m)) {
    expect_null(m)
  } else {
    expect_s3_class(m, "data.frame")
    expect_true(all(c("drid", "http_code", "body_bytes", "case_number")
    %in% names(m)))
    expect_true(all(m$http_code == 200L))
    expect_true(all(m$body_bytes >= 1000L))
    expect_true(all(nzchar(m$case_number)))
  }
})
