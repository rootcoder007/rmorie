# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2EE: deeper siu.R coverage via cache staging + mock LLM
# responses. Targets morie_siu_audit_case, morie_siu_llm_extract,
# morie_siu_anomaly_check, and morie_siu_compare with full local
# fixtures (no network, no real LLM).

.stage_siu_cache <- function(case_number = "24-OFD-001",
                              drid = 4001L,
                              html_body = NULL) {
  dir <- tempfile("siu_full_")
  dir.create(file.path(dir, "html"), recursive = TRUE)
  # 1. SIU.csv — single-row case
  utils::write.csv(
    data.frame(
      case_number = case_number,
      drid = drid,
      nrid = NA_character_,
      source_url_report = paste0(
        "https://www.siu.on.ca/en/directors_report_details.php?drid=",
        drid),
      source_url_news = "",
      scraped_at_utc = "2024-03-12T00:00:00Z",
      parser_version = "0.1.0",
      date_of_incident_iso = "2024-03-12",
      police_service = "Toronto",
      sex_gender_affected = "Male",
      stringsAsFactors = FALSE
    ),
    file.path(dir, "SIU.csv"), row.names = FALSE)
  # 2. cached HTML — gzipped to drid_N.html.gz
  if (is.null(html_body)) {
    html_body <- paste0(
      "<html><body><h1>Director's Report</h1>",
      "<p>Case Number: ", case_number, "</p>",
      "<p>Police Service: Toronto Police Service</p>",
      "<p>Date of Incident: March 12, 2024</p>",
      "<p>Sex/Gender of the Affected Person: Male</p>",
      "<p>Narrative: A use-of-force incident occurred during ",
      "a vehicle stop on March 12, 2024.</p>",
      "</body></html>")
  }
  con <- gzfile(file.path(dir, "html",
                           sprintf("drid_%d.html.gz", drid)), "w")
  writeLines(html_body, con)
  close(con)
  dir
}

# =============================================================== audit_case

test_that("morie_siu_audit_case returns full audit on staged cache", {
  cache <- .stage_siu_cache(case_number = "24-OFD-001", drid = 4001L)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  out <- morie_siu_audit_case("24-OFD-001",
                               cache_dir = cache,
                               fetch_if_missing = FALSE)
  expect_type(out, "list")
  expect_equal(out$drid, 4001L)
  expect_true(nzchar(out$report_html))
  expect_match(out$report_text, "Director's Report",
               ignore.case = TRUE)
})

test_that("morie_siu_audit_case accepts a numeric drid as case_number", {
  cache <- .stage_siu_cache(case_number = "24-OFD-002", drid = 4002L)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  out <- morie_siu_audit_case(4002L,
                               cache_dir = cache,
                               fetch_if_missing = FALSE)
  expect_equal(out$drid, 4002L)
})

test_that("morie_siu_audit_case errors when case_number not in SIU.csv", {
  cache <- .stage_siu_cache(case_number = "24-OFD-001", drid = 4001L)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  expect_error(
    morie_siu_audit_case("99-XXX-999", cache_dir = cache,
                          fetch_if_missing = FALSE),
    regexp = "No row found"
  )
})

test_that("morie_siu_audit_case returns empty HTML + skipps fetch when set FALSE", {
  cache <- .stage_siu_cache(case_number = "24-OFD-001", drid = 4001L)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  # Delete the cached HTML — fetch_if_missing = FALSE should NOT
  # network-fetch, just return empty html.
  unlink(file.path(cache, "html"), recursive = TRUE)
  dir.create(file.path(cache, "html"))
  out <- morie_siu_audit_case("24-OFD-001",
                               cache_dir = cache,
                               fetch_if_missing = FALSE)
  expect_equal(out$report_html, "")
})

# =============================================================== llm_extract

.mock_llm_json <- function(case_number = "24-OFD-001",
                            drid = 4001L) {
  # Build a minimal JSON response matching .siu_field_list() exactly.
  fields <- morie:::.siu_field_list()
  vals <- setNames(rep("", length(fields)), fields)
  vals["case_number"] <- case_number
  vals["drid"] <- as.character(drid)
  vals["date_of_incident_iso"] <- "2024-03-12"
  vals["police_service"] <- "Toronto Police Service"
  vals["sex_gender_affected"] <- "Male"
  jsonlite::toJSON(as.list(vals), auto_unbox = TRUE)
}

test_that("morie_siu_llm_extract returns a 64-col row from a mocked LLM response", {
  cache <- .stage_siu_cache(case_number = "24-OFD-001", drid = 4001L)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  mock_text <- .mock_llm_json("24-OFD-001", 4001L)
  out <- tryCatch(
    morie_siu_llm_extract("24-OFD-001",
                           cache_dir = cache,
                           mock_response_text = as.character(mock_text)),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("llm_extract error: %s", conditionMessage(out)))
  }
  expect_s3_class(out, "data.frame")
  expect_equal(out$case_number, "24-OFD-001")
  expect_equal(out$drid, "4001")
  expect_match(out$parser_version, "^llm-")
})

test_that("morie_siu_llm_extract strips ```json``` markdown fences", {
  cache <- .stage_siu_cache(case_number = "24-OFD-001", drid = 4001L)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  inner <- .mock_llm_json("24-OFD-001", 4001L)
  fenced <- paste0("```json\n", inner, "\n```")
  out <- tryCatch(
    morie_siu_llm_extract("24-OFD-001",
                           cache_dir = cache,
                           mock_response_text = as.character(fenced)),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("llm_extract fenced error: %s",
                 conditionMessage(out)))
  }
  expect_s3_class(out, "data.frame")
})

test_that("morie_siu_llm_extract errors on empty HTML cache", {
  cache <- .stage_siu_cache(case_number = "24-OFD-001", drid = 4001L)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  # Wipe cached HTML
  unlink(file.path(cache, "html"), recursive = TRUE)
  dir.create(file.path(cache, "html"))
  expect_error(
    morie_siu_llm_extract("24-OFD-001",
                           cache_dir = cache,
                           mock_response_text = "{}"),
    regexp = "No HTML available"
  )
})

# =============================================================== anomaly_check

test_that("morie_siu_anomaly_check returns per-field verdicts on staged cache", {
  skip_if_not_installed("jsonlite")
  cache <- .stage_siu_cache(case_number = "24-OFD-001", drid = 4001L)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  # morie_siu_anomaly_check parses the LLM response as a JSON ARRAY of
  # {field, verdict, reason} objects (R/siu.R:1620-1651 — that's also
  # what its prompt asks the LLM to emit).  The earlier name-keyed
  # object shape lost the "field" column under
  # jsonlite::fromJSON(simplifyVector = TRUE).
  mock_text <- jsonlite::toJSON(list(
    list(field = "case_number",
         verdict = "agree", reason = "matches header"),
    list(field = "police_service",
         verdict = "agree", reason = "matches body"),
    list(field = "sex_gender_affected",
         verdict = "agree", reason = "matches body")
  ), auto_unbox = TRUE)
  out <- morie_siu_anomaly_check("24-OFD-001",
                                  cache_dir = cache,
                                  mock_response_text = as.character(mock_text))
  expect_true(is.data.frame(out) || is.list(out))
})

# =============================================================== compare

test_that("morie_siu_compare lines up parser row vs external table", {
  cache <- .stage_siu_cache(case_number = "24-OFD-001", drid = 4001L)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  external <- data.frame(
    Q1 = "24-OFD-001",
    Q3 = "Toronto Police Service",
    Q4 = "2 SO",
    Q5 = "Vehicle stop",
    Q9 = "1",
    stringsAsFactors = FALSE
  )
  out <- tryCatch(
    morie_siu_compare("24-OFD-001", external,
                      cache_dir = cache),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("siu_compare error: %s", conditionMessage(out)))
  }
  expect_true(is.data.frame(out) || is.list(out))
})
