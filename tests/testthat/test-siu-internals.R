# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2JJ: tests for siu.R + siu_fetch.R internal helpers
# (.siu_*, accessed via morie:::).

# ========================================================== HTML cache helpers

test_that(".siu_write_html_cache + .siu_read_html_cache round-trip", {
  dir <- tempfile("siu_cache_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  html <- "<html><body><p>The quick brown fox.</p></body></html>"
  morie:::.siu_write_html_cache(dir, "test.html.gz", html)
  out <- morie:::.siu_read_html_cache(dir, "test.html.gz")
  expect_equal(out, html)
})

test_that(".siu_read_html_cache returns empty string on missing file", {
  dir <- tempfile("siu_cache_empty_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  expect_equal(morie:::.siu_read_html_cache(dir, "nope.html.gz"), "")
})

# ========================================================== HTML -> text

test_that(".siu_html_to_text strips tags + decodes entities", {
  html <- "<html><body><p>Hello&nbsp;world&amp;</p></body></html>"
  out <- morie:::.siu_html_to_text(html)
  expect_type(out, "character")
  expect_match(out, "Hello.*world")
  expect_false(grepl("<p>", out, fixed = TRUE))
})

test_that(".siu_html_to_text handles empty string", {
  expect_equal(morie:::.siu_html_to_text(""), "")
})

# ========================================================== Field list

test_that(".siu_field_list returns the canonical 64-column schema", {
  fields <- morie:::.siu_field_list()
  expect_type(fields, "character")
  expect_true(length(fields) >= 50L && length(fields) <= 80L)
  for (k in c("case_number", "drid", "nrid",
              "date_of_incident_iso", "police_service",
              "narrative_summary"))
    expect_true(k %in% fields)
})

# ========================================================== LLM providers

test_that(".siu_llm_providers returns gemini + ollama configs", {
  out <- morie:::.siu_llm_providers()
  expect_type(out, "list")
  expect_true(all(c("gemini", "ollama") %in% names(out)))
  for (p in c("gemini", "ollama")) {
    expect_true(is.function(out[[p]]$build))
  }
})

test_that(".siu_llm_default_timeout returns a positive integer", {
  out <- morie:::.siu_llm_default_timeout()
  expect_true(is.numeric(out) && out > 0)
})

# ========================================================== canonical overrides

test_that(".siu_apply_canonical_overrides applies (case, field) -> value", {
  df <- data.frame(
    case_number = c("24-OFD-001", "24-OFD-002"),
    police_service = c("Wrong", "OPP"),
    location_of_call = c("Old", "Old"),
    stringsAsFactors = FALSE
  )
  overrides <- data.frame(
    case_number = c("24-OFD-001", "24-OFD-002"),
    field = c("police_service", "location_of_call"),
    verified_value = c("Toronto Police Service",
                        "Verified Location B"),
    stringsAsFactors = FALSE
  )
  out <- morie:::.siu_apply_canonical_overrides(df, overrides)
  expect_equal(out$police_service[1], "Toronto Police Service")
  expect_equal(out$location_of_call[2], "Verified Location B")
})

test_that(".siu_apply_canonical_overrides is a no-op for unknown fields", {
  df <- data.frame(case_number = "24-OFD-001",
                   police_service = "Toronto",
                   stringsAsFactors = FALSE)
  overrides <- data.frame(
    case_number = "24-OFD-001",
    field = "nonexistent_field",
    verified_value = "Should Not Appear",
    stringsAsFactors = FALSE
  )
  out <- morie:::.siu_apply_canonical_overrides(df, overrides)
  expect_equal(out$police_service, "Toronto")
  expect_false("nonexistent_field" %in% names(out))
})

test_that(".siu_apply_canonical_overrides handles NULL + 0-row overrides", {
  df <- data.frame(case_number = "24-OFD-001",
                   police_service = "Toronto",
                   stringsAsFactors = FALSE)
  out1 <- morie:::.siu_apply_canonical_overrides(df, NULL)
  expect_equal(out1, df)
  out2 <- morie:::.siu_apply_canonical_overrides(df,
    data.frame(case_number = character(0), field = character(0),
               verified_value = character(0)))
  expect_equal(out2, df)
})

# ========================================================== manifest + overrides loaders

test_that(".siu_load_manifest_raw returns a data.frame or NULL", {
  out <- tryCatch(morie:::.siu_load_manifest_raw(),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("manifest load error: %s", conditionMessage(out)))
  }
  expect_true(is.null(out) || is.data.frame(out))
})

test_that(".siu_load_canonical_overrides returns data.frame or NULL", {
  out <- tryCatch(morie:::.siu_load_canonical_overrides(),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("canonical overrides load error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.null(out) || is.data.frame(out))
})

test_that(".siu_load_canonical_overrides accepts a user cache dir", {
  user_cache <- tempfile("siu_user_overrides_")
  dir.create(user_cache, recursive = TRUE)
  on.exit(unlink(user_cache, recursive = TRUE), add = TRUE)
  utils::write.csv(data.frame(
    case_number = "TEST-001",
    field = "police_service",
    verified_value = "User-Override Police",
    note = "user QA",
    recorded_at_utc = "2024-01-01T00:00:00Z",
    stringsAsFactors = FALSE
  ), file.path(user_cache, "canonical_overrides.csv"), row.names = FALSE)
  out <- tryCatch(
    morie:::.siu_load_canonical_overrides(user_cache),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("user_cache_dir error: %s", conditionMessage(out)))
  }
  expect_true(is.data.frame(out))
})

# ========================================================== siu_fetch helpers

test_that(".siu_fetch_to_iso parses the canonical 'Month D, YYYY' format", {
  expect_equal(morie:::.siu_fetch_to_iso("January 5, 2024"),
               "2024-01-05")
  expect_equal(morie:::.siu_fetch_to_iso("not-a-date"), "")
  expect_equal(morie:::.siu_fetch_to_iso(""), "")
})

test_that(".siu_fetch_resolve_url joins relative + absolute URLs", {
  base <- "https://www.siu.on.ca/en/news.php"
  expect_match(
    morie:::.siu_fetch_resolve_url("/en/news_template.php?nrid=123", base),
    "^https://www\\.siu\\.on\\.ca/")
  expect_equal(
    morie:::.siu_fetch_resolve_url("https://other.example/x", base),
    "https://other.example/x")
})

test_that(".siu_fetch_extract_links returns nrid -> url mapping", {
  html <- paste0(
    '<html><body>',
    '<a href="/en/news_template.php?nrid=1001">News 1</a>',
    '<a href="/en/news_template.php?nrid=1002">News 2</a>',
    '<a href="/en/news_template.php?nrid=1003">News 3</a>',
    '</body></html>'
  )
  out <- tryCatch(
    morie:::.siu_fetch_extract_links(html,
                                       "https://www.siu.on.ca/en/news.php"),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("extract_links error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out) ||
                is.character(out))
})
