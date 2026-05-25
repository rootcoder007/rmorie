# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 13 -- src/siu_parser.cpp: parser branches beyond
# test-siu.R (French language, entity decoding, decision outcomes,
# mental-health/race tagging, dateline-less news pages).

test_that(".siu_parse_report flags a French-language report", {
  fr <- paste0(
    "<html><body><h2 id=\"section_1\">Mandat de l'UES</h2>",
    "<p>Le 5 janvier 2024.</p></body></html>"
  )
  r <- morie:::.siu_parse_report(fr, 1L, "u")
  expect_equal(r[["_language"]], "fr")
})

test_that(".siu_parse_report defaults unrecognised pages to 'en'", {
  # The parser fetches via /en/... URLs, so any page lacking an
  # explicit French marker is treated as English. This replaced
  # the prior 'unknown' tag (which left 313/4743 real English
  # reports unclassified in v0.9.4); see v0.9.5 NEWS.
  r <- morie:::.siu_parse_report(
    "<html><body><p>nothing here</p></body></html>", 2L, "u"
  )
  expect_equal(r[["_language"]], "en")
  expect_equal(r[["case_number"]], "")
})

test_that(".siu_parse_report extracts a reasonable-grounds decision", {
  h <- paste0(
    "<html><body><h2 id=\"section_1\">Mandate of the SIU</h2>",
    "<h2 id=\"section_8\">Analysis and Director's Decision</h2>",
    "<p>On my assessment there are reasonable grounds to believe that ",
    "an offence occurred and the charge of assault is warranted.</p>",
    "<p>Director's Report for Case # 24-OCI-100.</p></body></html>"
  )
  r <- morie:::.siu_parse_report(h, 3L, "u")
  expect_equal(r[["directors_decision_reasonable"]], "Yes")
  expect_true(nzchar(r[["charges_recommended"]]))
  expect_equal(r[["case_number"]], "24-OCI-100")
})

test_that(".siu_parse_report decodes HTML entities in the narrative", {
  h <- paste0(
    "<html><body><h2 id=\"section_1\">Mandate of the SIU</h2>",
    "<h2 id=\"section_6\">Incident Narrative</h2>",
    "<p>The officer&#8217;s report &#8212; dated &#8220;today&#8221;",
    "&#160;&#38; duly signed by all.</p></body></html>"
  )
  ns <- morie:::.siu_parse_report(h, 4L, "u")[["narrative_summary"]]
  expect_true(grepl("officer's report", ns, fixed = TRUE))
  expect_true(grepl("&", ns, fixed = TRUE))
})

test_that(".siu_parse_report flags mental-health / race indications", {
  h <- paste0(
    "<html><body><h2 id=\"section_1\">Mandate of the SIU</h2>",
    "<h2 id=\"section_6\">Incident Narrative</h2>",
    "<p>The man was in crisis during a mental health episode; ",
    "he is Indigenous.</p></body></html>"
  )
  tags <- morie:::.siu_parse_report(h, 5L, "u")[[
    "mental_health_or_race_indications"
  ]]
  expect_true(grepl("mental health", tags))
  expect_true(grepl("Indigenous", tags))
})

test_that(".siu_parse_news handles a page with no dateline", {
  h <- paste0(
    "<html><body><h4>A Plain Release</h4>",
    "<p>Some release body text, long enough to matter.</p>",
    "</body></html>"
  )
  n <- morie:::.siu_parse_news(h, 9L, "u")
  expect_equal(n[["news_release_title"]], "A Plain Release")
  expect_equal(n[["news_release_date_raw"]], "")
})

test_that(".siu_parse_news parses a full dateline + summary", {
  h <- paste0(
    "<html><body><h1>News Release</h1>",
    "<h4>SIU Closes Hamilton Case</h4>",
    "<strong>Hamilton, ON</strong> (12 March, 2025) --- ",
    "A 22-year-old was injured. If you or someone you know ...",
    "</body></html>"
  )
  n <- morie:::.siu_parse_news(h, 10L, "u")
  expect_equal(n[["news_release_title"]], "SIU Closes Hamilton Case")
  expect_equal(n[["news_release_date_iso"]], "2025-03-12")
  expect_true(grepl("22-year-old", n[["news_release_summary"]]))
})
