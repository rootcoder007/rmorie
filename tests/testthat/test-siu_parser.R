# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tests for R/siu_parser.R --- pure-R SIU director's-report HTML parser.

set.seed(1)

# Tiny inline fixtures (no external files; no network).
.fake_report_html <- function() {
  paste0(
    "<html><body>",
    "<h1>Director's Report</h1>",
    "<p>Mandate of the SIU</p>",
    "<p>Mandate of the SIU</p>",
    "<p>Case Number: 22-OCI-123</p>",
    "<p>Police Service: Toronto Police Service</p>",
    "<p>Date of Incident: March 5, 2022</p>",
    "<p>Date SIU was Notified: March 6, 2022</p>",
    "<p>Date of SIU Director's Decision: November 1, 2022</p>",
    "<p>Number of Officers: 3</p>",
    "<p>Location: Downtown</p>",
    "<p>Reason for Interaction: traffic stop</p>",
    "<p>Charges Recommended: No</p>",
    "<p>The Investigation</p>",
    "<p>A 35-year-old man was involved in an incident with mental health",
    " concerns and the Toronto Police Service.</p>",
    "<p>Notification of the SIU</p>",
    "<a href='news_template.php?nrid=4567'>news</a>",
    "<p>Endnotes</p>",
    "</body></html>"
  )
}

.fake_news_html <- function() {
  paste0(
    "<html><body>\
",
    "News Release\
",
    "Media Centre\
",
    "News Release\
",
    "SIU Concludes Investigation Into Downtown Incident\
",
    "Toronto, ON (5 November, 2022) -- The SIU has concluded its ",
    "investigation. Director Joseph Martino said no charges will be ",
    "laid.\
\
",
    "Director of the Special Investigations Unit, Joseph Martino",
    "</body></html>"
  )
}


# ---------------------------------------------------------------------------
# morie_siu_parse_html --- happy path
# ---------------------------------------------------------------------------

test_that("parse_html returns a list with all SIU column keys", {
  set.seed(1)
  r <- morie_siu_parse_html(.fake_report_html(),
                            drid = 1234L,
                            source_url = "https://example/foo.php?drid=1234")
  expect_type(r, "list")
  expect_true("case_number" %in% names(r))
  expect_true("narrative_full" %in% names(r))
  expect_true("parser_version" %in% names(r))
})

test_that("parse_html extracts the case_number", {
  set.seed(1)
  r <- morie_siu_parse_html(.fake_report_html(), drid = 1234L)
  expect_equal(r$case_number, "22-OCI-123")
})

test_that("parse_html records the drid", {
  set.seed(1)
  r <- morie_siu_parse_html(.fake_report_html(), drid = 9999L)
  expect_equal(r$drid, 9999L)
})

test_that("parse_html parses drid from URL when not given", {
  set.seed(1)
  r <- morie_siu_parse_html(.fake_report_html(),
                            source_url = "https://x/y.php?drid=4242")
  expect_equal(r$drid, 4242L)
})

test_that("parse_html detects English language", {
  set.seed(1)
  r <- morie_siu_parse_html(.fake_report_html(), drid = 1L)
  expect_equal(r$`_language`, "en")
})

test_that("parse_html records source URL", {
  set.seed(1)
  r <- morie_siu_parse_html(.fake_report_html(), drid = 1L,
                            source_url = "https://siu.on.ca/x.php")
  expect_equal(r$source_url_report, "https://siu.on.ca/x.php")
})

test_that("parse_html captures non-empty narrative_full", {
  set.seed(1)
  r <- morie_siu_parse_html(.fake_report_html(), drid = 1L)
  expect_true(nzchar(r$narrative_full %||% ""))
})

test_that("parse_html populates mental_health_or_race_indications", {
  set.seed(1)
  r <- morie_siu_parse_html(.fake_report_html(), drid = 1L)
  expect_true(grepl("mental health", r$mental_health_or_race_indications,
                    ignore.case = TRUE))
})

test_that("parse_html on empty HTML still returns a row of NAs", {
  set.seed(1)
  r <- morie_siu_parse_html("<html></html>", drid = NA_integer_)
  expect_type(r, "list")
  expect_true(is.na(r$case_number))
})

test_that("parse_html on French marker text returns lang=fr and short-circuits", {
  set.seed(1)
  fr <- paste0("<html><body>",
               "L'enquete et l'Exercice du mandat. ",
               "Elements de preuve. Temoins civils. ",
               "Agents impliques. Mandat de l'UES.",
               "</body></html>")
  r <- morie_siu_parse_html(fr, drid = 1L)
  expect_equal(r$`_language`, "fr")
})


# ---------------------------------------------------------------------------
# morie_siu_parse_news_html
# ---------------------------------------------------------------------------

test_that("parse_news_html returns the expected keys", {
  set.seed(1)
  r <- morie_siu_parse_news_html(.fake_news_html(), nrid = 4567L)
  expect_type(r, "list")
  expect_true(all(c("nrid", "news_release_title",
                    "news_release_date_iso", "directors_name") %in%
                  names(r)))
})

test_that("parse_news_html records the nrid", {
  set.seed(1)
  r <- morie_siu_parse_news_html(.fake_news_html(), nrid = 4567L)
  expect_equal(r$nrid, 4567L)
})

test_that("parse_news_html records source URL", {
  set.seed(1)
  r <- morie_siu_parse_news_html(.fake_news_html(), nrid = 4567L,
                                 source_url = "https://siu/n.php?nrid=4567")
  expect_equal(r$source_url_news, "https://siu/n.php?nrid=4567")
})

test_that("parse_news_html on empty input returns NA fields", {
  set.seed(1)
  r <- morie_siu_parse_news_html("", nrid = NA_integer_)
  expect_true(is.na(r$news_release_title))
  expect_true(is.na(r$directors_name))
})


# ---------------------------------------------------------------------------
# Internal helpers exposed via ::: only
# ---------------------------------------------------------------------------

test_that(".siu_p_blank_row template carries the SIU columns", {
  set.seed(1)
  bl <- morie:::.siu_p_blank_row()
  expect_type(bl, "list")
  expect_true("parser_version" %in% names(bl))
  expect_true("case_number" %in% names(bl))
  expect_true(is.na(bl$drid))
})

test_that(".siu_p_has_rvest returns logical", {
  set.seed(1)
  expect_true(is.logical(morie:::.siu_p_has_rvest()))
})

test_that(".siu_p_stripped_text removes script/style tags", {
  set.seed(1)
  h <- "<html><script>alert('x')</script><p>Hello world</p></html>"
  out <- morie:::.siu_p_stripped_text(h)
  expect_false(grepl("alert", out))
  expect_true(grepl("Hello", out))
})

test_that(".siu_p_stripped_text decodes &", {
  set.seed(1)
  out <- morie:::.siu_p_stripped_text("<p>A & B</p>")
  expect_true(grepl("A & B", out, fixed = TRUE))
})

test_that(".siu_p_re_escape escapes regex specials", {
  set.seed(1)
  s <- morie:::.siu_p_re_escape("Hello.World(*)")
  # The escape inserts a backslash before each regex special, so
  # the output literally contains "Hello\.World".
  expect_true(grepl("Hello\\\\.World", s) || grepl("\\\\\\.", s))
})

test_that(".siu_p_parse_drid_from_url extracts the integer", {
  set.seed(1)
  expect_equal(
    morie:::.siu_p_parse_drid_from_url("foo.php?drid=42&x=y"), 42L)
})

test_that(".siu_p_parse_drid_from_url returns NA on missing", {
  set.seed(1)
  expect_true(is.na(morie:::.siu_p_parse_drid_from_url("no-drid-here")))
})

test_that(".siu_p_parse_drid_from_url handles NULL/empty", {
  set.seed(1)
  expect_true(is.na(morie:::.siu_p_parse_drid_from_url(NULL)))
  expect_true(is.na(morie:::.siu_p_parse_drid_from_url("")))
})

test_that(".siu_p_parse_nrid_from_url extracts the integer", {
  set.seed(1)
  expect_equal(
    morie:::.siu_p_parse_nrid_from_url("x.php?nrid=99"), 99L)
})

test_that(".siu_p_normalise_sex maps common variants", {
  set.seed(1)
  expect_equal(morie:::.siu_p_normalise_sex("woman"), "Female")
  expect_equal(morie:::.siu_p_normalise_sex("man"), "Male")
  expect_equal(morie:::.siu_p_normalise_sex("F"), "Female")
  expect_equal(morie:::.siu_p_normalise_sex("nonbinary"), "Non-binary")
  expect_true(is.na(morie:::.siu_p_normalise_sex(NA)))
  expect_true(is.na(morie:::.siu_p_normalise_sex("")))
})

test_that(".siu_p_normalise_yes_no handles common variants", {
  set.seed(1)
  expect_true(morie:::.siu_p_normalise_yes_no("yes"))
  expect_true(morie:::.siu_p_normalise_yes_no("TRUE"))
  expect_true(morie:::.siu_p_normalise_yes_no("1"))
  expect_false(morie:::.siu_p_normalise_yes_no("no"))
  expect_false(morie:::.siu_p_normalise_yes_no("0"))
  expect_true(is.na(morie:::.siu_p_normalise_yes_no(NA)))
  expect_true(is.na(morie:::.siu_p_normalise_yes_no("maybe")))
})

test_that(".siu_p_parse_date parses 'Month DD YYYY'", {
  set.seed(1)
  d <- morie:::.siu_p_parse_date("March 5, 2022")
  expect_equal(d$iso, "2022-03-05")
  expect_equal(d$raw, "March 5, 2022")
})

test_that(".siu_p_parse_date parses ISO", {
  set.seed(1)
  d <- morie:::.siu_p_parse_date("2024-06-30")
  expect_equal(d$iso, "2024-06-30")
})

test_that(".siu_p_parse_date returns NA fields for empty/NULL", {
  set.seed(1)
  d <- morie:::.siu_p_parse_date(NULL)
  expect_true(is.na(d$iso))
})

test_that(".siu_p_parse_date returns iso=NA for garbage", {
  set.seed(1)
  d <- morie:::.siu_p_parse_date("not a date")
  expect_true(is.na(d$iso))
})

test_that(".siu_p_find_case_number returns the SIU case pattern", {
  set.seed(1)
  expect_equal(
    morie:::.siu_p_find_case_number("blah 22-OCI-123 blah"),
    "22-OCI-123")
  expect_true(is.na(morie:::.siu_p_find_case_number("no case here")))
})

test_that(".siu_p_detect_language returns 'unknown' on neutral text", {
  set.seed(1)
  expect_equal(morie:::.siu_p_detect_language("nothing relevant"),
               "unknown")
})

test_that(".siu_p_detect_language returns 'en' on 2+ English markers", {
  set.seed(1)
  txt <- "The Investigation. Civilian Witnesses. Subject Officers."
  expect_equal(morie:::.siu_p_detect_language(txt), "en")
})

test_that(".siu_p_detect_police_service prefers full names", {
  set.seed(1)
  expect_equal(
    morie:::.siu_p_detect_police_service(
      "The Toronto Police Service responded."),
    "Toronto Police Service")
})

test_that(".siu_p_detect_police_service falls back to abbreviations", {
  set.seed(1)
  out <- morie:::.siu_p_detect_police_service(
    "Officers from the OPP attended.")
  expect_equal(out, "Ontario Provincial Police")
})

test_that(".siu_p_detect_police_service returns NA on no match", {
  set.seed(1)
  expect_true(
    is.na(morie:::.siu_p_detect_police_service("nothing relevant here")))
})

test_that(".siu_p_scan_mh_race finds keywords", {
  set.seed(1)
  hits <- morie:::.siu_p_scan_mh_race(
    "The man had mental health concerns and was Indigenous.")
  expect_true(grepl("mental health", hits, fixed = TRUE))
  expect_true(grepl("Indigenous", hits, fixed = TRUE))
})

test_that(".siu_p_scan_mh_race returns empty on null narrative", {
  set.seed(1)
  expect_equal(morie:::.siu_p_scan_mh_race(NULL), "")
  expect_equal(morie:::.siu_p_scan_mh_race(NA), "")
  expect_equal(morie:::.siu_p_scan_mh_race(""), "")
})

test_that(".siu_p_extract_summary returns first paragraph >80 chars", {
  set.seed(1)
  txt <- paste0("Short.\
\
",
                paste(rep("Lorem ipsum dolor sit amet ", 10),
                      collapse = ""))
  out <- morie:::.siu_p_extract_summary(txt)
  expect_true(nzchar(out))
  expect_true(nchar(out) <= 1500L)
})

test_that(".siu_p_extract_summary returns NA when no long paragraph", {
  set.seed(1)
  expect_true(is.na(morie:::.siu_p_extract_summary("short.\
\
also short.")))
})

test_that(".siu_p_label_value pulls a value after a label", {
  set.seed(1)
  txt <- "Police Service: Toronto Police Service\
Other stuff"
  out <- morie:::.siu_p_label_value(txt, "Police Service")
  expect_true(!is.null(out) && grepl("Toronto", out))
})

test_that(".siu_p_label_value returns NULL on miss", {
  set.seed(1)
  expect_null(
    morie:::.siu_p_label_value("nothing here", "Police Service"))
})

test_that(".siu_p_label_int returns NA on missing label", {
  set.seed(1)
  expect_true(
    is.na(morie:::.siu_p_label_int("nothing here", "Number of Officers")))
})

test_that(".siu_p_label_int extracts integers", {
  set.seed(1)
  txt <- "Number of Officers: 5\
more"
  expect_equal(morie:::.siu_p_label_int(txt, "Number of Officers"), 5L)
})

test_that(".siu_p_find_news_release_link finds nrid links via regex fallback", {
  set.seed(1)
  h <- '<a href="news_template.php?nrid=999">news</a>'
  out <- morie:::.siu_p_find_news_release_link(h, NULL)
  expect_true(grepl("nrid=999", out))
})

test_that(".siu_p_find_news_release_link returns NA on miss", {
  set.seed(1)
  expect_true(
    is.na(morie:::.siu_p_find_news_release_link("<a>no link</a>", NULL)))
})