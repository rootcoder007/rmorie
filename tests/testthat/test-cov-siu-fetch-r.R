# R/siu_fetch.R, the pure-R scraper: the polite GET with retries, the index link extractor on
# both passes, the case-page parser's labelled and narrative routes, the cases driver on a
# synthetic index, the corpus refresh and the subject-official resolver. No network.

.sq_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.sq <- function(name) get(name, envir = environment(morie_siu_fetch_cases))

test_that("the polite GET retries on a 5xx or a dropped connection and reports the final status", {
  testthat::local_mocked_bindings(Sys.sleep = function(time) invisible(NULL), .package = "base")
  calls <- 0L
  replies <- list()
  testthat::local_mocked_bindings(
    .morie_http_get_with_status = function(url, timeout_s = 60L, headers = character(), user_agent = "", ...) {
      calls <<- calls + 1L
      replies[[min(calls, length(replies))]]
    },
    .package = .sq_pkg)
  get <- .sq(".siu_fetch_http_get")
  replies <- list(list(status_code = 503L, body = ""), list(status_code = 200L, body = c("<p>", "ok</p>")))
  expect_equal(get("https://h/x"), "<p>ok</p>")
  expect_equal(calls, 2L)
  calls <- 0L
  replies <- list(list(status_code = 0L, body = ""))
  expect_error(get("https://h/x"), "https://h/x answered HTTP 0")
  expect_equal(calls, 3L)
  calls <- 0L
  replies <- list(list(status_code = 404L, body = "gone"))
  expect_error(get("https://h/x"), "answered HTTP 404")
  expect_equal(calls, 1L)
  expect_equal(morie_siu_index_url(), "https://www.siu.on.ca/en/directors_reports.php")
  dir <- withr::local_tempdir()
  expect_equal(morie_siu_cache_path(file.path(dir, "s")), file.path(dir, "s", "SIU.csv"))
  expect_true(dir.exists(file.path(dir, "s")))
})

test_that("index links come out of the markup by tag or by regex, with absolute URLs and ISO dates", {
  base <- "https://www.siu.on.ca/en/directors_reports.php"
  html <- paste0(
    "<table><tr class=\"dr-item\" id=\"5001\"><td><nobr>26-OCI-168</nobr></td>",
    "<td><a href=\"directors_report_details.php?drid=5001\">Read Full Text</a></td></tr>",
    "<tr class=\"dr-item\" id=\"5002\"><td>no case</td>",
    "<td><a href=\"/en/directors_report_details.php?drid=5002\">Read</a></td></tr>",
    "<tr class=\"dr-item\" id=\"5003\"><td>25-TCI-001</td><td>no link</td></tr></table>")
  ex <- .sq(".siu_fetch_extract_links")
  expected <- rbind(c(case_number = "26-OCI-168", url = "https://www.siu.on.ca/en/directors_report_details.php?drid=5001"),
                    c(case_number = "drid-5002", url = "https://www.siu.on.ca/en/directors_report_details.php?drid=5002"))
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) !package %in% c("xml2", "rvest"),
                                  .package = "base")
  got <- ex(html, base)
  expect_equal(unname(got), unname(expected))
  expect_equal(colnames(got), c("case_number", "url"))
  none <- ex("<p>nothing here</p>", base)
  expect_equal(dim(none), c(0L, 2L))
  if (requireNamespace("xml2", quietly = TRUE) && requireNamespace("rvest", quietly = TRUE)) {
    testthat::local_mocked_bindings(requireNamespace = function(package, ...) TRUE, .package = "base")
    expect_equal(unname(ex(html, base)), unname(expected))
    expect_equal(dim(ex("<p>nothing here</p>", base)), c(0L, 2L))
  }
  ru <- .sq(".siu_fetch_resolve_url")
  expect_equal(ru(c("https://x/y", "/en/a.php", "b.php"), base),
               c("https://x/y", "https://www.siu.on.ca/en/a.php", "https://www.siu.on.ca/en/b.php"))
  iso <- .sq(".siu_fetch_to_iso")
  expect_equal(iso("June 12, 2026"), "2026-06-12")
  expect_equal(iso(" March 1, 2024 "), "2024-03-01")
  expect_equal(iso(""), "")
  expect_equal(iso("yesterday"), "")
})

test_that("the case-page parser takes the compiled fields first, then labels, then the prose", {
  parse <- .sq(".siu_fetch_parse_case_page")
  u <- "https://www.siu.on.ca/en/directors_report_details.php?drid=1"
  empty <- parse("", "26-OCI-168", u)
  expect_equal(empty$case_number, "26-OCI-168")
  expect_equal(empty$police_service, "")
  expect_equal(empty$source_url, u)
  # the compiled parser answers: its dates and service win
  testthat::local_mocked_bindings(
    morie_siu_parse_report = function(html, ...) c(police_service = "Toronto Police Service", date_of_incident_iso = "2026-01-02",
                                                  date_siu_notified_iso = "", date_of_director_decision_iso = NA),
    .package = .sq_pkg)
  r <- parse("<p>Notification: February 3, 2026. No reasonable grounds.</p>", "26-OCI-168", u)
  expect_equal(r$police_service, "Toronto Police Service")
  expect_equal(r$incident_iso, "2026-01-02")
  expect_equal(r$notification_iso, "2026-02-03")
  expect_equal(r$decision_iso, "")
  expect_equal(r$director_decision_text, "No reasonable grounds")
  # no compiled parser: labelled fields
  testthat::local_mocked_bindings(morie_siu_parse_report = function(html, ...) stop("no parser"), .package = .sq_pkg)
  lab <- paste0("<p>Incident: January 5, 2026</p><p>Notification - January 6, 2026</p>",
                "<p>Director's Decision: March 9, 2026</p><p>Police Service: Ottawa Police Service</p>",
                "<p>Analysis and Director's Decision</p><p>There were reasonable grounds to believe.</p>")
  r2 <- parse(lab, "26-OCI-169", u)
  expect_equal(r2$incident_iso, "2026-01-05")
  expect_equal(r2$notification_iso, "2026-01-06")
  expect_equal(r2$decision_iso, "2026-03-09")
  expect_equal(r2$police_service, "Ottawa Police Service")
  expect_equal(r2$director_decision_text, "reasonable grounds")
  # the 2026 layout: dates in the prose, the service in the modal <abbr>, the verdict after the last heading
  prose <- paste0(
    "<p>Analysis and Director's Decision mandate text says no reasonable grounds generally.</p>",
    "<p>On April 2, 2026, officers of the <abbr title=\"Waterloo Regional Police Service\">WRPS</abbr> ",
    "with the <abbr title=\"Ontario Provincial Police\">OPP</abbr> attended at 10:00 a.m. ",
    "The <abbr title=\"Waterloo Regional Police Service\">WRPS</abbr> contacted the SIU on April 3, 2026.</p>",
    "<p>Analysis and Director's Decision</p><p>The charge(s) was withdrawn later.</p>",
    "<p>Date: May 1, 2026 Electronically approved by A. Person Director</p>")
  r3 <- parse(prose, "26-OCI-170", u)
  expect_equal(r3$incident_iso, "2026-04-02")
  expect_equal(r3$notification_iso, "2026-04-02")
  expect_equal(r3$decision_iso, "2026-05-01")
  expect_equal(r3$police_service, "Waterloo Regional Police Service")
  expect_equal(r3$director_decision_text, "charge(s) was")
  alt <- "<p>Something happened. The SIU was notified of this on June 7, 2026 by the force.</p>"
  expect_equal(parse(alt, "x", u)$notification_iso, "2026-06-07")
  expect_equal(parse(alt, "x", u)$incident_iso, "2026-06-07")
})

test_that("the cases driver returns the cache, materialises the corpus, holds under check, and scrapes a synthetic index", {
  dir <- withr::local_tempdir()
  out <- file.path(dir, "SIU.csv")
  writeLines("case_number\nkept", out)
  expect_equal(morie_siu_fetch_cases(cache_dir = dir), out)
  expect_equal(readLines(out)[2], "kept")
  expect_error(morie_siu_fetch_cases(years = c(2020, NA), cache_dir = dir, overwrite = TRUE), "finite integer vector")
  withr::local_options(morie.siu.allow_fetch = NULL)
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) package != "rmoriedata", .package = "base")
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = "rmorie")
  # under check an existing cache is kept even with overwrite; without one a placeholder is written
  expect_message(p <- morie_siu_fetch_cases(years = 2099, cache_dir = dir, overwrite = TRUE),
                 "1 requested year\\(s\\) exceed the latest published \\(2023\\)")
  expect_equal(readLines(p), c("case_number", "kept"))
  unlink(out)
  expect_equal(readLines(morie_siu_fetch_cases(cache_dir = dir, progress = FALSE)), "case_number")
  expect_equal(morie_siu_fetch_cases(cache_dir = dir, overwrite = TRUE, progress = FALSE), out)
  # the corpus route, filtered to the requested years by the case-number prefix
  if (requireNamespace("rmoriedata", quietly = TRUE)) {
    testthat::local_mocked_bindings(requireNamespace = function(package, ...) TRUE, .package = "base")
    testthat::local_mocked_bindings(
      siuload = function(...) data.frame(case_number = c("22-OCI-001", "23-TCI-002", "21-OVI-003"),
                                                   stringsAsFactors = FALSE),
      .package = "rmoriedata")
    expect_message(morie_siu_fetch_cases(years = 2022:2023, cache_dir = dir, overwrite = TRUE),
                   "wrote the panel-reviewed corpus from rmoriedata \\(2 rows\\)")
    expect_equal(utils::read.csv(out)$case_number, c("22-OCI-001", "23-TCI-002"))
    testthat::local_mocked_bindings(requireNamespace = function(package, ...) package != "rmoriedata", .package = "base")
  }
  # the live scrape on a synthetic site: an index of two rows plus one more page, then the case pages
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = NA)
  withr::local_options(morie.siu.allow_fetch = TRUE)
  testthat::local_mocked_bindings(Sys.sleep = function(time) invisible(NULL), .package = "base")
  row <- function(cn, drid) sprintf("<tr class=\"dr-item\" id=\"%d\"><td>%s</td><td><a href=\"directors_report_details.php?drid=%d\">Read</a></td></tr>", drid, cn, drid)
  pages <- list(
    "https://www.siu.on.ca/en/directors_reports.php" = paste0("<input id=\"total_drs\" value=\"3\">", row("26-OCI-168", 1), row("25-TCI-002", 2)),
    "https://www.siu.on.ca/ssi/get_more_drs.php?lang=en&lastCount=2" = row("26-OVI-003", 3),
    "https://www.siu.on.ca/en/directors_report_details.php?drid=1" = "<p>Incident: January 5, 2026</p><p>Police Service: Toronto Police Service</p>",
    "https://www.siu.on.ca/en/directors_report_details.php?drid=3" = "<p>Incident: February 5, 2026</p>")
  fetched <- character()
  testthat::local_mocked_bindings(
    .siu_fetch_http_get = function(url, ...) {
      fetched <<- c(fetched, url)
      if (is.null(pages[[url]])) stop("no such page") else pages[[url]]
    },
    morie_siu_parse_report = function(html, ...) stop("no compiled parser"),
    .package = .sq_pkg)
  msgs <- testthat::capture_messages(p <- morie_siu_fetch_cases(cache_dir = dir, overwrite = TRUE))
  expect_match(msgs, "\\[siu\\] index page: lastCount=2 of 3", all = FALSE)
  expect_match(msgs, "\\[siu\\] wrote 3 cases", all = FALSE)
  got <- utils::read.csv(p, stringsAsFactors = FALSE, na.strings = character())
  expect_equal(got$case_number, c("26-OCI-168", "25-TCI-002", "26-OVI-003"))
  expect_equal(got$incident_iso, c("2026-01-05", "", "2026-02-05"))
  expect_equal(got$police_service, c("Toronto Police Service", "", ""))
  expect_equal(got$source_url, paste0("https://www.siu.on.ca/en/directors_report_details.php?drid=", 1:3))
  expect_length(fetched, 5)
  # the year filter keeps 2026 only; a filter nobody matches is an error
  p26 <- morie_siu_fetch_cases(years = 2026, cache_dir = dir, overwrite = TRUE, progress = FALSE)
  expect_equal(utils::read.csv(p26)$case_number, c("26-OCI-168", "26-OVI-003"))
  expect_error(morie_siu_fetch_cases(years = 1999, cache_dir = dir, overwrite = TRUE, progress = FALSE), "Scraped 0 SIU index entries")
  pages[["https://www.siu.on.ca/en/directors_reports.php"]] <- NULL
  expect_error(morie_siu_fetch_cases(cache_dir = dir, overwrite = TRUE, progress = FALSE), "Scraped 0 SIU index entries")
  df <- morie_siu_fetch_dataframe(cache_dir = dir)
  expect_equal(df$case_number, c("26-OCI-168", "26-OVI-003"))
  expect_true(is.character(df$incident_iso))
})

test_that("several reports parse into one frame, column by column", {
  expect_error(morie_siu_parse_report(NA_character_), "is.na")
  testthat::local_mocked_bindings(
    morie_siu_parse_report = function(html, engine = "auto") {
      if (html == "a") c(case_number = "A", police_service = "TPS") else c(case_number = "B", directors_name = "D")
    },
    .package = .sq_pkg)
  df <- morie_siu_parse_reports(list("a", "b"))
  expect_equal(names(df), c("case_number", "police_service", "directors_name"))
  expect_equal(df$police_service, c("TPS", NA))
  expect_equal(df$directors_name, c(NA, "D"))
})

test_that("the corpus refresh appends only genuine newer reports, through either fetch engine", {
  skip_if_not_installed("rmoriedata")
  corpus <- data.frame(case_number = "24-OCI-001", drid = "5000", source_url_report = "u", police_service = "TPS",
                       date_of_incident_iso = "2024-01-01", number_of_subject_officials = "2", X_language = "en",
                       panel_reviewed = TRUE, stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(siuload = function(...) corpus, .package = "rmoriedata")
  expect_equal(morie_siu_reports(), corpus)
  testthat::local_mocked_bindings(.siu_discover_max_drid = function(default = 6000L, ...) 5000L, .package = .sq_pkg)
  expect_message(morie_siu_reports(update = TRUE), "corpus is current \\(max drid 5000\\)")
  testthat::local_mocked_bindings(.siu_discover_max_drid = function(default = 6000L, ...) 5004L, .package = .sq_pkg)
  parsed <- function(html, ...) {
    switch(html,
      "page5001" = c(police_service = "Ottawa Police Service", date_of_incident_iso = "2025-02-02", number_of_subject_officers = "1",
                     "_language" = "en", unknown_field = "x"),
      "page5002" = c(police_service = "", date_of_incident_iso = ""),
      "page5003" = stop("unparsable"))
  }
  testthat::local_mocked_bindings(morie_siu_parse_report = parsed, .package = .sq_pkg)
  # bricklayer's engine writes the page to a file
  skip_if_not_installed("rmoriebricklayer")
  testthat::local_mocked_bindings(
    siufetch = function(id, dest, ...) {
      if (id == 5004L) stop("dead")
      writeLines(paste0("page", id), dest)
      TRUE
    },
    .package = "rmoriebricklayer")
  msgs <- testthat::capture_messages(out <- morie_siu_reports(update = TRUE, max_new = 10L))
  expect_match(msgs, "4 report id\\(s\\) newer than the reviewed corpus", all = FALSE)
  expect_match(msgs, "siu: added drid 5001", all = FALSE)
  expect_match(msgs, "1 new report\\(s\\) appended", all = FALSE)
  expect_equal(nrow(out), 2L)
  expect_equal(out$drid[2], "5001")
  expect_equal(out$police_service[2], "Ottawa Police Service")
  expect_equal(out$number_of_subject_officials[2], "1")
  expect_equal(out$X_language[2], "en")
  expect_false(out$panel_reviewed[2])
  expect_equal(out$source_url_report[2], "https://www.siu.on.ca/en/directors_report_details.php?drid=5001")
  # the ceiling on new ids, and the plain-R engine when bricklayer is absent
  expect_equal(nrow(morie_siu_reports(update = TRUE, max_new = 2L, quiet = TRUE)), 2L)
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) package != "rmoriebricklayer", .package = "base")
  testthat::local_mocked_bindings(
    morie_siu_fetch_report = function(drid, ...) if (drid == 5001L) "page5001" else stop("no"),
    .siu_fetch_http_get = function(url, ...) if (grepl("drid=5002", url)) "page5002" else stop("down"),
    .package = .sq_pkg)
  out2 <- morie_siu_reports(update = TRUE, quiet = TRUE)
  expect_equal(out2$drid, c("5000", "5001"))
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) package != "rmoriedata", .package = "base")
  expect_error(morie_siu_reports(), "needs the rmoriedata package")
})

test_that("the subject-official count comes from the corpus when it can, else from the rules", {
  skip_if_not_installed("rmoriedata")
  corpus <- data.frame(case_number = c("17-OVI-201", "18-OCI-009"), drid = c("5038", "5100"),
                       number_of_subject_officials = c("2", NA), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(siuload = function(...) corpus, .package = "rmoriedata")
  expect_equal(morie_siu_resolve_so("17-OVI-201"), list(count = 2L, reason = "panel-reviewed corpus (verified), case 17-OVI-201, drid 5038"))
  expect_error(morie_siu_resolve_so("18-OCI-009"), "no verified count in the reviewed corpus")
  expect_equal(morie_siu_resolve_so(drid = 5038), list(count = 2L, reason = "panel-reviewed corpus (verified)"))
  expect_error(morie_siu_resolve_so(drid = 5100), "supply `text`")
  text <- "The SIU investigated the conduct of two subject officials, SO #1 and SO #2, in this matter."
  r <- morie_siu_resolve_so(text, drid = 5100, engine = "native")
  expect_true(is.list(r) && all(c("count", "reason") %in% names(r)))
  testthat::local_mocked_bindings(siuload = function(...) stop("no corpus"), .package = "rmoriedata")
  expect_error(morie_siu_resolve_so("17-OVI-201"), "with no verified count|not installed")
})
