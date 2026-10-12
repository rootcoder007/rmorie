# morie_fetch_siu(): the corpus, check-guard and live-sweep routes, with the HTTP pool and the
# two C++ page parsers replaced by recorders so the interleaved batching, the news join, the
# language de-duplication and the override overlay are checked row by row.

.sf_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.sf <- function(name) get(name, envir = environment(morie_fetch_siu))

.sf_row <- function(drid, case = "", lang = "en", nrid = "") {
  fields <- .sf(".siu_field_list")()
  v <- stats::setNames(rep("", length(fields)), fields)
  v["case_number"] <- case
  v["drid"] <- as.character(drid)
  v["nrid"] <- nrid
  v["_language"] <- lang
  v["source_url_report"] <- paste0("https://www.siu.on.ca/en/directors_report_details.php?drid=", drid)
  v
}

# the synthetic site: three report pages (one case in English and French, one more case),
# every other drid an empty page
.sf_site <- function(drid) {
  switch(as.character(drid),
    "1" = .sf_row(1L, "24-TST-001", "en", "11"),
    "2" = .sf_row(2L, "24-TST-001", "fr", "11"),
    "3" = .sf_row(3L, "24-TST-002", "en", "12"),
    .sf_row(drid))
}

.sf_mock_live <- function(env, calls) {
  testthat::local_mocked_bindings(
    .siu_http_get_many = function(urls, concurrency = 4L, timeout_s = 60L, rate_rps = 4.0, max_retries = 3L) {
      calls$batches[[length(calls$batches) + 1L]] <- list(urls = urls, concurrency = concurrency, rate = rate_rps)
      paste0("<html>", urls, "</html>")
    },
    .siu_parse_report = function(html, drid, url) .sf_site(drid),
    .siu_parse_news = function(html, nrid, url) {
      c(nrid = as.character(nrid), source_url_news = url, news_release_title = paste("Release", nrid),
        news_release_date_iso = "2024-03-01", news_release_date_raw = "March 1, 2024",
        news_release_summary = paste("Summary", nrid))
    },
    .package = .sf_pkg, .env = env)
}

test_that("a cached SIU.csv is returned untouched unless overwrite is asked for", {
  dir <- withr::local_tempdir()
  writeLines("case_number\nX", file.path(dir, "SIU.csv"))
  expect_equal(morie_fetch_siu(cache_dir = dir), file.path(dir, "SIU.csv"))
  expect_equal(readLines(file.path(dir, "SIU.csv")), c("case_number", "X"))
})

test_that("under R CMD check without the corpus package the fetch writes a placeholder, never a sweep", {
  dir <- withr::local_tempdir()
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = "rmorie")
  withr::local_options(morie.siu.allow_fetch = NULL)
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) package != "rmoriedata", .package = "base")
  testthat::local_mocked_bindings(.siu_http_get_many = function(...) stop("no network allowed"), .package = .sf_pkg)
  out <- morie_fetch_siu(cache_dir = dir, progress = FALSE)
  expect_equal(readLines(out), "case_number")
  expect_equal(nrow(utils::read.csv(out)), 0L)
})

test_that("the panel-reviewed corpus is materialised when rmoriedata is installed", {
  skip_if_not_installed("rmoriedata")
  dir <- withr::local_tempdir()
  withr::local_options(morie.siu.allow_fetch = NULL)
  corpus <- data.frame(case_number = c("A", "B", "C"), police_service = c("TPS", "OPP", "TPS"),
                       `_language` = c("en", "fr", "unknown"), check.names = FALSE, stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(load_siu_reports = function(...) corpus, .package = "rmoriedata")
  testthat::local_mocked_bindings(.siu_http_get_many = function(...) stop("no network allowed"), .package = .sf_pkg)
  expect_message(out <- morie_fetch_siu(cache_dir = dir, lang = "fr"), "wrote the panel-reviewed corpus from rmoriedata \\(2 rows\\)")
  got <- utils::read.csv(out, stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(got$case_number, c("B", "C"))
  expect_silent(morie_fetch_siu(cache_dir = dir, overwrite = TRUE, progress = FALSE))
  expect_equal(nrow(utils::read.csv(out)), 3L)
  # an empty corpus falls through to the check guard
  testthat::local_mocked_bindings(load_siu_reports = function(...) stop("no corpus"), .package = "rmoriedata")
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = "rmorie")
  out2 <- morie_fetch_siu(cache_dir = withr::local_tempdir(), progress = FALSE)
  expect_equal(readLines(out2), "case_number")
})

test_that("a live sweep interleaves news pages with the next report batch and joins them by nrid", {
  dir <- withr::local_tempdir()
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = NA)
  withr::local_options(morie.siu.allow_fetch = TRUE)
  calls <- new.env()
  calls$batches <- list()
  .sf_mock_live(environment(), calls)
  testthat::local_mocked_bindings(
    .siu_load_manifest = function() data.frame(drid = 1:3, `_language` = c("en", "fr", "en"), check.names = FALSE),
    .siu_discover_max_drid = function(...) 2L,
    .package = .sf_pkg)
  msgs <- testthat::capture_messages(out <- morie_fetch_siu(cache_dir = dir, concurrency = 2L, rate_rps = 1.5))
  # the manifest floor plus 300 probes above it: 303 pages in two batches of 250 and 53
  expect_match(msgs, "SIU: fetching 303 report pages", all = FALSE)
  expect_match(msgs, "SIU batch 1/2: 250 reports \\+ 0 news", all = FALSE)
  expect_match(msgs, "SIU batch 2/2: 53 reports \\+ 2 news", all = FALSE)
  expect_match(msgs, "SIU: wrote 2 rows \\(2 with a case number\\)", all = FALSE)
  expect_length(calls$batches, 2)
  expect_equal(calls$batches[[1]]$concurrency, 2L)
  expect_equal(calls$batches[[1]]$rate, 1.5)
  expect_length(calls$batches[[1]]$urls, 250)
  expect_equal(utils::tail(calls$batches[[2]]$urls, 2),
               paste0("https://www.siu.on.ca/en/news_template.php?nrid=", c("11", "12")))
  got <- utils::read.csv(out, stringsAsFactors = FALSE, check.names = FALSE, na.strings = character())
  # one row per case: the English copy wins, with its drid and the joined release
  expect_equal(got$case_number, c("24-TST-001", "24-TST-002"))
  expect_equal(got$drid, c(1L, 3L))
  expect_equal(got[["_language"]], c("en", "en"))
  expect_equal(got$news_release_title, c("Release 11", "Release 12"))
  expect_equal(got$news_release_date_iso, rep("2024-03-01", 2))
  expect_equal(got$source_url_news, paste0("https://www.siu.on.ca/en/news_template.php?nrid=", c("11", "12")))
  # English only: the French drid is left out of the manifest floor
  calls$batches <- list()
  testthat::capture_messages(morie_fetch_siu(cache_dir = dir, overwrite = TRUE, lang = "en"))
  all_urls <- unlist(lapply(calls$batches, `[[`, "urls"))
  expect_false(any(grepl("drid=2$", all_urls)))
  expect_true(any(grepl("drid=1$", all_urls)) && any(grepl("drid=303$", all_urls)))
})

test_that("a sweep with an explicit ceiling fetches the trailing news pages last and caches the HTML", {
  dir <- withr::local_tempdir()
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = NA)
  withr::local_options(morie.siu.allow_fetch = TRUE)
  calls <- new.env()
  calls$batches <- list()
  .sf_mock_live(environment(), calls)
  testthat::local_mocked_bindings(.siu_load_manifest = function() stop("not consulted"), .package = .sf_pkg)
  # a user correction recorded in this cache directory is overlaid on the parsed rows
  utils::write.csv(data.frame(case_number = "24-TST-002", field = "police_service",
                              verified_value = "Toronto Police Service", stringsAsFactors = FALSE),
                   file.path(dir, "canonical_overrides.csv"), row.names = FALSE)
  msgs <- testthat::capture_messages(
    out <- morie_fetch_siu(cache_dir = dir, max_drid = 3L, use_manifest = FALSE, cache_html = TRUE))
  expect_match(msgs, "SIU: fetching 3 report pages", all = FALSE)
  expect_match(msgs, "SIU final: 2 remaining news pages", all = FALSE)
  expect_match(msgs, "SIU: applied 1 canonical-override correction", all = FALSE)
  expect_length(calls$batches, 2)
  expect_equal(calls$batches[[1]]$urls, paste0("https://www.siu.on.ca/en/directors_report_details.php?drid=", 1:3))
  expect_equal(calls$batches[[2]]$urls, paste0("https://www.siu.on.ca/en/news_template.php?nrid=", c("11", "12")))
  got <- utils::read.csv(out, stringsAsFactors = FALSE, check.names = FALSE, na.strings = character())
  expect_equal(got$case_number, c("24-TST-001", "24-TST-002"))
  expect_equal(got$police_service, c("", "Toronto Police Service"))
  expect_equal(got$news_release_summary, c("Summary 11", "Summary 12"))
  html_dir <- file.path(dir, "html")
  expect_setequal(list.files(html_dir), c("drid_1.html.gz", "drid_2.html.gz", "drid_3.html.gz",
                                          "nrid_11.html.gz", "nrid_12.html.gz"))
  expect_match(.sf(".siu_read_html_cache")(html_dir, "drid_2.html.gz"), "drid=2</html>$")
  # the same run, quietly, without the HTML cache: nothing is written under html/
  dir2 <- withr::local_tempdir()
  expect_silent(morie_fetch_siu(cache_dir = dir2, max_drid = 3L, use_manifest = FALSE, progress = FALSE))
  expect_false(dir.exists(file.path(dir2, "html")))
})
