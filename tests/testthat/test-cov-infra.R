# Coverage for local data-infrastructure helpers (datasets_vic.R,
# datasets_vpd_help.R, fetch_native.R, digest_native.R, cli_main.R,
# cli_unified_catalog.R, covLst.R). The Socrata/CKAN/ARSAU fetchers only
# talk to remote APIs and are not exercised here.

test_that("morie_datasets_vic_cache_dir resolves and creates the cache", {
  d <- file.path(tempdir(), "vic-cache-test")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_equal(morie_datasets_vic_cache_dir(d), d)
  expect_true(dir.exists(d))
  e <- file.path(tempdir(), "vic-env-test")
  on.exit(unlink(e, recursive = TRUE), add = TRUE)
  withr::with_envvar(c(MORIE_VIC_CACHE = e), expect_equal(morie_datasets_vic_cache_dir(), e))
  expect_true(dir.exists(e))
})

test_that("morie_vpd_download_instructions prints and optionally writes the guide", {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f), add = TRUE)
  out <- capture.output(res <- suppressMessages(morie_vpd_download_instructions(to = f)))
  expect_identical(readLines(f), res)
  expect_identical(out[seq_along(res)], res)
  expect_true(any(grepl("geodash.vpd.ca", res, fixed = TRUE)))
  expect_message(capture.output(morie_vpd_download_instructions(to = f)), "Also written")
})

test_that("morie_fetch_csv and morie_xml_sax parse local text", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("a,b,c", "1,\"x,y\",3", "4,z,6"), f)
  d <- morie_fetch_csv(f)
  expect_equal(d, data.frame(a = c(1L, 4L), b = c("x,y", "z"), c = c(3L, 6L)))
  g <- tempfile(fileext = ".tsv")
  on.exit(unlink(g), add = TRUE)
  writeLines(c("p\tq", "1\t2"), g)
  expect_equal(morie_fetch_csv(g, sep = "\t")$q, 2L)
  starts <- list()
  texts <- character(0)
  ends <- character(0)
  xml <- paste0("<?xml version=\"1.0\"?><!-- c --><root id=\"r1\"><item k=\"a &amp; b\">",
                "one</item><empty/><![CDATA[<raw>]]></root>")
  n <- morie_xml_sax(xml,
    on_start = function(nm, at) starts[[length(starts) + 1]] <<- list(nm, at),
    on_text = function(tx) texts <<- c(texts, tx),
    on_end = function(nm) ends <<- c(ends, nm))
  expect_equal(n, 3L)
  expect_equal(vapply(starts, `[[`, "", 1), c("root", "item", "empty"))
  expect_equal(starts[[2]][[2]], list(k = "a & b"))
  expect_equal(texts, c("one", "<raw>"))
  expect_equal(ends, c("item", "empty", "root"))
  expect_equal(morie_xml_sax("plain text"), 0L)
})

test_that("morie_make_raw converts to raw bytes", {
  expect_identical(morie_make_raw(as.raw(c(1, 255))), as.raw(c(1, 255)))
  expect_identical(morie_make_raw("abc"), charToRaw("abc"))
  expect_identical(morie_make_raw(c(65, 66)), as.raw(c(65, 66)))
  d <- structure("0a0bff10", class = "digest")
  expect_identical(morie_make_raw(d), as.raw(c(0x0a, 0x0b, 0xff, 0x10)))
})

test_that("cli_main dispatches analysis subjects and reports as JSON", {
  out <- capture.output(r <- cli_main("tps"))
  expect_identical(r$status, "not_available")
  expect_identical(.morie_from_json(paste(out, collapse = ""))$status, "not_available")
  capture.output(u <- cli_main("nothing"))
  expect_identical(u$status, "error")
  expect_match(u$message, "unknown analysis subject")
  capture.output(cp <- cli_main("cpd", "{\"unused_flag\": 1}"))
  expect_setequal(names(cp), c("crime_by_type", "arrests_by_area", "temporal", "arrest_race_disparity"))
  expect_error(cli_main(""), "nzchar")
})

test_that("morie_cli_dump_catalog writes the browse table in CLI schema", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  expect_identical(morie_cli_dump_catalog(f), f)
  u <- utils::read.csv(f, stringsAsFactors = FALSE)
  b <- morie_datasets_browse()
  expect_equal(u$dataset_key, b$dataset_key)
  expect_equal(u$portal, b$source)
  expect_equal(names(u), c("dataset_key", "portal", "title", "description", "url",
                           "license", "formats"))
})

test_that("covLst is catalogue coverage of recommendation lists", {
  r <- covLst(list(c(1, 2, 3), c(2, 9), c(4, 4)), catalog = c(1, 2, 3, 4, 5, 6))
  expect_equal(r$coverage, 4 / 6, tolerance = 1e-12)
  expect_equal(r$meanlen, 7 / 3, tolerance = 1e-12)
  expect_equal(r$covered, 4L)
  expect_same_function(morie_catalog_coverage, covLst)
  expect_true(is.na(covLst(list(), numeric(0))$coverage))
})
