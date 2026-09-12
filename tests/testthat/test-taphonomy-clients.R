# Taphonomy data-ingestion clients: the parts that do not need the network.
#
# The zip reader is driven against a zip this test builds itself, so the
# anchor is the data.frame that went in. The MorphoSource helpers are pure
# request builders; the anchors are the endpoint contract documented in the
# module header (q + search_field=all_fields, f.<facet>, per_page/page) and
# the rule that a key is read from the caller's environment and never put
# in the URL.

test_that("the zip reader streams the CSV member back unchanged", {
  skip_on_cran()
  dir <- file.path(tempdir(), "tap_zip_test")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  df <- data.frame(lab_id = c("A-1", "A-2", "A-3"),
                   ca_pct = c(1.5, 2.25, 3),
                   `fe ppm` = c(10L, 20L, 30L),
                   check.names = FALSE)
  csv <- file.path(dir, "ngdbsoil.csv")
  utils::write.csv(df, csv, row.names = FALSE)
  zip_path <- file.path(dir, "ngdbsoil-csv.zip")
  old <- getwd()
  setwd(dir)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  z <- utils::zip(zip_path, "ngdbsoil.csv", flags = "-q")
  skip_if(z != 0, "no zip binary on this host")

  got <- rmorie:::.morie_read_usgs_soil_zip(zip_path)
  expect_equal(got, df)
  # the column name with a space survives, because check.names is off
  expect_true("fe ppm" %in% names(got))
  # a character column is not silently turned into a factor
  expect_type(got$lab_id, "character")
  # nrows takes a head slice rather than the whole 482 MB member
  expect_equal(rmorie:::.morie_read_usgs_soil_zip(zip_path, nrows = 2), df[1:2, ])
  expect_equal(nrow(rmorie:::.morie_read_usgs_soil_zip(zip_path, nrows = 1)), 1L)
  # NULL means every row
  expect_equal(nrow(rmorie:::.morie_read_usgs_soil_zip(zip_path, nrows = NULL)),
               3L)
  # a zip with no CSV member is refused by name
  other <- file.path(dir, "notes.txt")
  writeLines("no data here", other)
  nocsv <- file.path(dir, "nocsv.zip")
  utils::zip(nocsv, "notes.txt", flags = "-q")
  expect_error(rmorie:::.morie_read_usgs_soil_zip(nocsv), "no CSV member")
})

test_that("the API base honours the environment override", {
  old <- Sys.getenv("MORPHOSOURCE_API_URL", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("MORPHOSOURCE_API_URL")
    else Sys.setenv(MORPHOSOURCE_API_URL = old)
  }, add = TRUE)
  Sys.unsetenv("MORPHOSOURCE_API_URL")
  expect_equal(rmorie:::.morie_morphosource_api(),
               "https://www.morphosource.org/api")
  Sys.setenv(MORPHOSOURCE_API_URL = "https://staging.example.org/api")
  expect_equal(rmorie:::.morie_morphosource_api(),
               "https://staging.example.org/api")
  # an empty override is not an override
  Sys.setenv(MORPHOSOURCE_API_URL = "")
  expect_equal(rmorie:::.morie_morphosource_api(),
               "https://www.morphosource.org/api")
})

test_that("the key comes from the argument or the environment, never the URL", {
  old <- Sys.getenv("MORPHOSOURCE_API_KEY", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("MORPHOSOURCE_API_KEY")
    else Sys.setenv(MORPHOSOURCE_API_KEY = old)
  }, add = TRUE)
  Sys.unsetenv("MORPHOSOURCE_API_KEY")
  # an explicit argument wins
  expect_equal(rmorie:::.morie_morphosource_key("tok-arg"), "tok-arg")
  # otherwise the caller's environment is read
  Sys.setenv(MORPHOSOURCE_API_KEY = "tok-env")
  expect_equal(rmorie:::.morie_morphosource_key(), "tok-env")
  expect_equal(rmorie:::.morie_morphosource_key("tok-arg"), "tok-arg")
  # an empty argument falls through to the environment rather than being used
  expect_equal(rmorie:::.morie_morphosource_key(""), "tok-env")
  # with no key at all, public search gets NULL and a download is refused
  Sys.unsetenv("MORPHOSOURCE_API_KEY")
  expect_null(rmorie:::.morie_morphosource_key(required = FALSE))
  expect_null(rmorie:::.morie_morphosource_key("", required = FALSE))
  expect_error(rmorie:::.morie_morphosource_key(), "API key required")
  # the refusal says where to put the key and does not echo one
  expect_error(rmorie:::.morie_morphosource_key(),
               "MORPHOSOURCE_API_KEY", fixed = TRUE)
})

test_that("search params follow the documented query contract", {
  f <- rmorie:::.morie_morphosource_search_params
  # pagination is always sent, as integers
  base <- f()
  expect_equal(base, list(per_page = 10L, page = 1L))
  expect_type(base$per_page, "integer")
  expect_equal(f(per_page = 25, page = 3)[c("per_page", "page")],
               list(per_page = 25L, page = 3L))
  # free text goes in q and pins search_field to all_fields
  q <- f("Homo sapiens cranium")
  expect_equal(q$q, "Homo sapiens cranium")
  expect_equal(q$search_field, "all_fields")
  # an empty query is not a query, so neither key appears
  expect_false(any(c("q", "search_field") %in% names(f(""))))
  expect_false(any(c("q", "search_field") %in% names(base)))
  # each facet is sent under its own f.<facet> key
  full <- f("cranium", media_type = "CT", taxonomy_gbif = "1234",
            visibility = "open", media_tag = "mummified")
  expect_equal(full[["f.media_type"]], "CT")
  expect_equal(full[["f.taxonomy_gbif"]], "1234")
  expect_equal(full[["f.publication_status"]], "open")
  expect_equal(full[["f.tag"]], "mummified")
  # an unset facet is omitted rather than sent empty
  expect_false("f.media_type" %in% names(f("cranium")))
  expect_equal(names(f(media_type = "CT")),
               c("f.media_type", "per_page", "page"))
  # no credential ever reaches the query string
  expect_false(any(grepl("key|token|auth", names(full), ignore.case = TRUE)))
})
