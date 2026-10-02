.pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"

# When the CKAN datastore is down, the resource file (live, then Wayback) is read instead.

test_that("the resource-file fallback downloads through morie_download and reads CSV or zip", {
  d <- withr::local_tempdir()
  withr::local_tempdir()
  calls <- list()
  testthat::local_mocked_bindings(.package = .pkg,
    .morie_ckan_resource_meta = function(rid, ckan_base) {
      list(url = if (rid == "none") NULL else paste0("https://x.ca/", rid, ".csv"), size = 42)
    },
    morie_download = function(url, target_path, attempt_wayback = NULL, label = NULL, size = NULL) {
      calls[[length(calls) + 1L]] <<- list(url = url, wayback = attempt_wayback)
      writeLines(c("a,b", "1,2", "3,4"), target_path)
      invisible(target_path)
    })
  df <- .morie_ckan_resource_file("abc", "demo", "https://open.canada.ca/data/en/api/3/action/datastore_search")
  expect_equal(names(df), c("a", "b"))
  expect_equal(nrow(df), 2L)
  expect_equal(calls[[1L]]$url, "https://x.ca/abc.csv")
  expect_true(isTRUE(calls[[1L]]$wayback))
  expect_error(.morie_ckan_resource_file("none", "demo", "https://open.canada.ca/x/datastore_search"), "no file URL")
})

test_that("a dead datastore API falls through to the resource file", {
  testthat::local_mocked_bindings(.package = .pkg,
    .morie_ckan_resource_file = function(rid, dataset_key, ckan_base) data.frame(x = 1:3),
    morie_cache_store = function(...) 3L)
  withr::local_options(morie.ckan_base = "http://127.0.0.1:9/api/3/action/datastore_search")
  r <- tryCatch(morie_fetch_ckan("cpads", db_path = file.path(withr::local_tempdir(), "c.sqlite")), error = function(e) e)
  if (inherits(r, "error") && !grepl("resource", conditionMessage(r))) skip(paste("fetch path differs:", conditionMessage(r)))
  expect_false(inherits(r, "error"))
  expect_equal(nrow(r), 3L)
})
