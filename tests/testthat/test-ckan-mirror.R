# The CKAN survey route when open.canada.ca is down (2026-10-16): the pinned file URL, then the
# data.rmorie.com mirror, checked against the SHA-256 its index records.

csv_text <- "SEQID,age\n1,20\n2,31\n"

mirror_files <- function(dir, sha = NULL, rid = "rid-1", name = "ocp21") {
  csv <- file.path(dir, "src.csv")
  writeLines(c("SEQID,age", "1,20", "2,31"), csv)
  gz <- file.path(dir, paste0(name, ".csv.gz"))
  con <- gzfile(gz, "wb"); writeLines(c("SEQID,age", "1,20", "2,31"), con); close(con)
  idx <- list(files = stats::setNames(list(list(
    file = paste0(name, ".csv.gz"), original_name = "cpads.csv",
    sha256 = sha %||% rmoriebricklayer::sha256_file(csv), gz_bytes = file.size(gz), ckan_resource_id = rid)), name))
  idx_path <- file.path(dir, "index.json")
  writeLines(jsonlite::toJSON(idx, auto_unbox = TRUE), idx_path)
  list(index = idx_path, gz = gz)
}

fake_get <- function(files) {
  function(path, dest, ...) {
    src <- if (grepl("index\\.json$", path)) files$index else files$gz
    file.copy(src, dest, overwrite = TRUE)
    invisible(dest)
  }
}

test_that("the mirror is read and its SHA-256 checked", {
  skip_if_not_installed("jsonlite")
  d <- withr::local_tempdir()
  f <- mirror_files(d)
  withr::local_envvar(TMPDIR = d)
  local_mocked_bindings(.morie_data_get = fake_get(f))
  csv_out <- file.path(tempdir(), "ocp21.csv")
  unlink(csv_out)
  expect_message(out <- .morie_ckan_mirror("rid-1", "cpads"), "data.rmorie.com mirror")
  expect_equal(nrow(out), 2L)
  expect_equal(out$age, c(20L, 31L))
  # a second call uses the checked file already there
  local_mocked_bindings(.morie_data_get = function(path, dest, ...) {
    if (!grepl("index", path)) stop("should not download again")
    file.copy(f$index, dest, overwrite = TRUE)
  })
  expect_equal(nrow(suppressMessages(.morie_ckan_mirror("rid-1", "cpads"))), 2L)
  unlink(csv_out)
})

test_that("a mirror copy that does not match its SHA-256 is refused and deleted", {
  skip_if_not_installed("jsonlite")
  d <- withr::local_tempdir()
  f <- mirror_files(d, sha = strrep("0", 64), name = "bad")
  local_mocked_bindings(.morie_data_get = fake_get(f))
  expect_error(.morie_ckan_mirror("rid-1", "cpads"), "does not match the SHA-256")
  expect_false(file.exists(file.path(tempdir(), "bad.csv")))
})

test_that("a dataset the mirror does not hold says so", {
  skip_if_not_installed("jsonlite")
  d <- withr::local_tempdir()
  f <- mirror_files(d, rid = "other")
  local_mocked_bindings(.morie_data_get = fake_get(f))
  expect_error(.morie_ckan_mirror("rid-404", "csads"), "holds no copy of csads")
})

test_that("the route falls through live -> mirror and reports both when both fail", {
  local_mocked_bindings(
    .morie_ckan_resource_file_live = function(...) stop("open.canada.ca could not be reached (504)"),
    .morie_ckan_mirror = function(...) data.frame(SEQID = 1:3))
  expect_message(out <- .morie_ckan_resource_file("rid-1", "cpads", "base"), "trying the data.rmorie.com mirror")
  expect_equal(nrow(out), 3L)

  local_mocked_bindings(
    .morie_ckan_resource_file_live = function(...) stop("open.canada.ca could not be reached (504)"),
    .morie_ckan_mirror = function(...) stop("the curated tables need your MORIE data key"))
  expect_error(suppressMessages(.morie_ckan_resource_file("rid-1", "cpads", "base")),
               "could not be reached \\(504\\); the data.rmorie.com mirror: the curated tables need")

  local_mocked_bindings(.morie_ckan_resource_file_live = function(...) data.frame(a = 1))
  expect_equal(.morie_ckan_resource_file("rid-1", "cpads", "base"), data.frame(a = 1))
})

test_that("with the portal's metadata down, the pinned file URL is used", {
  rid <- names(.MORIE_CKAN_FILES)[1]
  seen <- NULL
  local_mocked_bindings(
    .morie_ckan_resource_meta = function(...) list(url = NULL, size = NULL, host = "open.canada.ca", error = "timeout"),
    morie_download = function(url, target_path, ...) { seen <<- url; writeLines(c("SEQID", "7"), target_path); invisible(target_path) })
  expect_message(out <- .morie_ckan_resource_file_live(rid, "cpads", "https://open.canada.ca/x/datastore_search"),
                 "using the pinned file URL")
  expect_equal(seen, unname(.MORIE_CKAN_FILES[rid]))
  expect_equal(out$SEQID, 7L)
  # an unpinned resource with the portal down still fails clearly
  expect_error(.morie_ckan_resource_file_live("not-pinned", "x", "https://open.canada.ca/x/datastore_search"),
               "could not be reached \\(timeout\\); nothing cached for x")
})
