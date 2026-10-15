# R/hosted_data.R, the lines PR #77 added that no other test reached: the data service named by the
# signed services document, the restricted-table rule, the SHA-256 check against the manifest (with its
# one refetch) and the refusals of data.rmorie.com. No network: the downloader is a recorder that
# serves a synthetic manifest and table from memory.

.hd <- function(name) get(name, envir = environment(morie_hosted_manifest))

# A sandbox: no MORIE_DATA_URL, a test key, and the manifest cache in a temporary directory
.hd_sandbox <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(MORIE_DATA_URL = NA, MORIE_HOSTED_KEY = "sk-test", XDG_CONFIG_HOME = dir,
                      R_USER_CACHE_DIR = file.path(dir, "cache"), .local_envir = env)
  dir
}

.hd_manifest <- function(sha) {
  list(datasets = list(list(key = "db1/t1", sha256 = sha, bytes_gz = 10L)))
}

.hd_write_cache <- function(m) {
  p <- .hd(".morie_data_manifest_path")()
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, null = "null"), p)
  p
}

test_that("the data service comes from the signed services document, and an unavailable one is refused", {
  .hd_sandbox()
  url <- .hd(".morie_data_url")
  testthat::local_mocked_bindings(bricklayer_services = function(offline = TRUE) stop("too old"),
                                  .package = "rmoriebricklayer")
  expect_identical(url(), "https://data.rmorie.com")   # an unreadable document leaves the default
  testthat::local_mocked_bindings(
    bricklayer_services = function(offline = TRUE) list(data = list(mode = "key", base_url = "https://d.example.org")),
    .package = "rmoriebricklayer")
  expect_identical(url(), "https://d.example.org")
  testthat::local_mocked_bindings(
    bricklayer_services = function(offline = TRUE) list(data = list(mode = "off", request_access = "https://x.example/a")),
    .package = "rmoriebricklayer")
  expect_error(url(), "the curated-data service is not available right now (see https://x.example/a)", fixed = TRUE)
  testthat::local_mocked_bindings(
    bricklayer_services = function(offline = TRUE) list(data = list(mode = "key", base_url = "")),
    .package = "rmoriebricklayer")
  expect_error(url(), "not available right now (see https://rmorie.com/access)", fixed = TRUE)
  withr::local_envvar(MORIE_DATA_URL = " https://local.test/// ")
  expect_identical(url(), "https://local.test")
})

test_that("the restricted tables are the ICPSR, NCVS 38604 and a2aj ones, with or without a db/ or files/ prefix", {
  r <- .hd(".morie_data_restricted")
  expect_identical(r(c("icpsr_1/t", "db/ncvs_38604/x", "files/a2aj_cases/y", "ncvs_1/t", "fec/icpsr_x", "db/fec/t")),
                   c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("a table is checked against the manifest's SHA-256, with one refetch before a mismatch is final", {
  skip_if_not_installed("digest")
  .hd_sandbox()
  chk <- .hd(".morie_data_check_sha")
  f <- withr::local_tempfile()
  writeLines("a,b\n1,2", f)
  good <- digest::digest(file = f, algo = "sha256")
  bad <- strrep("0", 64L)
  served <- NULL
  fetched <- 0L
  testthat::local_mocked_bindings(
    .morie_dl = function(url, dest, headers = NULL, label = basename(dest), size = NULL, timeout = 3600, ...) {
      fetched <<- fetched + 1L
      expect_identical(url, "https://data.rmorie.com/manifest.json")
      expect_identical(headers[["Authorization"]], "Bearer sk-test")
      writeLines(jsonlite::toJSON(served, auto_unbox = TRUE, null = "null"), dest)
      invisible(dest)
    }, .package = "rmorie")
  testthat::local_mocked_bindings(bricklayer_services = function(offline = TRUE) NULL, .package = "rmoriebricklayer")
  copy <- function() {
    g <- withr::local_tempfile(.local_envir = parent.frame())
    file.copy(f, g)
    g
  }
  # the cached manifest is stale, the refreshed one names the table's digest: accepted
  .hd_write_cache(.hd_manifest(bad))
  served <- .hd_manifest(toupper(good))
  g <- copy()
  expect_true(chk("db1/t1", g))
  expect_identical(fetched, 1L)
  expect_true(file.exists(g))
  # still a mismatch after the refetch: the file is deleted and the message names both digests
  .hd_write_cache(.hd_manifest(bad))
  served <- .hd_manifest(bad)
  g <- copy()
  expect_error(chk("db1/t1", g),
               sprintf("db1/t1 does not match the manifest's SHA-256 (%s... vs %s...)", substr(good, 1, 12), strrep("0", 12)),
               fixed = TRUE)
  expect_false(file.exists(g))
  # the refreshed manifest lists the table without a digest: used as it is
  .hd_write_cache(.hd_manifest(bad))
  served <- .hd_manifest("")
  g <- copy()
  expect_true(chk("db1/t1", g))
  expect_true(file.exists(g))
  # the refreshed manifest no longer lists it: deleted, and the error says so
  .hd_write_cache(.hd_manifest(bad))
  served <- list(datasets = list())
  g <- copy()
  expect_error(chk("db1/t1", g), "db1/t1 is not listed in the manifest of data.rmorie.com", fixed = TRUE)
  expect_false(file.exists(g))
  # a fresh cached manifest without the table, or without its digest: no refetch at all
  n <- fetched
  .hd_write_cache(list(datasets = list(list(key = "other/t"))))
  g <- copy()
  expect_error(chk("db1/t1", g), "is not listed in the manifest")
  expect_false(file.exists(g))
  .hd_write_cache(list(datasets = list(list(key = "db1/t1", sha256 = NULL))))
  g <- copy()
  expect_true(chk("db1/t1", g))
  expect_identical(fetched, n)
})

test_that("a refused download names a restricted table's terms, a rejected key, or the service's own message", {
  .hd_sandbox()
  get_ <- .hd(".morie_data_get")
  testthat::local_mocked_bindings(bricklayer_services = function(offline = TRUE) NULL, .package = "rmoriebricklayer")
  .hd_write_cache(list(datasets = list(list(key = "icpsr_9/t", bytes_gz = 1234L))))
  seen <- list()
  fail <- "HTTP 403 Forbidden"
  testthat::local_mocked_bindings(
    .morie_dl = function(url, dest, headers = NULL, label = basename(dest), size = NULL, timeout = 3600, ...) {
      seen[[length(seen) + 1L]] <<- list(url = url, label = label, size = size)
      if (is.null(fail)) return(invisible(dest))
      if (grepl("warn", fail)) warning(fail) else stop(fail)
    }, .package = "rmorie")
  dest <- withr::local_tempfile()
  expect_error(get_("/icpsr_9/t.csv.gz", dest),
               paste("icpsr_9/t is a restricted table (its source asks more than a citation): read and accept the",
                     "terms at https://data.rmorie.com/terms, signed in with your key, then ask again."), fixed = TRUE)
  # the label drops .csv.gz and the manifest's compressed size drives the bar
  expect_identical(seen[[1]]$url, "https://data.rmorie.com/icpsr_9/t.csv.gz")
  expect_identical(seen[[1]]$label, "icpsr_9/t")
  expect_identical(seen[[1]]$size, 1234L)
  expect_error(get_("/fec/t.csv.gz", dest), "data.rmorie.com rejected the stored key; run `rmorie login` again.",
               fixed = TRUE)
  expect_null(seen[[2]]$size)   # not in the manifest: no size
  fail <- "HTTP 401 Unauthorized"
  expect_error(get_("/icpsr_9/t.csv.gz", dest), "rejected the stored key", fixed = TRUE)
  fail <- "a warning: server closed"
  expect_error(get_("/files/x.rds", dest), "data.rmorie.com: a warning: server closed", fixed = TRUE)
  expect_identical(seen[[4]]$label, "files/x.rds")
  fail <- NULL
  expect_identical(get_("/files/x.rds", dest, size = 5), dest)
  expect_identical(seen[[5]]$size, 5)
})
