# R/cli_entry.R, the lines PR #77 added that no other test reached: a warning that is not about a
# refused file passes through, a refused path gets its reason, and `rmorie help config` is
# rmoriebricklayer's config help. rmoriebricklayer's shell is a recorder.

.ce_capture <- function(...) {
  buf <- character()
  status <- morie_cli(c(...), out = function(s) buf <<- c(buf, s))
  list(text = paste(buf, collapse = ""), status = status)
}

.ce_sandbox <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(XDG_CONFIG_HOME = dir, R_USER_CACHE_DIR = dir, MORIE_CACHE_DB = file.path(dir, "c.duckdb"),
                      .local_envir = env)
  dir
}

test_that("a warning that is not about a refused file is left to the caller", {
  .ce_sandbox()
  testthat::local_mocked_bindings(
    bricklayer_cli = function(args, out = cat) {
      warning("the server is slow today")
      out("doctor ran\n")
      0L
    },
    .package = "rmoriebricklayer")
  expect_warning(r <- .ce_capture("doctor"), "the server is slow today", fixed = TRUE)
  expect_identical(r$status, 0L)
  expect_identical(r$text, "doctor ran\n")
})

test_that("a refused path says why, and a refused URL does not", {
  .ce_sandbox()
  msg <- NULL
  testthat::local_mocked_bindings(bricklayer_cli = function(args, out = cat) stop(msg), .package = "rmoriebricklayer")
  msg <- "Permission denied: /srv/data.csv"
  r <- .ce_capture("doctor")
  expect_identical(r$status, 1L)
  expect_identical(r$text, paste0("rmorie doctor: Permission denied: /srv/data.csv",
                                  " (permission denied, or the path does not exist)\n"))
  msg <- "cannot open the connection to 'https://x.example/'"
  r <- .ce_capture("doctor")
  expect_identical(r$text, "rmorie doctor: cannot open the connection to 'https://x.example/'\n")
})

test_that("`rmorie help config` is rmoriebricklayer's config help, named rmorie", {
  .ce_sandbox()
  seen <- NULL
  testthat::local_mocked_bindings(
    bricklayer_cli = function(args, out = cat) {
      seen <<- list(args = args, prog = Sys.getenv("RMBL_PROG"))
      out("config help\n")
      0L
    },
    .package = "rmoriebricklayer")
  r <- .ce_capture("help", "config")
  expect_identical(seen$args, c("config", "help"))
  expect_identical(seen$prog, "rmorie")
  expect_identical(r$status, 0L)
  expect_identical(r$text, "config help\n")
})
