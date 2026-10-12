# R/cli_verbs.R: the ingest, download-bootstrap, exec, edit and percysuits verbs and the
# table / flag / inspect helpers, driven with the same flag/has/out closures morie_cli()
# builds, every fetcher replaced by a recorder.

.cv_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.cv <- function(name) get(name, envir = environment(morie_cli))

.cv_args <- function(rest) {
  txt <- character()
  list(
    rest = rest,
    flag = function(name) {
      i <- match(name, rest)
      if (is.na(i) || i == length(rest)) NULL else rest[[i + 1L]]
    },
    has = function(name) name %in% rest,
    out = function(s) txt <<- c(txt, s),
    text = function() paste(txt, collapse = ""))
}

test_that("the flag stripper, the table finder and the inspect text behave", {
  sf <- .cv(".cli_strip_flag")
  expect_equal(sf(c("a", "--k", "v", "b"), "--k"), c("a", "b"))
  expect_equal(sf(c("a", "--q", "b"), "--q", with_value = FALSE), c("a", "b"))
  expect_equal(sf(c("a", "b"), "--none"), c("a", "b"))
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "power-design"))
  for (f in c("power_summary.csv", "ebac_core_fit.csv", "power-design/grid.csv", "other.txt", "descriptive_statistics_1.csv")) {
    writeLines("x", file.path(dir, f))
  }
  cf <- .cv(".cli_csv_files")
  expect_setequal(basename(cf(dir)), c("descriptive_statistics_1.csv", "ebac_core_fit.csv", "power_summary.csv", "grid.csv"))
  got <- cf(dir, "power-design")
  expect_true(all(basename(got) %in% c("grid.csv", "power_summary.csv")))
  none <- cf(dir, "nothing-here")
  expect_length(none, 0)
  expect_match(attr(none, "note"), "no table of nothing-here in")
  expect_equal(attr(none, "rc"), 1L)
  outs <- .cv(".morie_module_outputs")
  fig <- names(Filter(function(v) !any(grepl("\\.csv$", v, ignore.case = TRUE)), outs))
  if (length(fig)) {
    r <- cf(dir, fig[1])
    expect_match(attr(r, "note"), "writes no tables")
    expect_equal(attr(r, "rc"), 0L)
  }
  it <- .cv(".cli_inspect_text")
  expect_match(it(list(path = "a.csv", format = "csv", exists = FALSE, size_bytes = 0)), "status: missing")
  t2 <- it(list(path = "a.csv", format = "csv", exists = TRUE, size_bytes = 12, columns = c("x", "y"),
                contents_preview = data.frame(x = 1:2, y = c("a", "b"))))
  expect_match(t2, "columns: x, y")
  expect_match(t2, "rows: 2  columns: 2")
  t3 <- it(list(path = "a.rds", format = "rds", exists = TRUE, size_bytes = 3, contents_preview = list(a = 1)))
  expect_match(t3, "List of 1")
})

test_that("the ingest verb routes each portal to its fetcher and prints usage otherwise", {
  ingest <- .cv(".cli_ingest")
  run <- function(...) {
    a <- .cv_args(c(...))
    rc <- ingest(a$rest, a$flag, a$has, a$out)
    list(rc = rc, text = a$text())
  }
  expect_equal(run()$rc, 2L)
  expect_match(run()$text, "^usage: rmorie ingest ckan")
  r <- run("nope")
  expect_equal(r$rc, 2L)
  expect_match(r$text, "unknown portal 'nope'; valid: ckan, tps, siu, a2aj")
  seen <- new.env()
  testthat::local_mocked_bindings(
    morie_ingest_ckan_search_packages = function(portal, q, rows = 20L) {
      seen$search <- list(portal, q, rows)
      data.frame(id = c("p1", "p2"), title = c("A", "B"))
    },
    morie_ingest_ckan_fetch_package_csvs = function(portal, id) {
      seen$pkg <- id
      list(one = data.frame(a = 1:3), `_failed_two` = data.frame(error = "timed out"), note = "skip")
    },
    morie_ingest_tps_layers = function() data.frame(name = c("major-crime", "shootings")),
    morie_ingest_tps_fetch = function(layer, year = NULL, where = NULL, return_geometry = FALSE, max_features = NULL) {
      seen$tps <- list(layer, year, where, return_geometry, max_features)
      data.frame(OCC_YEAR = c(2020, 2021))
    },
    morie_siu_index = function() data.frame(drid = 1:3),
    morie_siu_fetch_report = function(drid, ...) {
      seen$drid <- drid
      if (drid == "9") "<p>Case Number: </p>" else "<p>Case Number: 24-OCI-001</p>"
    },
    morie_ingest_a2aj_coverage = function(doc_type = "cases") data.frame(doc_type = doc_type, n = 10),
    morie_ingest_a2aj_search = function(q, doc_type = "cases", size = 10L) data.frame(q = q, size = size, doc_type = doc_type),
    morie_ingest_a2aj_fetch = function(cit, doc_type = "cases") if (cit == "missing") NULL else list(citation = cit),
    .package = .cv_pkg)
  expect_equal(run("ckan")$rc, 2L)
  r <- run("ckan", "--portal", "https://p", "--search", "crime", "--rows", "5")
  expect_equal(r$rc, 0L)
  expect_equal(seen$search, list("https://p", "crime", 5L))
  expect_match(r$text, "p1")
  od <- file.path(withr::local_tempdir(), "ckan")
  r <- run("ckan", "--portal", "https://p", "--package", "pk", "--out", od)
  expect_equal(r$rc, 1L)
  expect_equal(seen$pkg, "pk")
  expect_match(r$text, "FAILED two: timed out")
  expect_match(r$text, "wrote .*one.csv \\(3 rows\\)")
  expect_true(file.exists(file.path(od, "one.csv")))
  expect_equal(run("ckan", "--portal", "https://p")$rc, 2L)
  r <- run("tps", "--list")
  expect_equal(r$rc, 0L)
  expect_match(r$text, "major-crime")
  expect_equal(run("tps")$rc, 0L)
  r <- run("tps", "--layer", "major-crime", "--year", "2021", "--where", "1=1", "--max", "50", "--geometry")
  expect_equal(r$rc, 0L)
  expect_equal(seen$tps, list("major-crime", 2021L, "1=1", TRUE, 50L))
  expect_match(r$text, "OCC_YEAR")
  dest <- file.path(withr::local_tempdir(), "tps.csv")
  r <- run("tps", "--layer", "shootings", "--out", dest)
  expect_match(r$text, "wrote .*tps.csv \\(2 rows\\)")
  expect_equal(seen$tps[[2]], NULL)
  expect_true(file.exists(dest))
  r <- run("siu", "--list")
  expect_equal(r$rc, 0L)
  expect_match(r$text, "drid")
  od2 <- file.path(withr::local_tempdir(), "siu")
  r <- run("siu", "--report-id", "5001", "--out", od2)
  expect_equal(r$rc, 0L)
  expect_true(file.exists(file.path(od2, "5001.html")))
  r <- run("siu", "--report-id", "9", "--out", od2)
  expect_equal(r$rc, 1L)
  expect_match(r$text, "no SIU report with id 9")
  expect_match(run("a2aj")$text, "cases")
  r <- run("a2aj", "coverage", "--doc-type", "laws")
  expect_equal(r$rc, 0L)
  expect_match(r$text, "laws")
  r <- run("a2aj", "search", "bail", "--size", "3")
  expect_match(r$text, "bail")
  expect_match(r$text, " 3 ")
  r <- run("a2aj", "fetch", "2020 ONCA 1")
  expect_equal(r$rc, 0L)
  expect_match(r$text, "citation")
  r <- run("a2aj", "fetch", "missing", "--doc-type", "laws")
  expect_equal(r$rc, 1L)
  expect_match(r$text, "no law found for 'missing'")
  expect_equal(run("a2aj", "search")$rc, 2L)
})

test_that("download-bootstrap, exec and edit answer their usage, errors and happy paths", {
  db <- .cv(".cli_download_bootstrap")
  a <- .cv_args(character())
  expect_equal(db(a$flag, a$out, a$has), 2L)
  expect_match(a$text(), "^usage: rmorie download-bootstrap")
  testthat::local_mocked_bindings(
    morie_download_bootstrap = function(survey, refresh = FALSE, ...) if (survey == "bad") stop("unknown survey bad") else 2L,
    .package = .cv_pkg)
  a <- .cv_args(c("--survey", "ocs22bt", "--refresh"))
  expect_equal(db(a$flag, a$out, a$has), 0L)
  expect_match(a$text(), "2 bootstrap file\\(s\\) cached")
  a <- .cv_args(c("--survey", "bad"))
  expect_equal(db(a$flag, a$out, a$has), 1L)
  expect_match(a$text(), "unknown survey bad")
  ex <- .cv(".cli_exec")
  a <- .cv_args(c("1", "+", "2"))
  expect_equal(ex(a$rest, a$flag, a$out), 0L)
  expect_match(a$text(), "\\[1\\] 3")
  a <- .cv_args(c("invisible(cat('hi\\n'))"))
  expect_equal(ex(a$rest, a$flag, a$out), 0L)
  expect_match(a$text(), "^hi")
  a <- .cv_args(c("--file", "/no/such/file.R"))
  expect_equal(ex(a$rest, a$flag, a$out), 1L)
  expect_match(a$text(), "no such file")
  f <- withr::local_tempfile(fileext = ".R")
  writeLines("x <- 20; x * 2", f)
  a <- .cv_args(c("--file", f))
  expect_equal(ex(a$rest, a$flag, a$out), 0L)
  expect_match(a$text(), "\\[1\\] 40")
  a <- .cv_args(c("   "))
  expect_equal(ex(a$rest, a$flag, a$out), 2L)
  expect_match(a$text(), "^usage: rmorie exec")
  ed <- .cv(".cli_edit")
  a <- .cv_args(character())
  expect_equal(ed(a$rest, a$out), 2L)
  expect_match(a$text(), "^usage: rmorie edit FILE")
  testthat::local_mocked_bindings(interactive = function() FALSE, .package = "base")
  withr::local_envvar(VISUAL = NA, EDITOR = NA)
  target <- file.path(withr::local_tempdir(), "notes.txt")
  a <- .cv_args(target)
  expect_equal(ed(a$rest, a$out), 1L)
  expect_match(a$text(), "No editor: set EDITOR")
  if (!isatty(stdin())) {
    withr::local_envvar(EDITOR = "/usr/bin/vim -u NONE")
    a <- .cv_args(target)
    expect_equal(ed(a$rest, a$out), 1L)
    expect_match(a$text(), "vim is a terminal editor and stdin is not a terminal")
  }
  called <- NULL
  testthat::local_mocked_bindings(system2 = function(command, args = character(), ...) {
    called <<- c(command, args)
    0L
  }, .package = "base")
  withr::local_envvar(EDITOR = "mygui-editor")
  a <- .cv_args(target)
  expect_equal(ed(a$rest, a$out), 0L)
  expect_true(file.exists(target))
  expect_equal(called, c("mygui-editor", shQuote(target)))
})

test_that("percysuits reports the model inventory and pulls what is missing unless told to only say so", {
  ps <- .cv(".cli_percysuits")
  testthat::local_mocked_bindings(morie_llm_probe_ollama = function(...) FALSE, .morie_llm_ollama_base = function() "http://o:1",
                                  .package = .cv_pkg)
  a <- .cv_args(character())
  expect_equal(ps(a$flag, a$has, a$out), 1L)
  expect_match(a$text(), "Ollama is not reachable at http://o:1")
  want <- .cv(".cli_percy_models")()
  testthat::local_mocked_bindings(
    morie_llm_probe_ollama = function(...) TRUE,
    morie_llm_ollama_models = function(...) data.frame(name = c("gemma3:4b-it", "phi4-mini:latest"), stringsAsFactors = FALSE),
    .package = .cv_pkg)
  a <- .cv_args(c("--host", "http://h:2", "--dry-run"))
  expect_equal(ps(a$flag, a$has, a$out), 0L)
  expect_match(a$text(), sprintf("Installed: 2/%d \\| Missing: %d", length(want), length(want) - 2L))
  expect_match(a$text(), "Would pull: functiongemma:270m")
  pulled <- character()
  testthat::local_mocked_bindings(system2 = function(command, args = character(), ...) {
    pulled <<- c(pulled, args[2])
    if (args[2] == "mistral-nemo") 1L else 0L
  }, .package = "base")
  a <- .cv_args(character())
  expect_equal(ps(a$flag, a$has, a$out), 0L)
  expect_equal(pulled, setdiff(want, c("gemma3:4b", "phi4-mini")))
  expect_match(a$text(), "failed: mistral-nemo")
  testthat::local_mocked_bindings(morie_llm_ollama_models = function(...) data.frame(name = want), .package = .cv_pkg)
  a <- .cv_args(character())
  expect_equal(ps(a$flag, a$has, a$out), 0L)
  expect_match(a$text(), sprintf("Installed: %d/%d \\| Missing: 0", length(want), length(want)))
})
