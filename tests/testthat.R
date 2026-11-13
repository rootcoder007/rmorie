library(testthat)
# covr merges one trace file per R process; under parallel testthat a worker
# that exits before its trace is written silently drops every line its test
# files touched, so the instrumented run stays in one process.
if (nzchar(Sys.getenv("R_COVR"))) Sys.setenv(TESTTHAT_PARALLEL = "FALSE")
# On Windows a parallel testthat worker dies with an access violation
# (exit code -1073741819) part-way through a file, which R-universe reports
# as a failed check; the suite runs in one process there.
if (.Platform$OS.type == "windows") Sys.setenv(TESTTHAT_PARALLEL = "FALSE")
library(rmorie)

# R-universe gives the whole check step 60 minutes, and its slower runners (R-oldrel
# macOS x86_64 above all) spend much of that before the tests start (install, code and
# Rd checks, examples), so the full suite (about 45 CPU minutes) cannot fit there. Under a
# reference check (no NOT_CRAN=true: R-universe, CRAN) the files run in order, in one
# process, inside a wall-clock budget (MORIE_TEST_BUDGET_SECONDS: 25 minutes, 10 on
# macOS x86_64), stopping before a chunk that would not fit; the check fails only on a
# real failure and the files not reached are counted. One process also keeps clear of
# parallel testthat workers stalling there. NOT_CRAN=true (the package's own CI and
# developer machines) always runs every file.
mac_x86 <- identical(Sys.info()[["sysname"]], "Darwin") && identical(R.version$arch, "x86_64")
slow_runner <- !identical(Sys.getenv("NOT_CRAN"), "true")
# One R process never gives memory back. The package's own CI runs the suite without
# parallel workers (TESTTHAT_PARALLEL=FALSE), so every test file ran in a single process
# whose memory only grew; on 2026-11-13 a hosted runner stopped answering part-way through
# the check ("lost communication with the server"), which is what a machine out of memory
# looks like from outside. The full suite therefore runs in batches of test files, each
# batch in a fresh R process: the peak is the largest batch's, not the whole suite's.
# Inside a batch testthat runs as it did before (parallel workers where they are on, one
# process where they are off), and covr still counts every line, because each process that
# loads the instrumented package writes its own trace file. MORIE_TEST_BATCH sets the
# number of files in a batch (default 100); MORIE_TEST_BATCH=0 runs the suite in one
# process, as before.
batch <- suppressWarnings(as.integer(Sys.getenv("MORIE_TEST_BATCH", "100")))
if (!slow_runner && (is.na(batch) || batch <= 0L)) {
  test_check("rmorie")
} else if (!slow_runner) {
  files <- sort(list.files("testthat", "^test.*\\.[rR]$"))
  names <- sub("^test[-_]?", "", sub("\\.[rR]$", "", files))
  groups <- split(names, ceiling(seq_along(names) / batch))
  rscript <- file.path(R.home("bin"), "Rscript")
  # a child R must not read R CMD check's startup file (R_TESTS); unset here, not through
  # system2(env =), which Windows ignores
  Sys.unsetenv("R_TESTS")
  child <- tempfile(fileext = ".R")
  writeLines(c(
    "testthat::test_dir(",
    "  'testthat', package = 'rmorie', load_package = 'installed',",
    "  filter = Sys.getenv('MORIE_TEST_FILTER'),",
    "  reporter = testthat::check_reporter(), stop_on_failure = TRUE",
    ")"
  ), child)
  failed <- character()
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    # test file names are letters, digits, "_", "-" and "."; only "." is special
    filter <- paste0("^(", paste(gsub(".", "\\.", g, fixed = TRUE), collapse = "|"), ")$")
    Sys.setenv(MORIE_TEST_FILTER = filter)
    cat(sprintf("test batch %d of %d: %d files, from %s\n", i, length(groups), length(g), g[1L]))
    if (!identical(system2(rscript, shQuote(child)), 0L)) failed <- c(failed, g[1L])
  }
  if (length(failed)) {
    stop("tests failed in the batches starting at: ", toString(failed), call. = FALSE)
  }
} else {
  # test_dir() reads Config/testthat/parallel from the DESCRIPTION; the budgeted run is
  # one process, so it starts no workers that could stall
  Sys.setenv(TESTTHAT_PARALLEL = "FALSE")
  budget <- as.numeric(Sys.getenv("MORIE_TEST_BUDGET_SECONDS", if (mac_x86) "600" else "1500"))
  files <- sort(list.files("testthat", "^test-.*\\.[rR]$"))
  names <- sub("^test-(.*)\\.[rR]$", "\\1", files)
  chunks <- split(seq_along(files), ceiling(seq_along(files) / 20))
  t0 <- Sys.time()
  ran <- character()
  failed <- 0L
  n <- 0L
  last <- 0
  for (idx in chunks) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (elapsed + last > budget) break
    t1 <- Sys.time()
    pat <- paste0("^(", paste(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", names[idx]), collapse = "|"), ")$")
    res <- as.data.frame(testthat::test_dir("testthat", filter = pat,
      package = "rmorie", load_package = "installed",
      reporter = testthat::ListReporter$new(), stop_on_failure = FALSE))
    last <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
    ran <- c(ran, files[idx])
    n <- n + nrow(res)
    failed <- failed + sum(res$failed) + sum(res$error)
  }
  left <- setdiff(files, ran)
  cat(sprintf("test budget: %d of %d files, %d tests, %d failed, %.0f s\n",
              length(ran), length(files), n, failed,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (length(left)) cat(length(left), "files not reached, starting at", left[1], "\n")
  if (failed > 0L) stop(failed, " test failure(s) on the budgeted runner")
}
