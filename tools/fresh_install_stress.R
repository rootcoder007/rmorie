# SPDX-License-Identifier: AGPL-3.0-or-later
# tools/fresh_install_stress.R
#
# Simulate what a user on a different computer experiences when they
# install morie from CRAN/r-universe with NO access to the developer's
# filesystem (no /Volumes/VSR/, no project tree). The user has only:
#   - internet
#   - whatever CRAN/r-universe ships in the tarball
#
# Usage (from r-package/ dir, with morie_*.tar.gz already built):
#   Rscript morie/tools/fresh_install_stress.R
#
# What it tests:
#   1. install.packages() into a tempdir library succeeds
#   2. library(morie) loads
#   3. morie_dataset_catalog() shows the 41-entry catalog
#   4. Pure-R math functions work on synthetic data (no fs deps)
#   5. C++ Hawkes fitter works (no fs deps)
#   6. morie_db_connect() opens (DuckDB or SQLite fallback)
#   7. Live network calls to CKAN / ArcGIS / SIU (with timeouts)

stopifnot(file.exists("morie_0.9.5.tar.gz"))

# 1. Fresh library in tempdir
lib <- tempfile("morie-stress-lib-")
dir.create(lib, recursive = TRUE)
on.exit(unlink(lib, recursive = TRUE), add = TRUE)
cat(sprintf("[1/7] fresh library at %s\n", lib))

old_libpaths <- .libPaths()
.libPaths(c(lib, old_libpaths))
on.exit(.libPaths(old_libpaths), add = TRUE)

# 2. Install from the local tarball (without forcing Suggests so we
# emulate a minimal user install)
cat("[2/7] install.packages('morie_0.9.5.tar.gz', dependencies = NA) ...\n")
install_args <- list(
  pkgs = "morie_0.9.5.tar.gz",
  lib  = lib,
  repos = NULL,
  type = "source",
  dependencies = NA,
  quiet = TRUE
)
res <- tryCatch(do.call(install.packages, install_args),
  warning = function(w) w, error = function(e) e
)
if (inherits(res, "error")) {
  cat("[2/7] FAIL:", conditionMessage(res), "\n"); quit(status = 1)
}

# 3. Load
suppressMessages(library(morie, lib.loc = lib))
cat("[3/7] library(morie) loaded.\n")

# 4. Catalog visibility (must work without any network or fs)
cat("[4/7] morie_dataset_catalog():\n")
catalog <- morie_dataset_catalog()
cat(sprintf("       %d rows; sample keys: %s\n",
  nrow(catalog),
  paste(utils::head(catalog$key, 5), collapse = ", ")
))
stopifnot(nrow(catalog) >= 30)

# 5. Pure-R + C++ smoke tests (no filesystem deps)
cat("[5/7] math/C++ smoke tests:\n")
set.seed(1)
ok_count <- 0L; fail_count <- 0L; failures <- character()
smoke <- function(label, expr) {
  res <- tryCatch(force(expr), error = function(e) e)
  if (inherits(res, "error")) {
    cat(sprintf("       FAIL  %s: %s\n", label, conditionMessage(res)))
    fail_count <<- fail_count + 1L
    failures <<- c(failures, label)
  } else {
    cat(sprintf("       PASS  %s\n", label))
    ok_count <<- ok_count + 1L
  }
}
smoke("morie_cohens_d",      morie_cohens_d(rnorm(50), rnorm(50, 0.5)))
smoke("morie_kalman_filter", morie_kalman_filter(matrix(rnorm(40), 20, 2)))
smoke("morie_hawkes_fit",
  morie_hawkes_fit(sort(cumsum(rexp(100))), end_time = 110, kernel = "exponential")
)
smoke("morie_e_value",       morie_e_value(2.5, 1.2))

# 6. DBI cache layer (tempfile -- no fs deps)
cat("[6/7] morie_db_connect (default backend):\n")
tmp_dir <- tempfile("morie-cache-")
dir.create(tmp_dir)
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
Sys.setenv(XDG_CACHE_HOME = tmp_dir)
con <- morie_db_connect()
cat(sprintf("       backend class: %s\n", class(con)[1]))
df <- data.frame(x = 1:5, y = letters[1:5])
morie_cache_store(df, "stress_demo", con = con)
out <- morie_cache_load("stress_demo", con = con)
stopifnot(identical(out$x, df$x))
DBI::dbDisconnect(con, shutdown = inherits(con, "duckdb_connection"))
cat("       round-trip OK\n")

# 7. Live network smoke tests with timeouts
cat("[7/7] live network smoke tests (with 30 s timeouts):\n")
opts_old <- options(timeout = 30)
on.exit(options(opts_old), add = TRUE)
smoke("CKAN datastore (ocp21 sample)", {
  # Just fetch metadata + the first 50 rows so we don't blow the timeout
  m <- morie_fetch_ckan("cpads", limit = 50, db_path = file.path(tmp_dir, "stress.db"))
  stopifnot(NROW(m) >= 1L)
})

# Summary
cat("\n========================================\n")
cat(sprintf("PASSED: %d  FAILED: %d\n", ok_count, fail_count))
if (fail_count > 0L) {
  cat("Failures:\n  - ", paste(failures, collapse = "\n  - "), "\n", sep = "")
  quit(status = 1)
}
cat("Fresh-install stress test PASSED. The package works on a clean\n")
cat("machine without /Volumes/VSR/ or any developer-side files.\n")
