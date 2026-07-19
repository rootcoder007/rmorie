# SPDX-License-Identifier: AGPL-3.0-or-later

#' Assemble the rest of the morie family from R
#'
#' The R-side entry to the morie "bricklayer": from an R session it reports
#' which family members are present and offers to install the ones that are
#' missing. You are already in R with \pkg{rmorie}, so this focuses on the
#' Python side (\code{morie} via \command{pip}) and on verifying the shared
#' C/C++ numeric core. The proprietary \code{rmorie-cli} is never
#' auto-installed -- only pointed to.
#'
#' The whole family is built on one shared C/C++ core (\code{libmorie} ->
#' \code{morie._core} in Python; \pkg{rmoriebricklayer}'s compiled kernels in
#' R). Without a C/C++ toolchain the packages fall back to slow pure-language
#' kernels or fail to build from source, so this also checks the toolchain
#' and whether the compiled backend actually loaded.
#'
#' @param yes Logical; install without prompting (default \code{FALSE}).
#' @param check Logical; report status only, install nothing (default
#'   \code{FALSE}).
#' @return Invisibly, a named logical vector of what is present.
#' @examples
#' \donttest{
#' morie_bricklayer(check = TRUE)
#' morie_bricklayer()
#' }
#' @export
morie_bricklayer <- function(yes = FALSE, check = FALSE) {
  RUNIV <- "https://rootcoder007.r-universe.dev"
  CRAN <- "https://cloud.r-project.org"
  CLI_URL <- "https://github.com/rootcoder007/rmorie-cli"

  find_python <- function() {
    for (p in c("python3", "python")) {
      exe <- Sys.which(p)
      if (nzchar(exe)) return(unname(exe))
    }
    ""
  }
  py <- find_python()
  py_run_ok <- function(code) {
    if (!nzchar(py)) return(FALSE)
    isTRUE(tryCatch(
      system2(py, c("-c", shQuote(code)), stdout = FALSE, stderr = FALSE) == 0L,
      error = function(e) FALSE
    ))
  }
  have_py_morie <- py_run_ok(
    "import importlib.util,sys; sys.exit(0 if importlib.util.find_spec('morie') else 1)")
  py_backend_ok <- py_run_ok(
    "import importlib.util,sys; sys.exit(0 if importlib.util.find_spec('morie._core') else 1)")

  have_cli <- nzchar(Sys.which("rmorie"))
  have_rdata <- requireNamespace("rmoriedata", quietly = TRUE)
  have_rbrick <- requireNamespace("rmoriebricklayer", quietly = TRUE)
  r_backend_ok <- isTRUE(tryCatch(morie_fast_available(), error = function(e) FALSE))

  has_cc <- any(nzchar(Sys.which(c("cc", "gcc", "clang"))))
  has_cxx <- any(nzchar(Sys.which(c("c++", "g++", "clang++"))))
  tc_ok <- has_cc && has_cxx

  mark <- function(ok, label) cat(sprintf("  [%s] %s\n", if (ok) "x" else " ", label))
  cat("morie family status:\n")
  mark(have_py_morie, "morie            (Python / pip)")
  mark(TRUE,          "rmorie           (R / this session)")
  mark(have_rdata,    "rmoriedata       (R)")
  mark(have_rbrick,   "rmoriebricklayer (R / shared C core)")
  mark(have_cli,      "rmorie-cli       (proprietary -- not auto-installed)")
  mark(tc_ok,         "C/C++ toolchain  (cc + c++ -- REQUIRED for the compiled core)")
  if (have_py_morie && !py_backend_ok)
    cat("  !! morie (Python) is installed but morie._core (C++ backend) is NOT active -- degraded.\n")
  if (!r_backend_ok)
    cat("  !! rmorie's C/C++ kernels are NOT active (morie_fast_available() is FALSE) -- slow pure-R fallback.\n")
  if (!tc_ok)
    cat("  !! No C/C++ toolchain detected. Install one (e.g. build-essential / Rtools / xcode-select)\n",
        "     -- the family needs it to build the shared core and run properly.\n", sep = "")
  cat("\n")

  present <- c(morie = have_py_morie, rmorie = TRUE, rmoriedata = have_rdata,
              rmoriebricklayer = have_rbrick, `rmorie-cli` = have_cli)
  if (isTRUE(check)) return(invisible(present))

  if (!have_cli)
    cat("note: rmorie-cli is proprietary (Receipt-of-Custody); obtain it at", CLI_URL, "\n")

  if (have_py_morie) {
    cat("Python morie is already present. Nothing to install.\n")
    return(invisible(present))
  }
  if (!nzchar(py)) {
    cat("Python not found. Install Python (https://www.python.org/downloads/), then `pip install morie`.\n")
    return(invisible(present))
  }

  do_it <- isTRUE(yes)
  if (!do_it) {
    if (!interactive()) {
      cat("Non-interactive; not installing. Run with yes = TRUE, or:\n  ",
          py, " -m pip install morie\n", sep = "")
      return(invisible(present))
    }
    ans <- readline("Install the Python package morie now? [Y/n] ")
    do_it <- ans %in% c("", "y", "Y", "yes", "YES")
  }
  if (!do_it) {
    cat("Skipped. Re-run morie_bricklayer() anytime.\n")
    return(invisible(present))
  }

  cat("-> installing Python morie ...\n")
  .morie_ensure_exec_allowed("pip install of Python morie")
  rc <- system2(py, c("-m", "pip", "install", "--upgrade", "morie"))
  if (rc != 0L) {
    cat("Python morie install failed (see output above).\n")
  } else if (!py_run_ok(
      "import importlib.util,sys; sys.exit(0 if importlib.util.find_spec('morie._core') else 1)")) {
    cat("WARNING: morie installed but morie._core (C++ backend) did not load -- it will be degraded.\n")
  } else {
    cat("Done. morie (Python) is ready.\n")
  }
  invisible(present)
}

#' Resolve a Wayback Machine snapshot URL (via rmoriebricklayer)
#'
#' Thin bridge to \code{rmoriebricklayer::wayback_snapshot_url()}: given a live
#' URL, returns the closest \code{archive.org} snapshot URL (or \code{NULL} if
#' none exists). Lets the morie family reuse the shared bricklayer Wayback
#' helpers -- e.g. to reach an archived copy of an Ontario Data Catalogue page
#' (the "Data on Inmates in Ontario" / OTIS releases) when the live portal is
#' unreachable -- instead of duplicating them.
#'
#' @param url A live URL to look up on the Wayback Machine.
#' @param timestamp Optional \code{YYYYMMDD[hhmmss]} to request the snapshot
#'   closest to that time; \code{NULL} (default) returns the most recent.
#' @return The snapshot URL (https), or \code{NULL} if unavailable.
#' @seealso \code{\link{morie_bricklayer}}, \code{\link{morie_download}}
#' @examples
#' \donttest{
#' morie_wayback_url("https://data.ontario.ca/dataset/data-on-inmates-in-ontario")
#' }
#' @export
morie_wayback_url <- function(url, timestamp = NULL) {
  if (!requireNamespace("rmoriebricklayer", quietly = TRUE)) {
    stop("rmoriebricklayer is required for morie_wayback_url(); run morie_bricklayer().",
         call. = FALSE)
  }
  rmoriebricklayer::wayback_snapshot_url(url, timestamp = timestamp)
}

#' Download a file with an automatic Wayback fallback (via rmoriebricklayer)
#'
#' Thin bridge to \code{rmoriebricklayer::friendly_download()}: downloads
#' \code{url} to \code{target_path}, falling back to the archived Wayback
#' snapshot if the live source is unreachable -- useful for pinned open-data
#' sources (e.g. the OTIS "Data on Inmates in Ontario" releases) that may move
#' or change upstream.
#'
#' @param url Live source URL.
#' @param target_path Destination file path.
#' @param attempt_wayback Logical or \code{NULL}; whether to fall back to the
#'   Wayback snapshot when the live fetch fails (passed through to bricklayer;
#'   \code{NULL} uses the bricklayer default).
#' @return The downloaded path, per \code{rmoriebricklayer::friendly_download()}.
#' @seealso \code{\link{morie_bricklayer}}, \code{\link{morie_wayback_url}}
#' @examples
#' \donttest{
#' morie_download(
#'   "https://data.ontario.ca/dataset/data-on-inmates-in-ontario",
#'   tempfile(fileext = ".html"))
#' }
#' @export
morie_download <- function(url, target_path, attempt_wayback = NULL) {
  if (!requireNamespace("rmoriebricklayer", quietly = TRUE)) {
    stop("rmoriebricklayer is required for morie_download(); run morie_bricklayer().",
         call. = FALSE)
  }
  rmoriebricklayer::friendly_download(url, target_path,
                                      attempt_wayback = attempt_wayback)
}
