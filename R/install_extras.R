# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3LLL: morie_install_extras() -- user-callable installer for
# optional Suggests packages and system-library hints.
#
# CRAN policy forbids install.packages() inside .onLoad() (no
# user-home writes, no network at load), so morie ships this opt-in
# helper instead. Users run morie_install_extras() once after install
# to populate the optional dependency surface their workflow needs.

#' Install morie's optional dependencies (interactive helper)
#'
#' morie's `Suggests:` list spans ~50 R packages (causal/ML/spatial/IO
#' families). CRAN policy requires us to leave their install to the
#' user (no `install.packages()` at load time, no user-home writes).
#' This helper resolves which Suggests are missing and (with user
#' confirmation) installs them, plus prints platform-specific install
#' hints for the system libraries morie's C/C++ backends use
#' (`libcurl`, `libsodium`, optional `liboqs`).
#'
#' @section System libraries:
#' morie's compiled backends need three C libraries available at build
#' time. Two are typically pre-installed on developer machines; one is
#' optional and gates the post-quantum cryptography family. Install
#' BEFORE installing/upgrading morie so the configure-time probes pick
#' them up.
#'
#' \itemize{
#'   \item \strong{libcurl} (required for HTTP fetchers)
#'     \itemize{
#'       \item Debian/Ubuntu: \code{sudo apt-get install libcurl4-openssl-dev}
#'       \item Fedora/RHEL:   \code{sudo dnf install libcurl-devel}
#'       \item macOS:         pre-installed (Apple's libcurl); or \code{brew install curl}
#'       \item Windows:       bundled with Rtools
#'     }
#'   \item \strong{libsodium} (required for ChaCha20-Poly1305 + HKDF-SHA256)
#'     \itemize{
#'       \item Debian/Ubuntu: \code{sudo apt-get install libsodium-dev}
#'       \item Fedora/RHEL:   \code{sudo dnf install libsodium-devel}
#'       \item macOS:         \code{brew install libsodium}
#'     }
#'   \item \strong{liboqs} (optional, gates ML-KEM-768 + ML-DSA-65)
#'     \itemize{
#'       \item Debian/Ubuntu: build from source (not yet packaged);
#'         \url{https://github.com/open-quantum-safe/liboqs}
#'       \item Fedora/RHEL:   \code{sudo dnf install liboqs-devel} (recent releases)
#'       \item macOS:         \code{brew install liboqs}
#'     }
#' }
#'
#' @param which Either `"missing"` (install only missing Suggests --
#'   the default), `"all"` (install/upgrade every Suggests), or a
#'   character vector of specific package names.
#' @param ask Logical. If `TRUE` (default), prompt the user before
#'   installing. Set to `FALSE` for non-interactive CI workflows.
#' @param repos The CRAN-like repository URL(s) to install from.
#'   Default uses `getOption("repos")`, falling back to the RStudio
#'   CRAN mirror.
#' @param dependencies Passed through to [utils::install.packages()].
#'   Default `NA` honours the package's `Depends`/`Imports`/`LinkingTo`
#'   only -- not the optional Suggests-of-Suggests cascade.
#' @param ... Extra args forwarded to [utils::install.packages()].
#'
#' @return Invisibly: a list with `installed` (character of packages
#'   added this call), `already_present` (already installed),
#'   `failed` (failed to install), and `system_libs` (named logical of
#'   detected system libraries).
#'
#' @examples
#' \dontrun{
#'   # Interactive: install whichever Suggests are missing
#'   morie_install_extras()
#'
#'   # CI / scripted: install all, no prompt
#'   morie_install_extras(which = "all", ask = FALSE)
#'
#'   # Just one family
#'   morie_install_extras(which = c("hawkes", "sf", "spdep"))
#' }
#'
#' @export
morie_install_extras <- function(which = "missing",
                                 ask = interactive(),
                                 repos = NULL,
                                 dependencies = NA,
                                 ...) {
  suggests <- .morie_get_suggests()
  if (is.character(which) && length(which) == 1L && which == "missing") {
    needs <- suggests[!vapply(suggests, .morie_pkg_installed, logical(1L))]
  } else if (is.character(which) && length(which) == 1L && which == "all") {
    needs <- suggests
  } else if (is.character(which)) {
    needs <- which
  } else {
    stop("`which` must be \"missing\", \"all\", or a character vector.",
         call. = FALSE)
  }

  already <- suggests[vapply(suggests, .morie_pkg_installed, logical(1L))]
  message(sprintf(
    "morie optional dependencies:\n  Already installed: %d\n  Will install: %d",
    length(already), length(needs)
  ))
  if (length(needs)) {
    message("Packages to install:")
    message(paste("   ", needs, collapse = "\n"))
  }

  syslibs <- .morie_check_system_libs()
  message("\nSystem libraries:")
  for (nm in names(syslibs)) {
    message(sprintf("  %-10s %s", nm,
                    if (syslibs[[nm]]) "OK" else "MISSING (see ?morie_install_extras)"))
  }
  if (!syslibs[["libcurl"]] || !syslibs[["libsodium"]]) {
    message(
      "\nWARNING: A required system library is missing. Re-install morie ",
      "AFTER installing the system library so the configure probe ",
      "links morie's C/C++ backends against it."
    )
  }

  if (length(needs) == 0L) {
    message("\nNothing to install.")
    return(invisible(list(installed = character(0),
                          already_present = already,
                          failed = character(0),
                          system_libs = syslibs)))
  }

  if (isTRUE(ask)) {
    ans <- readline(sprintf(
      "\nInstall %d package%s now? [y/N] ",
      length(needs), if (length(needs) == 1L) "" else "s"
    ))
    if (!tolower(substr(ans, 1L, 1L)) %in% c("y")) {
      message("Skipped (user declined).")
      return(invisible(list(installed = character(0),
                            already_present = already,
                            failed = character(0),
                            system_libs = syslibs)))
    }
  }

  if (is.null(repos)) {
    repos <- getOption("repos", default = c(CRAN = "https://cloud.r-project.org"))
    if (any(repos == "@CRAN@")) {
      repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
    }
  }

  installed <- character(0)
  failed <- character(0)
  for (pkg in needs) {
    res <- tryCatch({
      utils::install.packages(pkg, repos = repos,
                              dependencies = dependencies, ...)
      .morie_pkg_installed(pkg)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(res)) {
      installed <- c(installed, pkg)
    } else {
      failed <- c(failed, pkg)
    }
  }

  message(sprintf("\nInstalled: %d  Failed: %d",
                  length(installed), length(failed)))
  if (length(failed)) {
    message("Failed packages: ", paste(failed, collapse = ", "))
  }

  invisible(list(installed = installed,
                 already_present = already,
                 failed = failed,
                 system_libs = syslibs))
}


# Internal: read this package's Suggests field from its DESCRIPTION.
.morie_get_suggests <- function() {
  desc_path <- system.file("DESCRIPTION", package = "morie")
  if (!nzchar(desc_path)) {
    stop("Could not locate morie's DESCRIPTION; package not installed?",
         call. = FALSE)
  }
  d <- read.dcf(desc_path)
  if (!"Suggests" %in% colnames(d)) return(character(0))
  raw <- d[1L, "Suggests"]
  pkgs <- strsplit(raw, ",\\s*")[[1L]]
  # Strip version constraints like "testthat (>= 3.0.0)" -> "testthat"
  pkgs <- sub("\\s*\\(.*\\)\\s*$", "", pkgs)
  pkgs <- trimws(pkgs)
  pkgs[nzchar(pkgs)]
}


# Internal: is a CRAN package installed locally?
.morie_pkg_installed <- function(pkg) {
  isTRUE(requireNamespace(pkg, quietly = TRUE))
}


#' Ensure optional packages are installed (interactive helper)
#'
#' Function-body helper for morie endpoints that require optional
#' `Suggests:` packages. If every package in `pkgs` is already
#' installed, returns silently. Otherwise:
#'
#' \itemize{
#'   \item In an \strong{interactive} session, prompts the user once,
#'     and on consent installs the missing packages via
#'     [utils::install.packages()].
#'   \item In a \strong{non-interactive} session (R CMD check, CI,
#'     Rscript), throws an informative error with the morie command
#'     the user should run to fix things:
#'     \code{morie_install_extras(c("X", "Y"))}.
#' }
#'
#' Why not auto-install silently? CRAN policy forbids packages from
#' writing to the user's library or making network calls at function
#' call time without explicit consent. The interactive prompt is the
#' CRAN-blessed escape: the user IS the one consenting, and during
#' R CMD check or any non-interactive run, the function refuses to
#' touch the library.
#'
#' Typical use inside a morie function body that needs DoubleML and
#' ranger:
#'
#' \preformatted{
#' morie_estimate_irm <- function(...) {
#'   morie_ensure_extras(c("DoubleML", "ranger"))
#'   ...
#' }
#' }
#'
#' @param pkgs Character vector of required package names.
#' @param ask Logical. If `TRUE` (default), prompt in interactive
#'   sessions. Pass `FALSE` to skip the prompt and behave like
#'   non-interactive: error out with the install-hint message.
#' @param repos Optional CRAN repo URL(s). Default uses
#'   `getOption("repos")`, falling back to RStudio CRAN.
#'
#' @return Invisibly `TRUE` if all `pkgs` are (now) installed; throws
#'   otherwise.
#'
#' @examples
#' \dontrun{
#'   # Interactive (RStudio / R console): prompts to install if needed
#'   morie_ensure_extras(c("DoubleML", "ranger"))
#'
#'   # CI / Rscript: errors with install-hint instead of installing
#'   morie_ensure_extras(c("DoubleML"), ask = FALSE)
#' }
#'
#' @seealso [morie_install_extras()] for the user-facing bulk installer.
#' @export
morie_ensure_extras <- function(pkgs, ask = interactive(), repos = NULL) {
  stopifnot(is.character(pkgs), length(pkgs) >= 1L)
  miss <- pkgs[!vapply(pkgs, .morie_pkg_installed, logical(1L))]
  if (length(miss) == 0L) {
    return(invisible(TRUE))
  }

  if (!isTRUE(ask) || !interactive()) {
    stop(sprintf(
      "morie requires package%s %s. Install with:\n  morie_install_extras(c(%s))",
      if (length(miss) == 1L) "" else "s",
      paste(sQuote(miss), collapse = ", "),
      paste0("\"", miss, "\"", collapse = ", ")
    ), call. = FALSE)
  }

  msg <- sprintf(
    "morie needs %d package%s: %s\nInstall now? [y/N] ",
    length(miss),
    if (length(miss) == 1L) "" else "s",
    paste(miss, collapse = ", ")
  )
  ans <- readline(msg)
  if (!tolower(substr(ans, 1L, 1L)) %in% c("y")) {
    stop("morie cannot proceed without the requested package(s). ",
         "Run morie_install_extras() when ready.", call. = FALSE)
  }

  if (is.null(repos)) {
    repos <- getOption("repos", default = c(CRAN = "https://cloud.r-project.org"))
    if (any(repos == "@CRAN@")) {
      repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
    }
  }

  utils::install.packages(miss, repos = repos)

  # Verify install actually succeeded.
  still_miss <- miss[!vapply(miss, .morie_pkg_installed, logical(1L))]
  if (length(still_miss)) {
    stop(sprintf("Install failed for: %s",
                 paste(sQuote(still_miss), collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}


# Internal: probe whether the C system libraries are available.
# Uses configure-time flags written into DESCRIPTION at install
# (Phase 3JJJ1/2: MORIE_HAVE_SODIUM / MORIE_HAVE_LIBOQS), with
# library-load fallback for libcurl (Imports R-side).
.morie_check_system_libs <- function() {
  list(
    libcurl   = .morie_pkg_installed("curl") || .morie_pkg_installed("httr2"),
    libsodium = isTRUE(tryCatch(
      morie_crypto_sodium_available(),
      error = function(e) FALSE
    )),
    liboqs    = isTRUE(tryCatch(
      morie_crypto_liboqs_available(),
      error = function(e) FALSE
    ))
  )
}
