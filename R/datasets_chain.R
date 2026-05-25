# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unified dataset-loader chain: live network -> bundled fixture ->
# synthetic generator -> typed-empty schema. Used by morie_datasets_*
# loaders that support a `source = c("auto", "live", "bundled",
# "synthetic", "empty")` argument so the same call site works on a
# fresh checkout, in CI with no network, and inside CRAN-safe tests
# that mock out the live endpoint.

#' Resolve a dataset loader through the live -> bundled -> synthetic chain.
#'
#' Internal dispatch used by the unified \code{morie_datasets_*}
#' loaders that accept a \code{source = c("auto", "live", "bundled",
#' "synthetic", "empty")} argument. The chain is:
#'
#' \describe{
#'   \item{auto}{Try \code{live_fn()} first (network). If it errors or
#'     returns \code{NULL}, fall back to the bundled fixture, then to
#'     \code{synth_fn(...)}, then to a typed-empty 0-row frame built
#'     from \code{columns}. Whichever step succeeds first is returned.}
#'   \item{live}{Run \code{live_fn()} only; error if it fails.}
#'   \item{bundled}{Read the bundled \code{inst/extdata/<fixture>.csv}
#'     only; error if missing.}
#'   \item{synthetic}{Run \code{synth_fn()} only; error if no generator
#'     is wired up for this dataset.}
#'   \item{empty}{Return a 0-row data.frame with the documented
#'     \code{columns}; error if no schema is wired up.}
#' }
#'
#' @param source One of \code{"auto"}, \code{"live"}, \code{"bundled"},
#'   \code{"synthetic"}, \code{"empty"}.
#' @param live_fn A function that returns the live frame. May error or
#'   return \code{NULL}; either is treated as a miss in \code{"auto"}.
#'   Pass \code{NULL} to mean "no live path".
#' @param bundled_name Character; the bundled fixture stem (without
#'   \code{.csv}). The file is looked up via
#'   \code{system.file("extdata", paste0(bundled_name, ".csv"), package = "morie")}.
#'   Pass \code{NULL} or \code{NA_character_} to mean "no bundled
#'   fixture".
#' @param synth_fn A function that returns the synthetic frame. Pass
#'   \code{NULL} to mean "no synthetic generator".
#' @param columns Character vector of the documented column schema for
#'   the typed-empty fallback. Pass \code{NULL} to mean "no schema".
#' @return A \code{data.frame}.
#' @keywords internal
#' @noRd
.morie_load_chain <- function(source = c("auto", "live", "bundled",
                                          "synthetic", "empty"),
                              live_fn = NULL,
                              bundled_name = NULL,
                              synth_fn = NULL,
                              columns = NULL) {
  source <- match.arg(source)

  step_live <- function() {
    if (is.null(live_fn)) return(NULL)
    out <- tryCatch(live_fn(), error = function(e) NULL)
    if (is.data.frame(out)) out else NULL
  }
  step_bundled <- function() {
    if (is.null(bundled_name) || is.na(bundled_name) ||
        !nzchar(bundled_name)) return(NULL)
    path <- system.file("extdata",
                        paste0(bundled_name, ".csv"),
                        package = "morie")
    if (!nzchar(path) || !file.exists(path)) return(NULL)
    out <- tryCatch(utils::read.csv(path,
                                     check.names = FALSE,
                                     stringsAsFactors = FALSE),
                    error = function(e) NULL)
    if (is.data.frame(out)) out else NULL
  }
  step_synth <- function() {
    if (is.null(synth_fn)) return(NULL)
    out <- tryCatch(synth_fn(), error = function(e) NULL)
    if (is.data.frame(out)) out else NULL
  }
  step_empty <- function() {
    if (is.null(columns) || length(columns) == 0L) return(NULL)
    as.data.frame(
      stats::setNames(
        replicate(length(columns), character(0), simplify = FALSE),
        as.character(columns)),
      stringsAsFactors = FALSE
    )
  }

  if (identical(source, "live")) {
    out <- step_live()
    if (is.null(out)) {
      stop("morie: source = 'live' but live_fn() failed or returned NULL.",
           call. = FALSE)
    }
    return(out)
  }
  if (identical(source, "bundled")) {
    out <- step_bundled()
    if (is.null(out)) {
      stop("morie: source = 'bundled' but no inst/extdata/",
           bundled_name, ".csv was found.",
           call. = FALSE)
    }
    return(out)
  }
  if (identical(source, "synthetic")) {
    out <- step_synth()
    if (is.null(out)) {
      stop("morie: source = 'synthetic' but no synthetic generator is wired.",
           call. = FALSE)
    }
    return(out)
  }
  if (identical(source, "empty")) {
    out <- step_empty()
    if (is.null(out)) {
      stop("morie: source = 'empty' but no documented column schema is wired.",
           call. = FALSE)
    }
    return(out)
  }

  # source = "auto" (default): try every step in order.
  out <- step_live()
  if (!is.null(out)) return(out)
  out <- step_bundled()
  if (!is.null(out)) return(out)
  out <- step_synth()
  if (!is.null(out)) return(out)
  out <- step_empty()
  if (!is.null(out)) return(out)
  stop(paste(
    "morie: every step of the load chain (live/bundled/synthetic/empty)",
    "returned NULL. Wire at least one of live_fn, bundled_name,",
    "synth_fn, or columns."
  ), call. = FALSE)
}
