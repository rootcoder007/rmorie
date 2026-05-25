# SPDX-License-Identifier: AGPL-3.0-or-later
#' OTIS analysis pipeline (RichResult-style driver)
#'
#' Wraps the six OTIS primitives in \code{otis.R} as morie RichResult
#' lists and exposes:
#'
#' \itemize{
#'   \item \code{\link{morie_otis_load}}: canonical CSV loader (reads
#'     the Rscript-exported \code{otis_main.csv} mirror).
#'   \item \code{\link{morie_otis_all_analyses}}: driver that runs
#'     rplace / astcmb / volat / rctrnd / otdesc on one data.frame and
#'     optionally serialises each result to disk under a user-supplied
#'     directory (CRAN-safe: never writes without an explicit
#'     \code{out_dir}).
#' }
#'
#' \code{morie_otis_otdml} is excluded from the bundle because it
#' requires the caller to specify \code{(treatment, outcome,
#' covariates)} -- call it directly when needed.
#'
#' Year-lock invariant
#' -------------------
#' OTIS \code{UniqueIndividual_ID} is randomly reassigned every fiscal
#' year. All analyses are computed within \code{EndFiscalYear};
#' cross-year ID joins are forbidden (the
#' \code{variable_taxonomy.R} registry sets
#' \code{cross_year_safe = FALSE}).
#'
#' @name morie_otis_pipeline
NULL


# ---------------------------------------------------------------------------
# Loader
# ---------------------------------------------------------------------------

#' Load the canonical OTIS CSV
#'
#' Reads the Rscript-exported mirror at
#' \code{file.path(morie_cache_dir("otis"), "otis_main.csv")} unless
#' \code{csv_path} is supplied. The expected schema is 10 columns:
#' \code{end_fiscal_year}, \code{unique_individual_id},
#' \code{region_at_time_of_placement},
#' \code{region_most_recent_placement}, \code{gender},
#' \code{age_category}, \code{mental_health_alert},
#' \code{suicide_risk_alert}, \code{suicide_watch_alert},
#' \code{number_of_placements}.
#'
#' To refresh the cache from the canonical
#' \code{correctional_stats_report_environment.RData} fixture, run the
#' repository script \code{scripts/export_otis_csv.R}.
#'
#' @param csv_path Optional explicit CSV path.
#' @param use_readr If \code{TRUE} and \pkg{readr} is installed, use
#'   \code{readr::read_csv}; otherwise base \code{read.csv}. Default
#'   \code{FALSE} (base R for CRAN portability).
#' @return data.frame.
#' @seealso \code{\link{morie_cache_dir}}.
#' @examples
#' \dontrun{
#'   df <- morie_otis_load()
#' }
#' @export
morie_otis_load <- function(csv_path = NULL, use_readr = FALSE) {
  if (!is.null(csv_path)) {
    if (!file.exists(csv_path)) {
      stop(sprintf(paste0("OTIS dataset not found at %s. Pass an ",
                          "existing csv_path or call morie_otis_load() ",
                          "with csv_path = NULL to use the bundled ",
                          "data.ontario.ca a01 fixture."), csv_path))
    }
    if (isTRUE(use_readr) && requireNamespace("readr", quietly = TRUE)) {
      return(as.data.frame(readr::read_csv(csv_path, show_col_types = FALSE)))
    }
    return(utils::read.csv(csv_path, check.names = FALSE,
                            stringsAsFactors = FALSE))
  }
  cached <- file.path(morie_cache_dir("otis"), "otis_main.csv")
  if (file.exists(cached)) {
    if (isTRUE(use_readr) && requireNamespace("readr", quietly = TRUE)) {
      return(as.data.frame(readr::read_csv(cached, show_col_types = FALSE)))
    }
    return(utils::read.csv(cached, check.names = FALSE,
                            stringsAsFactors = FALSE))
  }
  # Fall back to the bundled OTIS A01 fixture (real CKAN slice from
  # data.ontario.ca; Open Government Licence -- Ontario). morie ships
  # this so morie_otis_load() works on a fresh checkout without
  # requiring users to download the full OTIS first.
  morie_datasets_otis_a01(offline = TRUE)
}


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

#' Run the full OTIS analysis bundle
#'
#' Calls \code{morie_otis_rplace} / \code{morie_otis_astcmb} /
#' \code{morie_otis_volat} / \code{morie_otis_rctrnd} /
#' \code{morie_otis_otdesc} on \code{df} for one fiscal \code{year} and
#' returns a named list of \code{morie_otis_result} objects. If
#' \code{out_dir} is supplied, each result is also written to disk as a
#' \code{.txt} (\code{format()}) and a \code{.json}
#' (\code{jsonlite::toJSON} when available, else \code{dput}).
#'
#' CRAN-safe: with \code{out_dir = NULL} (default) no files are written.
#'
#' @param df OTIS data.frame.
#' @param year Integer fiscal year.
#' @param sex Optional gender filter passed to
#'   \code{morie_otis_rplace}.
#' @param out_dir Optional output directory. When non-NULL the
#'   directory is created if missing.
#' @return Named list of \code{morie_otis_result}s.
#' @examples
#' \dontrun{
#'   df <- morie_otis_load()
#'   res <- morie_otis_all_analyses(df, year = 2024)
#' }
#' @export
morie_otis_all_analyses <- function(df, year,
                                     sex = NULL,
                                     out_dir = NULL) {
  stopifnot(is.data.frame(df))
  fns <- list(
    rplace = function() morie_otis_rplace(df, year = year, sex = sex),
    astcmb = function() morie_otis_astcmb(df),
    volat  = function() morie_otis_volat(df),
    rctrnd = function() morie_otis_rctrnd(df),
    otdesc = function() morie_otis_otdesc(df)
  )
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  results <- list()
  for (nm in names(fns)) {
    r <- tryCatch(fns[[nm]](), error = function(e) {
      out <- list(title = sprintf("otis.%s (failed)", nm),
                  warnings = sprintf("%s: %s",
                                      class(e)[1], conditionMessage(e)))
      class(out) <- c("morie_otis_result", "morie_rich_result", "list")
      out
    })
    results[[nm]] <- r
    if (!is.null(out_dir)) {
      tryCatch({
        writeLines(format(r),
                   con = file.path(out_dir,
                                   sprintf("otis_analysis_%s.txt", nm)))
        if (requireNamespace("jsonlite", quietly = TRUE)) {
          writeLines(jsonlite::toJSON(r$payload, pretty = TRUE,
                                       auto_unbox = TRUE, null = "null",
                                       force = TRUE),
                     con = file.path(out_dir,
                                     sprintf("otis_analysis_%s.json", nm)))
        }
      }, error = function(e) {
        warning(sprintf("Could not write %s output: %s", nm,
                        conditionMessage(e)))
      })
    }
  }
  results
}


# ---------------------------------------------------------------------------
# Default print/format for morie_otis_result
# ---------------------------------------------------------------------------

#' @export
format.morie_otis_result <- function(x, ...) {
  lines <- character(0)
  lines <- c(lines, sprintf("== %s ==", x$title %||% "(untitled)"))
  if (length(x$summary_lines)) {
    for (nm in names(x$summary_lines)) {
      v <- x$summary_lines[[nm]]
      lines <- c(lines, sprintf("  %s: %s", nm,
                                 paste(as.character(v), collapse = " ")))
    }
  }
  for (tb in x$tables %||% list()) {
    lines <- c(lines, "", tb$title)
    if (length(tb$headers)) {
      lines <- c(lines, paste(tb$headers, collapse = " | "))
    }
    for (row in tb$rows) {
      lines <- c(lines, paste(vapply(row, function(v)
        paste(as.character(v), collapse = " "), character(1)),
        collapse = " | "))
    }
  }
  if (nzchar(x$interpretation %||% "")) {
    lines <- c(lines, "", "Interpretation:", strwrap(x$interpretation,
                                                       width = 72))
  }
  if (length(x$warnings)) {
    lines <- c(lines, "", "Warnings:",
               paste0("  - ", x$warnings))
  }
  paste(lines, collapse = "\
")
}

#' @export
print.morie_otis_result <- function(x, ...) {
  cat(format(x, ...), "\
", sep = "")
  invisible(x)
}

# tiny %||% helper (in case it isn't already exported elsewhere)
