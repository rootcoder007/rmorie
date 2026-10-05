# SPDX-License-Identifier: AGPL-3.0-or-later

#' Command-line analysis entry point
#'
#' Single R-side dispatcher for the \code{rmorie} command line's
#' \code{rmorie analyze <subject>} verb. The CLI shells out with
#' \code{Rscript -e 'rmorie::cli_main("<subject>", "<json>")'} and forwards
#' the parsed command-line flags as one JSON object; this function loads the
#' subject's data, runs the corresponding analysis suite, and prints the
#' result to \code{stdout} as JSON. It is not intended for interactive use.
#'
#' Supported subjects:
#' \itemize{
#'   \item \code{"otis"} -- Ontario OTIS carceral data. Loads the bundled
#'     OTIS fixture (offline) and runs \code{\link{morie_otis_all_analyses}};
#'     the \code{year} flag is forwarded.
#'   \item \code{"siu"} -- Special Investigations Unit; runs
#'     \code{\link{morie_siu_all_analyses}}.
#'   \item \code{"nypd"} -- New York Police Department; runs
#'     \code{\link{morie_nypd_all_analyses}} (bundled samples offline).
#'   \item \code{"cpd"} -- Chicago Police Department; runs
#'     \code{\link{morie_cpd_all_analyses}} (bundled samples offline).
#' }
#' The \code{"tps"} subject runs \code{\link{morie_tps_analyze_all}} on the
#' datasets named in \code{\{"datasets": [...], "nrows": N\}} (as
#' \code{\link{morie_tps_load}} takes them) or read from \code{\{"data": FILE\}};
#' without either it returns an error status (exit code 1 from the CLI).
#'
#' @param subject Character scalar naming the analysis subject.
#' @param json Character scalar: a JSON object of options forwarded from the
#'   CLI flags (default \code{"\\\\\\\\\\\\\\\{\\\\\\\\\\\\\\\}"}). Unknown keys are ignored.
#' @return Invisibly, the analysis result (a list). As a side effect, prints
#'   that result to \code{stdout} as JSON.
#' @examples
#' \donttest{
#' cli_main("otis", "{}")
#' }
#' @export
cli_main <- function(subject, json = "{}") {
  stopifnot(is.character(subject), length(subject) == 1L, nzchar(subject))
  if (!is.character(json) || length(json) != 1L) json <- "{}"

  opts <- tryCatch(
    if (nzchar(json) && !identical(json, "{}")) {
      as.list(.morie_from_json(json))
    } else {
      list()
    },
    error = function(e) list()
  )

  # Keep only options that are real formals of `fn` -- lets the CLI pass
  # extra flags (or flags valid only for other subjects) without error.
  keep <- function(fn, args) {
    args[intersect(names(args), names(formals(fn)))]
  }

  result <- tryCatch(
    switch(subject,
      otis = {
        if (is.null(opts$data)) {
          message("analyze otis: a 5-row SYNTHETIC sample shaped like the data.ontario.ca A01 table (it demonstrates the pipeline; its numbers are not findings); the real table: rmorie pull otisa01 --out FILE, then '{\"data\":\"FILE\"}'")
        }
        df <- morie_otis_load(opts$data)
        do.call(
          morie_otis_all_analyses,
          keep(morie_otis_all_analyses, c(list(df = df), opts))
        )
      },
      siu = {
        do.call(morie_siu_all_analyses, keep(morie_siu_all_analyses, opts))
      },
      nypd = {
        if (is.null(opts$arrests_df) && is.null(opts$complaint_df)) {
          message("analyze nypd: a 5-record built-in sample, not NYPD data; pass your own frames from R (morie_nypd_all_analyses(arrests_df = ...))")
        }
        do.call(morie_nypd_all_analyses, keep(morie_nypd_all_analyses, opts))
      },
      cpd = {
        if (is.null(opts$crime_df) && is.null(opts$arrests_df)) {
          message("analyze cpd: a 5-record built-in sample, not CPD data; pass your own frames from R (morie_cpd_all_analyses(crime_df = ...))")
        }
        do.call(morie_cpd_all_analyses, keep(morie_cpd_all_analyses, opts))
      },
      tps = {
        # the TPS feeds are many datasets: name them ({"datasets":["Assault"],"nrows":5000})
        # or give files ({"data":"FILE.csv"} or a list of files)
        dfs <- if (!is.null(opts$data)) {
          files <- as.character(unlist(opts$data))
          stats::setNames(lapply(files, function(f) {
            if (!file.exists(f)) stop(sprintf("analyze tps: no such file: %s", f), call. = FALSE)
            utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
          }), tools::file_path_sans_ext(basename(files)))
        } else if (!is.null(opts$datasets) || !is.null(opts$dataset)) {
          nm <- as.character(unlist(opts$datasets %||% opts$dataset))
          nr <- if (is.null(opts$nrows)) NULL else as.integer(opts$nrows)
          stats::setNames(lapply(nm, function(n) morie_tps_load(n, nrows = nr)), nm)
        }
        if (is.null(dfs)) {
          list(
            subject = "tps", status = "error",
            message = paste0("analyze tps needs the datasets: '{\"datasets\":[\"Assault\"],\"nrows\":5000}' ",
                             "(names as morie_tps_load() takes) or '{\"data\":\"FILE.csv\"}'")
          )
        } else {
          morie_tps_analyze_all(dfs)
        }
      },
      stop(sprintf(
        "unknown analysis subject: '%s' (expected one of otis, siu, tps, nypd, cpd)",
        subject
      ), call. = FALSE)
    ),
    error = function(e) {
      list(
        subject = subject, status = "error",
        message = conditionMessage(e)
      )
    }
  )

  cat(.morie_to_json(result,
    auto_unbox = TRUE, force = TRUE,
    null = "null", na = "null", digits = 6
  ))
  cat("\n")
  invisible(result)
}
