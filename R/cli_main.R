# SPDX-License-Identifier: AGPL-3.0-or-later

#' Command-line analysis entry point
#'
#' Single R-side dispatcher for the proprietary \code{rmorie-cli} binary's
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
#' }
#' The \code{"tps"}, \code{"nypd"}, and \code{"cpd"} subjects are recognised
#' by the CLI but have no single-call R analysis entry point yet; they return
#' a structured, non-crashing message pointing at the R API.
#'
#' @param subject Character scalar naming the analysis subject.
#' @param json Character scalar: a JSON object of options forwarded from the
#'   CLI flags (default \code{"{}"}). Unknown keys are ignored.
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
      as.list(jsonlite::fromJSON(json))
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

  not_wired <- function(subj, hint) {
    list(subject = subj, status = "not_available",
         message = hint)
  }

  result <- tryCatch(
    switch(
      subject,
      otis = {
        df <- morie_otis_load()
        do.call(morie_otis_all_analyses,
                keep(morie_otis_all_analyses, c(list(df = df), opts)))
      },
      siu = {
        do.call(morie_siu_all_analyses, keep(morie_siu_all_analyses, opts))
      },
      tps = not_wired(
        "tps",
        paste0("`analyze tps` requires selecting TPS datasets first; use the ",
               "R API: morie_tps_load(<name>) then morie_tps_analyze_all(dfs).")),
      nypd = not_wired(
        "nypd", "No NYPD analysis backend is implemented in rmorie yet."),
      cpd = not_wired(
        "cpd", "No Chicago PD analysis backend is implemented in rmorie yet."),
      stop(sprintf("unknown analysis subject: '%s' (expected one of otis, siu, tps, nypd, cpd)",
                   subject), call. = FALSE)
    ),
    error = function(e) list(subject = subject, status = "error",
                             message = conditionMessage(e))
  )

  cat(jsonlite::toJSON(result, auto_unbox = TRUE, force = TRUE,
                       null = "null", na = "null", digits = 6))
  cat("\n")
  invisible(result)
}
