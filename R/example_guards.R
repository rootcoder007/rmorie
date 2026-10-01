# SPDX-License-Identifier: AGPL-3.0-or-later

#' Are an example's prerequisites available?
#'
#' One short predicate for the \code{@examplesIf} guards of this package:
#' installed packages, a configured service, or a bundled data file. Each
#' argument is a package name or one of the tokens below; the result is
#' \code{TRUE} when every one holds (\code{any = TRUE}: when at least one
#' holds). Nothing is loaded or attached; nothing leaves the machine.
#'
#' Tokens: \code{"sql"} (DBI and RSQLite), \code{"bigquery"} (bigrquery and
#' a \code{GCP_PROJECT} in the environment), \code{"ollama"} (a reachable
#' local Ollama server), \code{"sodium"} (the libsodium-backed crypto
#' routines), and a data file name such as \code{"vpd_crime_sample.csv"}
#' (the file under this package's \code{extdata}, or the rmoriedata package).
#'
#' @param ... Package names or tokens, as character strings.
#' @param any Logical; \code{TRUE} returns \code{TRUE} when any requirement
#'   holds rather than all of them.
#' @return A single logical.
#' @examples
#' morie_has("stats")
#' morie_has("sql")
#' morie_has("xgboost", "gbm", any = TRUE)
#' morie_has("vpd_crime_sample.csv")
#' @export
morie_has <- function(..., any = FALSE) {
  need <- as.character(unlist(list(...)))
  if (!length(need)) return(TRUE)
  pkg <- utils::packageName()
  has_pkg <- function(p) requireNamespace(p, quietly = TRUE)
  one <- function(x) {
    if (identical(x, "sql")) return(has_pkg("DBI") && has_pkg("RSQLite"))
    if (identical(x, "bigquery")) {
      return(nzchar(Sys.getenv("GCP_PROJECT")) && has_pkg("bigrquery"))
    }
    if (identical(x, "ollama")) {
      return(isTRUE(tryCatch(morie_llm_probe_ollama(), error = function(e) FALSE)))
    }
    if (identical(x, "sodium")) {
      return(isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE)))
    }
    if (startsWith(x, "data:") || grepl("[.](csv|gz|json|parquet)$", x)) {
      f <- sub("^data:", "", x)
      return(nzchar(system.file("extdata", f, package = pkg)) || has_pkg("rmoriedata"))
    }
    has_pkg(x)
  }
  ok <- vapply(need, one, logical(1))
  if (isTRUE(any)) base::any(ok) else all(ok)
}
