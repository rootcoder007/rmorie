# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Google BigQuery public-data adapter (R port).
#
# A thin wrapper around the CRAN `bigrquery` package that lets morie
# pull BigQuery public datasets (e.g. `bigquery-public-data.chicago_crime`,
# `bigquery-public-data.new_york`, ...) with a single call.
#
# `bigrquery` is declared in Suggests, not Imports: the heavyweight
# Google auth / gargle stack should not be forced on every morie user.
# All entry points gate the dependency through `requireNamespace()`.
#
# Authentication: Application Default Credentials (ADC) -- the same
# flow the rest of the HADES-LLM Pi-rendered architecture uses
# (`gcloud auth application-default login` on a laptop; a
# service-account key or workload-identity binding in service
# contexts). `bigrquery::bq_auth()` discovers these automatically.
#
# Billing project resolution order:
#   1. explicit `billing_project` argument
#   2. `GCP_PROJECT` environment variable
#   3. NULL (let bigrquery infer from ADC)
# Public datasets cost the *caller's* project, not the dataset owner's.

# Internal: backtick-quote a BigQuery identifier; refuse anything that
# isn't a legal project / dataset / table name.
.morie_bq_quote_ident <- function(name) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("Illegal BigQuery identifier: ",
      utils::capture.output(print(name))[1L],
      call. = FALSE
    )
  }
  if (!grepl("^[A-Za-z0-9_-]+$", name)) {
    stop("Illegal BigQuery identifier: '", name, "'", call. = FALSE)
  }
  paste0("`", name, "`")
}

# Internal: resolve the billing project.
.morie_bq_billing_project <- function(billing_project = NULL) {
  if (!is.null(billing_project) && nzchar(billing_project)) {
    return(billing_project)
  }
  env <- Sys.getenv("GCP_PROJECT", "")
  if (nzchar(env)) {
    return(env)
  }
  NULL
}

# Internal: hard-fail with the canonical install hint if bigrquery is
# missing.
.morie_bq_require <- function() {
  if (!requireNamespace("bigrquery", quietly = TRUE)) {
    stop(
      "morie BigQuery ingest requires the optional 'bigrquery' ",
      "package.\
  install.packages('bigrquery')",
      call. = FALSE
    )
  }
}

#' Build a parameter-safe BigQuery SELECT
#'
#' Builds a \code{SELECT ... FROM `project`.`dataset`.`table`}
#' string with identifier validation and backtick-quoting.
#' \code{where} is passed through unchanged; callers compose SQL
#' fragments themselves and are responsible for not injecting hostile
#' clauses (same contract as
#' \code{\link{morie_ingest_bigquery_query}}).
#'
#' @param project,dataset,table Fully-qualified BigQuery table
#'   reference, e.g. project \code{"bigquery-public-data"}, dataset
#'   \code{"chicago_crime"}, table \code{"crime"}.
#' @param where Optional raw SQL \code{WHERE} clause (no leading
#'   \code{WHERE}).
#' @param limit Optional \code{LIMIT}.
#' @param select Projection list (default \code{"*"}).
#' @return A SQL string.
#' @examples
#' morie_ingest_bigquery_build_sql(
#'   project = "bigquery-public-data",
#'   dataset = "chicago_crime",
#'   table   = "crime",
#'   where   = "year = 2024",
#'   limit   = 10000L
#' )
#' @export
morie_ingest_bigquery_build_sql <- function(project, dataset, table,
                                            where = NULL,
                                            limit = NULL,
                                            select = "*") {
  qp <- .morie_bq_quote_ident(project)
  qd <- .morie_bq_quote_ident(dataset)
  qt <- .morie_bq_quote_ident(table)
  if (!is.character(select) || length(select) != 1L || !nzchar(select)) {
    stop("`select` must be a single non-empty string.", call. = FALSE)
  }
  sql <- sprintf("SELECT %s FROM %s.%s.%s", select, qp, qd, qt)
  if (!is.null(where) && nzchar(where)) {
    sql <- paste0(sql, "\
WHERE ", where)
  }
  if (!is.null(limit)) {
    lim <- suppressWarnings(as.integer(limit))
    if (is.na(lim) || lim < 0L) {
      stop("`limit` must be a non-negative integer.", call. = FALSE)
    }
    sql <- paste0(sql, "\
LIMIT ", format(lim, scientific = FALSE))
  }
  sql
}

#' Execute a BigQuery SQL query and return a data.frame
#'
#' Runs arbitrary SQL against BigQuery via \pkg{bigrquery}, downloads
#' the full result set, and returns it as a base R \code{data.frame}.
#' Authentication uses Application Default Credentials (the same flow
#' the rest of the HADES-LLM stack uses); to authenticate
#' interactively, run \code{bigrquery::bq_auth()} first.
#'
#' Billing project is resolved from \code{billing_project}, then the
#' \code{GCP_PROJECT} environment variable, then ADC discovery; if
#' none of those yields a project the call errors out with a clear
#' message before contacting BigQuery.
#'
#' @param sql A SQL string to execute.
#' @param billing_project Project to bill the query to. \code{NULL}
#'   falls back to the \code{GCP_PROJECT} env var, then to ADC.
#' @param page_size Rows per download page (forwarded to
#'   \code{\link[bigrquery]{bq_table_download}}).
#' @param max_rows Optional cap on rows downloaded (defaults to
#'   \code{Inf}, i.e. all rows).
#' @param quiet Suppress \pkg{bigrquery} progress output.
#' @return A base R \code{data.frame}.
#' @examples
#' \dontrun{
#' # Requires the 'bigrquery' package, ADC, and a billing project.
#' Sys.setenv(GCP_PROJECT = "my-billing-project")
#' df <- morie_ingest_bigquery_query(
#'   "SELECT year, COUNT(*) AS n
#'      FROM `bigquery-public-data.chicago_crime.crime`
#'     GROUP BY year
#'     ORDER BY year"
#' )
#' head(df)
#' }
#' @seealso \code{\link{morie_ingest_bigquery_table}},
#'   \code{\link{morie_ingest_bigquery_build_sql}}
#' @export
morie_ingest_bigquery_query <- function(sql,
                                        billing_project = NULL,
                                        page_size = 10000L,
                                        max_rows = Inf,
                                        quiet = TRUE) {
  if (!is.character(sql) || length(sql) != 1L || !nzchar(sql)) {
    stop("`sql` must be a single non-empty string.", call. = FALSE)
  }
  .morie_bq_require()
  billing <- .morie_bq_billing_project(billing_project)
  if (is.null(billing)) {
    stop(
      "BigQuery billing project is unset.\
  Pass `billing_project = ...` ",
      "or set the GCP_PROJECT environment variable\
  ",
      "(public datasets are billed to the caller's project, not the ",
      "dataset owner).",
      call. = FALSE
    )
  }

  tryCatch(
    {
      job <- bigrquery::bq_project_query(billing, sql)
      df <- bigrquery::bq_table_download(job,
        page_size = page_size,
        n_max = max_rows,
        quiet = isTRUE(quiet)
      )
      as.data.frame(df)
    },
    error = function(e) {
      stop(
        "morie_ingest_bigquery_query: query failed (billing=",
        billing, ")\
  SQL: ",
        substr(sql, 1L, 200L),
        if (nchar(sql) > 200L) " ..." else "",
        "\
  ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Pull a BigQuery table (or filtered slice) into a data.frame
#'
#' Convenience wrapper around
#' \code{\link{morie_ingest_bigquery_build_sql}} +
#' \code{\link{morie_ingest_bigquery_query}}: builds a validated,
#' backtick-quoted \code{SELECT} against a fully-qualified table and
#' downloads the result.
#'
#' @param project,dataset,table Fully-qualified BigQuery table
#'   reference, e.g. \code{project = "bigquery-public-data"},
#'   \code{dataset = "chicago_crime"}, \code{table = "crime"}.
#' @param where Optional raw SQL \code{WHERE} clause.
#' @param limit Optional \code{LIMIT}.
#' @param select Projection list (default \code{"*"}).
#' @param billing_project Billing project; falls back to
#'   \code{GCP_PROJECT}, then ADC.
#' @param page_size Rows per download page.
#' @param max_rows Optional cap on rows downloaded.
#' @param quiet Suppress \pkg{bigrquery} progress output.
#' @return A base R \code{data.frame}.
#' @examples
#' \dontrun{
#' # Requires the 'bigrquery' package, ADC, and a billing project.
#' df <- morie_ingest_bigquery_table(
#'   project = "bigquery-public-data",
#'   dataset = "chicago_crime",
#'   table   = "crime",
#'   where   = "year = 2024",
#'   limit   = 10000L,
#'   billing_project = "my-billing-project"
#' )
#' head(df)
#' }
#' @seealso \code{\link{morie_ingest_bigquery_query}}
#' @export
morie_ingest_bigquery_table <- function(project, dataset, table,
                                        where = NULL,
                                        limit = NULL,
                                        select = "*",
                                        billing_project = NULL,
                                        page_size = 10000L,
                                        max_rows = Inf,
                                        quiet = TRUE) {
  sql <- morie_ingest_bigquery_build_sql(
    project = project, dataset = dataset, table = table,
    where = where, limit = limit, select = select
  )
  ref <- paste(project, dataset, table, sep = ".")
  tryCatch(
    morie_ingest_bigquery_query(
      sql = sql,
      billing_project = billing_project,
      page_size = page_size,
      max_rows = max_rows,
      quiet = quiet
    ),
    error = function(e) {
      stop(
        "morie_ingest_bigquery_table: failed for table ", ref, "\
  ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}
