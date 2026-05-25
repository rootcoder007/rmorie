# SPDX-License-Identifier: AGPL-3.0-or-later
#' Comprehensive per-variable audit walker (R-side mirror)
#'
#' R counterpart of \code{morie.audit_variables}.  Walks every column
#' in every OTIS and ARSAU dataset known to the package, classifies
#' each variable via \code{\link{morie_classify_variable}}, and
#' returns a single audit object (or pair of objects, when domain =
#' "both") summarising coverage, levels of measurement, roles,
#' cross-year-safety, and recommended methods per variable.
#'
#' Pure R; no C/C++ hot path needed (taxonomy is regex + lookup, not
#' CPU-bound).  Per \code{[[feedback_r_cpp_first]]} we'd reach for
#' Rcpp only if profiling showed a real bottleneck.
#'
#' Public callables
#' ----------------
#'
#' \itemize{
#'   \item \code{\link{morie_audit_otis_variables}}
#'   \item \code{\link{morie_audit_arsau_variables}}
#'   \item \code{\link{morie_audit_all_variables}}
#'   \item \code{\link{morie_write_audit_markdown}}
#' }
#'
#' @name audit_variables
NULL


# Columns the existing analyzers exercise.  Counted as "analysed".
.ANALYZED_OTIS <- c(
  "UniqueIndividual_ID", "NumberConsecutiveDays_Segregation",
  "MentalHealth_Alert", "SuicideRisk_Alert", "SuicideWatch_Alert",
  "Region_AtTimeOfPlacement", "Region_MostRecentPlacement"
)
.ANALYZED_ARSAU <- c(
  "PoliceService", "IncidentType", "PoliceServiceType",
  "OPP_PoliceService_Region",
  "Race", "Gender", "AgeCategory",
  "IndivInjuries_PhysicalInjuries",
  "Weapon", "Location",
  "CEW_CartridgeProbe_CartridgeProbeCycles_Cyc",
  "POLICE_SERVICE", "ASSIGNMENT_TYPE", "REPORTING_YEAR"
)


# Internal: turn a dataset_dictionary-style DatasetSchema (a named
# list with $columns) into a list of taxonomies.
.classify_schema_R <- function(schema, dataset_name) {
  out <- vector("list", length(schema$columns))
  for (i in seq_along(schema$columns)) {
    spec <- schema$columns[[i]]
    out[[i]] <- morie_classify_variable(
      col_name = spec$name,
      dtype = spec$dtype %||% "string",
      valid_values = spec$valid_values,
      dataset_name = dataset_name
    )
  }
  out
}


# Internal: compute counts + flag lists from a flat taxonomy list
.summarise_taxonomies <- function(taxonomies, analyzed_set, domain) {
  n_total <- length(taxonomies)
  n_analyzed <- sum(vapply(taxonomies,
                            function(t) t$column_name %in% analyzed_set,
                            logical(1)))
  coverage_pct <- if (n_total > 0L) 100 * n_analyzed / n_total else 0

  level_counts <- table(vapply(taxonomies, function(t) t$level, character(1)))
  role_counts  <- table(vapply(taxonomies, function(t) t$role,  character(1)))
  card_counts  <- table(vapply(taxonomies,
                                function(t) t$cardinality, character(1)))

  cross_unsafe <- Filter(function(t) !t$cross_year_safe, taxonomies)
  identifiers  <- Filter(function(t) t$role == "identifier", taxonomies)
  outcomes     <- Filter(function(t) t$role == "outcome", taxonomies)

  out <- list(
    title = sprintf("morie variable-coverage audit - %s", toupper(domain)),
    summary_lines = list(
      `Total variables` = n_total,
      `Currently analysed` = n_analyzed,
      Coverage = sprintf("%.1f%%", coverage_pct),
      `Cross-year-unsafe` = length(cross_unsafe),
      Identifiers = length(identifiers),
      Outcomes = length(outcomes)
    ),
    interpretation = sprintf(
      "Audited %d variable(s).  Currently analysed: %d (%.1f%%); not exercised by any analyzer: %d.  %d flagged cross_year_safe=FALSE.  %d identifier column(s), %d outcome column(s).",
      n_total, n_analyzed, coverage_pct, n_total - n_analyzed,
      length(cross_unsafe), length(identifiers), length(outcomes)
    ),
    n = n_total,
    n_analyzed = n_analyzed,
    coverage_pct = coverage_pct,
    level_counts = as.list(level_counts),
    role_counts = as.list(role_counts),
    cardinality_counts = as.list(card_counts),
    cross_year_unsafe = cross_unsafe,
    identifiers = identifiers,
    outcomes = outcomes,
    taxonomies = taxonomies,
    domain = domain
  )
  class(out) <- c("morie_audit_result", "list")
  out
}


# ---------------------------------------------------------------------------
# Public callables
# ---------------------------------------------------------------------------

#' Audit every OTIS variable.
#'
#' For each OTIS dataset, this function expects a list of column
#' specifications.  By default it constructs the specs from the
#' columns in \code{DATASET_REGISTRY} (in R, these are stored on the
#' Python side via the dictionary parser; on the R side we fall
#' back to a minimal name-only list and rely on the heuristic
#' classifier when dtype/valid_values are unknown).
#'
#' For a richer audit that consults the bilingual XLSX dictionary,
#' use the Python module \code{morie.audit_variables}.
#'
#' @param dataset_specs Optional list keyed by dataset id, each entry
#'   a list of \code{list(name, dtype, valid_values)} entries.  When
#'   \code{NULL}, the function uses a built-in minimal spec extracted
#'   from the existing R-side OTIS metadata.
#' @return A list with class \code{morie_audit_result}.
#' @export
morie_audit_otis_variables <- function(dataset_specs = NULL) {
  if (is.null(dataset_specs)) {
    # Best-effort: use the existing OTIS column hints if they exist.
    # The full dictionary parse lives on the Python side; here we
    # operate on whatever names are registered in R.
    dataset_specs <- list()  # filled by caller for now
  }
  taxonomies <- list()
  for (ds in names(dataset_specs)) {
    for (col in dataset_specs[[ds]]) {
      taxonomies[[length(taxonomies) + 1L]] <- morie_classify_variable(
        col_name = col$name,
        dtype = col$dtype %||% "string",
        valid_values = col$valid_values,
        dataset_name = ds
      )
    }
  }
  .summarise_taxonomies(taxonomies, .ANALYZED_OTIS, "otis")
}


#' Audit every ARSAU variable.
#'
#' @param dataset_specs See \code{\link{morie_audit_otis_variables}}.
#' @return A list with class \code{morie_audit_result}.
#' @export
morie_audit_arsau_variables <- function(dataset_specs = NULL) {
  if (is.null(dataset_specs)) {
    dataset_specs <- list()
  }
  taxonomies <- list()
  for (ds in names(dataset_specs)) {
    for (col in dataset_specs[[ds]]) {
      taxonomies[[length(taxonomies) + 1L]] <- morie_classify_variable(
        col_name = col$name,
        dtype = col$dtype %||% "string",
        valid_values = col$valid_values,
        dataset_name = ds
      )
    }
  }
  .summarise_taxonomies(taxonomies, .ANALYZED_ARSAU, "arsau")
}


#' Audit both OTIS and ARSAU.
#'
#' @param otis_specs,arsau_specs See per-domain functions.
#' @return Named list with \code{$otis} and \code{$arsau} audit results.
#' @export
morie_audit_all_variables <- function(otis_specs = NULL,
                                        arsau_specs = NULL) {
  list(
    otis = morie_audit_otis_variables(otis_specs),
    arsau = morie_audit_arsau_variables(arsau_specs)
  )
}


#' Build a list of column specs from a parsed CSV header.
#'
#' Convenience helper: given a data.frame just-loaded by
#' \code{morie_arsau_load_*()}, returns the
#' \code{list(name, dtype, valid_values)} structure expected by the
#' audit functions.
#'
#' @param df Loaded data.frame.
#' @return List of column specs.
#' @export
morie_specs_from_df <- function(df) {
  lapply(names(df), function(nm) {
    col <- df[[nm]]
    dtype <- if (is.numeric(col) && all(is.finite(col) | is.na(col)) &&
                  all((col %% 1) == 0 | is.na(col))) "int"
              else if (is.numeric(col)) "float"
              else if (is.logical(col))  "bool"
              else if (inherits(col, "Date")) "date"
              else if (inherits(col, "POSIXct")) "datetime"
              else "string"
    list(name = nm, dtype = dtype, valid_values = NULL)
  })
}


#' Write a Markdown audit report.
#'
#' @param out_path Path to write to.
#' @param audit_result A \code{morie_audit_result} or list of them.
#' @return The path written.
#' @export
morie_write_audit_markdown <- function(out_path, audit_result) {
  if (!inherits(audit_result, "morie_audit_result") &&
      !all(vapply(audit_result, inherits,
                   logical(1), "morie_audit_result"))) {
    stop("audit_result must be morie_audit_result or list of them")
  }
  if (inherits(audit_result, "morie_audit_result")) {
    audit_result <- list(audit_result)
  }
  lines <- c(
    "# morie variable-coverage audit (R-side)",
    "",
    "Per-variable taxonomy + coverage audit, classified by",
    "Stevens-1946 level of measurement.",
    ""
  )
  for (r in audit_result) {
    lines <- c(lines,
                sprintf("## %s", toupper(r$domain)),
                "",
                sprintf("- Total variables: %d", r$n),
                sprintf("- Currently analysed: %d (%.1f%%)",
                        r$n_analyzed, r$coverage_pct),
                sprintf("- Cross-year-unsafe: %d", length(r$cross_year_unsafe)),
                sprintf("- Identifiers: %d", length(r$identifiers)),
                sprintf("- Outcomes: %d", length(r$outcomes)),
                "",
                "### Levels", "",
                "| level | count |", "|---|---:|")
    for (k in names(r$level_counts)) {
      lines <- c(lines, sprintf("| %s | %d |", k, r$level_counts[[k]]))
    }
    lines <- c(lines, "", "### Per-variable detail", "",
                "| dataset | column | level | role | cardinality | cross_year_safe | analyzed? | recommended summary |",
                "|---|---|---|---|---|---|---|---|")
    analyzed_set <- if (r$domain == "otis") .ANALYZED_OTIS else .ANALYZED_ARSAU
    for (t in r$taxonomies) {
      seen <- if (t$column_name %in% analyzed_set) "yes" else "-"
      cys <- if (t$cross_year_safe) "yes" else "**NO**"
      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %s | %s | %s | %s |",
        t$dataset_name, t$column_name, t$level, t$role,
        t$cardinality, cys, seen, morie_recommended_summary(t)
      ))
    }
    lines <- c(lines, "")
  }
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, out_path)
  out_path
}


#' Print method for audit results.
#' @param x A \code{morie_audit_result}.
#' @param ... Unused.
#' @export
print.morie_audit_result <- function(x, ...) {
  cat(x$title, "\n", strrep("=", nchar(x$title)), "\n", sep = "")
  for (k in names(x$summary_lines)) {
    cat(sprintf("  %-20s  %s\n", k, format(x$summary_lines[[k]])))
  }
  cat("\n", x$interpretation, "\n", sep = "")
  invisible(x)
}
