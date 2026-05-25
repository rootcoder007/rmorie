# SPDX-License-Identifier: AGPL-3.0-or-later
#' Per-variable taxonomy + dispatcher (R mirror of morie.variable_taxonomy)
#'
#' Classifies every column in OTIS / ARSAU datasets by Stevens-1946
#' level of measurement (nominal / ordinal / interval / ratio + the
#' practical extensions boolean / date / datetime / identifier /
#' free-text), cardinality, functional role (identifier / outcome /
#' covariate / weight / metadata), and cross-year safety.
#'
#' Drives a method dispatcher (\code{morie_recommended_summary},
#' \code{morie_recommended_pair_test}) that picks the right
#' statistical analysis per variable based on its measurement level.
#'
#' Hard-coded invariant overrides (the data dictionary itself states
#' these, but we encode them in code so analyses cannot accidentally
#' violate them):
#'
#' \itemize{
#'   \item OTIS \code{UniqueIndividual_ID}: random per-fiscal-year
#'         reassignment -> \code{cross_year_safe = FALSE},
#'         \code{role = "identifier"}.  Cross-year joins on this
#'         column are statistically meaningless.
#'   \item ARSAU \code{BatchFileName} / \code{Indiv_Index}: per-
#'         incident identifiers -> \code{role = "identifier"}.
#'   \item ARSAU \code{IndivInjuries_PhysicalInjuries}: boolean
#'         injury outcome -> \code{role = "outcome"}.
#' }
#'
#' @references
#' Stevens, S.S. (1946) "On the theory of scales of measurement."
#' \emph{Science}, 103(2684), 677-680.
#'
#' Velleman, P.F. and Wilkinson, L. (1993) "Nominal, ordinal,
#' interval, and ratio typologies are misleading."
#' \emph{The American Statistician}, 47(1), 65-72.
#'
#' @name variable_taxonomy
NULL


# ---------------------------------------------------------------------------
# Invariant override registry
# ---------------------------------------------------------------------------

# Keyed by (dataset_prefix, column_name); dataset_prefix matches via
# startsWith() so all OTIS series share the same UniqueIndividual_ID
# entry.
.MORIE_INVARIANT_OVERRIDES <- list(
  # OTIS — random per-fiscal-year ID reassignment
  list(ds_prefix = "b01", col = "UniqueIndividual_ID",
       patch = list(cross_year_safe = FALSE, role = "identifier",
                     notes = "Random per-fiscal-year reassignment (OTIS dict).",
                     source = "override")),
  list(ds_prefix = "a01", col = "UniqueIndividual_ID",
       patch = list(cross_year_safe = FALSE, role = "identifier",
                     notes = "Random per-fiscal-year reassignment (OTIS dict).",
                     source = "override")),
  # ARSAU — per-incident identifiers
  list(ds_prefix = "uof_main_records", col = "BatchFileName",
       patch = list(role = "identifier",
                     notes = "Joins main<->individual<->weapon<->probe within year.",
                     source = "override")),
  list(ds_prefix = "uof_individual_records", col = "BatchFileName",
       patch = list(role = "identifier", source = "override")),
  list(ds_prefix = "uof_weapon_records", col = "BatchFileName",
       patch = list(role = "identifier", source = "override")),
  list(ds_prefix = "uof_probe_cycle_records", col = "BatchFileName",
       patch = list(role = "identifier", source = "override")),
  list(ds_prefix = "uof_individual_records", col = "Indiv_Index",
       patch = list(role = "identifier", source = "override")),
  # ARSAU — outcome
  list(ds_prefix = "uof_individual_records",
       col = "IndivInjuries_PhysicalInjuries",
       patch = list(role = "outcome",
                     notes = "Yes/No -> 1/0 boolean outcome for disparity analysis.",
                     source = "override"))
)

.override_for <- function(dataset_name, col_name) {
  ds_lc <- tolower(dataset_name)
  col_lc <- tolower(trimws(col_name))
  for (entry in .MORIE_INVARIANT_OVERRIDES) {
    if (startsWith(ds_lc, tolower(entry$ds_prefix)) &&
        col_lc == tolower(entry$col)) {
      return(entry$patch)
    }
  }
  NULL
}


# ---------------------------------------------------------------------------
# Regex / heuristic helpers
# ---------------------------------------------------------------------------

.RE_IDENTIFIER <- paste0(
  "^_?id$|^id_$|_id$|[a-z]id$|^uniqueindividual|^batchfile|",
  "^recordnum|^record_id$|^incidentnumber$|index$"
)
.RE_OUTCOME <- "injur|death|killed|incident_outcome|outcome|fatal|hospital|medical|treatment"
.RE_RATIO    <- "number|count|days|hours|minutes|seconds|cycles|placements|reports|amount|rate|score|n_"
.RE_ORDINAL  <- "category|level|severity|grade|tier|rank|order"
.BOOLEAN_SETS <- list(c("yes", "no"), c("true", "false"), c("y", "n"),
                       c("0", "1"), c("1", "0"))


.is_boolean_value_set <- function(vv) {
  if (is.null(vv) || length(vv) == 0L) return(FALSE)
  lc <- tolower(trimws(as.character(vv)))
  for (s in .BOOLEAN_SETS) {
    if (setequal(lc, s)) return(TRUE)
  }
  FALSE
}

.cardinality_from_vv <- function(vv) {
  if (is.null(vv) || length(vv) == 0L) return("unknown")
  n <- length(vv)
  if (n == 2L) return("binary")
  if (n <= 10L) return("discrete_low")
  if (n <= 100L) return("discrete_medium")
  "discrete_high"
}

.level_from_spec <- function(col_name, dtype, valid_values, dataset_name) {
  dtype <- tolower(dtype %||% "string")
  if (grepl(.RE_IDENTIFIER, col_name, ignore.case = TRUE)) return("identifier")
  if (dtype == "bool") return("boolean")
  if (dtype == "date") return("date")
  if (dtype == "datetime") return("datetime")
  if (!is.null(valid_values) && length(valid_values) > 0L) {
    if (.is_boolean_value_set(valid_values)) return("boolean")
    if (grepl(.RE_ORDINAL, col_name, ignore.case = TRUE)) return("ordinal")
    if (dtype == "string") return("nominal")
  }
  if (dtype == "int" && grepl(.RE_RATIO, col_name, ignore.case = TRUE)) {
    return("ratio")
  }
  if (dtype == "float") return("ratio")
  if (dtype == "int") {
    if (grepl(.RE_ORDINAL, col_name, ignore.case = TRUE)) return("ordinal")
    return("ratio")
  }
  "nominal"
}

.role_from_name <- function(col_name) {
  if (grepl(.RE_IDENTIFIER, col_name, ignore.case = TRUE)) return("identifier")
  if (grepl(.RE_OUTCOME,    col_name, ignore.case = TRUE)) return("outcome")
  "covariate"
}


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Classify one variable.
#'
#' @param col_name Character; the column name.
#' @param dtype Character; one of \code{int} / \code{float} /
#'   \code{string} / \code{date} / \code{datetime} / \code{bool}.
#' @param valid_values Optional character vector of closed-set values.
#' @param dataset_name Character; the owning dataset id (e.g.
#'   \code{"b01"} for OTIS, \code{"uof_main_records"} for ARSAU).
#' @return A named list with classes \code{morie_variable_taxonomy} /
#'   \code{list}.
#' @export
morie_classify_variable <- function(col_name, dtype = "string",
                                     valid_values = NULL,
                                     dataset_name = "unknown") {
  level <- .level_from_spec(col_name, dtype, valid_values, dataset_name)
  role  <- .role_from_name(col_name)
  card  <- .cardinality_from_vv(valid_values)
  if (level == "boolean" && card == "unknown") card <- "binary"
  if (level == "ratio"   && card == "unknown") card <- "continuous"
  if (level == "identifier") card <- "discrete_high"

  out <- list(
    dataset_name = dataset_name,
    column_name = col_name,
    level = level,
    cardinality = card,
    role = role,
    cross_year_safe = TRUE,
    dictionary_described = TRUE,
    valid_values = valid_values,
    nullable = TRUE,
    raw_dtype = dtype,
    notes = NULL,
    source = "dictionary"
  )

  # Apply overrides last
  patch <- .override_for(dataset_name, col_name)
  if (!is.null(patch)) {
    for (k in names(patch)) {
      out[[k]] <- patch[[k]]
    }
  }
  class(out) <- c("morie_variable_taxonomy", "list")
  out
}


#' Recommended summary statistic for a single variable.
#'
#' @param tax A \code{morie_variable_taxonomy}.
#' @return Character scalar — plain-language hint at which summary suits.
#' @export
morie_recommended_summary <- function(tax) {
  if (tax$role == "identifier" || tax$level == "identifier") {
    return("n distinct, top-5 frequencies (identifier - no stats)")
  }
  switch(tax$level,
    boolean    = "proportion + Wilson 95% CI",
    nominal    = "mode + frequency table + chi-square goodness-of-fit",
    ordinal    = "median + IQR + Mann-Whitney/Wilcoxon for pairwise",
    ratio      = "mean + SD + Gini if non-negative + log-scale histogram + Pareto Hill-MLE if heavy-tailed",
    interval   = "mean + SD + skew/kurtosis + linear regression",
    date       = "min/max date + temporal histogram + change-point if treated as time series",
    datetime   = "min/max date + temporal histogram + change-point if treated as time series",
    free_text  = "n distinct, length distribution (NLP otherwise)",
    "(unknown level - manual inspection)"
  )
}


#' Recommended bivariate test for a pair of variables.
#'
#' Looks up the (level_a, level_b) combination and returns the right
#' default test (Stevens-1946 hierarchy).
#'
#' @param tax_a,tax_b Two \code{morie_variable_taxonomy} objects.
#' @return Character scalar — recommended test name.
#' @export
morie_recommended_pair_test <- function(tax_a, tax_b) {
  a <- tax_a$level
  b <- tax_b$level
  if (a == "identifier" || b == "identifier") {
    return("(identifier - use for grouping, not for tests)")
  }
  if (a %in% c("nominal","boolean") && b %in% c("nominal","boolean")) {
    return("chi-square (or Fisher exact if small) + Cramer's V")
  }
  if (a == "ordinal" && b == "ordinal") {
    return("Spearman rho + Kendall tau")
  }
  if ((a == "nominal" && b %in% c("interval","ratio")) ||
      (b == "nominal" && a %in% c("interval","ratio"))) {
    return("Welch's ANOVA (or Kruskal-Wallis if non-normal)")
  }
  if ((a == "ordinal" && b %in% c("interval","ratio")) ||
      (b == "ordinal" && a %in% c("interval","ratio"))) {
    return("Spearman rho (rank-based) or polychoric correlation if cells are sufficient")
  }
  if (a %in% c("interval","ratio") && b %in% c("interval","ratio")) {
    return("Pearson r + linear regression (or Spearman if not normal)")
  }
  if (a == "date" || b == "date") {
    return("stratify by date bucket, then apply pair test on the rest")
  }
  "(unsupported pair - manual inspection)"
}


#' Print method for taxonomy entries.
#' @param x A \code{morie_variable_taxonomy} object.
#' @param ... Unused.
#' @export
print.morie_variable_taxonomy <- function(x, ...) {
  cat(sprintf("Variable taxonomy: %s :: %s\n", x$dataset_name, x$column_name))
  cat(sprintf("  level        : %s\n", x$level))
  cat(sprintf("  cardinality  : %s\n", x$cardinality))
  cat(sprintf("  role         : %s\n", x$role))
  cat(sprintf("  cross_year_safe : %s\n", x$cross_year_safe))
  if (!is.null(x$notes)) cat(sprintf("  notes        : %s\n", x$notes))
  invisible(x)
}
