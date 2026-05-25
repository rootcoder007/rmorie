# SPDX-License-Identifier: AGPL-3.0-or-later
#
# dataset.R -- dataset-agnostic profiling engine, ported from
# src/morie/dataset.py.  Walks an arbitrary data.frame, infers Stevens
# (1946) NOIR levels for each column, detects likely treatment /
# outcome / weight / id / stratum / cluster roles, and suggests an
# ordered analysis plan.
#
# Distinct from `dataset_profile.R` (which carries a minimal
# `morie_infer_measurement_level()`).  This file ships the full
# DatasetProfile / ColumnProfile record types and the suggest-plan
# helper, all under the `morie_dataset_*` prefix to avoid collision.

# ---------------------------------------------------------------------------
# Measurement-level vocabulary
# ---------------------------------------------------------------------------

#' Allowed Stevens (1946) measurement levels.
#' @keywords internal
#' @noRd
.MORIE_DATASET_LEVELS <- c("nominal", "ordinal", "interval", "ratio")

# ---------------------------------------------------------------------------
# Heuristic role-detection patterns (compiled once, case-insensitive)
# ---------------------------------------------------------------------------

.MORIE_DATASET_PATTERNS <- list(
  treatment = "(treat|cannabis|drug|alcohol|interven|expos|assign|smok|vaccin|medic)",
  outcome   = paste0(
    "(outcome|result|response|y_|_freq|_harm|_drink|disorder|death|mortal|",
    "surviv|event|diagnos|prevalence|incidence|hospitali|readmit|relapse)"
  ),
  weight    = "(weight|^wt$|^wt_|_wt$|^pw$|^pw_|_pw$|survey_wt|wtpumf|samp.*wt|ipw|iptw)",
  stratum   = "(strat|stratum|_strata$)",
  cluster   = "(cluster|psu|^clust_|_clust$)",
  id        = "(^id$|_id$|^id_|^index$|^rowid|^record|^uid$|^caseid)",
  interval  = "(year|index|score|temperature|temp_|_temp$|date|time|latitude|longitude)",
  ordinal   = paste0(
    "(likert|rank|grade|level|stage|scale|class|quartile|quintile|decile|",
    "tercile|rating|satisfaction|severity|frequency_cat|education|income_group|",
    "age_group|health)"
  )
)

#' Case-insensitive single-pattern search.
#' @keywords internal
#' @noRd
.morie_dataset_match <- function(name, key) {
  if (!nzchar(name %||% "")) return(FALSE)
  grepl(.MORIE_DATASET_PATTERNS[[key]], name, perl = TRUE, ignore.case = TRUE)
}

# ---------------------------------------------------------------------------
# Per-column measurement-level inference (NOIR)
# ---------------------------------------------------------------------------

#' Infer the Stevens NOIR measurement level for a single vector.
#'
#' Decision rules, in order:
#' 1. Character / factor with `n_unique <= ordinal_threshold` and an ordinal
#'    name hit (likert/grade/scale/...): `"ordinal"`.
#' 2. Character / factor otherwise: `"nominal"`.
#' 3. Logical: `"nominal"`.
#' 4. Numeric with `n_unique <= 2` (binary): `"nominal"`.
#' 5. Numeric with `n_unique <= 20` + ordinal name hit: `"ordinal"`.
#' 6. Double with interval name hit (year/index/date/...): `"interval"`.
#' 7. Double otherwise: `"ratio"`.
#' 8. Integer with non-negative range: `"ratio"`; else `"interval"`.
#' 9. Date / POSIXct: `"interval"`.
#'
#' @param x A vector (any atomic type or factor).
#' @param name Optional column name to drive the name-based heuristics.
#'   Defaults to `NULL` (no name-based promotion).
#' @param ordinal_threshold Integer; max unique values for a categorical
#'   column to be considered ordinal (default 10).
#' @return Character scalar; one of `"nominal"`, `"ordinal"`, `"interval"`,
#'   `"ratio"`.
#' @export
morie_dataset_infer_level <- function(x, name = NULL, ordinal_threshold = 10L) {
  nm <- as.character(name %||% "")
  non_null <- x[!is.na(x)]
  n_unique <- length(unique(non_null))

  if (is.character(x) || is.factor(x)) {
    if (n_unique <= ordinal_threshold && .morie_dataset_match(nm, "ordinal")) {
      return("ordinal")
    }
    return("nominal")
  }
  if (is.logical(x)) {
    return("nominal")
  }
  if (is.numeric(x)) {
    if (n_unique <= 2L) return("nominal")
    if (n_unique <= 20L && .morie_dataset_match(nm, "ordinal")) return("ordinal")
    if (is.double(x) && !is.integer(x)) {
      if (.morie_dataset_match(nm, "interval")) return("interval")
      return("ratio")
    }
    if (length(non_null) > 0L) {
      return(if (min(non_null) >= 0) "ratio" else "interval")
    }
    return("ratio")
  }
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return("interval")
  }
  "nominal"
}

# ---------------------------------------------------------------------------
# Per-column role detection
# ---------------------------------------------------------------------------

#' Detect the suggested epidemiological role of a column.
#'
#' @param x A vector.
#' @param name Column name (drives the heuristic patterns).
#' @return One of `"id"`, `"weight"`, `"stratum"`, `"cluster"`,
#'   `"treatment"`, `"outcome"`, `"covariate"`.
#' @export
morie_dataset_detect_role <- function(x, name) {
  nm <- as.character(name %||% "")
  non_null <- x[!is.na(x)]
  is_binary <- length(unique(non_null)) == 2L
  if (.morie_dataset_match(nm, "id"))      return("id")
  if (.morie_dataset_match(nm, "weight") && is.numeric(x)) return("weight")
  if (.morie_dataset_match(nm, "stratum")) return("stratum")
  if (.morie_dataset_match(nm, "cluster")) return("cluster")
  if (is_binary && .morie_dataset_match(nm, "treatment")) return("treatment")
  if (.morie_dataset_match(nm, "outcome")) return("outcome")
  if (.morie_dataset_match(nm, "treatment")) return("treatment")
  "covariate"
}

# ---------------------------------------------------------------------------
# Per-column summary statistics
# ---------------------------------------------------------------------------

#' Compute level-appropriate summary statistics for one column.
#'
#' Interval / ratio columns get mean/sd/min/q25/median/q75/max; nominal
#' / ordinal columns get a `top_counts` list of value -> count for the
#' top ten levels.
#'
#' @param x A vector.
#' @param level Inferred measurement level (one of nominal/ordinal/
#'   interval/ratio).
#' @return Named list of summary statistics.
#' @export
morie_dataset_summarize_column <- function(x, level) {
  non_null <- x[!is.na(x)]
  if (level %in% c("interval", "ratio") && is.numeric(non_null) && length(non_null) > 0L) {
    return(list(
      mean   = as.numeric(mean(non_null)),
      std    = as.numeric(stats::sd(non_null)),
      min    = as.numeric(min(non_null)),
      q25    = as.numeric(stats::quantile(non_null, 0.25, names = FALSE)),
      median = as.numeric(stats::median(non_null)),
      q75    = as.numeric(stats::quantile(non_null, 0.75, names = FALSE)),
      max    = as.numeric(max(non_null))
    ))
  }
  if (length(non_null) > 0L) {
    tab <- sort(table(non_null), decreasing = TRUE)
    top <- utils::head(tab, 10L)
    return(list(top_counts = as.list(as.integer(top)) |> stats::setNames(names(top))))
  }
  list(top_counts = list())
}

# ---------------------------------------------------------------------------
# Column-level + dataset-level profile records
# ---------------------------------------------------------------------------

#' Build a single-column profile record.
#'
#' @param series A vector.
#' @param name Column name.
#' @param ordinal_threshold Integer; passed to `morie_dataset_infer_level()`.
#' @param binary_threshold Integer; max unique values to count as binary
#'   (default 2).
#' @return Named list with fields `name`, `dtype`, `level`, `n_unique`,
#'   `missing_pct`, `is_binary`, `is_constant`, `suggested_role`,
#'   `summary_stats`.
#' @export
morie_dataset_column_profile <- function(series, name,
                                         ordinal_threshold = 10L,
                                         binary_threshold = 2L) {
  level <- morie_dataset_infer_level(series, name = name,
                                     ordinal_threshold = ordinal_threshold)
  role <- morie_dataset_detect_role(series, name)
  non_null <- series[!is.na(series)]
  n_unique <- length(unique(non_null))
  missing_pct <- 100 * mean(is.na(series))
  list(
    name = name,
    dtype = paste(class(series), collapse = "/"),
    level = level,
    n_unique = as.integer(n_unique),
    missing_pct = as.numeric(missing_pct),
    is_binary = isTRUE(n_unique == binary_threshold),
    is_constant = isTRUE(n_unique <= 1L),
    suggested_role = role,
    summary_stats = morie_dataset_summarize_column(series, level)
  )
}

#' Fully profile a data frame without prior schema knowledge.
#'
#' Walks every column, infers its NOIR level and epidemiological role,
#' computes summary statistics, and resolves a best-guess treatment,
#' outcome, and survey-weight column.  User-supplied hints override
#' heuristic detection.
#'
#' @param df A `data.frame`.
#' @param hint_treatment Optional character; force this column as the
#'   treatment.
#' @param hint_outcome Optional character; force this column as the
#'   outcome.
#' @param hint_weights Optional character; force this column as the
#'   survey weight.
#' @param ordinal_threshold Integer; max unique values for a categorical
#'   column to be classified as ordinal (default 10).
#' @param binary_threshold Integer; max unique values for a binary
#'   column (default 2).
#' @return A named list (the dataset profile) with fields `n_rows`,
#'   `n_cols`, `columns` (named list of column profiles),
#'   `suggested_treatment`, `suggested_outcome`, `suggested_weights`.
#' @export
morie_dataset_profile <- function(df,
                                  hint_treatment = NULL,
                                  hint_outcome = NULL,
                                  hint_weights = NULL,
                                  ordinal_threshold = 10L,
                                  binary_threshold = 2L) {
  if (!is.data.frame(df)) {
    stop(sprintf("Expected data.frame, got %s", paste(class(df), collapse = "/")))
  }
  if (nrow(df) == 0L || ncol(df) == 0L) {
    stop(sprintf(
      "data.frame must have at least one row and one column; got shape (%d, %d)",
      nrow(df), ncol(df)
    ))
  }

  columns <- list()
  treatment_candidates <- list()
  outcome_candidates <- list()
  weight_candidates <- character()

  for (col_name in colnames(df)) {
    cp <- morie_dataset_column_profile(
      df[[col_name]], col_name,
      ordinal_threshold = ordinal_threshold,
      binary_threshold = binary_threshold
    )
    columns[[col_name]] <- cp
    if (identical(cp$suggested_role, "treatment")) {
      score <- if (cp$is_binary) 2L else 1L
      treatment_candidates[[length(treatment_candidates) + 1L]] <- list(name = col_name, score = score)
    } else if (identical(cp$suggested_role, "outcome")) {
      score <- if (is.numeric(df[[col_name]])) 2L else 1L
      outcome_candidates[[length(outcome_candidates) + 1L]] <- list(name = col_name, score = score)
    } else if (identical(cp$suggested_role, "weight")) {
      weight_candidates <- c(weight_candidates, col_name)
    }
  }

  pick_best <- function(candidates) {
    if (length(candidates) == 0L) return(NULL)
    scores <- vapply(candidates, function(c) c$score, integer(1))
    candidates[[which.max(scores)]]$name
  }

  suggested_treatment <- hint_treatment %||% pick_best(treatment_candidates)
  suggested_outcome   <- hint_outcome   %||% pick_best(outcome_candidates)
  suggested_weights   <- hint_weights   %||% (if (length(weight_candidates)) weight_candidates[[1L]] else NULL)

  structure(
    list(
      n_rows = nrow(df),
      n_cols = ncol(df),
      columns = columns,
      suggested_treatment = suggested_treatment,
      suggested_outcome = suggested_outcome,
      suggested_weights = suggested_weights
    ),
    class = c("morie_dataset_profile", "list")
  )
}

#' Serialize a dataset profile to a plain nested list.
#'
#' Suitable for JSON / RDS round-trips.
#'
#' @param profile A `morie_dataset_profile` (output of
#'   `morie_dataset_profile()`).
#' @return Nested named list.
#' @export
morie_dataset_profile_to_list <- function(profile) {
  list(
    n_rows = profile$n_rows,
    n_cols = profile$n_cols,
    suggested_treatment = profile$suggested_treatment,
    suggested_outcome = profile$suggested_outcome,
    suggested_weights = profile$suggested_weights,
    columns = profile$columns
  )
}

#' Render a human-readable dataset profile summary table.
#'
#' Plain text only; no `rich` dependency.
#'
#' @param profile A `morie_dataset_profile`.
#' @return Character scalar with embedded newlines.
#' @export
morie_dataset_profile_summary_table <- function(profile) {
  header <- sprintf("Dataset Profile  (%d rows x %d cols)",
                    profile$n_rows, profile$n_cols)
  sep <- strrep("-", 100L)
  cols_header <- sprintf(
    "%-30s %-12s %-10s %7s %7s %7s %-15s",
    "Column", "dtype", "Level", "Unique", "Miss%", "Binary", "Role"
  )
  rows <- vapply(profile$columns, function(cp) {
    sprintf(
      "%-30s %-12s %-10s %7d %6.1f%% %7s %-15s",
      cp$name, cp$dtype, cp$level, cp$n_unique,
      cp$missing_pct, as.character(cp$is_binary), cp$suggested_role
    )
  }, character(1))
  paste(c(header, sep, cols_header, sep, rows), collapse = "\
")
}

# ---------------------------------------------------------------------------
# File loader
# ---------------------------------------------------------------------------

#' Load a dataset from a CSV / TSV / Excel / Parquet / JSON file.
#'
#' File format is detected from the extension.  Supported extensions:
#' `.csv`, `.tsv`, `.xlsx` / `.xls`, `.parquet` / `.pq`, `.json` /
#' `.jsonl`.
#'
#' @param path Character; file path.
#' @param encoding Character; encoding for text formats (default
#'   `"UTF-8"`).
#' @param ... Forwarded to the underlying reader
#'   (`utils::read.csv`, `readxl::read_excel`, etc.).
#' @return A `data.frame`.
#' @export
morie_dataset_load <- function(path, encoding = "UTF-8", ...) {
  if (!file.exists(path)) {
    stop(sprintf("Dataset file not found: %s", path))
  }
  suffix <- tolower(tools::file_ext(path))
  if (suffix == "csv") {
    return(utils::read.csv(path, fileEncoding = encoding, stringsAsFactors = FALSE, ...))
  }
  if (suffix == "tsv") {
    return(utils::read.delim(path, fileEncoding = encoding, stringsAsFactors = FALSE, ...))
  }
  if (suffix %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("morie_dataset_load: install 'readxl' to read Excel files.")
    }
    return(as.data.frame(readxl::read_excel(path, ...)))
  }
  if (suffix %in% c("parquet", "pq")) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("morie_dataset_load: install 'arrow' to read Parquet files.")
    }
    return(as.data.frame(arrow::read_parquet(path, ...)))
  }
  if (suffix == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("morie_dataset_load: install 'jsonlite' to read JSON files.")
    }
    return(jsonlite::fromJSON(path, simplifyDataFrame = TRUE, ...))
  }
  if (suffix == "jsonl") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("morie_dataset_load: install 'jsonlite' to read JSON-lines files.")
    }
    return(jsonlite::stream_in(file(path), verbose = FALSE, ...))
  }
  stop(sprintf(
    "Unsupported file extension: '%s'. Supported: .csv, .tsv, .xlsx, .xls, .parquet, .pq, .json, .jsonl",
    suffix
  ))
}

# ---------------------------------------------------------------------------
# Analysis plan suggestion
# ---------------------------------------------------------------------------

#' Suggest an ordered analysis plan based on a dataset profile.
#'
#' Uses the inferred measurement levels, binary indicators, and
#' detected treatment/outcome/weight columns to recommend
#' epidemiological analyses (descriptive profile, propensity scores,
#' IPW-ATE, AIPW, ATT/ATC, double-ML, GATE, survey-weighted estimates).
#'
#' @param profile A `morie_dataset_profile`.
#' @return A list of suggestion lists; each has `analysis`, `rationale`,
#'   and `required_vars`.
#' @export
morie_dataset_suggest_plan <- function(profile) {
  suggestions <- list()
  push <- function(s) suggestions[[length(suggestions) + 1L]] <<- s

  push(list(
    analysis = "descriptive_profile",
    rationale = "Summarize variable distributions, missingness, and sample size before any modelling.",
    required_vars = list(dataset = "all columns")
  ))

  treatment <- profile$suggested_treatment
  outcome <- profile$suggested_outcome
  weights <- profile$suggested_weights

  covariates <- vapply(
    names(profile$columns), function(nm) {
      cp <- profile$columns[[nm]]
      if (identical(cp$suggested_role, "covariate") && !isTRUE(cp$is_constant)) nm else NA_character_
    }, character(1)
  )
  covariates <- covariates[!is.na(covariates)]

  if (!is.null(treatment) && !is.null(outcome)) {
    treatment_cp <- profile$columns[[treatment]]
    outcome_cp <- profile$columns[[outcome]]

    if (!is.null(treatment_cp) && isTRUE(treatment_cp$is_binary) && length(covariates) > 0L) {
      push(list(
        analysis = "propensity_scores",
        rationale = sprintf(
          "Binary treatment '%s' detected with covariates. Estimate propensity scores for covariate balance.",
          treatment
        ),
        required_vars = list(treatment = treatment, covariates = covariates)
      ))
      push(list(
        analysis = "ipw_ate",
        rationale = sprintf(
          "Estimate ATE of '%s' on '%s' via inverse probability weighting.",
          treatment, outcome
        ),
        required_vars = list(treatment = treatment, outcome = outcome, covariates = covariates)
      ))
      push(list(
        analysis = "aipw",
        rationale = paste(
          "Doubly-robust ATE estimation (AIPW) combining propensity and",
          "outcome models for robustness to partial misspecification."
        ),
        required_vars = list(treatment = treatment, outcome = outcome, covariates = covariates)
      ))
      push(list(
        analysis = "att_atc",
        rationale = paste(
          "Estimate ATT and ATC to understand treatment effects on the",
          "treated and control populations separately."
        ),
        required_vars = list(treatment = treatment, outcome = outcome, covariates = covariates)
      ))
      if (!is.null(outcome_cp) && outcome_cp$level %in% c("interval", "ratio")) {
        push(list(
          analysis = "double_ml_plr",
          rationale = sprintf(
            paste(
              "Continuous outcome '%s' with binary treatment: apply Double ML (PLR)",
              "with cross-fitting for Neyman-orthogonal ATE."
            ),
            outcome
          ),
          required_vars = list(treatment = treatment, outcome = outcome, covariates = covariates)
        ))
      }
    }

    group_candidates <- vapply(
      names(profile$columns), function(nm) {
        cp <- profile$columns[[nm]]
        if (identical(cp$suggested_role, "covariate") &&
            cp$level %in% c("nominal", "ordinal") &&
            cp$n_unique > 2L && cp$n_unique <= 10L) nm else NA_character_
      }, character(1)
    )
    group_candidates <- group_candidates[!is.na(group_candidates)]
    if (length(group_candidates) > 0L && !is.null(treatment_cp) && isTRUE(treatment_cp$is_binary)) {
      push(list(
        analysis = "gate",
        rationale = sprintf(
          "Estimate Group Average Treatment Effects across %s to detect effect heterogeneity.",
          paste(utils::head(group_candidates, 3L), collapse = ", ")
        ),
        required_vars = list(
          treatment = treatment, outcome = outcome,
          group_cols = utils::head(group_candidates, 3L),
          covariates = covariates
        )
      ))
    }
  }

  if (!is.null(weights)) {
    push(list(
      analysis = "survey_weighted_estimates",
      rationale = sprintf(
        "Survey weight column '%s' detected. Apply design-based weighted estimation.",
        weights
      ),
      required_vars = list(weights = weights)
    ))
  }

  suggestions
}
