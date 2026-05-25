# SPDX-License-Identifier: AGPL-3.0-or-later
#' Per-record-type ARSAU analysis pipelines (R-side)
#'
#' Six analyzers that load one ARSAU dataset via the loaders in
#' \code{R/arsau.R} and chain the generic MRM Use-of-Force callables
#' from \code{R/mrm_uof.R}, producing a single named list with
#' multi-paragraph interpretation, the loaded data, all sub-analyses,
#' and the source sidecar (if present).
#'
#' Functions
#' ---------
#'
#' \itemize{
#'   \item \code{\link{morie_arsau_analyze_main_records}}
#'   \item \code{\link{morie_arsau_analyze_individual_records}}
#'   \item \code{\link{morie_arsau_analyze_probe_cycle_records}}
#'   \item \code{\link{morie_arsau_analyze_weapon_records}}
#'   \item \code{\link{morie_arsau_analyze_aggregate_summary}}
#'   \item \code{\link{morie_arsau_analyze_detailed_dataset}}
#' }
#'
#' Each analyzer accepts the same \code{year} / \code{language} /
#' \code{data_dir} arguments as the matching loader, and returns a
#' named list whose constituent sub-results are available under named
#' keys (\code{force_concentration}, \code{disparity_by_race}, etc.).
#'
#' @name mrm_arsau
NULL


# ---------------------------------------------------------------------------
# Column-name constants
# ---------------------------------------------------------------------------

.MAIN_FORCE_COL          <- "PoliceService"
.MAIN_INCIDENT_TYPE_COL  <- "IncidentType"
.MAIN_REGION_COL         <- "OPP_PoliceService_Region"

.INDIV_RACE_COL          <- "Race"
.INDIV_GENDER_COL        <- "Gender"
.INDIV_AGE_COL           <- "AgeCategory"
.INDIV_OUTCOME_COL       <- "IndivInjuries_PhysicalInjuries"

.WEAPON_WEAPON_COL       <- "Weapon"
.WEAPON_LOCATION_COL     <- "Location"

.PROBE_CYCLE_COL         <- "CEW_CartridgeProbe_CartridgeProbeCycles_Cyc"

.AGG_SECTION_COL         <- "SECTION"
.AGG_YEAR_PREFIX         <- "YEAR_"


# ---------------------------------------------------------------------------
# Internal: result wrapper
# ---------------------------------------------------------------------------

.arsau_wrap <- function(title, call, sub_results, data, sidecar,
                         year_or_range, kind, language, is_valid,
                         extra_interpretation = "") {
  warnings <- character(0)
  if (!is_valid) {
    warnings <- c(warnings,
      "Source dataset flagged invalid by the publishing ministry; results presented for data-quality review only.")
  }
  for (nm in names(sub_results)) {
    for (w in sub_results[[nm]]$warnings) {
      warnings <- c(warnings, sprintf("[%s] %s", nm, w))
    }
  }

  base_interp <- sprintf(
    "Ran %d sub-analysis(es) over the ARSAU %s dataset for %s: %s.",
    length(sub_results), kind, year_or_range, paste(names(sub_results), collapse = ", ")
  )

  interp <- trimws(paste(base_interp, extra_interpretation, sep = " "))

  out <- c(
    list(
      title = title,
      call = call,
      summary_lines = list(
        `Year/range` = year_or_range,
        Kind = kind,
        `Rows analysed` = nrow(data),
        `Columns analysed` = ncol(data),
        Valid = if (is_valid) "yes" else "no",
        `Sub-analyses` = length(sub_results)
      ),
      warnings = warnings,
      interpretation = interp,
      data = data,
      sidecar = sidecar,
      year_or_range = year_or_range,
      kind = kind,
      language = language,
      is_valid = is_valid,
      n_rows = nrow(data),
      n_cols = ncol(data)
    ),
    sub_results
  )
  class(out) <- c("morie_arsau_analysis_result", "morie_rich_result", "list")
  out
}


# ---------------------------------------------------------------------------
# 1. main_records
# ---------------------------------------------------------------------------

#' End-to-end analysis of the ARSAU main_records CSV for one year.
#'
#' Chains \code{mrm_uof_force_concentration} (PoliceService),
#' \code{mrm_uof_weapon_diversity} (IncidentType x PoliceService), and
#' \code{mrm_uof_data_quality_audit} (against the CKAN sidecar).
#'
#' @param year 2023 or 2024.
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @export
morie_arsau_analyze_main_records <- function(year, language = "en", data_dir = NULL) {
  loaded <- morie_arsau_load_main_records(year, language = language, data_dir = data_dir)
  df <- loaded$data
  sub <- list()
  if (.MAIN_FORCE_COL %in% names(df)) {
    sub$force_concentration <- mrm_uof_force_concentration(df, .MAIN_FORCE_COL)
  }
  if (.MAIN_INCIDENT_TYPE_COL %in% names(df) && .MAIN_FORCE_COL %in% names(df)) {
    sub$incident_type_x_force <- mrm_uof_weapon_diversity(
      df, .MAIN_INCIDENT_TYPE_COL, .MAIN_FORCE_COL
    )
  }
  sub$data_quality <- mrm_uof_data_quality_audit(df, sidecar = loaded$sidecar)
  .arsau_wrap(
    sprintf("ARSAU main_records analysis (%s)", loaded$year),
    sprintf("morie_arsau_analyze_main_records(year=%s)", sQuote(year)),
    sub, df, loaded$sidecar, loaded$year, "main_records", language, loaded$is_valid
  )
}


# ---------------------------------------------------------------------------
# 2. individual_records
# ---------------------------------------------------------------------------

#' Analysis of ARSAU individual_records.
#'
#' Chains demographic disparity by Race, Gender, AgeCategory against
#' the IndivInjuries_PhysicalInjuries outcome (Yes/No coerced).
#' Tolerates the 2023 trailing-space typo in the outcome column.
#'
#' @inheritParams morie_arsau_analyze_main_records
#' @param bootstrap_reps Forwarded to
#'   \code{mrm_uof_demographic_disparity}.
#' @export
morie_arsau_analyze_individual_records <- function(year, language = "en",
                                                     data_dir = NULL,
                                                     bootstrap_reps = 0L) {
  loaded <- morie_arsau_load_individual_records(year, language = language, data_dir = data_dir)
  df <- loaded$data
  sub <- list()

  # Locate outcome col case-insensitively + whitespace-tolerant.
  match_idx <- which(tolower(trimws(names(df))) == tolower(.INDIV_OUTCOME_COL))
  outcome_actual <- if (length(match_idx) > 0L) names(df)[match_idx[1]] else NULL

  if (is.null(outcome_actual)) {
    sub$data_quality <- mrm_uof_data_quality_audit(df, sidecar = loaded$sidecar)
    return(.arsau_wrap(
      sprintf("ARSAU individual_records analysis (%s)", loaded$year),
      sprintf("morie_arsau_analyze_individual_records(year=%s)", sQuote(year)),
      sub, df, loaded$sidecar, loaded$year, "individual_records",
      language, loaded$is_valid,
      extra_interpretation = sprintf(
        "Disparity analysis skipped: outcome column '%s' not found.",
        .INDIV_OUTCOME_COL
      )
    ))
  }

  # Coerce outcome strings to 0/1 robustly.
  os <- tolower(trimws(as.character(df[[outcome_actual]])))
  keep <- os %in% c("yes", "no", "true", "false", "0", "1")
  work <- df[keep, , drop = FALSE]
  work$._outcome <- ifelse(os[keep] %in% c("yes", "true", "1"), 1L, 0L)

  for (cfg in list(
    list(col = .INDIV_RACE_COL,   name = "disparity_by_race"),
    list(col = .INDIV_GENDER_COL, name = "disparity_by_gender"),
    list(col = .INDIV_AGE_COL,    name = "disparity_by_age")
  )) {
    if (cfg$col %in% names(work)) {
      sub[[cfg$name]] <- mrm_uof_demographic_disparity(
        work, cfg$col, "._outcome", bootstrap_reps = bootstrap_reps
      )
    }
  }
  sub$data_quality <- mrm_uof_data_quality_audit(df, sidecar = loaded$sidecar)

  .arsau_wrap(
    sprintf("ARSAU individual_records analysis (%s)", loaded$year),
    sprintf("morie_arsau_analyze_individual_records(year=%s)", sQuote(year)),
    sub, df, loaded$sidecar, loaded$year, "individual_records",
    language, loaded$is_valid,
    extra_interpretation = sprintf(
      "Outcome variable is %s (coerced Yes/No to 1/0). Baseline = largest-N group.",
      sQuote(outcome_actual)
    )
  )
}


# ---------------------------------------------------------------------------
# 3. probe_cycle_records
# ---------------------------------------------------------------------------

#' Analysis of ARSAU probe_cycle_records (CEW telemetry).
#'
#' @inheritParams morie_arsau_analyze_main_records
#' @export
morie_arsau_analyze_probe_cycle_records <- function(year, language = "en",
                                                      data_dir = NULL) {
  loaded <- morie_arsau_load_probe_cycle_records(year, language = language, data_dir = data_dir)
  df <- loaded$data
  sub <- list()

  if (.PROBE_CYCLE_COL %in% names(df)) {
    raw <- ifelse(is.na(df[[.PROBE_CYCLE_COL]]), "", as.character(df[[.PROBE_CYCLE_COL]]))
    cycle_counts <- vapply(strsplit(raw, ","), function(parts) {
      sum(nzchar(trimws(parts)))
    }, integer(1))
    descriptive <- list(
      n_rows = nrow(df),
      n_with_cycles = sum(cycle_counts > 0L),
      mean_cycles = if (length(cycle_counts) > 0L) mean(cycle_counts) else NA_real_,
      median_cycles = if (length(cycle_counts) > 0L) stats::median(cycle_counts) else NA_real_,
      max_cycles = if (length(cycle_counts) > 0L) max(cycle_counts) else 0L
    )
    res <- list(
      title = "CEW cycle-count distribution",
      call = sprintf("(internal) cycle parse of %s", sQuote(.PROBE_CYCLE_COL)),
      summary_lines = descriptive,
      warnings = character(0),
      interpretation = sprintf(
        "Across %d probe-cycle row(s), mean number of cycles per row is %.2f (median %.2f, max %d).",
        descriptive$n_rows, descriptive$mean_cycles, descriptive$median_cycles,
        as.integer(descriptive$max_cycles)
      )
    )
    res <- c(res, descriptive)
    class(res) <- c("morie_mrm_uof_result", "morie_rich_result", "list")
    sub$cycle_distribution <- res
  }

  sub$data_quality <- mrm_uof_data_quality_audit(df, sidecar = loaded$sidecar)
  .arsau_wrap(
    sprintf("ARSAU probe_cycle_records analysis (%s)", loaded$year),
    sprintf("morie_arsau_analyze_probe_cycle_records(year=%s)", sQuote(year)),
    sub, df, loaded$sidecar, loaded$year, "probe_cycle_records",
    language, loaded$is_valid
  )
}


# ---------------------------------------------------------------------------
# 4. weapon_records
# ---------------------------------------------------------------------------

#' Analysis of ARSAU weapon_records.
#'
#' Chains Weapon x Location chi-square + weapon frequency table + DQ
#' audit.  2023 needs \code{allow_invalid = TRUE}.
#'
#' @inheritParams morie_arsau_analyze_main_records
#' @param allow_invalid See \code{\link{morie_arsau_load_weapon_records}}.
#' @export
morie_arsau_analyze_weapon_records <- function(year, allow_invalid = FALSE,
                                                 language = "en", data_dir = NULL) {
  loaded <- morie_arsau_load_weapon_records(year, allow_invalid = allow_invalid,
                                              language = language, data_dir = data_dir)
  df <- loaded$data
  sub <- list()

  if (.WEAPON_WEAPON_COL %in% names(df) && .WEAPON_LOCATION_COL %in% names(df)) {
    sub$weapon_x_location <- mrm_uof_weapon_diversity(
      df, .WEAPON_WEAPON_COL, .WEAPON_LOCATION_COL
    )
  }
  if (.WEAPON_WEAPON_COL %in% names(df)) {
    wc <- sort(table(df[[.WEAPON_WEAPON_COL]]), decreasing = TRUE)
    total <- sum(wc)
    rows <- lapply(seq_along(wc), function(i) {
      list(
        weapon = names(wc)[i],
        n = as.integer(wc[i]),
        share = as.numeric(wc[i]) / total
      )
    })
    res <- list(
      title = "Weapon frequency distribution",
      call = sprintf("(internal) table(df[[%s]])", sQuote(.WEAPON_WEAPON_COL)),
      summary_lines = list(
        `Distinct weapons` = length(wc),
        `Total weapon rows` = as.integer(total),
        `Top weapon` = if (length(wc) > 0L) names(wc)[1] else "-",
        `Top weapon share` = if (length(wc) > 0L) as.numeric(wc[1]) / total else 0
      ),
      warnings = character(0),
      interpretation = sprintf(
        "%d distinct weapon type(s) recorded across %d weapon-row(s).",
        length(wc), as.integer(total)
      ),
      n_distinct = length(wc),
      n_total = as.integer(total),
      rows = rows
    )
    class(res) <- c("morie_mrm_uof_result", "morie_rich_result", "list")
    sub$weapon_frequencies <- res
  }
  sub$data_quality <- mrm_uof_data_quality_audit(df, sidecar = loaded$sidecar)

  extra <- if (!loaded$is_valid) {
    "These results are computed for data-quality review only \u2014 the underlying file is the ministry-flagged invalid 2023 release."
  } else ""

  .arsau_wrap(
    sprintf("ARSAU weapon_records analysis (%s)", loaded$year),
    sprintf("morie_arsau_analyze_weapon_records(year=%s, allow_invalid=%s)",
            sQuote(year), allow_invalid),
    sub, df, loaded$sidecar, loaded$year, "weapon_records",
    language, loaded$is_valid, extra_interpretation = extra
  )
}


# ---------------------------------------------------------------------------
# 5. aggregate_summary
# ---------------------------------------------------------------------------

#' Analysis of the ARSAU aggregate-summary-by-year file (2020-2022).
#'
#' Builds an implied YoY series from the YEAR_2020 / YEAR_2021 /
#' YEAR_2022 columns against the REPORT_SCOPE headline volume row.
#'
#' @inheritParams morie_arsau_load_aggregate_summary
#' @export
morie_arsau_analyze_aggregate_summary <- function(year_range = "2020-2022",
                                                    language = "en", data_dir = NULL) {
  loaded <- morie_arsau_load_aggregate_summary(year_range, language = language, data_dir = data_dir)
  df <- loaded$data
  sub <- list()

  year_cols <- grep(paste0("^", .AGG_YEAR_PREFIX), names(df), value = TRUE)
  if (length(year_cols) > 0L) {
    yrs <- sort(as.integer(gsub(.AGG_YEAR_PREFIX, "", year_cols)))
    mask <- if (.AGG_SECTION_COL %in% names(df)) {
      df[[.AGG_SECTION_COL]] == "REPORT_SCOPE"
    } else {
      rep(TRUE, nrow(df))
    }
    headline_idx <- which(mask)[1]
    if (!is.na(headline_idx)) {
      headline <- df[headline_idx, , drop = FALSE]
      dfs_by_year <- list()
      for (y in yrs) {
        col <- paste0(.AGG_YEAR_PREFIX, y)
        val <- if (col %in% names(headline)) headline[[col]][1] else 0
        cnt <- suppressWarnings(as.integer(val))
        if (is.na(cnt)) cnt <- 0L
        dfs_by_year[[as.character(y)]] <- if (cnt > 0L) {
          data.frame(row = seq_len(cnt))
        } else {
          data.frame()
        }
      }
      sub$yoy_change_headline <- mrm_uof_yoy_change(dfs_by_year = dfs_by_year)
    }
  }
  sub$data_quality <- mrm_uof_data_quality_audit(df, sidecar = loaded$sidecar)

  .arsau_wrap(
    sprintf("ARSAU aggregate_summary analysis (%s)", loaded$year),
    sprintf("morie_arsau_analyze_aggregate_summary(year_range=%s)", sQuote(year_range)),
    sub, df, loaded$sidecar, loaded$year, "aggregate_summary",
    language, loaded$is_valid,
    extra_interpretation = "YoY computed against the '1 to 3 Subjects - Individual Reports' REPORT_SCOPE headline row."
  )
}


# ---------------------------------------------------------------------------
# 6. detailed_dataset
# ---------------------------------------------------------------------------

#' Wide-format analysis of the 2020-2022 detailed-incident dataset.
#'
#' @inheritParams morie_arsau_load_detailed_dataset
#' @export
morie_arsau_analyze_detailed_dataset <- function(year_range = "2020-2022",
                                                   language = "en", data_dir = NULL) {
  loaded <- morie_arsau_load_detailed_dataset(year_range, language = language, data_dir = data_dir)
  df <- loaded$data
  sub <- list()

  force_col <- if ("POLICE_SERVICE" %in% names(df)) "POLICE_SERVICE" else NULL
  year_col <- if ("REPORTING_YEAR" %in% names(df)) "REPORTING_YEAR" else NULL
  assignment_col <- if ("ASSIGNMENT_TYPE" %in% names(df)) "ASSIGNMENT_TYPE" else NULL

  if (!is.null(force_col)) {
    sub$force_concentration <- mrm_uof_force_concentration(df, force_col)
  }
  if (!is.null(force_col) && !is.null(assignment_col)) {
    sub$assignment_x_force <- mrm_uof_weapon_diversity(df, assignment_col, force_col)
  }
  if (!is.null(year_col)) {
    sub$yoy_change <- mrm_uof_yoy_change(df = df, year_col = year_col)
  }
  sub$data_quality <- mrm_uof_data_quality_audit(df, sidecar = loaded$sidecar)

  .arsau_wrap(
    sprintf("ARSAU detailed_dataset analysis (%s)", loaded$year),
    sprintf("morie_arsau_analyze_detailed_dataset(year_range=%s)", sQuote(year_range)),
    sub, df, loaded$sidecar, loaded$year, "detailed_dataset",
    language, loaded$is_valid
  )
}


#' @export
print.morie_arsau_analysis_result <- function(x, ...) {
  cat(x$title, "\n", strrep("=", nchar(x$title)), "\n", sep = "")
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      cat(sprintf("  %-*s  %s\n", label_w, nms[i], format(x$summary_lines[[i]])))
    }
    cat("\n")
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\n")
    cat("\n")
  }
  cat(x$interpretation, "\n")
  invisible(x)
}
