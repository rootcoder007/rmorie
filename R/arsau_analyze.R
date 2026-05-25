# SPDX-License-Identifier: AGPL-3.0-or-later
#' Per-record-type ARSAU analysis pipelines (R-side mirror of
#' \code{morie.arsau_analyze}).
#'
#' Each public callable in this file loads one ARSAU dataset via the
#' \code{morie_arsau_load_*} loaders defined in \code{R/arsau.R} and
#' chains the jurisdiction-agnostic MRM Use-of-Force primitives from
#' \code{R/mrm_uof.R} over it, returning a single named-list result
#' (classed \code{c("morie_arsau_result", "morie_rich_result", "list")})
#' that bundles the loaded data, every sub-analysis, and a
#' multi-paragraph natural-language interpretation.
#'
#' These analyzers do NOT invent new statistical methods.  They wire
#' the generic \code{mrm_uof_*} callables against the column names
#' that the Ontario open-data release actually publishes.  If the
#' upstream schema changes, the generic callables in
#' \code{R/mrm_uof.R} continue to work; only the column-name constants
#' below need patching.
#'
#' Public callables:
#'
#' \itemize{
#'   \item \code{\link{morie_arsau_analyze_main_records}}
#'         (per year, 2023 / 2024)
#'   \item \code{\link{morie_arsau_analyze_individual_records}}
#'         (per year, 2023 / 2024)
#'   \item \code{\link{morie_arsau_analyze_probe_cycle_records}}
#'         (per year, 2023 / 2024)
#'   \item \code{\link{morie_arsau_analyze_weapon_records}}
#'         (per year, 2023 / 2024; 2023 requires
#'         \code{allow_invalid = TRUE})
#'   \item \code{\link{morie_arsau_analyze_aggregate_summary}}
#'         (2020-2022)
#'   \item \code{\link{morie_arsau_analyze_detailed_dataset}}
#'         (2020-2022)
#' }
#'
#' Every analyzer returns a list whose named slots (\code{data},
#' \code{sidecar}, \code{force_concentration}, \code{data_quality},
#' \code{disparity_by_race}, ...) hold the constituent sub-results,
#' so callers can drill into individual tests without re-running the
#' full pipeline.
#'
#' @references
#' Ontario Ministry of the Solicitor General. \emph{Annual Report on
#' Special and Adaptive Units / Data on Police Use of Force in
#' Ontario}: 2020-2022, 2023, and 2024 releases.
#' \url{https://data.ontario.ca/dataset/police-use-of-force-race-based-data}.
#' Technical notes accompanying each annual release describe the
#' data-quality reasons for the 2023 weapon-records invalidity flag.
#'
#' @name arsau_analyze
NULL


# ---------------------------------------------------------------------------
# Column-name constants for the ARSAU schemas
# ---------------------------------------------------------------------------

.MORIE_ARSAU_MAIN_FORCE_COL          <- "PoliceService"
.MORIE_ARSAU_MAIN_FORCE_TYPE_COL     <- "PoliceServiceType"
.MORIE_ARSAU_MAIN_REGION_COL         <- "OPP_PoliceService_Region"
.MORIE_ARSAU_MAIN_INCIDENT_TYPE_COL  <- "IncidentType"
.MORIE_ARSAU_MAIN_LOCATION_PREFIX    <- "LocationType_"

.MORIE_ARSAU_INDIV_RACE_COL    <- "Race"
.MORIE_ARSAU_INDIV_GENDER_COL  <- "Gender"
.MORIE_ARSAU_INDIV_AGE_COL     <- "AgeCategory"
.MORIE_ARSAU_INDIV_OUTCOME_COL <- "IndivInjuries_PhysicalInjuries"
.MORIE_ARSAU_INDIV_KEY_COLS    <- c("BatchFileName", "Indiv_Index")

.MORIE_ARSAU_WEAPON_WEAPON_COL   <- "Weapon"
.MORIE_ARSAU_WEAPON_LOCATION_COL <- "Location"

.MORIE_ARSAU_PROBE_CYCLE_COL <- "CEW_CartridgeProbe_CartridgeProbeCycles_Cyc"

.MORIE_ARSAU_AGG_SECTION_COL  <- "SECTION"
.MORIE_ARSAU_AGG_CATEGORY_COL <- "CATEGORY"
.MORIE_ARSAU_AGG_UNITS_COL    <- "UNITS OF MEASURE"
.MORIE_ARSAU_AGG_YEAR_PREFIX  <- "YEAR_"


# ---------------------------------------------------------------------------
# Internal: locate a (possibly whitespace-corrupted) outcome column
# ---------------------------------------------------------------------------

.morie_arsau_locate_outcome_col <- function(df, target) {
  if (target %in% names(df)) return(target)
  trimmed <- tolower(trimws(names(df)))
  hit <- which(trimmed == tolower(target))
  if (length(hit) == 0L) return(NULL)
  names(df)[hit[1L]]
}


# ---------------------------------------------------------------------------
# Internal: wrap sub-results into a single morie_arsau_result list.
#
# Mirrors morie.arsau_analyze._wrap.  Hoists sub-result warnings up to
# the top so they appear in the wrapped result's printed banner.
# ---------------------------------------------------------------------------

.morie_arsau_wrap <- function(title, call, sub_results, data, sidecar,
                              year_or_range, kind, language, is_valid,
                              extra_interpretation = "") {
  warnings <- character(0)
  if (!isTRUE(is_valid)) {
    warnings <- c(
      warnings,
      paste0(
        "Source dataset flagged invalid by the publishing ministry \u2014 ",
        "results below are presented for data-quality review only."
      )
    )
  }
  if (length(sub_results) > 0L) {
    for (nm in names(sub_results)) {
      sub <- sub_results[[nm]]
      sub_ws <- sub$warnings
      if (length(sub_ws) > 0L) {
        warnings <- c(warnings, sprintf("[%s] %s", nm, sub_ws))
      }
    }
  }

  summary_lines <- list(
    `Year/range`       = year_or_range,
    Kind               = kind,
    `Rows analysed`    = if (is.null(data)) 0L else nrow(data),
    `Columns analysed` = if (is.null(data)) 0L else ncol(data),
    Valid              = if (isTRUE(is_valid)) "yes" else "no",
    `Sub-analyses`     = length(sub_results)
  )

  base_interp <- sprintf(
    paste0(
      "Ran %d sub-analysis(es) over the ARSAU '%s' dataset for '%s': ",
      "%s. Each sub-result is available as `result$<name>` and the ",
      "underlying data.frame as `result$data`."
    ),
    length(sub_results),
    kind,
    year_or_range,
    paste(names(sub_results), collapse = ", ")
  )
  interpretation <- trimws(paste(base_interp, extra_interpretation))

  payload <- list(
    title          = title,
    call           = call,
    summary_lines  = summary_lines,
    warnings       = warnings,
    interpretation = interpretation,
    data           = data,
    sidecar        = sidecar,
    year_or_range  = year_or_range,
    kind           = kind,
    language       = language,
    is_valid       = isTRUE(is_valid),
    n_rows         = if (is.null(data)) 0L else nrow(data),
    n_cols         = if (is.null(data)) 0L else ncol(data),
    value          = length(sub_results)
  )
  # Splice sub-results in by name so `result$force_concentration` works.
  for (nm in names(sub_results)) {
    payload[[nm]] <- sub_results[[nm]]
  }

  class(payload) <- c("morie_arsau_result", "morie_rich_result", "list")
  payload
}


# ---------------------------------------------------------------------------
# 1. main_records analysis
# ---------------------------------------------------------------------------

#' End-to-end analysis of the ARSAU main_records CSV for one year.
#'
#' Chains:
#' \itemize{
#'   \item \code{\link{mrm_uof_force_concentration}} over
#'         \code{PoliceService}
#'   \item \code{\link{mrm_uof_weapon_diversity}} over
#'         \code{IncidentType x PoliceService}
#'   \item \code{\link{mrm_uof_data_quality_audit}} against the
#'         published CKAN sidecar (when present)
#' }
#'
#' Region-locality is NOT meaningful for main_records -- only the
#' \code{OPP_PoliceService_Region} column is published, and it pairs
#' one column with itself.  See
#' \code{\link{morie_arsau_analyze_detailed_dataset}} for the
#' 2020-2022 layout that exposes more region columns.
#'
#' @param year 2023 or 2024.
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @return A list classed
#'   \code{c("morie_arsau_result", "morie_rich_result", "list")}.
#' @references Ontario Ministry of the Solicitor General, ARSAU 2023
#'   and 2024 main_records technical release notes.
#' @export
morie_arsau_analyze_main_records <- function(year, language = "en", data_dir = NULL) {
  loaded <- morie_arsau_load_main_records(year, language = language, data_dir = data_dir)
  df <- loaded$data

  sub_results <- list()

  if (.MORIE_ARSAU_MAIN_FORCE_COL %in% names(df)) {
    sub_results$force_concentration <- mrm_uof_force_concentration(
      df, force_col = .MORIE_ARSAU_MAIN_FORCE_COL
    )
  }

  if (.MORIE_ARSAU_MAIN_INCIDENT_TYPE_COL %in% names(df) &&
      .MORIE_ARSAU_MAIN_FORCE_COL %in% names(df)) {
    sub_results$incident_type_x_force <- mrm_uof_weapon_diversity(
      df,
      weapon_col = .MORIE_ARSAU_MAIN_INCIDENT_TYPE_COL,
      force_col  = .MORIE_ARSAU_MAIN_FORCE_COL
    )
  }

  sub_results$data_quality <- mrm_uof_data_quality_audit(
    df, sidecar = loaded$sidecar
  )

  .morie_arsau_wrap(
    title         = sprintf("ARSAU main_records analysis (%s)", loaded$year),
    call          = sprintf("morie_arsau_analyze_main_records(year=%s)", sQuote(year)),
    sub_results   = sub_results,
    data          = df,
    sidecar       = loaded$sidecar,
    year_or_range = loaded$year,
    kind          = "main_records",
    language      = language,
    is_valid      = loaded$is_valid
  )
}


# ---------------------------------------------------------------------------
# 2. individual_records analysis
# ---------------------------------------------------------------------------

#' End-to-end analysis of the ARSAU individual_records CSV for one year.
#'
#' Chains demographic-disparity tests over Race, Gender, and
#' AgeCategory against the \code{IndivInjuries_PhysicalInjuries}
#' outcome column, plus a data-quality audit against the sidecar.
#'
#' @param year 2023 or 2024.
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @param bootstrap_reps Integer; forwarded to
#'   \code{\link{mrm_uof_demographic_disparity}}.  Set to e.g. 1000 to
#'   get percentile-bootstrap CIs on the risk ratios.
#' @return A list classed
#'   \code{c("morie_arsau_result", "morie_rich_result", "list")}.
#' @references Ontario Ministry of the Solicitor General, ARSAU 2023
#'   and 2024 individual_records technical release notes.
#' @export
morie_arsau_analyze_individual_records <- function(year, language = "en",
                                                   data_dir = NULL,
                                                   bootstrap_reps = 0L) {
  loaded <- morie_arsau_load_individual_records(year, language = language,
                                                data_dir = data_dir)
  df <- loaded$data

  sub_results <- list()

  outcome_col_actual <- .morie_arsau_locate_outcome_col(
    df, .MORIE_ARSAU_INDIV_OUTCOME_COL
  )

  if (is.null(outcome_col_actual)) {
    sub_results$data_quality <- mrm_uof_data_quality_audit(
      df, sidecar = loaded$sidecar
    )
    return(.morie_arsau_wrap(
      title         = sprintf("ARSAU individual_records analysis (%s)", loaded$year),
      call          = sprintf("morie_arsau_analyze_individual_records(year=%s)", sQuote(year)),
      sub_results   = sub_results,
      data          = df,
      sidecar       = loaded$sidecar,
      year_or_range = loaded$year,
      kind          = "individual_records",
      language      = language,
      is_valid      = loaded$is_valid,
      extra_interpretation = sprintf(
        "Disparity analysis skipped: outcome column %s not found in this CSV.",
        sQuote(.MORIE_ARSAU_INDIV_OUTCOME_COL)
      )
    ))
  }

  # Robust Yes/No -> 0/1 coercion: stringify, strip, lower, then map.
  outcome_str <- tolower(trimws(as.character(df[[outcome_col_actual]])))
  coerce_map <- c(yes = 1L, "true" = 1L, "1" = 1L,
                  no  = 0L, "false" = 0L, "0" = 0L)
  mask <- outcome_str %in% names(coerce_map)
  work <- df[mask, , drop = FALSE]
  work[["_outcome"]] <- unname(coerce_map[outcome_str[mask]])

  demos <- list(
    disparity_by_race   = .MORIE_ARSAU_INDIV_RACE_COL,
    disparity_by_gender = .MORIE_ARSAU_INDIV_GENDER_COL,
    disparity_by_age    = .MORIE_ARSAU_INDIV_AGE_COL
  )
  for (nm in names(demos)) {
    demo_col <- demos[[nm]]
    if (demo_col %in% names(work)) {
      sub_results[[nm]] <- mrm_uof_demographic_disparity(
        work,
        demo_col       = demo_col,
        outcome_col    = "_outcome",
        bootstrap_reps = bootstrap_reps
      )
    }
  }

  sub_results$data_quality <- mrm_uof_data_quality_audit(
    df, sidecar = loaded$sidecar
  )

  .morie_arsau_wrap(
    title         = sprintf("ARSAU individual_records analysis (%s)", loaded$year),
    call          = sprintf("morie_arsau_analyze_individual_records(year=%s)", sQuote(year)),
    sub_results   = sub_results,
    data          = df,
    sidecar       = loaded$sidecar,
    year_or_range = loaded$year,
    kind          = "individual_records",
    language      = language,
    is_valid      = loaded$is_valid,
    extra_interpretation = sprintf(
      paste0(
        "Outcome variable is %s (coerced from Yes/No strings to 1/0). ",
        "Disparity tests use the largest-N demographic group as the ",
        "baseline; pass bootstrap_reps > 0 to attach percentile CIs ",
        "to the risk ratios."
      ),
      sQuote(.MORIE_ARSAU_INDIV_OUTCOME_COL)
    )
  )
}


# ---------------------------------------------------------------------------
# 3. probe_cycle_records analysis
# ---------------------------------------------------------------------------

#' Analysis of ARSAU probe_cycle_records (CEW telemetry).
#'
#' The probe-cycle file is intentionally narrow (BatchFileName +
#' Indiv_Index + a comma-separated cycle string).  This function
#' computes the cycle-count distribution per incident and runs a
#' data-quality audit.
#'
#' @param year 2023 or 2024.
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @return A list classed
#'   \code{c("morie_arsau_result", "morie_rich_result", "list")}.
#' @references Ontario Ministry of the Solicitor General, ARSAU
#'   probe_cycle_records technical notes (2023 and 2024).
#' @export
morie_arsau_analyze_probe_cycle_records <- function(year, language = "en",
                                                    data_dir = NULL) {
  loaded <- morie_arsau_load_probe_cycle_records(year, language = language,
                                                 data_dir = data_dir)
  df <- loaded$data

  sub_results <- list()

  if (.MORIE_ARSAU_PROBE_CYCLE_COL %in% names(df)) {
    raw <- df[[.MORIE_ARSAU_PROBE_CYCLE_COL]]
    raw[is.na(raw)] <- ""
    raw <- as.character(raw)
    cycle_counts <- vapply(raw, function(s) {
      if (!nzchar(trimws(s))) return(0L)
      toks <- strsplit(s, ",", fixed = TRUE)[[1]]
      toks <- toks[nzchar(trimws(toks))]
      length(toks)
    }, integer(1))

    n_rows <- as.integer(length(cycle_counts))
    n_with <- as.integer(sum(cycle_counts > 0L))
    mean_c <- if (n_rows > 0L) mean(cycle_counts) else NA_real_
    med_c  <- if (n_rows > 0L) stats::median(cycle_counts) else NA_real_
    max_c  <- if (n_rows > 0L) max(cycle_counts) else 0L

    descriptive <- list(
      n_rows        = n_rows,
      n_with_cycles = n_with,
      mean_cycles   = mean_c,
      median_cycles = med_c,
      max_cycles    = as.integer(max_c)
    )

    cycle_summary <- list(
      title         = "CEW cycle-count distribution",
      call          = sprintf("(internal) cycle parse of %s",
                              sQuote(.MORIE_ARSAU_PROBE_CYCLE_COL)),
      summary_lines = descriptive,
      warnings      = character(0),
      interpretation = sprintf(
        paste0(
          "Across %d probe-cycle row(s), the mean number of cycles ",
          "per row is %.2f (median %.2f, max %d)."
        ),
        n_rows,
        if (is.finite(mean_c)) mean_c else NA_real_,
        if (is.finite(med_c))  med_c  else NA_real_,
        as.integer(max_c)
      )
    )
    cycle_summary <- c(cycle_summary, descriptive)
    class(cycle_summary) <- c("morie_arsau_result",
                              "morie_rich_result", "list")
    sub_results$cycle_distribution <- cycle_summary
  }

  sub_results$data_quality <- mrm_uof_data_quality_audit(
    df, sidecar = loaded$sidecar
  )

  .morie_arsau_wrap(
    title         = sprintf("ARSAU probe_cycle_records analysis (%s)", loaded$year),
    call          = sprintf("morie_arsau_analyze_probe_cycle_records(year=%s)", sQuote(year)),
    sub_results   = sub_results,
    data          = df,
    sidecar       = loaded$sidecar,
    year_or_range = loaded$year,
    kind          = "probe_cycle_records",
    language      = language,
    is_valid      = loaded$is_valid
  )
}


# ---------------------------------------------------------------------------
# 4. weapon_records analysis
# ---------------------------------------------------------------------------

#' Analysis of ARSAU weapon_records.
#'
#' Chains \code{\link{mrm_uof_weapon_diversity}} over
#' \code{Weapon x Location} (the only two categorical columns the file
#' publishes) plus a Weapon-only frequency table plus a data-quality
#' audit.
#'
#' The 2023 file is the ministry-flagged-invalid release and requires
#' \code{allow_invalid = TRUE}.
#'
#' @param year 2023 or 2024.
#' @param allow_invalid Logical; required \code{TRUE} for 2023.
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @return A list classed
#'   \code{c("morie_arsau_result", "morie_rich_result", "list")}.
#' @references Ontario Ministry of the Solicitor General, ARSAU 2023
#'   and 2024 weapon_records technical notes -- the 2023 release
#'   accompanies an explicit invalidity flag.
#' @export
morie_arsau_analyze_weapon_records <- function(year, allow_invalid = FALSE,
                                               language = "en",
                                               data_dir = NULL) {
  loaded <- morie_arsau_load_weapon_records(
    year,
    allow_invalid = allow_invalid,
    language      = language,
    data_dir      = data_dir
  )
  df <- loaded$data

  sub_results <- list()

  if (.MORIE_ARSAU_WEAPON_WEAPON_COL %in% names(df) &&
      .MORIE_ARSAU_WEAPON_LOCATION_COL %in% names(df)) {
    sub_results$weapon_x_location <- mrm_uof_weapon_diversity(
      df,
      weapon_col = .MORIE_ARSAU_WEAPON_WEAPON_COL,
      force_col  = .MORIE_ARSAU_WEAPON_LOCATION_COL
    )
  }

  if (.MORIE_ARSAU_WEAPON_WEAPON_COL %in% names(df)) {
    wc <- sort(table(df[[.MORIE_ARSAU_WEAPON_WEAPON_COL]]), decreasing = TRUE)
    total_n <- as.integer(sum(wc))
    n_distinct <- as.integer(length(wc))

    if (n_distinct > 0L) {
      shares <- as.numeric(wc) / total_n
      top_weapon <- names(wc)[1L]
      top_share  <- shares[1L]
    } else {
      shares <- numeric(0)
      top_weapon <- "-"
      top_share  <- 0
    }

    rows <- if (n_distinct > 0L) {
      data.frame(
        weapon = names(wc),
        n      = as.integer(wc),
        share  = shares,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    } else {
      data.frame(weapon = character(0), n = integer(0),
                 share = numeric(0))
    }

    weapon_freq <- list(
      title         = "Weapon frequency distribution",
      call          = sprintf("(internal) table() on %s",
                              sQuote(.MORIE_ARSAU_WEAPON_WEAPON_COL)),
      summary_lines = list(
        `Distinct weapons`  = n_distinct,
        `Total weapon rows` = total_n,
        `Top weapon`        = top_weapon,
        `Top weapon share`  = top_share
      ),
      warnings      = character(0),
      interpretation = sprintf(
        "%d distinct weapon type(s) recorded across %d weapon-row(s).",
        n_distinct, total_n
      ),
      table     = rows,
      n_distinct = n_distinct,
      n_total    = total_n,
      value      = n_distinct
    )
    class(weapon_freq) <- c("morie_arsau_result",
                            "morie_rich_result", "list")
    sub_results$weapon_frequencies <- weapon_freq
  }

  sub_results$data_quality <- mrm_uof_data_quality_audit(
    df, sidecar = loaded$sidecar
  )

  extra <- ""
  if (!isTRUE(loaded$is_valid)) {
    extra <- paste0(
      "These results are computed for data-quality review only \u2014 the ",
      "underlying file is the ministry-flagged invalid 2023 release. ",
      "Do NOT use the weapon frequencies or the chi-square association ",
      "in comparative analysis."
    )
  }

  .morie_arsau_wrap(
    title         = sprintf("ARSAU weapon_records analysis (%s)", loaded$year),
    call          = sprintf(
      "morie_arsau_analyze_weapon_records(year=%s, allow_invalid=%s)",
      sQuote(year), if (isTRUE(allow_invalid)) "TRUE" else "FALSE"
    ),
    sub_results          = sub_results,
    data                 = df,
    sidecar              = loaded$sidecar,
    year_or_range        = loaded$year,
    kind                 = "weapon_records",
    language             = language,
    is_valid             = loaded$is_valid,
    extra_interpretation = extra
  )
}


# ---------------------------------------------------------------------------
# 5. aggregate_summary analysis
# ---------------------------------------------------------------------------

#' Analysis of the ARSAU aggregate-summary-by-year file (2020-2022).
#'
#' The aggregate file is a long-format
#' \code{YEAR_2020 / YEAR_2021 / YEAR_2022} panel keyed by
#' \code{(SECTION, CATEGORY, UNITS OF MEASURE)}.  This function
#' rebuilds the implied time series, runs year-on-year change against
#' the \code{REPORT_SCOPE} rows (the headline volume series), and
#' surfaces a data-quality audit.
#'
#' @param year_range "2020-2022".
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @return A list classed
#'   \code{c("morie_arsau_result", "morie_rich_result", "list")}.
#' @references Ontario Ministry of the Solicitor General, ARSAU
#'   2020-2022 aggregate-summary-by-year technical notes.
#' @export
morie_arsau_analyze_aggregate_summary <- function(year_range = "2020-2022",
                                                  language = "en",
                                                  data_dir = NULL) {
  loaded <- morie_arsau_load_aggregate_summary(
    year_range, language = language, data_dir = data_dir
  )
  df <- loaded$data

  sub_results <- list()

  year_cols <- grep(paste0("^", .MORIE_ARSAU_AGG_YEAR_PREFIX), names(df),
                    value = TRUE)
  if (length(year_cols) > 0L) {
    years <- sort(as.integer(sub(
      paste0("^", .MORIE_ARSAU_AGG_YEAR_PREFIX), "", year_cols
    )))

    if (.MORIE_ARSAU_AGG_SECTION_COL %in% names(df)) {
      mask <- df[[.MORIE_ARSAU_AGG_SECTION_COL]] == "REPORT_SCOPE"
    } else {
      mask <- rep(TRUE, nrow(df))
    }
    if (any(mask)) {
      headline <- df[which(mask)[1L], , drop = FALSE]
    } else {
      headline <- df[1L, , drop = FALSE]
    }

    dfs_by_year <- list()
    for (y in years) {
      col <- paste0(.MORIE_ARSAU_AGG_YEAR_PREFIX, y)
      value <- if (col %in% names(headline)) headline[[col]] else 0
      count <- suppressWarnings(as.integer(value))
      if (is.na(count) || count < 0L) count <- 0L
      dfs_by_year[[as.character(y)]] <-
        if (count > 0L) data.frame(row = seq_len(count))
        else data.frame()
    }

    sub_results$yoy_change_headline <- mrm_uof_yoy_change(
      dfs_by_year = dfs_by_year
    )
  }

  sub_results$data_quality <- mrm_uof_data_quality_audit(
    df, sidecar = loaded$sidecar
  )

  .morie_arsau_wrap(
    title         = sprintf("ARSAU aggregate_summary analysis (%s)", loaded$year),
    call          = sprintf("morie_arsau_analyze_aggregate_summary(year_range=%s)",
                            sQuote(year_range)),
    sub_results   = sub_results,
    data          = df,
    sidecar       = loaded$sidecar,
    year_or_range = loaded$year,
    kind          = "aggregate_summary",
    language      = language,
    is_valid      = loaded$is_valid,
    extra_interpretation = paste0(
      "Year-on-year change is computed against the '1 to 3 Subjects - ",
      "Individual Reports' REPORT_SCOPE row, which is the headline ",
      "volume metric in the ministry's annual technical reports."
    )
  )
}


# ---------------------------------------------------------------------------
# 6. detailed_dataset analysis
# ---------------------------------------------------------------------------

#' Wide-format analysis of the 2020-2022 detailed-incident dataset.
#'
#' Chains:
#' \itemize{
#'   \item \code{\link{mrm_uof_force_concentration}} on
#'         \code{POLICE_SERVICE}
#'   \item \code{\link{mrm_uof_weapon_diversity}} on
#'         \code{POLICE_SERVICE x ASSIGNMENT_TYPE}
#'   \item \code{\link{mrm_uof_yoy_change}} on \code{REPORTING_YEAR}
#'   \item \code{\link{mrm_uof_data_quality_audit}}
#' }
#'
#' @param year_range "2020-2022".
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @return A list classed
#'   \code{c("morie_arsau_result", "morie_rich_result", "list")}.
#' @references Ontario Ministry of the Solicitor General, ARSAU
#'   2020-2022 detailed_dataset technical notes.
#' @export
morie_arsau_analyze_detailed_dataset <- function(year_range = "2020-2022",
                                                 language = "en",
                                                 data_dir = NULL) {
  loaded <- morie_arsau_load_detailed_dataset(
    year_range, language = language, data_dir = data_dir
  )
  df <- loaded$data

  sub_results <- list()

  force_col      <- if ("POLICE_SERVICE"   %in% names(df)) "POLICE_SERVICE"   else NULL
  year_col       <- if ("REPORTING_YEAR"   %in% names(df)) "REPORTING_YEAR"   else NULL
  assignment_col <- if ("ASSIGNMENT_TYPE"  %in% names(df)) "ASSIGNMENT_TYPE"  else NULL

  if (!is.null(force_col)) {
    sub_results$force_concentration <- mrm_uof_force_concentration(
      df, force_col = force_col
    )
  }
  if (!is.null(force_col) && !is.null(assignment_col)) {
    sub_results$assignment_x_force <- mrm_uof_weapon_diversity(
      df, weapon_col = assignment_col, force_col = force_col
    )
  }
  if (!is.null(year_col)) {
    sub_results$yoy_change <- mrm_uof_yoy_change(df = df, year_col = year_col)
  }

  sub_results$data_quality <- mrm_uof_data_quality_audit(
    df, sidecar = loaded$sidecar
  )

  .morie_arsau_wrap(
    title         = sprintf("ARSAU detailed_dataset analysis (%s)", loaded$year),
    call          = sprintf("morie_arsau_analyze_detailed_dataset(year_range=%s)",
                            sQuote(year_range)),
    sub_results   = sub_results,
    data          = df,
    sidecar       = loaded$sidecar,
    year_or_range = loaded$year,
    kind          = "detailed_dataset",
    language      = language,
    is_valid      = loaded$is_valid
  )
}
