# SPDX-License-Identifier: AGPL-3.0-or-later
#' RichResult-emitting analyses for the 13 TPS crime datasets
#'
#' R-side port of \code{morie.tps_all_analyze}. Provides a uniform
#' bundle (temporal + spatial + offence + neighbourhood-concentration)
#' that runs on any of Toronto Police Service's 13 public crime CSVs
#' plus a cross-category comparison driver.
#'
#' Functions
#' ---------
#'
#' \itemize{
#'   \item \code{\link{morie_tps_temporal_summary}}: year / month / dow / hour rollups.
#'   \item \code{\link{morie_tps_spatial_summary}}: neighbourhood / division /
#'     premises / location-type + lat-lon bbox.
#'   \item \code{\link{morie_tps_offence_summary}}: OFFENCE / UCR / CSI rollups.
#'   \item \code{\link{morie_tps_neighbourhood_concentration}}: Gini +
#'     top-10/top-20 share across HOOD_158.
#'   \item \code{\link{morie_tps_crime_compare}}: side-by-side counts
#'     and YoY across multiple TPS data.frames.
#'   \item \code{\link{morie_tps_analyze_one}}: full bundle on one frame.
#'   \item \code{\link{morie_tps_analyze_all}}: full bundle across every
#'     TPS data.frame supplied in a named list.
#'   \item \code{morie_tps_analyze_assault()}, ...,
#'     \code{morie_tps_analyze_theftover()}: 13 thin convenience aliases.
#' }
#'
#' Each summary callable returns a named \code{list} carrying summary
#' lines, optional tables, optional warnings, and a plain-language
#' \code{interpretation}. The aggregate \code{morie_tps_analyze_one()}
#' nests these sub-results under named keys.
#'
#' @name morie_tps_analyze
NULL


# ---------------------------------------------------------------------------
# Canonical TPS dataset name list (matches src/morie/tps_datasets.py)
# ---------------------------------------------------------------------------

.MORIE_TPS_NAMES <- c(
  "Assault", "AutoTheft", "BicycleTheft", "BreakandEnter",
  "CommunitySafetyIndicators", "HateCrimes", "Homicides",
  "IntimatePartnerAndFamilyViolence", "NeighbourhoodCrimeRates",
  "Robbery", "ShootingAndFirearmDiscarges",
  "TheftFromMovingVehicle", "TheftOver"
)


# ---------------------------------------------------------------------------
# Internal: rich-result wrapper (matches morie.fn._richresult.RichResult shape)
# ---------------------------------------------------------------------------

.tps_result <- function(title, summary_lines = list(),
                        tables = list(),
                        warnings = character(0),
                        interpretation = "",
                        sections = list(),
                        payload = list(),
                        ...) {
  out <- list(
    title = title,
    summary_lines = summary_lines,
    tables = tables,
    warnings = warnings,
    interpretation = interpretation,
    sections = sections,
    payload = payload,
    ...
  )
  class(out) <- c("morie_tps_result", "morie_rich_result", "list")
  out
}


# ---------------------------------------------------------------------------
# Internal: column-detection + value-counts helpers
# ---------------------------------------------------------------------------

.tps_safe_year_col <- function(df) {
  for (c in c("OCC_YEAR", "REPORT_YEAR", "Year")) {
    if (c %in% names(df)) return(c)
  }
  NULL
}

.tps_vc_rows <- function(x, top = 20L) {
  counts <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
  if (length(counts) > top) counts <- counts[seq_len(top)]
  total <- max(sum(counts), 1L)
  lapply(seq_along(counts), function(i) {
    list(
      label = as.character(names(counts)[i]),
      count = as.integer(counts[i]),
      percent = sprintf("%.1f%%", 100 * as.numeric(counts[i]) / total)
    )
  })
}


# ---------------------------------------------------------------------------
# 1. Temporal summary
# ---------------------------------------------------------------------------

#' Temporal rollup for a TPS crime data.frame
#'
#' Year / month / day-of-week / hour-of-day rollups, plus a coverage
#' line.  Robust to missing columns: only includes tables for the
#' fields actually present.
#'
#' @param df A TPS crime data.frame.
#' @param ds_name Optional dataset label used in the result title.
#' @return A \code{morie_tps_result} named list.
#' @export
morie_tps_temporal_summary <- function(df, ds_name = "?") {
  stopifnot(is.data.frame(df))
  yc <- .tps_safe_year_col(df)
  summary_lines <- list(
    Dataset = ds_name,
    `Total incidents` = nrow(df)
  )
  if (!is.null(yc) && nrow(df) > 0L) {
    years <- suppressWarnings(as.numeric(df[[yc]]))
    years <- years[is.finite(years)]
    if (length(years) > 0L) {
      summary_lines[["Years covered"]] <- sprintf(
        "%d-%d", as.integer(min(years)), as.integer(max(years))
      )
      summary_lines[["Mean per year"]] <- nrow(df) / max(1L, length(unique(years)))
    }
  }

  tables <- list()
  if (!is.null(yc) && nrow(df) > 0L) {
    by_year <- as.list(table(df[[yc]]))
    rows <- lapply(names(by_year), function(y) {
      list(year = as.integer(y), count = as.integer(by_year[[y]]))
    })
    tables[[length(tables) + 1L]] <- list(
      title = sprintf("Incidents by %s:", yc),
      headers = c(yc, "Count"),
      rows = rows
    )
  }
  for (cfg in list(
    list(col = "OCC_MONTH", label = "Incidents by month of occurrence"),
    list(col = "OCC_DOW",   label = "Incidents by day of week of occurrence"),
    list(col = "OCC_HOUR",  label = "Incidents by hour of occurrence")
  )) {
    if (cfg$col %in% names(df)) {
      tables[[length(tables) + 1L]] <- list(
        title = paste0(cfg$label, ":"),
        headers = c(cfg$col, "Count", "Percent"),
        rows = .tps_vc_rows(df[[cfg$col]], top = 24L)
      )
    }
  }
  .tps_result(
    title = sprintf("TPS temporal -- %s", ds_name),
    summary_lines = summary_lines,
    tables = tables,
    interpretation = sprintf(
      "Temporal rollup over %d incident(s) in the %s dataset.",
      nrow(df), ds_name
    )
  )
}


# ---------------------------------------------------------------------------
# 2. Spatial summary
# ---------------------------------------------------------------------------

#' Spatial rollup for a TPS crime data.frame
#'
#' Neighbourhood + division + premises + location-type rollups plus
#' a lat/long bounding-box summary.  Tolerates missing columns.
#'
#' @inheritParams morie_tps_temporal_summary
#' @return A \code{morie_tps_result} named list.
#' @export
morie_tps_spatial_summary <- function(df, ds_name = "?") {
  stopifnot(is.data.frame(df))
  summary_lines <- list(
    Dataset = ds_name,
    Incidents = nrow(df)
  )
  if (all(c("LAT_WGS84", "LONG_WGS84") %in% names(df))) {
    lat <- suppressWarnings(as.numeric(df[["LAT_WGS84"]]))
    lon <- suppressWarnings(as.numeric(df[["LONG_WGS84"]]))
    lat <- lat[is.finite(lat)]
    lon <- lon[is.finite(lon)]
    if (length(lat) > 0L && length(lon) > 0L) {
      summary_lines[["Latitude range"]] <- sprintf(
        "%.4f-%.4f", min(lat), max(lat)
      )
      summary_lines[["Longitude range"]] <- sprintf(
        "%.4f-%.4f", min(lon), max(lon)
      )
      summary_lines[["Geocoded incidents"]] <- min(length(lat), length(lon))
    }
  }

  tables <- list()
  for (cfg in list(
    list(col = "DIVISION",          title = "Top divisions:",
         hdr = "Division"),
    list(col = "NEIGHBOURHOOD_158", title = "Top 20 neighbourhoods (158-system):",
         hdr = "Neighbourhood"),
    list(col = "PREMISES_TYPE",     title = "By premises type:",
         hdr = "Premises"),
    list(col = "LOCATION_TYPE",     title = "Top 20 location types:",
         hdr = "Location")
  )) {
    if (cfg$col %in% names(df)) {
      tables[[length(tables) + 1L]] <- list(
        title = cfg$title,
        headers = c(cfg$hdr, "Count", "Percent"),
        rows = .tps_vc_rows(df[[cfg$col]], top = 20L)
      )
    }
  }
  .tps_result(
    title = sprintf("TPS spatial -- %s", ds_name),
    summary_lines = summary_lines,
    tables = tables,
    interpretation = sprintf(
      "Spatial rollup over %d incident(s) in the %s dataset.",
      nrow(df), ds_name
    )
  )
}


# ---------------------------------------------------------------------------
# 3. Offence summary
# ---------------------------------------------------------------------------

#' Offence-distribution rollup for a TPS crime data.frame
#'
#' Top-20 OFFENCE / UCR_CODE / CSI_CATEGORY tables (whichever are
#' present).
#'
#' @inheritParams morie_tps_temporal_summary
#' @return A \code{morie_tps_result} named list.
#' @export
morie_tps_offence_summary <- function(df, ds_name = "?") {
  stopifnot(is.data.frame(df))
  summary_lines <- list(Dataset = ds_name, Incidents = nrow(df))
  tables <- list()
  for (cfg in list(
    list(col = "OFFENCE",      label = "Top 20 offences"),
    list(col = "UCR_CODE",     label = "UCR code distribution"),
    list(col = "CSI_CATEGORY", label = "CSI category distribution")
  )) {
    if (cfg$col %in% names(df)) {
      tables[[length(tables) + 1L]] <- list(
        title = paste0(cfg$label, ":"),
        headers = c(cfg$col, "Count", "Percent"),
        rows = .tps_vc_rows(df[[cfg$col]], top = 20L)
      )
    }
  }
  .tps_result(
    title = sprintf("TPS offences -- %s", ds_name),
    summary_lines = summary_lines,
    tables = tables,
    interpretation = sprintf(
      "Offence rollup over %d incident(s) in the %s dataset.",
      nrow(df), ds_name
    )
  )
}


# ---------------------------------------------------------------------------
# 4. Gini + neighbourhood concentration
# ---------------------------------------------------------------------------

#' Gini coefficient of a numeric vector
#'
#' G=0 perfectly even, G=1 perfectly concentrated.  Used for the
#' neighbourhood-concentration callable below; exposed because tests
#' and downstream code may want it directly.
#'
#' @param x Numeric vector (e.g. per-spatial-unit incident counts).
#' @return A scalar Gini coefficient in `[0, 1]` (or NA when input is empty).
#' @export
morie_tps_gini_concentration <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  sorted_v <- sort(x)
  n <- length(sorted_v)
  cum <- cumsum(sorted_v)
  if (cum[n] == 0) return(0.0)
  as.numeric((n + 1 - 2 * sum(cum) / cum[n]) / n)
}


#' How concentrated is crime across Toronto's 158 neighbourhoods?
#'
#' Uses HOOD_158 (the 158-neighbourhood scheme) and reports a Gini
#' coefficient plus the cumulative share of incidents in the top-10
#' and top-20 neighbourhoods.
#'
#' @inheritParams morie_tps_temporal_summary
#' @return A \code{morie_tps_result} list with \code{payload$gini},
#'   \code{payload$n_hoods}, \code{payload$p_top10}, \code{payload$p_top20}.
#' @export
morie_tps_neighbourhood_concentration <- function(df, ds_name = "?") {
  stopifnot(is.data.frame(df))
  if (!("HOOD_158" %in% names(df))) {
    return(.tps_result(
      title = sprintf("TPS concentration -- %s", ds_name),
      warnings = "HOOD_158 column missing -- cannot compute neighbourhood concentration.",
      interpretation = sprintf(
        "No analysis: HOOD_158 column absent from the %s dataset.", ds_name
      )
    ))
  }
  hood_counts <- sort(table(df[["HOOD_158"]]), decreasing = TRUE)
  n_hoods <- length(hood_counts)
  g <- morie_tps_gini_concentration(as.numeric(hood_counts))
  cum <- cumsum(as.numeric(hood_counts))
  total <- cum[length(cum)]
  cum_pct <- if (total > 0) cum / total else cum
  p_top10 <- as.numeric(cum_pct[min(10L, length(cum_pct))])
  p_top20 <- as.numeric(cum_pct[min(20L, length(cum_pct))])

  show <- min(20L, n_hoods)
  rows <- lapply(seq_len(show), function(i) {
    list(
      hood = as.character(names(hood_counts)[i]),
      count = as.integer(hood_counts[i]),
      cum_pct = sprintf("%.1f%%", 100 * cum_pct[i])
    )
  })
  .tps_result(
    title = sprintf("TPS neighbourhood concentration -- %s", ds_name),
    summary_lines = list(
      `Neighbourhoods with >=1 incident` = n_hoods,
      `Gini coefficient (concentration)` = round(g, 4),
      `% incidents in top-10 neighbourhoods` = sprintf("%.1f%%", 100 * p_top10),
      `% incidents in top-20 neighbourhoods` = sprintf("%.1f%%", 100 * p_top20)
    ),
    tables = list(list(
      title = "Top 20 neighbourhoods (count, cum %):",
      headers = c("HOOD_158", "Count", "Cum %"),
      rows = rows
    )),
    interpretation = sprintf(
      paste0("Gini = %.3f measures how unequally incidents are distributed ",
             "across neighbourhoods. Higher = more concentrated. ",
             "%.0f%% of incidents fall in the top-10 neighbourhoods alone."),
      g, 100 * p_top10
    ),
    payload = list(
      gini = g, n_hoods = n_hoods,
      p_top10 = p_top10, p_top20 = p_top20,
      top20 = as.list(hood_counts[seq_len(show)])
    )
  )
}


# ---------------------------------------------------------------------------
# 5. Cross-dataset crime compare
# ---------------------------------------------------------------------------

#' Compare counts and trends across multiple TPS categories
#'
#' Accepts a named list of TPS data.frames (e.g.
#' \code{list(Assault = df_a, Robbery = df_r, ...)}) and returns a
#' \code{morie_tps_result} with a total-counts table and (when
#' \code{OCC_YEAR} is present in every frame) a side-by-side
#' year-by-year matrix.
#'
#' @param dfs Named \code{list} of TPS data.frames.
#' @return A \code{morie_tps_result} list.
#' @export
morie_tps_crime_compare <- function(dfs) {
  stopifnot(is.list(dfs), length(dfs) > 0L,
            !is.null(names(dfs)), all(nzchar(names(dfs))))
  rows <- lapply(names(dfs), function(nm) {
    list(category = nm, incidents = nrow(dfs[[nm]]))
  })
  rows <- rows[order(-vapply(rows, `[[`, integer(1), "incidents"))]

  # Year-by-year side-by-side
  year_table <- NULL
  yc <- "OCC_YEAR"
  pivots <- list()
  for (nm in names(dfs)) {
    df <- dfs[[nm]]
    if (yc %in% names(df)) {
      s <- table(df[[yc]])
      pivots[[nm]] <- s
    }
  }
  if (length(pivots) > 0L) {
    all_years <- sort(unique(unlist(lapply(pivots, names))))
    m <- matrix(0L, nrow = length(all_years), ncol = length(pivots),
                dimnames = list(all_years, names(pivots)))
    for (nm in names(pivots)) {
      yrs <- names(pivots[[nm]])
      m[yrs, nm] <- as.integer(pivots[[nm]])
    }
    year_rows <- lapply(seq_len(nrow(m)), function(i) {
      c(list(year = as.integer(rownames(m)[i])),
        as.list(setNames(as.integer(m[i, ]), colnames(m))))
    })
    year_table <- list(
      title = "Incidents by OCC_YEAR (side-by-side):",
      headers = c("OCC_YEAR", colnames(m)),
      rows = year_rows
    )
  }

  tables <- list(list(
    title = "Total counts per category:",
    headers = c("Category", "Incidents"),
    rows = rows
  ))
  if (!is.null(year_table)) tables[[length(tables) + 1L]] <- year_table

  total <- sum(vapply(dfs, nrow, integer(1)))
  .tps_result(
    title = "TPS -- cross-category comparison",
    summary_lines = list(
      `Categories compared` = length(dfs),
      `Total incidents (sum)` = total
    ),
    tables = tables,
    interpretation = sprintf(
      "Compared %d TPS categor%s carrying %d total incident-row(s).",
      length(dfs), if (length(dfs) == 1L) "y" else "ies", total
    )
  )
}


# ---------------------------------------------------------------------------
# 6. Master per-dataset driver
# ---------------------------------------------------------------------------

#' Run the standard TPS analysis bundle on one data.frame
#'
#' Chains temporal + spatial + offence + concentration into a single
#' nested result.  This is the function the 13 convenience aliases
#' (\code{morie_tps_analyze_assault()}, etc.) wrap.
#'
#' @param df A TPS crime data.frame.
#' @param name The canonical TPS dataset name (used in titles).
#' @return A \code{morie_tps_result} with named sub-results under
#'   \code{temporal}, \code{spatial}, \code{offences}, \code{concentration}.
#' @export
morie_tps_analyze_one <- function(df, name = "?") {
  stopifnot(is.data.frame(df))
  temp <- morie_tps_temporal_summary(df, ds_name = name)
  spat <- morie_tps_spatial_summary(df, ds_name = name)
  offc <- morie_tps_offence_summary(df, ds_name = name)
  conc <- morie_tps_neighbourhood_concentration(df, ds_name = name)

  sections <- list(
    list(title = "TEMPORAL",      lines = temp$summary_lines),
    list(title = "SPATIAL",       lines = spat$summary_lines),
    list(title = "OFFENCES",      lines = offc$summary_lines),
    list(title = "CONCENTRATION", lines = conc$summary_lines)
  )
  warnings <- unique(c(temp$warnings, spat$warnings,
                       offc$warnings, conc$warnings))

  out <- .tps_result(
    title = sprintf("TPS %s -- full analysis bundle", name),
    summary_lines = list(
      Dataset = name,
      Rows = nrow(df),
      Columns = ncol(df)
    ),
    sections = sections,
    warnings = warnings,
    interpretation = sprintf(
      "Ran 4 sub-analyses (temporal, spatial, offences, concentration) over the %s dataset (%d rows, %d cols).",
      name, nrow(df), ncol(df)
    ),
    temporal = temp,
    spatial = spat,
    offences = offc,
    concentration = conc
  )
  out
}


# ---------------------------------------------------------------------------
# 7. Convenience aliases (13)
# ---------------------------------------------------------------------------

.tps_alias_factory <- function(name) {
  force(name)
  function(df) morie_tps_analyze_one(df, name = name)
}

#' Convenience alias: full TPS bundle on the Assault dataset.
#' @param df A TPS Assault data.frame.
#' @return A \code{morie_tps_result}.
#' @export
morie_tps_analyze_assault <- .tps_alias_factory("Assault")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_autotheft <- .tps_alias_factory("AutoTheft")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_bicycletheft <- .tps_alias_factory("BicycleTheft")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_breakandenter <- .tps_alias_factory("BreakandEnter")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_communitysafetyindicators <- .tps_alias_factory("CommunitySafetyIndicators")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_hatecrimes <- .tps_alias_factory("HateCrimes")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_homicides <- .tps_alias_factory("Homicides")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_intimatepartnerandfamilyviolence <- .tps_alias_factory("IntimatePartnerAndFamilyViolence")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_neighbourhoodcrimerates <- .tps_alias_factory("NeighbourhoodCrimeRates")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_robbery <- .tps_alias_factory("Robbery")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_shootingandfirearmdiscarges <- .tps_alias_factory("ShootingAndFirearmDiscarges")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_theftfrommovingvehicle <- .tps_alias_factory("TheftFromMovingVehicle")

#' @rdname morie_tps_analyze_assault
#' @export
morie_tps_analyze_theftover <- .tps_alias_factory("TheftOver")


# ---------------------------------------------------------------------------
# 8. Master orchestrator
# ---------------------------------------------------------------------------

#' Run the full TPS analysis bundle across many TPS data.frames
#'
#' Mirrors \code{morie.tps_all_analyze.analyze_all}.  Caller supplies
#' the data.frames; loading from disk is left to the user (R-side loaders
#' live in \code{R/data_access.R} and \code{R/dataset_catalog.R}).  An
#' optional \code{out_dir} writes per-dataset \code{tps_<name>.txt}
#' transcripts.
#'
#' @param dfs Named \code{list} of TPS data.frames keyed by canonical
#'   TPS name (e.g. \code{"Assault"}, \code{"Homicides"}, ...).
#' @param out_dir Optional directory to write per-dataset text dumps.
#'   When \code{NULL}, no files are written.
#' @return A named \code{list} of \code{morie_tps_result} values, plus
#'   a \code{`__cross_compare__`} entry from
#'   \code{\link{morie_tps_crime_compare}}.
#' @export
morie_tps_analyze_all <- function(dfs, out_dir = NULL) {
  stopifnot(is.list(dfs), length(dfs) > 0L,
            !is.null(names(dfs)), all(nzchar(names(dfs))))
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  results <- list()
  ordered_names <- sort(names(dfs))
  for (name in ordered_names) {
    df <- dfs[[name]]
    r <- tryCatch(
      morie_tps_analyze_one(df, name = name),
      error = function(e) {
        .tps_result(
          title = sprintf("TPS %s (failed)", name),
          warnings = sprintf("%s: %s",
                             class(e)[1], conditionMessage(e)),
          interpretation = sprintf(
            "Analysis failed for %s: %s", name, conditionMessage(e)
          )
        )
      }
    )
    results[[name]] <- r
    if (!is.null(out_dir)) {
      txt <- utils::capture.output(print(r))
      writeLines(txt, file.path(out_dir, sprintf("tps_%s.txt", name)))
    }
  }

  cross <- tryCatch(
    morie_tps_crime_compare(dfs),
    error = function(e) {
      .tps_result(
        title = "TPS cross-compare (failed)",
        warnings = sprintf("%s: %s",
                           class(e)[1], conditionMessage(e)),
        interpretation = sprintf(
          "Cross-compare failed: %s", conditionMessage(e)
        )
      )
    }
  )
  results[["__cross_compare__"]] <- cross
  if (!is.null(out_dir)) {
    txt <- utils::capture.output(print(cross))
    writeLines(txt, file.path(out_dir, "tps_cross_compare.txt"))
  }
  results
}


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' @export
print.morie_tps_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      cat(sprintf("  %-*s  %s\
", label_w, nms[i],
                  format(x$summary_lines[[i]])))
    }
    cat("\
")
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\
")
    cat("\
")
  }
  if (nzchar(x$interpretation)) {
    cat(x$interpretation, "\
")
  }
  invisible(x)
}
