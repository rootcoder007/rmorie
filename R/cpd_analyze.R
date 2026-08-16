# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Chicago Police Department (CPD) analysis aggregator. Mirrors the
# morie_<subject>_all_analyses pattern (otis/siu/tps): loads the Chicago
# crime + arrests data (bundled sample offline by default) and runs the
# applicable descriptive, predictive-policing (predpol), and fairness
# surfaces, each returning a rich-result-shaped list.

# Build a rich-result-shaped surface payload.
#' Build a rich-result-shaped surface payload
#'
#' Part of the cpd_analyze implementation; see the file header for the
#' source it follows.
#'
#' @param title See Usage.
#' @param summary_lines Defaults to \code{list()}.
#' @param tables Defaults to \code{list()}.
#' @param interpretation Defaults to \code{""}.
#' @param warnings Defaults to \code{""}.
#' @param payload Defaults to \code{list()}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_cpd_result <- function(title, summary_lines = list(), tables = list(),
                              interpretation = "", warnings = "",
                              payload = list()) {
  out <- list(
    title = title, summary_lines = summary_lines, tables = tables,
    interpretation = interpretation, warnings = warnings,
    payload = payload
  )
  class(out) <- c("morie_cpd_result", "morie_rich_result", "list")
  out
}

# Load a bundled CPD fixture (offline). `which` is "crime" or "arrests".
#' Load a bundled CPD fixture (offline). `which` is "crime" or "arrests"
#'
#' Part of the cpd_analyze implementation; see the file header for the
#' source it follows.
#'
#' @param which Defaults to \code{c("crime", "arrests")}.
#' @return The value of \code{utils::read.csv}.
#' @export
.morie_cpd_load_sample <- function(which = c("crime", "arrests")) {
  which <- match.arg(which)
  file <- switch(which,
    crime   = "chicago_crime_synthetic.csv",
    arrests = "chicago_arrests_dpt3_jri9_sample.csv"
  )
  path <- system.file("extdata", file, package = "rmorie")
  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
}

#' Run the full Chicago Police Department analysis suite
#'
#' Aggregates the applicable descriptive, predictive-policing, and fairness
#' analyses for Chicago crime + arrests data, mirroring
#' \code{\link{morie_otis_all_analyses}} and \code{\link{morie_siu_all_analyses}}.
#'
#' @param crime_df Optional Chicago crime data frame; \code{NULL} (default)
#'   loads the bundled Chicago crime fixture (offline).
#' @param arrests_df Optional Chicago arrests data frame (carries a
#'   \code{race} column); \code{NULL} loads the bundled arrests fixture.
#' @param out_dir Optional directory to write per-surface outputs.
#' @return A named list of rich-result surfaces: \code{crime_by_type},
#'   \code{arrests_by_area} (predpol area concentration), \code{temporal},
#'   and \code{arrest_race_disparity} (fairness).
#' @examples
#' \donttest{
#' res <- morie_cpd_all_analyses()
#' names(res)
#' }
#' @seealso \code{\link{morie_predpol_aggregate_areas}},
#'   \code{\link{morie_fairness_disparate_impact}}
#' @export
morie_cpd_all_analyses <- function(crime_df = NULL, arrests_df = NULL,
                                   out_dir = NULL) {
  if (is.null(crime_df)) crime_df <- .morie_cpd_load_sample("crime")
  if (is.null(arrests_df)) arrests_df <- .morie_cpd_load_sample("arrests")

  have_col <- function(df, col) is.data.frame(df) && col %in% names(df)

  fns <- list(
    crime_by_type = function() {
      if (!have_col(crime_df, "primary_type")) {
        return(.morie_cpd_result("CPD crimes by primary type",
          warnings = "missing column: primary_type"
        ))
      }
      tab <- sort(table(crime_df$primary_type), decreasing = TRUE)
      .morie_cpd_result(
        "CPD crimes by primary type",
        summary_lines = list(
          list("distinct types", length(tab)),
          list("total records", sum(tab))
        ),
        tables = list(by_type = as.data.frame(tab,
          responseName = "count"
        )),
        interpretation = "Incident volume by Chicago primary crime type.",
        payload = as.list(tab)
      )
    },
    arrests_by_area = function() {
      if (!have_col(crime_df, "community_area") ||
        !have_col(crime_df, "arrest")) {
        return(.morie_cpd_result("CPD arrest concentration by community area",
          warnings = "missing column(s): community_area, arrest"
        ))
      }
      area <- as.character(crime_df$community_area)
      arrest <- as.integer(as.logical(crime_df$arrest) %in% TRUE)
      agg <- morie_predpol_aggregate_areas(
        area = area,
        risk = ave(arrest, area, FUN = function(z) mean(z, na.rm = TRUE)),
        outcome = arrest
      )
      # `group` is NULL here (no protected attribute), so build the table
      # from the equal-length area-level vectors only.
      area_tbl <- data.frame(
        area = agg$areas, mean_risk = agg$mean_risk,
        outcome_rate = agg$outcome_rate,
        n_records = agg$n_records,
        stringsAsFactors = FALSE
      )
      .morie_cpd_result(
        "CPD arrest concentration by community area",
        summary_lines = list(list("areas", length(unique(area)))),
        tables = list(area_aggregate = area_tbl),
        interpretation = paste(
          "Predictive-policing area aggregation: arrest",
          "rate and concentration by Chicago community area."
        ),
        payload = list(aggregate = agg)
      )
    },
    temporal = function() {
      if (!have_col(crime_df, "year")) {
        return(.morie_cpd_result("CPD temporal trend",
          warnings = "missing column: year"
        ))
      }
      tab <- table(crime_df$year)
      .morie_cpd_result(
        "CPD temporal trend",
        summary_lines = list(list("years", length(tab))),
        tables = list(by_year = as.data.frame(tab, responseName = "count")),
        interpretation = "Incident counts by year.",
        payload = as.list(tab)
      )
    },
    arrest_race_disparity = function() {
      if (!have_col(arrests_df, "race")) {
        return(.morie_cpd_result("CPD arrests: race disparity",
          warnings = "missing column: race (arrests data)"
        ))
      }
      race <- as.character(arrests_df$race)
      keep <- !is.na(race) & nzchar(race)
      race <- race[keep]
      di <- morie_fairness_disparate_impact(
        y_pred = rep(1L, length(race)), group = race
      )
      .morie_cpd_result(
        "CPD arrests: race disparity",
        summary_lines = list(
          list("groups", length(unique(race))),
          list("records", length(race))
        ),
        tables = list(by_race = as.data.frame(
          sort(table(race),
            decreasing = TRUE
          ),
          responseName = "count"
        )),
        interpretation = paste(
          "Disparate-impact ratio of arrest representation",
          "across recorded race categories."
        ),
        payload = list(
          disparate_impact = di,
          counts = as.list(table(race))
        )
      )
    }
  )

  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  results <- list()
  for (nm in names(fns)) {
    results[[nm]] <- tryCatch(fns[[nm]](), error = function(e) {
      .morie_cpd_result(sprintf("cpd.%s (failed)", nm),
        warnings = sprintf(
          "%s: %s", class(e)[1],
          conditionMessage(e)
        )
      )
    })
    if (!is.null(out_dir)) {
      tryCatch(
        writeLines(
          .morie_to_json(results[[nm]]$payload,
            auto_unbox = TRUE,
            null = "null", force = TRUE
          ),
          file.path(out_dir, sprintf("cpd_%s.json", nm))
        ),
        error = function(e) NULL
      )
    }
  }
  results
}
