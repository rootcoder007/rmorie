# SPDX-License-Identifier: AGPL-3.0-or-later
#
# New York Police Department (NYPD) analysis aggregator. Mirrors the
# morie_<subject>_all_analyses pattern: loads NYPD arrests + complaint data
# (bundled samples offline by default) and runs the applicable descriptive
# and fairness surfaces. NYPD arrests carry perp_race and law_cat_cd
# (felony/misdemeanour), so a felony-charge disparate-impact analysis by
# race is computable directly from the data.

#' .morie_nypd_result
#'
#' A step of the nypd_analyze implementation. Called by \code{morie_nypd_all_analyses}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param title Carried through into a list the body builds.
#' @param summary_lines Carried through into a list the body builds. Defaults to \code{list()}.
#' @param tables Carried through into a list the body builds. Defaults to \code{list()}.
#' @param interpretation Carried through into a list the body builds. Defaults to \code{""}.
#' @param warnings Carried through into a list the body builds. Defaults to \code{""}.
#' @param payload Carried through into a list the body builds. Defaults to \code{list()}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_nypd_result <- function(title, summary_lines = list(), tables = list(),
                               interpretation = "", warnings = "",
                               payload = list()) {
  out <- list(title = title, summary_lines = summary_lines, tables = tables,
              interpretation = interpretation, warnings = warnings,
              payload = payload)
  class(out) <- c("morie_nypd_result", "morie_rich_result", "list")
  out
}

#' .morie_nypd_load_sample
#'
#' A step of the nypd_analyze implementation. Called by \code{morie_nypd_all_analyses}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param which Defaults to \code{c("arrests", "complaint")}.
#' @return The value of \code{utils::read.csv}.
#' @export
.morie_nypd_load_sample <- function(which = c("arrests", "complaint")) {
  which <- match.arg(which)
  file <- switch(which,
                 arrests   = "nypd_arrests_historic_sample.csv",
                 complaint = "nypd_complaint_historic_sample.csv")
  path <- system.file("extdata", file, package = "rmorie")
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
}

#' Run the full New York Police Department analysis suite
#'
#' Aggregates the applicable descriptive and fairness analyses for NYPD
#' arrests + complaint data, mirroring \code{\link{morie_otis_all_analyses}}.
#'
#' @param arrests_df Optional NYPD arrests data frame (carries
#'   \code{perp_race} and \code{law_cat_cd}); \code{NULL} (default) loads the
#'   bundled arrests fixture (offline).
#' @param complaint_df Optional NYPD complaint data frame (carries
#'   \code{susp_race}); \code{NULL} loads the bundled complaint fixture.
#' @param out_dir Optional directory to write per-surface outputs.
#' @return A named list of rich-result surfaces: \code{arrests_by_offense},
#'   \code{arrests_by_boro}, \code{felony_race_disparity} (fairness), and
#'   \code{complaints_by_race}.
#' @examples
#' \donttest{
#' res <- morie_nypd_all_analyses()
#' names(res)
#' }
#' @seealso \code{\link{morie_fairness_disparate_impact}}
#' @export
morie_nypd_all_analyses <- function(arrests_df = NULL, complaint_df = NULL,
                                    out_dir = NULL) {
  if (is.null(arrests_df)) arrests_df <- .morie_nypd_load_sample("arrests")
  if (is.null(complaint_df)) complaint_df <- .morie_nypd_load_sample("complaint")

  have_col <- function(df, col) is.data.frame(df) && col %in% names(df)
  count_table <- function(x) sort(table(x[!is.na(x) & nzchar(x)]),
                                   decreasing = TRUE)

  fns <- list(
    arrests_by_offense = function() {
      if (!have_col(arrests_df, "ofns_desc")) {
        return(.morie_nypd_result("NYPD arrests by offense",
                                  warnings = "missing column: ofns_desc"))
      }
      tab <- count_table(as.character(arrests_df$ofns_desc))
      .morie_nypd_result(
        "NYPD arrests by offense",
        summary_lines = list(list("distinct offenses", length(tab)),
                             list("records", sum(tab))),
        tables = list(by_offense = as.data.frame(utils::head(tab, 25L),
                                                 responseName = "count")),
        interpretation = "Arrest volume by NYPD offense description.",
        payload = as.list(utils::head(tab, 50L)))
    },

    arrests_by_boro = function() {
      if (!have_col(arrests_df, "arrest_boro")) {
        return(.morie_nypd_result("NYPD arrests by borough",
                                  warnings = "missing column: arrest_boro"))
      }
      tab <- count_table(as.character(arrests_df$arrest_boro))
      .morie_nypd_result(
        "NYPD arrests by borough",
        tables = list(by_boro = as.data.frame(tab, responseName = "count")),
        interpretation = "Arrest volume by borough.",
        payload = as.list(tab))
    },

    felony_race_disparity = function() {
      if (!have_col(arrests_df, "perp_race") ||
          !have_col(arrests_df, "law_cat_cd")) {
        return(.morie_nypd_result("NYPD felony-charge race disparity",
                                  warnings = "missing column(s): perp_race, law_cat_cd"))
      }
      race <- as.character(arrests_df$perp_race)
      # law_cat_cd: "F" = felony (unfavorable), else misdemeanour/violation.
      felony <- as.integer(toupper(trimws(as.character(arrests_df$law_cat_cd))) == "F")
      keep <- !is.na(race) & nzchar(race) & !is.na(felony)
      race <- race[keep]; felony <- felony[keep]
      di <- morie_fairness_disparate_impact(y_pred = felony, group = race)
      dp <- morie_fairness_demographic_parity(y_pred = felony, group = race)
      rates <- tapply(felony, race, mean)
      .morie_nypd_result(
        "NYPD felony-charge race disparity",
        summary_lines = list(list("groups", length(unique(race))),
                             list("records", length(race))),
        tables = list(felony_rate_by_race = data.frame(
          race = names(rates), felony_rate = as.numeric(rates),
          row.names = NULL)),
        interpretation = paste("Disparate impact and demographic parity of",
                               "felony (vs. lesser) charging across recorded",
                               "perpetrator race categories."),
        payload = list(disparate_impact = di, demographic_parity = dp,
                       felony_rate_by_race = as.list(rates)))
    },

    complaints_by_race = function() {
      if (!have_col(complaint_df, "susp_race")) {
        return(.morie_nypd_result("NYPD complaints: suspect race",
                                  warnings = "missing column: susp_race"))
      }
      tab <- count_table(as.character(complaint_df$susp_race))
      .morie_nypd_result(
        "NYPD complaints: suspect race",
        tables = list(by_race = as.data.frame(tab, responseName = "count")),
        interpretation = "Complaint volume by recorded suspect race.",
        payload = as.list(tab))
    }
  )

  if (!is.null(out_dir)) dir.create(out_dir, showWarnings = FALSE,
                                    recursive = TRUE)
  results <- list()
  for (nm in names(fns)) {
    results[[nm]] <- tryCatch(fns[[nm]](), error = function(e) {
      .morie_nypd_result(sprintf("nypd.%s (failed)", nm),
                         warnings = sprintf("%s: %s", class(e)[1],
                                            conditionMessage(e)))
    })
    if (!is.null(out_dir)) {
      tryCatch(
        writeLines(.morie_to_json(results[[nm]]$payload, auto_unbox = TRUE,
                                    null = "null", force = TRUE),
                   file.path(out_dir, sprintf("nypd_%s.json", nm))),
        error = function(e) NULL)
    }
  }
  results
}
