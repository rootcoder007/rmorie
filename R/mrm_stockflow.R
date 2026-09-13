# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stock and flow across the OTIS strata
#'
#' MRM (Multilevel Reconciliation Methodology) applied to person-days:
#' the same confinement appears at three strata in the OTIS release and
#' they do not automatically agree.
#'
#' * PERSON stratum (b02): one row per individual per year, carrying the
#'   days that person served across the year. This is the additive
#'   quantity, and the one Lakner's measures need.
#' * PLACEMENT stratum (b01): several rows per individual, carrying the
#'   consecutive days of a spell and the count of placements. Spell
#'   lengths are NOT an additive share of the year.
#' * AGGREGATE stratum (c01): the yearly totals stated directly.
#'
#' Reconciling them is the point. Summing the placement stratum's
#' consecutive days as though they were person-days understates the
#' total by about a third on the published release, and the two strata
#' can disagree about how many people there even were. Both are reported
#' rather than assumed away.
#'
#' The measures themselves come from `rmoriebricklayer`: the average
#' daily population is days per day, the average length of stay is days
#' per person, and a change in days decomposes exactly into a change in
#' people and a change in stay. After Lakner, A Manual of Statistical
#' Sampling Methods for Corrections Planners (University of Illinois at
#' Urbana-Champaign, 1976), p.15-18.
#'
#' @param person_days Data frame at the PERSON stratum: one row per
#'   individual per period, with a period column, an identifier column
#'   and a column of days served.
#' @param placements Optional data frame at the PLACEMENT stratum, used
#'   only for reconciliation: rows per individual per spell, with the
#'   consecutive-days and placement-count columns.
#' @param totals Optional data frame at the AGGREGATE stratum, one or
#'   more rows per period carrying a stated total of individuals.
#' @param year_col Period column name, present in every stratum given.
#' @param id_col Identifier column in `person_days` and `placements`.
#' @param days_col Days column in `person_days`.
#' @param consecutive_col Consecutive-days column in `placements`.
#' @param placement_col Placement-count column in `placements`.
#' @param totals_col Stated-total column in `totals`.
#' @param t Length of each period in days. Default 365.
#' @param exposure Optional population to express rates against, one
#'   value or one per period.
#' @param per Rate denominator when `exposure` is given. Default 100000.
#'
#' @return A list with:
#'   \itemize{
#'     \item `stock_flow`: the measures per period, from
#'       `rmoriebricklayer::stock_flow()`.
#'     \item `reconciliation`: one row per period comparing the strata --
#'       people at the person and placement strata, the stated aggregate
#'       total, whether they agree, and the ratio of summed consecutive
#'       days to person-days.
#'     \item `strata_agree`: `TRUE` when every stratum given agrees on
#'       the number of people in every period.
#'     \item `decomposition_exact`: `TRUE` when the change in days is
#'       recovered by the changes in people and stay.
#'   }
#'
#' @references
#' Lakner, E. (1976) A Manual of Statistical Sampling Methods for
#' Corrections Planners. University of Illinois at Urbana-Champaign.
#'
#' @examples
#' # needs rmoriebricklayer (>= 0.4.8) for adp()/alos()/stock_flow()
#' if (utils::packageVersion("rmoriebricklayer") >= "0.4.8") {
#' person <- data.frame(
#'   EndFiscalYear = c(rep(2023, 3), rep(2025, 2)),
#'   UniqueIndividual_ID = c("a", "b", "c", "a", "d"),
#'   TotalAggregatedDays_Segregation = c(10, 20, 30, 40, 50))
#' mrm_otis_stock_flow(person)$stock_flow
#' }
#' @export
mrm_otis_stock_flow <- function(person_days,
                                placements = NULL,
                                totals = NULL,
                                year_col = "EndFiscalYear",
                                id_col = "UniqueIndividual_ID",
                                days_col = "TotalAggregatedDays_Segregation",
                                consecutive_col =
                                  "NumberConsecutiveDays_Segregation",
                                placement_col = "Number_Of_Placements",
                                totals_col = "NumberIndividuals_Segregation",
                                t = 365, exposure = NULL, per = 100000) {
  .need <- function(d, cols, what) {
    if (!is.data.frame(d))
      stop(sprintf("`%s` must be a data frame", what), call. = FALSE)
    miss <- setdiff(cols, names(d))
    if (length(miss))
      stop(sprintf("`%s` is missing column(s): %s", what,
                   paste(miss, collapse = ", ")), call. = FALSE)
    invisible(TRUE)
  }
  ## The measures come from rmoriebricklayer, and an installed copy older
  ## than 0.4.8 does not have them. Say so here rather than let a `::`
  ## call fail with a message about an object not being exported.
  for (.fn in c("adp", "alos", "stock_flow")) {
    if (!exists(.fn, envir = asNamespace("rmoriebricklayer"),
                inherits = FALSE)) {
      stop("this needs rmoriebricklayer (>= 0.4.8) for ", .fn,
           "(); the installed copy is ",
           as.character(utils::packageVersion("rmoriebricklayer")),
           call. = FALSE)
    }
  }
  .need(person_days, c(year_col, id_col, days_col), "person_days")
  if (!is.null(placements))
    .need(placements, c(year_col, id_col, consecutive_col), "placements")
  if (!is.null(totals)) .need(totals, c(year_col, totals_col), "totals")

  yrs <- sort(unique(as.character(person_days[[year_col]])))
  py <- as.character(person_days[[year_col]])
  days <- vapply(yrs, function(y)
    sum(person_days[[days_col]][py == y], na.rm = TRUE), numeric(1))
  people <- vapply(yrs, function(y)
    length(unique(person_days[[id_col]][py == y])), numeric(1))
  if (any(people == 0))
    stop("every period must contain at least one person", call. = FALSE)

  sf <- rmoriebricklayer::stock_flow(days = days, people = people,
                                     period = yrs, t = t,
                                     exposure = exposure, per = per)

  ## ── the reconciliation ──
  rec <- data.frame(period = yrs, person_stratum_people = people,
                    person_stratum_days = days, stringsAsFactors = FALSE)
  if (!is.null(placements)) {
    qy <- as.character(placements[[year_col]])
    rec$placement_stratum_people <- vapply(yrs, function(y)
      length(unique(placements[[id_col]][qy == y])), numeric(1))
    rec$consecutive_days <- vapply(yrs, function(y)
      sum(placements[[consecutive_col]][qy == y], na.rm = TRUE), numeric(1))
    ## Below one because a spell length is not an additive share of the
    ## year. Reported so that summing the wrong column is visible rather
    ## than silently understating the total.
    rec$consecutive_over_person_days <- rec$consecutive_days / rec$person_stratum_days
    if (placement_col %in% names(placements)) {
      rec$placements <- vapply(yrs, function(y)
        sum(placements[[placement_col]][qy == y], na.rm = TRUE), numeric(1))
      rec$placements_per_person <- rec$placements / rec$placement_stratum_people
    }
  } else {
    rec$placement_stratum_people <- NA_real_
    rec$consecutive_over_person_days <- NA_real_
  }
  if (!is.null(totals)) {
    ty <- as.character(totals[[year_col]])
    rec$aggregate_stratum_total <- vapply(yrs, function(y)
      sum(totals[[totals_col]][ty == y], na.rm = TRUE), numeric(1))
  } else {
    rec$aggregate_stratum_total <- NA_real_
  }
  rec$strata_agree <- mapply(function(a, b, c) {
    v <- c(a, b, c)
    v <- v[!is.na(v)]
    length(unique(v)) == 1L
  }, rec$person_stratum_people, rec$placement_stratum_people,
     rec$aggregate_stratum_total)

  ## days = people x stay, so the change in days must multiply out. This
  ## is the check that would catch a stratum being mixed into the wrong
  ## column.
  exact <- TRUE
  if (nrow(sf) > 1L) {
    i <- nrow(sf)
    p <- sf$people_change[i] / 100
    l <- sf$alos_change[i] / 100
    exact <- isTRUE(all.equal((1 + p) * (1 + l) - 1, sf$days_change[i] / 100,
                              tolerance = 1e-8))
  }
  list(stock_flow = sf, reconciliation = rec,
       strata_agree = all(rec$strata_agree),
       decomposition_exact = exact)
}
