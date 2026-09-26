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

# ---------------------------------------------------------------------------
# Lakner (1976) stock and flow measures -- ported from bricklayer's
# R/custody.R; mirrors morie.mrm_stockflow (adp, alos, admissions,
# adp_from_counts, stock_flow, period_days, stay_summary).
#
#   mrm_adp(days, t)   = sum(days) / t    a STOCK: how many are held at once
#   mrm_alos(days, n)  = sum(days) / n    a FLOW denominator: how long each stays
#   adp = n * alos / t                     the identity linking them
# ---------------------------------------------------------------------------

.mrm_sf_pos_num <- function(x, arg, allow_zero = FALSE) {
  if (!is.numeric(x) && !is.logical(x)) {
    stop(sprintf("`%s` must be numeric", arg), call. = FALSE)
  }
  if (any(!is.finite(x[!is.na(x)]))) {
    stop(sprintf("`%s` must be finite (no Inf)", arg), call. = FALSE)
  }
  x <- as.numeric(x)
  if (!length(x)) stop(sprintf("`%s` must not be empty", arg), call. = FALSE)
  if (anyNA(x)) stop(sprintf("`%s` must not contain NA", arg), call. = FALSE)
  bad <- if (allow_zero) any(x < 0) else any(x <= 0)
  if (bad) {
    stop(sprintf("`%s` must be %s", arg,
                 if (allow_zero) "non-negative" else "positive"),
         call. = FALSE)
  }
  x
}

#' Average daily population
#'
#' The mean number of person-days served per day over a period: a
#' STOCK, answering how many people are held at one time.
#'
#' @param days Person-days served during the period. A vector is summed,
#'   so one element per person is the usual input.
#' @param t Length of the period in days. Default 365.
#'
#' @return A single number: person-days per day.
#'
#' @details
#' This is Lakner's \eqn{\bar{X}_{hc} = \sum X_i / t} (1976, p.15). The
#' denominator is TIME, which is what makes it a stock. Compare
#' [mrm_alos()], whose denominator is people.
#'
#' @references
#' Lakner, E. (1976) \emph{A Manual of Statistical Sampling Methods for
#' Corrections Planners}. University of Illinois at Urbana-Champaign.
#'
#' @seealso [mrm_alos()],
#'   [mrm_stock_flow()],
#'   [mrm_adp_from_counts()]
#'
#' @examples
#' # Lakner's own worked example (p.15): 3,000 inmates served 13,500
#' # detention days in a year.
#' mrm_adp(13500)
#'
#' # per-person days give the same total
#' mrm_adp(c(10, 20, 30), t = 30)
#' @export
mrm_adp <- function(days, t = 365) {
  days <- .mrm_sf_pos_num(days, "days", allow_zero = TRUE)
  t <- .mrm_sf_pos_num(t, "t")[1L]
  sum(days) / t
}

#' Average length of stay
#'
#' The mean number of person-days served per person: the FLOW side of
#' the same person-days.
#'
#' @param days Person-days served during the period, summed.
#' @param n Number of people. For an unbiased average this should be the
#'   people both admitted AND released within the period, because anyone
#'   still held has an unfinished stay.
#'
#' @return A single number: days per person.
#'
#' @details
#' Lakner's \eqn{\bar{X}_t = \sum X_i / N'} (1976, p.16), with a caveat
#' worth repeating (p.16-17): the period must be longer than the longest
#' stay people actually serve, or the average is biased DOWNWARD, since
#' the longest stays are the ones that fail to finish inside the window.
#' For short-stay facilities a year is comfortable; for long sentences it
#' is not, and the period has to be set from the records.
#'
#' @references
#' Lakner, E. (1976) \emph{A Manual of Statistical Sampling Methods for
#' Corrections Planners}. University of Illinois at Urbana-Champaign.
#'
#' @seealso [mrm_adp()], [mrm_stock_flow()]
#'
#' @examples
#' # Lakner (p.17): 2,700 inmates admitted and released served 12,150
#' # detention days between them.
#' mrm_alos(12150, 2700)
#' @export
mrm_alos <- function(days, n) {
  days <- .mrm_sf_pos_num(days, "days", allow_zero = TRUE)
  n <- .mrm_sf_pos_num(n, "n")[1L]
  sum(days) / n
}

#' Admissions implied by a population and a length of stay
#'
#' @param adp Average daily population.
#' @param alos Average length of stay in days.
#' @param t Length of the period in days. Default 365.
#'
#' @return The implied number of admissions.
#'
#' @details
#' Rearranging the identity \eqn{\bar{X}_{hc} = N_a \bar{X}_t / t}
#' (Lakner 1976, p.18-20) for \eqn{N_a}. Useful when two of the three
#' quantities are published and the third is not.
#'
#' @seealso [mrm_adp()],
#'   [mrm_alos()],
#'   [mrm_stock_flow()]
#'
#' @examples
#' # Lakner (p.20) runs this the other way: t = 365, an average daily
#' # population of 25 and 1,750 admissions imply a stay of 5.2 days.
#' alos_implied <- 25 * 365 / 1750
#' round(alos_implied, 1)
#'
#' # and back again
#' mrm_admissions(25, alos_implied)
#' @export
mrm_admissions <- function(adp, alos, t = 365) {
  adp <- .mrm_sf_pos_num(adp, "adp")[1L]
  alos <- .mrm_sf_pos_num(alos, "alos")[1L]
  t <- .mrm_sf_pos_num(t, "t")[1L]
  adp * t / alos
}

#' Person-days estimated from periodic headcounts
#'
#' When only a count of people present on certain days is available --
#' not a record per person -- the person-days over the period are the
#' mean of those counts scaled to the period's length.
#'
#' @param counts Headcounts, one per day on which a count was taken.
#' @param t Length of the period in days. Default 365.
#'
#' @return Estimated person-days over the period.
#'
#' @details
#' Lakner's \eqn{X_t = (\frac{1}{C}\sum N_i) t} (1976, p.21). The counts
#' need not cover every day: counting on weekdays only is the usual case,
#' and the mean of the days counted stands in for the days not counted.
#' That substitution is an assumption, and it fails if the days counted
#' differ systematically from the days missed -- weekday-only counting
#' where weekend admissions are released before Monday, for instance.
#'
#' @seealso [mrm_adp()]
#'
#' @examples
#' # Lakner (p.21): counts on 255 days summing to 34,935 imply just over
#' # fifty thousand detention days across the year.
#' mrm_adp_from_counts(rep(34935 / 255, 255))
#' @export
mrm_adp_from_counts <- function(counts, t = 365) {
  counts <- .mrm_sf_pos_num(counts, "counts", allow_zero = TRUE)
  t <- .mrm_sf_pos_num(t, "t")[1L]
  mean(counts) * t
}

#' Stock and flow side by side, with the decomposition
#'
#' Reports the same person-days as a stock and as a flow, and says how
#' much of any change in the stock came from the number of people and
#' how much from how long they stayed.
#'
#' @param days Person-days, one element per period.
#' @param people Number of people, one element per period.
#' @param period Optional labels for the periods.
#' @param t Length of each period in days, one value or one per period.
#'   Default 365. [mrm_period_days()] turns dates into this.
#' @param exposure Optional population to express rates against, one per
#'   period; for example provincial residents.
#' @param per Rate denominator when `exposure` is given. Default 100000.
#' @param baseline What the change columns compare against: `"first"`
#'   (the default) measures every period against the first, which is
#'   what a report on a whole window wants; `"previous"` measures each
#'   period against the one before it, which is what a series wants.
#'
#' @return A data frame of class `mrm_stock_flow`, one row per period:
#'   `people`, `days`, `alos`, `adp`, and when `exposure` is supplied
#'   `flow_rate` and `stock_rate`. Change columns compare each period
#'   with the first.
#'
#' @details
#' The decomposition is exact, because days are people times length of
#' stay: a change in days is \eqn{(1+p)(1+l) - 1} for proportional
#' changes \eqn{p} in people and \eqn{l} in stay. The two rates can
#' therefore carry OPPOSITE signs, and the point of putting them in one
#' table is that neither can then be quoted alone.
#'
#' @references
#' Lakner, E. (1976) \emph{A Manual of Statistical Sampling Methods for
#' Corrections Planners}. University of Illinois at Urbana-Champaign.
#'
#' @seealso [mrm_adp()], [mrm_alos()]
#'
#' @examples
#' # Fewer people, held longer: the flow falls while the stock rises.
#' mrm_stock_flow(days = c(115674, 126121), people = c(12647, 9608),
#'            period = c("2023", "2025"),
#'            exposure = c(15495050, 16256538))
#' @export
mrm_stock_flow <- function(days, people, period = NULL, t = 365,
                       exposure = NULL, per = 100000,
                       baseline = c("first", "previous")) {
  baseline <- match.arg(baseline)
  days <- .mrm_sf_pos_num(days, "days", allow_zero = TRUE)
  people <- .mrm_sf_pos_num(people, "people")
  if (length(days) != length(people)) {
    stop("`days` and `people` must be the same length", call. = FALSE)
  }
  t <- .mrm_sf_pos_num(t, "t")
  if (length(t) == 1L) t <- rep(t, length(days))
  if (is.null(period)) period <- seq_along(days)
  out <- data.frame(period = as.character(period), people = people,
                    days = days, stringsAsFactors = FALSE)
  out$alos <- days / people
  out$adp <- days / t
  if (!is.null(exposure)) {
    exposure <- .mrm_sf_pos_num(exposure, "exposure")
    if (length(exposure) == 1L) exposure <- rep(exposure, length(days))
    if (length(exposure) != length(days)) {
      stop("`exposure` must be length 1 or the same length as `days`",
           call. = FALSE)
    }
    per <- .mrm_sf_pos_num(per, "per")[1L]
    out$exposure <- exposure
    out$flow_rate <- per * people / exposure
    out$stock_rate <- per * out$adp / exposure
  }
  ## The change columns are what make the two signs visible beside each
  ## other, so which baseline they use is a reporting decision rather
  ## than a detail: against the first period for a window, against the
  ## previous one for a series.
  chg <- if (identical(baseline, "first")) {
    function(v) 100 * (v / v[1L] - 1)
  } else {
    function(v) c(NA_real_, 100 * (v[-1L] / v[-length(v)] - 1))
  }
  out$people_change <- chg(out$people)
  out$alos_change <- chg(out$alos)
  out$days_change <- chg(out$days)
  out$adp_change <- chg(out$adp)
  if (!is.null(exposure)) {
    out$flow_rate_change <- chg(out$flow_rate)
    out$stock_rate_change <- chg(out$stock_rate)
  }
  structure(out, class = c("mrm_stock_flow", "data.frame"),
            stock_flow = list(t = t, baseline = baseline,
                              per = if (is.null(exposure)) NA else per))
}

#' Period length in days, from dates
#'
#' @param from Start of the period: a `Date`, or anything `as.Date()`
#'   accepts.
#' @param to End of the period, inclusive.
#'
#' @return The number of days in the period, for use as `t`.
#'
#' @details
#' Inclusive of both ends, because a period running from the 1st to the
#' 31st is thirty-one days of exposure, not thirty. Leap years need no
#' special handling: the arithmetic is on dates, so 2024 comes out at
#' 366 and 2023 at 365 without anyone choosing.
#'
#' @seealso [mrm_adp()]
#'
#' @examples
#' mrm_period_days("2024-01-01", "2024-12-31")   # a leap year
#' mrm_period_days("2023-01-01", "2023-12-31")
#' mrm_period_days("2025-04-01", "2026-03-31")   # a fiscal year
#' @export
mrm_period_days <- function(from, to) {
  if (is.numeric(from) || is.numeric(to)) {
    stop("`from` and `to` must be dates or date strings, not numbers",
         call. = FALSE)
  }
  from <- as.Date(from); to <- as.Date(to)
  if (length(from) != 1L || length(to) != 1L)
    stop("`from` and `to` must each be a single date", call. = FALSE)
  if (is.na(from) || is.na(to))
    stop("`from` and `to` must be dates", call. = FALSE)
  if (to < from) stop("`to` must not precede `from`", call. = FALSE)
  as.numeric(to - from) + 1
}

#' Length of stay with its distribution and interval
#'
#' Where [mrm_alos()] takes the total days and returns a mean, this takes
#' one value per person and reports the spread as well, because a mean
#' stay is a poor summary of a distribution that is usually skewed.
#'
#' @param days_per_person Days served, one element per person.
#' @param conf_level Confidence level for the interval on the mean.
#'
#' @return A one-row data frame: `n`, `total_days`, `mean`, `sd`,
#'   `median`, `iqr`, `max`, `se`, `lower`, `upper`.
#'
#' @details
#' The interval is the ordinary t interval on a mean. It describes
#' uncertainty about the AVERAGE stay, not the spread of stays, and on a
#' skewed distribution the median and the interquartile range say more
#' about a typical stay than the mean does -- which is why they are
#' returned beside it rather than left to be asked for.
#'
#' Lakner's caveat on [mrm_alos()] applies here too (1976, p.16-17): a
#' person still held has an unfinished stay, so a window shorter than
#' the longest stay biases the mean DOWNWARD.
#'
#' @seealso [mrm_alos()],
#'   [mrm_adp()],
#'   [mrm_stock_flow()]
#'
#' @examples
#' # a skewed distribution: most stays short, a few long
#' stays <- c(rep(1, 40), rep(3, 30), rep(10, 20), 60, 90, 120)
#' mrm_stay_summary(stays)
#'
#' # the mean is pulled well above the median by the long tail
#' mrm_stay_summary(stays)[, c("mean", "median", "max")]
#' @export
mrm_stay_summary <- function(days_per_person, conf_level = 0.95) {
  x <- .mrm_sf_pos_num(days_per_person, "days_per_person", allow_zero = TRUE)
  conf_level <- as.numeric(conf_level)[1L]
  if (is.na(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("`conf_level` must lie strictly inside (0, 1)", call. = FALSE)
  n <- length(x)
  m <- mean(x)
  s <- if (n > 1L) stats::sd(x) else NA_real_
  se <- if (n > 1L) s / sqrt(n) else NA_real_
  tq <- if (n > 1L) stats::qt(1 - (1 - conf_level) / 2, df = n - 1L) else NA_real_
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE, type = 7)
  data.frame(n = n, total_days = sum(x), mean = m, sd = s, median = q[2],
             iqr = q[3] - q[1], max = max(x), se = se,
             lower = if (n > 1L) m - tq * se else NA_real_,
             upper = if (n > 1L) m + tq * se else NA_real_,
             stringsAsFactors = FALSE)
}
