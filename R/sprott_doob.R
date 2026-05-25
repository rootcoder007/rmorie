# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sprott & Doob (CRIMSL UToronto) SIU analyses
#'
#' Replicates the analytical contribution of the four CRIMSL UToronto
#' research reports authored by \strong{Prof. Jane B. Sprott} (TMU,
#' formerly Ryerson) and \strong{Prof. Anthony N. Doob} (University of
#' Toronto), with \strong{Prof. Adelina Iftene} (Dalhousie) co-
#' authoring the May 2021 paper on Independent External Decision
#' Makers (IEDMs).
#'
#' \describe{
#'   \item{Sprott & Doob (Oct 2020)}{\emph{Understanding the Operation
#'     of CSC's Structured Intervention Units} -- first systematic
#'     outside analysis of CSC SIU data.}
#'   \item{Sprott & Doob (Nov 2020)}{\emph{COVID attribution} -- tests
#'     CSC's COVID-attribution defense.}
#'   \item{Sprott & Doob (Feb 2021)}{\emph{Solitary Confinement,
#'     Torture, and Canada's SIUs} -- introduces the Mandela-Rules
#'     classifier; the most data-intensive of the four.}
#'   \item{Sprott, Doob & Iftene (May 2021)}{\emph{Independent External
#'     Decision Makers} -- evaluates the IEDM review mechanism.}
#' }
#'
#' Headline tables (Feb 2021): Tables 13, 19, 23 reproduce SIU
#' person-stay rates per 1000 prisoners, the Mandela-Rules
#' classification of N=1960 SIU stays (solitary 28.4%, torture 9.9%,
#' all-other 61.7%), and the regional torture/solitary rates.
#'
#' Headline tables (May 2021): Tables 1, 3, 5, 7, 8, 9, 10, 14, 15
#' reproduce IEDM-reviewed population characteristics and review
#' outcomes (N=265 stays, 380 reviews).
#'
#' @section Citation:
#' Sprott, J. B., & Doob, A. N. (2021, February). Solitary
#' Confinement, Torture, and Canada's Structured Intervention Units.
#' Centre for Criminology & Sociolegal Studies, U. of Toronto.
#'
#' @name morie_sprott_doob
NULL


# ---------------------------------------------------------------------------
# RichResult-style constructor for the R side
# ---------------------------------------------------------------------------
# Returns a named list classed for morie's rich-output dispatch. Mirrors
# the structure of morie.fn._richresult.RichResult so that downstream
# `describe()` / `morie_print_rich()` consumers can render the same
# multi-section paragraph layout from R.
.morie_siu_rich <- function(title, summary_lines = list(), tables = list(),
                            interpretation = "", warnings = character(),
                            payload = list()) {
  out <- list(
    title = title,
    summary_lines = summary_lines,
    tables = tables,
    interpretation = interpretation,
    warnings = as.character(warnings),
    payload = payload
  )
  class(out) <- c("morie_siu_result", "morie_rich_result", "list")
  out
}


# ---------------------------------------------------------------------------
# Hardcoded tables (Feb 2021 paper -- pp. 3-26)
# ---------------------------------------------------------------------------

.SD_TABLE13_REGIONAL_RATES_PER_1000 <- data.frame(
  region            = c("Atlantic", "Quebec", "Ontario", "Prairie",
                         "Pacific", "Total"),
  short_stay_rate   = c(70.2, 178.1, 18.2, 41.4, 102.7, 73.4),
  long_stay_rate    = c(124.8, 118.1, 30.3, 91.2, 99.8, 84.1),
  overall_rate      = c(195.0, 296.2, 48.5, 132.7, 202.5, 157.5),
  stringsAsFactors  = FALSE
)


.SD_TABLE19_MANDELA_CLASSIFICATION <- data.frame(
  category = c("Solitary Confinement", "Torture",
                "All other person stays"),
  definition = c(
    paste0("Missed full 4 hrs out of cell 100% of days; <=2 hrs ",
            "average out of cell during stay; stayed <=15 days"),
    paste0("Missed full 4 hrs out of cell 100% of days; <=2 hrs ",
            "average out of cell during stay; stayed >=16 days"),
    "Did not meet either threshold above"
  ),
  percent = c(28.4, 9.9, 61.7),
  n       = c(556L, 195L, 1209L),
  stringsAsFactors = FALSE
)


.SD_TABLE23_REGIONAL_TORTURE_RATES <- data.frame(
  region        = c("Atlantic", "Quebec", "Ontario", "Prairie",
                     "Pacific", "Total"),
  solitary_rate = c(45.9, 118.1, 11.8, 13.2, 66.9, 44.2),
  torture_rate  = c(15.6, 25.2, 1.73, 10.5, 39.1, 15.5),
  stringsAsFactors = FALSE
)


.SD_HEADLINE_FINDINGS <- list(
  missed_full_4hrs_overall_pct = 38.9,
  long_stay_missed_4hrs_in_76pct_of_days = 63.0,
  pacific_torture_rate = 39.1,
  ontario_torture_rate = 1.73,
  pacific_to_ontario_ratio = 22.6,
  n_total_stays = 1960L
)


.SD_TABLE4_LENGTH_OF_STAY <- data.frame(
  days = c("1-5", "6-15", "16-31", "32-61", "62-380"),
  n    = c(456L, 468L, 320L, 326L, 413L),
  pct  = c(23.0, 23.6, 16.1, 16.4, 20.8),
  stringsAsFactors = FALSE
)
.SD_TABLE4_N <- 1983L


.SD_TABLE11_REGION_X_STAY_LENGTH <- data.frame(
  region   = c("Atlantic", "Quebec", "Ontario", "Prairies", "Pacific"),
  `1-5`    = c(25L, 266L, 23L, 52L, 90L),
  `6-15`   = c(56L, 179L, 40L, 102L, 91L),
  `16-31`  = c(32L, 90L, 24L, 96L, 78L),
  `32-61`  = c(50L, 79L, 28L, 118L, 51L),
  `62-380` = c(62L, 126L, 53L, 125L, 47L),
  total    = c(225L, 740L, 168L, 493L, 357L),
  stringsAsFactors = FALSE, check.names = FALSE
)
.SD_TABLE11_CHISQ <- list(chi2 = 201.00, df = 16L, p = 0.001)


.SD_TABLE12_REGIONAL_OVERREP <- data.frame(
  region  = c("Atlantic", "Quebec", "Ontario", "Prairie", "Pacific"),
  siu_pct = c(11.3, 37.3, 8.5, 24.9, 18.0),
  pop_pct = c(9.2, 19.8, 27.5, 29.5, 14.0),
  stringsAsFactors = FALSE
)


.SD_TABLE15_REGION_X_MENTAL_HEALTH <- data.frame(
  region  = c("Atlantic", "Quebec", "Ontario", "Prairies", "Pacific"),
  no_mh   = c(169L, 652L, 160L, 369L, 291L),
  yes_mh  = c(88L, 196L, 48L, 190L, 116L),
  total   = c(257L, 848L, 208L, 559L, 407L),
  yes_pct = c(34.2, 23.1, 23.1, 34.0, 28.5),
  stringsAsFactors = FALSE
)
.SD_TABLE15_CHISQ <- list(chi2 = 27.51, df = 4L, p = 0.001)


.SD_TABLE20_TORTURE_GROUP_DAYS <- data.frame(
  days = c("16-31", "32-61", "62-380"),
  n    = c(88L, 59L, 48L),
  pct  = c(45.1, 30.3, 24.6),
  stringsAsFactors = FALSE
)


.SD_TABLE22_REGION_X_MANDELA <- data.frame(
  region         = c("Atlantic", "Quebec", "Ontario", "Prairies", "Pacific"),
  solitary       = c(53L, 295L, 41L, 49L, 118L),
  torture        = c(18L, 63L, 6L, 39L, 69L),
  everyone_else  = c(152L, 369L, 118L, 403L, 167L),
  total          = c(223L, 727L, 165L, 491L, 354L),
  solitary_pct   = c(23.8, 40.6, 24.8, 10.0, 33.3),
  torture_pct    = c(8.1, 8.7, 3.6, 7.9, 19.5),
  stringsAsFactors = FALSE
)
.SD_TABLE22_CHISQ <- list(chi2 = 208.54, df = 8L, p = 0.001)


# ---------------------------------------------------------------------------
# Hardcoded tables (May 2021 IEDM paper -- pp. 1-7)
# ---------------------------------------------------------------------------

.SD_TABLE1_IEDM_POPULATION <- list(
  n_total = 265L,
  gender = data.frame(
    category = c("Female", "Male"),
    percent  = c(0.8, 99.2),
    n        = c(2L, 263L),
    stringsAsFactors = FALSE
  ),
  age_group = data.frame(
    category = c("18-24", "25-29", "30-39", "40-49", "50-64"),
    percent  = c(15.5, 29.8, 32.8, 17.0, 4.9),
    n        = c(41L, 79L, 87L, 45L, 13L),
    stringsAsFactors = FALSE
  ),
  race = data.frame(
    category = c("White", "Indigenous", "Black", "Other/Missing"),
    percent  = c(30.9, 40.4, 15.8, 12.8),
    n        = c(82L, 107L, 42L, 34L),
    stringsAsFactors = FALSE
  ),
  mental_health = data.frame(
    category = c("Mental health need flag",
                  "No mental health need flag"),
    percent  = c(26.4, 73.6),
    n        = c(71L, 194L),
    stringsAsFactors = FALSE
  )
)


.SD_HEADLINE_MAY2021 <- list(
  n_iedm_stays_reviewed = 265L,
  n_iedm_decisions_rendered = 380L,
  pct_stay_in_decisions_among_rendered = 87L,
  pct_csc_moved_prisoner_before_iedm = 30L,
  n_long_stay_no_iedm_record_min76d = 105L,
  n_long_stay_no_iedm_record_min120d = 5L,
  iedm_min_should_remain_pct = 38L,
  iedm_max_should_remain_pct = 86L,
  n_iedms = 12L,
  pct_referred_55_to_62_days = 74.3,
  indigenous_share_of_reviewed_stays_pct = 40.4,
  black_share_of_reviewed_stays_pct = 15.8
)


.SD_TABLE9_MAY2021_IEDM_DECISIONS <- data.frame(
  decision = c("Decision moot", "Inmate to be removed from SIU",
                "Inmate to remain in SIU",
                paste0("N/A -- Inmate transferred out of SIU before ",
                        "decision rendered")),
  n   = c(8L, 33L, 224L, 115L),
  pct = c(2.1, 8.7, 58.9, 30.3),
  stringsAsFactors = FALSE
)
.SD_TABLE9_MAY2021_TOTAL <- 380L


.SD_TABLE10_MAY2021_PER_IEDM <- data.frame(
  iedm             = 1:12,
  remain           = c(22L, 26L, 18L, 27L, 18L, 30L, 23L, 12L,
                       16L, 8L, 6L, 18L),
  removed_or_moot  = c(25L, 9L, 15L, 23L, 10L, 9L, 8L, 20L,
                       13L, 11L, 1L, 12L),
  total            = c(47L, 35L, 33L, 50L, 28L, 39L, 31L, 32L,
                       29L, 19L, 7L, 30L),
  remain_pct       = c(46.8, 74.3, 54.5, 54.0, 64.3, 76.9, 74.2,
                       37.5, 55.2, 42.1, 85.7, 60.0),
  stringsAsFactors = FALSE
)
.SD_TABLE10_MAY2021_CHISQ <- list(chi2 = 26.12, df = 11L, p = 0.01)


.SD_TABLE5_MAY2021_RACE_X_REVIEWS <- data.frame(
  race        = c("White", "Indigenous", "Black", "Other/Missing"),
  one         = c(56L, 84L, 23L, 24L),
  two_plus    = c(26L, 23L, 19L, 10L),
  total       = c(82L, 107L, 42L, 34L),
  two_plus_pct = c(31.7, 21.5, 45.2, 29.4),
  stringsAsFactors = FALSE
)
.SD_TABLE5_MAY2021_CHISQ <- list(chi2 = 8.50, df = 3L, p = 0.05)


.SD_TABLE15_MAY2021_NO_IEDM_BY_STAY_LENGTH <- data.frame(
  days        = c("<=65", "66-75", "76-90", "91-120", ">120"),
  no_iedm     = c(1580L, 30L, 29L, 27L, 49L),
  with_iedm   = c(21L, 40L, 51L, 64L, 88L),
  total       = c(1601L, 70L, 80L, 91L, 137L),
  no_iedm_pct = c(98.7, 42.9, 36.3, 29.7, 35.8),
  stringsAsFactors = FALSE
)
.SD_TABLE15_MAY2021_TOTAL <- 1979L
.SD_TABLE15_MAY2021_LONG_STAY_NO_IEDM <- 105L  # 29 + 27 + 49


# ---------------------------------------------------------------------------
# Mandela classifier
# ---------------------------------------------------------------------------

#' Apply the Sprott-Doob Mandela-Rules classifier to one SIU stay
#'
#' Operationalizes UN Mandela Rules 43 and 44 against a single
#' SIU person-stay's days, average hours out of cell, and percent-
#' of-days that missed the legislatively-required 4 hours out of cell.
#'
#' \itemize{
#'   \item \strong{Solitary Confinement} (Rule 44): <=2 hrs avg out of
#'         cell, missed full 4 hrs every day, stay <=15 days.
#'   \item \strong{Torture} (Rules 43+44): same conditions but stay
#'         length >=16 days (crosses the "prolonged" threshold).
#'   \item \strong{All other}: did not meet either threshold.
#' }
#'
#' @param days_in_siu Length of the stay (days).
#' @param hrs_out_of_cell_avg Average hours out of cell per day during
#'   the stay.
#' @param missed_full_4hrs_pct_of_days Percent of days (0-100) where
#'   the inmate did not receive the legislatively-required 4 hours
#'   out of cell.
#' @return A named list with elements \code{category}, \code{rule},
#'   and \code{reason}.
#' @examples
#' morie_siu_classify_mandela(20, 1.5, 100)$category
#' morie_siu_classify_mandela(8, 1.5, 100)$category
#' morie_siu_classify_mandela(20, 5, 50)$category
#' @export
morie_siu_classify_mandela <- function(days_in_siu,
                                       hrs_out_of_cell_avg,
                                       missed_full_4hrs_pct_of_days) {
  meets_22hr <- (hrs_out_of_cell_avg <= 2.0 &&
                  missed_full_4hrs_pct_of_days >= 100.0)
  if (meets_22hr && days_in_siu <= 15) {
    return(list(
      category = "Solitary Confinement",
      rule = "Mandela Rule 44",
      reason = sprintf(
        "<=2 hrs out of cell (%s), missed full 4 hrs every day, stay %s <= 15 days",
        hrs_out_of_cell_avg, days_in_siu
      )
    ))
  }
  if (meets_22hr && days_in_siu >= 16) {
    return(list(
      category = "Torture",
      rule = "Mandela Rules 43+44",
      reason = sprintf(
        paste0("<=2 hrs out of cell (%s), missed full 4 hrs every day, ",
               "stay %s >= 16 days (crosses Mandela Rule 43 'prolonged' ",
               "threshold)"),
        hrs_out_of_cell_avg, days_in_siu
      )
    ))
  }
  list(
    category = "All other",
    rule = "--",
    reason = paste0(
      "Did not meet the joint threshold of <=2 hrs out of cell, all ",
      "days missed 4 hrs, and stay length"
    )
  )
}


# ---------------------------------------------------------------------------
# Replication analyzers
# ---------------------------------------------------------------------------

#' Sprott-Doob (Feb 2021) Table 13: regional SIU person-stay rates
#'
#' @return A \code{morie_siu_result} (named list with the replicated
#'   table, summary lines, interpretation, and payload).
#' @examples
#' morie_siu_sprott_doob_table13()$payload$qc_on_short_stay_ratio
#' @export
morie_siu_sprott_doob_table13 <- function() {
  t <- .SD_TABLE13_REGIONAL_RATES_PER_1000
  qc <- t$short_stay_rate[t$region == "Quebec"]
  on <- t$short_stay_rate[t$region == "Ontario"]
  ratio <- qc / on
  .morie_siu_rich(
    title = paste0(
      "Sprott & Doob (Feb 2021) Table 13 -- SIU person-stays per ",
      "1,000 regional prisoners"
    ),
    summary_lines = list(
      Source = "Sprott & Doob (Feb 2021), p. 3",
      `Quebec short-stay rate / 1000` = qc,
      `Ontario short-stay rate / 1000` = on,
      `Quebec/Ontario short-stay ratio` =
        sprintf("%.1fx (matches the report's 'almost 10 times' claim)",
                ratio),
      `Total overall rate / 1000` = 157.5
    ),
    tables = list(list(
      title = "Table 13 (rates per 1000 prisoners by region)",
      data = t
    )),
    interpretation = paste0(
      "Reproduces Sprott & Doob's Table 13. Quebec's rate of short SIU ",
      "stays (<=15 days) was nearly 10x Ontario's, and the long-stay ",
      "rate was higher in EVERY other region than in Ontario. Sprott & ",
      "Doob argue this regional variation is not explained by ",
      "population characteristics alone -- it points to structurally ",
      "different decision-making across CSC regions."
    ),
    payload = list(table13 = t, qc_on_short_stay_ratio = ratio)
  )
}


#' Sprott-Doob (Feb 2021) Table 19: Mandela-Rules classification
#'
#' @return A \code{morie_siu_result}.
#' @examples
#' morie_siu_sprott_doob_table19()$payload$pct_problematic
#' @export
morie_siu_sprott_doob_table19 <- function() {
  t <- .SD_TABLE19_MANDELA_CLASSIFICATION
  n_solitary <- t$n[t$category == "Solitary Confinement"]
  n_torture <- t$n[t$category == "Torture"]
  n_problematic <- n_solitary + n_torture
  pct_problematic <- 100 * n_problematic / 1960
  .morie_siu_rich(
    title = paste0(
      "Sprott & Doob (Feb 2021) Table 19 -- Mandela-Rules ",
      "classification of SIU person-stays"
    ),
    summary_lines = list(
      Source = "Sprott & Doob (Feb 2021), p. 4 of 28",
      `Total person-stays classified` = 1960L,
      `Solitary Confinement (Rule 44)` = "28.4% (556)",
      `Torture (Rules 43+44, >=16 days)` = "9.9% (195)",
      `All other person-stays` = "61.7% (1,209)",
      `% meeting either Mandela threshold` =
        sprintf("%.1f%% (%d)", pct_problematic, n_problematic)
    ),
    tables = list(list(
      title = "Table 19 -- Mandela-Rules classification (stays > 1 day)",
      data = t
    )),
    interpretation = paste0(
      "Reproduces Sprott & Doob's Table 19 -- the headline Mandela-",
      "Rules classification. ~38% of SIU person-stays (longer than 1 ",
      "day) meet international thresholds for either solitary ",
      "confinement (28.4%, N=556) or torture (9.9%, N=195) under UN ",
      "Mandela Rules. CSC's post-Bill-C-83 SIU regime -- designed ",
      "precisely to avoid these international classifications -- does ",
      "not in fact do so for ~38% of stays."
    ),
    payload = list(
      table19 = t,
      n_problematic = n_problematic,
      pct_problematic = pct_problematic
    )
  )
}


#' Sprott-Doob (Feb 2021) Table 23: regional torture/solitary rates
#'
#' @return A \code{morie_siu_result}.
#' @examples
#' morie_siu_sprott_doob_table23()$payload$pac_on_torture_ratio
#' @export
morie_siu_sprott_doob_table23 <- function() {
  t <- .SD_TABLE23_REGIONAL_TORTURE_RATES
  pacific <- t$torture_rate[t$region == "Pacific"]
  ontario <- t$torture_rate[t$region == "Ontario"]
  ratio <- pacific / ontario
  .morie_siu_rich(
    title = paste0(
      "Sprott & Doob (Feb 2021) Table 23 -- Regional rates of ",
      "Solitary Confinement and Torture per 1,000 prisoners"
    ),
    summary_lines = list(
      Source = "Sprott & Doob (Feb 2021), p. 4 of 28",
      `Pacific torture rate / 1000` = pacific,
      `Ontario torture rate / 1000` = ontario,
      `Pacific/Ontario torture ratio` =
        sprintf("%.1fx (Pacific is the report's 'alarmingly high' region)",
                ratio)
    ),
    tables = list(list(
      title = "Table 23 -- Regional rates per 1000 prisoners",
      data = t
    )),
    interpretation = sprintf(paste0(
      "Reproduces Sprott & Doob's Table 23. The Pacific region's ",
      "torture rate (39.1 per 1000 prisoners) is %.1fx the Ontario ",
      "rate (1.73). This regional disparity is the empirical core of ",
      "the Sprott-Doob argument that SIU placement decisions vary ",
      "structurally across CSC regions in ways that no population-",
      "level explanation can account for."
    ), ratio),
    payload = list(table23 = t, pac_on_torture_ratio = ratio)
  )
}


#' Sprott-Doob (Feb 2021) Table 4: length-of-stay distribution
#'
#' @return A \code{morie_siu_result}.
#' @export
morie_siu_sprott_doob_table4 <- function() {
  t <- .SD_TABLE4_LENGTH_OF_STAY
  short <- sum(t$pct[t$days %in% c("1-5", "6-15")])
  long  <- sum(t$pct[t$days %in% c("16-31", "32-61", "62-380")])
  .morie_siu_rich(
    title = paste0(
      "Sprott & Doob (Feb 2021) Table 4 -- SIU length-of-stay ",
      "distribution (N=1,983 admissions Nov 2019 - Sept 2020)"
    ),
    summary_lines = list(
      N = .SD_TABLE4_N,
      `<=15 day stays` = sprintf("%.1f%%", short),
      `>=16 day stays` = sprintf("%.1f%%", long),
      `>=62 day stays (longest bin)` =
        sprintf("%.1f%%", t$pct[t$days == "62-380"])
    ),
    tables = list(list(title = "Length-of-stay distribution", data = t)),
    interpretation = paste0(
      "Reproduces Table 4. Roughly half of SIU stays are 15 days or ",
      "shorter, but 20.8% (N=413) are >=62 days -- well past the ",
      "Mandela Rule 43 'prolonged' threshold."
    ),
    payload = list(table4 = t)
  )
}


#' Sprott-Doob (Feb 2021) Table 11: Region x stay length
#'
#' @return A \code{morie_siu_result}.
#' @export
morie_siu_sprott_doob_table11 <- function() {
  .morie_siu_rich(
    title = "Sprott & Doob (Feb 2021) Table 11 -- Region x total days in SIU",
    summary_lines = list(
      Source = "Sprott & Doob (Feb 2021), p.18",
      `Pearson chi2` = .SD_TABLE11_CHISQ$chi2,
      df = .SD_TABLE11_CHISQ$df,
      p = sprintf("<%.3f", .SD_TABLE11_CHISQ$p),
      Headline = paste0(
        "Region distribution of stays differs significantly across ",
        "length bins"
      )
    ),
    tables = list(list(
      title = paste0("Region x stay length (% of region in 62-380d bin ",
                      "shows long-stay concentration)"),
      data = .SD_TABLE11_REGION_X_STAY_LENGTH
    )),
    payload = list(
      table11 = .SD_TABLE11_REGION_X_STAY_LENGTH,
      chisq = .SD_TABLE11_CHISQ
    )
  )
}


#' Sprott-Doob (Feb 2021) Table 12: regional over-/under-representation
#'
#' @return A \code{morie_siu_result}.
#' @export
morie_siu_sprott_doob_table12 <- function() {
  t <- .SD_TABLE12_REGIONAL_OVERREP
  t$over_under_ratio <- ifelse(t$pop_pct > 0,
                                t$siu_pct / t$pop_pct, NA_real_)
  .morie_siu_rich(
    title = paste0(
      "Sprott & Doob (Feb 2021) Table 12 -- Regional over-/under-",
      "representation in SIU stays vs. Dec 2020 penitentiary population"
    ),
    summary_lines = list(
      Source = "Sprott & Doob (Feb 2021), p.18",
      `Quebec ratio` = "1.88x (37.3% / 19.8%)",
      `Ontario ratio` = "0.31x (8.5% / 27.5%)",
      Headline = paste0(
        "Quebec stays are 1.88x over-represented and Ontario stays are ",
        "0.31x under-represented in SIU vs. their share of the federal ",
        "prison population"
      )
    ),
    tables = list(list(
      title = "Region -- SIU share vs. prison-pop share + ratio",
      data = t
    )),
    payload = list(table12 = t)
  )
}


#' Sprott-Doob (Feb 2021) Table 15: Region x MH-flag
#'
#' @return A \code{morie_siu_result}.
#' @export
morie_siu_sprott_doob_table15 <- function() {
  .morie_siu_rich(
    title = paste0(
      "Sprott & Doob (Feb 2021) Table 15 -- Region x Mental-health ",
      "flag at SIU entry (N=2,279)"
    ),
    summary_lines = list(
      `Pearson chi2` = .SD_TABLE15_CHISQ$chi2,
      df = .SD_TABLE15_CHISQ$df,
      p = sprintf("<%.3f", .SD_TABLE15_CHISQ$p),
      `Range across regions` = "23.1% (Quebec/Ontario) to 34.2% (Atlantic)",
      `Overall MH-flag rate` = "28.0%"
    ),
    tables = list(list(
      title = "Region x MH flag at SIU entry",
      data = .SD_TABLE15_REGION_X_MENTAL_HEALTH
    )),
    payload = list(
      table15 = .SD_TABLE15_REGION_X_MENTAL_HEALTH,
      chisq = .SD_TABLE15_CHISQ
    )
  )
}


#' Sprott-Doob (Feb 2021) Table 22: Region x Mandela groups
#'
#' @return A \code{morie_siu_result}.
#' @export
morie_siu_sprott_doob_table22 <- function() {
  .morie_siu_rich(
    title = paste0(
      "Sprott & Doob (Feb 2021) Table 22 -- Region x Mandela ",
      "classification (N=1,960)"
    ),
    summary_lines = list(
      `Pearson chi2` = .SD_TABLE22_CHISQ$chi2,
      df = .SD_TABLE22_CHISQ$df,
      p = sprintf("<%.3f", .SD_TABLE22_CHISQ$p),
      `Quebec solitary share` = "40.6% (N=295)",
      `Pacific torture share` = "19.5% (N=69)",
      Headline = paste0(
        "Pacific has the highest torture share (19.5%); Quebec has ",
        "the highest solitary share (40.6%)"
      )
    ),
    tables = list(list(
      title = "Region x Mandela classification",
      data = .SD_TABLE22_REGION_X_MANDELA
    )),
    payload = list(
      table22 = .SD_TABLE22_REGION_X_MANDELA,
      chisq = .SD_TABLE22_CHISQ
    )
  )
}


#' Sprott-Doob-Iftene (May 2021) Table 1: IEDM-reviewed population
#'
#' @return A \code{morie_siu_result}.
#' @export
morie_siu_sprott_doob_iftene_table1 <- function() {
  sections <- list(
    list(title = "Gender (N=265)",
         data = .SD_TABLE1_IEDM_POPULATION$gender),
    list(title = "Age group (N=265)",
         data = .SD_TABLE1_IEDM_POPULATION$age_group),
    list(title = "Race (N=265)",
         data = .SD_TABLE1_IEDM_POPULATION$race),
    list(title = "Mental health flag (N=265)",
         data = .SD_TABLE1_IEDM_POPULATION$mental_health)
  )
  .morie_siu_rich(
    title = paste0(
      "Sprott, Doob & Iftene (May 2021) Table 1 -- Population ",
      "characteristics of SIU stays receiving >=1 IEDM review under ",
      "CCRA s.37.8"
    ),
    summary_lines = list(
      Source = "Sprott, Doob & Iftene (9 May 2021), p. 7 of 23",
      `N total stays reviewed by an IEDM` =
        .SD_TABLE1_IEDM_POPULATION$n_total,
      `Female / Male` = "0.8% (2) vs 99.2% (263)",
      `Indigenous share` = "40.4% (107)",
      `Black share` = "15.8% (42)",
      `Mental health need flag share` = "26.4% (71)"
    ),
    tables = sections,
    interpretation = paste0(
      "Reproduces Sprott, Doob & Iftene's Table 1. Indigenous people ",
      "make up 40.4% of IEDM-reviewed SIU stays -- a stark over-",
      "representation against the Indigenous share of the Canadian ",
      "adult population (~5%). Black people are 15.8% of stays vs ",
      "~4% of the adult population (~4x over-representation). The ",
      "mental-health flag applies to 26.4% of reviewed stays."
    ),
    payload = list(
      table1_iedm_population = .SD_TABLE1_IEDM_POPULATION
    )
  )
}


#' Sprott-Doob-Iftene (May 2021) Table 9: IEDM review outcomes
#'
#' @return A \code{morie_siu_result}.
#' @export
morie_siu_sprott_doob_iftene_table9 <- function() {
  t <- .SD_TABLE9_MAY2021_IEDM_DECISIONS
  pct_non_removal <- sum(t$pct[grepl("remain|transferred", t$decision,
    ignore.case = TRUE)])
  .morie_siu_rich(
    title = paste0(
      "Sprott, Doob & Iftene (May 2021) Table 9 -- IEDM review ",
      "outcomes (N=380 reviews from 265 stays)"
    ),
    summary_lines = list(
      `N reviews with outcomes` = .SD_TABLE9_MAY2021_TOTAL,
      `'Remove' decisions` = "33 (8.7%)",
      `'Remain' decisions` = "224 (58.9%)",
      `Pre-empted by CSC` = "115 (30.3%)",
      `Decision moot` = "8 (2.1%)",
      `% non-removal outcomes` =
        sprintf("%.1f%% -- either pre-emption or 'remain'",
                pct_non_removal)
    ),
    tables = list(list(
      title = "IEDM review outcomes (s.37.8)",
      data = t
    )),
    interpretation = paste0(
      "Reproduces Table 9. Of 380 IEDM reviews with outcomes, fewer ",
      "than 1 in 11 resulted in a 'remove from SIU' decision. Combined ",
      "with the 30% of cases that CSC pre-empted (transferred the ",
      "prisoner BEFORE the IEDM could rule), this severely limits ",
      "IEDMs' practical ability to order earlier release."
    ),
    payload = list(table9 = t)
  )
}


#' Sprott-Doob-Iftene (May 2021) Table 10: per-IEDM decision variance
#'
#' @return A \code{morie_siu_result}.
#' @export
morie_siu_sprott_doob_iftene_table10 <- function() {
  pcts <- .SD_TABLE10_MAY2021_PER_IEDM$remain_pct
  .morie_siu_rich(
    title = paste0(
      "Sprott, Doob & Iftene (May 2021) Table 10 -- Per-IEDM ",
      "decision variance (12 anonymized IEDMs)"
    ),
    summary_lines = list(
      `Pearson chi2` = .SD_TABLE10_MAY2021_CHISQ$chi2,
      df = .SD_TABLE10_MAY2021_CHISQ$df,
      p = sprintf("<%.3f", .SD_TABLE10_MAY2021_CHISQ$p),
      `Min 'remain' rate` = sprintf("%.1f%% (IEDM #8)", min(pcts)),
      `Max 'remain' rate` = sprintf("%.1f%% (IEDM #11)", max(pcts)),
      Range = sprintf("%.1f percentage points", max(pcts) - min(pcts)),
      Headline = paste0(
        "Substantial variance across IEDMs -- one decided 37.5% should ",
        "remain; another decided 85.7% should remain"
      )
    ),
    tables = list(list(
      title = "Per-IEDM (anonymized 1-12) decisions",
      data = .SD_TABLE10_MAY2021_PER_IEDM
    )),
    payload = list(
      table10 = .SD_TABLE10_MAY2021_PER_IEDM,
      chisq = .SD_TABLE10_MAY2021_CHISQ,
      min_remain_pct = min(pcts),
      max_remain_pct = max(pcts)
    )
  )
}


#' Sprott-Doob-Iftene (May 2021) Table 15: long-stay no-IEDM cases
#'
#' @return A \code{morie_siu_result}.
#' @export
morie_siu_sprott_doob_iftene_table15 <- function() {
  .morie_siu_rich(
    title = paste0(
      "Sprott, Doob & Iftene (May 2021) Table 15 -- Long-stay SIU ",
      "cases with NO IEDM record (N=1,979)"
    ),
    summary_lines = list(
      `N total stays examined` = .SD_TABLE15_MAY2021_TOTAL,
      `Long-stay (>=76d) with NO IEDM` =
        sprintf("%d cases (29+27+49)",
                .SD_TABLE15_MAY2021_LONG_STAY_NO_IEDM),
      `>120d with NO IEDM` =
        "49 cases (35.8% of 137 long-stay >120d cases)",
      Headline = paste0(
        "105 cases stayed 76+ days in an SIU with no IEDM record -- ",
        "apparent compliance failure with CCRA s.37.8"
      )
    ),
    tables = list(list(
      title = "SIU stay length x IEDM-flag",
      data = .SD_TABLE15_MAY2021_NO_IEDM_BY_STAY_LENGTH
    )),
    interpretation = paste0(
      "Reproduces Table 15. The CCRA requires IEDM review for stays ",
      "beyond 60 days, but 105 stays of 76+ days in this dataset had ",
      "NO record of any IEDM review. Even more striking: 49 of the ",
      "137 stays >120 days (35.8%) had no IEDM record. This is a ",
      "structural failure of the SIU oversight regime."
    ),
    payload = list(
      table15_may2021 = .SD_TABLE15_MAY2021_NO_IEDM_BY_STAY_LENGTH,
      long_stay_no_iedm = .SD_TABLE15_MAY2021_LONG_STAY_NO_IEDM
    )
  )
}


# ---------------------------------------------------------------------------
# Chi-square verifier
# ---------------------------------------------------------------------------

#' Recompute Pearson chi-square from a 2D contingency table
#'
#' Pure-base-R Pearson chi-square without Yates correction. Intended
#' for quick self-checks of the transcribed cell counts against the
#' published chi-square values.
#'
#' @param observed A 2D matrix or data frame of non-negative counts.
#' @return A named list with elements \code{chi2}, \code{df},
#'   \code{p_value}, \code{expected}, and \code{n}.
#' @examples
#' morie_siu_verify_chi2(matrix(c(10, 10, 10, 10), nrow = 2))$chi2
#' @export
morie_siu_verify_chi2 <- function(observed) {
  obs <- as.matrix(observed)
  storage.mode(obs) <- "double"
  if (any(obs < 0, na.rm = TRUE)) {
    stop("`observed` must contain non-negative counts.", call. = FALSE)
  }
  n <- sum(obs)
  row_tot <- rowSums(obs)
  col_tot <- colSums(obs)
  expected <- outer(row_tot, col_tot) / n
  contrib <- ifelse(expected > 0, (obs - expected)^2 / expected, 0)
  chi2 <- sum(contrib)
  df <- (nrow(obs) - 1L) * (ncol(obs) - 1L)
  p <- if (df > 0L) stats::pchisq(chi2, df, lower.tail = FALSE) else 1
  list(
    chi2 = round(chi2, 2),
    df = as.integer(df),
    p_value = round(p, 6),
    expected = round(expected, 2),
    n = as.integer(n)
  )
}


#' Verify every published chi-square against its transcribed cells
#'
#' Cross-checks Sprott-Doob Tables 11, 15, 22 (Feb 2021) and Sprott-
#' Doob-Iftene Tables 5, 10 (May 2021) by recomputing the chi-square
#' from each transcribed contingency table and comparing it to the
#' published value. A "pass" means the recomputed chi-square is within
#' rounding tolerance (1.0-1.5 units) of the published value.
#'
#' @return A \code{morie_siu_result} with the verification table and
#'   per-table warnings for any mismatch.
#' @examples
#' v <- morie_siu_verify_published_chi_squares()
#' v$payload$n_pass
#' @export
morie_siu_verify_published_chi_squares <- function() {
  cases <- list(
    list(
      label = "SD-2021-Feb T11 Region x stay length",
      obs = as.matrix(.SD_TABLE11_REGION_X_STAY_LENGTH[, c(
        "1-5", "6-15", "16-31", "32-61", "62-380"
      )]),
      published = .SD_TABLE11_CHISQ, tol = 1.0
    ),
    list(
      label = "SD-2021-Feb T15 Region x MH",
      obs = as.matrix(.SD_TABLE15_REGION_X_MENTAL_HEALTH[, c(
        "no_mh", "yes_mh"
      )]),
      published = .SD_TABLE15_CHISQ, tol = 1.0
    ),
    list(
      label = "SD-2021-Feb T22 Region x Mandela",
      obs = as.matrix(.SD_TABLE22_REGION_X_MANDELA[, c(
        "solitary", "torture", "everyone_else"
      )]),
      published = .SD_TABLE22_CHISQ, tol = 1.5
    ),
    list(
      label = "SDI-2021-May T5 Race x #reviews",
      obs = as.matrix(.SD_TABLE5_MAY2021_RACE_X_REVIEWS[, c(
        "one", "two_plus"
      )]),
      published = .SD_TABLE5_MAY2021_CHISQ, tol = 1.0
    ),
    list(
      label = "SDI-2021-May T10 Per-IEDM",
      obs = as.matrix(.SD_TABLE10_MAY2021_PER_IEDM[, c(
        "remain", "removed_or_moot"
      )]),
      published = .SD_TABLE10_MAY2021_CHISQ, tol = 1.5
    )
  )
  rows <- vector("list", length(cases))
  warns <- character()
  for (i in seq_along(cases)) {
    cs <- cases[[i]]
    v <- morie_siu_verify_chi2(cs$obs)
    ok <- abs(v$chi2 - cs$published$chi2) < cs$tol
    rows[[i]] <- data.frame(
      source = cs$label,
      recomputed_chi2 = v$chi2,
      recomputed_df = v$df,
      published_chi2 = cs$published$chi2,
      published_df = cs$published$df,
      pass = ok,
      stringsAsFactors = FALSE
    )
    if (!ok) {
      warns <- c(warns, sprintf(
        "%s: recomputed chi2 = %.2f differs from published %.2f by > %.1f",
        cs$label, v$chi2, cs$published$chi2, cs$tol
      ))
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  n_pass <- sum(out$pass)
  .morie_siu_rich(
    title = paste0(
      "chi-square verification -- recomputed from transcribed cell ",
      "counts vs. published values"
    ),
    summary_lines = list(
      `Tables verified` = nrow(out),
      `Pass count` = sprintf("%d/%d", n_pass, nrow(out)),
      Method = "Pearson chi2 without Yates correction",
      Tolerance = "1.0-1.5 chi2 units (rounding in published values)"
    ),
    tables = list(list(
      title = paste0("Recomputed chi-square vs. published (pass = ",
                      "within rounding tolerance)"),
      data = out
    )),
    interpretation = paste0(
      "Recomputes every published chi-square from the transcribed ",
      "contingency-table cells. A pass means the recomputed value is ",
      "within rounding tolerance of the published value -- confirming ",
      "the transcription is accurate and the published statistic is ",
      "correctly derived. A failure indicates either a transcription ",
      "error or a difference in the chi-square formula (e.g., Yates ",
      "correction)."
    ),
    warnings = warns,
    payload = list(
      n_pass = n_pass,
      n_total = nrow(out),
      verification_table = out
    )
  )
}


#' Comprehensive replication of Sprott & Doob (Feb 2021)
#'
#' Bundles Tables 13, 19, and 23 (the three headline tables) into a
#' single \code{morie_siu_result} with cross-references.
#'
#' @return A \code{morie_siu_result}.
#' @examples
#' morie_siu_sprott_doob_feb2021()$payload$headline_findings$n_total_stays
#' @export
morie_siu_sprott_doob_feb2021 <- function() {
  t13 <- morie_siu_sprott_doob_table13()
  t19 <- morie_siu_sprott_doob_table19()
  t23 <- morie_siu_sprott_doob_table23()
  .morie_siu_rich(
    title = paste0(
      "Sprott & Doob (Feb 2021) -- Solitary Confinement, Torture, ",
      "and Canada's SIUs (CRIMSL UToronto)"
    ),
    summary_lines = list(
      Authors = "Jane B. Sprott (TMU/Ryerson) & Anthony N. Doob (UofT)",
      Date = "23 February 2021",
      URL = paste0("crimsl.utoronto.ca/.../",
                    "TortureSolitarySIUsSprottDoob23Feb2021_0.pdf"),
      `N total person-stays` = .SD_HEADLINE_FINDINGS$n_total_stays,
      `% missed full 4 hrs every day` =
        sprintf("%.1f%%",
                .SD_HEADLINE_FINDINGS$missed_full_4hrs_overall_pct),
      `Long-stay missed 4 hrs in >=76% of days` =
        sprintf("%.0f%%",
                .SD_HEADLINE_FINDINGS$long_stay_missed_4hrs_in_76pct_of_days),
      `Pacific/Ontario torture ratio` =
        sprintf("%.1fx", .SD_HEADLINE_FINDINGS$pacific_to_ontario_ratio)
    ),
    tables = list(
      list(title = paste0("Section: Table 13 -- ", t13$title),
           data = t13$payload$table13),
      list(title = paste0("Section: Table 19 -- ", t19$title),
           data = t19$payload$table19),
      list(title = paste0("Section: Table 23 -- ", t23$title),
           data = t23$payload$table23)
    ),
    interpretation = paste0(
      "Comprehensive replication of the three core tables from Sprott ",
      "& Doob's third CRIMSL report (Feb 2021). The report's central ",
      "empirical contribution is the Mandela-Rules classifier ",
      "(operationalized in `morie_siu_classify_mandela()`): 28.4% of ",
      "SIU stays meet the UN definition of solitary confinement, ",
      "9.9% meet the definition of torture or other cruel, inhuman, ",
      "or degrading treatment. The regional disparities -- Pacific ",
      "22.6x Ontario for torture rate -- point to decision-making ",
      "rather than population characteristics as the driver."
    ),
    payload = list(
      table13 = .SD_TABLE13_REGIONAL_RATES_PER_1000,
      table19 = .SD_TABLE19_MANDELA_CLASSIFICATION,
      table23 = .SD_TABLE23_REGIONAL_TORTURE_RATES,
      headline_findings = .SD_HEADLINE_FINDINGS
    )
  )
}
