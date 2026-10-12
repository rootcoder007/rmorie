# NCVS 1992-2022 (ICPSR 38604): the victimisation-survey applications of the ledger.
#
# Data: United States. Bureau of Justice Statistics. National Crime Victimization Survey,
# Concatenated File, [United States], 1992-2022. ICPSR [distributor], 2023-09-18.
# https://doi.org/10.3886/ICPSR38604.v1 -- served as ncvs_38604/{incident, person_1992_2006,
# person_2007_2022, household_1992_2006, household_2007_2022}
# at data.rmorie.com (`rmorie pull ncvs_38604/incident`); the ICPSR codebooks, user guide and
# terms of use sit beside them under files/ncvs_38604/.
#
# What it does, in the ledger's terms:
#   P1  the survey/record pair at last: victimisations the survey counts (v_obs) against the
#       ones the victim says the police learned of (r), by year and crime class, with the
#       partial-identification interval of Research.P1 under stated misreport bounds and the
#       breakdown value of the 1993 -> 2022 decline (Research.P1Breakdown).
#   P7  the law of crime concentration on victims: victimisations per person-year (and per
#       household-year), the Gini decomposition of Research.P7 against the Poisson null.
#   P10 the Bartlett non-identification (Research.P10.Moments): the first two moments of the
#       person-year counts, which contagion and heterogeneity fit alike.
#   A14 the comparable-subset rate (Research.P1.Comparable): the recording rate the survey
#       identifies, and the width the dark figure would have for any recorded mass outside
#       the survey's offence set.
# Usage: Rscript ncvs_victimisation.R [data_dir] [out_dir]
suppressPackageStartupMessages({
  library(rmorie)
  library(data.table)
})
args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[[1]] else Sys.getenv("MORIE_DATA_ROOT", "~/data/datasets")
out_dir <- if (length(args) >= 2) args[[2]] else "research/applications/results"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
# the hosted tables: incident whole; person and household in two year ranges each
csv <- function(t) file.path(data_dir, "ncvs_38604", "csv", paste0(t, ".csv.gz"))
read_split <- function(t, select) rbindlist(lapply(c("_1992_2006", "_2007_2022"), function(h) {
  fread(csv(paste0(t, h)), select = select, showProgress = FALSE)
}))
code <- function(x) suppressWarnings(as.integer(sub("^\\((-?[0-9]+)\\).*$", "\\1", as.character(x))))

inc <- fread(csv("incident"), select = c("YEAR", "IDHH", "IDPER", "WGTVICCY", "V4016", "V4017", "V4018", "V4019",
                                          "V4399", "V4529"), showProgress = FALSE)
per <- read_split("person", c("YEAR", "IDPER", "WGTPERCY"))
hh <- read_split("household", c("YEAR", "IDHH", "WGTHHCY"))

# the BJS counting rule: a series victimisation (6 or more similar incidents the respondent cannot
# detail) counts as the number reported, up to 10; everything else counts once
toc <- code(inc$V4529)
n16 <- suppressWarnings(as.numeric(as.character(inc$V4016)))
series <- code(inc$V4017) == 2L & code(inc$V4018) == 1L & code(inc$V4019) == 2L
series[is.na(series)] <- FALSE
mult <- ifelse(series, pmin(ifelse(is.na(n16) | n16 < 6, 10, n16), 10), 1)
inc[, mult := mult]   # a column, so the filter below keeps it aligned with its rows
inc[, w := as.numeric(WGTVICCY) * mult]
inc[, class := fifelse(toc %in% 1:20, "violent", fifelse(toc %in% 31:59, "property", fifelse(toc %in% 21:23, "personal theft", NA_character_)))]
inc[, reported := code(V4399) == 1L]
inc <- inc[!is.na(class) & is.finite(w) & w > 0]

persons <- per[is.finite(WGTPERCY), .(persons = sum(WGTPERCY)), by = YEAR]
households <- hh[is.finite(WGTHHCY), .(households = sum(WGTHHCY)), by = YEAR]

# ---- P1: the survey/record pair by year and class -----------------------------------------------
by_year <- inc[, .(victimisations = sum(w), reported = sum(w * reported, na.rm = TRUE),
                   reported_share = sum(w * reported, na.rm = TRUE) / sum(w)), by = .(YEAR, class)]
by_year <- merge(by_year, persons, by = "YEAR")
by_year <- merge(by_year, households, by = "YEAR")
by_year[, denom := fifelse(class == "property", households, persons)]
by_year[, `:=`(v_obs = victimisations / denom, r = reported / denom)]
setorder(by_year, class, YEAR)
alpha_max <- 0.05   # a victim counting a non-crime as a crime: at most 5 percent of reports
beta_max <- 0.25    # a crime the survey misses (non-recall, refusal): at most a quarter
p1 <- by_year[, {
  b <- morie_dark_figure_bounds(v_obs = pmin(v_obs, 1), r = pmin(r, 1), alpha_max = alpha_max, beta_max = beta_max)
  list(v_obs = v_obs, r = r, v_lower = b$v_lower, v_upper = b$v_upper, dark_lower = b$dark_lower,
       dark_upper = b$dark_upper, ratio_upper = b$ratio_upper, reported_share = reported_share)
}, by = .(YEAR, class)]
# breakdown of the decline: how much under-reporting in 2022 (beta) would it take for the 2022 upper
# bound to reach the 1993 lower bound, per class (Research.P1Breakdown: beta* = 1 - v_obs / t)
breakdown <- p1[YEAR %in% c(1993, 2022), .(YEAR, class, v_obs, v_lower)]
bd <- breakdown[, {
  v22 <- v_obs[YEAR == 2022]
  low93 <- v_lower[YEAR == 1993]
  list(v_obs_2022 = v22, v_lower_1993 = low93, beta_star = 1 - v22 / low93,
       decline_survives_beta_max = (v22 / (1 - beta_max)) < low93)
}, by = class]

# ---- P7 and P10: concentration and moments of victimisations per person-year and household-year ----
person_counts <- function(y) {
  ci <- inc[YEAR == y & class %in% c("violent", "personal theft"), .(n = sum(mult)), by = IDPER]
  base <- per[YEAR == y, .(IDPER)]
  base <- unique(base)
  base <- merge(base, ci, by = "IDPER", all.x = TRUE)
  base[is.na(n), n := 0]
  base$n
}
household_counts <- function(y) {
  ci <- inc[YEAR == y & class == "property", .(n = sum(mult)), by = IDHH]
  base <- unique(hh[YEAR == y, .(IDHH)])
  base <- merge(base, ci, by = "IDHH", all.x = TRUE)
  base[is.na(n), n := 0]
  base$n
}
conc <- rbindlist(lapply(c(1994, 2008, 2022), function(y) {
  rbindlist(lapply(list(list(unit = "person (violent + personal theft)", x = person_counts(y)),
                        list(unit = "household (property)", x = household_counts(y))), function(u) {
    x <- as.integer(round(u$x))
    d <- morie_concentration_decompose(x)
    data.table(YEAR = y, unit = u$unit, n = d$n, mean_count = d$mean_count, variance = var(x),
               dispersion = var(x) / mean(x), zero_share = d$zero_share, null_zero_share = d$null_zero_share,
               excess_zero_share = d$excess_zero_share, gini_all = d$gini_all, gini_positive = d$gini_positive,
               null_gini_same_positive = d$null_gini_same_positive,
               top1_share = {
                 s <- sort(x, decreasing = TRUE)
                 sum(s[seq_len(max(1, length(x) %/% 100))]) / sum(x)
               })
  }))
}))

# ---- A14: the comparable subset --------------------------------------------------------------
# the survey covers rape and sexual assault, robbery, assault, personal theft, burglary, motor
# vehicle theft and theft; the recording rate it identifies is the reported share on that set.
# For recorded offences outside it (homicide, arson, commercial crime: R_B, which the survey never
# sees) the dark figure's interval widens by R_B (1 - lambda r_A) / (lambda r_A).
a14 <- p1[YEAR == 2022, .(class, rate_A = reported_share)]
a14[, width_per_recorded_outside := vapply(rate_A, function(rA) (1 - 0.5 * rA) / (0.5 * rA), numeric(1))]  # lambda = 1/2

# ---- write ----------------------------------------------------------------------------------------
fmt <- function(x, d = 3) formatC(x, digits = d, format = "f")
md <- c("# NCVS 1992-2022: the victimisation applications of the ledger", "",
        "Data: ICPSR 38604 (BJS, National Crime Victimization Survey, Concatenated File 1992-2022),",
        "https://doi.org/10.3886/ICPSR38604.v1, served as `ncvs_38604/{incident,person,household}` at",
        "data.rmorie.com with the ICPSR codebooks, user guide and terms of use. Weights: WGTVICCY per",
        "victimisation with the BJS series rule (a series counts its reported incidents, at most 10);",
        "WGTPERCY persons and WGTHHCY households as denominators. Reported = V4399 \"Were the police",
        "informed\" = Yes. Classes: violent = TOC 1-20, property = 31-59, personal theft = 21-23.", "",
        sprintf("Script: `research/applications/ncvs_victimisation.R` (run %s).", format(Sys.Date())), "",
        "## P1: the survey/record pair (Research.P1.survey_record_interval, dark-figure version)", "",
        sprintf("Misreport bounds stated: alpha_max = %s (over-report), beta_max = %s (under-report).", alpha_max, beta_max),
        "Rates per person (violent, personal theft) or per household (property); v_obs is the survey",
        "rate, r the rate the police learned of; [v_lower, v_upper] the identified set for the true rate.", "",
        "| year | class | v_obs | r | reported share | v_lower | v_upper | dark figure (lower, upper) | ratio upper |",
        "|---|---|---|---|---|---|---|---|---|")
for (i in seq_len(nrow(p1))) {
  if (!p1$YEAR[i] %in% c(1993, 1998, 2003, 2008, 2013, 2018, 2022)) next
  md <- c(md, sprintf("| %d | %s | %s | %s | %s | %s | %s | (%s, %s) | %s |", p1$YEAR[i], p1$class[i], fmt(p1$v_obs[i], 4),
                      fmt(p1$r[i], 4), fmt(p1$reported_share[i]), fmt(p1$v_lower[i], 4), fmt(p1$v_upper[i], 4),
                      fmt(p1$dark_lower[i], 4), fmt(p1$dark_upper[i], 4), fmt(p1$ratio_upper[i], 2)))
}
md <- c(md, "", "### Breakdown of the 1993 to 2022 decline (Research.P1Breakdown)", "",
        "beta* is the under-reporting in 2022 that would lift the 2022 upper bound to the 1993 lower bound;",
        "the decline survives the stated beta_max when the 2022 bound stays below it.", "",
        "| class | v_obs 2022 | v_lower 1993 | beta* | survives beta_max |", "|---|---|---|---|---|")
for (i in seq_len(nrow(bd))) md <- c(md, sprintf("| %s | %s | %s | %s | %s |", bd$class[i], fmt(bd$v_obs_2022[i], 4),
                                                   fmt(bd$v_lower_1993[i], 4), fmt(bd$beta_star[i]), bd$decline_survives_beta_max[i]))
md <- c(md, "", "## P7 and P10: concentration of victimisations on units (Research.P7, Research.P10.Moments)", "",
        "Counts per unit-year (zeros from the person and household files). gini_all is the Gini over",
        "every unit, gini_positive over the victimised; null_zero_share and null_gini_same_positive are",
        "what a Poisson with the same mean would give (Research.P7.gini_zero_decomposition,",
        "poisson_zero_prob). The dispersion index var/mean and the zero share are the two moments that",
        "contagion and heterogeneity fit alike (Research.P10.Moments): the data cannot tell them apart.", "",
        "| year | unit | n | mean | dispersion | zero share | Poisson zero | excess zero | Gini all | Gini victimised | Poisson Gini | top 1% share |",
        "|---|---|---|---|---|---|---|---|---|---|---|---|")
for (i in seq_len(nrow(conc))) md <- c(md, sprintf("| %d | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |", conc$YEAR[i], conc$unit[i],
                                                   format(conc$n[i], big.mark = ","), fmt(conc$mean_count[i], 4), fmt(conc$dispersion[i], 2),
                                                   fmt(conc$zero_share[i], 4), fmt(conc$null_zero_share[i], 4), fmt(conc$excess_zero_share[i], 4),
                                                   fmt(conc$gini_all[i]), fmt(conc$gini_positive[i]), fmt(conc$null_gini_same_positive[i]), fmt(conc$top1_share[i])))
md <- c(md, "", "## A14: the comparable subset (Research.P1.Comparable)", "",
        "The survey's recording rate r_A is identified on the offences it covers (the reported share,",
        "2022). For every recorded offence outside its reach (homicide, arson, commercial crime; a",
        "police count the survey never sees) the dark figure's interval is wider by the factor below",
        "under the assumption that such offences are recorded at least half as well as the comparable",
        "ones (lambda = 1/2): width = R_B (1 - lambda r_A) / (lambda r_A). The FBI count R_B is not held",
        "here, so the factor is reported, not the interval.", "",
        "| class | r_A (2022) | interval width per recorded offence outside the survey |", "|---|---|---|")
for (i in seq_len(nrow(a14))) md <- c(md, sprintf("| %s | %s | %s |", a14$class[i], fmt(a14$rate_A[i]), fmt(a14$width_per_recorded_outside[i], 2)))
writeLines(md, file.path(out_dir, "ncvs_victimisation.md"))
fwrite(p1, file.path(out_dir, "ncvs_p1_by_year.csv"))
fwrite(conc, file.path(out_dir, "ncvs_p7_concentration.csv"))
cat("NCVS_APP_DONE rows", nrow(p1), nrow(conc), "\n")
