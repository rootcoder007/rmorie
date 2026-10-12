# P7, the law of crime concentration: how much of Toronto's concentration is chance?
#
# Data: Toronto Police Service neighbourhood crime counts, 158 neighbourhoods, 2014-2025
# (City of Toronto open data, "Neighbourhood Crime Rates"). The Gini of the counts across
# neighbourhoods is compared with what a Poisson null (every neighbourhood the same rate)
# produces at the same mean: the proved bound Gini <= CV / sqrt(2), the simulated null mean and
# 95th percentile, and the share of null draws at or above the observed Gini
# (Research.P7.Bound.gini_le_cv, Research.P7.Mixture).
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

f <- need_file(dpath("OT", "neighbourhood-crime-rates - 4326.csv"))
ncr <- utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
offences <- c("ASSAULT", "AUTOTHEFT", "BIKETHEFT", "BREAKENTER", "HOMICIDE", "ROBBERY", "SHOOTING",
              "THEFTFROMMV", "THEFTOVER")
years <- sort(as.integer(sub("^ASSAULT_", "", grep("^ASSAULT_[0-9]{4}$", names(ncr), value = TRUE))))

one <- function(off, yr) {
  x <- as.numeric(ncr[[paste0(off, "_", yr)]])
  x <- x[!is.na(x)]
  r <- morie_concentration_null_bound(x, n_sim = 2000L, seed = 1L)
  data.frame(offence = off, year = yr, places = length(x), total = sum(x), mean = r$mean,
             gini = r$gini, cv_bound = r$cv_bound, null_bound = r$null_bound, null_mean = r$null_mean,
             null_q95 = r$null_q95, excess = r$excess, p_value = r$p_value, stringsAsFactors = FALSE)
}

latest <- max(years)
by_offence <- do.call(rbind, lapply(offences, one, yr = latest))
bne_years <- do.call(rbind, lapply(years, function(y) one("BREAKENTER", y)))
pop <- as.numeric(ncr[[paste0("POPULATION_", latest)]])

lines <- c(
  sprintf("# P7: concentration of crime across Toronto's %d neighbourhoods", nrow(ncr)),
  "",
  sprintf("Source: City of Toronto / Toronto Police Service, Neighbourhood Crime Rates (158 neighbourhoods, %d-%d).", min(years), max(years)),
  "Statistic: the Gini of the neighbourhood counts. Null: every neighbourhood draws a Poisson count at the",
  "common mean (2,000 simulations, seed 1). Bound: Gini <= CV / sqrt(2), so a Poisson null caps the Gini at",
  "about 1 / sqrt(2 mean) (`Research.P7.Bound.gini_le_cv`, `morie_concentration_null_bound()`).",
  "",
  sprintf("## Every offence, %d", latest),
  "",
  md_table(by_offence[, c("offence", "total", "mean", "gini", "null_bound", "null_mean", "null_q95", "excess", "p_value")]),
  "",
  "## Break and enter, every year",
  "",
  md_table(bne_years[, c("year", "total", "mean", "gini", "null_bound", "null_mean", "null_q95", "excess", "p_value")]),
  "",
  "## Reading",
  "",
  sprintf(paste("For every offence with more than a handful of events the observed Gini sits far above the",
                "Poisson null's mean and its 95th percentile (p = 0 in 2,000 draws): the concentration is not the",
                "arithmetic of rare counts. Homicide and shootings (means of %s and %s per neighbourhood) are the",
                "cases the null bound warns about: their null Ginis are already %s and %s, so most of their",
                "observed concentration is what a Poisson count at that mean produces by itself."),
          fmt(by_offence$mean[by_offence$offence == "HOMICIDE"], 2), fmt(by_offence$mean[by_offence$offence == "SHOOTING"], 2),
          fmt(by_offence$null_mean[by_offence$offence == "HOMICIDE"], 2), fmt(by_offence$null_mean[by_offence$offence == "SHOOTING"], 2)),
  "",
  sprintf(paste("The null is the equal-rate Poisson; neighbourhood populations range from %s to %s, so part of the",
                "excess is population, not concentration of risk. A rate-adjusted null (Poisson at a mean proportional",
                "to population) is the next step and needs a proved bound of its own."),
          fmt(min(pop, na.rm = TRUE), 0), fmt(max(pop, na.rm = TRUE), 0)),
  ""
)
write_result("p07_concentration_toronto", lines)
