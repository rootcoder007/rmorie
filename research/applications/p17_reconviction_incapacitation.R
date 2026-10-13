# P17 incapacitation, applied to published reconviction rates: Statistics Canada's Juristat on
# adults released from custody or community supervision in five provinces (2015/16 to 2018/19),
# and Ontario's two-year reconviction by risk level (2014/15 releases). Neither publishes
# offending, only convictions, so everything here is in convictions: the conviction rate while
# free, lambda q, is what a reconviction hazard measures, and the prevented share
# lambda q S / (1 + lambda q S) depends on lambda and q only through it (Research.P17).
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

num <- function(x) as.numeric(gsub(",", "", x))
# Juristat table 4: by quarter in the community, the probability of a first new conviction among
# those still conviction-free (rows 5 onward are the quarters)
t4 <- utils::read.csv(need_file(dpath("JURISTAT", "reconviction_2024_tbl04_grid.csv")), stringsAsFactors = FALSE)
q <- t4[grepl("^[0-9]+ to [0-9]+$", t4$c1), ]
hz <- data.frame(quarter = q$c1, custody = num(q$c5), community = num(q$c7), surv_custody = num(q$c4),
                 surv_community = num(q$c6))
# a conditional probability p over a quarter is a rate of -log(1 - p) per quarter
rate <- function(p) -log(1 - p)
paths <- lapply(c(custody = "custody", community = "community"), function(k) rate(hz[[k]]))

# one year (4 quarters) of custody imposed at release, a year later, and two years later
career <- do.call(rbind, lapply(names(paths), function(k) {
  do.call(rbind, lapply(c(0, 4, 8), function(t0) {
    r <- morie_incapacitation_career(paths[[k]], t0 = t0, S = 4)
    data.frame(released_from = k, starts_after_quarters = t0, prevented = r$prevented, upper = r$upper,
               lower = r$lower, later = r$later)
  }))
}))

# Ontario, institutional releases 2014/15: two-year reconviction share by risk level; the conviction
# rate while free is -log(1 - share)/2 a year, and a six-month or one-year term prevents the
# share h S / (1 + h S) of a constant-rate offender's convictions
on <- utils::read.csv(need_file(dpath("ONTARIO", "ontario_adult_releases_by_risk_2014_15.csv")), stringsAsFactors = FALSE)
on <- on[on$risk_level != "Total" & is.finite(on$inst_reconvicted_pct), ]
h <- -log(1 - on$inst_reconvicted_pct / 100) / 2
inc6 <- morie_incapacitation(lambda = h, q = 1, S = 0.5)
inc12 <- morie_incapacitation(lambda = h, q = 1, S = 1)
risk <- data.frame(risk_level = on$risk_level, releases = on$inst_releases, reconvicted_2y_pct = on$inst_reconvicted_pct,
                   conviction_rate_per_year = h, prevented_share_6m = inc6$prevented_share,
                   prevented_share_12m = inc12$prevented_share)

lines <- c(
  "## P17 incapacitation: what a custody year prevents, read from published reconviction", "",
  paste("Data: Statistics Canada, Juristat 85-002-X 2024001 article 00002 (reconvictions of adults",
        "released from custody or community supervision in five provincial programs, 2015/16 to",
        "2018/19), table 4; Government of Ontario, Rates of recidivism in Ontario, releases by risk",
        "level, 2014/15."), "",
  paste("Only convictions are published, so the unit is convictions: the conviction rate while free",
        "is lambda q, and the incapacitation identity's prevented share lambda q S/(1 + lambda q S)",
        "depends on lambda and q only through it (Research.P17.steady_state_rate,",
        "prevented_share_eq)."), "",
  "### The conviction hazard falls with time in the community", "",
  md_table(data.frame(quarter = hz$quarter, custody = hz$custody, community = hz$community,
                      custody_rate = round(paths$custody, 4), community_rate = round(paths$community, 4))), "",
  paste("Read as the rate path of a person still conviction-free, the hazard is non-increasing, which is",
        "the desistance assumption of Research.P17Replacement. A year of custody then prevents between",
        "S lambda(t0 + S) and S lambda(t0) first reconvictions (prevented_ge_const, prevented_le_const),",
        "and fewer when it is served later (later_sentence_prevents_less):"), "",
  md_table(career), "",
  sprintf(paste("A year imposed at release prevents %.3f first reconvictions per person released from",
                "custody, against %.3f from the entry-rate constant estimate (its upper bound); the same",
                "year served two years later prevents %.3f. For community supervision the figures are",
                "%.3f, %.3f and %.3f."),
          career$prevented[1], career$upper[1], career$prevented[3], career$prevented[4], career$upper[4],
          career$prevented[6]), "",
  "### A uniform term by risk level (Ontario, institutional releases)", "",
  md_table(risk), "",
  sprintf(paste("The prevented share rises with the conviction rate (Research.P17.high_rate_more_prevented):",
                "a uniform six-month term removes %.1f%% of a very-low-risk releasee's convictions and",
                "%.1f%% of a very-high-risk one's, %.0f times as much."),
          100 * risk$prevented_share_6m[1], 100 * risk$prevented_share_6m[nrow(risk)],
          risk$prevented_share_6m[nrow(risk)] / risk$prevented_share_6m[1]), "",
  paste("Caveats: the bounds above are theorems about one person's non-increasing rate path; the",
        "published path is the cohort's hazard among those still conviction-free, which falls both",
        "because people desist and because the most likely to be reconvicted leave the risk set first.",
        "The table cannot separate the two. The hazard counts first reconvictions only, and neither",
        "source says how much offending lies behind each conviction (q), so the results are in",
        "convictions, not crimes."), "",
  sprintf("Theorems: %s.", paste(unique(c(attr(inc6, "theorems"),
                                          morie_incapacitation_career(paths$custody, 0, 4)$theorems)),
                                 collapse = ", ")))
write_result("p17_reconviction_incapacitation", lines)
