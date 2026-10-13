# P11 sentencing effects as intervals, applied to Statistics Canada's reconviction study: adults
# sentenced to custody or to community supervision in five provincial correctional programs
# (2015/16 to 2018/19) and their reconviction within one, two and three years. What the data
# alone say about the effect of custody against community supervision is an interval of width one
# (Research.P11.ate_width_one); each assumption that narrows it is stated with the theorem that
# gives the narrower interval.
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

num <- function(x) as.numeric(gsub(",", "", x))
g <- utils::read.csv(need_file(dpath("JURISTAT", "reconviction_2024_tbl02_grid.csv")), stringsAsFactors = FALSE)
types <- c(any = "c3", violent = "c5", property = "c7", other = "c9", administration_of_justice = "c11")
follow <- which(grepl("^Fixed .* follow-up period$", g$c1))
rows <- list()
for (f in follow) {
  period <- sub("^Fixed (.*) follow-up period$", "\\1", g$c1[f])
  cu <- g[f + 1L, ]
  co <- g[f + 2L, ]
  stopifnot(cu$c1 == "Custody", co$c1 == "Community")
  for (k in names(types)) {
    col <- types[[k]]
    nc <- num(cu$c2)
    nm <- num(co$c2)
    yc <- num(cu[[col]])
    ym <- num(co[[col]])
    y <- c(1, 0, 1, 0)
    z <- c("custody", "custody", "community", "community")
    w <- c(yc, nc - yc, ym, nm - ym)
    b <- morie_sentence_effect_bounds(y, z, w, contrast = "custody")
    m <- morie_sentence_effect_mts(y, z, w, contrast = "custody")
    rows[[length(rows) + 1L]] <- data.frame(
      follow_up = period, conviction = k, custody_rate = yc / nc, community_rate = ym / nm,
      naive = b$naive_difference, worst_lower = b$ate_bounds[["lower"]], worst_upper = b$ate_bounds[["upper"]],
      mts_lower = m$ate_bounds_mts[["lower"]], mts_upper = m$ate_bounds_mts[["upper"]],
      mtr_mts_lower = m$ate_bounds_mtr_mts[["lower"]], mtr_mts_upper = m$ate_bounds_mtr_mts[["upper"]],
      stringsAsFactors = FALSE)
  }
}
tab <- do.call(rbind, rows)
any2 <- tab[tab$conviction == "any" & tab$follow_up == "two-year", ]
n_people <- num(g$c2[follow[1] + 3L])

lines <- c(
  "## P11 sentencing effects as intervals: custody against community supervision", "",
  sprintf(paste("Data: Statistics Canada, Juristat 85-002-X 2024001 article 00002, table 2: adults released",
                "from custody or starting community supervision in five provincial programs, 2015/16 to",
                "2018/19 (%s people in the one-year follow-up), and whether they were convicted again within",
                "one, two or three years, by type of new conviction."), format(n_people, big.mark = ",")), "",
  paste("The effect is the difference in reconviction had everyone been sentenced to custody rather",
        "than community supervision. The data identify each sentence's outcome only for those who got",
        "it, so:"),
  "",
  paste("* **No assumption** (`morie_sentence_effect_bounds()`): the interval has width exactly one and",
        "contains zero (Research.P11.outcome_bounds, ate_width_one, ate_contains_zero)."),
  paste("* **Monotone treatment selection** (`morie_sentence_effect_mts()`): people sent to custody",
        "would be no less likely to be reconvicted than those supervised in the community under either",
        "sentence. The naive difference becomes an upper bound (Research.P11.Pop.mts_ate_le_naive)."),
  paste("* **Monotone treatment response as well**: custody does not lower anyone's chance of",
        "reconviction. The effect then lies in [0, naive difference] (Research.P11.Pop.mtr_mts_bounds)."), "",
  md_table(tab), "",
  sprintf(paste("Reading, any new conviction within two years: %.1f%% after custody and %.1f%% after",
                "community supervision, a naive difference of %.1f points. Without assumptions the effect",
                "of custody lies anywhere in [%.1f, %.1f] points; under selection monotonicity in [%.1f, %.1f];",
                "adding response monotonicity, in [0, %.1f]. The naive gap is the largest effect custody",
                "could have under the assumptions the field usually makes about who is sent to custody,",
                "not an estimate of the effect, and nothing in these data rules out an effect of zero."),
          100 * any2$custody_rate, 100 * any2$community_rate, 100 * any2$naive, 100 * any2$worst_lower,
          100 * any2$worst_upper, 100 * any2$mts_lower, 100 * any2$mts_upper, 100 * any2$mtr_mts_upper), "",
  paste("The cohorts differ before sentencing (table 1: 59% of the custody cohort had ten or more prior",
        "convictions against 20% of the community cohort), which is the selection the MTS assumption",
        "describes; the published tables give no covariates jointly with outcomes, so the intervals are",
        "not tightened by conditioning."), "",
  "Theorems: Research.P11.outcome_bounds, Research.P11.ate_width_one, Research.P11.ate_contains_zero, Research.P11.Pop.mts_ate_le_naive, Research.P11.Pop.mtr_mts_bounds.")
write_result("p11_custody_reconviction", lines)
