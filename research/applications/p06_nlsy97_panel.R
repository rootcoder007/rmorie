# P6 the age-crime curve, applied to a cohort panel: BJS's NLSY97 recidivism standalone file (ICPSR
# 34562), self-reported offending in the calendar year each respondent turned 14 through 23. The
# aggregate age curve alone cannot tell one offender type from a mixture (Research.P6: every mixture's
# aggregate is a one-type curve); the same people observed at two ages can: under one type the
# cross-age covariance is exactly zero (Research.P6.Panel.one_type_cov_zero), under several it is the
# covariance of the type rates (cross_moment_eq).
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

f <- need_file(dpath("NLSY97REC", "ICPSR_34562", "DS0001", "34562-0001-Data.rda"))
e <- new.env()
load(f, envir = e)
x <- get(ls(e)[1], envir = e)
crimes <- c("DESTROY", "STEAL_UNDER_50", "STEAL_OVER_50", "OTHER_PROPERTY", "SELL_DRUGS", "ASSAULT")
ages <- 14:23
yes <- function(v) {
  v <- as.character(v)
  ifelse(is.na(v), NA_real_, as.numeric(grepl("Yes", v, fixed = TRUE)))
}
# any of the six offences in the year the respondent turned `a`; missing when every item is missing
any_at <- function(a) {
  m <- sapply(crimes, function(cn) yes(x[[sprintf("%s_CY%d", cn, a)]]))
  out <- apply(m, 1L, function(r) if (all(is.na(r))) NA_real_ else max(r, na.rm = TRUE))
  out
}
Y <- sapply(ages, any_at)
colnames(Y) <- ages
agg <- data.frame(age = ages, respondents = colSums(!is.na(Y)), offending_share = colMeans(Y, na.rm = TRUE))

pairs <- list(c(14, 15), c(14, 18), c(15, 20), c(16, 17), c(16, 20), c(18, 19), c(18, 22), c(20, 23))
pc <- do.call(rbind, lapply(pairs, function(ab) {
  a <- as.character(ab[1])
  b <- as.character(ab[2])
  ok <- !is.na(Y[, a]) & !is.na(Y[, b])
  r <- morie_age_crime_panel_cov(Y[ok, a], Y[ok, b], n_perm = 2000L, seed = 1L)
  data.frame(age_a = ab[1], age_b = ab[2], n = r$n, rate_a = mean(Y[ok, a]), rate_b = mean(Y[ok, b]),
             cross_cov = r$cross_cov, correlation = stats::cor(Y[ok, a], Y[ok, b]), p_value = r$p_value,
             stringsAsFactors = FALSE)
}))
# a two-type reading of one pair, for scale: types with rates (r1a, r1b) and (r2a, r2b) in shares w give
# covariance w1 w2 (r1a - r2a)(r1b - r2b) (Research.P6.Panel.two_type_cov)
two <- morie_age_crime_type_cov(c(0.8, 0.2), c(0.10, 0.70), c(0.05, 0.50))

lines <- c(
  "## P6 the age-crime curve: a cohort panel (NLSY97)", "",
  sprintf(paste("Data: Bureau of Justice Statistics, Recidivism in the National Longitudinal Survey of Youth 1997,",
                "Standalone Data (Rounds 1 to 13), ICPSR 34562-v1 (2014), DS0001: %s respondents. Offending at",
                "an age is any of six self-reported acts (destroying property, theft under or over $50, other",
                "property crime, selling drugs, assault) in the calendar year the respondent turned that age.",
                "The file is used under ICPSR's terms of use and is not redistributed here."),
          format(nrow(x), big.mark = ",")), "",
  "### The aggregate curve", "",
  md_table(agg), "",
  paste("This curve alone cannot separate one offender type from a mixture of types: every mixture's",
        "aggregate is also the curve of a single type (Research.P6.aggregate_not_identifying, single_aggregate)."), "",
  "### The same people at two ages", "",
  md_table(pc), "",
  sprintf(paste("Under a single type (everyone's offending at each age drawn from one shared rate, independently",
                "across ages) the covariance across ages is exactly zero (Research.P6.Panel.one_type_cov_zero);",
                "the permutation null keeps each age's marginal and breaks the link. Every pair above has a",
                "positive covariance, %s, and correlations from %.2f to %.2f: the people who offend at one",
                "age are the ones who offend at another, which one shared curve cannot produce. A",
                "two-type population with 20%% at rates 0.70 and 0.50 against 80%% at 0.10 and 0.05 would",
                "give covariance %.4f (two_type_cov), the order of what is observed. The test refutes",
                "invariance-of-type; it does not by itself say how many types there are (the Teicher-type",
                "identification of K types from T periods is the open part of P6)."),
          if (all(pc$p_value < 0.001)) "every one beyond all 2,000 permutations" else "most beyond the permutation null",
          min(pc$correlation), max(pc$correlation), two$cross_cov), "",
  "Theorems: Research.P6.single_aggregate, Research.P6.aggregate_not_identifying, Research.P6.Panel.cross_moment_eq, Research.P6.Panel.one_type_cov_zero, Research.P6.Panel.two_type_cov, Research.P6.Panel.aggregate_blind.")
write_result("p06_nlsy97_panel", lines)
