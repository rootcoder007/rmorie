# P14 judge-leniency designs, applied to Cook County (Illinois) felony sentencing: whether the
# judges can be ordered by leniency at all. A judge design's instrument needs monotonicity
# (nesting: whoever a lenient judge incarcerates, a stricter one does too); the file has no
# outcome after sentencing, so the Wald ratio cannot be formed, but nesting has an implication
# that needs no outcome: the judges' incarceration rates keep their order inside every offence
# category (Research.P14Slope.subgroup_propensity_mono), and a significant reversal refutes the
# nesting of that pair in both directions (crossing_refutes_both).
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

f <- need_file(dpath("COOK", "cook_sentencing.csv"))
d <- utils::read.csv(f, stringsAsFactors = FALSE)[c("CASE_PARTICIPANT_ID", "PRIMARY_CHARGE_FLAG",
  "CURRENT_SENTENCE_FLAG", "SENTENCE_PHASE", "SENTENCE_JUDGE", "SENTENCE_TYPE", "SENTENCE_COURT_FACILITY",
  "SENTENCE_DATE", "UPDATED_OFFENSE_CATEGORY")]
n_all <- nrow(d)
d <- d[d$PRIMARY_CHARGE_FLAG %in% c(TRUE, "true", "TRUE") & d$CURRENT_SENTENCE_FLAG %in% c(TRUE, "true", "TRUE"), ]
d <- d[d$SENTENCE_PHASE == "Original Sentencing", ]
custody <- c("Prison", "Jail", "Cook County Boot Camp")
community <- c("Probation", "Conditional Discharge", "Supervision", "2nd Chance Probation")
d <- d[d$SENTENCE_TYPE %in% c(custody, community), ]
d$incarcerated <- as.integer(d$SENTENCE_TYPE %in% custody)
d$year <- as.integer(format(as.Date(substr(d$SENTENCE_DATE, 1, 10), "%m/%d/%Y"), "%Y"))
d <- d[!is.na(d$year) & d$year >= 2011 & d$year <= 2023, ]
# one courthouse, where felony cases are assigned to courtrooms by calendar, and categories with
# enough cases to compare judges inside them
d <- d[d$SENTENCE_COURT_FACILITY == "26TH Street" & nzchar(d$SENTENCE_JUDGE), ]
big <- names(which(table(d$UPDATED_OFFENSE_CATEGORY) >= 2000))
d <- d[d$UPDATED_OFFENSE_CATEGORY %in% big, ]
jn <- table(d$SENTENCE_JUDGE)
d <- d[d$SENTENCE_JUDGE %in% names(jn[jn >= 300]), ]

r <- morie_judge_monotonicity(d$SENTENCE_JUDGE, d$incarcerated, d$UPDATED_OFFENSE_CATEGORY, min_n = 50)
s <- r$summary
# the same test across sentencing years inside one category: a judge's place in the order should
# not depend on the year either
nar <- d[d$UPDATED_OFFENSE_CATEGORY == "Narcotics", ]
ry <- morie_judge_monotonicity(nar$SENTENCE_JUDGE, nar$incarcerated, nar$year, min_n = 50)

top <- r$comparisons[r$comparisons$significant, ]
top <- top[order(top$p_adjusted), ]
top <- utils::head(top, 12)
# judges are identified by name in the public file; the result names them only by rank
rank <- stats::setNames(seq_len(nrow(r$judges)), r$judges$judge)
top$stricter <- sprintf("judge %d", rank[top$stricter])
top$lenient <- sprintf("judge %d", rank[top$lenient])

lines <- c(
  "## P14 judge leniency: can Cook County's felony judges be ordered?", "",
  sprintf(paste("Data: Cook County State's Attorney sentencing file (%s rows); primary charge, current",
                "sentence, original sentencing, prison/jail/boot camp against probation/conditional",
                "discharge/supervision, 2011-2023, the 26th Street courthouse, offence categories with",
                "at least 2,000 sentences and judges with at least 300: %s sentences, %d judges,",
                "%d categories."), format(n_all, big.mark = ","), format(nrow(d), big.mark = ","),
          nrow(r$judges), length(big)), "",
  paste("Monotonicity says a stricter judge incarcerates everyone a more lenient judge does. The file",
        "has no outcome after sentencing, so no Wald ratio is formed here; what can be tested is the",
        "implication that needs none: inside every offence category the stricter judge overall is",
        "still at least as strict (Research.P14Slope.subgroup_propensity_mono). Each pair of judges",
        "with 50 or more cases in a category is compared, one-sided, and the p-values Holm-adjusted",
        "over all comparisons; a significant reversal means that pair is nested in neither direction",
        "(crossing_refutes_both)."), "",
  "### Across offence categories", "",
  md_table(s), "",
  md_table(r$by_group), "",
  paste("The slope is the regression of a judge's rate in the category on the judge's overall rate",
        "(weighted by cases): positive slopes everywhere are what average monotonicity looks like;",
        "a slope at or below zero would mean the overall ranking says nothing about that category."), "",
  "### The strongest reversals", "",
  md_table(top[c("group", "stricter", "lenient", "rate_stricter", "rate_lenient", "group_rate_stricter",
                 "group_rate_lenient", "n_stricter", "n_lenient", "p_adjusted")]), "",
  "### Across years, narcotics only", "",
  md_table(ry$summary), "",
  sprintf(paste("Reading: %s of %s comparisons reverse the overall order, and %d reversals are significant",
                "after adjustment, involving %d pairs and %d of the %d judges. Nesting is a statement about",
                "the same defendant before two judges, so the subgroup implication compares like with like",
                "only when cases reach judges as good as at random within the category. Felony cases at",
                "26th Street are assigned to courtrooms by calendar, not by lottery, so each significant",
                "reversal is one of two things: the pair is not nested (monotonicity fails), or the two",
                "judges saw different kinds of case inside the category (the instrument is not as good as",
                "random). Either one breaks a judge design that ranks these judges on one leniency scale;",
                "a design here needs average monotonicity, case-mix controls finer than the category, or",
                "the defier sensitivity of morie_judge_iv()."),
          format(s$reversed, big.mark = ","), format(s$comparisons, big.mark = ","), s$significant,
          s$pairs_refuted, s$judges_involved, nrow(r$judges)),
  "", sprintf("Theorems: %s.", paste(r$theorems, collapse = ", ")))
write_result("p14_cook_judges", lines)
