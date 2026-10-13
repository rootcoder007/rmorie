# P18 selective labels, applied to New York State's supplemental pretrial release file (DCJS,
# cases arraigned 2019-2024): a proposed release rule's 180-day rearrest rate is known only through
# the defendants a judge released; for the part of the rule's set that was held, the data say
# nothing, and the identified interval is as wide as that part (Research.P18).
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

cols <- c("arr_class", "arr_vfo", "pend_vfo", "pend_nonvfo", "pend_misd", "pvfo_cnt", "larg_court_name",
          "larg_court_type", "larg_rel_decision", "larg_rel_outcome", "rearr_vfo_180", "rearr_nonvfo_180",
          "rearr_misd_180")
files <- c(dpath("NYPRETRIAL", "PretrialRelease_2019_2024_Part_1.csv"),
           dpath("NYPRETRIAL", "PretrialRelease_2019_2024_Part_2.csv"))
d <- do.call(rbind, lapply(files, function(f) utils::read.csv(need_file(f), stringsAsFactors = FALSE)[cols]))
n_all <- nrow(d)
# at liberty pending trial: released on recognizance or non-monetary conditions, or bail posted within
# five days; held: remanded, or bail set and not posted
released <- d$larg_rel_decision %in% c("ROR", "NMR/RUS") |
  d$larg_rel_outcome %in% c("BailPaid @arraign", "BailPaid w/in 1-5days")
held <- d$larg_rel_outcome %in% c("Remanded", "BailNotPaid w/in 5days")
d <- d[released | held, ]
released <- released[released | held]
y <- as.integer(d$rearr_vfo_180 == "Yes" | d$rearr_nonvfo_180 == "Yes" | d$rearr_misd_180 == "Yes")
yv <- as.integer(d$rearr_vfo_180 == "Yes")
misd <- grepl("Misdemeanor", d$arr_class)
no_pending <- d$pend_vfo == "No" & d$pend_nonvfo == "No" & d$pend_misd == "No"
nonviolent <- d$arr_vfo == "Not VFO"
no_vfo_prior <- d$pvfo_cnt == "0"
rules <- list(
  "misdemeanour charge, no pending case" = misd & no_pending,
  "non-violent charge, no violent prior" = nonviolent & no_vfo_prior,
  "non-violent charge, no violent prior, no pending case" = nonviolent & no_vfo_prior & no_pending,
  "release everyone" = rep(TRUE, nrow(d)))
row_for <- function(name, rule, outcome, label) {
  s <- morie_selective_labels(outcome, released, rule)
  data.frame(rule = name, outcome = label, cases = sum(rule), held_share = s$rule_share_unobserved,
             naive_rate = s$naive_rate, lower = s$bounds[[1]], upper = s$bounds[[2]], width = s$width,
             identified = s$identified, stringsAsFactors = FALSE)
}
tab <- do.call(rbind, c(lapply(names(rules), function(k) row_for(k, rules[[k]], y, "any rearrest")),
                        lapply(names(rules), function(k) row_for(k, rules[[k]], yv, "violent felony rearrest"))))
obs <- mean(y[released])

# by court: the same rule scored on each court's own caseload; the more lenient the court, the less
# of the rule's set it held, and the narrower the rule's interval there (the contraction argument)
rule2 <- rules[[2]]
big <- names(which(table(d$larg_court_name) >= 3000))
ct <- do.call(rbind, lapply(big, function(cn) {
  i <- d$larg_court_name == cn
  s <- morie_selective_labels(y[i], released[i], rule2[i])
  data.frame(court = cn, type = d$larg_court_type[i][1], cases = sum(i), release_share = mean(released[i]),
             rule_cases = sum(rule2[i]), held_share = s$rule_share_unobserved, lower = s$bounds[[1]],
             upper = s$bounds[[2]], naive_rate = s$naive_rate, stringsAsFactors = FALSE)
}))
ct <- ct[order(-ct$release_share), ]
rho <- stats::cor(ct$release_share, ct$upper - ct$lower, method = "spearman")

lines <- c(
  "## P18 selective labels: New York pretrial release, 2019-2024", "",
  sprintf(paste("Data: New York State DCJS Supplemental Pretrial Release Data File (%s arrest cycles, cases",
                "arraigned 2019-2024). %s defendants were either at liberty pending trial (released on",
                "recognizance or non-monetary conditions, or bail posted within five days: %s) or held",
                "(remanded or bail not posted: %s); cases disposed at arraignment are excluded. The outcome is",
                "a fingerprintable rearrest within 180 days, observed only for those at liberty: %.1f%% of",
                "them were rearrested."),
          format(n_all, big.mark = ","), format(nrow(d), big.mark = ","), format(sum(released), big.mark = ","),
          format(sum(!released), big.mark = ","), 100 * obs), "",
  paste("A rule's rearrest rate is identified only on the part of its set that judges released; for the",
        "part they held it lies anywhere from no one to everyone rearrested (Research.P18.unobserved_bounds),",
        "so the interval's width is exactly the held share (unobserved_width) and a rule that releases",
        "only people judges already released is scored exactly (nested_rate_identified)."), "",
  "### Four release rules, statewide", "",
  md_table(tab), "",
  sprintf(paste("Reading: the naive rate (rearrest among the rule's set that judges released) is one point",
                "inside each interval, chosen by assuming the held would have behaved like the released.",
                "Releasing everyone has a rearrest rate between %.1f%% and %.1f%%; even the rule that releases",
                "only non-violent charges with no violent prior leaves %.1f%% of its set unobserved."),
          100 * tab$lower[4], 100 * tab$upper[4], 100 * tab$held_share[2]), "",
  sprintf("### The same rule (non-violent charge, no violent prior) court by court (%d courts with 3,000+ cases)",
          nrow(ct)), "",
  md_table(utils::head(ct, 10)), "",
  md_table(utils::tail(ct, 5)), "",
  sprintf(paste("Across courts the rank correlation between a court's release share and the width of the",
                "rule's interval there is %.2f: the most lenient courts leave the least of the rule",
                "unobserved, which is the contraction argument (Research.P18.chain_contraction), here across",
                "different caseloads rather than one defendant pool, so the courts' intervals are each valid",
                "for their own cases and not for one another's."), rho), "",
  "Theorems: Research.P18.observed_rate_is_conditional, Research.P18.nested_rate_identified, Research.P18.unobserved_bounds, Research.P18.unobserved_width, Research.P18.unobserved_ends_attained.")
write_result("p18_ny_pretrial", lines)
