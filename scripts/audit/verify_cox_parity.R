#!/usr/bin/env Rscript
# Cox-shelf R-vs-Python parity. Every function here is deterministic,
# so these are value comparisons, not invariants.
#
# The anchor data carries 27 tied event times on purpose -- with no ties
# Breslow and Efron coincide and the tie corrections would be untested.
#
# Usage: Rscript scripts/audit/verify_cox_parity.R <anchor-dir> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
for (f in c("causal_shared_native.R", "cox_native.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
X <- as.matrix(utils::read.csv(file.path(anch, "X.csv"), header = FALSE))
td <- as.matrix(utils::read.csv(file.path(anch, "td.csv"), header = FALSE))
time <- td[, 1]
event <- td[, 2]
strat <- td[, 3]

pass <- 0L
fail <- 0L
chk <- function(label, got, want, tol = 1e-8) {
  got <- as.numeric(got)
  want <- as.numeric(want)
  if (length(got) != length(want)) {
    cat(sprintf("FAIL %-22s length %d vs %d\n", label, length(got), length(want)))
    fail <<- fail + 1L
    return(invisible(NULL))
  }
  err <- max(abs(got - want) / pmax(1, abs(want)))
  if (is.finite(err) && err <= tol) {
    cat(sprintf("ok   %-22s max rel err %.3g\n", label, err))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-22s max rel err %.3g\n", label, err))
    fail <<- fail + 1L
  }
}

bre <- morie_breslow_tie_correction(time, event, X)
efr <- morie_efron_tie_correction(time, event, X)
chk("breslow beta", bre$beta, exp$breslow_beta)
chk("breslow se", bre$se, exp$breslow_se)
chk("breslow loglik", bre$loglik, exp$breslow_ll)
chk("efron beta", efr$beta, exp$efron_beta)
chk("efron se", efr$se, exp$efron_se)
chk("efron loglik", efr$loglik, exp$efron_ll)
chk("tied event count", efr$n_ties, exp$n_ties)

bh <- morie_cox_breslow_step(time, event, X)
chk("baseline times", bh$times, exp$bh_times)
chk("baseline cumhaz", bh$cumhazard, exp$bh_cumhaz)

mg <- morie_cox_martingale_residuals(efr)
chk("martingale resid", mg$residuals, exp$mg_resid)

sch <- morie_cox_schoenfeld_residuals(efr)
# Python ravels row-major; R's as.numeric is column-major, so transpose
# first or the values match while the positions do not.
chk("schoenfeld resid", as.numeric(t(sch$residuals)), exp$sch_resid)
chk("schoenfeld corr", sch$correlation, exp$sch_corr)
chk("schoenfeld p", sch$p_value, exp$sch_p)
chk("schoenfeld rank corr",
    morie_cox_schoenfeld_residuals(efr, "rank")$correlation, exp$schrank_corr)

st <- morie_cox_stratified(time, event, X, strat)
chk("stratified beta", st$beta, exp$strat_beta)
chk("stratified se", st$se, exp$strat_se)
chk("stratified loglik", st$loglik, exp$strat_ll)

inf <- morie_cox_dfbeta_influence(efr)
chk("dfbeta", as.numeric(t(inf$dfbeta)), exp$dfbeta)
chk("max |dfbetas|", inf$max_influence, exp$dfbetas_max)
chk("most influential", inf$most_influential, exp$most_influential)
chk("dfbeta_cox one-shot", as.numeric(t(morie_dfbeta_cox(time, event, X)$dfbeta)),
    exp$dlb_dfbeta)

dev <- morie_deviance_residual_cox(efr)
chk("deviance resid", dev$residuals, exp$dev_resid)
chk("deviance extremes", dev$n_extreme, exp$dev_extreme)

# Untied data must make the two corrections agree exactly -- the one
# property that says the Efron branch is doing what it claims.
tu <- time + seq_along(time) * 1e-7
b2 <- morie_breslow_tie_correction(tu, event, X)$beta
e2 <- morie_efron_tie_correction(tu, event, X)$beta
if (max(abs(b2 - e2)) < 1e-10) {
  cat("ok   breslow==efron untied  max diff 0\n")
  pass <- pass + 1L
} else {
  cat(sprintf("FAIL breslow==efron untied  max diff %.3g\n", max(abs(b2 - e2))))
  fail <- fail + 1L
}

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
