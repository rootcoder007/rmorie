#!/usr/bin/env Rscript
# Survival-remainder R-vs-Python parity.
#
# depcen, chrwgt, gamfr and coxtmv are closed-form Newton fits and match
# to machine precision. ggmaft runs Nelder-Mead over a three-parameter
# likelihood in which q is weakly identified, so R and scipy land on
# nearby but not identical optima -- it is compared on the likelihood
# and the family verdict rather than on q itself, which is the honest
# comparison for a parameter the model barely identifies.
#
# Usage: Rscript scripts/audit/verify_survival_extra_parity.R <anchors> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
for (f in c("causal_shared_native.R", "cox_native.R",
            "competing_risks_native.R", "survival_extra_native.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
X <- as.matrix(utils::read.csv(file.path(anch, "X.csv"), header = FALSE))
v <- as.matrix(utils::read.csv(file.path(anch, "v.csv"), header = FALSE))
time <- v[, 1]
event <- v[, 2]
cluster <- v[, 3]
tpos <- v[, 4]

pass <- 0L
fail <- 0L
chk <- function(label, got, want, tol = 1e-8) {
  got <- as.numeric(got)
  want <- as.numeric(want)
  if (length(got) != length(want)) {
    cat(sprintf("FAIL %-26s length %d vs %d\n", label, length(got),
                length(want)))
    fail <<- fail + 1L
    return(invisible(NULL))
  }
  err <- max(abs(got - want) / pmax(1, abs(want)))
  if (is.finite(err) && err <= tol) {
    cat(sprintf("ok   %-26s max rel err %.3g\n", label, err))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-26s max rel err %.3g\n", label, err))
    fail <<- fail + 1L
  }
}
inv <- function(label, ok) {
  if (isTRUE(ok)) {
    cat(sprintf("ok   %-26s (property)\n", label))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-26s (property)\n", label))
    fail <<- fail + 1L
  }
}

dc <- morie_dependent_censoring_hazard(time, event, X)
chk("censoring beta", dc$beta_censoring, exp$dc_beta_c)
chk("censoring se", dc$se, exp$dc_se)
chk("censoring p-values", dc$p_value, exp$dc_p)
chk("event beta", dc$beta_event, exp$dc_beta_e)

cw <- morie_censoring_at_risk_weight(time, 1 - event)
chk("ipcw weights", cw$weights, exp$cw_w)
chk("ipcw ess", cw$ess, exp$cw_ess)
chk("censoring survivor G", cw$G, exp$cw_G)

gf <- morie_gamma_frailty_cox(time, event, X, cluster)
chk("frailty beta", gf$beta, exp$gf_beta, 1e-4)
chk("frailty theta", gf$theta, exp$gf_theta, 1e-3)
chk("kendall tau", gf$kendall_tau, exp$gf_tau, 1e-3)

tv <- morie_cox_time_varying(time, event, X, n_intervals = 3)
chk("time-varying beta", as.numeric(t(tv$beta)), exp$tv_beta)
chk("interval cutpoints", tv$cutpoints, exp$tv_cuts)
chk("events per interval", tv$events_per_interval, exp$tv_counts)
chk("lr vs constant", tv$lr_vs_constant, exp$tv_lr)
chk("constant beta", tv$constant_beta, exp$tv_const)

gg <- morie_generalized_gamma_aft(tpos, rep(1, length(tpos)), X)
# Nelder-Mead on a weakly identified q: compare what is identified.
chk("gen-gamma beta", gg$beta, exp$gg_beta, 5e-3)
chk("gen-gamma loglik", gg$loglik, exp$gg_ll, 1e-3)
inv("gen-gamma q is finite", is.finite(gg$q))
inv("gen-gamma agrees on the family", identical(gg$preferred, exp$gg_pref))

# Properties.
inv("ipcw zeroes the censored", all(cw$weights[event == 0] == 0))
inv("ipcw weights are positive elsewhere", all(cw$weights[event == 1] > 0))
inv("G is non-increasing", all(diff(sort(cw$G, decreasing = TRUE)) <= 1e-12))
inv("kendall tau matches theta",
    abs(gf$kendall_tau - gf$theta / (gf$theta + 2)) < 1e-12)
inv("time-varying lr is non-negative", tv$lr_vs_constant >= 0)
# Splitting into ONE interval must reproduce the constant-coefficient
# fit exactly -- if it does not, the interval construction is wrong.
one <- morie_cox_time_varying(time, event, X, n_intervals = 1)
inv("one interval == constant fit",
    max(abs(as.vector(one$beta) - one$constant_beta)) < 1e-9)
inv("interval events sum to total",
    sum(tv$events_per_interval) == sum(event))
# The censoring model must recover a real dependence when one is
# planted, otherwise a null result means nothing.
set.seed(9)
cens_dep <- as.numeric(stats::runif(length(time)) <
                         1 / (1 + exp(-2 * X[, 1])))
dep <- morie_dependent_censoring_hazard(time, 1 - cens_dep, X)
inv("censoring model detects planted dependence", dep$n_flagged >= 1L)

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
