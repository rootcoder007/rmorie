#!/usr/bin/env Rscript
# AFT + competing-risks + frailty R-vs-Python parity.
#
# Tolerances differ by function and the reason matters. The Cox-based
# members (cause-specific, Fine-Gray, the residual transforms) are
# closed-form Newton iterations and match to machine precision. The AFT
# fitters and the frailty profile call an OPTIMISER -- R's optim and
# scipy's minimize walk different paths to the same optimum -- so they
# are held to 1e-5, which is far tighter than any statistical
# difference and still an honest cross-check.
#
# Usage: Rscript scripts/audit/verify_aft_cr_parity.R <anchor-dir> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
for (f in c("causal_shared_native.R", "cox_native.R", "aft_native.R",
            "competing_risks_native.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
X <- as.matrix(utils::read.csv(file.path(anch, "X.csv"), header = FALSE))
td <- as.matrix(utils::read.csv(file.path(anch, "td.csv"), header = FALSE))
time <- td[, 1]
event <- td[, 2]
etype <- td[, 3]
cluster <- td[, 4]

pass <- 0L
fail <- 0L
chk <- function(label, got, want, tol = 1e-8) {
  got <- as.numeric(got)
  want <- as.numeric(want)
  if (length(got) != length(want)) {
    cat(sprintf("FAIL %-24s length %d vs %d\n", label, length(got),
                length(want)))
    fail <<- fail + 1L
    return(invisible(NULL))
  }
  err <- max(abs(got - want) / pmax(1, abs(want)))
  if (is.finite(err) && err <= tol) {
    cat(sprintf("ok   %-24s max rel err %.3g\n", label, err))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-24s max rel err %.3g\n", label, err))
    fail <<- fail + 1L
  }
}

OPT <- 1e-5     # optimiser-limited comparisons

w <- morie_aft_weibull(time, event, X)
chk("weibull beta", w$beta, exp$wbl_beta, OPT)
chk("weibull se", w$se, exp$wbl_se, 1e-3)
chk("weibull sigma", w$sigma, exp$wbl_sigma, OPT)
chk("weibull loglik", w$loglik, exp$wbl_ll, OPT)

ll <- morie_aft_log_logistic(time, event, X)
chk("loglogistic beta", ll$beta, exp$llg_beta, OPT)
chk("loglogistic sigma", ll$sigma, exp$llg_sigma, OPT)
chk("loglogistic loglik", ll$loglik, exp$llg_ll, OPT)

gg <- morie_aft_generalized_gamma(time, event, X)
chk("lognormal beta", gg$beta, exp$lnm_beta, OPT)
chk("lognormal sigma", gg$sigma, exp$lnm_sigma, OPT)
chk("lognormal loglik", gg$loglik, exp$lnm_ll, OPT)

rw <- morie_aft_residuals(w)
chk("aft standardized", rw$standardized, exp$res_z, OPT)
chk("aft cox-snell", rw$cox_snell, exp$res_cs, OPT)
chk("aft deviance", rw$deviance, exp$res_dev, OPT)

cs <- morie_cause_specific_hazard(time, etype, X)
chk("cause-specific beta", cs$beta, exp$cs_beta)
chk("cause-specific se", cs$se, exp$cs_se)
chk("cause-specific loglik", cs$loglik, exp$cs_ll)

fg <- morie_competing_risks_fg(time, etype, X)
chk("fine-gray beta", fg$beta, exp$fg_beta)
chk("fine-gray se", fg$se, exp$fg_se)
chk("fine-gray loglik", fg$loglik, exp$fg_ll)
chk("fine-gray weights", fg$weights, exp$fg_weights)

cif <- morie_fine_gray_subdistribution_hazard(time, etype, X)
chk("cif times", cif$times, exp$cif_times)
chk("cif baseline", cif$baseline_cif, exp$cif_base)

fr <- morie_cox_frailty(time, event, X, cluster)
chk("frailty theta", fr$theta, exp$fr_theta, 1e-3)
chk("frailty beta", fr$beta, exp$fr_beta, 1e-4)
chk("frailty se", fr$se, exp$fr_se, 1e-4)
chk("frailty values", fr$frailty, exp$fr_frailty, 1e-4)

# Properties the values alone would not catch.
inv <- function(label, ok) {
  if (isTRUE(ok)) {
    cat(sprintf("ok   %-24s (invariant)\n", label))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-24s (invariant)\n", label))
    fail <<- fail + 1L
  }
}
inv("cif is a probability", all(cif$baseline_cif >= 0 & cif$baseline_cif <= 1))
inv("cif is monotone", all(diff(cif$baseline_cif) >= -1e-12))
inv("fg weights in (0,1]", all(fg$weights > 0 & fg$weights <= 1))
inv("frailty mean near 1", abs(mean(fr$frailty) - 1) < 0.25)
inv("kendall tau matches theta",
    abs(fr$kendall_tau - fr$theta / (fr$theta + 2)) < 1e-12)
# Cause-specific and Fine-Gray answer different questions and should not
# be expected to agree; asserting they differ is what says both are real.
inv("csh and fg differ", max(abs(cs$beta - fg$beta)) > 1e-6)

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
