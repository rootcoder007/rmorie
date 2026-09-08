#!/usr/bin/env Rscript
# MCMC diagnostics + block bootstrap R-vs-Python parity.
#
# Everything except the bootstrap is deterministic. The bootstrap draws
# block indices, so its estimate and block geometry are compared exactly
# and its standard error only approximately -- two different resamplings
# of the same series agree on the SE to sampling error, not to machine
# precision, and demanding more would be demanding the wrong thing.
#
# Usage: Rscript scripts/audit/verify_mcmc_parity.R <anchor-dir> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
for (f in c("dp_native.R", "mcmc_native.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
y <- as.numeric(utils::read.csv(file.path(anch, "y.csv"), header = FALSE)[[1]])
C <- as.matrix(utils::read.csv(file.path(anch, "C.csv"), header = FALSE))
acc <- as.matrix(utils::read.csv(file.path(anch, "acc.csv"), header = FALSE))
dimnames(C) <- NULL
dimnames(acc) <- NULL

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

a <- morie_autocorrelation(y)
chk("acf", a$acf, exp$acf)
chk("acf band", a$ci_bound, exp$acf_bound)
chk("ljung-box", a$ljung_box, exp$ljung)
chk("ljung-box p", a$ljung_p <- a$ljung_box_p, exp$ljung_p)

ar <- morie_acceptance_rate_diagnostic(acc)
chk("acceptance rate", ar$acceptance_rate, exp$acc_rate)
chk("acceptance per chain", ar$per_chain, exp$acc_per)

eb <- morie_effective_sample_size_bayes(C)
chk("ess", eb$ess, exp$ess)
chk("mcse", eb$mcse, exp$mcse)
chk("split rhat", eb$rhat, exp$rhat)

bk <- morie_effective_sample_size_bulk(C)
chk("bulk ess", bk$ess_bulk, exp$ess_bulk)

tl <- morie_effective_sample_size_tail(C)
chk("tail ess", tl$ess_tail, exp$ess_tail)
chk("tail ess lower", tl$ess_lower, exp$ess_lo)
chk("tail ess upper", tl$ess_upper, exp$ess_hi)

bb <- morie_boot_nonoverlap_block(y, block_len = 7, B = 400, seed = 0)
chk("bootstrap estimate", bb$estimate, exp$bb_est)
chk("bootstrap block count", bb$n_blocks, exp$bb_nblocks)
# Two different resamplings; 20% is generous on 400 replicates and would
# still catch a wrong block length or a missing dependence correction.
chk("bootstrap se (resampled)", bb$se, exp$bb_se, 0.2)

# Properties.
inv("acf starts at 1", abs(a$acf[1L] - 1) < 1e-12)
inv("acf is within [-1, 1]", all(abs(a$acf) <= 1 + 1e-12))
# The shared-denominator estimator is what keeps this true; a per-lag
# denominator can and does produce |rho| > 1.
inv("ar(1) acf decays geometrically",
    abs(a$acf[3L] - a$acf[2L]^2) < 0.12)
inv("ljung-box rejects white noise", a$ljung_box_p < 0.001)
set.seed(4)
wn <- morie_autocorrelation(stats::rnorm(400))
inv("ljung-box passes white noise", wn$ljung_box_p > 0.01)

inv("ess below draw count for ar(1)", eb$ess < 4 * ncol(C))
inv("mcse follows ess not n",
    abs(eb$mcse - stats::sd(as.vector(C)) / sqrt(eb$ess)) < 1e-9)
inv("rhat near 1 for converged chains", eb$rhat < 1.05)
# Tail and bulk ESS are different statistics; on a well-behaved target
# both are large, but they are not required to agree.
inv("tail ess is reported separately", tl$ess_tail != bk$ess_bulk)
# An iid chain must have ESS close to the draw count -- the sanity check
# that says the autocorrelation correction is not simply always firing.
set.seed(6)
iid <- matrix(stats::rnorm(4000), nrow = 4)
inv("iid ess is near the draw count",
    abs(morie_effective_sample_size_bayes(iid)$ess / 4000 - 1) < 0.25)

# The block bootstrap must give a LARGER se than the iid bootstrap on a
# dependent series. That is the entire reason the module exists.
set.seed(8)
iid_se <- stats::sd(vapply(seq_len(400),
                           function(b) mean(sample(y, length(y), TRUE)),
                           numeric(1)))
inv("block se exceeds iid se", bb$se > iid_se * 1.2)

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
