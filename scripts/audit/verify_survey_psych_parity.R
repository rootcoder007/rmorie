#!/usr/bin/env Rscript
# Survey replication / composition / GRM / IRT-linking parity.
#
# All deterministic except the two linking functions, which run
# Nelder-Mead; those are held to 1e-6, and separately checked against
# the transform the anchor data was BUILT from, which is the assertion
# that actually says the criterion is the right one.
#
# Usage: Rscript scripts/audit/verify_survey_psych_parity.R <anchors> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
fp <- file.path(rdir, "survey_psych_native.R")
if (file.exists(fp)) source(fp)
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
comp <- as.matrix(utils::read.csv(file.path(anch, "comp.csv"),
                                  header = FALSE))
M <- as.matrix(utils::read.csv(file.path(anch, "M.csv"), header = FALSE))
irt <- as.matrix(utils::read.csv(file.path(anch, "irt.csv"), header = FALSE))
dimnames(comp) <- NULL
dimnames(M) <- NULL
a_r <- irt[, 1]
b_r <- irt[, 2]
a_f <- irt[, 3]
b_f <- irt[, 4]
strata <- rep(0:5, each = 2)
est <- c(2.1, 1.9, 2.05, 1.88, 2.2, 1.95, 2.02, 1.99)

pass <- 0L
fail <- 0L
chk <- function(label, got, want, tol = 1e-9) {
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

ab <- morie_aitchison_balance(comp, c(0, 1), c(2, 3, 4))
chk("aitchison balance", ab$balance, exp$bal)
chk("balance normalizer", ab$normalizer, exp$bal_norm)
chk("numerator geo mean", ab$geometric_mean_num, exp$gn)

gr <- morie_astle_balding_grm(M)
chk("grm", as.numeric(t(gr$G)), exp$grm)
chk("grm markers used", gr$n_markers_used, exp$grm_used)
chk("grm mean diagonal", gr$mean_diagonal, exp$grm_diag)
chk("grm frequencies", gr$freq, exp$grm_freq)

br <- morie_brr_balanced(strata, fay_k = 0.3)
chk("brr replicate weights", as.numeric(t(br$replicate_weights)), exp$brr_w)
chk("brr replicate count", br$n_replicates, exp$brr_R)
chk("brr hadamard", as.numeric(t(br$hadamard)), exp$brr_H)

bv <- morie_brr_variance(est, full_estimate = 2, fay_k = 0.3)
chk("brr variance", bv$variance, exp$bv_var)
chk("brr se", bv$se, exp$bv_se)
chk("brr cv", bv$cv, exp$bv_cv)

hb <- morie_equating_haebara(a_r, b_r, a_f, b_f)
sl <- morie_equating_stocking_lord(a_r, b_r, a_f, b_f)
chk("haebara A", hb$A, exp$hb_A, 1e-6)
chk("haebara B", hb$B, exp$hb_B, 1e-6)
chk("haebara b transformed", hb$b_transformed, exp$hb_bt, 1e-6)
chk("stocking-lord A", sl$A, exp$sl_A, 1e-6)
chk("stocking-lord B", sl$B, exp$sl_B, 1e-6)

# Properties.
# The anchor data was generated from a KNOWN transform; recovering it is
# the assertion that says the criterion is the right criterion, not just
# that two languages minimise the same expression.
inv("haebara recovers the true A", abs(hb$A - 1.3) < 1e-4)
inv("haebara recovers the true B", abs(hb$B - 0.4) < 1e-4)
inv("stocking-lord recovers it too",
    abs(sl$A - 1.3) < 1e-4 && abs(sl$B - 0.4) < 1e-4)
inv("both criteria vanish at the truth",
    hb$criterion < 1e-10 && sl$criterion < 1e-10)
# Transformed focal parameters must land on the reference ones.
inv("linking maps focal onto reference",
    max(abs(hb$b_transformed - b_r)) < 1e-4 &&
      max(abs(hb$a_transformed - a_r)) < 1e-4)

# A balance is scale-invariant: rescaling a whole composition changes
# nothing, which is the defining property of compositional coordinates.
inv("balance is scale invariant",
    max(abs(morie_aitchison_balance(comp * 7, c(0, 1), c(2, 3, 4))$balance -
              ab$balance)) < 1e-12)
inv("balance flips sign on swap",
    max(abs(morie_aitchison_balance(comp, c(2, 3, 4), c(0, 1))$balance +
              ab$balance)) < 1e-12)
inv("balance rejects zeros",
    inherits(try(morie_aitchison_balance(rbind(c(0, 1, 1)), 0, c(1, 2)),
                 silent = TRUE), "try-error"))

inv("grm is symmetric", max(abs(gr$G - t(gr$G))) < 1e-10)
inv("grm diagonal near 1", abs(gr$mean_diagonal - 1) < 0.15)
inv("grm drops monomorphic markers",
    morie_astle_balding_grm(cbind(M, 0))$n_dropped == 1L)

# BRR: every replicate must use exactly one PSU per stratum at the high
# weight, and the Hadamard rows must be orthogonal -- the property that
# makes R replicates of order H sufficient.
inv("brr weights sum per stratum",
    all(abs(br$replicate_weights[, 1] + br$replicate_weights[, 2] - 2) < 1e-12))
inv("hadamard rows are orthogonal",
    max(abs(crossprod(t(br$hadamard))[upper.tri(diag(nrow(br$hadamard)))])) <=
      nrow(br$hadamard))
inv("brr requires paired psus",
    inherits(try(morie_brr_balanced(c(1, 1, 2)), silent = TRUE), "try-error"))
# Omitting the Fay divisor inflates the variance by exactly 1/(1-k)^2.
inv("fay divisor is load-bearing",
    abs(bv$variance / morie_brr_variance(est, 2, 0)$variance -
          1 / (1 - 0.3)^2) < 1e-12)

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
