#!/usr/bin/env Rscript
# Causal matching / weighting / diagnostics R-vs-Python parity.
#
# All deterministic, so these are value comparisons. The matching
# functions are the ones worth watching: a tie in a distance matrix is
# broken by stable order in both languages, and an unstable sort on
# either side makes the chosen match depend on the partition rather than
# on the data.
#
# Usage: Rscript scripts/audit/verify_causal_match_parity.R <anchors> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
for (f in c("causal_shared_native.R", "causal_match_native.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
X <- as.matrix(utils::read.csv(file.path(anch, "X.csv"), header = FALSE))
v <- as.matrix(utils::read.csv(file.path(anch, "v.csv"), header = FALSE))
pd <- as.numeric(utils::read.csv(file.path(anch, "pd.csv"),
                                 header = FALSE)[[1]])
tr <- v[, 1]
ps <- v[, 2]
ypre <- v[, 3]
g <- v[, 4]
po <- v[, 5]
ydd <- v[, 6]

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

mm <- morie_causal_mahalanobis_match(X, tr, k = 2)
# Python ravels row-major; R's as.numeric is column-major, so transpose.
chk("mahalanobis matches", as.numeric(t(mm$matches)), exp$mm_matches)
chk("mahalanobis distances",
    as.numeric(t(ifelse(is.na(mm$distances), -1, mm$distances))),
    exp$mm_dists)
chk("mahalanobis reuse max", mm$reuse_max, exp$mm_reuse)
chk("mahalanobis unmatched", mm$n_unmatched, exp$mm_unmatched)

cp <- morie_causal_caliper_matching(ps, tr, k = 1)
chk("caliper default width", cp$caliper_used, exp$cp_caliper)
chk("caliper match rate", cp$match_rate, exp$cp_rate)
chk("caliper matches", as.numeric(t(cp$matches)), exp$cp_matches)

ov <- morie_causal_overlap_diagnostic(ps, tr)
chk("overlap common support", ov$common_support, exp$ov_support)
chk("overlap coefficient", ov$overlap_coefficient, exp$ov_coef)
chk("overlap treated hist", ov$hist_treated, exp$ov_h1)
chk("overlap prop extreme", ov$prop_extreme, exp$ov_extreme)

wt <- morie_causal_iptw_atoweights(tr, ps, estimand = "ato")
wa <- morie_causal_iptw_atoweights(tr, ps, estimand = "ate")
chk("ato weights", wt$weights, exp$ato_w)
chk("ato ess", wt$ess, exp$ato_ess)
chk("ate weights", wa$weights, exp$ate_w)
chk("ate ess", wa$ess, exp$ate_ess)

cb <- morie_covariate_balance_check(X, tr, weights = wt$weights)
chk("smd before", cb$smd_before, exp$cb_before)
chk("smd after weighting", cb$smd_after, exp$cb_after)
chk("variance ratio", cb$variance_ratio, exp$cb_vr)

fl <- morie_causal_falsification_test(ypre, tr, X_baseline = X[, 1:2])
chk("falsification estimate", fl$estimate, exp$fl_est)
chk("falsification se", fl$se, exp$fl_se)
chk("falsification p", fl$p_value, exp$fl_p)
chk("min detectable effect", fl$min_detectable_effect, exp$fl_mde)

eb <- morie_entropy_balancing(X, tr, moments = 2)
chk("entropy weights", eb$weights, exp$eb_w, 1e-6)
chk("entropy max imbalance", eb$max_imbalance, exp$eb_imb, 1e-4)
chk("entropy ess", eb$ess, exp$eb_ess, 1e-6)

rb <- morie_causal_rosenbaum_bound(pd)
chk("rosenbaum gamma*", rb$gamma_critical, exp$rb_gcrit)
chk("rosenbaum p upper", rb$p_upper, exp$rb_p)

dd <- morie_causal_did_three_way(ydd, tr, po, g)
chk("ddd", dd$ddd, exp$ddd)
chk("did eligible", dd$did_eligible, exp$ddd_e)
chk("did placebo", dd$did_placebo, exp$ddd_p)
chk("ddd se", dd$se, exp$ddd_se)

# Properties: the claims these functions make, which the values alone
# would not enforce.
# The 1/4 bound belongs to the TILTING function e(1-e), not to the
# weights themselves: a treated unit carries 1-e and a control carries e,
# each in (0, 1). What matters operationally is that no ATO weight can
# blow up the way an IPTW weight does as e approaches 0 or 1.
inv("overlap weights bounded by 1", max(wt$weights) <= 1 + 1e-12)
inv("tilting function bounded by 1/4",
    max(ps * (1 - ps)) <= 0.25 + 1e-12)
inv("ato max share beats ate max share",
    wt$max_weight_share < wa$max_weight_share)
inv("overlap ess beats ate ess", wt$ess > wa$ess)
inv("entropy balance is exact", eb$max_imbalance < 1e-6)
inv("entropy weights are a simplex",
    abs(sum(eb$weights) - 1) < 1e-12 && all(eb$weights >= 0))
# Entropy balancing must hit the treated moments dead on, which is the
# whole difference from propensity weighting.
Cw <- cbind(X[tr == 0, ], X[tr == 0, ]^2)
inv("entropy hits treated moments",
    max(abs(as.vector(eb$weights %*% Cw) - eb$target)) < 1e-6)
inv("caliper drops rather than extrapolates",
    cp$n_unmatched == 0L || cp$estimand == "ATT among matchable units")
inv("trimming changes the estimand",
    morie_causal_iptw_atoweights(tr, ps, "ate", trim = 0.2)$n_trimmed > 0L)
# Rosenbaum's p-value must rise with Gamma: a larger allowed bias can
# only make the result less significant, never more.
inv("rosenbaum p rises with gamma", all(diff(rb$p_upper) >= -1e-12))
# DDD on data with no triple interaction must be near zero.
set.seed(2)
y0 <- 1 + g + tr + po + stats::rnorm(length(g))
inv("ddd is null when it should be",
    abs(morie_causal_did_three_way(y0, tr, po, g)$z) < 3)

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
