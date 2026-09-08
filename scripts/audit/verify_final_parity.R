#!/usr/bin/env Rscript
# Final-batch parity: privacy accounting, wavelet/autoencoder anomaly,
# V-trace, score matching, the zonal EBM, and the DR/forest tier.
#
# The RNG-driven members (autoencoder, score matching, DR cross-fitting,
# the loss-balanced forest) are checked by property; everything else is
# compared by value.
#
# Usage: Rscript scripts/audit/verify_final_parity.R <anchors> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
for (f in c("causal_shared_native.R", "dp_native.R", "misc_native.R",
            "causal_forest_honest.R", "dr_forest_native.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
y <- as.numeric(utils::read.csv(file.path(anch, "y.csv"), header = FALSE)[[1]])
Xs <- as.matrix(utils::read.csv(file.path(anch, "Xs.csv"), header = FALSE))
rl <- as.matrix(utils::read.csv(file.path(anch, "rl.csv"), header = FALSE))
dimnames(Xs) <- NULL

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

cp <- morie_basic_composition(rep(0.1, 50), delta_prime = 1e-6)
chk("basic composition", cp$basic_epsilon, exp$cp_basic)
chk("advanced composition", cp$advanced_epsilon, exp$cp_adv)
inv("composition recommendation agrees",
    identical(cp$recommended, exp$cp_rec))

cd <- morie_cdp_subgaussian_amplification(0.01, 100, delta = 1e-6)
chk("zcdp rho total", cd$rho_total, exp$cd_rho)
chk("zcdp epsilon", cd$epsilon, exp$cd_eps)
chk("zcdp equivalent sigma", cd$equivalent_sigma, exp$cd_sig)

at <- morie_private_accuracy_tradeoff(2, 0.5, n = 500)
chk("tradeoff noise scale", at$noise_scale, exp$at_scale)
chk("tradeoff half width", at$half_width, exp$at_hw)
chk("noise-to-signal n", at$noise_to_signal_n, exp$at_cross)

dw <- morie_discrete_wavelet_anomaly(y)
chk("wavelet sigma", dw$sigma, exp$dw_sigma)
chk("wavelet threshold", dw$threshold, exp$dw_thr)
chk("wavelet score", dw$score, exp$dw_score)
chk("wavelet level fired", dw$level_fired, exp$dw_level)
chk("wavelet per-level count", dw$per_level_count, exp$dw_perlev)

im <- morie_impala_vtrace(rl[, 1], rl[, 2], rl[, 3], rl[, 4], gamma = 0.97,
                          rho_bar = 1.2, c_bar = 1, bootstrap_value = 0.3)
chk("vtrace targets", im$vs, exp$im_vs)
chk("vtrace advantage", im$advantage, exp$im_adv)
chk("vtrace rho", im$rho, exp$im_rho)
chk("vtrace delta", im$delta, exp$im_delta)

warm <- morie_zonal_ebm(1, start = 20)
cold <- morie_zonal_ebm(1, start = -40)
chk("ebm warm temperatures", warm$temperature, exp$warm_T)
chk("ebm warm global mean", warm$global_mean, exp$warm_mean)
chk("ebm cold global mean", cold$global_mean, exp$cold_mean)
chk("ebm latitudes", warm$latitude, exp$warm_lat)

# Properties.
inv("advanced beats basic at k=50", cp$advanced_epsilon < cp$basic_epsilon)
# ...and loses at small k, which is the point of computing both.
inv("basic wins at small k",
    morie_basic_composition(rep(0.5, 3), 1e-6)$recommended == "basic")
# eps = rho + 2 sqrt(rho log(1/delta)): the sqrt term dominates only
# while rho is small, so quadrupling k multiplies epsilon by something
# between 2 (pure sqrt growth) and 4 (pure linear), never exactly 2.
inv("zcdp growth is sublinear in k", {
  ratio <- morie_cdp_subgaussian_amplification(0.01, 400,
                                               delta = 1e-6)$epsilon / cd$epsilon
  ratio >= 2 && ratio < 4
})

# The wavelet module's own claim: a spike fires at the FINEST level, a
# level shift fires at coarse ones. Both are planted in the anchor
# series (a spike at 70, a shift at 120).
# The anchor spike is at Python index 70, i.e. R index 71; the level
# shift starts at Python 120, i.e. R 121.
inv("spike fires at level 1", dw$level_fired[71L] == 1L)
# A sustained level shift produces large COARSE coefficients -- level 4
# carries 8 of them here -- but max_span deliberately refuses to let a
# span-16 coefficient mark 16 points, so the shift is recorded in
# per_level_count and NOT marked in `anomaly`. That is the trade-off the
# cap buys: sharp localisation of spikes at the cost of not flagging
# sustained shifts. Asserting it both ways keeps either half from
# drifting.
inv("shift shows in coarse coefficients", sum(dw$per_level_count[4:5]) > 0L)
inv("max_span suppresses the coarse marking",
    !any(dw$anomaly[119:136]) && max(dw$level_fired) <= 3L)
inv("spike is marked and tightly localised",
    dw$anomaly[71L] && sum(dw$anomaly) <= 8L)

# V-trace: rho_bar moves the fixed point, c_bar does not. With no
# truncation at all the targets must equal the ordinary n-step return.
free <- morie_impala_vtrace(rl[, 1], rl[, 2], rl[, 3], rl[, 4], gamma = 0.97,
                            rho_bar = 1e6, c_bar = 1e6, bootstrap_value = 0.3)
inv("untruncated vtrace differs", max(abs(free$vs - im$vs)) > 1e-8)
inv("c_bar alone leaves rho alone",
    max(abs(morie_impala_vtrace(rl[, 1], rl[, 2], rl[, 3], rl[, 4],
                                gamma = 0.97, rho_bar = 1.2, c_bar = 0.5,
                                bootstrap_value = 0.3)$rho - im$rho)) < 1e-12)

# The EBM's entire reason for existing: two stable states, one forcing.
inv("ebm is bistable", warm$global_mean > 0 && cold$snowball)
inv("ebm equator is warmer than pole",
    warm$temperature[5] > warm$temperature[1])
inv("weak sun tips the warm branch", morie_zonal_ebm(0.7, start = 20)$snowball)

# Autoencoder: the linear bottleneck error IS pca error, and the
# module's documented failure mode is MASKING -- the anomalies are in
# the training data, so a lone extreme point defines the leading
# direction and is then reconstructed perfectly by it. Both halves are
# asserted, because a port that only ever detected would have lost the
# caveat and a port that only ever masked would be broken.
set.seed(3)
Xa <- matrix(stats::rnorm(300), ncol = 3)
Xa[7, ] <- c(9, -9, 9)
ae <- morie_autoencoder_anomaly(Xa, k = 1)
inv("lone outlier masks itself at k=1", which.max(ae$score) != 7L)
set.seed(3)
Xb <- matrix(stats::rnorm(300), ncol = 3) %*% diag(c(5, 1, 1))
Xb[7, ] <- c(0, 7, 7)
inv("off-axis outlier is found",
    which.max(morie_autoencoder_anomaly(Xb, k = 1)$score) == 7L)
inv("autoencoder explains most variance", ae$explained_fraction > 0.3)

# Score matching: the objective must fall when the score function is
# right for the noised distribution.
set.seed(4)
Xd <- matrix(stats::rnorm(200), ncol = 2)
good <- morie_diffusion_score_matching(Xd, function(z) -z, sigma = 0.5)
bad <- morie_diffusion_score_matching(Xd, function(z) z * 5, sigma = 0.5)
inv("score matching prefers the right score", good$objective < bad$objective)

# DR estimator on a known effect.
set.seed(5)
Xdr <- matrix(stats::rnorm(600), ncol = 3)
Ddr <- stats::rbinom(200, 1, plogis(Xdr[, 1]))
ydr <- Xdr[, 1] + 2 * Ddr + stats::rnorm(200)
dr <- morie_dr_overlap_weighted(ydr, Ddr, Xdr)
inv("dr recovers a planted effect", abs(dr$ate - 2) < 0.4)
inv("dr interval covers it", dr$ci[1] < 2 && dr$ci[2] > 2)
pl <- morie_placebo_dr_did(stats::rnorm(200), stats::rnorm(200), Ddr, Xdr)
inv("placebo did passes on no pre-trend", isTRUE(pl$passed))

# Loss-balanced forest: the penalty must coarsen the partition.
set.seed(6)
Xf <- matrix(stats::rnorm(1200), ncol = 4)
Df <- stats::rbinom(300, 1, 0.5)
yf <- Xf[, 2] + ifelse(Xf[, 1] > 0, 2, -2) * Df + stats::rnorm(300, 0, 0.5)
f0 <- morie_egregious_loss_forest(yf, Df, Xf, n_trees = 40,
                                  imbalance_penalty = 0)
f1 <- morie_egregious_loss_forest(yf, Df, Xf, n_trees = 40,
                                  imbalance_penalty = 500)
inv("penalty coarsens the partition", f1$n_leaves <= f0$n_leaves)
inv("forest finds the effect modifier",
    mean(f0$cate[Xf[, 1] > 0], na.rm = TRUE) >
      mean(f0$cate[Xf[, 1] <= 0], na.rm = TRUE))
inv("forest rejects a negative penalty",
    inherits(try(morie_egregious_loss_forest(yf, Df, Xf,
                                             imbalance_penalty = -1),
                 silent = TRUE), "try-error"))

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
