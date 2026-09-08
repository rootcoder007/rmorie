#!/usr/bin/env Rscript
# Differential-privacy shelf R-vs-Python parity.
#
# Every mechanism here is randomised, so there is nothing to gain from
# comparing individual releases: R's generator is not numpy's and the
# two draws are simply different random numbers. What CAN be compared,
# and is, is everything the mechanism computes before it draws -- noise
# scales, sigma, selection probabilities, sensitivities, clipped
# fractions, bin counts -- plus the sampling distribution of the release
# over many draws, which is where a wrong scale would actually show.
#
# Usage: Rscript scripts/audit/verify_dp_parity.R <anchor-dir> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
fp <- file.path(rdir, "dp_native.R")
if (file.exists(fp)) source(fp)
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
x <- as.numeric(utils::read.csv(file.path(anch, "x.csv"), header = FALSE)[[1]])

pass <- 0L
fail <- 0L
chk <- function(label, got, want, tol = 1e-10) {
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
inv <- function(label, ok) {
  if (isTRUE(ok)) {
    cat(sprintf("ok   %-24s (distributional)\n", label))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-24s (distributional)\n", label))
    fail <<- fail + 1L
  }
}

chk("laplace scale", morie_dp_laplace_mechanism(1, 3, 0.7)$noise_scale,
    exp$lap_scale)
chk("laplace noise sd", morie_dp_laplace_mechanism(1, 3, 0.7)$noise_sd,
    exp$lap_sd)
chk("gaussian sigma", morie_dp_gaussian_mechanism(1, 2, 0.5, 1e-6)$sigma,
    exp$gau_sigma)
chk("exponential probs",
    morie_dp_exponential_mechanism(c("a", "b", "c", "d"), c(0, 3, 1, -2),
                                   epsilon = 1.5, sensitivity = 2)$probabilities,
    exp$exp_probs)
chk("randomized p(truth)",
    morie_randomized_response_dp(c(0, 1, 1, 0), epsilon = 1.3)$p_truth,
    exp$rr_p)
chk("count noise scale", morie_dp_count(rep(1, 50), epsilon = 0.25)$noise_scale,
    exp$cnt_scale)
chk("count true value", morie_dp_count(rep(1, 50), epsilon = 0.25)$true_count,
    exp$cnt_true)

s <- morie_dp_sum(x, -2, 2, epsilon = 0.5)
chk("sum sensitivity", s$sensitivity, exp$sum_sens)
chk("sum true (clipped)", s$true_sum, exp$sum_true)
chk("sum clipped fraction", s$clipped_fraction, exp$sum_clipfrac)

q <- morie_dp_quantile(x, q = 0.3, epsilon = 2, a = -6, b = 6)
chk("quantile rank probs", q$probabilities, exp$q_probs)
chk("quantile true value", q$true_quantile, exp$q_true)
chk("median true value",
    morie_dp_median(x, epsilon = 2, a = -6, b = 6)$true_median, exp$med_true)

h <- morie_dp_histogram(x, bins = 6, epsilon = 1, range_ = c(-6, 6))
chk("histogram counts", h$true_counts, exp$hist_counts)
chk("histogram edges", h$edges, exp$hist_edges)
chk("histogram scale", h$noise_scale, exp$hist_scale)

# Distributional checks: a wrong noise scale passes every deterministic
# assertion above and fails here.
set.seed(7)
lap <- vapply(seq_len(4000),
              function(i) morie_dp_laplace_mechanism(0, 1, 0.5)$release,
              numeric(1))
inv("laplace variance = 2b^2", abs(stats::var(lap) / (2 * 2^2) - 1) < 0.12)
inv("laplace is centred", abs(mean(lap)) < 0.25)
gau <- vapply(seq_len(4000),
              function(i) morie_dp_gaussian_mechanism(0, 1, 0.5, 1e-5)$release,
              numeric(1))
sig <- as.numeric(exp$gau_sigma)
sig05 <- morie_dp_gaussian_mechanism(0, 1, 0.5, 1e-5)$sigma
inv("gaussian sd = sigma", abs(stats::sd(gau) / sig05 - 1) < 0.06)
rr <- morie_randomized_response_dp(rbinom(4000, 1, 0.3), epsilon = 2)
inv("randomized response debiases", abs(rr$estimate - 0.3) < 0.06)
inv("raw proportion IS biased toward 1/2",
    abs(rr$raw_proportion - 0.5) < abs(rr$estimate - 0.5))
qs <- vapply(seq_len(400),
             function(i) morie_dp_quantile(x, 0.3, 2, -6, 6)$release,
             numeric(1))
inv("dp quantile concentrates",
    abs(stats::median(qs) - as.numeric(exp$q_true)) < 0.3)
inv("count clamp is recorded",
    isTRUE(morie_dp_count(rep(1, 1), epsilon = 1, nonneg = TRUE)$release >= 0))

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
