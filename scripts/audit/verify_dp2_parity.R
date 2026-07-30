#!/usr/bin/env Rscript
# Differential-privacy shelf part 2 R-vs-Python parity.
#
# The accounting members (amplification, Renyi composition, budget
# calibration, the privacy unit) are pure arithmetic and are compared
# exactly. The covariance and PCA draw noise, so their calibration and
# clipping are compared exactly and their behaviour is checked by
# property -- PSD after projection, eigenvalues descending, and the
# subspace recovered when the budget is generous.
#
# Usage: Rscript scripts/audit/verify_dp2_parity.R <anchor-dir> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
for (f in c("dp_native.R", "dp_native2.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
A <- as.matrix(utils::read.csv(file.path(anch, "A.csv"), header = FALSE))
rec <- as.numeric(utils::read.csv(file.path(anch, "rec.csv"),
                                  header = FALSE)[[1]])

pass <- 0L
fail <- 0L
chk <- function(label, got, want, tol = 1e-12) {
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

amp <- morie_privacy_amplification(1.5, 0.02, delta = 1e-6)
chk("amplified epsilon", amp$epsilon_amplified, exp$amp_eps)
chk("amplification linear approx", amp$linear_approx, exp$amp_lin)
chk("amplification ratio", amp$ratio, exp$amp_ratio)
chk("amplified delta", amp$delta_amplified, exp$amp_delta)

rd <- morie_renyi_dp_composition(rep(0.05, 20), alpha = 8, delta = 1e-6)
chk("rdp total", rd$rdp_total, exp$rdp_total)
chk("rdp converted epsilon", rd$epsilon, exp$rdp_eps)
chk("rdp conversion penalty", rd$conversion_penalty, exp$rdp_penalty)

c1 <- morie_dp_release_calibration(2, target_error = 0.05, n = 500)
chk("calibration error->eps", c1$epsilon, exp$cal1_eps)
chk("calibration half-width", c1$half_width, exp$cal1_w)
chk("calibration noise scale", c1$noise_scale, exp$cal1_b)
c2 <- morie_dp_release_calibration(2, epsilon = 0.8, n = 500)
chk("calibration eps->error", c2$half_width, exp$cal2_w)
chk("calibration eps passthrough", c2$epsilon, exp$cal2_eps)

un <- morie_dp_unit_definition(rec)
chk("unit max contribution", un$max_contribution, exp$unit_max)
chk("unit count", un$n_units, exp$unit_n)
chk("unit contributions", un$contributions, exp$unit_contrib)

cv <- morie_dp_covariance(A, C = 4, epsilon = 3, delta = 1e-5)
chk("covariance sigma", cv$sigma, exp$cov_sigma)
chk("covariance clipped frac", cv$clipped_fraction, exp$cov_clipfrac)

# Properties. Exact values are unavailable across generators; these are
# the claims the module actually makes.
inv("projection restores PSD",
    min(eigen(cv$release, symmetric = TRUE, only.values = TRUE)$values) >
      -1e-8)
inv("release is symmetric", max(abs(cv$release - t(cv$release))) < 1e-10)
pca <- morie_dp_pca(A, k = 2, epsilon = 20, C = 6)
inv("eigenvalues descend", all(diff(pca$eigenvalues) <= 1e-10))
inv("components are orthonormal",
    max(abs(crossprod(pca$components) - diag(2))) < 1e-8)
inv("explained ratios in [0,1]",
    all(pca$explained_variance_ratio >= 0 &
          pca$explained_variance_ratio <= 1))
# With a generous budget the private leading direction should track the
# non-private one; sign is arbitrary so compare the absolute cosine.
true_v1 <- eigen(stats::cov(A) * (nrow(A) - 1) / nrow(A),
                 symmetric = TRUE)$vectors[, 1]
inv("leading direction recovered",
    abs(sum(pca$components[, 1] * true_v1)) > 0.9)

mm <- morie_dp_minmax(A[, 1], epsilon = 4, a = -8, b = 8)
inv("dp range is ordered", mm$lower < mm$upper)
# The release is a uniform draw inside a rank interval whose outer edges
# ARE the supplied bounds, so it can legitimately fall outside the
# observed data range -- that is the point of taking bounds from outside
# the data. The property to assert is containment in [a, b].
inv("dp range within given bounds",
    mm$lower >= -8 && mm$upper <= 8)
inv("dp range splits the budget", abs(mm$epsilon_each - 2) < 1e-12)

# Amplification must never make epsilon worse. The exact bound is
# LARGER than the q*epsilon everyone quotes, not smaller: e^eps - 1 > eps
# for eps > 0, so the linear form is optimistic and the two agree only in
# the small-epsilon limit. Asserting the wrong direction here would have
# let a sign error through.
inv("amplification never increases eps", amp$epsilon_amplified < 1.5)
inv("linear approx is optimistic",
    amp$epsilon_amplified > amp$linear_approx)
inv("linear approx is tight for small eps",
    abs(morie_privacy_amplification(1e-4, 0.02)$epsilon_amplified /
          morie_privacy_amplification(1e-4, 0.02)$linear_approx - 1) < 1e-3)

m <- function(dd) sum(dd) + morie_dp_laplace_mechanism(0, 1, 1)$release
set.seed(11)
ee <- morie_epsilon_dp(m, rep(1, 10), rep(1, 9), n_samples = 4000, bins = 30)
inv("empirical eps is a lower bound", ee$epsilon_empirical <= 1.5)
ad <- morie_approx_dp(m, rep(1, 10), rep(1, 9), epsilon = 1, n_samples = 4000)
inv("empirical delta in [0,1]",
    ad$delta_empirical >= 0 && ad$delta_empirical <= 1)

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
