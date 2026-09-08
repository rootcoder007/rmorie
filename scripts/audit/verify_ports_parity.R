# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Parity harness for R/verification_ports_native.R.
#
# Run on l14 (never the Mac), against either R tree:
#
#   ssh l14 'cd ~/morie-parity && LD_LIBRARY_PATH=/home/rootcoder/liboqs/build/lib \
#     Rscript scripts/audit/verify_ports_parity.R'
#
# Deterministic members are checked against values computed directly
# from the arithmetic (Reinsch, Morris, AIPW), NOT against numbers
# copied out of the Python run -- an expectation lifted from the code
# it is testing proves only that the code is self-consistent. The
# RNG-driven members are checked by the property the method guarantees
# (parameter recovery, monotonicity), which is the only thing that can
# be asserted across two independent RNG streams.

pkg_dir <- if (dir.exists("r-package/morie")) "r-package/morie" else "."
suppressMessages(pkgload::load_all(pkg_dir, quiet = TRUE))

fails <- 0L
ok <- function(label, cond) {
  cat(sprintf("%-58s %s\n", label, if (isTRUE(cond)) "PASS" else "FAIL"))
  if (!isTRUE(cond)) fails <<- fails + 1L
}
near <- function(a, b, tol = 1e-9) all(abs(a - b) < tol)

# ---- eslsmt: Reinsch smoothing spline -------------------------------
# Independent reference: build (I + lambda D'W^-1 D)^-1 y from scratch.
x <- c(0, 1, 2, 3, 4)
y <- c(0, 2, 1, 3, 2)
lam <- 1.0
h <- diff(x); n <- length(x)
D <- matrix(0, n - 2L, n); W <- matrix(0, n - 2L, n - 2L)
for (i in seq_len(n - 2L)) {
  D[i, i] <- 1 / h[i]
  D[i, i + 1L] <- -(1 / h[i] + 1 / h[i + 1L])
  D[i, i + 2L] <- 1 / h[i + 1L]
  W[i, i] <- (h[i] + h[i + 1L]) / 3
  if (i < n - 2L) { W[i, i + 1L] <- h[i + 1L] / 6; W[i + 1L, i] <- h[i + 1L] / 6 }
}
S_ref <- solve(diag(n) + lam * crossprod(D, solve(W, D)))
fit_ref <- as.vector(S_ref %*% y)
r <- morie_esl_smoothing_spline(x, y, lam)
ok("eslsmt: fit matches (I + lam D'W^-1 D)^-1 y", near(r$estimate, fit_ref))
ok("eslsmt: effective_df = tr(S)", near(r$effective_df, sum(diag(S_ref))))
ok("eslsmt: lambda = 0 interpolates",
   near(morie_esl_smoothing_spline(x, y, 0)$estimate, y, 1e-8))
ok("eslsmt: df decreases as lambda grows",
   morie_esl_smoothing_spline(x, y, 100)$effective_df < r$effective_df)
# Cross-language: the same inputs through morie.fn.eslsmt in Python.
# Deterministic, so this is a true bit-level parity check, and it is a
# SECOND opinion on top of the from-scratch reference above -- if the
# two languages agreed on a wrong formula, the reference would catch it.
ok("eslsmt: matches Python fit to 1e-9",
   near(r$estimate, c(0.3913043478, 1.2173913043, 1.7826086957,
                      2.2173913043, 2.3913043478), 1e-9))
ok("eslsmt: matches Python effective_df", near(r$effective_df, 2.6751918159, 1e-9))
ok("eslsmt: matches Python rss", near(r$rss, 2.1436672968, 1e-9))

# ---- empby: Morris (1983) parametric EB -----------------------------
# All estimates identical => grand mean is that value, no shrinkage move.
e <- morie_empirical_bayes(rep(10, 6), rep(1, 6))
ok("empby: identical estimates give grand_mean = 10", near(e$grand_mean, 10, 1e-6))
ok("empby: shrunk stay at 10", near(e$shrunk_estimates, rep(10, 6), 1e-6))
# Shrinkage pulls toward the estimated mean, not toward zero.
e2 <- morie_empirical_bayes(c(8, 10, 12), c(1, 1, 1))
ok("empby: shrinks toward grand mean, not zero",
   all(abs(e2$shrunk_estimates - e2$grand_mean) <=
       abs(c(8, 10, 12) - e2$grand_mean) + 1e-12))
ok("empby: shrinkage factors in (0, 1]", all(e2$shrinkage_factors > 0 &
                                             e2$shrinkage_factors <= 1))
# Cross-language against morie.fn.empby. The tolerance is 1e-5 rather
# than 1e-9 because the tau^2 profile is maximised by a 1-D optimiser
# (R optimize vs scipy minimize_scalar) whose stopping points differ in
# the last digits; the shrunk estimates are stable well inside that.
ok("empby: matches Python shrunk estimates",
   near(e2$shrunk_estimates, c(8.74999999, 10.0, 11.25000001), 1e-5))
ok("empby: matches Python tau2", near(e2$tau2, 1.66666668, 1e-5))

# ---- vlfctn: regime value -------------------------------------------
set.seed(0)
n <- 800
X <- matrix(rnorm(n), ncol = 1)
d <- as.numeric(runif(n) < 0.5)
yv <- d * X[, 1] + rnorm(n)
v <- morie_regime_value(yv, d, X, as.numeric(X[, 1] > 0))
# The regime treats exactly where the effect is positive, so it must
# beat treat-none; that is the whole claim of a value function.
ok("vlfctn: fitted regime beats treat-none", v$value > v$value_treat_none)
ok("vlfctn: SE positive and finite", is.finite(v$se) && v$se > 0)
ok("vlfctn: CI brackets the estimate",
   v$ci[1] < v$estimate && v$estimate < v$ci[2])
ok("vlfctn: near_indifferent is a proportion",
   v$near_indifferent >= 0 && v$near_indifferent <= 1)
# Under a constant-zero effect the regime cannot beat the static rules
# by much -- a large gain here would mean the estimator is fitting noise.
y0 <- rnorm(n)
v0 <- morie_regime_value(y0, d, X, as.numeric(X[, 1] > 0))
ok("vlfctn: no spurious gain when the effect is zero",
   abs(v0$gain_over_static) < 0.35)

# ---- bshrk: horseshoe Gibbs (property, not bit-parity) --------------
set.seed(1)
n <- 200; p <- 6
Xb <- matrix(rnorm(n * p), n, p)
beta_true <- c(3, -2, rep(0, p - 2))
yb <- as.vector(Xb %*% beta_true) + rnorm(n, sd = 0.5)
b <- morie_bayesian_horseshoe(Xb, yb, n_iter = 1500L, seed = 42L)
pm <- b$posterior_mean
ok("bshrk: recovers the +3 signal", abs(pm[1] - 3) < 0.5)
ok("bshrk: recovers the -2 signal", abs(pm[2] + 2) < 0.5)
ok("bshrk: shrinks the nulls to ~0", max(abs(pm[3:p])) < 0.3)
ok("bshrk: posterior sd positive (chain moved)", all(b$posterior_sd > 1e-6))

# ---- otmapnk: monotone transport map --------------------------------
set.seed(0)
s <- rnorm(300)
m <- morie_neural_kantorovich_map(s, 2 * s + 1, n_iter = 150L)
ok("otmapnk: map is monotone (Brenier)", isTRUE(m$monotone))
# What can honestly be asserted here is CONVERGENCE, not a fixed error
# floor at an arbitrary iteration count. At n_iter = 150 this is still
# far from converged in BOTH languages -- R 0.569, Python 0.643 -- so a
# "rmse < 0.5" gate would have been a made-up number that the reference
# implementation also fails. The real property is that the loss keeps
# falling and the converged error is small next to the target's scale.
m2 <- morie_neural_kantorovich_map(s, 2 * s + 1, n_iter = 400L)
m3 <- morie_neural_kantorovich_map(s, 2 * s + 1, n_iter = 2000L)
# The loss is non-increasing BY CONSTRUCTION: the loop halves the step
# whenever a step would raise it. This is a property of the algorithm,
# true for every input, so it is the right thing to assert. A numeric
# error threshold is not -- the error here has no plateau in reach
# (Python: 0.139 at 2e3, 0.088 at 8e3, 0.051 at 2e4, 0.030 at 6e4, with
# `converged` still FALSE), so any fixed cutoff would encode the
# iteration budget rather than the correctness of the port.
ok("otmapnk: loss is non-increasing (backtracking invariant)",
   all(diff(m3$loss_history) <= 1e-12))
ok("otmapnk: error falls as the fit runs longer",
   m3$rmse_vs_exact < m2$rmse_vs_exact && m2$rmse_vs_exact < m$rmse_vs_exact)
ok("otmapnk: monotone at every horizon",
   isTRUE(m2$monotone) && isTRUE(m3$monotone))
# T(x) = 2x + 1 is the exact map for this pair; check it at the median,
# where the quantile coupling is best determined.
ok("otmapnk: recovers the affine map near the centre",
   abs(m3$map_at_source[which.min(abs(s - median(s)))] -
       (2 * s[which.min(abs(s - median(s)))] + 1)) < 0.6)

cat(sprintf("\n%d failure(s)\n", fails))
quit(status = if (fails > 0L) 1L else 0L)
