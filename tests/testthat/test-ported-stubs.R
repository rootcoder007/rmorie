# SPDX-License-Identifier: AGPL-3.0-or-later
# Truth-recovery tests for the estimators ported off their
# NotYetPorted stubs (2026-07): OTIS SuperLearner-AIPW / PLR-DML /
# PSM / PS-subclass, optimal multiframe weights, native Turnbull EM,
# and the five native Bayesian spatial-voting samplers.

# Namespace portability: resolve dotted internals under pkgload,
# test_check, and installed-namespace harnesses alike.
for (.nm in c(".morie_sv_bayes_am", ".morie_sv_bayes_mds",
              ".morie_sv_bayes_unfold", ".morie_sv_bayes_cjr",
              ".morie_sv_bayes_ordinal")) {
  if (!exists(.nm, inherits = TRUE)) {
    assign(.nm, get(.nm, envir = asNamespace(environmentName(
      environment(morie_otis_aipw_superlearner)))))
  }
}

.ported_df <- function(n = 600L, seed = 1L) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, stats::plogis(0.5 * x1))
  y <- 1 + 2 * d + x1 - 0.5 * x2 + rnorm(n)
  data.frame(y, d, x1, x2)
}

test_that("OTIS SuperLearner-AIPW recovers the simulated ATE", {
  df <- .ported_df()
  r <- morie_otis_aipw_superlearner(df, "d", "y", c("x1", "x2"),
                                    n_folds = 3L)
  expect_lt(abs(r$ate - 2), 0.3)
  expect_gt(r$ate_se, 0)
})

test_that("OTIS PLR-DML recovers the simulated effect", {
  df <- .ported_df()
  r <- morie_otis_plr(df, "d", "y", c("x1", "x2"))
  expect_lt(abs(r$ate - 2), 0.3)
})

test_that("OTIS PSM caliper matching recovers the ATT", {
  df <- .ported_df()
  r <- morie_otis_psm(df, "d", "y", c("x1", "x2"))
  expect_lt(abs(r$ate - 2), 0.4)
})

test_that("OTIS PS-subclassification recovers the ATE", {
  df <- .ported_df()
  r <- morie_otis_psm_subclass(df, "d", "y", c("x1", "x2"))
  expect_lt(abs(r$ate - 2), 0.4)
  expect_true(is.matrix(r$strata))
})

test_that("optimal multiframe weights return an interior theta", {
  set.seed(1)
  w <- morie_weights_multiframe(runif(50, 1, 3), runif(60, 1, 3),
                                c(rep(TRUE, 20), rep(FALSE, 30)),
                                c(rep(TRUE, 25), rep(FALSE, 35)),
                                method = "optimal")
  expect_gt(w$theta, 0)
  expect_lt(w$theta, 1)
})

test_that("native Turnbull EM is a proper NPMLE on interval-censored data", {
  set.seed(2)
  L <- round(stats::rexp(60, 0.2), 1)
  R <- L + round(stats::runif(60, 0.5, 3), 1)
  R[sample(60, 12)] <- NA
  nat <- morie_survival_turnbull(L, R)
  expect_match(nat$method, "Turnbull")
  expect_true(all(diff(nat$surv) <= 1e-12))          # non-increasing
  expect_true(all(nat$surv >= 0 & nat$surv <= 1))
  expect_true(abs(sum(nat$mass) - 1) < 1e-6)          # mass sums to one
  expect_true(nat$converged || nat$iterations > 0)
})

test_that("native Bayesian AM recovers stimulus ordering", {
  set.seed(3)
  zeta_t <- seq(-2, 2, length.out = 6)
  Z <- t(replicate(40, 0.5 + 1.2 * zeta_t + rnorm(6, sd = 0.4)))
  f <- .morie_sv_bayes_am(Z, n_samples = 200L, burn_in = 100L)
  expect_gt(abs(stats::cor(f$zeta_mean, zeta_t, method = "spearman")),
            0.9)
})

test_that("native Bayesian MDS recovers the distance structure", {
  set.seed(4)
  Xt <- matrix(rnorm(20), 10, 2)
  Dt <- as.matrix(stats::dist(Xt))
  f <- .morie_sv_bayes_mds(Dt, n_dims = 2L, n_samples = 200L,
                           burn_in = 100L)
  expect_gt(stats::cor(as.numeric(stats::dist(f$positions)),
                       as.numeric(stats::dist(Xt))), 0.9)
})

test_that("native Bayesian unfolding returns finite configurations", {
  set.seed(5)
  P <- matrix(stats::runif(80, 1, 9), 20, 4)
  f <- .morie_sv_bayes_unfold(P, n_dims = 2L, n_samples = 150L,
                              burn_in = 50L)
  expect_true(all(is.finite(f$stimuli)))
  expect_true(all(is.finite(f$ideal_points)))
})

test_that("native CJR IRT recovers latent ideal-point ordering", {
  set.seed(6)
  th <- rnorm(40); b <- stats::runif(12, 0.8, 1.6)
  a <- rnorm(12, 0, 0.5)
  V <- (matrix(stats::plogis(outer(th, b) -
                               matrix(a, 40, 12, TRUE)), 40, 12) >
          matrix(stats::runif(480), 40, 12)) * 1L
  f <- .morie_sv_bayes_cjr(V, n_samples = 200L, burn_in = 100L)
  expect_gt(abs(stats::cor(f$ideal_points, th, method = "spearman")),
            0.7)
})

test_that("native ordinal IRT recovers latent ideal-point ordering", {
  set.seed(7)
  th <- rnorm(40); b <- stats::runif(12, 0.8, 1.6)
  a <- rnorm(12, 0, 0.5)
  eta <- outer(th, b) - matrix(a, 40, 12, TRUE)
  Y3 <- matrix(cut(eta + rnorm(480), c(-Inf, -0.5, 0.5, Inf),
                   labels = FALSE), 40, 12)
  f <- .morie_sv_bayes_ordinal(Y3, n_samples = 200L, burn_in = 100L)
  expect_gt(abs(stats::cor(f$ideal_points, th, method = "spearman")),
            0.7)
})
