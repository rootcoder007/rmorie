# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/snpest_native.R (sequential Dirichlet-process mixture
# density estimation). The Student-t log density is checked against
# stats::dt, the Normal-Inverse-Gamma posterior predictive against its
# closed form, and the particle filter's log marginal likelihood
# against the exact Polya-urn / conjugate recursion at one particle.

.sp_y <- c(0.4, -1.2, 0.8, 2.1, -0.3, 1.5, -0.9, 0.2, 1.1, -1.8, 0.6, 2.4)

test_that("t_logpdf is the scaled Student-t log density", {
  for (df in c(1, 4, 9.5)) for (sc2 in c(0.5, 2)) {
    x <- c(-2, 0, 1.3)
    expect_equal(morie_snpest_t_logpdf(x, df, 0.7, sc2),
                 dt((x - 0.7) / sqrt(sc2), df, log = TRUE) - 0.5 * log(sc2),
                 tolerance = 1e-12)
  }
  # symmetric about the location
  expect_equal(morie_snpest_t_logpdf(1.7, 5, 0.7, 1), morie_snpest_t_logpdf(-0.3, 5, 0.7, 1),
               tolerance = 1e-12)
})

test_that("predictive is the Normal-Inverse-Gamma posterior predictive", {
  m0 <- 0.2
  k0 <- 0.5
  a0 <- 2.5
  b0 <- 1.3
  # prior predictive: t with 2 a0 df, scale2 = b0 (k0 + 1) / (a0 k0)
  expect_equal(morie_snpest_predictive(0.9, 0, 0, 0, m0, k0, a0, b0),
               morie_snpest_t_logpdf(0.9, 2 * a0, m0, b0 * (k0 + 1) / (a0 * k0)),
               tolerance = 1e-12)
  yy <- c(1, 2, 4)
  n <- 3
  s <- sum(yy)
  ss <- sum(yy^2)
  kn <- k0 + n
  mn <- (k0 * m0 + s) / kn
  an <- a0 + n / 2
  bn <- b0 + 0.5 * (ss - s^2 / n) + 0.5 * k0 * n * (mean(yy) - m0)^2 / kn
  expect_equal(morie_snpest_predictive(0.9, n, s, ss, m0, k0, a0, b0),
               morie_snpest_t_logpdf(0.9, 2 * an, mn, bn * (kn + 1) / (an * kn)),
               tolerance = 1e-12)
  # the predictive is a density: it integrates to one
  f <- function(x) exp(morie_snpest_predictive(x, n, s, ss, m0, k0, a0, b0))
  expect_equal(integrate(function(v) vapply(v, f, 0), -40, 40, rel.tol = 1e-10)$value, 1,
               tolerance = 1e-6)
})

test_that("the filter's log marginal likelihood matches the exact recursion", {
  # one particle with the optimal proposal: the weight increment is
  # log p(y_t | y_<t), so the trace is the exact running log evidence
  f <- morie_snpest(.sp_y, alpha = 1, n_particles = 1, seed = 3, seed_stats = FALSE)
  e <- .ghc_rng(3)
  cl <- list()
  ll <- 0
  trace <- numeric(12)
  for (t in seq_along(.sp_y)) {
    x <- .sp_y[t]
    K <- length(cl)
    lp <- numeric(K + 1)
    for (j in seq_len(K)) {
      lp[j] <- log(cl[[j]][1]) + morie_snpest_predictive(x, cl[[j]][1], cl[[j]][2], cl[[j]][3],
                                                         0, 0.01, 2, 1)
    }
    lp[K + 1] <- log(1) + morie_snpest_predictive(x, 0, 0, 0, 0, 0.01, 2, 1)
    norm <- log(sum(exp(lp - max(lp)))) + max(lp)
    pr <- exp(lp - norm)
    u <- .ghc_unif(e, 1L)
    pick <- min(which(u <= cumsum(pr)), K + 1)
    ll <- ll + norm - log(1 + (t - 1))
    trace[t] <- ll
    if (pick == K + 1) {
      cl[[K + 1]] <- c(1, x, x * x)
    } else {
      cl[[pick]] <- cl[[pick]] + c(1, x, x * x)
    }
  }
  expect_equal(f$log_marginal_trace, trace, tolerance = 1e-10)
  expect_equal(f$log_marginal, trace[12], tolerance = 1e-10)
  expect_equal(f$clusters[12], length(cl), tolerance = 1e-12)
})

test_that("morie_snpest returns a normalised density and validates its options", {
  f <- morie_snpest(.sp_y, n_particles = 8, seed = 2, grid = seq(-4, 4, length.out = 41))
  expect_equal(length(f$density), 41L)
  expect_true(all(f$density >= 0))
  # the mixture density integrates to one over the whole line; the
  # Student-t components have heavy tails, so a grid of [-4, 4] holds
  # about 94 percent of the mass
  wide <- morie_snpest(.sp_y, n_particles = 8, seed = 2, grid = seq(-60, 60, length.out = 4001))
  expect_equal(sum(wide$density) * 0.03, 1, tolerance = 0.01)
  expect_lt(sum(f$density) * 0.2, 1)
  expect_true(all(f$ess >= 1 - 1e-9))
  expect_true(all(f$ess <= 8 + 1e-9))
  expect_true(all(diff(f$log_marginal_trace) <= 1e-9))
  # a resample happens when the ESS falls below the threshold
  always <- morie_snpest(.sp_y, n_particles = 4, ess_threshold = 1.5, seed = 1)
  expect_equal(length(always$resampled_at), 12L)
  never <- morie_snpest(.sp_y, n_particles = 4, ess_threshold = 0, seed = 1)
  expect_length(never$resampled_at, 0L)
  for (rs in c("systematic", "stratified", "multinomial")) {
    r <- morie_snpest(.sp_y, n_particles = 4, resampler = rs, ess_threshold = 1.5, seed = 5)
    expect_true(is.finite(r$log_marginal))
  }
  pr <- morie_snpest(.sp_y, n_particles = 6, proposal = "prior", seed = 4)
  expect_true(is.finite(pr$log_marginal))
  # a larger concentration makes more clusters
  expect_gt(morie_snpest(.sp_y, alpha = 20, n_particles = 6, seed = 7)$final_clusters,
            morie_snpest(.sp_y, alpha = 0.05, n_particles = 6, seed = 7)$final_clusters)
  expect_error(morie_snpest(.sp_y, proposal = "mcmc"), "proposal must be one of")
  expect_error(morie_snpest(.sp_y, resampler = "residual"), "resampler must be one of")
  expect_error(morie_snpest(.sp_y, alpha = 0), "alpha must be positive")
  expect_error(morie_snpest(1), "at least two observations")
  expect_error(morie_snpest(.sp_y, n_particles = 0), "at least one particle")
})

test_that("morie_snpest_cheatsheet lists the proposals and resamplers", {
  s <- morie_snpest_cheatsheet()
  expect_match(s, "optimal, prior", fixed = TRUE)
  expect_match(s, "systematic, stratified, multinomial", fixed = TRUE)
})
