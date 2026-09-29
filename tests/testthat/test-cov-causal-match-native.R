# Coverage tests for R/causal_match_native.R: matching, overlap, balance,
# weighting, falsification, entropy balancing, Rosenbaum bounds and DDD.

cm_X <- cbind(c(1.2, 0.4, 2.2, 1.8, 0.1, 0.9, 1.5, 2.5, 0.6, 1.1),
  c(3, 2.1, 1.5, 2.8, 2.2, 3.3, 1.9, 2.4, 2.9, 1.2))
cm_t <- c(1, 0, 1, 0, 0, 1, 0, 0, 0, 1)

test_that("Mahalanobis nearest-neighbour matching", {
  r <- morie_causal_mahalanobis_match(cm_X, cm_t, k = 2)
  ti <- which(cm_t == 1)
  ci <- which(cm_t == 0)
  S <- cov(cm_X)
  D <- t(sapply(ti, function(a) sqrt(mahalanobis(cm_X[ci, ], cm_X[a, ], S))))
  expect_equal(r$distances, t(apply(D, 1, function(d) sort(d)[1:2])), tolerance = 1e-10)
  expect_equal(r$matches, t(apply(D, 1, function(d) ci[order(d)[1:2]] - 1L)))
  nr <- morie_causal_mahalanobis_match(cm_X, cm_t, k = 1, replace = FALSE)
  expect_equal(anyDuplicated(nr$matches[, 1]), 0L)
  cal <- morie_causal_mahalanobis_match(cm_X, cm_t, caliper = 0.5)
  expect_true(all(is.na(cal$distances[, 1]) | cal$distances[, 1] <= 0.5))
  expect_equal(cal$n_unmatched, sum(apply(D, 1, min) > 0.5))
  expect_error(morie_causal_mahalanobis_match(cm_X, cm_t, k = 2, replace = FALSE), "needs 8 controls")
  expect_error(morie_causal_mahalanobis_match(cm_X, cm_t + 1), "0/1")
})

test_that("propensity caliper matching on the logit scale", {
  ps <- c(0.7, 0.3, 0.6, 0.55, 0.2, 0.8, 0.45, 0.5, 0.35, 0.65)
  r <- morie_causal_caliper_matching(ps, cm_t)
  lg <- qlogis(ps)
  expect_equal(r$caliper_used, 0.2 * sd(lg), tolerance = 1e-12)
  ti <- which(cm_t == 1)
  ci <- which(cm_t == 0)
  for (a in seq_along(ti)) {
    d <- abs(lg[ci] - lg[ti[a]])
    if (min(d) <= r$caliper_used) {
      expect_equal(r$matches[a, 1], ci[which.min(d)] - 1L)
    } else {
      expect_equal(r$matches[a, 1], -1L)
    }
  }
  expect_equal(r$match_rate, mean(r$matches[, 1] >= 0))
  wide <- morie_causal_caliper_matching(ps, cm_t, caliper = 10, on_logit = FALSE)
  expect_equal(wide$n_unmatched, 0L)
  expect_identical(wide$estimand, "ATT")
  expect_error(morie_causal_caliper_matching(c(ps[-1], 1), cm_t), "strictly inside")
})

test_that("overlap diagnostic and covariate balance", {
  ps <- c(0.7, 0.3, 0.6, 0.55, 0.2, 0.8, 0.45, 0.5, 0.35, 0.65)
  o <- morie_causal_overlap_diagnostic(ps, cm_t, bins = 5)
  expect_equal(o$common_support, c(max(0.6, 0.2), min(0.8, 0.55)))
  expect_equal(o$n_outside, sum(ps < 0.6 | ps > 0.55))
  h1 <- tabulate(findInterval(ps[cm_t == 1], seq(0, 1, 0.2)), 5) / 4
  h0 <- tabulate(findInterval(ps[cm_t == 0], seq(0, 1, 0.2)), 5) / 6
  expect_equal(o$overlap_coefficient, sum(pmin(h1, h0)), tolerance = 1e-12)
  b <- morie_covariate_balance_check(cm_X, cm_t)
  v1 <- apply(cm_X[cm_t == 1, ], 2, var)
  v0 <- apply(cm_X[cm_t == 0, ], 2, var)
  smd <- (colMeans(cm_X[cm_t == 1, ]) - colMeans(cm_X[cm_t == 0, ])) / sqrt((v1 + v0) / 2)
  expect_equal(b$smd_before, smd, tolerance = 1e-12)
  expect_equal(b$variance_ratio, v1 / v0, tolerance = 1e-12)
  w <- seq(0.5, 1.4, by = 0.1)
  bw <- morie_covariate_balance_check(cm_X, cm_t, weights = w)
  wm <- function(m) colSums(cm_X[m, ] * w[m]) / sum(w[m])
  expect_equal(bw$smd_after, (wm(cm_t == 1) - wm(cm_t == 0)) / sqrt((v1 + v0) / 2), tolerance = 1e-12)
  expect_equal(bw$n_imbalanced, sum(abs(bw$smd_after) > 0.1))
  expect_error(morie_covariate_balance_check(cm_X, cm_t, weights = 1:3), "weights has 3")
})

test_that("IPTW weights for ATO, ATE and ATT", {
  ps <- c(0.7, 0.3, 0.6, 0.55, 0.2, 0.8, 0.45, 0.5, 0.35, 0.65)
  a <- morie_causal_iptw_atoweights(cm_t, ps)
  expect_equal(a$weights, ifelse(cm_t == 1, 1 - ps, ps))
  e <- morie_causal_iptw_atoweights(cm_t, ps, "ate")
  expect_equal(e$weights, ifelse(cm_t == 1, 0.4 / ps, 0.6 / (1 - ps)), tolerance = 1e-12)
  expect_equal(e$ess, sum(e$weights)^2 / sum(e$weights^2), tolerance = 1e-12)
  eu <- morie_causal_iptw_atoweights(cm_t, ps, "ate", stabilize = FALSE)
  expect_equal(eu$weights, ifelse(cm_t == 1, 1 / ps, 1 / (1 - ps)), tolerance = 1e-12)
  at <- morie_causal_iptw_atoweights(cm_t, ps, "att")
  raw <- ifelse(cm_t == 1, 1, ps / (1 - ps))
  expect_equal(at$weights, raw / mean(raw), tolerance = 1e-12)
  tr <- morie_causal_iptw_atoweights(cm_t, ps, "ate", trim = 0.25)
  expect_equal(tr$n_trimmed, sum(ps < 0.25 | ps > 0.75))
  expect_equal(tr$weights[ps < 0.25 | ps > 0.75], c(0, 0))
  expect_error(morie_causal_iptw_atoweights(cm_t, ps, "atc"), "should be one of")
})

test_that("falsification test is the OLS treatment coefficient", {
  y <- c(2.1, 1.8, 2.5, 2.0, 1.4, 2.9, 2.2, 2.6, 1.7, 2.3)
  r <- morie_causal_falsification_test(y, cm_t)
  f <- summary(lm(y ~ cm_t))$coefficients
  expect_equal(r$estimate, f[2, 1], tolerance = 1e-12)
  expect_equal(r$se, f[2, 2], tolerance = 1e-12)
  expect_equal(r$p_value, 2 * pnorm(-abs(f[2, 1] / f[2, 2])), tolerance = 1e-12)
  expect_equal(r$min_detectable_effect, 2.8 * f[2, 2], tolerance = 1e-12)
  rx <- morie_causal_falsification_test(y, cm_t, X_baseline = cm_X)
  expect_equal(rx$estimate, unname(coef(lm(y ~ cm_t + cm_X))[2]), tolerance = 1e-12)
  expect_error(morie_causal_falsification_test(y, rep(1, 10)), "non-empty")
})

test_that("entropy balancing reweights controls to the treated moments", {
  eb <- morie_entropy_balancing(cm_X, cm_t)
  C <- cm_X[cm_t == 0, ]
  expect_true(eb$converged)
  expect_equal(sum(eb$weights), 1, tolerance = 1e-12)
  expect_equal(colSums(C * eb$weights), colMeans(cm_X[cm_t == 1, ]), tolerance = 1e-8)
  # maximum entropy: weights are exponential tilts of the covariates
  lw <- log(eb$weights)
  expect_equal(unname(lw - mean(lw)), as.numeric(sweep(C, 2, colMeans(C)) %*% eb$lambda), tolerance = 1e-8)
  e2 <- morie_entropy_balancing(cm_X, cm_t, moments = 2)
  expect_equal(e2$n_constraints, 4L)
  expect_error(morie_entropy_balancing(cm_X, cm_t, moments = 3), "1 or 2")
})

test_that("Rosenbaum bounds: at Gamma = 1 the signed-rank normal test", {
  d <- c(1.2, 0.4, -0.3, 2.1, 0.8, 1.5, -0.6, 0.9, 1.1, 0.25)
  r <- morie_causal_rosenbaum_bound(d, gamma_max = 4, n_gamma = 7)
  expect_equal(r$p_upper[1], wilcox.test(d, alternative = "greater", exact = FALSE, correct = FALSE)$p.value, tolerance = 1e-12)
  rk <- rank(abs(d))
  g <- r$gamma_grid[4]
  pg <- g / (1 + g)
  expect_equal(r$p_upper[4], pnorm((sum(rk[d > 0]) - pg * 55) / sqrt(pg * (1 - pg) * sum(rk^2)), lower.tail = FALSE), tolerance = 1e-12)
  expect_false(is.unsorted(r$p_upper))
  expect_equal(r$gamma_critical, max(c(1, r$gamma_grid[r$p_upper < 0.05])))
  expect_error(morie_causal_rosenbaum_bound(c(1, 0, 2)), "at least 5")
})

test_that("triple differences from the eight cell means", {
  grid <- expand.grid(rep = 1:2, p = 0:1, t = 0:1, g = 0:1)
  y <- with(grid, 1 + 0.5 * g + 0.3 * t + 0.2 * p + 0.4 * t * p + 1.1 * g * t * p + rep / 10)
  r <- morie_causal_did_three_way(y, grid$t, grid$p, grid$g)
  expect_equal(r$ddd, 1.1, tolerance = 1e-12)
  expect_equal(r$did_placebo, 0.4, tolerance = 1e-12)
  expect_equal(r$did_eligible, 1.5, tolerance = 1e-12)
  expect_equal(r$se, sqrt(8 * var(c(0.1, 0.2)) / 2), tolerance = 1e-12)
  expect_error(morie_causal_did_three_way(y[-(1:2)], grid$t[-(1:2)], grid$p[-(1:2)], grid$g[-(1:2)]), "is empty")
  expect_error(morie_causal_did_three_way(y, grid$t + 1, grid$p, grid$g), "treated must be 0/1")
})
