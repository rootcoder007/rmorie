# SPDX-License-Identifier: AGPL-3.0-or-later
# Parity tests for the R causal-forest and TMLE tiers.

.hetero <- function(seed = 42, n = 1200) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * 3), ncol = 3)
  d <- as.numeric(stats::runif(n) < 0.5)
  tau <- 1 + 2 * X[, 1]
  y <- X[, 2] + tau * d + stats::rnorm(n, sd = 0.5)
  list(y = y, d = d, X = X, tau = tau)
}

.confounded <- function(seed = 42, n = 2000) {
  set.seed(seed)
  W <- matrix(stats::rnorm(n * 3), ncol = 3)
  e <- 1 / (1 + exp(-(W %*% c(1, -0.5, 0.3))))
  a <- as.numeric(stats::runif(n) < e)
  y <- 2 * a + as.vector(W %*% rep(1, 3)) + stats::rnorm(n, sd = 0.5)
  list(y = y, a = a, W = W)
}

.surv <- function(seed = 42, n = 1200) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * 2), ncol = 2)
  d <- as.numeric(stats::runif(n) < 0.5)
  t_ev <- stats::rexp(n, rate = exp(-0.5 * d))
  cens <- stats::rexp(n, rate = 1 / 4)
  list(time = pmin(t_ev, cens), event = as.numeric(t_ev <= cens),
       d = d, X = X)
}

test_that("the honest causal forest tracks true heterogeneity", {
  s <- .hetero()
  out <- morie_causal_forest(s$y, s$d, s$X, n_trees = 80L, min_leaf = 15L,
                             seed = 0L)
  ok <- is.finite(out$cate_oob)
  expect_gt(stats::cor(out$cate_oob[ok], s$tau[ok]), 0.4)
  expect_equal(out$ate, 1, tolerance = 0.5)
  expect_gt(out$cate_sd, 0.2)  # a constant-effect forest would be flat
  expect_error(morie_causal_forest(s$y, rep(0.5, length(s$y)), s$X), "binary")
})

test_that("the BLP test finds heterogeneity and rejects its absence", {
  s <- .hetero()
  f <- morie_causal_forest(s$y, s$d, s$X, n_trees = 80L, min_leaf = 15L,
                           seed = 0L)
  het <- morie_hte_blp_test(s$y, s$d, f$cate_oob)
  expect_true(het$heterogeneous)
  expect_gt(het$beta, 0)

  set.seed(7)
  n <- 1200
  Xc <- matrix(stats::rnorm(n * 3), ncol = 3)
  dc <- as.numeric(stats::runif(n) < 0.5)
  yc <- Xc[, 2] + 1 * dc + stats::rnorm(n, sd = 0.5)   # constant effect
  fc <- morie_causal_forest(yc, dc, Xc, n_trees = 80L, min_leaf = 15L,
                            seed = 1L)
  expect_gt(morie_hte_blp_test(yc, dc, fc$cate_oob)$p_value, 0.01)
})

test_that("bootstrap CATE intervals bracket the point estimate", {
  s <- .hetero(n = 700)
  out <- morie_causal_forest_bootstrap(s$y, s$d, s$X, B = 8L, n_trees = 30L,
                                       min_leaf = 15L, seed = 0L)
  expect_true(all(out$ci_low <= out$cate))
  expect_true(all(out$cate <= out$ci_high))
  expect_true(out$ate_ci[1] <= out$ate && out$ate <= out$ate_ci[2])
  expect_error(morie_causal_forest_bootstrap(s$y, s$d, s$X, B = 1L), "at least 2")
})

test_that("the quantile forest reports the shift direction", {
  set.seed(3)
  n <- 1200
  X <- matrix(stats::rnorm(n * 2), ncol = 2)
  d <- as.numeric(stats::runif(n) < 0.5)
  y <- 2 * d + stats::rnorm(n)          # treatment shifts the distribution up
  out <- morie_quantile_causal_forest(y, d, X, quantile = 0.5, n_trees = 60L,
                                      min_leaf = 20L, seed = 0L)
  expect_gt(mean(out$shift_effect, na.rm = TRUE), 0.15)
  expect_equal(out$threshold, stats::median(y))
  expect_error(morie_quantile_causal_forest(y, d, X, quantile = 1.5),
               "strictly")
})

test_that("the isotonic constraint removes every monotonicity violation", {
  s <- .hetero(n = 900)
  out <- morie_monotone_causal_forest(s$y, s$d, s$X, monotone_feature = 1L,
                                      n_trees = 50L, min_leaf = 20L, seed = 0L)
  expect_gt(out$violations_before, 0)
  expect_equal(out$violations_after, 0L)
  o <- order(s$X[, 1])
  expect_true(all(diff(out$cate[o]) >= -1e-9))
  dec <- morie_monotone_causal_forest(s$y, s$d, s$X, monotone_feature = 1L,
                                      direction = -1L, n_trees = 50L,
                                      min_leaf = 20L, seed = 0L)
  expect_true(all(diff(dec$cate[o]) <= 1e-9))
  expect_error(morie_monotone_causal_forest(s$y, s$d, s$X,
                                            monotone_feature = 9L), "column")
})

test_that("the causal survival forest gives a positive RMST difference", {
  s <- .surv()
  out <- morie_causal_survival_forest(s$time, s$event, s$d, s$X,
                                      n_trees = 50L, min_leaf = 20L, seed = 0L)
  expect_gt(out$ate, 0)          # treatment lengthens survival
  expect_gt(out$horizon, 0)
  blp <- morie_causal_survival_blp(s$time, s$event, s$d, s$X, n_trees = 50L,
                                   min_leaf = 20L, seed = 0L)
  expect_true(is.finite(blp$beta))
  expect_equal(blp$ate, out$ate)
  expect_error(morie_causal_survival_forest(-s$time, s$event, s$d, s$X),
               "positive")
})

test_that("the DR-learner recovers the ATE and the CATE slope", {
  s <- .hetero()
  out <- morie_dr_learner(s$y, s$d, s$X, n_folds = 5L, seed = 0L)
  expect_equal(out$ate, 1, tolerance = 0.3)
  expect_gt(stats::cor(out$cate, s$tau), 0.8)
  expect_error(morie_dr_learner(s$y, s$d, s$X, n_folds = 1L), "n_folds")
})

test_that("interventional effects sum to the overall effect", {
  set.seed(5)
  n <- 3000
  cv <- stats::rnorm(n)
  x <- as.numeric(stats::runif(n) < 0.5)
  m <- 0.8 * x + 0.4 * cv + stats::rnorm(n, sd = 0.6)
  y <- 0.5 * x + 1 * m + 0.3 * cv + stats::rnorm(n, sd = 0.6)
  out <- morie_interventional_effects(y, x, m, c = cv)
  expect_equal(out$overall, out$ide + out$iie)
  expect_equal(out$iie, 0.8, tolerance = 0.2)
  expect_equal(out$ide, 0.5, tolerance = 0.2)
  expect_error(morie_interventional_effects(y, x, m, n_draws = 10L),
               "at least 100")
})

test_that("TMLE removes confounding that the naive contrast keeps", {
  hits <- 0
  for (seed in 1:6) {
    s <- .confounded(seed)
    out <- morie_tmle_ate(s$y, s$a, s$W)
    naive <- mean(s$y[s$a == 1]) - mean(s$y[s$a == 0])
    expect_gt(abs(naive - 2), 0.3)
    hits <- hits + (abs(out$ate - 2) < 0.3)
    expect_true(out$ci[1] <= out$ate && out$ate <= out$ci[2])
  }
  expect_gte(hits, 5)
})

test_that("propensity-only TMLE stays consistent with a null outcome model", {
  s <- .confounded()
  out <- morie_tmle_propensity_only(s$y, s$a, s$W)
  expect_equal(out$ate, 2, tolerance = 0.35)
  expect_equal(out$ate_full, 2, tolerance = 0.3)
})

test_that("the truncation sweep is monotone in units affected", {
  s <- .confounded()
  out <- morie_tmle_truncation_sweep(s$y, s$a, s$W)
  expect_equal(length(out$ate), length(out$eps))
  expect_true(all(diff(out$n_truncated) >= 0))
  expect_error(morie_tmle_truncation_sweep(s$y, s$a, s$W, eps_grid = 0.6),
               "at least 2")
})

test_that("sensitivity bounds widen with Gamma and pinch at Gamma = 1", {
  s <- .confounded()
  out <- morie_tmle_sensitivity(s$y, s$a, s$W, gamma_grid = c(1, 1.5, 3))
  widths <- out$upper - out$lower
  expect_equal(widths[1], 0, tolerance = 1e-6)
  expect_lt(widths[2], widths[3])
  expect_equal(out$lower[1], out$ate, tolerance = 1e-6)
  expect_error(morie_tmle_sensitivity(s$y, s$a, s$W, gamma_grid = 0.5),
               "at least 1")
})

test_that("quantile TMLE recovers a location shift", {
  set.seed(9)
  n <- 2000
  W <- matrix(stats::rnorm(n * 2), ncol = 2)
  a <- as.numeric(stats::runif(n) < 1 / (1 + exp(-W[, 1])))
  y <- 2 * a + W[, 1] + stats::rnorm(n, sd = 0.5)
  out <- morie_tmle_quantile(y, a, W, quantile = 0.5, n_grid = 30L)
  expect_equal(out$qte, 2, tolerance = 0.7)
  expect_true(all(diff(out$f1) >= -1e-12))   # monotonised
  expect_error(morie_tmle_quantile(y, a, W, quantile = 0), "strictly")
})

test_that("TMLE mediation decomposes exactly", {
  set.seed(11)
  n <- 3000
  W <- matrix(stats::rnorm(n * 2), ncol = 2)
  A <- as.numeric(stats::runif(n) < 1 / (1 + exp(-W[, 1])))
  M <- 0.8 * A + 0.4 * W[, 1] + stats::rnorm(n, sd = 0.6)
  y <- 0.5 * A + 1 * M + 0.3 * W[, 1] + stats::rnorm(n, sd = 0.6)
  out <- morie_tmle_mediation(y, A, M, W)
  expect_equal(out$total, out$nde + out$nie)
  expect_equal(out$total, 1.3, tolerance = 0.35)
})

test_that("TMLE LATE recovers the complier effect", {
  set.seed(13)
  n <- 4000
  W <- matrix(stats::rnorm(n * 2), ncol = 2)
  Z <- as.numeric(stats::runif(n) < 0.5)
  typ <- sample(c("c", "a", "n"), n, replace = TRUE, prob = c(0.6, 0.2, 0.2))
  D <- ifelse(typ == "a", 1, ifelse(typ == "n", 0, Z))
  y <- 2 * D * (typ == "c") + W[, 1] + stats::rnorm(n, sd = 0.5)
  out <- morie_tmle_late(y, D, Z, W)
  expect_equal(out$late, 2, tolerance = 0.4)
  expect_true(out$compliance > 0.4 && out$compliance < 0.8)
  # a constant instrument trips the arm-variation guard inside the
  # first TMLE call, before the compliance check is ever reached
  expect_error(morie_tmle_late(y, D, rep(1, n), W), "both treatment arms")
})

test_that("sequential TMLE handles treatment-confounder feedback", {
  set.seed(17)
  n <- 3000
  L1 <- stats::rnorm(n)
  A1 <- as.numeric(stats::runif(n) < 1 / (1 + exp(-L1)))
  L2 <- 0.5 * L1 + 0.7 * A1 + stats::rnorm(n, sd = 0.7)
  A2 <- as.numeric(stats::runif(n) < 1 / (1 + exp(-1.5 * L2)))
  y <- A1 + A2 + L2 + stats::rnorm(n, sd = 0.5)
  A <- cbind(A1, A2); L <- cbind(L1, L2)
  hi <- morie_tmle_time_varying(y, A, L, regime = 1)
  lo <- morie_tmle_time_varying(y, A, L, regime = 0)
  expect_equal(hi$estimate - lo$estimate, 2.7, tolerance = 0.7)
  expect_equal(length(hi$epsilons), 2L)
  lng <- morie_tmle_longitudinal(y, A, L)
  expect_equal(lng$estimate, hi$estimate - lo$estimate)
})

test_that("TMLE RMST gives a positive difference when treatment helps", {
  set.seed(19)
  n <- 2000
  W <- matrix(stats::rnorm(n * 2), ncol = 2)
  a <- as.numeric(stats::runif(n) < 1 / (1 + exp(-W[, 1])))
  t_ev <- stats::rexp(n, rate = exp(-0.6 * a))
  cens <- stats::rexp(n, rate = 1 / 4)
  time <- pmin(t_ev, cens)
  event <- as.numeric(t_ev <= cens)
  out <- morie_tmle_rmst(time, event, a, W)
  expect_gt(out$rmst_difference, 0)
  expect_gt(out$rmst1, out$rmst0)
  expect_error(morie_tmle_rmst(-time, event, a, W), "positive")
})

test_that("TMLE matches the Python core to 10 decimals on a fixed dataset", {
  # Cross-language anchor. These values were produced by
  # morie.fn._tmle.tmle_ate on this exact dataset (verified 2026-07-27);
  # a drift in either implementation breaks this test.
  set.seed(101)
  n <- 1500
  W <- matrix(stats::rnorm(n * 3), ncol = 3)
  e <- 1 / (1 + exp(-(W %*% c(1, -0.5, 0.3))))
  a <- as.numeric(stats::runif(n) < e)
  y <- 2 * a + as.vector(W %*% rep(1, 3)) + stats::rnorm(n, sd = 0.5)
  out <- morie_tmle_ate(y, a, W)
  # testthat's tolerance is relative, so the anchors carry full precision
  expect_equal(out$ate, 1.9887976490933, tolerance = 1e-10)
  expect_equal(out$se, 0.027967669659031, tolerance = 1e-10)
  expect_equal(out$epsilon, -0.000208907469516885, tolerance = 1e-10)
  expect_equal(out$ey1, 2.03204542507311, tolerance = 1e-10)
  expect_equal(out$ey0, 0.043247775979815, tolerance = 1e-10)
})
