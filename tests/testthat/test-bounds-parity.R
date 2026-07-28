# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the permutation language-model loss, the
# differentially private mean, and the efficiency and minimax bounds.
# Anchors are the values printed by the Python core to full double
# precision.
#
# The fixture uses a linear congruential generator with exact integer
# arithmetic and a division by a power of two, so R doubles and Python
# integers agree bit for bit. Treatment assignment is thresholded
# against a RATIONAL function of the covariate rather than a logistic
# one, so the fixture itself never touches exp(). No qnorm anywhere.

bounds_fixture <- function(n = 200L, seed = 20260729) {
  s <- seed
  nxt <- function() {
    s <<- (1664525 * s + 1013904223) %% 4294967296
    (s + 0.5) / 4294967296
  }
  X <- matrix(0, n, 2L)
  for (i in seq_len(n)) X[i, ] <- c(2 * nxt() - 1, 2 * nxt() - 1)
  D <- numeric(n)
  for (i in seq_len(n)) D[i] <- as.numeric(nxt() < 0.5 + 0.3 * X[i, 1] / 2)
  y <- 1 + X[, 1] - 0.5 * X[, 2] + D
  for (i in seq_len(n)) y[i] <- y[i] + 2 * nxt() - 1
  V <- 4L; T_ <- 6L
  lg <- matrix(0, T_, V)
  for (i in seq_len(T_)) for (j in seq_len(V)) lg[i, j] <- 4 * nxt() - 2
  tg <- integer(T_)
  for (i in seq_len(T_)) tg[i] <- as.integer(V * nxt())
  list(X = X, D = D, y = y, logits = lg, targets = tg)
}

test_that("the fixture is bit-identical across languages", {
  fx <- bounds_fixture()
  expect_equal(sum(fx$D), 94)
  expect_equal(sum(fx$y), 290.843334046192, tolerance = 1e-12)
  expect_equal(sum(fx$logits), -3.89072284847498, tolerance = 1e-12)
  expect_equal(sum(fx$targets), 7L)
})

# ------------------------------------------------------------------
# XLNet permutation language-model loss
# ------------------------------------------------------------------

test_that("the permutation LM loss matches the Python core exactly", {
  fx <- bounds_fixture()
  z <- c(3L, 0L, 5L, 1L, 4L, 2L)
  out <- morie_permutation_lm_loss(fx$logits, fx$targets, z)
  expect_equal(out$loss, 1.7368767848656479, tolerance = 1e-12)
  expect_equal(out$perplexity, 5.6795771518533664, tolerance = 1e-12)
  expect_equal(out$token_nll[1], 0.66082049083655292, tolerance = 1e-12)
  expect_equal(out$mean_context_length, 2.5, tolerance = 1e-12)
})

test_that("partial prediction matches the Python core exactly", {
  fx <- bounds_fixture()
  z <- c(3L, 0L, 5L, 1L, 4L, 2L)
  out <- morie_permutation_lm_loss(fx$logits, fx$targets, z, num_predict = 2L)
  expect_equal(out$loss, 1.6963044706590762, tolerance = 1e-12)
  expect_equal(out$scored_positions, c(4L, 2L))
  expect_equal(out$mean_context_length, 4.5, tolerance = 1e-12)
  expect_false(out$order_invariant)
})

test_that("the full-sequence loss is invariant to the factorization order", {
  # reordering the terms of a sum does not change the sum; the
  # permutation acts on the model's conditioning, never on this
  # arithmetic
  fx <- bounds_fixture()
  base <- morie_permutation_lm_loss(fx$logits, fx$targets, 0:5)$loss
  set.seed(4)
  for (i in seq_len(10L)) {
    z <- sample(0:5)
    expect_equal(morie_permutation_lm_loss(fx$logits, fx$targets, z)$loss,
                 base, tolerance = 1e-14)
  }
  expect_true(morie_permutation_lm_loss(fx$logits, fx$targets, 0:5)$
                order_invariant)
})

test_that("a uniform model has perplexity equal to the vocabulary size", {
  out <- morie_permutation_lm_loss(matrix(0, 5L, 17L), rep(0L, 5L), 0:4)
  expect_equal(out$perplexity, 17, tolerance = 1e-12)
})

test_that("the two attention masks differ exactly on the diagonal", {
  m <- morie_permutation_attention_masks(c(2L, 0L, 3L, 1L))
  expect_equal(m$content & !m$query, diag(4L) == 1)
  expect_equal(m$query & !m$content, matrix(FALSE, 4L, 4L))
})

test_that("the identity and reversed orders give the causal masks", {
  expect_equal(morie_permutation_attention_masks(0:5)$content,
               lower.tri(matrix(0, 6L, 6L), diag = TRUE))
  expect_equal(morie_permutation_attention_masks(5:0)$content,
               upper.tri(matrix(0, 6L, 6L), diag = TRUE))
})

test_that("each position attends to exactly its rank many predecessors", {
  m <- morie_permutation_attention_masks(c(3L, 1L, 4L, 0L, 2L))
  expect_equal(as.integer(rowSums(m$query)), m$rank)
})

test_that("log-softmax stays finite at extreme logits", {
  out <- morie_permutation_lm_loss(matrix(c(1e4, -1e4, 0, 0), 2L, 2L),
                                   c(0L, 1L), 0:1)
  expect_true(all(is.finite(out$token_nll)))
  expect_equal(out$token_nll[1], 0, tolerance = 1e-9)
})

test_that("permutation LM input validation", {
  fx <- bounds_fixture()
  expect_error(morie_permutation_lm_loss(fx$logits, fx$targets,
                                         c(0L, 0L, 1L, 2L, 3L, 4L)),
               "must be a permutation")
  expect_error(morie_permutation_lm_loss(fx$logits, fx$targets[1:3], 0:5),
               "targets has length")
  expect_error(morie_permutation_lm_loss(fx$logits, rep(99L, 6L), 0:5),
               "targets must lie")
  expect_error(morie_permutation_lm_loss(fx$logits, fx$targets, 0:5,
                                         num_predict = 0L),
               "num_predict")
  expect_error(morie_permutation_attention_masks(integer(0)),
               "must not be empty")
})

# ------------------------------------------------------------------
# Differentially private mean
# ------------------------------------------------------------------

test_that("the private mean's deterministic parts match Python exactly", {
  # R and Python cannot be made to draw the same Laplace variate, so
  # the anchors are on everything the draw does not touch
  fx <- bounds_fixture()
  out <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 0.5, noise = 0)
  expect_equal(out$clipped_mean, 1.4371415204083313, tolerance = 1e-12)
  expect_equal(out$clipping_bias, -0.01707514982263092, tolerance = 1e-12)
  expect_equal(out$sensitivity, 0.02, tolerance = 1e-14)
  expect_equal(out$noise_scale, 0.04, tolerance = 1e-14)
  expect_equal(out$n_clipped, 13)
  expect_equal(out$sampling_se, 0.068432240551964466, tolerance = 1e-12)
})

test_that("supplying the noise makes the estimate exactly reproducible", {
  fx <- bounds_fixture()
  a <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 0.5, noise = 0.25)
  expect_equal(a$estimate, a$clipped_mean + 0.25, tolerance = 1e-14)
})

test_that("the mechanism is unbiased about the clipped mean", {
  set.seed(11)
  fx <- bounds_fixture()
  draws <- vapply(seq_len(4000L), function(i) {
    morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 1)$estimate
  }, numeric(1))
  mu <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 1,
                      noise = 0)$clipped_mean
  expect_lt(abs(mean(draws) - mu), 0.005)
})

test_that("the Laplace draw has the right scale", {
  set.seed(12)
  fx <- bounds_fixture()
  d <- vapply(seq_len(6000L), function(i) {
    morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 0.5)$noise_drawn
  }, numeric(1))
  expect_equal(stats::sd(d), sqrt(2) * 0.04, tolerance = 0.06)
})

test_that("tighter privacy costs more noise and more data costs less", {
  fx <- bounds_fixture()
  a <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 0.1, noise = 0)
  b <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 10, noise = 0)
  expect_gt(a$noise_sd, 10 * b$noise_sd)
  big <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 1, n = 20000L,
                       noise = 0)
  small <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 1, n = 200L,
                         noise = 0)
  expect_equal(small$noise_sd / big$noise_sd, 100, tolerance = 1e-9)
})

test_that("the naive interval is narrower than the honest one", {
  fx <- bounds_fixture()
  o <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 0.05, noise = 0)
  expect_gt(o$ci_upper - o$ci_lower,
            o$ci_naive_upper - o$ci_naive_lower)
})

test_that("clipping bias is reported, signed and warned about", {
  o <- morie_dp_mean(c(0, 0, 0, 100), C = 1, lower = 0, epsilon = 1,
                     noise = 0)
  expect_equal(o$n_clipped, 1)
  expect_lt(o$clipping_bias, 0)
  expect_true(any(grepl("clipped", o$warnings)))
})

test_that("choosing the width from the data is flagged as a leak", {
  fx <- bounds_fixture()
  o <- morie_dp_mean(fx$y, epsilon = 1, noise = 0)
  expect_true(any(grepl("range of y", o$warnings)))
})

test_that("the Gaussian mechanism uses the Dwork-Roth sigma", {
  fx <- bounds_fixture()
  o <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 0.5,
                     mechanism = "gaussian", delta = 1e-6, noise = 0)
  expect_equal(o$noise_scale, (4 / 200) * sqrt(2 * log(1.25 / 1e-6)) / 0.5,
               tolerance = 1e-12)
  hi <- morie_dp_mean(fx$y, C = 4, lower = -1, epsilon = 2,
                      mechanism = "gaussian", noise = 0)
  expect_true(any(grepl("epsilon < 1", hi$warnings)))
})

test_that("private mean input validation", {
  expect_error(morie_dp_mean(c(1, 2), C = 1, epsilon = 0),
               "epsilon must be positive")
  expect_error(morie_dp_mean(c(1, 2), C = -1), "C must be positive")
  expect_error(morie_dp_mean(numeric(0), C = 1), "at least one finite")
  expect_error(morie_dp_mean(c(1, 2), C = 1, mechanism = "gaussian",
                             delta = 2), "delta")
})

# ------------------------------------------------------------------
# Efficiency and minimax bounds
# ------------------------------------------------------------------

test_that("the efficiency bound matches the Python core exactly", {
  fx <- bounds_fixture()
  out <- morie_efficiency_bound_ate(fx$y, fx$D, fx$X)
  expect_equal(out$estimate, 0.95298854036596292, tolerance = 1e-10)
  expect_equal(out$efficiency_bound, 1.4264526381428226, tolerance = 1e-10)
  expect_equal(out$se_bound, 0.084452727550471174, tolerance = 1e-10)
  expect_equal(out$var_aipw, 1.3862424478850013, tolerance = 1e-10)
  expect_equal(out$var_ipw, 2.8876199760645114, tolerance = 1e-10)
  expect_equal(out$overlap_term, 1.4096066343858116, tolerance = 1e-10)
  expect_equal(out$heterogeneity_term, 0.01684600375701116,
               tolerance = 1e-10)
  expect_equal(out$minimax_regret_bound, 0.014354532076724911,
               tolerance = 1e-10)
  expect_equal(out$ate_ipw, 0.95297760131937603, tolerance = 1e-10)
  expect_equal(out$propensity[1], 0.45833796833256901, tolerance = 1e-10)
  expect_equal(out$tau_x[1], 1.0609963925045651, tolerance = 1e-10)
})

test_that("the minimax constant solves its own stationarity condition", {
  mc <- morie_minimax_regret_constant()
  expect_lt(mc$stationarity_residual, 1e-12)
  # solved, not quoted: the two-figure 0.17 in the literature is not
  # precise enough to check an attainment claim against simulation
  expect_equal(mc$constant, 0.16997120747990366, tolerance = 1e-12)
  expect_equal(mc$t_star, 0.75179152469356136, tolerance = 1e-10)
})

test_that("the constant really is the maximum of t * Phi(-t)", {
  mc <- morie_minimax_regret_constant()
  t <- seq(0, 4, length.out = 40001L)
  expect_lte(max(t * stats::pnorm(-t)), mc$constant + 1e-9)
})

test_that("the two components sum to the bound", {
  fx <- bounds_fixture()
  o <- morie_efficiency_bound_ate(fx$y, fx$D, fx$X)
  expect_equal(o$overlap_term + o$heterogeneity_term, o$efficiency_bound,
               tolerance = 1e-12)
  expect_equal(o$minimax_regret_bound, o$minimax_constant * o$se_bound,
               tolerance = 1e-14)
})

ate_design <- function(seed, n = 4000L, tau = 1, conf = 0.6, het = 0,
                       sd = 1) {
  set.seed(seed)
  X <- matrix(stats::rnorm(2 * n), n, 2L)
  e <- 1 / (1 + exp(-conf * X[, 1]))
  D <- as.numeric(stats::runif(n) < e)
  y <- 2 + X %*% c(1, -0.5) + D * (tau + het * X[, 2]) +
    stats::rnorm(n, sd = sd)
  list(y = as.vector(y), D = D, X = X)
}

test_that("AIPW recovers the effect and sits at the bound; IPW does not", {
  d <- ate_design(1L, n = 8000L)
  o <- morie_efficiency_bound_ate(d$y, d$D, d$X)
  expect_lt(abs(o$estimate - 1), 0.1)
  expect_equal(o$aipw_efficiency_ratio, 1, tolerance = 0.1)
  expect_gt(o$ipw_efficiency_ratio, 1.3)
})

test_that("worse overlap raises the bound", {
  a <- ate_design(2L, conf = 0.2); b <- ate_design(2L, conf = 2.5)
  oa <- morie_efficiency_bound_ate(a$y, a$D, a$X)
  ob <- morie_efficiency_bound_ate(b$y, b$D, b$X)
  expect_gt(ob$efficiency_bound, oa$efficiency_bound)
  expect_gt(ob$overlap_term, oa$overlap_term)
})

test_that("heterogeneity adds to the bound and is isolated from overlap", {
  a <- ate_design(3L, het = 0); b <- ate_design(3L, het = 1.5)
  oa <- morie_efficiency_bound_ate(a$y, a$D, a$X)
  ob <- morie_efficiency_bound_ate(b$y, b$D, b$X)
  expect_lt(oa$heterogeneity_term, 0.05)
  expect_gt(ob$heterogeneity_term, 1)
  expect_gt(ob$efficiency_bound, oa$efficiency_bound)
})

test_that("a noisier outcome raises only the overlap term", {
  a <- ate_design(4L, sd = 0.5); b <- ate_design(4L, sd = 2)
  oa <- morie_efficiency_bound_ate(a$y, a$D, a$X)
  ob <- morie_efficiency_bound_ate(b$y, b$D, b$X)
  expect_gt(ob$overlap_term, 3 * oa$overlap_term)
  expect_lt(abs(ob$heterogeneity_term - oa$heterogeneity_term), 0.05)
})

test_that("the bound on the standard error scales as one over root n", {
  a <- ate_design(5L, n = 1000L); b <- ate_design(5L, n = 16000L)
  oa <- morie_efficiency_bound_ate(a$y, a$D, a$X)
  ob <- morie_efficiency_bound_ate(b$y, b$D, b$X)
  expect_equal(oa$se_bound / ob$se_bound, 4, tolerance = 0.2)
})

test_that("the plug-in rule does not beat the minimax regret bound", {
  # simulate the local experiment the bound is stated for
  set.seed(6)
  mc <- morie_minimax_regret_constant()
  V <- 4; n <- 2000L
  worst <- 0
  for (h in seq(0, 6, length.out = 61L)) {
    tau <- h / sqrt(n)
    est <- stats::rnorm(40000L, tau, sqrt(V / n))
    worst <- max(worst, mean(ifelse(est > 0, 0, max(tau, 0))))
  }
  bound <- mc$constant * sqrt(V / n)
  expect_lte(worst, bound * 1.05)
  expect_gt(worst, bound * 0.9)
})

test_that("a binary outcome uses the Bernoulli variance", {
  set.seed(7)
  n <- 6000L
  X <- matrix(stats::rnorm(2 * n), n, 2L)
  D <- as.numeric(stats::runif(n) < 1 / (1 + exp(-0.5 * X[, 1])))
  p <- 1 / (1 + exp(-(0.3 * X[, 1] + 0.8 * D)))
  y <- as.numeric(stats::runif(n) < p)
  o <- morie_efficiency_bound_ate(y, D, X, family = "binomial")
  expect_gt(o$estimate, 0)
  expect_lt(o$estimate, 0.4)
  expect_true(all(o$mu1 > 0 & o$mu1 < 1))
})

test_that("trimming is reported when it binds and silent when it does not", {
  d <- ate_design(8L, conf = 4, n = 3000L)
  o <- morie_efficiency_bound_ate(d$y, d$D, d$X, trim = 0.05)
  expect_gt(o$trim_binding, 0)
  expect_true(any(grepl("trimmed", o$warnings)))
  g <- ate_design(9L, conf = 0.2)
  og <- morie_efficiency_bound_ate(g$y, g$D, g$X, trim = 0.01)
  expect_equal(og$trim_binding, 0)
  expect_false(any(grepl("trimmed", og$warnings)))
})

test_that("bounds input validation", {
  fx <- bounds_fixture()
  expect_error(morie_efficiency_bound_ate(fx$y[1:100], fx$D, fx$X),
               "must agree in length")
  expect_error(morie_efficiency_bound_ate(fx$y, fx$D * 2, fx$X),
               "D must be binary")
  expect_error(morie_efficiency_bound_ate(fx$y, fx$D, fx$X,
                                          family = "binomial"),
               "binary outcome")
  expect_error(morie_efficiency_bound_ate(fx$y[1:5], fx$D[1:5],
                                          fx$X[1:5, ]),
               "at least 10")
})
