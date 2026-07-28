# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the targeted-learning / heterogeneous-effect
# shelf. The anchors on the R side are the values printed by
# `scripts/audit/gen_cate_parity_anchors.py` in the Python tree, to full
# double precision.
#
# The fixture is a linear congruential generator using ONLY exact integer
# arithmetic and a division by a power of two. Every intermediate stays
# below 2^53, so R doubles and Python integers agree bit for bit. There
# is deliberately no `qnorm` here: R's Wichura AS241 and Python's Acklam
# inverse-normal disagree in the last ulp, which is by itself enough to
# break a ten-significant-digit anchor and send an afternoon chasing a
# numerical difference that is not a defect.

lcg_fixture <- function(n = 300L, seed = 20260728) {
  s <- seed
  nxt <- function() {
    s <<- (1664525 * s + 1013904223) %% 4294967296
    (s + 0.5) / 4294967296
  }
  w1 <- numeric(n); w2 <- numeric(n); a <- numeric(n); y <- numeric(n)
  for (i in seq_len(n)) {
    w1[i] <- 2 * nxt() - 1
    w2[i] <- 2 * nxt() - 1
    g <- 0.5 + 0.3 * w1[i]
    a[i] <- if (nxt() < g) 1 else 0
    e <- nxt() - 0.5
    y[i] <- 1 + 0.5 * w1[i] + 0.8 * w2[i] + (1 + 0.6 * w1[i]) * a[i] + 0.4 * e
  }
  list(y = y, a = a, w = cbind(w1, w2))
}

test_that("the fixture itself is bit-identical across languages", {
  fx <- lcg_fixture()
  # if any of these four drift, every anchor below is meaningless and
  # the failure is in the generator, not in the estimator
  expect_equal(sum(fx$y), 467.4340454476885, tolerance = 1e-12)
  expect_equal(sum(fx$a), 152)
  expect_equal(sum(fx$w[, 1]), 4.1483926856890321, tolerance = 1e-12)
  expect_equal(sum(fx$w[, 2]), -9.1403354154899716, tolerance = 1e-12)
})

test_that("morie_tmle_ate matches morie.fn._tmle.tmle_ate exactly", {
  fx <- lcg_fixture()
  out <- morie_tmle_ate(fx$y, fx$a, fx$w)
  # deterministic algorithm on both sides -- no RNG anywhere, so this is
  # a genuine ten-significant-digit anchor rather than a coincidence of
  # rounding. testthat's tolerance is RELATIVE.
  expect_equal(out$ate, 1.0056840246976821, tolerance = 1e-10)
  expect_equal(out$se, 0.024925117191629313, tolerance = 1e-10)
  expect_equal(out$epsilon, 0.00018496756831602379, tolerance = 1e-8)
  expect_equal(out$ey1, 1.9861556273216774, tolerance = 1e-10)
  expect_equal(out$ey0, 0.98047160262399491, tolerance = 1e-10)
})

test_that("the design's true ATE is recovered", {
  # tau(x) = 1 + 0.6 w1 with w1 ~ U(-1, 1), so E[tau] = 1 exactly
  fx <- lcg_fixture()
  out <- morie_tmle_ate(fx$y, fx$a, fx$w)
  expect_lt(abs(out$ate - 1), 0.05)
})

test_that("targeting solves the influence-function equation", {
  fx <- lcg_fixture()
  out <- morie_tmle_ate(fx$y, fx$a, fx$w)
  # this is the property that makes the EIF a legitimate source of the
  # standard error, not a decoration
  expect_lt(abs(mean(out$eif)), 1e-6)
})

test_that("TMLE cannot leave the parameter space", {
  # a substitution estimator evaluates the parameter at a fitted
  # distribution, so a bounded outcome yields a bounded contrast however
  # badly the nuisance fits behave -- AIPW with the same influence
  # function can and does escape
  fx <- lcg_fixture()
  yb <- as.numeric(fx$y > median(fx$y))
  out <- morie_tmle_ate(yb, fx$a, fx$w)
  expect_gte(out$ate, -1)
  expect_lte(out$ate, 1)
  expect_gte(out$ey1, 0)
  expect_lte(out$ey1, 1)
})

test_that("known propensities are used rather than refitted", {
  fx <- lcg_fixture()
  gtrue <- 0.5 + 0.3 * fx$w[, 1]
  fitted <- morie_tmle_ate(fx$y, fx$a, fx$w)
  known <- morie_tmle_ate(fx$y, fx$a, fx$w, g = gtrue)
  expect_equal(known$g, pmin(pmax(gtrue, 0.01), 0.99), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(fitted$epsilon, known$epsilon)))
  # both are consistent, so they must land close to each other and to 1
  expect_lt(abs(known$ate - fitted$ate), 0.05)
})

test_that("the causal forest recovers the same tau(x) as the Python forest", {
  # NOT a bit-parity test, and it cannot be one: R draws its subsamples
  # with sample.int (Mersenne Twister) while the Python forest uses
  # numpy's PCG64, so the two forests see different subsamples by
  # construction. What must agree is the estimand.
  fx <- lcg_fixture()
  out <- morie_causal_forest(fx$y, fx$a, fx$w, n_trees = 60L,
                             min_leaf = 10L, max_depth = 4L, seed = 0L)
  tau_true <- 1 + 0.6 * fx$w[, 1]
  # Python side on the same fixture: ate 1.0479, sd 0.3305
  expect_lt(abs(out$ate - 1), 0.15)
  expect_gt(stats::cor(out$cate, tau_true), 0.5)
  expect_gt(out$cate_sd, 0.1)
})

test_that("the forest finds heterogeneity only where there is some", {
  fx <- lcg_fixture()
  yflat <- 1 + 0.5 * fx$w[, 1] + 0.8 * fx$w[, 2] + 1.0 * fx$a
  het <- morie_causal_forest(fx$y, fx$a, fx$w, n_trees = 60L, seed = 0L)
  flat <- morie_causal_forest(yflat, fx$a, fx$w, n_trees = 60L, seed = 0L)
  # a constant effect must produce a visibly tighter CATE spread than
  # one that genuinely varies with w1
  expect_gt(het$cate_sd, flat$cate_sd)
})

test_that("out-of-bag predictions are not the in-bag ones", {
  fx <- lcg_fixture()
  out <- morie_causal_forest(fx$y, fx$a, fx$w, n_trees = 60L, seed = 0L)
  ok <- !is.na(out$cate_oob)
  expect_true(any(ok))
  # honest OOB predictions exclude the trees that saw the observation,
  # so they must differ from the all-tree average
  expect_false(isTRUE(all.equal(out$cate[ok], out$cate_oob[ok])))
})
