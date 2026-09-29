# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tlgcmp_native.R (g-computation, van der Laan & Rose
# 2018 Chaps. 2 and 4). The point-treatment formula, the sequential
# recursion over two time points and the stratified counterfactual
# mean are recomputed by hand.

test_that("g_computation integrates the outcome means over the covariate law", {
  expect_equal(g_computation(c("a", "b", "c"), c(1, 2, 5), c(0.5, 0.2, 0.3)),
               0.5 * 1 + 0.2 * 2 + 0.3 * 5, tolerance = 1e-12)
  expect_equal(g_computation("only", 3, 1), 3)
  expect_error(g_computation(c("a", "b"), c(1, 2), c(0.5, 0.4)), "must sum to 1")
})

test_that("positivity_check reports the worst tail of the propensity", {
  r <- positivity_check(c(0.2, 0.5, 0.95))
  expect_equal(c(r$min_g, r$max_g), c(0.2, 0.95))
  expect_equal(r$worst, 0.05)
  expect_true(r$satisfied)
  expect_false(positivity_check(c(0.2, 0.995))$satisfied)
  expect_true(positivity_check(c(0.2, 0.995), delta = 0.001)$satisfied)
  expect_error(positivity_check(numeric(0)), "no propensity scores")
})

test_that("sequential_g_formula integrates the covariate law forward under the rule", {
  # two time points, L_t in {0, 1}; Q at the end reads the whole history
  Ls <- list(c(0, 1), c(0, 1))
  Lp <- list(function(h) c(0.7, 0.3),
             function(h) if (h[2] == 1) c(0.2, 0.8) else c(0.6, 0.4))
  Q <- list(NULL, NULL, function(h) h[1] + 2 * h[2] + 3 * h[3] + 4 * h[4])
  rule <- function(h) as.numeric(h[length(h)] > 0)
  ex <- 0
  for (j in 1:2) {
    l1 <- Ls[[1]][j]
    a1 <- as.numeric(l1 > 0)
    p1 <- Lp[[1]](numeric(0))[j]
    pr2 <- Lp[[2]](c(l1, a1))
    for (k in 1:2) {
      l2 <- Ls[[2]][k]
      a2 <- as.numeric(l2 > 0)
      ex <- ex + p1 * pr2[k] * Q[[3]](c(l1, a1, l2, a2))
    }
  }
  r <- sequential_g_formula(Q, Ls, Lp, rule)
  expect_equal(r$psi, ex, tolerance = 1e-12)
  expect_equal(r$horizon, 2L)
  expect_equal(r$estimate, r$psi)
  # a rule that always treats gives a different value
  always <- sequential_g_formula(Q, Ls, Lp, function(h) 1)
  expect_false(isTRUE(all.equal(always$psi, ex)))
  expect_error(sequential_g_formula(Q, Ls, Lp[1], rule), "2 covariate supports but 1")
  expect_error(sequential_g_formula(Q, Ls, list(function(h) c(0.5, 0.2), Lp[[2]]), rule),
               "conditional law at time 0 sums to")
})

test_that("counterfactual_mean averages the stratum means over the stratum law", {
  Y <- c(1, 2, 3, 4, 5, 6, 7, 8)
  A <- c(1, 0, 1, 0, 1, 0, 1, 0)
  L <- c("x", "x", "x", "x", "y", "y", "y", "y")
  ex <- 0.5 * mean(Y[c(1, 3)]) + 0.5 * mean(Y[c(5, 7)])
  expect_equal(counterfactual_mean(Y, A, L, 1), ex, tolerance = 1e-12)
  expect_equal(counterfactual_mean(Y, A, L, 0), 0.5 * mean(Y[c(2, 4)]) + 0.5 * mean(Y[c(6, 8)]),
               tolerance = 1e-12)
  # an external stratum law reweights the same stratum means
  expect_equal(counterfactual_mean(Y, A, L, 1, strata_probs = list(x = 0.25, y = 0.75)),
               0.25 * mean(Y[c(1, 3)]) + 0.75 * mean(Y[c(5, 7)]), tolerance = 1e-12)
  expect_error(counterfactual_mean(Y[-1], A, L, 1), "differ in length")
  expect_error(counterfactual_mean(Y, c(1, 1, 1, 1, 0, 0, 0, 0), L, 0), "positivity violation")
})

test_that("morie_tlgcmp dispatches on mode", {
  expect_equal(morie_tlgcmp(c("a", "b"), c(1, 3), c(0.4, 0.6)), 0.4 + 1.8, tolerance = 1e-12)
  expect_true(morie_tlgcmp(g = c(0.3, 0.7), mode = "positivity")$satisfied)
  Y <- c(1, 2, 3, 4)
  expect_equal(morie_tlgcmp(Y = Y, A = c(1, 0, 1, 0), L = c("x", "x", "y", "y"), a_star = 1,
                            mode = "counterfactual"), 0.5 * 1 + 0.5 * 3, tolerance = 1e-12)
  sq <- morie_tlgcmp(Q_functions = list(NULL, function(h) h[1]), L_supports = list(c(0, 1)),
                     L_probs = list(function(h) c(0.5, 0.5)), rule = function(h) 1,
                     mode = "sequential")
  expect_equal(sq$psi, 0.5, tolerance = 1e-12)
  expect_error(morie_tlgcmp(mode = "iptw"))
})
