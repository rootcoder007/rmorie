# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/trclrn_native.R (tree-based individualised treatment
# rules, Laber & Zhao 2015). The IPW value is recomputed as the mean
# of 1{A = pi(X)} Y / p, the augmented value from its influence-curve
# form, and a fitted tree on data whose optimal rule is known by
# construction must recover that rule and beat both fixed arms.

.tr_x <- c(-2, -1.5, -1, -0.8, -0.6, -0.2, 0.2, 0.6, 0.8, 1, 1.5, 2,
           -1.8, -1.2, -0.4, 0.4, 1.2, 1.8, -0.9, 0.9)
.tr_a <- rep(c(0L, 1L), 10)
# treatment 1 helps when x > 0, treatment 0 helps when x < 0
.tr_y <- ifelse(.tr_x > 0, ifelse(.tr_a == 1L, 3, 1), ifelse(.tr_a == 0L, 3, 1))

.tr_val <- function(rule, p = rep(0.5, 20)) {
  mean(vapply(seq_along(.tr_x), function(i) {
    (rule(.tr_x[i]) == .tr_a[i]) * .tr_y[i] / p[i]
  }, 0))
}

test_that("rule_value is the IPW value of the rule", {
  r1 <- function(x) as.integer(x > 0)
  expect_equal(trclrn_rule_value(.tr_y, .tr_a, .tr_x, r1), .tr_val(r1), tolerance = 1e-12)
  expect_equal(trclrn_rule_value(.tr_y, .tr_a, .tr_x, function(x) 0L), .tr_val(function(x) 0L),
               tolerance = 1e-12)
  # the correct rule beats either fixed arm
  expect_gt(.tr_val(r1), .tr_val(function(x) 0L))
  expect_gt(.tr_val(r1), .tr_val(function(x) 1L))
  # a non-uniform propensity reweights the concordant subjects
  p <- rep(c(0.4, 0.6), 10)
  expect_equal(trclrn_rule_value(.tr_y, .tr_a, .tr_x, r1, propensity = p), .tr_val(r1, p), tolerance = 1e-12)
  expect_equal(trclrn_rule_value(.tr_y, .tr_a, .tr_x, r1, propensity = 0.5), .tr_val(r1), tolerance = 1e-12)
  # augmented: m(x, pi(x)) + 1{A = pi} (Y - m) / p
  om <- function(x, a) if (a == 1L) 2 else 2.5
  aug <- mean(vapply(seq_along(.tr_x), function(i) {
    m <- om(.tr_x[i], r1(.tr_x[i]))
    m + (r1(.tr_x[i]) == .tr_a[i]) * (.tr_y[i] - m) / 0.5
  }, 0))
  expect_equal(trclrn_rule_value(.tr_y, .tr_a, .tr_x, r1, method = "augmented", outcome_model = om),
               aug, tolerance = 1e-12)
  expect_error(trclrn_rule_value(.tr_y, .tr_a, .tr_x, r1, method = "q"), "method must be ipw or augmented")
  expect_error(trclrn_rule_value(.tr_y[-1], .tr_a, .tr_x, r1), "must agree in length")
  expect_error(trclrn_rule_value(.tr_y[1:3], .tr_a[1:3], .tr_x[1:3], r1), "at least 4 observations")
  expect_error(trclrn_rule_value(.tr_y, rep(1L, 20), .tr_x, r1), "at least 2 treatment arms")
  expect_error(trclrn_rule_value(.tr_y, .tr_a, .tr_x, r1, propensity = c(0.5, 0.5)), "2 propensities for 20")
  expect_error(trclrn_rule_value(.tr_y, .tr_a, .tr_x, r1, propensity = 0.001), "positivity")
  expect_error(trclrn_rule_value(.tr_y, .tr_a, .tr_x, r1, method = "augmented"), "needs an")
})

test_that("fit_tree recovers the sign rule and reports its value", {
  for (fn in list(trclrn_fit_tree, morie_trclrn, trclrn_treeoptimalregime, trclrn_tree_based_regime)) {
    f <- fn(.tr_y, .tr_a, .tr_x, max_depth = 2, min_leaf = 4)
    expect_false(f$tree$leaf)
    expect_equal(f$tree$feature, 1L)
    # the split lands at one of the x values bracketing zero
    expect_true(f$tree$threshold %in% c(-0.2, 0.2, 0.4))
    expect_equal(f$n_leaves, 2L)
    expect_identical(trclrn_predict_rule(f$tree, matrix(c(-1, 1), ncol = 1)), c(0L, 1L))
    expect_equal(f$value, trclrn_rule_value(.tr_y, .tr_a, .tr_x, f$rule), tolerance = 1e-12)
    # the fitted rule's IPW value is at least the sign rule's (it may
    # exceed it: one discordant point at x = -0.2 is scored either way)
    expect_gte(f$value, .tr_val(function(x) as.integer(x > 0)))
    expect_equal(unlist(f$fixed_arm_values),
                 c("0" = .tr_val(function(x) 0L), "1" = .tr_val(function(x) 1L)), tolerance = 1e-12)
    expect_gt(f$value, max(unlist(f$fixed_arm_values)))
    expect_equal(f$arms, c(0L, 1L))
  }
  # depth 0 cannot split: one leaf with the better fixed arm
  d0 <- trclrn_fit_tree(.tr_y, .tr_a, .tr_x, max_depth = 0)
  expect_true(d0$tree$leaf)
  expect_equal(d0$n_leaves, 1L)
  expect_equal(d0$value, max(unlist(d0$fixed_arm_values)), tolerance = 1e-12)
  expect_identical(d0$tree$treatment, d0$best_fixed_arm)
  a <- trclrn_fit_tree(.tr_y, .tr_a, .tr_x, method = "augmented",
                       outcome_model = function(x, a) 2, max_depth = 2, min_leaf = 4)
  expect_equal(a$value, trclrn_rule_value(.tr_y, .tr_a, .tr_x, a$rule, method = "augmented",
                                          outcome_model = function(x, a) 2), tolerance = 1e-12)
  expect_error(trclrn_fit_tree(.tr_y, .tr_a, .tr_x, method = "augmented"), "needs an")
  expect_error(trclrn_fit_tree(.tr_y, .tr_a, .tr_x, method = "bart"), "method must be ipw or augmented")
  expect_error(trclrn_fit_tree(.tr_y, .tr_a, .tr_x, min_leaf = 0), "min_leaf must be at least 1")
})

test_that("predict_rule and tree_rules walk the tree", {
  f <- trclrn_fit_tree(.tr_y, .tr_a, .tr_x, max_depth = 2, min_leaf = 4)
  thr <- f$tree$threshold
  xs <- c(-5, thr - 1e-9, thr, 5)
  expect_identical(trclrn_predict_rule(f$tree, xs), c(0L, 0L, 1L, 1L))
  expect_identical(trclrn_predict_rule(f$tree, list(-3, 3)), c(0L, 1L))
  expect_identical(trclrn_predict_rule(f$tree, matrix(numeric(0), 0, 1)), integer(0))
  txt <- trclrn_tree_rules(f$tree, names = "score")
  expect_match(txt[1], sprintf("if score < %.6g:", thr), fixed = TRUE)
  expect_match(txt[2], "treat with 0")
  expect_identical(txt[3], "else:")
  expect_match(txt[4], "treat with 1")
  expect_match(trclrn_tree_rules(f$tree)[1], "if x1 <", fixed = TRUE)
  expect_identical(trclrn_tree_rules(list(leaf = TRUE, treatment = 1L, n = 7L)),
                   "treat with 1  (n = 7)")
})

test_that("trclrn_cheatsheet gives the value formula", {
  expect_match(trclrn_cheatsheet(), "1{A = pi(X)} / p(A|X)", fixed = TRUE)
})
