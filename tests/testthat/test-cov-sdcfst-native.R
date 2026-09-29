# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/sdcfst_native.R (cross-fitted doubly robust treatment
# effects). With the linear learner and K = 1 the nuisance fits are
# lm() and a ridge logistic regression, so each score (AIPW, IPW,
# plug-in, partialling-out) can be recomputed exactly, together with
# its influence-function standard error.

.dr_n <- 24
.dr_X <- cbind(x1 = cos(1:24), x2 = (1:24) %% 4 / 3)
.dr_D <- c(rep(c(1, 0), 10), 1, 1, 0, 0)
.dr_y <- 1 + 2 * .dr_D + 0.5 * .dr_X[, 1] - .dr_X[, 2] + sin(1:24) / 4

test_that("logistic is a ridge-penalised logistic regression", {
  rows <- seq_len(.dr_n)
  b <- morie_sdcfst_logistic(.dr_X, .dr_D, rows, ridge = 1e-6)
  g <- glm(.dr_D ~ .dr_X, family = binomial(), control = list(epsilon = 1e-13, maxit = 200))
  expect_equal(b, unname(coef(g)), tolerance = 1e-4)
  # the score of the penalised likelihood vanishes at the fit
  d <- cbind(1, .dr_X)
  mu <- plogis(as.numeric(d %*% b))
  expect_equal(as.numeric(crossprod(d, .dr_D - mu)) - 1e-6 * b, rep(0, 3), tolerance = 1e-8)
  sub <- morie_sdcfst_logistic(.dr_X, .dr_D, 1:12, ridge = 1e-6)
  expect_length(sub, 3L)
})

test_that("forest and predict average honest leaf means", {
  rows <- seq_len(.dr_n)
  f <- morie_sdcfst_forest(.dr_X, .dr_y, rows, n_trees = 6, mtry = 2, min_leaf = 3, max_depth = 3,
                           e = .ghc_rng(4))
  expect_length(f$trees, 6L)
  expect_equal(f$mtry, 2)
  p <- morie_sdcfst_predict(f, .dr_X[1, ])
  each <- vapply(f$trees, function(t) {
    nd <- t
    while (!nd$leaf) nd <- if (.dr_X[1, nd$feature] <= nd$cut) nd$left else nd$right
    nd$value
  }, 0)
  expect_equal(p, mean(each), tolerance = 1e-12)
  expect_gte(p, min(.dr_y))
  expect_lte(p, max(.dr_y))
  # mtry defaults to round(sqrt(p))
  expect_equal(morie_sdcfst_forest(.dr_X, .dr_y, rows, n_trees = 1, e = .ghc_rng(1))$mtry, 1L)
  # a tree grown on a constant outcome predicts that constant
  cf <- morie_sdcfst_forest(.dr_X, rep(2, 24), rows, n_trees = 2, e = .ghc_rng(2))
  expect_equal(morie_sdcfst_predict(cf, .dr_X[5, ]), 2)
})

test_that("the linear learner reproduces each score exactly at K = 1", {
  des <- cbind(1, .dr_X)
  b1 <- unname(coef(lm(.dr_y[.dr_D == 1] ~ .dr_X[.dr_D == 1, ])))
  b0 <- unname(coef(lm(.dr_y[.dr_D == 0] ~ .dr_X[.dr_D == 0, ])))
  ba <- unname(coef(lm(.dr_y ~ .dr_X)))
  bp <- morie_sdcfst_logistic(.dr_X, .dr_D, seq_len(.dr_n), 1e-6)
  m1 <- as.numeric(des %*% b1)
  m0 <- as.numeric(des %*% b0)
  ma <- as.numeric(des %*% ba)
  ps <- pmin(pmax(plogis(as.numeric(des %*% bp)), 0.02), 0.98)
  r <- morie_sdcfst(.dr_y, .dr_D, .dr_X, K_fold = 1, learner = "linear")
  expect_equal(r$m1, m1, tolerance = 1e-8)
  expect_equal(r$m0, m0, tolerance = 1e-8)
  expect_equal(r$propensity, ps, tolerance = 1e-6)
  aipw <- m1 - m0 + .dr_D * (.dr_y - m1) / ps - (1 - .dr_D) * (.dr_y - m0) / (1 - ps)
  expect_equal(r$influence, aipw, tolerance = 1e-6)
  expect_equal(r$estimate, mean(aipw), tolerance = 1e-6)
  expect_equal(r$se, sqrt(sum((aipw - mean(aipw))^2) / (24 * 23)), tolerance = 1e-6)
  expect_equal(r$ci_lower, r$estimate - 1.959963984540054 * r$se, tolerance = 1e-12)
  expect_equal(r$z, r$estimate / r$se, tolerance = 1e-9)
  # the effect is 2 by construction
  expect_lt(abs(r$estimate - 2), 4 * r$se)
  pl <- morie_sdcfst(.dr_y, .dr_D, .dr_X, K_fold = 1, learner = "linear", score = "plugin")
  expect_equal(pl$estimate, mean(m1 - m0), tolerance = 1e-8)
  ip <- morie_sdcfst(.dr_y, .dr_D, .dr_X, K_fold = 1, learner = "linear", score = "ipw")
  expect_equal(ip$estimate, mean(.dr_D * .dr_y / ps - (1 - .dr_D) * .dr_y / (1 - ps)), tolerance = 1e-6)
  po <- morie_sdcfst(.dr_y, .dr_D, .dr_X, K_fold = 1, learner = "linear", score = "partialling_out")
  v <- .dr_D - ps
  u <- .dr_y - ma
  expect_equal(po$estimate, sum(v * u) / sum(v * v), tolerance = 1e-6)
  expect_equal(po$influence, v * (u - po$estimate * v) / (sum(v * v) / 24), tolerance = 1e-5)
})

test_that("cross-fitting splits the sample and the forest learner runs", {
  r <- morie_sdcfst(.dr_y, .dr_D, .dr_X, K_fold = 3, learner = "linear", seed = 5)
  expect_length(r$fold_estimates, 3L)
  expect_equal(r$estimate, mean(r$influence), tolerance = 1e-12)
  expect_equal(r$n_treated, sum(.dr_D))
  expect_equal(r$K_fold, 3L)
  # the folds partition the sample: their sizes add to n
  e <- .ghc_rng(5)
  lab <- .sdcfst_folds(24, 3, e)
  expect_setequal(lab, 0:2)
  expect_equal(length(lab), 24L)
  f <- morie_sdcfst(.dr_y, .dr_D, .dr_X, K_fold = 2, n_trees = 4, min_leaf = 3, max_depth = 3, seed = 2)
  expect_true(is.finite(f$estimate))
  expect_gte(f$min_propensity, 0.02)
  expect_lte(f$max_propensity, 0.98)
  expect_equal(f$learner, "forest")
  # trimming is counted
  tt <- morie_sdcfst(.dr_y, .dr_D, .dr_X, K_fold = 1, learner = "linear", trim = 0.45)
  expect_gt(tt$trimmed, 0L)
  expect_error(morie_sdcfst(.dr_y, .dr_D, .dr_X, score = "tmle"), "score must be one of")
  expect_error(morie_sdcfst(.dr_y, .dr_D, .dr_X, learner = "boost"), "learner must be one of")
  expect_error(morie_sdcfst(.dr_y, .dr_D[-1], .dr_X), "same length")
  expect_error(morie_sdcfst(.dr_y, .dr_D * 2, .dr_X), "must be binary")
  expect_error(morie_sdcfst(.dr_y[1:4], .dr_D[1:4], .dr_X[1:4, ]), "at least eight")
  expect_error(morie_sdcfst(.dr_y, .dr_D, .dr_X, K_fold = 99), "K_fold must lie in 1..n")
  expect_error(morie_sdcfst(.dr_y, rep(1, 24), .dr_X), "must be binary|one treatment arm empty")
})

test_that("morie_sdcfst_cheatsheet lists the scores and learners", {
  s <- morie_sdcfst_cheatsheet()
  expect_match(s, "aipw, partialling_out, ipw, plugin", fixed = TRUE)
  expect_match(s, "forest, linear", fixed = TRUE)
})
