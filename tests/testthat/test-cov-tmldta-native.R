# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tmldta_native.R (data-adaptive target parameters,
# Hubbard, Kennedy & van der Laan 2018, Ch. 9). The outcome model is
# refitted with glm() on the saturated-in-A design, the level choice
# is argmin / argmax of the mean predictions, the targeting step is
# checked through its score equation, and the cross-validated
# estimate through eqs. (9.14)-(9.15).

.td_n <- 40
.td_W <- cbind(sin(1:40), cos(1:40 / 3))
.td_A <- (1:40) %% 3
.td_y <- plogis(0.8 * (.td_A == 2) - 0.6 * (.td_A == 1) - .td_W[, 1] + 0.3 * cos(1:40))

.td_design <- function(a, rows) {
  d1 <- as.numeric(a == 1)
  d2 <- as.numeric(a == 2)
  W <- .td_W[rows, , drop = FALSE]
  cbind(1, d1, d2, W, d1 * W, d2 * W)
}
.td_qfit <- function(rows) {
  X <- .td_design(.td_A[rows], rows)
  suppressWarnings(coef(glm(.td_y[rows] ~ X - 1, family = binomial(), control = list(epsilon = 1e-14, maxit = 100))))
}

test_that("discover_levels picks argmin / argmax of the mean fitted outcome", {
  rows <- 1:30
  b <- .td_qfit(rows)
  means <- vapply(0:2, function(a) mean(plogis(.td_design(rep(a, 40), 1:40) %*% b)), 0)
  r <- discover_levels(.td_y, .td_A, .td_W, c(0, 1, 2), rows = rows, eval_rows = 1:40)
  # glm's default convergence tolerance inside the package: 1e-6 on the means
  expect_equal(unname(r$info$means), means, tolerance = 1e-6)
  expect_equal(r$aL, c(0, 1, 2)[which.min(means)])
  expect_equal(r$aH, c(0, 1, 2)[which.max(means)])
  expect_equal(r$info$spread, max(means) - min(means), tolerance = 1e-6)
})

test_that("split_specific_tmle solves the efficient-score equation for epsilon", {
  fit <- 1:28
  est <- 29:40
  s <- split_specific_tmle(.td_y, .td_A, .td_W, c(0, 1, 2), 1, 2, fit, est)
  s0 <- split_specific_tmle(.td_y, .td_A, .td_W, c(0, 1, 2), 1, 2, fit, est, target = FALSE)
  b <- .td_qfit(fit)
  qa <- function(a) plogis(.td_design(rep(a, 40), 1:40) %*% b)[est]
  expect_equal(s0$psi, mean(qa(2) - qa(1)), tolerance = 1e-6)
  expect_equal(s0$info$eps, 0)
  expect_equal(unname(mean(s$D)), 0, tolerance = 1e-9)
  expect_length(s$D, 12L)
  expect_gte(s$info$max_weight, 1)
})

test_that("morie_tmldta averages split estimates (9.14) with the IC variance (9.15)", {
  for (fn in list(morie_tmldta, morie_tmledataadaptive)) {
    r <- fn(.td_y, .td_A, .td_W, n_folds = 4)
    expect_equal(r$n_splits, 4L)
    expect_equal(r$estimate, mean(r$split_estimates), tolerance = 1e-12)
    expect_equal(r$ci, r$estimate + c(-1, 1) * qnorm(0.975) * r$se, tolerance = 1e-12)
    expect_equal(r$se, r$sigma / sqrt(40), tolerance = 1e-12)
    expect_equal(sum(unlist(r$level_counts)), 4L)
    # fold v estimates on rows i with i %% 4 == v, levels from the rest
    ss <- split_specific_tmle((.td_y - min(.td_y)) / diff(range(.td_y)), .td_A, .td_W, c(0, 1, 2),
                              r$levels_by_split[[1]][1], r$levels_by_split[[1]][2],
                              which((1:40) %% 4 != 0), which((1:40) %% 4 == 0))
    expect_equal(r$split_estimates[1], diff(range(.td_y)) * ss$psi, tolerance = 1e-12)
    nv <- fn(.td_y, .td_A, .td_W, method = "naive")
    expect_equal(nv$n_splits, 1L)
    expect_equal(nv$epsilon, 0)
    ss2 <- fn(.td_y, .td_A, .td_W, method = "sample-split", n_folds = 2, candidate_strata = c(2, 0, 1))
    expect_equal(ss2$candidate_levels, c(2, 0, 1))
  }
  expect_error(morie_tmldta(.td_y, .td_A, .td_W, method = "x"), "method must be one of")
  expect_error(morie_tmldta(.td_y, .td_A[-1], .td_W), "differ in length")
  expect_error(morie_tmldta(.td_y, .td_A, .td_W[-1, ]), "covariate rows")
  expect_error(morie_tmldta(.td_y, .td_A, .td_W, trim = 0.6), "trim must be")
  expect_error(morie_tmldta(.td_y[1:5], .td_A[1:5], .td_W[1:5, ]), "at least 8")
  expect_error(morie_tmldta(.td_y, .td_A, .td_W, candidate_strata = c(0, 7)), "never occur")
  expect_error(morie_tmldta(.td_y, rep(1, 40), .td_W), "2 exposure levels")
  expect_error(morie_tmldta(rep(1, 40), .td_A, .td_W), "no range")
  expect_error(morie_tmldta(.td_y, .td_A, .td_W, bounds = c(0.5, 1)), "outside bounds")
})

test_that("morie_tmle_data_adaptive (IRLS arm) agrees with the glm arm", {
  a <- morie_tmldta(.td_y, .td_A, .td_W, n_folds = 4)
  b <- morie_tmle_data_adaptive(.td_y, .td_A, .td_W, n_folds = 4)
  # the IRLS arm carries a 1e-10 ridge and glm stops at its own tolerance
  expect_equal(b$estimate, a$estimate, tolerance = 1e-6)
  expect_equal(b$se, a$se, tolerance = 1e-6)
  expect_identical(b$levels_by_split, a$levels_by_split)
  nb <- morie_tmle_data_adaptive(.td_y, .td_A, NULL, method = "naive")
  na <- morie_tmldta(.td_y, .td_A, NULL, method = "naive")
  expect_equal(nb$estimate, na$estimate, tolerance = 1e-6)
  expect_error(morie_tmle_data_adaptive(.td_y, .td_A, .td_W, method = "x"), "method must be one of")
  expect_error(morie_tmle_data_adaptive(.td_y, .td_A, .td_W, trim = 0), "trim must be")
})

test_that("morie_variable_importance runs tmldta per column and ranks by |effect|", {
  X <- cbind(.td_A, round(.td_W[, 1]))
  vi <- morie_variable_importance(.td_y, X, n_folds = 3, names = c("A", "W1"))
  e1 <- morie_tmldta(.td_y, X[, 1], X[, 2, drop = FALSE], n_folds = 3)$estimate
  e2 <- morie_tmldta(.td_y, X[, 2], X[, 1, drop = FALSE], n_folds = 3)$estimate
  est <- c(A = e1, W1 = e2)
  expect_identical(vapply(vi, function(d) d$variable, ""), names(est)[order(-abs(est))])
  expect_equal(vapply(vi, function(d) d$estimate, 0), unname(est[order(-abs(est))]), tolerance = 1e-12)
  expect_equal(vapply(vi, function(d) d$rank, 0L), 1:2)
  expect_error(morie_variable_importance(.td_y, X[, 1, drop = FALSE]), "at least 2 columns")
  expect_error(morie_variable_importance(.td_y, X, names = "A"), "differ in length")
})

test_that("morie_tmldta_cheatsheet names the split rule", {
  expect_match(morie_tmldta_cheatsheet(), "NOT on the same rows", fixed = TRUE)
})
