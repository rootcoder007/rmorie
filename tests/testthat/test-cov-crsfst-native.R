# Coverage tests for R/crsfst_native.R: the step-function RMST, the
# Fisher-Yates fold assignment on the .ghc stream and the cross-fitted
# survival forest's bookkeeping.

test_that("restricted mean survival time integrates the step curve", {
  cv <- list(t = c(1, 2, 4), s = c(0.9, 0.7, 0.4))
  expect_equal(morie_crsfst_rmst(cv, 3), 1 + 0.9 + 0.7)
  expect_equal(morie_crsfst_rmst(cv, 0.5), 0.5)
  expect_equal(morie_crsfst_rmst(cv, 10), 1 + 0.9 + 0.7 * 2 + 0.4 * 6)
  expect_equal(morie_crsfst_rmst(list(t = numeric(0), s = numeric(0)), 2), 2)
  expect_error(morie_crsfst_rmst(cv, 0), "horizon must be positive")
})

test_that("folds come from a Fisher-Yates shuffle", {
  f <- morie_crsfst_folds(11, 3, seed = 5)
  e <- .ghc_rng(5)
  idx <- 1:11
  for (i in 11:2) {
    j <- min(floor(.ghc_unif(e, 1L) * i), i - 1)
    tmp <- idx[i]
    idx[i] <- idx[j + 1]
    idx[j + 1] <- tmp
  }
  ref <- integer(11)
  ref[idx] <- (0:10) %% 3
  expect_equal(f, ref)
  expect_equal(as.vector(table(f)), c(4L, 4L, 3L))
  expect_error(morie_crsfst_folds(5, 1), "at least two folds")
  expect_error(morie_crsfst_folds(2, 3), "more folds than observations")
})

test_that("cross-fitted forest: out-of-fold effects and their summary", {
  n <- 48
  x <- cbind(sin(1:n), cos(2 * (1:n)))
  D <- rep(0:1, 24)
  tm <- round(1 + 3 * abs(sin(1:n * 1.7)) + 0.8 * D, 3)
  ev <- as.numeric((1:n) %% 4 != 0)
  r <- morie_crsfst(tm, ev, D, x, K = 3, n_trees = 3, seed = 2)
  ok <- !is.nan(r$cate)
  expect_equal(r$cate[ok], r$rmst_treated[ok] - r$rmst_control[ok])
  expect_equal(r$estimate, mean(r$cate[ok]), tolerance = 1e-12)
  expect_equal(r$se, sd(r$cate[ok]) / sqrt(sum(ok)), tolerance = 1e-12)
  expect_equal(r$ci_upper - r$ci_lower, 2 * 1.959963984540054 * r$se, tolerance = 1e-12)
  expect_equal(r$fold, morie_crsfst_folds(n, 3, 2))
  expect_equal(r$n_leaked, 0L)
  expect_equal(r$n_scored, sum(ok))
  expect_equal(r$tau, min(max(tm[D == 1]), max(tm[D == 0])))
  expect_equal(c(r$n_treated, r$n_events), c(24L, 36L))
  expect_true(all(r$rmst_treated[ok] <= r$tau + 1e-12))
  expect_match(morie_crsfst_cheatsheet(), "restricted mean")
  expect_error(morie_crsfst(tm[-1], ev, D, x), "agree in length")
  expect_error(morie_crsfst(tm[1:6], ev[1:6], D[1:6], x[1:6, ]), "at least eight")
  expect_error(morie_crsfst(tm, ev, rep(1, n), x), "both arms")
})
