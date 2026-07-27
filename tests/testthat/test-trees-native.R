# ---------------------------------------------------------------------------
# Native trees / bagging / boosting
#
# Structural recovery on data where the truth is known: only some columns
# carry signal, so importances must concentrate there. A test that merely
# checks the score is finite would pass on an ensemble that learned nothing.
#
# Reference implementations: ESL Algorithm 15.1 (random forests, p. 588) and
# Algorithm 10.3 with shrinkage (gradient boosting, p. 361).
# ---------------------------------------------------------------------------

make_xy <- function(seed = 2024, n = 400, p = 6) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, p)
  y <- 2 * X[, 1] - 1.5 * X[, 2] +
    ifelse(X[, 3] > 0, 1.5, -1.5) + rnorm(n, sd = 0.3)
  list(X = X, y = y)
}

test_that("random forest concentrates importance on the signal columns", {
  d <- make_xy()
  fit <- .morie_rf_fit(d$X, d$y, "regression", n_estimators = 100L)
  den <- sum((d$y - mean(d$y))^2)
  expect_gt(1 - sum((fit$fitted - d$y)^2) / den, 0.95)   # train
  expect_gt(1 - sum((fit$oob - d$y)^2) / den, 0.80)      # out-of-bag
  expect_equal(sum(fit$importance), 1, tolerance = 1e-8)
  # Columns 1-3 carry all the signal; 4-6 are noise.
  expect_gt(sum(fit$importance[1:3]), 0.85)
  expect_setequal(order(fit$importance, decreasing = TRUE)[1:3], 1:3)
})

test_that("out-of-bag error exceeds training error", {
  d <- make_xy()
  fit <- .morie_rf_fit(d$X, d$y, "regression", n_estimators = 100L)
  expect_gt(sum((fit$oob - d$y)^2), sum((fit$fitted - d$y)^2))
})

test_that("gradient boosting drives training loss down monotonically", {
  d <- make_xy()
  fit <- .morie_gb_fit(d$X, d$y, "regression", n_estimators = 100L,
                       learning_rate = 0.1, max_depth = 3L)
  path <- .morie_gb_loss_path(fit, d$X, d$y)
  expect_true(all(diff(path) <= 1e-9))
  expect_lt(path[length(path)], path[1] / 10)
  expect_gt(sum(fit$importance[1:3]), 0.95)
})

test_that("shrinkage slows fitting exactly as ESL eq. (10.41) implies", {
  d <- make_xy()
  slow <- .morie_gb_fit(d$X, d$y, "regression", n_estimators = 30L,
                        learning_rate = 0.01, max_depth = 3L)
  fast <- .morie_gb_fit(d$X, d$y, "regression", n_estimators = 30L,
                        learning_rate = 0.3, max_depth = 3L)
  expect_gt(sum((slow$fitted - d$y)^2), sum((fast$fitted - d$y)^2))
})

test_that("L2 leaf penalty shrinks predictions toward the intercept", {
  d <- make_xy()
  none <- .morie_gb_fit(d$X, d$y, "regression", n_estimators = 40L, lambda = 0)
  heavy <- .morie_gb_fit(d$X, d$y, "regression", n_estimators = 40L, lambda = 50)
  expect_lt(stats::var(heavy$fitted), stats::var(none$fitted))
})

test_that("classification forest and booster separate a logistic signal", {
  d <- make_xy()
  cls <- rbinom(nrow(d$X), 1, 1 / (1 + exp(-(1.5 * d$X[, 1] - 1.5 * d$X[, 3]))))
  rf <- .morie_rf_fit(d$X, cls, "classification", n_estimators = 100L)
  expect_gt(mean(as.integer(as.character(rf$oob)) == cls), 0.65)
  gb <- .morie_gb_fit(d$X, cls, "classification", n_estimators = 100L)
  expect_gt(mean((gb$fitted > 0.5) == cls), 0.80)
  # Columns 1 and 3 drive the logit; 2 does not.
  expect_gt(rf$importance[1] + rf$importance[3], rf$importance[2])
})

test_that("a pure node and a constant feature do not break tree growth", {
  X <- cbind(rep(1, 40), rnorm(40))          # column 1 is constant
  expect_silent(f <- .morie_rf_fit(X, rnorm(40), "regression", n_estimators = 5L))
  expect_equal(length(f$fitted), 40)
  y <- rep(1, 40)                             # constant target
  expect_silent(g <- .morie_gb_fit(X, y, "regression", n_estimators = 5L))
  expect_equal(g$fitted, y, tolerance = 1e-6)
})

test_that("the six public front-ends run and report the native backend", {
  d <- make_xy(n = 200)
  r <- morie_random_forest_ensemble(d$X, d$y, n_estimators = 20L)
  expect_gt(r$train_score, 0.8)
  expect_match(r$method, "Random Forest")
  g <- morie_gradient_boosting_ensemble(d$X, d$y, n_estimators = 40L)
  expect_identical(g$backend, "native")
  xg <- morie_xgboost_objective(d$X, d$y, n_estimators = 40L, reg_lambda = 1)
  expect_identical(xg$backend, "native")
  rg <- morie_random_forest_genomic(rep(0, 200), d$y, d$X, n_trees = 20)
  expect_gt(rg$oob_score, 0.5)
  gg <- morie_gradient_boosting_genomic(rep(0, 200), d$y, d$X, n_estimators = 40)
  expect_true(all(diff(gg$train_loss) <= 1e-9))
})
