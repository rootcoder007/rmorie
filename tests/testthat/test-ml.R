# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

make_ml_split <- function(n = 80, seed = 1) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- factor(ifelse(X$x1 + X$x2 + rnorm(n, 0, 0.3) > 0, "yes", "no"))
  idx <- sample.int(n, floor(n * 0.7))
  list(X = X[idx, ], y = y[idx],
       test_X = X[-idx, ], test_y = y[-idx])
}

test_that("morie_ml_eval_robustness returns classification report", {
  s <- make_ml_split()
  r <- morie_ml_eval_robustness(s$X, s$y, s$test_X, s$test_y,
                                n_estimators = 20L)
  expect_true("accuracy" %in% names(r))
  expect_true(r$accuracy >= 0 && r$accuracy <= 1)
  expect_true(all(c("yes","no") %in% names(r)))
  expect_true(all(c("precision","recall","f1-score","support") %in%
                  names(r[["yes"]])))
})

test_that("morie_ml_eval_robustness errors without randomForest", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "randomForest")) FALSE

      else TRUE

    },

    .package = "base"

  )
  s <- make_ml_split()
  expect_error(
    morie_ml_eval_robustness(s$X, s$y, s$test_X, s$test_y),
    "randomForest"
  )
})

test_that("morie_ml_apply_smote returns balanced output (random fallback)", {
  set.seed(2)
  X <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
  y <- c(rep("a", 40), rep("b", 10))
  out <- morie_ml_apply_smote(X, y, random_state = 1L, k_neighbors = 1L)
  expect_true(all(c("X","y","status") %in% names(out)))
  expect_true(out$status$method %in% c("smote","random_oversample"))
  expect_equal(out$status$minority_before, 10L)
  expect_equal(out$status$majority_before, 40L)
  expect_gte(out$status$total_after, out$status$total_before)
})

test_that("morie_ml_apply_smote per-class counts present", {
  set.seed(3)
  X <- data.frame(x1 = rnorm(30))
  y <- c(rep("a", 20), rep("b", 10))
  out <- morie_ml_apply_smote(X, y)
  expect_true("class_a_before" %in% names(out$status))
  expect_true("class_b_before" %in% names(out$status))
})

test_that("morie_ml_apply_smote handles already-balanced input", {
  set.seed(4)
  X <- data.frame(x1 = rnorm(20))
  y <- c(rep("a", 10), rep("b", 10))
  out <- morie_ml_apply_smote(X, y)
  expect_equal(out$status$total_after, out$status$total_before)
})

test_that("morie_ml_apply_smote preserves factor levels", {
  set.seed(5)
  X <- data.frame(x1 = rnorm(30))
  y <- factor(c(rep("a", 25), rep("b", 5)))
  out <- morie_ml_apply_smote(X, y)
  if (is.factor(out$y)) {
    expect_true(all(levels(out$y) %in% c("a","b")))
  }
})

test_that("morie_ml_apply_smote auto-picks k_neighbors", {
  set.seed(6)
  X <- data.frame(x1 = rnorm(40))
  y <- c(rep("a", 35), rep("b", 5))
  out <- morie_ml_apply_smote(X, y)
  expect_true(out$status$method %in% c("smote","random_oversample"))
})