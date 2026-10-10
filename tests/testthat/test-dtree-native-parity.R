# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Parity of the native CART in morie_decision_tree_split() against
# rpart::rpart with the control settings the function used to pass.

rpart_ref <- function(x, y, criterion = "gini", max_depth = 30L) {
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  yf <- as.factor(y)
  if (is.null(colnames(x))) colnames(x) <- paste0("x", seq_len(ncol(x)) - 1L)
  df <- as.data.frame(x)
  df$.y <- yf
  fit <- rpart::rpart(.y ~ .,
    data = df, method = "class",
    parms = list(split = if (criterion == "entropy") "information" else "gini"),
    control = rpart::rpart.control(maxdepth = max_depth, cp = 0,
                                   minsplit = 2L, minbucket = 1L, xval = 0L)
  )
  fr <- fit$frame
  fi <- fit$variable.importance
  fi_full <- stats::setNames(rep(0, ncol(x)), colnames(x))
  if (!is.null(fi)) {
    fi_full[names(fi)] <- fi
    fi_full <- fi_full / sum(fi_full)
  }
  list(
    train_accuracy = mean(stats::predict(fit, df, type = "class") == yf),
    root_feature = match(as.character(fr$var[1]), colnames(x)) - 1L,
    root_threshold = if (is.null(fit$splits)) NA_real_ else fit$splits[1, "index"],
    n_leaves = sum(fr$var == "<leaf>"),
    feature_importances = as.numeric(fi_full)
  )
}

expect_tree_parity <- function(x, y, ...) {
  nat <- morie_decision_tree_split(x, y, ...)
  ref <- rpart_ref(x, y, ...)
  # Same splits, thresholds and leaves exactly; importances agree to
  # rounding (rpart sums them with tapply in long double).
  expect_identical(nat$root_feature, ref$root_feature)
  expect_identical(nat$root_threshold, ref$root_threshold)
  expect_identical(nat$n_leaves, as.integer(ref$n_leaves))
  expect_equal(nat$train_accuracy, ref$train_accuracy, tolerance = 1e-12)
  expect_equal(nat$feature_importances, ref$feature_importances,
               tolerance = 1e-12)
}

test_that("native CART matches rpart: gini/information, 2-4 classes, ties", {
  skip_if_not_installed("rpart")
  set.seed(11)
  for (it in 1:60) {
    n <- sample(c(8, 20, 60, 150), 1)
    p <- sample(1:5, 1)
    K <- sample(2:4, 1)
    x <- matrix(rnorm(n * p), n, p)
    if (it %% 3 == 0) x <- round(x, 1) # tied predictor values
    if (it %% 5 == 0) x[, 1] <- sample(1:3, n, TRUE)
    lin <- x[, 1] + if (p > 1) 0.5 * x[, 2] else 0
    y <- cut(lin + rnorm(n, sd = 0.8), K, labels = letters[1:K])
    crit <- if (it %% 4 == 0) "entropy" else "gini"
    md <- if (it %% 6 == 0) 2L else 30L
    expect_tree_parity(x, y, criterion = crit, max_depth = md)
  }
})

test_that("native CART matches rpart with missing values and surrogates", {
  skip_if_not_installed("rpart")
  set.seed(5)
  for (it in 1:25) {
    n <- sample(c(40, 120), 1)
    p <- 7 # more candidates than maxsurrogate = 5
    x <- matrix(rnorm(n * p), n, p)
    x[, -1] <- x[, -1] + outer(x[, 1], runif(p - 1))
    x[sample(n * p, n)] <- NA
    if (it %% 3 == 0) x[sample(n * p, 3)] <- Inf
    y <- factor(sample(c("u", "v", "w"), n, TRUE, prob = c(0.5, 0.3, 0.2)))
    y[!is.na(x[, 1]) & x[, 1] > 0.5] <- "w"
    if (it %% 4 == 0) y[1:2] <- NA
    expect_tree_parity(x, y)
  }
})

test_that("morie_decision_tree_split keeps its return shape without rpart", {
  set.seed(1)
  x <- matrix(rnorm(120), 60, 2)
  y <- factor(ifelse(x[, 1] > 0, "pos", "neg"))
  r <- morie_decision_tree_split(x, y)
  expect_named(r, c("estimate", "train_accuracy", "root_feature",
                    "root_threshold", "root_impurity", "n_leaves",
                    "feature_importances", "criterion", "n", "method"))
  expect_identical(r$root_feature, 0L)
  expect_equal(r$train_accuracy, 1)
  expect_equal(sum(r$feature_importances), 1)
  # single-class response: no split, one leaf
  r1 <- morie_decision_tree_split(x, factor(rep("a", 60)))
  expect_identical(r1$n_leaves, 1L)
  expect_true(is.na(r1$root_feature))
  expect_error(morie_decision_tree_split(x, y, max_depth = 31L), "30")
})
