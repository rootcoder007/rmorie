# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Parity of the native replacements for MASS::ginv (geron_w4d_native.R)
# and pROC (morie_roc_auc_score) against the reference packages.

# -- .morie_ginv in the w4d learners ---------------------------------------

test_that(".morie_ginv equals MASS::ginv on the w4d normal-equation matrices", {
  skip_if_not_installed("MASS")
  ginv <- getFromNamespace(".morie_ginv", "rmorie")
  set.seed(42)
  X <- matrix(rnorm(40 * 3), 40, 3)
  D <- cbind(1, X)
  Dc <- cbind(D, D[, 2]) # rank-deficient design: exercises the cutoff
  y <- 1 + X %*% c(0.5, -1, 2) + rnorm(40)
  P <- diag(c(0.3, 0.3, 0.3, 0))
  mats <- list(
    crossprod(D), crossprod(Dc), crossprod(D) + P,
    crossprod(cbind(embed(as.numeric(y), 3), 1)),
    matrix(0, 3, 3), D
  )
  for (M in mats) {
    # Same SVD and the same sqrt(eps) * d[1] cutoff as MASS::ginv, so
    # the results are bitwise identical.
    expect_identical(ginv(M), MASS::ginv(M))
  }
})

test_that("w4d learners give the same results with .morie_ginv as with MASS::ginv", {
  skip_if_not_installed("MASS")
  set.seed(7)
  X <- matrix(rnorm(30 * 2), 30, 2)
  y <- as.numeric(X %*% c(1, -0.5) + rnorm(30, sd = 0.3))
  Xu <- matrix(rnorm(10 * 2), 10, 2)
  s <- cumsum(rnorm(40))
  run <- function() {
    list(
      sup = morie_geron_supervised_learning(X, y, ridge = 0.1),
      semi = morie_geron_semisupervised(X, y, Xu),
      ss = morie_geron_self_supervised(cbind(X, X[, 1] + X[, 2])),
      pre = morie_geron_unsupervised_pretraining(Xu, X, y),
      ts = morie_geron_time_series_forecast(s, horizon = 3, window = 4)
    )
  }
  strip <- function(r) rapply(r, function(f) NULL, classes = "function",
                              how = "replace")
  native <- strip(run())
  local_mocked_bindings(.morie_ginv = function(X, tol = sqrt(.Machine$double.eps))
    MASS::ginv(X, tol), .package = "rmorie")
  ref <- strip(run())
  expect_identical(native, ref)
})

# -- morie_roc_auc_score vs pROC --------------------------------------------

roc_ref <- function(y, s) {
  yt <- as.numeric(y)
  cls <- sort(unique(yt))
  yb <- as.integer(yt == cls[2])
  rc <- pROC::roc(response = yb, predictor = s, levels = c(0, 1),
                  direction = "<", quiet = TRUE)
  fpr <- 1 - rc$specificities
  tpr <- rc$sensitivities
  ord <- order(fpr, tpr)
  list(auc = as.numeric(pROC::auc(rc)), fpr = fpr[ord], tpr = tpr[ord],
       thresholds = rc$thresholds[ord])
}

test_that("morie_roc_auc_score matches pROC (AUC, thresholds, sens/spec)", {
  skip_if_not_installed("pROC")
  set.seed(2024)
  for (i in 1:60) {
    n <- sample(c(10, 50, 200), 1)
    y <- rbinom(n, 1, runif(1, 0.2, 0.8))
    if (length(unique(y)) < 2) y[1:2] <- c(0, 1)
    s <- switch(i %% 3 + 1,
      y + rnorm(n), # continuous, no ties
      round(y * 0.7 + rnorm(n), 1), # many ties
      sample(1:4, n, TRUE) # heavy ties, uninformative
    )
    if (i %% 5 == 0) y <- ifelse(y == 1, 7, 3) # non 0/1 coding
    nat <- morie_roc_auc_score(y, s)
    ref <- roc_ref(y, s)
    # Same thresholds and the same counts, so the curve is identical; the
    # AUC is the same trapezoid sum in the same order (tolerance 1e-12
    # only guards against platform rounding).
    expect_identical(nat$thresholds, ref$thresholds)
    expect_equal(nat$fpr, ref$fpr, tolerance = 1e-12)
    expect_equal(nat$tpr, ref$tpr, tolerance = 1e-12)
    expect_equal(nat$auc, ref$auc, tolerance = 1e-12)
  }
})

test_that("morie_roc_auc_score reproduces pROC's near-tie threshold fix", {
  skip_if_not_installed("pROC")
  y <- c(0, 1, 0, 1)
  s <- c(1, 1 + 2 * .Machine$double.eps, 2, 3)
  nat <- morie_roc_auc_score(y, s)
  ref <- roc_ref(y, s)
  expect_identical(nat$thresholds, ref$thresholds)
  expect_equal(nat$auc, ref$auc, tolerance = 1e-12)
})

test_that("morie_roc_auc_score keeps its return shape and Mann-Whitney AUC", {
  set.seed(1)
  y <- rbinom(80, 1, 0.4)
  s <- y + rnorm(80)
  r <- morie_roc_auc_score(y, s)
  expect_named(r, c("estimate", "auc", "fpr", "tpr", "thresholds", "n",
                    "n_positive", "n_negative", "method"))
  rk <- rank(s)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  mw <- (sum(rk[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  expect_equal(r$auc, mw, tolerance = 1e-12)
  expect_equal(r$fpr[1], 0)
  expect_equal(r$tpr[length(r$tpr)], 1)
  expect_error(morie_roc_auc_score(rep(1, 5), 1:5), "binary")
})
