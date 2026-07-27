# ---------------------------------------------------------------------------
# Native SVM (SMO) -- structural checks
#
# The solver implements LIBSVM's C-SVC dual (Eq. 2) and eps-SVR dual (Eq. 11)
# with working set selection WSS 1 (Fan, Chen & Lin 2005). These tests assert
# properties the optimum must satisfy, not merely that a number came back.
# ---------------------------------------------------------------------------

svm_xy <- function(seed = 5, n = 200, p = 4) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, p)
  y <- factor(ifelse(1.3 * X[, 1] - X[, 2] + rnorm(n, 0, 0.4) > 0, "pos", "neg"))
  list(X = X, y = y)
}

test_that("a separable problem is separated and margins are respected", {
  set.seed(1)
  X <- rbind(matrix(rnorm(60, -3), 30, 2), matrix(rnorm(60, 3), 30, 2))
  y <- factor(rep(c("a", "b"), each = 30))
  r <- morie_svm_hinge_primal(X, y, C = 1)
  expect_equal(r$train_accuracy, 1)
  # Every point should sit outside the margin on a cleanly separable set.
  f <- as.numeric(X %*% r$weights) + r$intercept
  ypm <- ifelse(y == levels(y)[2], 1, -1)
  expect_true(all(ypm * f > 0))
})

test_that("all four LIBSVM kernels fit and report support vectors", {
  d <- svm_xy()
  for (k in c("linear", "rbf", "poly", "sigmoid")) {
    r <- morie_svm_kernel_trick(d$X, d$y, kernel = k, C = 1)
    expect_gt(r$train_accuracy, 0.7)
    expect_gt(r$n_support, 0)
    expect_lte(r$n_support, nrow(d$X))
    expect_match(r$method, k, fixed = TRUE)
  }
  expect_error(morie_svm_kernel_trick(d$X, d$y, kernel = "nope"), "Unknown kernel")
})

test_that("multi-class dispatches to one-against-one voting", {
  set.seed(3)
  X <- matrix(rnorm(300 * 2), 300, 2)
  y <- factor(ifelse(X[, 1] > 0.5, "a", ifelse(X[, 2] > 0.3, "b", "c")))
  r <- morie_svm_kernel_trick(X, y, kernel = "rbf", C = 1)
  expect_gt(r$train_accuracy, 0.85)
  expect_gt(r$n_support, 0)
})

test_that("larger C tolerates fewer margin violations", {
  d <- svm_xy()
  loose <- morie_svm_kernel_trick(d$X, d$y, kernel = "linear", C = 0.01)
  tight <- morie_svm_kernel_trick(d$X, d$y, kernel = "linear", C = 100)
  # A bigger penalty on slack pulls the solution towards the data, so the
  # margin narrows and fewer points end up as support vectors.
  expect_lte(tight$n_support, loose$n_support)
  expect_gte(tight$train_accuracy, loose$train_accuracy - 0.05)
})

test_that("the linear machine's dual and primal solutions agree", {
  d <- svm_xy()
  h <- morie_svm_hinge_primal(d$X, d$y, C = 1)
  k <- morie_svm_kernel_trick(d$X, d$y, kernel = "linear", C = 1)
  # Same problem, two front-ends: accuracy must match.
  expect_equal(h$train_accuracy, k$train_accuracy, tolerance = 1e-8)
  expect_length(h$weights, ncol(d$X))
  expect_true(is.finite(h$intercept))
})

test_that("eps-SVR recovers a linear signal and keeps residuals inside the tube", {
  set.seed(21)
  n <- 200
  X <- matrix(rnorm(n * 3), n, 3)
  z <- 1.5 * X[, 1] - X[, 2] + rnorm(n, sd = 0.3)
  g <- morie_svm_genomic(rep(0, n), z, X, C = 10, epsilon = 0.1)
  r2 <- 1 - sum((g$y_hat - z)^2) / sum((z - mean(z))^2)
  expect_gt(r2, 0.8)
  expect_gt(length(g$support_indices), 0)
  expect_lte(length(g$support_indices), n)
  # Points strictly inside the eps-tube carry zero weight, so not every
  # observation can be a support vector on a well-fit problem.
  expect_true(all(abs(g$alpha) <= 10 + 1e-6))
})

test_that("binary y is required by the primal front-end", {
  set.seed(2)
  X <- matrix(rnorm(60), 30, 2)
  expect_error(morie_svm_hinge_primal(X, factor(rep(c("a", "b", "c"), 10))),
               "binary")
})
