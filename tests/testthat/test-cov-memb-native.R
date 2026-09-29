# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/memb_native.R (Shokri et al. 2017 membership
# inference). The softmax-regression trainer is recomputed by its
# gradient-ascent updates, the k-NN trainer by smoothed neighbour
# votes, the synthesisers on the package uniform stream, and the
# metrics from the confusion counts.

.mb_X <- list(c(0, 0), c(0, 1), c(1, 0), c(1, 1), c(2, 2), c(2, 1))
.mb_y <- list(0, 0, 1, 1, 1, 0)

test_that("logistic_trainer runs l2-penalised softmax gradient ascent", {
  pred <- logistic_trainer(l2 = 0.01, epochs = 7, lr = 0.3)(.mb_X, .mb_y)
  X <- do.call(rbind, .mb_X)
  Y <- cbind(unlist(.mb_y) == 0, unlist(.mb_y) == 1) * 1
  W <- matrix(0, 2, 2)
  b <- c(0, 0)
  sm <- function(Z) exp(Z - apply(Z, 1, max)) / rowSums(exp(Z - apply(Z, 1, max)))
  for (ep in 1:7) {
    E <- Y - sm(X %*% t(W) + rep(1, 6) %o% b)
    W <- W + 0.3 * (t(E) %*% X / 6 - 0.01 * W)
    b <- b + 0.3 * colSums(E) / 6
  }
  q <- list(c(0.5, 0.2), c(3, 1))
  expect_equal(unname(pred(q)), sm(do.call(rbind, q) %*% t(W) + rep(1, 2) %o% b), tolerance = 1e-12)
  expect_equal(attr(pred, "classes"), c(0, 1))
  expect_error(logistic_trainer()(list(), list()), "empty dataset")
})

test_that("knn_trainer votes over the k nearest rows with smoothing", {
  p1 <- knn_trainer(k = 1, smoothing = 0.5)(.mb_X, .mb_y)
  # (0.9, 0.1) is nearest to (1, 0), class 1
  expect_equal(p1(list(c(0.9, 0.1))), matrix(c(0.5, 1.5) / 2, 1))
  p3 <- knn_trainer(k = 3, smoothing = 0)(.mb_X, .mb_y)
  # nearest three to (2, 1.6): (2, 2) c1, (2, 1) c0, (1, 1) c1
  expect_equal(p3(list(c(2, 1.6))), matrix(c(1, 2) / 3, 1))
  expect_error(knn_trainer(k = 0), "k must be")
  expect_error(knn_trainer()(list(), list()), "empty dataset")
})

test_that("attack_dataset labels in-sample outputs 1 and held-out outputs 0", {
  pred <- knn_trainer(k = 1, smoothing = 0)(.mb_X[1:4], .mb_y[1:4])
  ad <- attack_dataset(pred, .mb_X[1:2], .mb_y[1:2], .mb_X[5:6], .mb_y[5:6])
  expect_equal(ad$labels, c(1, 1, 0, 0))
  expect_identical(ad$classes, c("0", "0", "1", "0"))
  expect_equal(ad$rows[[1]], c(1, 0))
  expect_equal(ad$rows[[3]], c(0, 1))       # (2, 2) is nearest (1, 1), class 1
  # list-returning predictors are used as they are
  ad2 <- attack_dataset(function(q) lapply(q, function(x) c(sum(x), 1)), .mb_X[3], .mb_y[3], list(), list())
  expect_equal(ad2$rows, list(c(1, 1)))
})

test_that("synthesize (Algorithm 1) returns a record the target labels c with confidence", {
  target <- function(rows) lapply(rows, function(x) {
    p <- (sum(x) + 0.5) / (length(x) + 1)
    c(1 - p, p)
  })
  # k_max = 1: flipping all four features at once only toggles a record
  # with its complement, which scores the same and never shrinks k
  x <- synthesize(target, 1, 4, k_max = 1, conf_min = 0.8, seed = 3)
  expect_length(x, 4L)
  expect_true(all(x %in% c(0, 1)))
  yx <- target(list(x))[[1]]
  expect_gt(yx[2], 0.8)
  x0 <- synthesize(target, 0, 4, k_max = 1, conf_min = 0.6, seed = 5)
  expect_gt(target(list(x0))[[1]][1], 0.6)
  # a confidence the target can never reach: no record
  expect_null(synthesize(target, 1, 2, conf_min = 0.95, iter_max = 30))
  # the matrix-returning trainers work as targets
  pr <- knn_trainer(k = 1, smoothing = 0.01)(.mb_X, .mb_y)
  xs <- synthesize(pr, 1, 2, feature_values = list(c(0, 1, 2), c(0, 1, 2)), seed = 1)
  expect_gt(pr(list(xs))[1, 2], 0.8)
  expect_error(synthesize(target, 2, 4), "outside the target")
  expect_error(synthesize(target, 1, 0), "n_features")
  expect_error(synthesize(target, 1, 4, conf_min = 1), "conf_min")
  expect_error(synthesize(target, 1, 4, k_min = 0), "k_min")
  expect_error(synthesize(target, 1, 4, k_max = 1, k_min = 2), "at least k_min")
  expect_error(synthesize(target, 1, 4, feature_values = list(0:1)), "one entry per feature")
})

test_that("synthesize_marginals draws each feature from its own column", {
  out <- synthesize_marginals(.mb_X, 5, seed = 2)
  e <- .ghc_rng(2)
  X <- do.call(rbind, .mb_X)
  for (i in 1:5) {
    u <- .ghc_unif(e, 2L)
    expect_equal(out[[i]], c(X[1 + floor(u[1] * 6), 1], X[1 + floor(u[2] * 6), 2]))
  }
  expect_error(synthesize_marginals(list(), 3), "no data")
})

test_that("synthesize_noisy flips a fraction of features to another observed value", {
  expect_equal(synthesize_noisy(.mb_X, 0), lapply(.mb_X, as.numeric))
  B <- list(c(0, 1, 0), c(1, 1, 0), c(0, 0, 1))
  flipped <- synthesize_noisy(B, 1, seed = 4)
  for (r in 1:3) expect_equal(flipped[[r]], 1 - B[[r]])
  expect_error(synthesize_noisy(B, 1.5), "fraction")
})

test_that("precision_recall counts the confusion table", {
  r <- precision_recall(c(1, 1, 0, 0, 1), c(1, 0, 0, 1, 1))
  expect_equal(c(r$tp, r$fp, r$fn, r$tn), c(2, 1, 1, 1))
  expect_equal(r$precision, 2 / 3)
  expect_equal(r$recall, 2 / 3)
  expect_equal(r$accuracy, 3 / 5)
  expect_true(is.nan(precision_recall(c(0, 0), c(0, 0))$precision))
})

test_that("memb trains per-class attack models on shadow outputs", {
  set.seed(1)
  mk <- function(n) {
    X <- lapply(seq_len(n), function(i) round(stats::runif(2) * 3))
    list(X, lapply(X, function(x) as.numeric(x[1] + stats::rnorm(1, sd = 0.8) > 1.5)))
  }
  sh <- lapply(1:2, function(i) c(mk(12), mk(12)))
  tr <- mk(12)
  te <- mk(12)
  target <- knn_trainer(k = 1)(tr[[1]], tr[[2]])
  for (fn in list(memb, membership_inference)) {
    r <- fn(target, sh, tr, te, train_fn = knn_trainer(k = 1))
    expect_equal(r$truth, rep(1:0, each = 12))
    expect_equal(r$attack_train_size, 48L)
    expect_equal(r$n_shadow, 2L)
    ok <- !is.nan(r$scores)
    expect_equal(r$predictions[ok], as.integer(r$scores[ok] >= 0.5))
    expect_equal(r$metrics, precision_recall(r$predictions, r$truth))
    expect_true(all(r$scores[ok] >= 0 & r$scores[ok] <= 1))
  }
  expect_equal(memb(target, sh, tr, te, train_fn = knn_trainer(k = 1), n_shadow = 1)$attack_train_size, 24L)
  expect_error(memb(target, list(), tr, te), "at least one shadow")
  expect_error(memb(target, list(list(list(), list(), list(), list())), tr, te), "no training data")
})

test_that("morie_memb dispatches every op", {
  expect_equal(morie_memb("precision_recall", c(1, 0), c(1, 1))$recall, 0.5)
  expect_true(is.function(morie_memb("knn_trainer", k = 2)$train_fn))
  expect_true(is.function(morie_memb("logistic_trainer")$train_fn))
  expect_match(morie_memb("cheatsheet")$cheatsheet, "SHADOW")
  expect_equal(morie_memb("synthesize_marginals", .mb_X, 2, seed = 2), synthesize_marginals(.mb_X, 2, seed = 2))
  expect_error(morie_memb("nope"), "unknown op")
  expect_error(morie_memb(), "op must be one of")
})
