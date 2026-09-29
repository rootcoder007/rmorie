# Coverage for honest forests (Wager & Athey 2018): tree traversal, the
# honesty property (leaf values are means of the I-sample responses, the
# I and J halves partition the subsample), the infinitesimal-jackknife
# variance with the n(n-1)/(n-s)^2 correction (eq. 8), the forest
# rebuilt tree by tree from the same subsamples and seeds, and the
# adaptive-nearest-neighbour forest weights.

.dat <- function() {
  set.seed(6)
  X <- matrix(stats::runif(120), 40)
  y <- 2 * (X[, 1] > 0.5) + X[, 2] + stats::rnorm(40, sd = 0.2)
  list(X = X, y = y)
}
.leaves <- function(nd) if (isTRUE(nd$leaf)) list(nd) else c(.leaves(nd$left), .leaves(nd$right))

test_that("leaf_of follows the thresholds", {
  tr <- list(leaf = FALSE, feature = 2, threshold = 0.5,
             left = list(leaf = TRUE, value = 1),
             right = list(leaf = FALSE, feature = 1, threshold = 0,
                          left = list(leaf = TRUE, value = 2), right = list(leaf = TRUE, value = 3)))
  expect_identical(leaf_of(tr, c(9, 0.2))$node$value, 1)
  expect_identical(leaf_of(tr, c(-1, 0.7))$node$value, 2)
  r <- leaf_of(tr, c(1, 0.7))
  expect_identical(r$node$value, 3)
  expect_length(r$path, 2L)
})

test_that("honest trees estimate leaves from I only and partition I and J", {
  d <- .dat()
  t <- honest_tree(d$X, d$y, min_leaf = 3, seed = 2)
  L <- .leaves(t$tree)
  I <- sort(unlist(lapply(L, `[[`, "I")))
  J <- sort(unlist(lapply(L, `[[`, "J")))
  expect_identical(I, sort(t$info$I))
  expect_identical(J, sort(t$info$J))
  expect_length(intersect(t$info$I, t$info$J), 0L)
  expect_identical(sort(c(I, J)), 0:39)
  for (l in L) expect_equal(l$value, mean(d$y[l$I + 1]), tolerance = 1e-12)
  expect_true(all(vapply(L, function(l) length(l$I), 1L) >= 3))
  p <- honest_tree(d$X, d$y, W = as.numeric(d$X[, 1] > 0.5), kind = "propensity", min_leaf = 3, seed = 1)
  expect_identical(p$info$I, p$info$J)
  expect_error(honest_tree(d$X, d$y, kind = "greedy"), "kind must be one of")
  expect_error(honest_tree(d$X, d$y, alpha = 0.5), "alpha must be in")
  expect_error(honest_tree(d$X, d$y, pi = 0), "pi must be in")
  expect_error(honest_tree(d$X, d$y, kind = "propensity"), "needs W")
  expect_error(honest_tree(d$X, d$y, min_leaf = 20), "too small")
})

test_that("the IJ variance is (n-1)/n (n/(n-s))^2 sum_i Cov(N_i, T)^2", {
  set.seed(1)
  preds <- stats::rnorm(6)
  bag <- matrix(stats::runif(60) < 0.5, 6)
  cv <- vapply(1:10, function(i) mean((preds - mean(preds)) * (bag[, i] - mean(bag[, i]))), 1)
  expect_equal(infinitesimal_jackknife(preds, bag, 10, 5, correction = FALSE), sum(cv^2), tolerance = 1e-12)
  expect_equal(infinitesimal_jackknife(preds, bag, 10, 5), sum(cv^2) * 0.9 * 4, tolerance = 1e-12)
  expect_error(infinitesimal_jackknife(1, bag, 10, 5), "at least 2 trees")
  expect_error(infinitesimal_jackknife(preds, bag, 5, 5), "need n > s")
})

test_that("the forest averages trees grown on the documented subsamples", {
  d <- .dat()
  at <- d$X[1:3, ]
  f <- honest_forest(d$X, d$y, n_trees = 5, min_leaf = 3, seed = 4, at = at)
  e <- .ghc_rng(4)
  s <- 20L
  P <- matrix(0, 5, 3)
  bag <- matrix(FALSE, 5, 40)
  for (b in 1:5) {
    sub <- order(.ghc_unif(e, 40))[1:s] - 1L
    bag[b, sub + 1] <- TRUE
    tr <- honest_tree(d$X, d$y, min_leaf = 3, seed = 4 * 7919 + b - 1, subsample = sub)$tree
    P[b, ] <- vapply(1:3, function(q) leaf_of(tr, at[q, ])$node$value, 1)
  }
  expect_equal(f$fitted, colMeans(P), tolerance = 1e-12)
  v <- vapply(1:3, function(q) infinitesimal_jackknife(P[, q], bag, 40, s), 1)
  expect_equal(f$variance, v, tolerance = 1e-12)
  expect_equal(f$ci[[2]], f$fitted[2] + c(-1, 1) * stats::qnorm(0.975) * sqrt(v[2]), tolerance = 1e-12)
  expect_equal(sum(f$split_share), 1, tolerance = 1e-12)
  expect_identical(morie_hntfst(d$X, d$y, n_trees = 5, min_leaf = 3, seed = 4, at = at)$fitted, f$fitted)
  expect_identical(honestforest, honest_forest)
  expect_identical(honest_random_forest, honest_forest)
  expect_error(honest_forest(d$X[1:10, ], d$y[1:10]), "at least 16")
  expect_error(honest_forest(d$X, d$y[-1]), "39 responses")
  expect_error(honest_forest(d$X, d$y, subsample_frac = 1), "subsample_frac")
  expect_error(honest_forest(d$X, d$y, n_trees = 1), "at least 2 trees")
})

test_that("forest weights average 1 / |leaf| over trees and sum to one", {
  d <- .dat()
  trs <- lapply(1:3, function(b) honest_tree(d$X, d$y, min_leaf = 3, seed = b)$tree)
  x <- d$X[5, ]
  w <- forest_weights(trs, d$X, x)
  ref <- numeric(40)
  for (tr in trs) {
    I <- leaf_of(tr, x)$node$I
    ref[I + 1] <- ref[I + 1] + 1 / length(I)
  }
  expect_equal(w, ref / 3, tolerance = 1e-12)
  expect_equal(sum(w), 1, tolerance = 1e-12)
  pr <- mean(vapply(trs, function(tr) leaf_of(tr, x)$node$value, 1))
  expect_equal(sum(w * d$y), pr, tolerance = 1e-12)
  expect_error(forest_weights(list(), d$X, x), "no trees")
})
