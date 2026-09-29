# Coverage for cluster-aware forests (Athey, Tibshirani & Wager 2019):
# first-appearance cluster indexing, the cluster-level infinitesimal
# jackknife with the m(m-1)/(m-sc)^2 correction, and the forest's
# predictions rebuilt tree by tree (leaf means averaged within clusters
# first, or row-wise) from the same cluster-sampled trees.

test_that("clusters are indexed in order of first appearance", {
  ci <- clrgrf_cluster_index(c("b", "a", "b", "c", "a"))
  expect_identical(ci$labels, c("b", "a", "c"))
  expect_identical(ci$groups, list(c(0L, 2L), c(1L, 4L), 3L))
})

test_that("the cluster jackknife sums squared covariances with cluster membership", {
  set.seed(3)
  preds <- stats::rnorm(8)
  groups <- list(0:1, 2:3, 4, 5:6)
  bags <- lapply(1:8, function(b) stats::runif(7) < 0.4)
  inb <- t(vapply(bags, function(bg) vapply(groups, function(g) any(bg[g + 1]), TRUE), logical(4)))
  cv <- colMeans((preds - mean(preds)) * sweep(inb * 1, 2, colMeans(inb)))
  j0 <- clrgrf_cluster_jackknife(preds, bags, groups, correction = FALSE)
  expect_equal(j0$variance, sum(cv^2), tolerance = 1e-12)
  sc <- mean(rowSums(inb))
  j <- clrgrf_cluster_jackknife(preds, bags, groups)
  expect_equal(j$variance, sum(cv^2) * 3 / 4 * (4 / (4 - sc))^2, tolerance = 1e-12)
  expect_equal(j$info$clusters_per_tree, sc)
  full <- lapply(1:8, function(b) rep(TRUE, 7))
  expect_false(clrgrf_cluster_jackknife(preds, full, groups)$info$subsampled)
  expect_error(clrgrf_cluster_jackknife(1, bags, groups), "at least 2 trees")
  expect_error(clrgrf_cluster_jackknife(preds, bags, groups[1:2]), "at least 3 clusters")
})

test_that("forest predictions average cluster-level leaf means over trees", {
  set.seed(5)
  cl <- rep(1:8, each = 6)
  X <- matrix(stats::runif(96), 48)
  y <- X[, 1] + stats::rnorm(8)[cl] * 0.3 + stats::rnorm(48, sd = 0.1)
  at <- X[c(1, 20), ]
  r <- morie_clrgrf(y, X, cl, at = at, n_trees = 6, min_leaf = 2, seed = 2)
  gf <- grow_forest(X, y, n_trees = 6, min_leaf = 2, subsample_frac = 0.5, seed = 2, clusters = cl)
  P <- sapply(gf$trees, function(tr) vapply(1:2, function(q) {
    I <- leaf_of(tr, at[q, ])$node$I
    if (!length(I)) return(0)
    mean(tapply(y[I + 1], cl[I + 1], mean))
  }, 1))
  expect_equal(r$fitted, rowMeans(P), tolerance = 1e-12)
  grp <- clrgrf_cluster_index(cl)$groups
  expect_equal(r$variance[2], clrgrf_cluster_jackknife(P[2, ], gf$bags, grp)$variance, tolerance = 1e-12)
  rr <- morie_clrgrf(y, X, cl, at = at, n_trees = 6, min_leaf = 2, seed = 2, unit = "row")
  Pr <- sapply(gf$trees, function(tr) vapply(1:2, function(q) mean(y[leaf_of(tr, at[q, ])$node$I + 1]), 1))
  expect_equal(rr$fitted, rowMeans(Pr), tolerance = 1e-12)
  expect_identical(r$n_clusters, 8L)
  expect_error(morie_clrgrf(y, X, cl, unit = "tree"), "cluster or row")
  expect_error(morie_clrgrf(y, X, cl[-1]), "47 cluster labels for 48")
  expect_error(morie_clrgrf(y, X, rep(1:4, 12)), "at least 6 clusters")
  expect_error(morie_clrgrf(y[-1], X, cl), "48 covariate rows for 47")
  expect_match(clrgrf_cheatsheet(), "CLUSTERS")
})
