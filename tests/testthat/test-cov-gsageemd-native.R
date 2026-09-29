# Coverage tests for R/gsageemd_native.R (Hamilton, Ying and Leskovec
# 2017, GraphSAGE): aggregators, neighbour sampling, the concat-ReLU-
# normalise layer, multi-layer embedding and the unsupervised loss.

gs_dd <- list(`0` = list(`1` = 1, `2` = 1), `1` = list(`0` = 1), `2` = list(`0` = 1, `3` = 1), `3` = list(`2` = 1))
gs_lv <- list(`0` = c(1L, 2L), `1` = 0L, `2` = c(0L, 3L), `3` = 2L)
gs_H <- rbind(c(0.5, -0.2), c(0.1, 0.9), c(-0.7, 0.3), c(0.4, 0.4))
gs_W <- matrix(c(0.3, -0.5, 0.8, 0.2, 0.6, 0.1, -0.4, 0.7, 0.5, 0.2, 0.1, -0.3), 3, 4)

gs_ref_layer <- function(agg_fun, normalize = TRUE) {
  nbrs <- list(c(2, 3), 1, c(1, 4), 3)
  t(vapply(1:4, function(v) {
    z <- pmax(as.numeric(gs_W %*% c(gs_H[v, ], agg_fun(gs_H[nbrs[[v]], , drop = FALSE]))), 0)
    if (normalize) z / sqrt(sum(z^2)) else z
  }, numeric(3)))
}

test_that("mean, max-pool and first-neighbour aggregators", {
  V <- rbind(c(1, -2), c(3, 0.5), c(-1, 4))
  expect_equal(morie_gsageemd_aggregate(V), colMeans(V))
  expect_equal(morie_gsageemd_aggregate(V, "max_pool"), c(3, 4))
  Wp <- rbind(c(1, 0), c(0.5, -1), c(-1, 1))
  expect_equal(morie_gsageemd_aggregate(V, "max_pool", W = Wp), apply(pmax(V %*% t(Wp), 0), 2, max))
  expect_equal(morie_gsageemd_aggregate(list(c(1, 2), c(3, 4)), "lstm_order"), c(1, 2))
  expect_error(morie_gsageemd_aggregate(V, "sum"), "aggregator must be")
  expect_error(morie_gsageemd_aggregate(matrix(0, 0, 2)), "no neighbours")
})

test_that("neighbour sampling uses the documented uniform draws", {
  u <- .ghc_unif(.ghc_rng(5), 6)
  nb <- c(1L, 2L)
  expect_equal(sample_neighbors(gs_dd, 0, 6, .ghc_rng(5)), nb[pmin(floor(u * 2), 1) + 1])
  expect_equal(morie_gsageemd_sample(gs_lv, 0, 6, .ghc_rng(5)), c(1L, 2L)[floor(u * 2) %% 2 + 1])
  expect_error(sample_neighbors(gs_dd, 1, 0), "at least 1")
  expect_error(morie_gsageemd_sample(list(`0` = integer(0)), 0, 2, .ghc_rng(1)), "no neighbours")
})

test_that("SAGE layer: concat self and aggregate, ReLU, L2-normalise", {
  expect_equal(sage_layer(gs_H, gs_dd, gs_W), gs_ref_layer(colMeans), tolerance = 1e-12)
  expect_equal(morie_gsageemd_layer(gs_H, gs_lv, gs_W), gs_ref_layer(colMeans), tolerance = 1e-12)
  expect_equal(sage_layer(gs_H, gs_dd, gs_W, normalize = FALSE), gs_ref_layer(colMeans, FALSE), tolerance = 1e-12)
  mp <- function(M) apply(M, 2, max)
  expect_equal(sage_layer(gs_H, gs_dd, gs_W, how = "max_pool"), gs_ref_layer(mp), tolerance = 1e-12)
  expect_equal(morie_gsageemd_layer(gs_H, gs_lv, gs_W, how = "max_pool"), gs_ref_layer(mp), tolerance = 1e-12)
  expect_error(sage_layer(gs_H, gs_dd, gs_W[, 1:3]), "W expects 3")
  expect_error(morie_gsageemd_layer(gs_H, gs_lv, gs_W[, 1:3]), "W expects 3")
})

test_that("multi-layer embedding composes the layers", {
  W2 <- matrix(sin(1:18) / 2, 3, 6)
  e <- morie_gsageemd_embed(gs_H, gs_dd, list(gs_W, W2))
  h1 <- sage_layer(gs_H, gs_dd, gs_W)
  expect_equal(e$embeddings, sage_layer(h1, gs_dd, W2), tolerance = 1e-12)
  expect_equal(e$depth, 2L)
  expect_null(e$per_batch_bound)
  es <- morie_gsageemd_embed(gs_H, gs_dd, list(gs_W), sizes = 3, seed = 4)
  expect_equal(es$per_batch_bound, 3L)
  expect_equal(es$embeddings, sage_layer(gs_H, gs_dd, gs_W, sizes = 3, rng = .ghc_rng(4)), tolerance = 1e-12)
  expect_identical(graphsage, morie_gsageemd_embed)
})

test_that("unsupervised negative-sampling loss", {
  zu <- c(0.6, 0.8)
  zv <- c(0.5, 0.5)
  neg <- list(c(-1, 0.2), c(0.3, -0.9))
  ref <- -(log(plogis(sum(zu * zv))) + sum(vapply(neg, function(z) log(plogis(-sum(zu * z))), 0)))
  expect_equal(unsupervised_loss(zu, zv, neg), ref, tolerance = 1e-12)
  expect_equal(morie_gsageemd_loss(zu, zv, neg), ref, tolerance = 1e-12)
  expect_equal(unsupervised_loss(zu, zv, list()), -log(plogis(sum(zu * zv))), tolerance = 1e-12)
})
