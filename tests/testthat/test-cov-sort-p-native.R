# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/sortP_native.R (SortPooling, Zhang et al. 2018): the
# WL-style colour refinement, the sort-and-truncate pooling, the
# coverage quantile for k and the permutation check.

.sp_adj <- list("0" = c(1L, 2L), "1" = c(0L, 2L), "2" = c(0L, 1L, 3L), "3" = 2L)

test_that("wl_colours refines by neighbour sums and renormalises each round", {
  c1 <- wl_colours(.sp_adj, 4, rounds = 1)
  # one round: c_v + sum over neighbours, divided by the mean
  raw <- c(1 + 2, 1 + 2, 1 + 3, 1 + 1)
  expect_equal(c1, raw / mean(raw), tolerance = 1e-12)
  c2 <- wl_colours(.sp_adj, 4, rounds = 2)
  raw2 <- c(c1[1] + c1[2] + c1[3], c1[2] + c1[1] + c1[3],
            c1[3] + c1[1] + c1[2] + c1[4], c1[4] + c1[3])
  expect_equal(c2, raw2 / mean(raw2), tolerance = 1e-12)
  # symmetric vertices keep equal colours; the degree-3 vertex separates
  expect_equal(c2[1], c2[2], tolerance = 1e-12)
  expect_gt(c2[3], c2[1])
  expect_lt(c2[4], c2[1])
  expect_equal(wl_colours(.sp_adj, 4, rounds = 0), rep(1, 4))
  expect_equal(wl_colours(list(), 3, rounds = 1), rep(1, 3))
  init <- c(2, 1, 1, 3)
  expect_equal(wl_colours(list("0" = 1L, "1" = 0L), 2, rounds = 1, initial = c(1, 3)),
               c(4, 4) / 4, tolerance = 1e-12)
  expect_error(wl_colours(.sp_adj, 4, initial = init[-1]), "3 initial colours for 4 vertices")
})

test_that("sort_pooling sorts by a channel, truncates and zero-pads", {
  X <- rbind(c(1, 5), c(3, 2), c(2, 9), c(0, 1))
  r <- sort_pooling(X, 3)
  # the default channel is the last one
  expect_equal(r$sort_channel, 2L)
  expect_identical(r$order, c(3L, 1L, 2L))
  expect_equal(r$pooled, X[c(3, 1, 2), ])
  expect_equal(r$n_truncated, 1L)
  expect_equal(r$n_padded, 0L)
  f <- sort_pooling(X, 3, sort_channel = 0)
  expect_equal(f$sort_channel, 1L)
  expect_identical(f$order, c(2L, 3L, 1L))
  p <- sort_pooling(X, 6)
  expect_equal(dim(p$pooled), c(6L, 2L))
  expect_equal(p$pooled[5:6, ], matrix(0, 2, 2))
  expect_equal(p$n_padded, 2L)
  # ties keep the original order
  T2 <- rbind(c(1, 5), c(2, 5))
  expect_identical(sort_pooling(T2, 2)$order, c(1L, 2L))
  expect_equal(sort_pooling(list(c(1, 2), c(3, 4)), 1)$pooled, matrix(c(3, 4), 1))
  expect_equal(sortpool(X, 3)$pooled, r$pooled)
  expect_equal(morie_sortP(X, 3)$pooled, r$pooled)
  expect_error(sort_pooling(X, 0), "k must be at least 1")
})

test_that("choose_k is the coverage quantile of the size distribution", {
  sizes <- c(4, 6, 10, 12, 20)
  k <- choose_k(sizes, 0.6)
  expect_equal(k$k, 10L)
  expect_equal(k$fraction_untruncated, 0.6)
  expect_equal(choose_k(sizes, 1)$k, 20L)
  expect_equal(choose_k(sizes, 0.01)$k, 4L)
  expect_equal(choose_k(sizes, 1)$fraction_untruncated, 1)
  expect_error(choose_k(sizes, 0), "coverage must lie")
  expect_error(choose_k(integer(0)), "no graph sizes")
})

test_that("order_is_graph_determined passes a graph-derived key and fails an index one", {
  X <- cbind(c(0.4, 0.4, 0.9, 0.1))
  perm <- c(3L, 1L, 4L, 2L)
  good <- order_is_graph_determined(X, .sp_adj, perm, 3)
  expect_equal(good$max_deviation, 0)
  expect_true(good$invariant)
  # a key that does not separate two vertices lets the listing decide
  # which of them survives truncation
  tie <- rbind(c(1, 5), c(2, 5), c(3, 1))
  bad <- order_is_graph_determined(tie, .sp_adj, c(2L, 1L, 3L), 1)
  expect_equal(bad$max_deviation, 1)
  expect_false(bad$invariant)
  # keeping both rows does not help: their ORDER is still the listing's
  expect_equal(order_is_graph_determined(tie, .sp_adj, c(2L, 1L, 3L), 3)$max_deviation, 1)
  # separating them on the sort channel makes the output well defined
  sep <- rbind(c(1, 5), c(2, 6), c(3, 1))
  expect_true(order_is_graph_determined(sep, .sp_adj, c(2L, 1L, 3L), 1)$invariant)
  # the WL colours are graph determined, so pooling on them is invariant
  col <- wl_colours(.sp_adj, 4, rounds = 2)
  expect_true(order_is_graph_determined(cbind(col), .sp_adj, c(1L, 2L, 3L, 4L), 4)$invariant)
})
