# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/node2v_native.R (Grover & Leskovec 2016). The search
# bias alpha_pq(t, x) is 1/p, 1 or 1/q by d_tx (Sec. 3.2.2); transition
# probabilities are w_vx alpha_pq / Z; walks are recomputed by
# inverse-CDF sampling on the package's uniform stream.

# named-list adjacency (transition_probabilities / generate_walks)
.nv_adj <- list(a = list(b = 1, c = 1), b = list(a = 1, c = 1, d = 1),
                c = list(a = 1, b = 1), d = list(b = 1))
# character-vector adjacency (node2v_*)
.nv_adjv <- list(a = c("b", "c"), b = c("a", "c", "d"), c = c("a", "b"), d = "b")

.nv_walk <- function(adj, start, len, p, q, rng, tp_fn) {
  path <- start
  prev <- NULL
  for (k in seq_len(len - 1L)) {
    tp <- tp_fn(adj, prev, path[length(path)], p, q)
    u <- .ghc_unif(rng, 1L)
    nxt <- tp$nodes[min(which(u <= cumsum(tp$probabilities)), length(tp$nodes))]
    prev <- path[length(path)]
    path <- c(path, nxt)
  }
  path
}

test_that("alpha_pq and node2v_check_pq follow the d_tx rule", {
  for (fn in list(alpha_pq, node2v_alpha_pq)) {
    expect_equal(fn(0, 4, 0.5), 1 / 4)
    expect_equal(fn(1, 4, 0.5), 1)
    expect_equal(fn(2, 4, 0.5), 1 / 0.5)
    expect_error(fn(3, 1, 1), "must be 0, 1 or 2")
    expect_error(fn(0, -1, 1), "positive")
  }
  expect_identical(node2v_check_pq("2", 3), list(p = 2, q = 3))
  expect_error(node2v_check_pq(1, 0), "positive")
  expect_error(node2v_check_pq(Inf, 1), "positive")
})

test_that("node2v_dist is 0 for t itself, 1 for a neighbour of t, 2 otherwise", {
  expect_identical(node2v_dist(.nv_adjv, "a", "a"), 0L)
  expect_identical(node2v_dist(.nv_adjv, "a", "c"), 1L)
  expect_identical(node2v_dist(.nv_adjv, "a", "d"), 2L)
  expect_identical(node2v_dist(list(`1` = 2L), 1L, 2L), 1L)
})

test_that("transition probabilities are w * alpha_pq / Z over sorted neighbours", {
  # from t = a at v = b: x = a returns (1/p), c neighbours a (1), d is 2 away (1/q)
  un <- c(1 / 2, 1, 1 / 0.5)
  r <- transition_probabilities(.nv_adj, "a", "b", 2, 0.5)
  expect_identical(r$nodes, c("a", "c", "d"))
  expect_equal(r$unnormalized, un)
  expect_equal(r$probabilities, un / sum(un), tolerance = 1e-12)
  expect_equal(r$Z, sum(un))
  rw <- transition_probabilities(.nv_adj, "a", "b", 2, 0.5, weights = list("b\rc" = 3))
  expect_equal(rw$unnormalized, un * c(1, 3, 1))
  r0 <- transition_probabilities(.nv_adj, NULL, "b", 2, 0.5)
  expect_equal(r0$probabilities, rep(1 / 3, 3), tolerance = 1e-12)
  expect_error(transition_probabilities(list(a = list()), NULL, "a", 1, 1), "no neighbours")
  v <- node2v_transition_probabilities(.nv_adjv, "a", "b", 2, 0.5)
  expect_identical(v$nodes, c("a", "c", "d"))
  expect_equal(v$probabilities, un / sum(un), tolerance = 1e-12)
  for (key in c("b\rc", "b|c", "b,c")) {
    vw <- node2v_transition_probabilities(.nv_adjv, "a", "b", 2, 0.5, weights = setNames(list(3), key))
    expect_equal(vw$unnormalized, un * c(1, 3, 1))
  }
  expect_equal(node2v_transition_probabilities(.nv_adjv, NULL, "d", 1, 1)$probabilities, 1)
  expect_error(node2v_transition_probabilities(list(a = character(0)), NULL, "a", 1, 1), "no neighbours")
})

test_that("node2v_walk and generate_walks sample by inverse CDF on one shared stream", {
  rng <- .ghc_rng(0)
  expected <- .nv_walk(.nv_adjv, "a", 6L, 0.5, 2, rng, node2v_transition_probabilities)
  expect_identical(node2v_walk(.nv_adjv, "a", 6L, 0.5, 2), expected)
  expect_identical(node2v_walk(.nv_adjv, "a", 6L, 0.5, 2, rng = .ghc_rng(0)), expected)
  g <- generate_walks(.nv_adj, num_walks = 2, length = 5, p = 0.5, q = 2, seed = 7)
  rng <- .ghc_rng(7)
  exp_walks <- list()
  for (j in 1:2) for (v in c("a", "b", "c", "d")) {
    exp_walks[[length(exp_walks) + 1L]] <- .nv_walk(.nv_adj, v, 5L, 0.5, 2, rng, transition_probabilities)
  }
  expect_identical(g$walks, exp_walks)
  expect_identical(g$estimate, g$walks)
  expect_equal(g$n_walks, 8L)
  expect_equal(g$length, 5L)
  for (w in g$walks) for (k in 2:5) expect_true(w[k] %in% .nv_adjv[[w[k - 1]]])
})

test_that("skipgram_pairs lists every (centre, context) pair inside the window", {
  walks <- list(c("a", "b", "c", "d"))
  exp_pairs <- list()
  for (i in 1:4) for (j in max(1, i - 1):min(4, i + 1)) if (j != i) exp_pairs[[length(exp_pairs) + 1]] <- walks[[1]][c(i, j)]
  expect_identical(node2v_skipgram_pairs(walks, 1), exp_pairs)
  expect_identical(skipgram_pairs(walks, 1), do.call(rbind, exp_pairs))
  expect_equal(nrow(skipgram_pairs(walks, 2)), 2 * (3 + 2))
  expect_length(node2v_skipgram_pairs(walks, 3), 12L)
  expect_identical(skipgram_pairs(list(), 2), list())
  expect_error(skipgram_pairs(walks, 0), "at least 1")
  expect_error(node2v_skipgram_pairs(walks, 0), "at least 1")
})

test_that("node2v_cheatsheet explains the BFS / DFS interpolation", {
  s <- node2v_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "SECOND-ORDER")
  expect_match(s, "1/q")
})
