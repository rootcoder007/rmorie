# FolkRank (Hotho et al. 2006): the adapted PageRank fixed point is
# recomputed by a linear solve, w = (1 - d) (I - d M)^-1 p with M the
# column-normalised adjacency, and the baseline is the degree share.

fr_triples <- list(c("ann", "stats", "r1"), c("ann", "stats", "r2"),
                   c("bob", "stats", "r1"), c("bob", "ml", "r3"),
                   c("cat", "ml", "r3"), c("cat", "art", "r4"))
fr_matrix <- function(triples) {
  nodes <- sort(unique(unlist(lapply(triples, function(t)
    c(paste0("u:", t[1]), paste0("t:", t[2]), paste0("r:", t[3]))))))
  A <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  for (t in triples) {
    v <- c(paste0("u:", t[1]), paste0("t:", t[2]), paste0("r:", t[3]))
    for (pr in list(v[1:2], v[2:3], v[c(1, 3)])) {
      A[pr[1], pr[2]] <- A[pr[1], pr[2]] + 1
      A[pr[2], pr[1]] <- A[pr[2], pr[1]] + 1
    }
  }
  A
}
fr_pagerank <- function(A, p, d) {
  M <- sweep(A, 2, colSums(A), "/")
  as.numeric((1 - d) * solve(diag(nrow(A)) - d * M, p))
}

test_that("the tripartite graph flattens each triple to three undirected edges", {
  A <- fr_matrix(fr_triples)
  for (g in list(tripartite_graph(fr_triples), tagRC_tripartite_graph(fr_triples))) {
    expect_identical(g$nodes, rownames(A))
    expect_identical(g$n_triples, 6L)
    for (a in g$nodes) for (b in names(g$adjacency[[a]])) {
      expect_equal(g$adjacency[[a]][[b]], A[a, b])
    }
    tot <- sum(unlist(lapply(g$adjacency, unlist)))
    expect_equal(tot, sum(A))
  }
})

test_that("preference vectors put weight on the focus and keep unit mass", {
  N <- rownames(fr_matrix(fr_triples))
  for (pv in list(preference_vector(N, c("t:ml", "zz"), 0.8),
                  tagRC_preference_vector(N, c("t:ml", "zz"), 0.8))) {
    expect_equal(pv$p[["t:ml"]], 0.8, tolerance = 1e-12)
    expect_equal(pv$p[["u:ann"]], 0.2 / (length(N) - 1), tolerance = 1e-12)
    expect_equal(pv$mass, 1, tolerance = 1e-12)
    expect_equal(unlist(pv$focus), "t:ml")
  }
  expect_error(preference_vector(N, "nope"), "none of the focus")
  expect_error(tagRC_preference_vector(N, "nope"), "none of the focus")
  expect_error(preference_vector(N, "t:ml", 1), "\\(0,1\\)")
  expect_error(tagRC_preference_vector(N, "t:ml", 0), "\\(0,1\\)")
})

test_that("adapted PageRank reaches the linear-system fixed point", {
  A <- fr_matrix(fr_triples)
  N <- rownames(A)
  g <- tripartite_graph(fr_triples)
  pv <- preference_vector(N, "u:cat", 0.9)
  ref <- fr_pagerank(A, unlist(pv$p)[N], 0.7)
  for (f in list(adapted_pagerank, tagRC_adapted_pagerank)) {
    r <- f(g$adjacency, N, pv$p, d = 0.7, iters = 2000, tol = 1e-15)
    expect_equal(unname(unlist(r$w)[N]), ref, tolerance = 1e-10)
    ws <- unlist(r$w)[unlist(r$ranking)]
    expect_true(all(diff(ws) <= 1e-15))
    u <- f(g$adjacency, N, d = 0.5, iters = 2000, tol = 1e-15)
    expect_equal(unname(unlist(u$w)[N]), fr_pagerank(A, rep(1 / length(N), length(N)), 0.5),
                 tolerance = 1e-10)
  }
  expect_error(adapted_pagerank(g$adjacency, character(0)), "empty")
  expect_error(tagRC_adapted_pagerank(g$adjacency, character(0)), "empty")
  expect_error(adapted_pagerank(g$adjacency, N, d = 1), "damping")
  expect_error(tagRC_adapted_pagerank(g$adjacency, N, d = 0), "damping")
})

test_that("FolkRank is the preference run minus the degree baseline", {
  A <- fr_matrix(fr_triples)
  N <- rownames(A)
  r <- folkrank(fr_triples, "t:ml", iters = 2000)
  base <- rowSums(A) / sum(A)
  expect_equal(unname(unlist(r$without_preference)[N]), unname(base), tolerance = 1e-12)
  pv <- preference_vector(N, "t:ml", 0.9)
  pr <- fr_pagerank(A, unlist(pv$p)[N], 0.7)
  expect_equal(unname(unlist(r$difference)[N]), pr - unname(base), tolerance = 1e-9)
  expect_identical(unlist(r$ranking)[1:2], N[order(-(pr - base))][1:2])
  for (f in list(morie_tagRC, tagRC_folkrank, tagRC_tag_aware_rec)) {
    expect_equal(unlist(f(fr_triples, "t:ml", iters = 2000)$difference), unlist(r$difference))
  }
  expect_match(tagRC_cheatsheet(), "FOLKRANK", fixed = TRUE)
})
