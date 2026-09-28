test_that("FastGreedyModularity and WalktrapCommunities equal igraph", {
  skip_if_not_installed("igraph")
  same <- function(a, b) all(outer(a, a, "==") == outer(b, b, "=="))
  for (seed in c(9, 21, 33)) {
    u <- .morie_random_uniform(2000, seed = seed, stream = 0)
    n <- 30
    A <- matrix(0, n, n)
    t <- 1
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if (u[t] < (if ((i - 1) %/% 10 == (j - 1) %/% 10) 0.5 else 0.05)) A[i, j] <- A[j, i] <- round(0.5 + 2 * u[t + 1], 3)
        t <- t + 2
      }
    }
    g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
    f <- igraph::cluster_fast_greedy(g)
    ours <- FastGreedyModularity(A)
    expect_true(same(ours$membership, as.integer(igraph::membership(f))))
    expect_equal(ours$max_modularity, max(f$modularity), tolerance = 1e-12)
    w <- igraph::cluster_walktrap(g, steps = 4)
    ow <- WalktrapCommunities(A, 4)
    expect_true(same(ow$membership, as.integer(igraph::membership(w))))
    expect_equal(ow$max_modularity, max(w$modularity), tolerance = 1e-12)
  }
})
