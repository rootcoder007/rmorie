test_that("network measures equal igraph", {
  skip_if_not_installed("igraph")
  u <- .morie_random_uniform(400, seed = 89, stream = 0)
  n <- 12
  E <- which(upper.tri(matrix(0, n, n)) & matrix(u[1:144] < 0.3, n), arr.ind = TRUE)
  E <- unique(rbind(E, cbind(1:(n - 1), 2:n)))
  E <- E[order(E[, 1], E[, 2]), ]
  g <- igraph::graph_from_edgelist(E, directed = FALSE)
  c <- Centralities(n, E - 1)
  expect_equal(c$betweenness, igraph::betweenness(g), tolerance = 1e-12)
  expect_equal(c$pagerank, igraph::page_rank(g)$vector, tolerance = 1e-12)
  expect_equal(c$eigenvector, igraph::eigen_centrality(g)$vector, tolerance = 1e-10)
  s <- NetworkSummary(n, E - 1)
  expect_equal(s$transitivity, igraph::transitivity(g), tolerance = 1e-14)
  expect_equal(s$efficiency, igraph::global_efficiency(g), tolerance = 1e-14)
  expect_equal(s$assortativity, igraph::assortativity_degree(g), tolerance = 1e-12)
})
