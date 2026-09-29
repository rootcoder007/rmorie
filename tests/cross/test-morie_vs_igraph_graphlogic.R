# Cross tests: graph indices against igraph.

test_that("degree, diameter and betweenness equal igraph", {
  skip_if_not_installed("igraph")
  n <- 30
  A <- outer(0:(n - 1), 0:(n - 1), function(i, j) as.numeric(i != j & ((i * 11 + j * 7) %% 13 < 2 | abs(i - j) == 1)))
  A <- pmax(A, t(A))
  W <- ifelse(A != 0, 1 + outer(0:(n - 1), 0:(n - 1)) %% 5, 0)
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected")
  gw <- igraph::graph_from_adjacency_matrix(W, mode = "undirected", weighted = TRUE)
  expect_equal(GraphDegreeCentrality(A), as.numeric(igraph::degree(g, normalized = TRUE)), tolerance = 1e-14)
  expect_equal(GraphDiameter(A)$diameter, igraph::diameter(g))
  expect_equal(GraphDiameter(W, weighted = TRUE)$diameter, igraph::diameter(gw))
  expect_equal(GraphBetweenness(A), as.numeric(igraph::betweenness(g)), tolerance = 1e-12)
  expect_equal(GraphBetweenness(W, weighted = TRUE, normalized = TRUE),
               as.numeric(igraph::betweenness(gw, normalized = TRUE)), tolerance = 1e-12)
  D <- outer(0:(n - 1), 0:(n - 1), function(i, j) as.numeric(i != j & (i * 5 + j * 2) %% 7 < 2))
  gd <- igraph::graph_from_adjacency_matrix(D, mode = "directed")
  expect_equal(GraphBetweenness(D), as.numeric(igraph::betweenness(gd)), tolerance = 1e-12)
  expect_equal(GraphDegreeCentrality(D, mode = "out"), as.numeric(igraph::degree(gd, mode = "out", normalized = TRUE)))
})
