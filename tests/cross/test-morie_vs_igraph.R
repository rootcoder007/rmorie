# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native Leiden and Girvan-Newman vs igraph.

karate <- function() {
  E <- matrix(c(
    0L, 1L, 0L, 2L, 0L, 3L, 0L, 4L, 0L, 5L, 0L, 6L, 0L, 7L, 0L, 8L,
    0L, 10L, 0L, 11L, 0L, 12L, 0L, 13L, 0L, 17L, 0L, 19L, 0L, 21L, 0L, 31L,
    1L, 2L, 1L, 3L, 1L, 7L, 1L, 13L, 1L, 17L, 1L, 19L, 1L, 21L, 1L, 30L,
    2L, 3L, 2L, 7L, 2L, 8L, 2L, 9L, 2L, 13L, 2L, 27L, 2L, 28L, 2L, 32L,
    3L, 7L, 3L, 12L, 3L, 13L, 4L, 6L, 4L, 10L, 5L, 6L, 5L, 10L, 5L, 16L,
    6L, 16L, 8L, 30L, 8L, 32L, 8L, 33L, 9L, 33L, 13L, 33L, 14L, 32L, 14L, 33L,
    15L, 32L, 15L, 33L, 18L, 32L, 18L, 33L, 19L, 33L, 20L, 32L, 20L, 33L, 22L, 32L,
    22L, 33L, 23L, 25L, 23L, 27L, 23L, 29L, 23L, 32L, 23L, 33L, 24L, 25L, 24L, 27L,
    24L, 31L, 25L, 31L, 26L, 29L, 26L, 33L, 27L, 33L, 28L, 31L, 28L, 33L, 29L, 32L,
    29L, 33L, 30L, 32L, 30L, 33L, 31L, 32L, 31L, 33L, 32L, 33L
  ), ncol = 2, byrow = TRUE) + 1
  A <- matrix(0, 34, 34)
  A[E] <- 1
  A + t(A)
}

test_that("Leidenclus reaches igraph's best Leiden modularity on karate", {
  skip_if_not_installed("igraph")
  A <- karate()
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected")
  expect_equal(igraph::modularity(g, Leidenclus(A)$labels + 1), 0.41978961209730437, tolerance = 1e-9)
  set.seed(1)
  best <- max(replicate(20, igraph::modularity(g, igraph::membership(igraph::cluster_leiden(g, objective_function = "modularity",
                                                                                          n_iterations = -1)))))
  expect_equal(Leidenclus(A)$estimate, best, tolerance = 1e-9)
})

test_that("GirvanNewman matches igraph::cluster_edge_betweenness", {
  skip_if_not_installed("igraph")
  A <- karate()
  ce <- igraph::cluster_edge_betweenness(igraph::graph_from_adjacency_matrix(A, mode = "undirected"))
  expect_equal(max(ce$modularity), GirvanNewman(A)$estimate, tolerance = 1e-7)
})
