test_that("map equation with igraph flow reproduces igraph::cluster_infomap code lengths", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  A <- as.matrix(igraph::as_adjacency_matrix(g))
  u <- .morie_random_uniform(900, seed = 44)
  B <- matrix(0, 30, 30)
  B[upper.tri(B)] <- ifelse(u[seq_len(435)] < ifelse(outer(0:29 %/% 10, 0:29 %/% 10, "==")[upper.tri(B)], 0.5, 0.04),
                            1 + floor(3 * u[435 + seq_len(435)]), 0)
  B <- B + t(B)
  for (M in list(A, B)) {
    gg <- igraph::graph_from_adjacency_matrix(M, mode = "undirected", weighted = TRUE)
    set.seed(1)
    ci <- igraph::cluster_infomap(gg)
    expect_equal(MapEquation(M, igraph::membership(ci) - 1, "igraph"), ci$codelength, tolerance = 1e-9)
    expect_lt(InfomapPartition(M, "igraph")$codelength, ci$codelength + 0.01)
  }
})
