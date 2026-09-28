test_that("SitePercolation clusters equal igraph components", {
  skip_if_not_installed("igraph")
  r <- SitePercolation(10, 12, 0.55, seed = 9)
  L <- r$labels[[1]]
  occ <- as.vector(t(L)) >= 0
  idx <- matrix(seq_len(120), 10, 12, byrow = TRUE)
  O <- matrix(occ, 10, 12, byrow = TRUE)
  E <- rbind(cbind(idx[, -12][O[, -12] & O[, -1]], idx[, -1][O[, -12] & O[, -1]]),
             cbind(idx[-10, ][O[-10, ] & O[-1, ]], idx[-1, ][O[-10, ] & O[-1, ]]))
  g <- igraph::add_vertices(igraph::graph_from_edgelist(E, directed = FALSE), 0)
  g <- igraph::add_vertices(g, 120 - igraph::vcount(g))
  comp <- igraph::components(igraph::induced_subgraph(g, which(occ)))
  expect_equal(r$n_clusters, comp$no)
  expect_equal(r$largest_fraction, max(comp$csize) / 120)
})
