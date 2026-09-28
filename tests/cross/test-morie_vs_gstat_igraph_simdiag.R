test_that("DirectionalVariogram equals gstat and Connectivity equals igraph components", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  u <- .morie_random_uniform(200, seed = 35, stream = 0)
  d <- data.frame(x = 10 * u[1:40], y = 10 * u[41:80], z = u[81:120] + 0.1 * (1:40))
  sp::coordinates(d) <- ~ x + y
  b <- c(0, 2, 4, 6)
  g <- gstat::variogram(z ~ 1, d, alpha = c(0, 45, 90, 135), tol.hor = 22.5, boundaries = b)
  r <- DirectionalVariogram(d$z, sp::coordinates(d), c(0, 45, 90, 135), b, tol = 22.5)
  for (k in seq_along(r$gamma)) {
    a <- c(0, 45, 90, 135)[k]
    gg <- g[g$dir.hor == a, ]
    ok <- r$np[[k]] > 0
    expect_equal(r$np[[k]][ok], gg$np)
    expect_equal(r$gamma[[k]][ok], gg$gamma, tolerance = 1e-12)
  }
  skip_if_not_installed("igraph")
  s <- as.numeric(u[121:185] < 0.55)[1:64]
  cn <- Connectivity(list(s), 8, 8, 1)
  on <- which(s == 1)
  gr <- matrix(s, 8, 8, byrow = TRUE)
  idx <- matrix(1:64, 8, 8, byrow = TRUE)
  E <- rbind(cbind(idx[, -8][gr[, -8] == 1 & gr[, -1] == 1], idx[, -1][gr[, -8] == 1 & gr[, -1] == 1]),
             cbind(idx[-8, ][gr[-8, ] == 1 & gr[-1, ] == 1], idx[-1, ][gr[-8, ] == 1 & gr[-1, ] == 1]))
  G <- igraph::graph_from_edgelist(E, directed = FALSE)
  G <- igraph::add_vertices(G, max(0, 64 - igraph::vcount(G)))
  comp <- igraph::components(igraph::induced_subgraph(G, on))
  expect_equal(cn$n_components, comp$no)
  expect_equal(cn$sizes[[1]], sort(as.vector(comp$csize), decreasing = TRUE))
})
