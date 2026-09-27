test_that("SpatialWeights graphs and styles match spdep values", {
  U <- .morie_random_uniform(200, seed = 51, stream = 0)
  P <- cbind(U[seq(1, 79, 2)], U[seq(2, 80, 2)])
  g <- SpatialWeights(P, "gabriel", style = "B")$neighbours
  expect_identical(g[[2]], c(15L, 17L, 26L, 34L))
  expect_identical(SpatialWeights(P, "relative", style = "B")$neighbours[[3]], c(11L, 25L))
  d <- SpatialWeights(P, "distance", threshold = 0.2, d_min = 0.02, style = "B")
  expect_identical(c(d$n_components, d$n_islands), c(6L, 4L))
  expect_lt(abs(sum(SpatialWeights(P, "gabriel", style = "S")$W) - 40), 1e-12)
})
