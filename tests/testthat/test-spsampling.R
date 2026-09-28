test_that("spatial sampling designs recompute", {
  poly <- rbind(c(0, 0), c(10, 0), c(12, 6), c(4, 9), c(-1, 5))
  h <- HexagonalGridSample(poly, 1.7, offset = c(0.3, 0.6))
  d <- as.matrix(dist(h))
  diag(d) <- Inf
  expect_equal(unname(apply(d, 1, min)), rep(1.7, nrow(h)), tolerance = 1e-9)
  Y <- rbind(c(0, 0, 0, 0), c(0, 5, 7, 0), c(0, 3, 0, 1))
  cmb <- combn(0:11, 2)
  hh <- vapply(seq_len(ncol(cmb)), function(j) {
    AdaptiveClusterSample(Y, cbind(cmb[, j] %/% 4, cmb[, j] %% 4), threshold = 1)$mean_ht
  }, 0)
  expect_equal(mean(hh), mean(Y), tolerance = 1e-12)
  g <- as.matrix(expand.grid(0:2 + 0.5, 0:2 + 0.5))
  expect_equal(VoronoiDeclusteringWeights(g, c(0, 0, 3, 3))$weights, rep(1 / 9, 9), tolerance = 1e-12)
})
