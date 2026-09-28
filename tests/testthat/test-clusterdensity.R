test_that("density clustering recovers blobs", {
  u <- .morie_random_uniform(80, seed = 11, stream = 0)
  X <- cbind(c(u[1:20], u[21:40] + 5), c(u[41:60], u[61:80] + 5))
  truth <- rep(1:2, each = 20)
  expect_identical(Denclue(X, 0.6, 0.01)$cluster, truth)
  fl <- FlameClustering(X, knn = 5)$cluster
  expect_true(all(tapply(truth, fl, function(v) length(unique(v))) == 1))
  r <- PossibilisticFcm(X, rbind(c(0, 0), c(5, 5)))
  expect_identical(r$cluster, truth)
  expect_equal(rowSums(r$membership), rep(1, 40), tolerance = 1e-12)
  expect_identical(GrowingNeuralGas(X, n_signals = 3000, max_nodes = 10)$cluster, truth)
})
