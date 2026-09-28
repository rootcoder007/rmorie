test_that("MantelTest, Kde2d and EofAnalysis equal vegan, MASS and prcomp", {
  skip_if_not_installed("vegan")
  skip_if_not_installed("MASS")
  u <- .morie_random_uniform(300, seed = 103, stream = 0)
  P <- cbind(u[1:20], u[21:40])
  A <- as.matrix(stats::dist(P))
  B <- A + abs(outer(u[41:60], u[41:60], `-`))
  expect_equal(MantelTest(A, B, nsim = 0)$statistic, vegan::mantel(stats::as.dist(A), stats::as.dist(B), permutations = 0)$statistic,
               tolerance = 1e-13)
  k <- MASS::kde2d(u[61:100], u[101:140], n = 5)
  expect_equal(Kde2d(u[61:100], u[101:140], n = 5)$z, k$z, tolerance = 1e-13)
  Z <- matrix(u[141:200], 12, 5)
  expect_equal(EofAnalysis(Z)$explained, (stats::prcomp(Z)$sdev^2 / sum(stats::prcomp(Z)$sdev^2)), tolerance = 1e-12)
})
