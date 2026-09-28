test_that("clustering equals kmeans, hclust, cluster::pam/silhouette and dbscan", {
  skip_if_not_installed("cluster")
  skip_if_not_installed("dbscan")
  u <- .morie_random_uniform(120, seed = 37, stream = 0)
  X <- cbind(rep(c(0, 3, 6), each = 10) + u[1:30], rep(c(0, 4, 0), each = 10) + u[31:60])
  D <- stats::dist(X)
  M <- as.matrix(D)
  k <- stats::kmeans(X, X[c(1, 11, 21), ], algorithm = "Lloyd")
  expect_equal(KmeansLloyd(X, X[c(1, 11, 21), ])$cluster, as.vector(k$cluster))
  p <- cluster::pam(D, 3)
  expect_equal(PamMedoids(M, 3)$medoids, p$id.med)
  expect_equal(SilhouetteWidths(M, p$clustering)$width, as.vector(cluster::silhouette(p$clustering, D)[, 3]),
               tolerance = 1e-13)
  for (m in c("single", "complete", "average", "ward.D2")) {
    h <- stats::hclust(D, m)
    ours <- HierarchicalClustering(M, m)
    expect_equal(ours$height, h$height, tolerance = 1e-12, info = m)
    expect_equal(CutTree(ours$merge, 3), as.vector(stats::cutree(h, 3)), info = m)
  }
  expect_equal(DbscanClusters(X, 0.6, 4)$cluster, as.vector(dbscan::dbscan(X, 0.6, 4)$cluster))
  expect_equal(OpticsOrdering(X, 1.5, 4)$order, dbscan::optics(X, 1.5, 4)$order)
})
