skip_if_not_installed("spdep")

test_that("MEM Moran's I equals spdep::moran of each eigenvector", {
  set.seed(3)
  xy <- cbind(stats::runif(15), stats::runif(15))
  nb <- spdep::knn2nb(spdep::knearneigh(xy, k = 3))
  lw <- spdep::nb2listw(spdep::make.sym.nb(nb), style = "B")
  W <- spdep::listw2mat(lw)
  me <- MoranEigenvectors(W)
  for (k in seq_along(me$eigenvalues)) {
    ref <- spdep::moran(me$vectors[, k], lw, n = 15, S0 = spdep::Szero(lw))$I
    expect_equal(me$moran_i[k], ref, tolerance = 1e-12)
  }
})
