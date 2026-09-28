skip_if_not_installed("spdep")
skip_if_not_installed("GWmodel")

plain <- function(m) matrix(as.numeric(m), nrow(m))

test_that("weights constructions match spdep and GWmodel", {
  set.seed(7)
  xy <- cbind(stats::runif(25) * 10, stats::runif(25) * 10)
  for (tp in c("queen", "rook")) {
    ref <- spdep::nb2mat(spdep::cell2nb(4, 5, type = tp), style = "B")
    expect_equal(plain(ref), GridContiguity(4, 5, type = tp))
  }
  ref <- spdep::nb2mat(spdep::dnearneigh(xy, 1, 3), style = "B", zero.policy = TRUE)
  expect_equal(plain(ref), DistanceBandWeights(xy, 3, d1 = 1))
  ref <- spdep::nb2mat(spdep::knn2nb(spdep::knearneigh(xy, k = 4)), style = "B")
  expect_equal(plain(ref), KnnWeights(xy, 4))
  nb <- spdep::dnearneigh(xy, 0, 2.5)
  expect_equal(spdep::n.comp.nb(nb)$nc, WeightsComponents(DistanceBandWeights(xy, 2.5))$n_components)
  sym <- spdep::nb2mat(spdep::make.sym.nb(spdep::knn2nb(spdep::knearneigh(xy, k = 2))), style = "B")
  expect_equal(plain(sym), SymmetrizeWeights(KnnWeights(xy, 2)))
  D <- as.matrix(stats::dist(xy))
  for (kn in c("gaussian", "bisquare", "tricube", "exponential", "boxcar")) {
    ref <- GWmodel::gw.weight(D, bw = 3, kernel = kn, adaptive = FALSE)
    expect_equal(plain(ref), KernelWeights(xy, 3, kernel = kn), tolerance = 1e-12)
    ref <- GWmodel::gw.weight(D, bw = 6, kernel = kn, adaptive = TRUE)
    # GWmodel stores one regression point per column: the transpose of our row layout
    expect_equal(plain(ref), t(KernelWeights(xy, 6, kernel = kn, adaptive = TRUE)), tolerance = 1e-12)
  }
})
