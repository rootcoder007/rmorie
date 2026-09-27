# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: Massey-Denton segregation indices vs OasisR (4-decimal output).

sg_data <- function() {
  U <- .morie_random_uniform(1000, seed = 13, stream = 0)
  nr <- 5
  nc <- 6
  i <- 0:(nr * nc - 1)
  xy <- cbind(i %% nc + 0.3 * U[2 * i + 1], i %/% nc + 0.3 * U[2 * i + 2])
  x <- cbind(round(40 * U[101 + 3 * i] * (1 + (i %% nc) / 3)), round(30 * U[102 + 3 * i] * (1 + (i %/% nc) / 2)),
             round(20 * U[103 + 3 * i]) + 1)
  cm <- outer(i, i, function(a, b) as.numeric(abs(a %% nc - b %% nc) + abs(a %/% nc - b %/% nc) == 1))
  d <- as.matrix(stats::dist(xy))
  dimnames(d) <- NULL
  list(x = x, area = 0.5 + U[301 + i], c = cm, d = d, dc = d[, 1])
}

test_that("segregation indices match OasisR", {
  skip_if_not_installed("OasisR")
  J <- sg_data()
  x <- J$x
  c1 <- J$c
  diag(c1) <- 1
  tol <- 5e-5
  expect_equal(DissimilarityIndex(x, J$c)$D, OasisR::DIDuncan(x), tolerance = tol)
  expect_equal(DissimilarityIndex(x, J$c)$IS, OasisR::ISDuncan(x), tolerance = tol)
  expect_equal(DissimilarityIndex(x, J$c)$D_morrill, OasisR::DIMorrill(x, c = J$c), tolerance = tol)
  expect_equal(ExposureIndex(x)$xPy, OasisR::xPy(x), tolerance = tol)
  expect_equal(IsolationIndex(x)$eta2, OasisR::Eta2(x), tolerance = tol)
  expect_equal(SpatialConcentration(x, J$area)$ACO, OasisR::ACO(x, a = J$area), tolerance = tol)
  expect_equal(SpatialConcentration(x, J$area)$RCO, OasisR::RCO(x, a = J$area), tolerance = tol)
  expect_equal(ClusteringIndex(x, contiguity = J$c)$ACL, OasisR::ACL(x, c = c1), tolerance = tol)
  expect_equal(ClusteringIndex(x, distance = J$d)$SP, OasisR::SP(x, d = J$d), tolerance = tol)
  expect_equal(CentralizationIndex(x, J$dc, J$area)$ACE, OasisR::ACE(x, a = J$area, dc = J$dc), tolerance = tol)
  expect_equal(CentralizationIndex(x, J$dc)$RCE, OasisR::RCE(x, dc = J$dc), tolerance = tol)
  expect_equal(SegregationEvenness(x)$H, OasisR::HTheil(x), tolerance = tol)
  expect_equal(SegregationEvenness(x)$gini, OasisR::Gini(x), tolerance = tol)
})
