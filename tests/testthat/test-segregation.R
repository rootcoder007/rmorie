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

test_that("segregation indices match the Python arm", {
  J <- sg_data()
  x <- J$x
  expect_equal(DissimilarityIndex(x, J$c)$IS, c(0.4175199967581249, 0.3979502442675977, 0.2781501537835574), tolerance = 1e-12)
  expect_equal(IsolationIndex(x, distance = J$d)$DPxx, c(0.48982610740430754, 0.4211546654088076, 0.18334038623890425), tolerance = 1e-12)
  expect_equal(SpatialConcentration(x, J$area)$ACO, c(0.5685863753084679, 0.4588945627178945, 0.520065927156216), tolerance = 1e-12)
  expect_equal(ClusteringIndex(x, distance = J$d)$SP, 1.089139396303219, tolerance = 1e-12)
  expect_equal(SegregationEvenness(x)$atkinson, c(0.2741657642995514, 0.253282212620774, 0.1203529735301746), tolerance = 1e-12)
  expect_equal(CentralizationIndex(rbind(c(10, 0), c(5, 5), c(0, 10)), c(0, 1, 2))$RCE[1, 2], 8 / 9, tolerance = 1e-15)
})
