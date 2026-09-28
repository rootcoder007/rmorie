test_that("spectral graph indices", {
  A2 <- matrix(c(0, 1, 1, 0), 2)
  expect_equal(EstradaIndex(A2), 2 * cosh(1))
  expect_equal(Communicability(A2)$matrix[1, 2], sinh(1))
  T2 <- matrix(0, 6, 6)
  for (e in list(c(1, 2), c(1, 3), c(2, 3), c(3, 4), c(4, 5), c(4, 6), c(5, 6))) T2[e[1], e[2]] <- T2[e[2], e[1]] <- 1
  expect_equal(ModularityMatrix(T2, rep(0:1, each = 3))$modularity, 2 * (3 / 7 - 0.25))
  expect_equal(PerronFrobenius(1 - diag(3))$eigenvalue, 2)
  expect_equal(RandicIndex(matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3)), sqrt(2))
  two <- T2
  two[3, 4] <- two[4, 3] <- 0
  expect_equal(LabelPropagation(two, seed = 3)$membership, rep(0:1, each = 3))
})
