sw_w3 <- matrix(c(0, 1, 0, .5, 0, .5, 0, 1, 0), 3, byrow = TRUE)

test_that("RhoBounds on the 3-path and rejection of a general W", {
  r <- RhoBounds(sw_w3)
  expect_equal(r$eigenvalues, c(-1, 0, 1), tolerance = 1e-12)
  expect_equal(c(r$lower, r$upper), c(-1, 1), tolerance = 1e-12)
  expect_error(RhoBounds(matrix(c(0, .9, .3, 0), 2)))
})

test_that("ErrorOperator and LagOperator", {
  M <- ErrorOperator(matrix(c(0, 1, 1, 0), 2), 0.5)
  expect_equal(round(M, 6), matrix(c(1.333333, 0.666667, 0.666667, 1.333333), 2))
  expect_equal(LagOperator(sw_w3, c(1, 2, 4)), c(2, 2.5, 2))
  expect_equal(LagOperator(sw_w3, c(1, 2, 4), power = 2), c(2.5, 2, 2.5))
})

test_that("block, regime and contiguity weights", {
  expect_equal(BlockWeights(c("a", "b", "a")), matrix(c(0, 0, 1, 0, 0, 0, 1, 0, 0), 3))
  expect_equal(RegimeWeights(1 - diag(3), c(1, 1, 2)), matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), 3))
  sq <- function(x, y) cbind(c(x, x + 1, x + 1, x, x), c(y, y, y + 1, y + 1, y))
  P <- list(sq(0, 0), sq(1, 0), sq(1, 1))
  expect_equal(PolygonContiguity(P, queen = FALSE), matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3))
  expect_equal(PolygonContiguity(P)[1, ], c(0, 1, 1))
})

test_that("NeighbourCardinality and CompareNeighbours", {
  path <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3)
  r <- NeighbourCardinality(path)
  expect_equal(r$cardinality, c(1L, 2L, 1L))
  expect_equal(as.vector(r$table), c(2L, 1L))
  c2 <- CompareNeighbours(path, 1 - diag(3))
  expect_equal(c2$difference, list(3L, integer(0), 1L))
  expect_equal(c(c2$only_first, c2$only_second), c(0, 2))
})
