test_that("polarization indices", {
  x <- c(-2, -1.5, -1, 0.5, 1, 1.8, 2.2)
  r <- PolarizationIndices(x)
  expect_equal(r$variance, var(x))
  expect_equal(r$gini_mean_difference, sum(abs(outer(x, x, `-`))) / 49)
  expect_equal(EstebanRayIndex(c(0, 1), c(0.5, 0.5)), 0.25)
  expect_equal(EarthMoversDistance(c(0, 1), c(2, 3)), 2)
  expect_equal(EarthMoversDistance(c(0, 0, 3), 1), 2 / 3 + 2 / 3)
  expect_equal(round(IssueConstraint(rbind(c(1, 2), c(2, 4), c(3, 5)))$constraint, 6), 0.981981)
  tr <- PolarizationTrend(1:4, c(0.5, 0.6, 0.8, 0.9))
  expect_equal(c(tr$slope, tr$intercept), c(0.14, 0.35))
})

test_that("party systems and roll calls", {
  expect_equal(round(PartySystemIndices(c(50, 30, 20))$golosov, 6), 2.139979)
  d <- PartyDivergence(c(-0.9, -0.4, 0.2, -0.1, 0.3, 0.8), rep(c("D", "R"), each = 3), "D", "R")
  expect_equal(d$overlap, 2)
  m <- RollcallMatrix(c("a", "a", "b", "b"), c(1, 2, 1, 2), c(1, 6, 4, 9))$matrix
  expect_equal(m, rbind(c(1, 0), c(0, NA)))
  V <- rbind(c(1, 1, 0), c(1, 0, 0), c(1, NA, 1), c(0, 0, 1), c(0, 0, 1), c(0, 1, 1))
  f <- RollcallFilter(V, lop = 0.35, minvotes = 2)
  expect_equal(f$votes, 1:2)
  expect_equal(f$legislators, c(1, 2, 4, 5, 6))
})
