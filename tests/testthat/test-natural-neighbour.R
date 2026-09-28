P <- rbind(c(0.3, 0.1), c(2.1, 0.4), c(1.7, 2.2), c(0.2, 1.9), c(1.1, 1), c(2.9, 1.6), c(0.9, 2.8), c(2.4, 2.9))

test_that("Sibson and Laplace coordinates reproduce linear functions", {
  for (m in c("sibson", "laplace")) for (q in list(c(1, 1.2), c(1.6, 0.9), c(0.35, 0.3))) {
    w <- NaturalNeighbourWeights(P, q, m)$weights
    ix <- as.integer(names(w))
    expect_equal(sum(w), 1, tolerance = 1e-12)
    expect_equal(c(sum(w * P[ix, 1]), sum(w * P[ix, 2])), q, tolerance = 1e-10)
  }
})

test_that("interpolation, gradients, cross-validation and domain", {
  lin <- 2 * P[, 1] - P[, 2] + 1
  r <- NnInterpolate(P, lin, rbind(P[5, ], c(1.2, 1.3), c(9, 9)))
  expect_equal(r$estimates[1:2], c(lin[5], 2 * 1.2 - 1.3 + 1), tolerance = 1e-10)
  expect_true(is.nan(r$estimates[3]))
  g <- NnGradientInterpolate(P, lin, rbind(c(1.3, 1.4)))
  expect_equal(g$gradients[5, ], c(2, -1), tolerance = 1e-10)
  expect_equal(NnCrossValidation(P, lin)$rmse, 0, tolerance = 1e-10)
  expect_equal(NnInDomain(P, rbind(c(1, 1), c(5, 5))), c(TRUE, FALSE))
})
