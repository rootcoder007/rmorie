test_that("MacKay coding results recompute", {
  r <- BoundedDistanceNoise(0.4)
  f <- r$f_shannon
  expect_equal(-f * log2(f) - (1 - f) * log2(1 - f), 0.6, tolerance = 1e-12)
  expect_true(SelfDualCodeCheck(diag(3)[c(2, 3, 1), ])$self_dual)
  expect_equal(RunlengthCapacity(1)$capacity, log2((1 + sqrt(5)) / 2), tolerance = 1e-14)
  s <- RobustSoliton(10000, 0.2, 0.05)
  expect_equal(s$m, 41L)
  expect_equal(sum(s$mu), 1, tolerance = 1e-12)
  cl <- CodeLengthDecomposition(c(0.3, 0.3, 0.2, 0.1, 0.1), c(2, 2, 2, 3, 4))
  expect_equal(cl$L, cl$H + cl$kl - log2(cl$kraft), tolerance = 1e-13)
  X <- array(0, c(2, 2, 2))
  for (i in 0:1) for (j in 0:1) X[i + 1, j + 1, bitwXor(i, j) + 1] <- 0.25
  expect_equal(McGillInteractionInformation(X)$ii, 1, tolerance = 1e-14)
  expect_equal(RepetitionErrorApprox(5, 0.1)$pb, (4 * 0.1 * 0.9)^2.5, tolerance = 1e-15)
})
