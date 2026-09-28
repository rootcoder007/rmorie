test_that("movement analysis recomputes", {
  r <- TrackSteps(c(0, 1, 2, 2, 3), c(0, 0, 1, 2, 2))
  expect_equal(r$R2n, c(0, 1, 5, 8, 13))
  expect_equal(r$rel_angle[2], pi / 4, tolerance = 1e-15)
  n <- 6
  K <- matrix(1, n, n) - diag(n)
  expect_equal(NetworkRobustness(K)$R, sum((n - seq_len(n)) / n) / n, tolerance = 1e-15)
  A <- rbind(c(0, 1, 1, 1, 1), c(1, 0, 1, 0, 0), c(1, 1, 0, 1, 0), c(1, 0, 1, 0, 0), c(1, 0, 0, 0, 0))
  cc <- CorePeriphery(A)$coreness
  expect_equal(cc[1], sum(A[1, -1] * cc[-1]) / (sum(cc^2) - cc[1]^2), tolerance = 1e-10)
})
