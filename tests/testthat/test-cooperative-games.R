g1 <- c(0, 0, 26.413, 0, 37.955, 39.934, 49.945, 0, 38.706, 23.507, 58.321, 31.357, 57.588, 58.409, 82.377)
g2 <- c(0, 0, 4.324, 0, 0.155, 6.061, 13.931, 0, 19.323, 5.85, 8.784, 4.403, 18.23, 17.584, 12.825)

test_that("Nucleolus matches CoopGame values", {
  ref <- c(20.611666666667, 21.102833333333, 20.699666666667, 19.962833333333)
  expect_lt(max(abs(Nucleolus(g1)$x - ref)), 1e-9)
  expect_lt(max(abs(Nucleolus(g1, pre = TRUE)$x - ref)), 1e-9)
  expect_lt(max(abs(Nucleolus(g2)$x - c(3.637, 0, 1.898, 7.29))), 1e-9)
  expect_lt(max(abs(Nucleolus(g2, pre = TRUE)$x - c(3.637, -2.359, 4.257, 7.29))), 1e-9)
  expect_equal(Nucleolus(c(0, 0, 60, 0, 60, 60, 72))$levels, 12, tolerance = 1e-12)
})

test_that("KernelPoint reaches the nucleolus where theory says so", {
  w <- c(1, 2, 3, 5, 8)
  v <- vapply(1:31, function(S) sum(w[bitwAnd(S, 2^(0:4)) > 0])^2, 0)
  nu <- Nucleolus(v)$x
  for (x0 in list(NULL, c(60, 60, 60, 60, 121), c(1, 4, 9, 25, 322))) {
    k <- KernelPoint(v, x0)
    expect_true(k$converged && k$in_kernel)
    expect_lt(max(abs(k$x - nu)), 1e-8)
  }
  g <- c(0, 0, 26, 0, 38, 40, 50)
  expect_lt(max(abs(KernelPoint(g)$x - Nucleolus(g)$x)), 1e-8)
  k <- KernelPoint(g2, Nucleolus(g2)$x)
  expect_identical(k$iterations, 0L)
  expect_true(k$in_kernel)
})
