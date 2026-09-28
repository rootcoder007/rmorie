test_that("police operations models", {
  erlang_b <- function(N, a) {
    b <- 1
    for (k in 1:N) b <- a * b / (k + a * b)
    b
  }
  r <- HypercubeQueue(c(0.6, 0.9, 0.5), list(c(0, 1, 2), c(1, 2, 0), c(2, 0, 1)), service_rate = 1.1)
  expect_equal(r$loss, erlang_b(3, 2 / 1.1))
  expect_equal(HypercubeQueue(1, list(c(0, 1)))$workload, c(0.5, 0.3))
  expect_equal(SquareRootLaw(100, 4)$distance, 2.5)
  z <- ShortCrimeLattice(3, 4, gamma = 0, B0 = rep(1, 9), omega = 0.1)
  expect_equal(z$B, rep(0.9^4, 9))
  e <- ShortCrimePde(matrix(0.03, 5, 5), matrix(0.002 / (1 / 30 + 0.03), 5, 5), steps = 50)
  expect_equal(e$B[3, 3], 0.03, tolerance = 1e-12)
  expect_error(HypercubeQueue(5, list(0), queue = "infinite"), "Lambda")
})
