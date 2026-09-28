test_that("cokriging reduces to kriging and keeps the unbiasedness constraints", {
  u <- .morie_random_uniform(100, seed = 6, stream = 0)
  P <- matrix(4 * u[1:80], 20, 4)
  z <- rowSums(P) + u[81:100]
  lmc <- list(list(model = "Nug", B = matrix(0.2)), list(model = "Exp", range = 2, B = matrix(1.3)))
  Q <- rbind(c(1, 1, 1, 1))
  r <- LmcCokriging(z, P, rep(0, 20), Q, lmc)
  ref <- Krige(z, P, Q, list(list(model = "Nug", psill = 0.2), list(model = "Exp", psill = 1.3, range = 2)))
  expect_equal(c(r$prediction, r$variance), c(ref$prediction, ref$variance), tolerance = 1e-10)
  expect_equal(sum(r$weights[[1]]), 1, tolerance = 1e-12)
})
