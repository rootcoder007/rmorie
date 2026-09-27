test_that("KernelIntensity matches spatstat density at the points", {
  U <- .morie_random_uniform(120, seed = 21, stream = 0)
  P <- cbind(2 * U[seq(1, 119, 2)], U[seq(2, 120, 2)])
  expect_lt(max(abs(KernelIntensity(P, c(0, 2, 0, 1), 0.15, correction = "none")$intensity[1:4] -
                      c(15.27830275986, 15.81007650758, 12.74879430664, 24.78991934139))), 1e-9)
  expect_lt(max(abs(KernelIntensity(P, c(0, 2, 0, 1), 0.15)$intensity[1:4] -
                      c(20.36689534952, 17.86577447956, 12.76588807138, 24.85439587018))), 1e-9)
  expect_lt(max(abs(KernelIntensity(P, c(0, 2, 0, 1), 0.15, correction = "diggle")$intensity[1:4] -
                      c(16.8166678279, 18.26119067353, 13.3843427791, 26.4264746192))), 1e-9)
  a <- KernelIntensity(P, c(0, 2, 0, 1), 0.15, at = rbind(c(0.1, 0.1), c(1, 0.5)))$intensity
  expect_lt(max(abs(a - c(51.84394804586, 18.6163631496))), 1e-9)
})
