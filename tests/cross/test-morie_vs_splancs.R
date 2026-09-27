test_that("SpaceTimeK equals splancs::stkhat", {
  skip_if_not_installed("splancs")
  U <- .morie_random_uniform(200, seed = 32, stream = 0)
  P <- cbind(2 * U[1:70], U[71:140])
  tt <- 10 * U[141:200]
  P <- P[1:60, ]
  s <- c(0.05, 0.1, 0.2, 0.3)
  tm <- c(0.5, 1, 2, 3)
  ref <- splancs::stkhat(P, tt, rbind(c(0, 0), c(2, 0), c(2, 1), c(0, 1)), c(0, 10), s, tm)
  r <- SpaceTimeK(P, tt, c(0, 2, 0, 1), c(0, 10), s, tm)
  expect_lt(max(abs(r$kst - ref$kst)), 1e-8)
  expect_lt(max(abs(r$ks - ref$ks)), 1e-8)
  expect_lt(max(abs(r$kt - ref$kt)), 1e-8)
})
