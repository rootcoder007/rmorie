test_that("Cox process moments recompute", {
  r <- ThomasPcf(0.1, 10, 0.05)
  expect_equal(r$K, pi * 0.01 + (1 - exp(-0.01 / 0.01)) / 10, tolerance = 1e-14)
  v <- VoronoiResiduals(rbind(c(0.25, 0.5), c(0.75, 0.5), c(0.5, 0.9)), c(0, 0, 1, 1), c(log(3), 0, 0))
  expect_equal(v$residuals, 1 - 3 * v$areas, tolerance = 1e-12)
  expect_equal(sum(v$areas), 1, tolerance = 1e-12)
  s <- LgcpSimulate(c(0, 1, 0, 1), 3, 2, list(model = "Exp", psill = 0.4, range = 0.3), seed = 1)
  expect_true(all(s$points >= 0 & s$points <= 1))
})
