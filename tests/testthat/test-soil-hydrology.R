test_that("soil functions match their published equations", {
  r <- SaxtonRawls(0.4, 0.2, 2.5)
  t15t <- -0.024 * 0.4 + 0.487 * 0.2 + 0.006 * 2.5 + 0.005 * 0.4 * 2.5 - 0.013 * 0.2 * 2.5 + 0.068 * 0.4 * 0.2 + 0.031
  expect_equal(r$theta1500, t15t + 0.14 * t15t - 0.02)
  expect_equal(r$A * r$theta33^(-r$B), 33)
  expect_equal(round(c(r$theta1500, r$theta33), 6), c(0.137024, 0.27961))
  se <- (1 + (0.02 * 50)^1.5)^(-1 / 3)
  expect_equal(VanGenuchten(50, 0.05, 0.45, 0.02, 1.5)$theta, 0.05 + 0.4 * se)
  g <- Infiltration(2, "green_ampt", Ks = 1, psi = 10, dtheta = 0.3)
  expect_equal(g$cumulative - 3 * log(1 + g$cumulative / 3), 2, tolerance = 1e-12)
  expect_equal(round(ScsRunoff(50, 80)$runoff, 6), 13.80248)
  expect_equal(round(Rusle(100, 0.3, 0.2, 1, slope_length = 22.13, slope_pct = 9)$LS, 6), 0.999312)
})

test_that("chemistry, carbon, texture and Sobel", {
  expect_equal(unlist(SoilChemistry(na = 10, ca = 4, mg = 4, k = 1, h_al = 1, na_ex = 2)), c(sar = 5, cec = 20, esp = 10))
  expect_equal(SoilCarbon(2, 1.2, 20, 0.1, 20, 30)$pieri_si, 6.896)
  expect_equal(c(UsdaTexture(92, 5, 3), UsdaTexture(35, 35, 30), UsdaTexture(5, 50, 45)), c("sand", "clay loam", "silty clay"))
  expect_equal(SobelFilter(rbind(c(0, 0, 0), c(1, 1, 1), c(2, 2, 2)), 2)$gy[2, 2], 0.5)
  expect_error(UsdaTexture(50, 30, 30), "100")
})
