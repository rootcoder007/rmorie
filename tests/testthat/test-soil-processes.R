test_that("soil process formulas", {
  expect_equal(Ec25Correction(2, 25, "linear"), 2)
  expect_equal(Ec25Correction(2, 25), 2 * (0.447 + 1.4034 * exp(-25 / 26.815)), tolerance = 1e-14)
  expect_equal(CesiumRedistribution(2400, 2400, 0.25, 1300, 50), 0)
  expect_equal(IpccSoilCarbon(70, 0.8, 1.15, 1.04, 5)$stock, 70 * 0.8 * 1.15 * 1.04 * 5, tolerance = 1e-14)
  w <- RweqWindErosion(15, 0.4, 0.5, 0.7, 0.8, field_length = 1e6)
  expect_equal(w$transport, w$q_max, tolerance = 1e-12)
})
