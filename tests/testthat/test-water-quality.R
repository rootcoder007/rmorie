test_that("water quality indices", {
  X <- rbind(c(1, 8, 0.2), c(3, 5, NA), c(1.5, 9, 0.9))
  r <- CcmeWqi(X, c(2, 6, 0.5), c("max", "min", "max"))
  nse <- 1.5 / 8
  expect_equal(r$wqi, 100 - sqrt(1e4 + 37.5^2 + (nse / (0.01 * nse + 0.01))^2) / 1.732)
  expect_equal(round(CcmeWqi(rbind(c(1, 8), c(3, 5), c(1.5, 9)), c(2, 6), c("max", "min"))$wqi, 6), 38.841939)
  expect_equal(round(WeightedArithmeticWqi(c(7.5, 250, 2), c(8.5, 500, 5), c(7, 0, 0))$wqi, 6), 37.608882)
  expect_equal(round(DoSaturation(c(0, 5, 10, 20, 30))$cs, 2), c(14.62, 12.77, 11.29, 9.09, 7.56))
  expect_equal(round(DoSaturation(20, salinity = 35)$cs, 3), 7.396)
  ct <- CarlsonTsi(secchi_m = 2, chla_ugl = 10, tp_ugl = 30)
  expect_equal(round(c(ct$tsi_sd, ct$tsi_chl, ct$tsi_tp), 6), c(50.011749, 53.18836, 53.195266))
  expect_error(CarlsonTsi(), "at least one")
})

test_that("irrigation, solids, loads, removal", {
  i <- IrrigationWaterQuality(na = 6, ca = 3, mg = 5, k = 0.5, hco3 = 4, ec_us_cm = 900)
  expect_equal(c(i$sar, i$rsc, round(i$percent_na, 6)), c(3, -4, 44.827586))
  expect_equal(i$salinity_class, "C3")
  expect_equal(unlist(TotalDissolvedSolids(500, ions = c(40, 10, 20, 30, 50), bicarbonate = 100)),
               c(tds_ec = 320, tds_ions = 199.17))
  expect_equal(unlist(SuspendedSolids(1523.4, 1510.2, 250, ignited_mg = 1514.6)), c(tss = 52.8, vss = 35.2, fss = 17.6))
  expect_equal(unlist(ConstituentLoad(c(2, 4), c(1, 3), dt = 1)), c(load = 14, fwmc = 3.5))
  expect_equal(RemovalEfficiency(c(200, 1e6), c(20, 1e2))$lrv, c(1, 4))
})
