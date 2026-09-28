test_that("practical salinity and oxygen solubility equal gsw; K0 equals seacarb", {
  skip_if_not_installed("gsw")
  skip_if_not_installed("seacarb")
  u <- .morie_random_uniform(90, seed = 44)
  C <- 5 + 55 * u[1:30]
  t <- -1 + 31 * u[31:60]
  p <- 4000 * u[61:90]
  sp <- gsw::gsw_SP_from_C(C, t, p)
  ok <- sp > 2
  expect_equal(PracticalSalinity(C, t, p)[ok], sp[ok], tolerance = 1e-12)
  S <- 5 + 35 * u[1:30]
  expect_equal(OxygenSolubility(t, S), gsw::gsw_O2sol_SP_pt(S, t), tolerance = 1e-12)
  k0 <- Co2Flux(5, t, S, 400, 400, "mol/kg")$k0
  expect_equal(k0, as.numeric(seacarb::K0(S = S, T = t, P = 0)), tolerance = 1e-12)
})
