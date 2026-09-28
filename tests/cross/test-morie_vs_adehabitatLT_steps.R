test_that("TrackSteps equals adehabitatLT::as.ltraj", {
  skip_if_not_installed("adehabitatLT")
  u <- .morie_random_uniform(80, seed = 2, stream = 0)
  x <- cumsum(u[1:40] - 0.3)
  y <- cumsum(u[41:80] - 0.6)
  tt <- cumsum(c(0, 30 + 60 * u[1:39]))
  lt <- adehabitatLT::as.ltraj(cbind(x, y), date = as.POSIXct(tt, origin = "1970-01-01"), id = "a")[[1]]
  r <- TrackSteps(x, y, tt)
  expect_equal(r$dist, lt$dist, tolerance = 1e-12)
  expect_equal(r$dt, lt$dt, tolerance = 1e-9)
  expect_equal(r$R2n, lt$R2n, tolerance = 1e-12)
  expect_equal(r$abs_angle, lt$abs.angle, tolerance = 1e-12)
  expect_equal(r$rel_angle, lt$rel.angle, tolerance = 1e-12)
})
