# Tests for AirDispersion: atmospheric dispersion models.

test_that("PgSigmas follows the Briggs formulas", {
  x <- c(100, 2500)
  r <- PgSigmas(x, "C")
  expect_equal(r$sigma_y, 0.11 * x / sqrt(1 + 1e-4 * x), tolerance = 1e-12)
  expect_equal(r$sigma_z, 0.08 * x / sqrt(1 + 2e-4 * x), tolerance = 1e-12)
  expect_equal(PgSigmas(x, "A", "urban")$sigma_z, 0.24 * x * sqrt(1 + 1e-3 * x), tolerance = 1e-12)
})

test_that("BriggsPlumeRise branches", {
  F_ <- 9.80616 * 12 * (420 - 285) / (4 * 420)
  r <- BriggsPlumeRise(c(10, 1e5), 4, diameter = 1, exit_velocity = 12, stack_temp = 420, ambient_temp = 285)
  expect_equal(r$flux, F_)
  expect_equal(r$rise, c(1.6 * F_^(1 / 3) * 10^(2 / 3) / 4, 21.425 * F_^0.75 / 4), tolerance = 1e-12)
  s <- 9.80616 * 0.02 / 285
  st <- BriggsPlumeRise(1e5, 4, diameter = 1, exit_velocity = 12, stack_temp = 420, ambient_temp = 285, stability = "E")
  expect_equal(st$final_rise, 2.6 * (F_ / (4 * s))^(1 / 3), tolerance = 1e-12)
})

test_that("GaussianPlume and GaussianPuff formulas", {
  s <- PgSigmas(800, "D")
  expect_equal(GaussianPlume(10, 3, 0, rbind(c(800, 0, 0)))[1], 10 / (pi * 3 * s$sigma_y * s$sigma_z), tolerance = 1e-14)
  s2 <- PgSigmas(600, "B", "urban")
  sy <- s2$sigma_y
  sz <- s2$sigma_z
  v <- exp(-49 / (2 * sz^2)) + exp(-169 / (2 * sz^2))
  want <- 5 / ((2 * pi)^1.5 * sy^2 * sz) * exp(-2500 / (2 * sy^2)) * exp(-49 / (2 * sy^2)) * v
  expect_equal(GaussianPuff(5, 2, 10, rbind(c(550, 7, 3)), 300, stability = "B", setting = "urban"), want, tolerance = 1e-12)
})

test_that("AdvectionDiffusion2d and LagrangianParticles", {
  c0 <- matrix(0, 6, 6)
  c0[3, 4] <- 1
  expect_equal(AdvectionDiffusion2d(c0, 1, 0, 0, 0, 1, 1, 1, 1)$field[4, 4], 1)
  d <- AdvectionDiffusion2d(c0, 0, 0, 0.1, 0.2, 1, 1, 1, 1)
  expect_equal(c(d$field[3, 4], d$field[2, 4], d$field[3, 5]), c(0.4, 0.1, 0.2), tolerance = 1e-15)
  r <- LagrangianParticles(7, 1, 2, 0.8, 0.1, 0.5, 0.3, 0.7, 3, seed = 5)
  x <- rep(1, 7)
  for (k in 0:2) x <- x + 0.8 * 0.7 + sqrt(2 * 0.5 * 0.7) * .morie_random_normal(7, seed = 5, stream = 2 * k)
  expect_equal(r$x, x, tolerance = 1e-12)
})
