test_that("TurningBands example matches the Python arm", {
  f <- TurningBands(rbind(c(0, 0), c(1, .5)), "gaussian", n_bands = 4, n_waves = 3, seed = 2)$field
  expect_equal(round(f, 6), c(0.881358, 0.400421))
})

test_that("one band with one wave is sqrt(2 sill) cos(r <x, u> + phi)", {
  x <- c(0.7, -0.2)
  r <- TurningBands(rbind(x), "gaussian", n_bands = 1, n_waves = 1, seed = 9, range_ = 2, sill = 3)$field
  off <- .morie_random_uniform(1, seed = 9, stream = 0)
  u <- c(cos(pi * off), sin(pi * off))
  rad <- 2 / 2 * sqrt(-log(.morie_random_uniform(1, seed = 9, stream = 1)))
  ph <- 2 * pi * .morie_random_uniform(1, seed = 9, stream = 2)
  expect_equal(r, sqrt(3) * sqrt(2) * cos(rad * sum(x * u) + ph), tolerance = 1e-12)
})

test_that("3-D exponential radial quantile inverts its CDF and directions are unit", {
  for (u in c(0.1, 0.5, 0.93)) {
    r <- .tb_radial_3d_exp(u, 1)
    expect_equal((2 / pi) * (atan(r) - r / (1 + r^2)), u, tolerance = 1e-12)
  }
  d <- TurningBands(rbind(c(0, 0, 0)), "exponential", n_bands = 10, n_waves = 2, seed = 4)$directions
  expect_equal(sqrt(rowSums(d^2)), rep(1, 10), tolerance = 1e-12)
})

test_that("Monte Carlo covariance matches the model within 4 standard errors", {
  cases <- list(
    list(rbind(c(0, 0), c(0.8, 0)), "exponential", list(), exp(-0.8)),
    list(rbind(c(0, 0), c(0.8, 0)), "matern", list(nu = 1.5), 1.8 * exp(-0.8)),
    list(rbind(c(0, 0, 0), c(0.8, 0, 0)), "gaussian", list(), exp(-0.64))
  )
  for (cs in cases) {
    p <- vapply(1:800, function(s) {
      f <- do.call(TurningBands, c(list(cs[[1]], cs[[2]], n_bands = 8, n_waves = 8, seed = 20000 + s), cs[[3]]))$field
      f[1] * f[2]
    }, 0)
    expect_lt(abs(mean(p) - cs[[4]]), 4 * sd(p) / sqrt(length(p)))
  }
})

test_that("TurningBands validates its inputs", {
  expect_error(TurningBands(rbind(c(0, 0, 0)), "matern"), "matern")
  expect_error(TurningBands(matrix(0, 1, 1)), "2-D or 3-D")
  expect_error(TurningBands(rbind(c(0, 0)), ratio = 0), "ratio")
})
