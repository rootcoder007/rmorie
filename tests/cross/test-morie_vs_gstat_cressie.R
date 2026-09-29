.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("gstat")
skip_if_not_installed("sp")

test_that("Sgcrh(method = 'gstat') equals gstat::variogram(cressie = TRUE)", {
  ii <- 0:24
  xy <- data.frame(x = (ii %% 5) + ((ii * 3) %% 7) / 10, y = (ii %/% 5) + ((ii * 5) %% 11) / 20)
  z <- 1 + xy$x / 2 + ((ii * 7) %% 9) / 4
  d <- data.frame(xy, z = z)
  sp::coordinates(d) <- ~ x + y
  v <- gstat::variogram(z ~ 1, d, cressie = TRUE)
  r <- Sgcrh(z, as.matrix(xy), method = "gstat")
  expect_equal(r$np, v$np)
  expect_equal(r$gamma, v$gamma, tolerance = 1e-12)
  expect_equal(r$dist, v$dist, tolerance = 1e-12)
})
