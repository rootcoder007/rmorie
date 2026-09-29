.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("spdep")
skip_if_not_installed("gstat")
skip_if_not_installed("sp")

.lat_case <- function() {
  nb <- spdep::cell2nb(6, 5)
  ii <- 0:29
  y <- 2 + ((ii * 7) %% 11) / 3 + ((ii %/% 6) %% 3) / 2
  list(nb = nb, lw = spdep::nb2listw(nb), lb = spdep::nb2listw(nb, style = "B"),
       W = spdep::listw2mat(spdep::nb2listw(nb)), B = spdep::listw2mat(spdep::nb2listw(nb, style = "B")),
       y = y, x = ((ii * 5) %% 13) / 4)
}

test_that("Lactest, Lacgear and Lacgetg equal spdep's global tests", {
  cs <- .lat_case()
  m <- spdep::moran.test(cs$y, cs$lw)
  r <- Lactest(cs$y, cs$W)
  expect_equal(c(r$statistic, r$expected, r$variance), unname(m$estimate), tolerance = 1e-12)
  expect_equal(r$p_value, m$p.value, tolerance = 1e-10)
  for (rnd in c(TRUE, FALSE)) {
    g <- spdep::geary.test(cs$y, cs$lw, randomisation = rnd)
    q <- Lacgear(cs$y, cs$W, rnd)
    expect_equal(c(q$statistic, q$expected, q$variance), unname(g$estimate), tolerance = 1e-12)
    expect_equal(q$p_value, g$p.value, tolerance = 1e-10)
  }
  gg <- spdep::globalG.test(cs$y, cs$lb)
  h <- Lacgetg(cs$y, cs$B)
  expect_equal(c(h$statistic, h$expected, h$variance), unname(gg$estimate), tolerance = 1e-12)
})

test_that("Laclisa equals spdep::localmoran and Lacbivl equals spdep::lee", {
  cs <- .lat_case()
  lm <- spdep::localmoran(cs$y, cs$lw)
  r <- Laclisa(cs$y, cs$W)
  expect_equal(r$local_values, as.numeric(lm[, 1]), tolerance = 1e-12)
  expect_equal(r$expected, as.numeric(lm[, 2]), tolerance = 1e-12)
  expect_equal(r$variance, as.numeric(lm[, 3]), tolerance = 1e-12)
  expect_equal(r$p_value, as.numeric(lm[, 5]), tolerance = 1e-10)
  q <- attr(lm, "quadr")$pysal
  hh <- Laclihh(cs$y, cs$W, 0.2)$indices + 1L
  expect_true(all(q[hh] == "High-High"))
  l <- spdep::lee(cs$x, cs$y, cs$lw, n = 30)
  b <- Lacbivl(cs$x, cs$y, cs$W)
  expect_equal(b$statistic, l$L, tolerance = 1e-12)
  expect_equal(b$local_values, l$localL, tolerance = 1e-12)
})

test_that("Lacvgm equals gstat::variogram", {
  ii <- 0:24
  xy <- data.frame(x = (ii %% 5) + ((ii * 3) %% 7) / 10, y = (ii %/% 5) + ((ii * 5) %% 11) / 20)
  z <- 1 + xy$x / 2 + ((ii * 7) %% 9) / 4
  d <- data.frame(xy, z = z)
  sp::coordinates(d) <- ~ x + y
  v <- gstat::variogram(z ~ 1, d)
  r <- Lacvgm(z, as.matrix(xy))
  expect_equal(r$np, v$np)
  expect_equal(r$dist, v$dist, tolerance = 1e-12)
  expect_equal(r$gamma, v$gamma, tolerance = 1e-12)
})
