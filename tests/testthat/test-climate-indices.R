# Tests for ClimateIndices: climate indices and climate-change diagnostics.

sst <- 27 + 1.3 * sin(2 * pi * (0:119) / 43) + 0.4 * cos((0:119) * 0.9)
clim_ <- function(x, start = 1) {
  m <- (start - 1 + seq_along(x) - 1) %% 12
  vapply(0:11, function(k) mean(x[m == k]), 0)
}

test_that("OniIndex is the running mean of monthly anomalies", {
  r <- OniIndex(sst, start_month = 4)
  cl <- clim_(sst, 4)
  a <- sst - cl[(3 + 0:119) %% 12 + 1]
  for (i in 2:119) expect_equal(r$oni[i], (a[i - 1] + a[i] + a[i + 1]) / 3, tolerance = 1e-12)
  expect_true(all(r$oni[r$phase == 1] >= 0.5))
  z <- c(rep(0, 12), rep(1, 5), rep(0, 19))
  expect_equal(sum(OniIndex(z, base = c(0, 12))$phase == 1), 5)
})

test_that("PdoIndex is the standardised leading principal component", {
  X <- outer(0:47, 0:3, function(i, j) sin(i * 0.3 + j) + 0.2 * j * cos(i * 0.11))
  r <- PdoIndex(X)
  A <- apply(X, 2, function(col) col - clim_(col)[(0:47) %% 12 + 1])
  C <- crossprod(A) / 47
  expect_equal(as.numeric(C %*% r$loadings), r$eigenvalue * r$loadings, tolerance = 1e-10)
  expect_equal(sum(r$index^2) / 47, 1, tolerance = 1e-12)
  expect_lt(r$loadings[which.max(abs(r$loadings))], 0)
})

test_that("QboIndex onsets and DegreeHeatingWeeks accumulation", {
  u <- 12 * sin(2 * pi * (0:119) / 27) + 3 * cos(0:119)
  r <- QboIndex(u)
  a <- u - clim_(u)[(0:119) %% 12 + 1]
  expect_equal(r$onsets, which(a[-120] < 0 & a[-1] >= 0) + 1)
  s <- 29 + 2 * sin((0:199) / 9)
  d <- DegreeHeatingWeeks(s, 29.3)
  hs <- pmax(s - 29.3, 0)
  for (t in c(1, 50, 120, 200)) {
    w <- hs[max(1, t - 83):t]
    expect_equal(d$dhw[t], sum(w[w >= 1]) / 7, tolerance = 1e-12)
  }
})

test_that("CycloneEnergy, HurricaneTrack and HadleyEdge definitions", {
  v <- c(20, 35, 50, 80, 110, 90, 60, 30)
  expect_equal(CycloneEnergy(v)$ace, 1e-4 * sum(v[v >= 35]^2))
  tr <- HurricaneTrack(c(0, 0), c(0, 1))
  expect_equal(tr$distance, 6371 * pi / 180, tolerance = 1e-12)
  expect_equal(tr$heading, 90)
  expect_equal(HurricaneTrack(c(12, 14, 17, 21), c(-50, -55, -56, -52))$recurvature, 3)
  h <- HadleyEdge(c(-40, -20, 0, 20, 40), c(20000, 80000), rbind(c(1, -2, 0, 2, -1), 0))
  c20 <- cos(20 * pi / 180)
  c40 <- cos(40 * pi / 180)
  expect_equal(h$edge_north, 20 + 20 * 2 * c20 / (2 * c20 + c40), tolerance = 1e-12)
  expect_equal(h$edge_south, -h$edge_north, tolerance = 1e-12)
})

test_that("CcScaling, SeaLevelSemiEmpirical, quantile mapping and attribution", {
  tt <- (0:59) %% 10
  expect_equal(CcScaling(tt, exp(0.07 * tt), min_count = 2)$slope, 0.07, tolerance = 1e-12)
  T_ <- 0.1 * (0:19)
  H <- cumsum(c(0, 3.4 * ((T_[-20] + T_[-1]) / 2 + 0.5)))
  r <- SeaLevelSemiEmpirical(T_, H)
  expect_equal(c(r$a, r$T0), c(3.4, -0.5), tolerance = 1e-9)
  obs <- c(3.1, 5.2, 1.7, 8.8, 4.4, 6.0)
  hist <- c(1, 2, 2.5, 4, 7, 9, 10)
  expect_equal(EmpiricalQuantileMap(obs, hist, hist), unname(quantile(obs, (0:6) / 6)), tolerance = 1e-12)
  b <- BcsdDownscale(list(c(1, 2, 3)), list(c(0, 1, 2)), list(c(1, 2)), rbind(c(0, 0)), rbind(c(3, 4)), 5)
  expect_equal(as.numeric(b$fine), c(5, 6))
  p <- ProbabilityRatio(c(1, 5, 6, 7, 3), c(1, 2, 3, 6), 4.5)
  expect_equal(p$pr, (3 / 5) / (1 / 4))
})
