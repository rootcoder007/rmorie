# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: Jaeger fragmentation vs landscapemetrics.

town <- function() {
  U <- .morie_random_uniform(2000, seed = 17, stream = 0)
  i <- 0:24
  j <- 0:5
  dem <- cbind(10 * U[2 * i + 1], 10 * U[2 * i + 2])
  sup <- cbind(10 * U[101 + 2 * j], 10 * U[102 + 2 * j])
  D <- outer(seq_len(25), seq_len(6), Vectorize(function(a, b) sqrt(sum((dem[a, ] - sup[b, ])^2))))
  grid <- matrix(as.numeric(U[501:620] < 0.55), 10, 12, byrow = TRUE)
  list(P = round(50 + 200 * U[201 + i]), S = round(5 + 20 * U[301 + j]), D = D, grid = grid)
}

test_that("LandscapeFragmentation matches landscapemetrics class metrics", {
  skip_if_not_installed("landscapemetrics")
  skip_if_not_installed("terra")
  f <- LandscapeFragmentation(town()$grid)
  r <- terra::rast(town()$grid)
  terra::ext(r) <- c(0, ncol(r), 0, nrow(r))
  terra::crs(r) <- "EPSG:3857"
  m <- landscapemetrics::lsm_c_mesh(r)
  s <- landscapemetrics::lsm_c_split(r)
  d <- landscapemetrics::lsm_c_division(r)
  expect_equal(f$mesh, m$value[m$class == 1] * 10000, tolerance = 1e-9)
  expect_equal(f$splitting, s$value[s$class == 1], tolerance = 1e-6)
  expect_equal(f$division, d$value[d$class == 1], tolerance = 1e-9)
})

test_that("LandscapeMetrics and ClassMetrics equal landscapemetrics::calculate_lsm", {
  skip_if_not_installed("landscapemetrics")
  skip_if_not_installed("terra")
  u <- .morie_random_uniform(80, seed = 13, stream = 0)
  m <- matrix(floor(u * 3) + 1, 8, 10)
  m[3, 4] <- NA
  r <- terra::rast(m, extent = terra::ext(0, 10, 0, 8))
  lm <- c("contag", "iji", "ai", "lsi", "ed", "shdi", "pd", "lpi")
  ref <- suppressWarnings(as.data.frame(landscapemetrics::calculate_lsm(r, what = paste0("lsm_l_", lm), directions = 8)))
  ours <- LandscapeMetrics(m, 1, 8)
  for (mt in lm) expect_equal(ours[[mt]], ref$value[ref$metric == mt], tolerance = 1e-12, info = mt)
  cm <- c("cohesion", "ai", "lsi", "iji", "enn_mn", "shape_mn")
  ref <- suppressWarnings(as.data.frame(landscapemetrics::calculate_lsm(r, what = paste0("lsm_c_", cm), directions = 8)))
  ours <- ClassMetrics(m, 1, 8)
  for (mt in cm) expect_equal(ours[[mt]], ref$value[ref$metric == mt], tolerance = 1e-12, info = mt)
})
