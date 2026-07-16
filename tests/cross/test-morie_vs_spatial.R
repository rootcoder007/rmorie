# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 19 cross-validation: native geostat/Moran vs gstat/spdep.
library(testthat)
library(rmorie)

test_that("native Moran machinery matches spdep::moran.test", {
  skip_if_not_installed("spdep")
  set.seed(170)
  n <- 60
  xy <- cbind(runif(n), runif(n))
  x <- as.numeric(xy[, 1] > 0.5) + rnorm(n, 0, 0.4)
  W <- rmorie:::.tps_knn_adjacency(xy, k = 5)
  lw <- spdep::mat2listw(W, style = "W", zero.policy = TRUE)
  ref <- spdep::moran.test(x, lw, zero.policy = TRUE,
                          randomisation = FALSE)
  # native pieces: row-standardization happens inside spdep via style
  # W; replicate with the same standardized matrix.
  Wr <- W / pmax(rowSums(W), 1e-12)
  z <- x - mean(x)
  S0 <- sum(Wr)
  I_val <- (n / S0) * as.numeric(t(z) %*% Wr %*% z) / sum(z^2)
  expect_equal(I_val, unname(ref$estimate["Moran I statistic"]),
               tolerance = 1e-10)
  var_I <- rmorie:::.tps_cliff_ord_variance(Wr, n, S0)
  expect_equal(var_I, unname(ref$estimate["Variance"]),
               tolerance = 1e-8)
})

test_that("native empirical variogram matches gstat::variogram", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  set.seed(171)
  n <- 200
  xy <- data.frame(x = runif(n), y = runif(n))
  v <- 5 + xy$x + rnorm(n, 0, 0.5)
  df <- cbind(xy, z = v)
  sp::coordinates(df) <- ~ x + y
  cutoff <- max(dist(xy)) / 3
  width <- cutoff / 15
  ref <- gstat::variogram(z ~ 1, data = df, cutoff = cutoff,
                          width = width)
  mine <- morie_spatial_variogram(as.matrix(xy), v, n_bins = 15,
                                  cutoff = cutoff)
  # Same bin boundaries; gstat reports mean pair distance per bin
  # while we report midpoints, so match by bin ORDER, not distance.
  kk <- min(nrow(mine), nrow(ref))
  expect_gt(kk, 8)
  expect_equal(mine$gamma[seq_len(kk)], ref$gamma[seq_len(kk)],
               tolerance = 1e-6)
  expect_equal(mine$np[seq_len(kk)], ref$np[seq_len(kk)])
})

test_that("native ordinary kriging tracks gstat::krige", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  set.seed(172)
  n <- 120
  xy <- data.frame(x = runif(n), y = runif(n))
  D <- as.matrix(dist(xy))
  C <- exp(-D / 0.3) + diag(0.05, n)
  v <- as.numeric(t(chol(C)) %*% rnorm(n)) + 10
  new_xy <- data.frame(x = runif(20), y = runif(20))
  vgm_fit <- list(model = "exponential", nugget = 0.05, psill = 1,
                  range = 0.3)
  mine <- morie_spatial_krige(as.matrix(xy), v, as.matrix(new_xy),
                              vgm = vgm_fit)
  df <- cbind(xy, z = v); sp::coordinates(df) <- ~ x + y
  nd <- new_xy; sp::coordinates(nd) <- ~ x + y
  gmod <- gstat::vgm(psill = 1, model = "Exp", range = 0.3,
                     nugget = 0.05)
  ref <- suppressWarnings(
    gstat::krige(z ~ 1, locations = df, newdata = nd, model = gmod,
                 debug.level = 0))
  expect_equal(mine$pred, ref$var1.pred, tolerance = 1e-6)
  expect_equal(mine$var, ref$var1.var, tolerance = 1e-6)
})
