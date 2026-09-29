skip_if_not_installed("GWmodel")
skip_if_not_installed("sp")

i <- 0:39
xy <- cbind((i * 7) %% 10 + 0.3 * sin(i), (i * 3) %% 8 + 0.2 * cos(i))
X <- cbind(x1 = sin(i * 0.7) + 0.1 * xy[, 1] + 1.5, x2 = cos(i * 1.1) * (1 + 0.05 * xy[, 2]) + 2)
y <- 1 + (0.5 + 0.1 * xy[, 1]) * X[, 1] - (0.3 + 0.05 * xy[, 2]) * X[, 2] + 0.2 * sin(i * 2.3)

# GWmodel 2.4 reaches the backfitting fixed point on its C++ path (force.armadillo = TRUE); its R path
# with uncentred predictors stops elsewhere.
test_that("mgwrfit with fixed bandwidths equals GWmodel::gwr.multiscale with the hat matrix", {
  spdf <- sp::SpatialPointsDataFrame(xy, data.frame(y = y, X))
  out <- utils::capture.output(m <- GWmodel::gwr.multiscale(y ~ x1 + x2, spdf, kernel = "bisquare", bws0 = c(8, 15, 6),
                                                              bw.seled = rep(TRUE, 3), threshold = 1e-12,
                                                              max.iterations = 5000, hatmatrix = TRUE,
                                                              predictor.centered = rep(FALSE, 2),
                                                              force.armadillo = TRUE))
  r <- mgwrfit(y, X, xy, c(8, 15, 6), threshold = 1e-12, max_iter = 5000)
  df <- as.data.frame(m$SDF)
  expect_equal(r$betas, unname(as.matrix(df[, c("Intercept", "x1", "x2")])), tolerance = 1e-8)
  expect_equal(r$se, unname(as.matrix(df[, c("Intercept_SE", "x1_SE", "x2_SE")])), tolerance = 1e-6)
  expect_equal(r$aicc, m$GW.diagnostic$AICc, tolerance = 1e-6)
})
