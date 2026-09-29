skip_if_not_installed("GWmodel")
skip_if_not_installed("sp")

set.seed(21)
n <- 30
P <- cbind(stats::runif(n, 0, 10), stats::runif(n, 0, 10))
x <- stats::rnorm(n)
y <- 1 + (1 + 0.1 * P[, 1]) * x + stats::rnorm(n, sd = 0.3)
df <- sp::SpatialPointsDataFrame(P, data.frame(y = y, x = x))

test_that("gwrcoef and gwrcv match GWmodel", {
  g <- suppressWarnings(GWmodel::gwr.basic(y ~ x, data = df, bw = 4, kernel = "bisquare"))
  expect_equal(gwrcoef(y, matrix(x), P, 4)[, 2], g$SDF$x, tolerance = 1e-10)
  expect_equal(gwrcv(y, matrix(x), P, 4), GWmodel::gwr.cv(4, cbind(1, x), y, "bisquare", FALSE, P,
                                                  dMat = as.matrix(stats::dist(P))), tolerance = 1e-10)
})

test_that("gwrpois and gwrlgt reproduce GWmodel::ggwr.basic with its tolerance", {
  cnt <- stats::rpois(n, exp(0.3 + 0.4 * x))
  dc <- sp::SpatialPointsDataFrame(P, data.frame(cnt = cnt, x = x))
  gp <- suppressWarnings(utils::capture.output(o <- GWmodel::ggwr.basic(cnt ~ x, data = dc, bw = 6, family = "poisson",
                                                                     kernel = "gaussian")))
  r <- gwrpois(cnt, matrix(x), P, 6, kernel = "gaussian", tol = 1e-5, maxiter = 20)
  expect_equal(r$betas[, 2], o$SDF$x, tolerance = 1e-10)
  expect_equal(r$betas[, 2], gwrpois(cnt, matrix(x), P, 6, kernel = "gaussian")$betas[, 2], tolerance = 1e-3)
  yb <- as.numeric(x + stats::rnorm(n) > 0)
  db <- sp::SpatialPointsDataFrame(P, data.frame(yb = yb, x = x))
  gb <- suppressWarnings(utils::capture.output(o2 <- GWmodel::ggwr.basic(yb ~ x, data = db, bw = 8, family = "binomial",
                                                                      kernel = "gaussian")))
  r2 <- gwrlgt(yb, matrix(x), P, 8, kernel = "gaussian", tol = 1e-5, maxiter = 20)
  expect_equal(r2$betas[, 2], o2$SDF$x, tolerance = 1e-10)
})
