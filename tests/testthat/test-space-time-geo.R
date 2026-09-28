M <- list(type = "productSum", k = 0.5, space = list(model = "Sph", psill = 0.8, range = 2, nugget = 0.05),
          time = list(model = "Exp", psill = 0.6, range = 1.5))
u <- .morie_random_uniform(200, seed = 41, stream = 0)
P <- cbind(3 * u[1:6], 3 * u[7:12])
coords <- P[rep(1:6, 5), ]
times <- rep(0:4, each = 6)
z <- 1 + u[22:51] + 0.3 * times

test_that("fit recovers a generating model", {
  h <- c(0.5, 1, 2, 3, 0.5, 1, 2, 3, 0, 0.5, 1, 2, 3)
  tu <- c(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2)
  g <- StModelVariogram(h, tu, "productSum", space = list(psill = 0.8, model = "Exp", range = 2, nugget = 0.1),
                        time = list(psill = 0.6, model = "Exp", range = 1.5, nugget = 0), k = 0.4)
  r <- StFit(h, tu, g, rep(10, 13), "productSum", c(0.5, 1, 0.05, 0.5, 1, 0.05, 0.2))
  expect_lt(r$objective, 1e-18)
  expect_equal(r$par, c(0.8, 2, 0.1, 0.6, 1.5, 0, 0.4), tolerance = 1e-3)
})

test_that("kriging variants agree with global ordinary kriging", {
  Q <- rbind(c(1, 1))
  g <- STKriging(z, coords, times, Q, 2, M)
  expect_equal(StUniversalKriging(z, matrix(1, 30, 1), coords, times, matrix(1, 1, 1), Q, 2, M)$prediction,
               g$prediction, tolerance = 1e-12)
  expect_equal(StLocalKriging(z, coords, times, Q, 2, M, nmax = 30, stani = 1)$prediction, g$prediction,
               tolerance = 1e-12)
  expect_equal(StBlockKriging(z, coords, times, Q, 2, M, 1, 1, 1, 1)$prediction, g$prediction, tolerance = 1e-12)
  expect_equal(StLeaveHOut(z, coords, times, M, 0, 0)$prediction, STKrigingCV(z, coords, times, M)$prediction,
               tolerance = 1e-12)
  d <- StKrigingDiagnostics(coords, times, Q, 2, M)
  expect_equal(sum(d$weights[[1]] * z), g$prediction, tolerance = 1e-12)
  expect_equal(StSmoothness(M)$space, -1)
  s <- StSimulate(coords[1:6, ], times[1:6], M, nsim = 1, seed = 5)
  expect_equal(tcrossprod(s$cholesky), STCovariance(as.matrix(dist(coords[1:6, ])), 0, M), tolerance = 1e-12,
               ignore_attr = TRUE)
})
