.kr_P <- rbind(c(0, 0), c(1.2, 0.3), c(0.4, 1.7), c(2.1, 1.1), c(1.5, 2.4), c(2.8, 0.2), c(0.9, 2.9), c(2.6, 2.7))
.kr_z <- c(1.0, 1.8, 0.7, 2.4, 1.3, 2.9, 0.4, 2.2)
.kr_Q <- rbind(c(0.8, 0.9), c(2.0, 2.0))
.kr_M <- list(model = "Exp", psill = 1, range = 1.5, nugget = 0.1)

test_that("Krige equals the gstat 2.1 values (OK, SK, UK, block, nmax)", {
  # gstat::krige(z ~ 1 / beta = 1.5 / z ~ x + y / block = c(.4, .6) / nmax = 4, vgm(1, "Exp", 1.5, 0.1))
  r <- Krige(.kr_z, .kr_P, .kr_Q, .kr_M)
  expect_equal(c(r$prediction, r$variance),
               c(1.3700319610959943, 1.8336566372755998, 0.58712741628861664, 0.55607174262983783), tolerance = 1e-12)
  r <- Krige(.kr_z, .kr_P, .kr_Q, .kr_M, beta = 1.5)
  expect_equal(c(r$prediction, r$variance),
               c(1.3657321296988003, 1.8290680140023887, 0.58652345365719227, 0.55538392702844674), tolerance = 1e-12)
  r <- Krige(.kr_z, .kr_P, .kr_Q, .kr_M, X = cbind(1, .kr_P), X0 = cbind(1, .kr_Q))
  expect_equal(c(r$prediction, r$variance),
               c(1.3046153589941756, 1.9094289827992641, 0.58824215244443556, 0.55773238768700539), tolerance = 1e-12)
  r <- Krige(.kr_z, .kr_P, .kr_Q, .kr_M, block = BlockDiscretize(c(0.4, 0.6)))
  expect_equal(c(r$prediction, r$variance),
               c(1.3676275906907347, 1.8350997220702985, 0.33303983608730015, 0.30408642636801864), tolerance = 5e-8)
  r <- Krige(.kr_z, .kr_P, .kr_Q, .kr_M, nmax = 4)
  expect_equal(c(r$prediction, r$variance),
               c(1.3817297908732462, 1.8451105843329656, 0.59058574824526833, 0.56056782493370072), tolerance = 1e-12)
})

test_that("KrigeCV equals gstat krige.cv (leave-one-out z-scores)", {
  r <- KrigeCV(.kr_z, .kr_P, .kr_M)
  expect_equal(r$zscore, c(-0.57682919341083627, 0.13032673208744461, -0.60586342354595335, 0.46892436160634643,
                           0.024918872421163512, 1.0488296060048288, -1.0854814655898963, 0.66070009595194623),
               tolerance = 1e-12)
  expect_equal(r$rmse, sqrt(mean(r$residual^2)))
})

test_that("kriging interpolates, the system agrees and factorial components add up", {
  r <- Krige(.kr_z, .kr_P, .kr_P[1:3, ], .kr_M)
  expect_equal(r$prediction, .kr_z[1:3], tolerance = 1e-12)
  s <- KrigingSystem(.kr_P, .kr_Q[1, ], .kr_M)
  k <- Krige(.kr_z, .kr_P, .kr_Q[1, , drop = FALSE], .kr_M)
  expect_equal(s$weights, k$weights[[1]], tolerance = 1e-12)
  expect_equal(s$lagrange, k$lagrange[[1]], tolerance = 1e-12)
  m <- list(list(model = "Exp", psill = 1, range = 1), list(model = "Sph", psill = 0.5, range = 4))
  a <- FactorialKrige(.kr_z, .kr_P, .kr_Q, m, 1)$prediction
  b <- FactorialKrige(.kr_z, .kr_P, .kr_Q, m, 2)$prediction
  ok <- Krige(.kr_z, .kr_P, .kr_Q, m)
  expect_equal(a + b + ok$beta, ok$prediction, tolerance = 1e-12)
})

test_that("lognormal back-transform, quantile and exceedance maps", {
  m <- list(model = "Exp", psill = 0.3, range = 2)
  r <- KrigeLognormal(.kr_z, .kr_P, .kr_Q, m)
  k <- Krige(log(.kr_z), .kr_P, .kr_Q, m)
  expect_equal(r$prediction, exp(k$prediction + k$variance / 2 + vapply(k$lagrange, `[`, 0, 1)), tolerance = 1e-14)
  expect_equal(round(KrigeLognormal(c(1, 3, 2), rbind(c(0, 0), c(2, 0), c(0, 2)), rbind(c(1, 1)), m)$prediction, 6),
               2.027853)
  expect_equal(KrigingQuantile(1, 0.25, 0.975), 1 + 1.959963984540054 * 0.5, tolerance = 1e-12)
  expect_equal(KrigingExceedance(1, 0.25, 1.5), 0.15865525393145705, tolerance = 1e-14)
  expect_equal(sum(BlockDiscretize(c(0.4, 0.6))$weights), 1)
})

test_that("Krige validates its inputs", {
  expect_error(Krige(.kr_z, .kr_P, .kr_Q, .kr_M, X = matrix(1, 8, 1)), "X0")
  expect_error(KrigeLognormal(c(0, 1), rbind(c(0, 0), c(1, 0)), rbind(c(0.5, 0)), .kr_M), "positive")
  expect_error(FactorialKrige(.kr_z, .kr_P, .kr_Q, .kr_M, 3), "component")
})
