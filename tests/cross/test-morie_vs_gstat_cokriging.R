test_that("LmcCokriging equals gstat ordinary cokriging in three dimensions", {
  skip_if_not_installed("gstat")
  u <- .morie_random_uniform(200, seed = 4, stream = 0)
  n <- 30
  P <- cbind(5 * u[1:n], 5 * u[31:60], 2 * u[61:90])
  var <- ifelse(seq_len(n) %% 3 == 0, 1L, 0L)
  z <- 0.5 * P[, 1] + P[, 3] + var + u[101:130]
  Q <- cbind(c(1, 3.3, 2.5, 4), c(2, 0.4, 2.5, 4.4), c(0.5, 1.7, 1, 0.2))
  lmc <- list(list(model = "Nug", B = matrix(c(0.1, 0.02, 0.02, 0.2), 2)),
              list(model = "Exp", range = 1.5, B = matrix(c(1, 0.7, 0.7, 1.2), 2)),
              list(model = "Sph", range = 4, B = matrix(c(0.5, -0.2, -0.2, 0.3), 2)))
  d <- data.frame(x = P[, 1], y = P[, 2], h = P[, 3], z = z)
  g <- gstat::gstat(NULL, "a", z ~ 1, d[var == 0, ], locations = ~x + y + h,
                    model = gstat::vgm(1, "Exp", 1.5, add.to = gstat::vgm(0.5, "Sph", 4, nugget = 0.1)))
  g <- gstat::gstat(g, "b", z ~ 1, d[var == 1, ], locations = ~x + y + h,
                    model = gstat::vgm(1.2, "Exp", 1.5, add.to = gstat::vgm(0.3, "Sph", 4, nugget = 0.2)))
  g <- gstat::gstat(g, c("a", "b"), model = gstat::vgm(0.7, "Exp", 1.5, add.to = gstat::vgm(-0.2, "Sph", 4, nugget = 0.02)))
  pr <- suppressMessages(predict(g, data.frame(x = Q[, 1], y = Q[, 2], h = Q[, 3])))
  a <- LmcCokriging(z, P, var, Q, lmc, target = 0)
  b <- LmcCokriging(z, P, var, Q, lmc, target = 1)
  expect_equal(a$prediction, pr$a.pred, tolerance = 1e-10)
  expect_equal(a$variance, pr$a.var, tolerance = 1e-10)
  expect_equal(b$prediction, pr$b.pred, tolerance = 1e-10)
  expect_equal(b$variance, pr$b.var, tolerance = 1e-10)
})
