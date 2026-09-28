# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: SdemML, GnsML, CarML, ResidualMoran vs spatialreg / spdep.

sd_data <- function(seed) {
  U <- .morie_random_uniform(400, seed = seed, stream = 0)
  nb <- spdep::cell2nb(6, 6)
  lw <- spdep::nb2listw(nb, style = "W")
  lb <- spdep::nb2listw(nb, style = "B")
  n <- 36
  x1 <- 2 * U[1:n]
  x2 <- U[n + 1:n] - 0.5
  Wm <- spdep::listw2mat(lw)
  e <- stats::qnorm(U[2 * n + 1:n])
  y <- as.vector(solve(diag(n) - 0.4 * Wm, 1 + 2 * x1 - x2 + 0.5 * Wm %*% x1 + e))
  list(d = data.frame(y = y, x1 = x1, x2 = x2), lw = lw, lb = lb, W = Wm, B = spdep::listw2mat(lb))
}

test_that("SdemML equals errorsarlm(Durbin = TRUE)", {
  skip_if_not_installed("spatialreg")
  s <- sd_data(3)
  m <- SdemML(s$d$y, cbind(1, s$d$x1, s$d$x2), s$W)
  r <- spatialreg::errorsarlm(y ~ x1 + x2, s$d, s$lw, Durbin = TRUE, method = "eigen", control = list(tol.opt = 1e-12))
  expect_equal(m$coefficients, unname(coef(r)[-1]), tolerance = 1e-6)
  expect_equal(m$lambda, unname(r$lambda), tolerance = 1e-6)
  expect_equal(m$loglik, as.numeric(logLik(r)), tolerance = 1e-9)
  expect_equal(m$se[seq_along(m$coefficients)], unname(summary(r)$Coef[, 2]), tolerance = 1e-5)
  im <- unclass(spatialreg::impacts(r))
  expect_equal(m$impacts$indirect, unname(im$impacts$indirect), tolerance = 1e-6)
  sm <- summary(spatialreg::impacts(r), zstats = TRUE)$semat
  expect_equal(m$impacts$se_total, unname(sm[, "Total"]), tolerance = 1e-5)
  expect_equal(m$impacts$se_indirect, unname(sm[, "Indirect"]), tolerance = 1e-5)
})

test_that("GnsML equals sacsarlm(Durbin = TRUE)", {
  skip_if_not_installed("spatialreg")
  s <- sd_data(5)
  m <- GnsML(s$d$y, cbind(1, s$d$x1, s$d$x2), s$W)
  r <- spatialreg::sacsarlm(y ~ x1 + x2, s$d, s$lw, Durbin = TRUE, method = "eigen")
  expect_equal(m$loglik, as.numeric(logLik(r)), tolerance = 1e-6)
  expect_equal(c(m$rho, m$lambda), unname(c(r$rho, r$lambda)), tolerance = 1e-3)
  imp <- spatialreg::impacts(r, listw = s$lw)
  ref <- SpatialImpacts(r$rho, coef(r)[c("x1", "x2")], s$W, coef(r)[c("lag.x1", "lag.x2")])
  expect_equal(unname(imp$direct), ref$direct, tolerance = 1e-8)
})

test_that("CarML equals spautolm(family = 'CAR') and ResidualMoran equals lm.morantest", {
  skip_if_not_installed("spatialreg")
  s <- sd_data(8)
  m <- CarML(s$d$y, cbind(1, s$d$x1, s$d$x2), s$B)
  r <- spatialreg::spautolm(y ~ x1 + x2, s$d, s$lb, family = "CAR", control = list(tol.opt = 1e-12))
  expect_equal(m$coefficients, unname(coef(r))[1:3], tolerance = 1e-6)
  expect_equal(m$lambda, unname(r$lambda), tolerance = 1e-6)
  expect_equal(m$loglik, as.numeric(logLik(r)), tolerance = 1e-9)
  for (lw in list(s$lw, s$lb)) {
    t <- spdep::lm.morantest(lm(y ~ x1 + x2, s$d), lw)
    q <- ResidualMoran(s$d$y, cbind(1, s$d$x1, s$d$x2), spdep::listw2mat(lw))
    expect_equal(c(q$I, q$expected, q$variance), unname(t$estimate), tolerance = 1e-12)
    expect_equal(q$pvalue, as.numeric(t$p.value), tolerance = 1e-12)
  }
})
