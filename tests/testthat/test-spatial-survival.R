# Tests for SpatialSurvival: Weibull gamma-frailty survival and the SPDE precision.

i <- 0:59
x1 <- sin(i * 0.7)
cl <- i %/% 6
tt <- exp(1 + 0.5 * x1 + 0.4 * cos(cl * 1.7) + 0.6 * sin(i * 2.3))
ev <- as.numeric(i %% 5 != 0)

test_that("WeibullFrailtyFit without frailty equals survreg's Weibull fit", {
  r <- WeibullFrailtyFit(tt, ev, matrix(x1))
  s <- survival::survreg(survival::Surv(tt, ev) ~ x1, dist = "weibull")
  expect_equal(r$rho, unname(1 / s$scale), tolerance = 1e-6)
  expect_equal(r$loglik, s$loglik[2], tolerance = 1e-9)
})

test_that("SpdePrecisionGrid rows sum to tau^2 kappa^4 h^2", {
  Q <- SpdePrecisionGrid(5, 4, 0.8, 1.2, h = 0.5)$Q
  expect_equal(rowSums(Q), rep(1.2^2 * 0.8^4 * 0.25, 20), tolerance = 1e-12)
  expect_true(all(eigen(Q, symmetric = TRUE)$values > 0))
})
