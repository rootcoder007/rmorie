test_that("csinv is Graybill's inverse and equals solve()", {
  r <- csinv(4, 3, 0.7)
  expect_equal(r$inverse[1, 1:2], c(0.375106564364876383, -0.059676044330775758), tolerance = 1e-14)
  expect_equal(r$inverse, solve((3 - 0.7) * diag(4) + 0.7), tolerance = 1e-14)
  expect_false(csinv(3, 2, 2)$exists)
  expect_false(csinv(3, 1, -0.5)$exists)
})

test_that("nestvc gives the nested ANOVA components (Example 1.1)", {
  fac <- rep(1:2, each = 15)
  bat <- rep(1:6, each = 5)
  k <- rep(1:5, 6)
  y <- 10 + 0.4 * fac + c(-0.3, 0.5, 0.1, 0.25, -0.4, 0.2)[bat] +
    0.1 * sin(1.7 * seq_along(bat)) + 0.05 * cos(3.1 * k * bat)
  r <- nestvc(y, fac, bat)
  # anova(lm(y ~ fac / bat)) mean squares
  expect_equal(r$ms_unit, 0.8368069116642972149, tolerance = 1e-12)
  expect_equal(r$ms_error, 0.0071806039350631344, tolerance = 1e-12)
  expect_equal(r$sigma2_unit, (0.8368069116642972149 - 0.0071806039350631344) / 5, tolerance = 1e-12)
  # nlme::lme(REML) reaches the same components to its convergence tolerance
  expect_equal(r$sigma2_unit, 0.1659252609654789379, tolerance = 1e-8)
})

test_that("bvcchy equals mvtnorm::dmvt with df = 1", {
  expect_equal(bvcchy(c(0, 0.5, 3), c(0, -1.2, 2), 1.5)$density,
               c(0.0707355302630646304, 0.0305258002453248876, 0.0040087283290591302),
               tolerance = 1e-14)
})

test_that("plackt equals copula::pCopula and rho(plackettCopula())", {
  r <- plackt(c(0.2, 0.5, 0.9), c(0.7, 0.5, 0.3), 3.5)
  expect_equal(r$F12, c(0.17407983862836823, 0.32583426132260590, 0.28814064431721093),
               tolerance = 1e-12)
  expect_equal(r$rho, 0.39690547528518771, tolerance = 1e-12)
  expect_equal(plackt(0.3, 0.6, 0.4)$rho, -0.29713170694632218, tolerance = 1e-12)
  expect_equal(plackt(0.3, 0.6, 1)$F12, 0.18)
})

test_that("rhobin equals the linear programme over exchangeable laws", {
  # lpSolve::lp("min", k (k - 1), ...) subject to sum q = 1, sum k q = n mu
  expect_equal(rhobin(0.3, 5)$rho_lower, -0.19047619047619047, tolerance = 1e-14)
  expect_equal(rhobin(0.5, 4)$rho_lower, -0.33333333333333337, tolerance = 1e-14)
  expect_equal(rhobin(0.37, 7)$rho_lower, -0.14195828481542783, tolerance = 1e-14)
  expect_equal(rhobin(0.1, 3)$rho_lower, -0.11111111111111112, tolerance = 1e-14)
  expect_equal(rhobin(0.5, 4)$rho_lower, rhobin(0.5, 4)$rho_min_any)
})

test_that("mgamrf moments follow Problem 2.3", {
  r <- mgamrf(c(2, 1, 1, 3), 0.5)
  expect_equal(r$cov, matrix(c(0.75, 0.5, 0.5, 0.5, 0.75, 0.5, 0.5, 0.5, 1.25), 3))
  expect_equal(r$corr[1, 3], 2 / sqrt(3 * 5))
  expect_equal(r$shape, c(3, 3, 5))
  expect_false(r$stationary)
  expect_true(mgamrf(c(2, 1, 1), 0.5)$stationary)
})
