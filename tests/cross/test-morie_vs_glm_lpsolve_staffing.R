skip_if_not_installed("lpSolve")

test_that("CallDemandModel equals stats::glm and ShiftSchedule equals the integer programme", {
  h <- 0:(24 * 7 * 4 - 1)
  mu <- exp(1.1 + 0.6 * sin(2 * pi * h / 24) + 0.2 * cos(2 * pi * h / 168) + 0.1 * sin(4 * pi * h / 24))
  y <- round(mu + 0.8 * sin(h * 1.37))
  m <- CallDemandModel(y, h, daily = 2, weekly = 1, annual = 0, trend = FALSE)
  X <- cbind(sin(2 * pi * h / 24), cos(2 * pi * h / 24), sin(4 * pi * h / 24), cos(4 * pi * h / 24),
             sin(2 * pi * h / 168), cos(2 * pi * h / 168))
  g <- stats::glm(y ~ X, family = stats::poisson(), control = stats::glm.control(epsilon = 1e-12, maxit = 100))
  expect_equal(m$coefficients, unname(coef(g)), tolerance = 1e-8)
  expect_equal(m$se, unname(sqrt(diag(vcov(g)))), tolerance = 1e-7)
  req <- c(3, 3, 2, 2, 2, 4, 6, 8, 8, 7, 7, 6, 6, 7, 8, 9, 9, 8, 7, 6, 6, 5, 4, 4)
  for (L in c(8, 10, 12)) {
    A <- t(vapply(0:23, function(t) as.numeric((t - 0:23) %% 24 < L), numeric(24)))
    ip <- lpSolve::lp("min", rep(1, 24), A, rep(">=", 24), req, all.int = TRUE)
    expect_equal(ShiftSchedule(req, L)$total, ip$objval)
  }
})
