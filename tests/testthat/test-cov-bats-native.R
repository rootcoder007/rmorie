# Coverage for BATS/TBATS (De Livera, Hyndman & Snyder 2010). The
# transforms and harmonic frequencies are recomputed from eqs 3-4, the
# filter against the hand-written innovations recursion, the probed state
# matrices against w, F, g written out for a trend + index-seasonal model,
# the seed state against the MASS::ginv least-squares solution, and the
# fitted model's likelihood, AIC and Holt forecasts from their formulas.

test_that("Box-Cox transform and its inverse", {
  y <- c(0.5, 2, 7.3)
  for (om in c(0, 0.5, 1.2)) {
    z <- box_cox(y, om)
    expect_equal(z, if (om == 0) log(y) else (y^om - 1) / om, tolerance = 1e-12)
    expect_equal(inv_box_cox(z, om), y, tolerance = 1e-12)
  }
  expect_error(box_cox(c(1, 0), 0.5), "strictly positive")
  expect_error(inv_box_cox(-5, 0.5), "undefined")
})

test_that("seasonal harmonics, specs and parameter counts", {
  expect_equal(seasonal_harmonics(12), 2 * pi * (1:6) / 12, tolerance = 1e-12)
  expect_equal(seasonal_harmonics(7), 2 * pi * (1:3) / 7, tolerance = 1e-12)
  expect_equal(seasonal_harmonics(365.25 / 7, 4), 2 * pi * (1:4) / (365.25 / 7), tolerance = 1e-12)
  expect_error(seasonal_harmonics(1), "must exceed 1")
  expect_error(seasonal_harmonics(6, 4), "exceeds m/2")
  s <- BatsSpec(c(4, 7), harmonics = list(2, 3), use_box_cox = TRUE, use_trend = TRUE, damped = TRUE, p = 1, q = 2)
  expect_identical(s$harmonics, c(2L, 3L))
  expect_identical(n_free(s), 1L + 2L + 4L + 3L + 1L)
  expect_identical(n_free(BatsSpec(c(4, 7))), 1L + 1L + 2L)
  expect_false(BatsSpec(damped = TRUE, use_trend = FALSE)$damped)
  expect_error(BatsSpec(52.18), "integer periods")
  expect_error(BatsSpec(c(4, 7), harmonics = list(2)), "harmonic counts")
  expect_error(BatsSpec(p = -1), "non-negative")
})

test_that("the filter is the innovations recursion of eqs 3a-3f", {
  spec <- BatsSpec(3, use_trend = TRUE)
  th <- c(0.3, 0.1, 0.2)
  x0 <- c(10, 0.5, 1, -2, 1)
  z <- c(11.2, 8.9, 12.4, 12.1, 9.8, 13.9, 13.2)
  r <- bats_filter(z, spec, th, x0)
  lev <- 10
  tr <- 0.5
  buf <- c(1, -2, 1)
  eps <- numeric(7)
  fit <- numeric(7)
  for (t in 1:7) {
    fit[t] <- lev + tr + buf[1]
    eps[t] <- z[t] - fit[t]
    lev <- lev + tr + 0.3 * eps[t]
    tr <- tr + 0.1 * eps[t]
    buf <- c(buf[-1], buf[1] + 0.2 * eps[t])
  }
  expect_equal(r$fitted, fit, tolerance = 1e-12)
  expect_equal(r$resid, eps, tolerance = 1e-12)
  expect_equal(c(r$carry$level, r$carry$trend), c(lev, tr), tolerance = 1e-12)
  sm <- state_matrices(spec, th)
  F <- rbind(c(1, 1, 0, 0, 0), c(0, 1, 0, 0, 0), c(0, 0, 0, 1, 0), c(0, 0, 0, 0, 1), c(0, 0, 1, 0, 0))
  expect_equal(sm$w, c(1, 1, 1, 0, 0), tolerance = 1e-12)
  expect_equal(sm$fmat, F, tolerance = 1e-12)
  expect_equal(sm$g, c(0.3, 0.1, 0, 0, 0.2), tolerance = 1e-12)
  D <- F - outer(c(0.3, 0.1, 0, 0, 0.2), c(1, 1, 1, 0, 0))
  expect_equal(all_eigenvalues(spec, th), sort(Mod(eigen(D, only.values = TRUE)$values), decreasing = TRUE), tolerance = 1e-10)
  ev <- Mod(eigen(D, only.values = TRUE)$values)
  expect_identical(is_forecastable(spec, th), max(c(0, ev[abs(ev - 1) >= 1e-6])) < 1 - 1e-8)
  expect_false(is_forecastable(BatsSpec(use_trend = FALSE), 2.5))
})

test_that("a trigonometric seasonal rotates each harmonic pair (eq 4a-4c)", {
  spec <- BatsSpec(4, harmonics = list(1), use_trend = FALSE)
  th <- c(0.2, 0.05, -0.03)
  x0 <- c(5, 0.7, -0.4)
  z <- c(5.9, 4.6, 4.1, 5.8, 6.1)
  r <- bats_filter(z, spec, th, x0)
  lam <- 2 * pi / 4
  lev <- 5
  s <- 0.7
  ss <- -0.4
  fit <- numeric(5)
  for (t in 1:5) {
    fit[t] <- lev + s
    e <- z[t] - fit[t]
    lev <- lev + 0.2 * e
    ns <- s * cos(lam) + ss * sin(lam) + 0.05 * e
    ss <- -s * sin(lam) + ss * cos(lam) - 0.03 * e
    s <- ns
  }
  expect_equal(r$fitted, fit, tolerance = 1e-12)
})

test_that("the seed state is the minimum-norm least-squares solution", {
  skip_if_not_installed("MASS")
  spec <- BatsSpec(3, use_trend = TRUE)
  th <- c(0.3, 0.1, 0.2)
  z <- c(11.2, 8.9, 12.4, 12.1, 9.8, 13.9, 13.2, 12.5, 11.1)
  x0 <- fit_seed_state(z, spec, th)
  base <- bats_filter(z, spec, th, rep(0, 5))$resid
  cols <- vapply(1:5, function(j) bats_filter(rep(0, 9), spec, th, as.numeric(1:5 == j))$resid, numeric(9))
  expect_equal(x0, as.numeric(MASS::ginv(-cols) %*% base), tolerance = 1e-8)
  r <- bats_filter(z, spec, th, x0)
  expect_lt(max(abs(crossprod(cols, r$resid))), 1e-8)
  expect_equal(concentrated_loglik(z, r$resid, 1), -0.5 * 9 * log(sum(r$resid^2)), tolerance = 1e-12)
  expect_equal(concentrated_loglik(z, r$resid, 0.5), -4.5 * log(sum(r$resid^2)) - 0.5 * sum(log(z)), tolerance = 1e-12)
  expect_identical(concentrated_loglik(z, rep(0, 9), 1), Inf)
})

test_that("a fitted Holt model reports its likelihood, AIC and linear forecasts", {
  y <- 20 + 0.8 * (1:30) + c(0.3, -0.5, 0.2, 0.9, -0.1, -0.7, 0.4, 0.1, -0.3, 0.6)
  f <- morie_bats(y, use_box_cox = FALSE, use_trend = TRUE, damped = FALSE, h = 4L, maxiter = 150L)
  expect_identical(f$model, "BATS(1, 1, 0, 0)")
  expect_equal(f$loglik, concentrated_loglik(y, f$residuals, 1), tolerance = 1e-12)
  expect_equal(f$aic, -2 * f$loglik + 2 * f$n_par, tolerance = 1e-12)
  expect_identical(f$n_par, 2L + 2L)
  # omega = 1 is the shifted identity z = y - 1 of eq. 3a
  expect_equal(f$fitted, f$fitted_transformed + 1, tolerance = 1e-12)
  expect_equal(f$sigma2, mean(f$residuals^2), tolerance = 1e-12)
  cr <- bats_filter(y - 1, f$spec, c(f$alpha, f$beta), f$seed_state)$carry
  expect_equal(f$forecast_transformed, cr$level + (1:4) * cr$trend, tolerance = 1e-10)
  expect_equal(f$forecast, cr$level + (1:4) * cr$trend + 1, tolerance = 1e-10)
  expect_error(morie_bats(1:3), "too short")
  expect_error(morie_bats(1:10, seasonal_periods = 6), "two full cycles")
})
