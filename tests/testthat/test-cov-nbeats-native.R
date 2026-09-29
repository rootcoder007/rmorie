# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/nbeats_native.R (N-BEATS doubly residual stacking,
# Oreshkin et al. 2020, with closed-form least-squares blocks). A
# trend block reproduces a quadratic window and extrapolates the same
# polynomial; a seasonality block continues a period-L signal; the
# stack residual telescopes.

.nb_L <- 12
.nb_t <- 0:11
.nb_quad <- 1 + 2 * .nb_t / 12 - 0.5 * (.nb_t / 12)^2

test_that("trend and seasonality bases are P x L polynomial / Fourier rows", {
  tb <- nbeats_trend_basis(5, 2, offset = 1, scale = 4)
  expect_equal(tb, rbind(rep(1, 5), (1:5) / 4, ((1:5) / 4)^2))
  expect_equal(dim(nbeats_trend_basis(1, 2)), c(3L, 1L))
  sb <- nbeats_seasonality_basis(6, 2, offset = 0, period = 6)
  expect_equal(sb[3, ], cos(2 * pi * 2 * (0:5) / 6), tolerance = 1e-12)
  expect_equal(sb[4, ], sin(2 * pi * 2 * (0:5) / 6), tolerance = 1e-12)
  expect_error(nbeats_trend_basis(5, -1), "non-negative")
  expect_error(nbeats_seasonality_basis(5, 0), "at least 1 harmonic")
})

test_that("lstsq solves the (ridged) normal equations", {
  X <- cbind(1, 1:6, (1:6)^2)
  y <- c(2, 3, 7, 8, 12, 20)
  expect_equal(nbeats_lstsq(X, y, 0), unname(coef(lm(y ~ X - 1))), tolerance = 1e-10)
  expect_equal(nbeats_lstsq(X, y, 0.5), as.numeric(solve(crossprod(X) + 0.5 * diag(3), crossprod(X, y))),
               tolerance = 1e-12)
})

test_that("blocks back-cast the window and forecast from the same theta", {
  tr <- nbeats_block(.nb_quad, 4, "trend", degree = 2, ridge = 0)
  expect_equal(tr$backcast, .nb_quad, tolerance = 1e-10)
  tt <- 12:15
  expect_equal(tr$forecast, 1 + 2 * tt / 12 - 0.5 * (tt / 12)^2, tolerance = 1e-10)
  sw <- 3 * cos(2 * pi * .nb_t / 12) + sin(4 * pi * .nb_t / 12)
  se <- nbeats_block(sw, 5, "seasonality", harmonics = 2, ridge = 0)
  expect_equal(se$backcast, sw, tolerance = 1e-10)
  expect_equal(se$forecast, sw[1:5], tolerance = 1e-10)
  ge <- nbeats_block(.nb_quad, 3, "generic", ridge = 0)
  expect_equal(ge$backcast, .nb_quad, tolerance = 1e-12)
  expect_equal(ge$forecast, rep(mean(.nb_quad), 3), tolerance = 1e-12)
  expect_length(nbeats_block(.nb_quad, 1, "trend")$forecast, 1L)
  expect_error(nbeats_block(.nb_quad, 3, "arima"), "kind must be")
  expect_error(nbeats_block(.nb_quad, 0), "horizon must be at least 1")
})

test_that("stack passes residuals forward and sums forecasts", {
  w <- .nb_quad + 0.3 * cos(2 * pi * .nb_t / 12)
  blocks <- list(c("trend", 1, 1), c("seasonality", 0, 1))
  s <- nbeats_stack(w, 4, blocks, ridge = 0)
  b1 <- nbeats_block(w, 4, "trend", degree = 1, harmonics = 1, ridge = 0)
  b2 <- nbeats_block(w - b1$backcast, 4, "seasonality", degree = 0, harmonics = 1, ridge = 0)
  expect_equal(s$forecast, b1$forecast + b2$forecast, tolerance = 1e-12)
  expect_equal(s$residual, w - b1$backcast - b2$backcast, tolerance = 1e-12)
  expect_equal(s$trace[[2]]$residual_norm, sqrt(sum(s$residual^2)), tolerance = 1e-12)
})

test_that("nbeats_forecast stacks over the last lookback points", {
  y <- c(rep(0, 5), .nb_quad)
  for (fn in list(nbeats_forecast, morie_nbeats)) {
    f <- fn(y, 3, lookback = 12, blocks = list(c("trend", 2, 1)), ridge = 0)
    tt <- 12:14
    expect_equal(f$forecast, 1 + 2 * tt / 12 - 0.5 * (tt / 12)^2, tolerance = 1e-9)
    expect_equal(f$residual_norm, 0, tolerance = 1e-9)
    expect_equal(f$lookback, 12L)
  }
  d <- nbeats_forecast(y, 2)
  expect_equal(d$lookback, 8L)
  expect_equal(d$n_blocks, 3L)
  expect_equal(d$forecast, nbeats_stack(y[10:17], 2, list(c("trend", 2, 3), c("seasonality", 2, 3),
                                                        c("trend", 1, 3)))$forecast)
  expect_length(nbeats_forecast(y, 1)$forecast, 1L)
  expect_error(nbeats_forecast(1:3, 1), "too short")
})

test_that("nbeats_cheatsheet states the residual rule", {
  expect_match(nbeats_cheatsheet(), "x_l = x_{l-1} - xhat_{l-1}", fixed = TRUE)
})
