# Coverage for the forecasting shelf: naive and seasonal-naive forecasts,
# the drift method (Hyndman & Athanasopoulos) with its h-step standard
# error, simple exponential smoothing (against stats::HoltWinters with the
# same start), additive and multiplicative Holt-Winters (against an
# independent transcription of the recursions), Croston / SBA with the
# Syntetos-Boylan classification, and MinT reconciliation by matrix
# algebra.

test_that("naive and seasonal-naive forecasts", {
  y <- c(3, 5, 4, 8, 6, 7, 9)
  r <- morie_joseph_naive_forecast(y, horizon = 3)
  expect_identical(r$forecast, rep(9, 3))
  expect_equal(r$in_sample_mae, mean(abs(diff(y))), tolerance = 1e-12)
  s <- morie_joseph_naive_forecast(y, horizon = 5, season = 3)
  expect_identical(s$forecast, y[c(5, 6, 7, 5, 6)])
  expect_equal(s$in_sample_mae, mean(abs(y[4:7] - y[1:4])), tolerance = 1e-12)
  expect_identical(s$method_used, "seasonal naive (m=3)")
  expect_true(is.na(morie_joseph_naive_forecast(2)$in_sample_mae))
  expect_error(morie_joseph_naive_forecast(y, season = 8), "between 1 and 7")
  expect_error(morie_joseph_naive_forecast(numeric(0)), "non-empty")
  expect_error(morie_joseph_naive_forecast(y, horizon = 0), "at least 1")
})

test_that("drift forecast: slope (y_n - y_1)/(n - 1) and sigma sqrt(h (1 + h/(n-1)))", {
  y <- c(10, 12, 11, 15, 16, 18, 17, 21)
  r <- morie_drift_forecast(y, h = 4)
  d <- (21 - 10) / 7
  expect_equal(r$drift, d, tolerance = 1e-12)
  expect_equal(r$forecast, 21 + (1:4) * d, tolerance = 1e-12)
  sig <- stats::sd(diff(y) - d)
  expect_equal(r$se, sig * sqrt((1:4) * (1 + (1:4) / 7)), tolerance = 1e-12)
  expect_equal(r$upper - r$forecast, 1.96 * r$se, tolerance = 1e-12)
  expect_identical(morie_drift_forecast(c(1, 3))$sigma, 0)
  expect_error(morie_drift_forecast(1), "at least 2")
  expect_error(morie_drift_forecast(y, h = 0), "at least 1")
})

test_that("simple exponential smoothing equals HoltWinters started at y[1]", {
  y <- c(12, 15, 11, 14, 18, 16, 13, 17, 19, 15)
  r <- morie_joseph_simple_exponential_smoothing(y, alpha = 0.35, horizon = 2)
  hw <- stats::HoltWinters(y, alpha = 0.35, beta = FALSE, gamma = FALSE, l.start = y[1])
  expect_equal(r$level, unname(hw$coefficients[["a"]]), tolerance = 1e-12)
  expect_equal(r$forecast, rep(r$level, 2))
  expect_equal(r$fitted[-1], as.numeric(hw$fitted[, "xhat"]), tolerance = 1e-12)
  expect_equal(r$sse, hw$SSE, tolerance = 1e-12)
  expect_equal(r$effective_window, 2 / 0.35 - 1, tolerance = 1e-12)
  auto <- morie_joseph_simple_exponential_smoothing(y)
  grid <- seq(0.01, 1, length.out = 100)
  sse <- vapply(grid, function(a) morie_joseph_simple_exponential_smoothing(y, alpha = a)$sse, 1)
  expect_identical(auto$alpha, grid[which.min(sse)])
  expect_error(morie_joseph_simple_exponential_smoothing(y, alpha = 0), "\\(0, 1\\]")
  expect_error(morie_joseph_simple_exponential_smoothing(1), "at least 2")
})

.hw <- function(y, m, a, b, g, h, mult) {
  l <- mean(y[1:m])
  tr <- (mean(y[(m + 1):(2 * m)]) - l) / m
  s <- if (mult) y[1:m] / l else y[1:m] - l
  fit <- numeric(length(y))
  for (t in seq_along(y)) {
    st <- s[t]
    if (mult) {
      fit[t] <- (l + tr) * st
      ln <- a * y[t] / st + (1 - a) * (l + tr)
      s[t + m] <- g * y[t] / ln + (1 - g) * st
    } else {
      fit[t] <- l + tr + st
      ln <- a * (y[t] - st) + (1 - a) * (l + tr)
      s[t + m] <- g * (y[t] - ln) + (1 - g) * st
    }
    tr <- b * (ln - l) + (1 - b) * tr
    l <- ln
  }
  n <- length(y)
  ss <- s[n + ((seq_len(h) - 1) %% m) + 1]
  fc <- if (mult) (l + seq_len(h) * tr) * ss else l + seq_len(h) * tr + ss
  list(fit = fit, fc = fc, l = l, tr = tr, s = s[(n + 1):(n + m)])
}

test_that("Holt-Winters additive and multiplicative recursions", {
  y <- c(20, 32, 25, 14, 22, 35, 28, 16, 25, 38, 30, 18, 27, 41)
  ad <- morie_holt_winters_additive(y, period = 4, alpha = 0.4, beta = 0.2, gamma = 0.3, horizon = 6)
  ref <- .hw(y, 4, 0.4, 0.2, 0.3, 6, FALSE)
  expect_equal(ad$fitted, ref$fit, tolerance = 1e-12)
  expect_equal(ad$forecast, ref$fc, tolerance = 1e-12)
  expect_equal(c(ad$level, ad$trend), c(ref$l, ref$tr), tolerance = 1e-12)
  expect_equal(ad$seasonal, ref$s, tolerance = 1e-12)
  expect_equal(ad$sse, sum((y - ref$fit)^2), tolerance = 1e-12)
  mu <- morie_holt_winters_mult(y, period = 4, alpha = 0.4, beta = 0.2, gamma = 0.3)
  rm <- .hw(y, 4, 0.4, 0.2, 0.3, 4, TRUE)
  expect_identical(mu$horizon, 4L)
  expect_equal(mu$fitted, rm$fit, tolerance = 1e-12)
  expect_equal(mu$forecast, rm$fc, tolerance = 1e-12)
  expect_equal(mu$seasonal, rm$s, tolerance = 1e-12)
  expect_error(morie_holt_winters_mult(c(y[-1], 0), period = 4), "strictly positive")
  expect_error(morie_holt_winters_additive(y[1:7], period = 4), "at least 8 observations")
  expect_error(morie_holt_winters_additive(y, period = 1), "at least 2")
  expect_error(morie_holt_winters_additive(y, beta = 1.5), "beta must be in")
})

test_that("Croston and SBA, and the Syntetos-Boylan classes", {
  y <- c(0, 0, 5, 0, 3, 0, 0, 0, 7, 0, 4)
  r <- morie_croston(y, alpha = 0.2)
  z <- 5
  p <- 3
  last <- 3
  for (i in c(5, 9, 11)) {
    z <- 0.2 * y[i] + 0.8 * z
    p <- 0.2 * (i - last) + 0.8 * p
    last <- i
  }
  expect_equal(c(r$demand_size, r$interval), c(z, p), tolerance = 1e-12)
  expect_equal(r$forecast, z / p, tolerance = 1e-12)
  expect_equal(r$intermittency, 1 - 4 / 11, tolerance = 1e-12)
  expect_match(r$warnings, "biased upward")
  s <- morie_croston(y, alpha = 0.2, variant = "sba")
  expect_equal(s$forecast, 0.9 * z / p, tolerance = 1e-12)
  expect_length(s$warnings, 0L)
  j <- morie_joseph_croston_intermittent(y, alpha = 0.2)
  nz <- c(5, 3, 7, 4)
  expect_equal(j$cv_squared, (stats::sd(nz) / mean(nz))^2, tolerance = 1e-12)
  expect_identical(j$classification, if ((stats::sd(nz) / mean(nz))^2 >= 0.49) "lumpy" else "intermittent")
  expect_equal(j$forecast, s$forecast)
  sm <- morie_joseph_croston_intermittent(c(4, 5, 4, 5, 4), variant = "croston")
  expect_identical(sm$classification, "smooth")
  expect_length(sm$warnings, 2L)
  er <- morie_joseph_croston_intermittent(c(1, 9, 1, 9, 1, 9), alpha = 0.5)
  expect_identical(er$classification, "erratic")
  expect_error(morie_croston(c(0, 0)), "no nonzero demand")
  expect_error(morie_croston(c(1, -1)), "non-negative")
  expect_error(morie_croston(1, alpha = 0), "\\(0, 1\\]")
})

test_that("MinT reconciliation is S (S' W^-1 S)^-1 S' W^-1 y_hat", {
  S <- rbind(c(1, 1, 1), c(1, 1, 0), diag(3))
  y <- c(30, 18, 9, 8, 11)
  W <- diag(c(4, 2, 1, 1, 1.5))
  W[1, 2] <- W[2, 1] <- 0.5
  proj <- function(Wm) {
    Wi <- solve(Wm)
    G <- solve(t(S) %*% Wi %*% S, t(S) %*% Wi)
    list(G = G, rec = as.vector(S %*% G %*% y))
  }
  o <- morie_joseph_mint_reconciliation(y, S)
  expect_equal(o$reconciled, proj(diag(5))$rec, tolerance = 1e-10)
  inc <- sqrt(sum(stats::lm.fit(S, y)$residuals^2))
  expect_equal(o$incoherence_before, inc, tolerance = 1e-10)
  expect_true(o$coherent)
  w <- morie_joseph_mint_reconciliation(y, S, W = W, method = "wls")
  expect_equal(w$reconciled, proj(diag(diag(W)))$rec, tolerance = 1e-10)
  m <- morie_joseph_mint_reconciliation(y, S, W = W, method = "mint")
  expect_equal(m$G, proj(W)$G, tolerance = 1e-10)
  expect_equal(m$reconciled, proj(W)$rec, tolerance = 1e-10)
  expect_equal(m$reconciled[1], sum(m$reconciled[3:5]), tolerance = 1e-10)
  expect_equal(m$adjustment, m$reconciled - y, tolerance = 1e-12)
  expect_error(morie_joseph_mint_reconciliation(y[-1], S), "4 entries but S has 5 rows")
  expect_error(morie_joseph_mint_reconciliation(y, S, W = diag(3)), "W must be \\(5, 5\\)")
})
