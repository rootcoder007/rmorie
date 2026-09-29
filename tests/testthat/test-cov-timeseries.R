# Coverage for ARIMA/ARIMAX, Box-Pierce, Baxter-King, ETS and FEVD
# (arimaF.R, arimab.R, arimax.R, bxprc.R, bxprfl.R, etsmod.R, fevdc.R):
# regressions are rebuilt with lm, the portmanteau test with Box.test and
# the recursions from their definitions.

ts_y <- c(1.2, 0.8, 1.9, 2.4, 2.1, 3.0, 2.7, 1.8, 1.1, 0.6, 1.4, 2.2, 2.9, 3.3, 2.5,
          1.6, 0.9, 1.3, 2.0, 2.6, 3.1, 2.8, 2.2, 1.5, 1.0, 1.7, 2.3, 3.0, 2.9, 2.1)

test_that("Arimacss accumulates conditional residuals", {
  r <- Arimacss(ts_y, phi = c(0.5, -0.2), theta = 0.3, d = 1, mu = 0.05)
  w <- diff(ts_y)
  z <- w - 0.05
  e <- numeric(29)
  for (t in 3:29) e[t] <- z[t] - 0.5 * z[t - 1] + 0.2 * z[t - 2] - 0.3 * e[t - 1]
  css <- sum(e[3:29]^2)
  expect_equal(r$css, css, tolerance = 1e-12)
  expect_equal(r$loglik, -0.5 * 27 * (1 + log(2 * pi) + log(css / 27)), tolerance = 1e-12)
  expect_equal(r$aic, -2 * r$loglik + 8, tolerance = 1e-12)
  expect_equal(Arimacss(ts_y)$css, sum(ts_y^2), tolerance = 1e-12)
  expect_error(Arimacss(ts_y, d = -1), "non-negative")
  expect_error(Arimacss(1:2, phi = c(1, 1)), "too short")
})

test_that("Arimahr and Arimaxhr are Hannan-Rissanen two-stage regressions", {
  r <- Arimahr(ts_y, p = 1, q = 1, m = 4)
  n <- 30
  lagm <- function(v, idx, k) sapply(seq_len(k), function(i) v[idx - i])
  i1 <- 5:n
  e1 <- numeric(n)
  e1[i1] <- resid(lm(ts_y[i1] ~ lagm(ts_y, i1, 4)))
  j <- 6:n
  f2 <- lm(ts_y[j] ~ ts_y[j - 1] + e1[j - 1])
  expect_equal(c(r$intercept, r$phi, r$theta), unname(coef(f2)), tolerance = 1e-9)
  expect_equal(r$sigma2, sum(resid(f2)^2) / (length(j) - 3), tolerance = 1e-9)
  expect_equal(Arimahr(ts_y, p = 1, q = 0, d = 1)$d, 1L)
  expect_error(Arimahr(ts_y, p = -1), "non-negative")
  expect_error(Arimahr(ts_y[1:6], p = 2, q = 2), "too short")
  X <- cbind(sin(1:30), cos(1:30 / 2))
  yx <- ts_y + 0.5 * X[, 1]
  ax <- Arimaxhr(yx, X, p = 1, q = 1, m = 4)
  f1 <- lm(yx ~ X)
  nz <- unname(resid(f1))
  ee <- numeric(n)
  ee[i1] <- resid(lm(nz[i1] ~ lagm(nz, i1, 4)))
  fx <- lm(nz[j] ~ nz[j - 1] + ee[j - 1])
  expect_equal(ax$beta, unname(coef(f1)[-1]), tolerance = 1e-9)
  expect_equal(c(ax$phi, ax$theta), unname(coef(fx)[2:3]), tolerance = 1e-9)
  axd <- Arimaxhr(yx, X, p = 1, q = 0, d = 1, m = 3)
  expect_equal(axd$beta, unname(coef(lm(diff(yx) ~ apply(X, 2, diff)))[-1]), tolerance = 1e-9)
  expect_error(Arimaxhr(yx, X[-1, ]), "one row per observation")
  expect_error(Arimaxhr(yx, X, q = -1), "non-negative")
  expect_error(Arimaxhr(yx[1:6], X[1:6, ], p = 2, q = 2), "too short")
})

test_that("Boxpierce matches stats::Box.test", {
  r <- Boxpierce(ts_y, lags = 5, fitdf = 1)
  ref <- Box.test(ts_y, lag = 5, type = "Box-Pierce", fitdf = 1)
  expect_equal(r$statistic, unname(ref$statistic), tolerance = 1e-12)
  # Box.test forms 1 - pchisq(), which cancels at small p; the upper tail
  # is taken directly here
  expect_equal(r$p_value, pchisq(unname(ref$statistic), 4, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(r$p_value, ref$p.value, tolerance = 1e-7)
  expect_true(is.nan(Boxpierce(ts_y, lags = 2, fitdf = 2)$p_value))
  expect_error(Boxpierce(ts_y, lags = 0), "lags")
})

test_that("Bxprfl applies the Baxter-King band-pass weights", {
  r <- Bxprfl(ts_y, p_low = 3, p_high = 10, K = 4)
  wh <- 2 * pi / 3
  wl <- 2 * pi / 10
  b <- c((wh - wl) / pi, (sin((1:4) * wh) - sin((1:4) * wl)) / (pi * (1:4)))
  th <- -(b[1] + 2 * sum(b[-1])) / 9
  a <- c(rev(b[-1]), b) + th
  expect_equal(r$weights, a, tolerance = 1e-12)
  expect_equal(r$weight_sum, 0, tolerance = 1e-12)
  cyc <- vapply(5:26, function(t) sum(a * ts_y[t - (-4:4)]), 0)
  expect_equal(r$cycle[5:26], cyc, tolerance = 1e-12)
  expect_true(all(is.na(r$cycle[c(1:4, 27:30)])))
  expect_equal(r$sd_cycle, sd(cyc), tolerance = 1e-12)
  expect_error(Bxprfl(ts_y[1:5], K = 4), "shorter than")
  expect_error(Bxprfl(ts_y, p_low = 1), "exceed 1")
  expect_error(Bxprfl(ts_y, p_low = 6, p_high = 5), "exceed p_low")
  expect_error(Bxprfl(ts_y, K = 0), "shorter than|at least 1")
})

test_that("Etsmod grid-searches the additive ETS smoothing constants", {
  sse <- function(y, a, b, g, trend, m) {
    n <- length(y)
    if (m > 0) {
      l <- mean(y[1:m])
      bt <- if (trend) (mean(y[(m + 1):(2 * m)]) - l) / m else 0
      s <- y[1:m] - l
    } else {
      l <- y[1]
      bt <- if (trend) y[2] - y[1] else 0
      s <- numeric(0)
    }
    tot <- 0
    for (t in 1:n) {
      k <- if (m > 0) (t - 1) %% m + 1 else 0
      sea <- if (m > 0) s[k] else 0
      e <- y[t] - (l + bt + sea)
      tot <- tot + e^2
      l <- l + bt + a * e
      if (trend) bt <- bt + b * e
      if (m > 0) s[k] <- sea + g * e
    }
    list(sse = tot, l = l, b = bt, s = s)
  }
  g <- (1:9) / 10
  cand <- expand.grid(a = g, b = g, gg = g)
  cand <- cand[cand$b <= cand$a & cand$gg <= 1 - cand$a, ]
  v <- vapply(seq_len(nrow(cand)), function(i)
    sse(ts_y, cand$a[i], cand$b[i], cand$gg[i], TRUE, 4)$sse, 0)
  r <- Etsmod(ts_y, trend = TRUE, season = 4)
  expect_equal(r$sse, min(v), tolerance = 1e-12)
  best <- cand[which.min(v), ]
  expect_equal(c(r$alpha, r$beta, r$gamma), unname(unlist(best)), tolerance = 1e-12)
  fin <- sse(ts_y, best$a, best$b, best$gg, TRUE, 4)
  expect_equal(r$forecast, fin$l + fin$b + fin$s[30 %% 4 + 1], tolerance = 1e-12)
  expect_equal(r$aic, 30 * log(min(v) / 30) + 2 * (1 + 1 + 1 + 1 + 1 + 4), tolerance = 1e-12)
  simple <- Etsmod(ts_y, alpha = 0.3)
  expect_equal(simple$sse, sse(ts_y, 0.3, 0, 0, FALSE, 0)$sse, tolerance = 1e-12)
  expect_error(Etsmod(numeric(0)), "empty")
  expect_error(Etsmod(ts_y, error = "M"), "additive")
  expect_error(Etsmod(ts_y, season = -1), "non-negative")
  expect_error(Etsmod(ts_y[1:5], season = 4), "two full seasons")
})

test_that("Fevdc decomposes VAR(1) forecast-error variance", {
  A <- rbind(c(0.5, 0.1, 0), c(0.2, 0.3, 0.1), c(0, -0.1, 0.4))
  S <- rbind(c(1, 0.3, 0.1), c(0.3, 0.8, 0.2), c(0.1, 0.2, 0.5))
  r <- Fevdc(A, S, periods = 3)
  P <- t(chol(S))
  contrib <- matrix(0, 3, 3)
  Ah <- diag(3)
  for (h in 0:3) {
    contrib <- contrib + (Ah %*% P)^2
    expect_equal(r$mse_contributions[h + 1, , ], contrib, tolerance = 1e-12)
    expect_equal(r$decomposition[h + 1, , ], contrib / rowSums(contrib), tolerance = 1e-12)
    Ah <- Ah %*% A
  }
  expect_equal(rowSums(r$decomposition[4, , ]), rep(1, 3), tolerance = 1e-12)
  expect_error(Fevdc(A[, 1:2], S), "square")
  expect_error(Fevdc(A, S[1:2, 1:2]), "k by k")
  expect_error(Fevdc(A, S, periods = -1), "non-negative")
})
