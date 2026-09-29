# Coverage for the Prophet decomposable model (Taylor & Letham 2018). The
# trend, Fourier and holiday blocks are rebuilt from eqs (1)-(4), the
# L1-penalised coordinate-descent fit is checked through its KKT
# conditions, and the restored list-of-rows helpers against the matrix
# versions.

test_that("piecewise trend is continuous and equals k t + m + sum delta_j (t - s_j)+", {
  tt <- seq(0, 10, by = 0.5)
  cps <- c(2, 6.5)
  d <- c(0.8, -1.5)
  g <- morie_prphet_piecewise_trend(tt, 0.3, 1, d, cps)
  expect_equal(g, 0.3 * tt + 1 + d[1] * pmax(tt - 2, 0) + d[2] * pmax(tt - 6.5, 0), tolerance = 1e-12)
  eps <- 1e-9
  for (s in cps) {
    expect_equal(morie_prphet_piecewise_trend(s - eps, 0.3, 1, d, cps), morie_prphet_piecewise_trend(s + eps, 0.3, 1, d, cps), tolerance = 1e-8)
  }
  expect_equal(piecewise_trend(tt, 0.3, 1, d, cps), g, tolerance = 1e-12)
  expect_identical(morie_prphet, morie_prphet_piecewise_trend)
  tm <- morie_prphet_trend_matrix(tt, cps)
  expect_equal(tm, cbind(tt, 1, pmax(tt - 2, 0), pmax(tt - 6.5, 0)), ignore_attr = TRUE)
  expect_equal(do.call(rbind, trend_matrix(tt, cps)), tm, ignore_attr = TRUE)
})

test_that("Fourier seasonality and holiday indicators", {
  tt <- c(0, 1.5, 3, 7.25)
  f <- morie_prphet_fourier_terms(tt, 7, 2)
  expect_equal(f, cbind(cos(2 * pi * tt / 7), sin(2 * pi * tt / 7), cos(4 * pi * tt / 7), sin(4 * pi * tt / 7)), ignore_attr = TRUE)
  expect_error(morie_prphet_fourier_terms(tt, 0, 1), "period must be positive")
  expect_error(morie_prphet_fourier_terms(tt, 7, 0), "order must be at least 1")
  hol <- list(xmas = c(3, 10), eid = 7)
  h <- morie_prphet_holiday_matrix(tt, hol, lower = 0.5, upper = 0.5)
  expect_identical(h$names, c("eid", "xmas"))
  expect_equal(h$rows, cbind(as.numeric(abs(tt - 7) <= 0.5), as.numeric(abs(tt - 3) <= 0.5 | abs(tt - 10) <= 0.5)))
  hl <- holiday_matrix(tt, hol, 0.5, 0.5)
  expect_equal(do.call(rbind, hl$matrix), h$rows)
})

test_that("the design stacks trend, seasonality and holidays with their names", {
  tt <- seq(0, 20, by = 1)
  seas <- list(list("weekly", 7, 2))
  hol <- list(h1 = c(5, 12))
  d <- morie_prphet_design(tt, c(8, 14), seas, hol, c(0, 1))
  expect_identical(d$cols, c("k", "m", "delta_0", "delta_1", "weekly_cos1", "weekly_sin1", "weekly_cos2", "weekly_sin2", "holiday_h1"))
  ref <- cbind(morie_prphet_trend_matrix(tt, c(8, 14)), morie_prphet_fourier_terms(tt, 7, 2),
               as.numeric((tt >= 5 & tt <= 6) | (tt >= 12 & tt <= 13)))
  expect_equal(d$X, ref, ignore_attr = TRUE)
  pd <- prophet_design(tt, c(8, 14), seas, hol, c(0, 1))
  expect_identical(pd$cols, d$cols)
  expect_equal(do.call(rbind, pd$X), d$X, ignore_attr = TRUE)
  expect_error(prophet_design(tt, 8, list(list("w", 0, 1))), "period must be positive")
})

test_that("the penalised fit satisfies its KKT conditions and predicts from its design", {
  tt <- seq(0, 40, by = 1)
  y <- 2 + 0.5 * tt - 0.9 * pmax(tt - 15, 0) + 1.2 * sin(2 * pi * tt / 7) + 0.3 * cos(1.3 * tt)
  seas <- list(list("weekly", 7, 1))
  tau <- 0.5
  fit <- morie_prphet_fit(tt, y, n_changepoints = 5, seasonalities = seas, changepoint_prior = tau)
  step <- 0.8 * 40 / 6
  expect_equal(fit$changepoints, step * 1:5, tolerance = 1e-12)
  X <- morie_prphet_design(tt, fit$changepoints, seas)$X
  expect_equal(fit$fitted, as.numeric(X %*% fit$beta), tolerance = 1e-12)
  expect_equal(fit$residual, y - fit$fitted, tolerance = 1e-12)
  # KKT of 0.5 ||y - X b||^2 + (1/tau) sum |delta| (+ 1e-8 ridge). With a
  # strong prior every delta is held at zero and the unpenalised columns
  # must be orthogonal to the residual, each delta inside the 1/tau band
  strong <- morie_prphet_fit(tt, y, n_changepoints = 5, seasonalities = seas, changepoint_prior = 1e-4)
  gs <- as.numeric(crossprod(X, y - X %*% strong$beta)) - 1e-8 * strong$beta
  pen <- grepl("^delta_", strong$columns)
  expect_true(all(strong$deltas == 0))
  expect_lt(max(abs(gs[!pen])), 1e-6)
  expect_true(all(abs(gs[pen]) <= 1e4))
  ols <- stats::lm.fit(X[, !pen], y)$coefficients
  expect_equal(unname(strong$beta[!pen]), unname(ols), tolerance = 1e-9)
  expect_identical(fit$n.active.changepoints, sum(fit$deltas != 0))
  expect_equal(fit$trend, morie_prphet_piecewise_trend(tt, fit$k, fit$m, fit$deltas, fit$changepoints), tolerance = 1e-12)
  expect_equal(fit$sigma, sqrt(sum(fit$residual^2) / (41 - 9)), tolerance = 1e-12)
  expect_equal(morie_prphet_predict(fit, tt, seasonalities = seas), fit$fitted, tolerance = 1e-12)
  tn <- c(41, 45.5)
  Xn <- morie_prphet_design(tn, fit$changepoints, seas)$X
  expect_equal(morie_prphet_predict(fit, tn, seasonalities = seas), as.numeric(Xn %*% fit$beta), tolerance = 1e-12)
  expect_equal(prophet_predict(fit, tn, seasonalities = seas), as.numeric(Xn %*% fit$beta), tolerance = 1e-12)
  expect_error(morie_prphet_predict(fit, tn), "does not match the fitted one")
  expect_identical(prophet, morie_prphet_fit)
  expect_identical(prophetfit, morie_prphet_fit)
  fc <- morie_prphet_fit(tt, y, changepoints = c(15), changepoint_prior = 1e6)
  expect_equal(fc$changepoints, 15)
  expect_error(morie_prphet_fit(1:5, 1:5), "at least 8 observations")
  expect_error(morie_prphet_fit(tt, y, changepoint_prior = 0), "must be positive")
  expect_match(prphet_cheatsheet(), "gamma_j = -s_j")
})
