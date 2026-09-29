# Prophet design pieces and the penalised fit, recomputed from Taylor &
# Letham (2018): g(t) = (k + a(t)'delta) t + (m + a(t)'gamma), gamma = -s delta.

pp_t <- c(0, 1.5, 2, 3.25, 4, 5, 6.5, 7, 8, 9.5)
pp_cps <- c(2, 5)

test_that("morie_prphet_trend_matrix builds [t, 1, (t - s_j)+]", {
  m <- morie_prphet_trend_matrix(pp_t, pp_cps)
  ref <- cbind(pp_t, 1, pmax(pp_t - 2, 0), pmax(pp_t - 5, 0))
  expect_equal(unname(m), unname(ref), tolerance = 1e-12)
  expect_equal(dim(morie_prphet_trend_matrix(pp_t, numeric(0))), c(10L, 2L))
})

test_that("trend_matrix (list form) matches the matrix form row by row", {
  rows <- trend_matrix(pp_t, pp_cps)
  expect_length(rows, length(pp_t))
  expect_equal(do.call(rbind, rows),
               unname(morie_prphet_trend_matrix(pp_t, pp_cps)),
               tolerance = 1e-12)
})

test_that("piecewise trend is continuous and matches the explicit formula", {
  k <- 0.7
  m0 <- -1.2
  d <- c(0.4, -1.1)
  ref <- vapply(pp_t, function(tv) {
    a <- as.numeric(tv >= pp_cps)
    (k + sum(a * d)) * tv + (m0 + sum(a * (-pp_cps * d)))
  }, numeric(1))
  # same curve from the design matrix: [t, 1, (t-s)+] %*% (k, m, delta)
  lin <- as.numeric(morie_prphet_trend_matrix(pp_t, pp_cps) %*% c(k, m0, d))
  got <- morie_prphet_piecewise_trend(pp_t, k, m0, d, pp_cps)
  expect_equal(got, ref, tolerance = 1e-12)
  expect_equal(got, lin, tolerance = 1e-12)
  expect_equal(piecewise_trend(pp_t, k, m0, d, pp_cps), ref, tolerance = 1e-12)
  # continuity at each changepoint
  eps <- 1e-9
  for (s in pp_cps) {
    lr <- morie_prphet_piecewise_trend(c(s - eps, s), k, m0, d, pp_cps)
    expect_lt(abs(lr[2] - lr[1]), 1e-6)
  }
})

test_that("morie_prphet_fourier_terms gives cos/sin pairs and validates", {
  f <- morie_prphet_fourier_terms(pp_t, 7, 2)
  ref <- cbind(cos(2 * pi * pp_t / 7), sin(2 * pi * pp_t / 7),
               cos(4 * pi * pp_t / 7), sin(4 * pi * pp_t / 7))
  expect_equal(f, ref, tolerance = 1e-12)
  expect_error(morie_prphet_fourier_terms(pp_t, 0, 2), "period")
  expect_error(morie_prphet_fourier_terms(pp_t, 7, 0), "order")
  expect_error(fourier_terms(pp_t, -1, 2), "period must be positive, got -1")
})

test_that("holiday matrices flag days inside the window, names sorted", {
  hol <- list(b = c(4), a = c(1.5, 8))
  h <- morie_prphet_holiday_matrix(pp_t, hol)
  expect_identical(h$names, c("a", "b"))
  expect_equal(h$rows[, 1], as.numeric(pp_t %in% c(1.5, 8)))
  expect_equal(h$rows[, 2], as.numeric(pp_t == 4))
  hw <- morie_prphet_holiday_matrix(pp_t, hol, lower = 1, upper = 1.5)
  ref_a <- as.numeric((pp_t >= 0.5 & pp_t <= 3) | (pp_t >= 7 & pp_t <= 9.5))
  ref_b <- as.numeric(pp_t >= 3 & pp_t <= 5.5)
  expect_equal(hw$rows[, 1], ref_a)
  expect_equal(hw$rows[, 2], ref_b)
  hl <- holiday_matrix(pp_t, hol, lower = 1, upper = 1.5)
  expect_identical(hl$names, c("a", "b"))
  expect_equal(do.call(rbind, hl$matrix), unname(hw$rows))
})

test_that("design and prophet_design assemble trend, seasonality, holidays", {
  seas <- list(list("wk", 7, 2))
  hol <- list(x = c(4))
  d <- morie_prphet_design(pp_t, pp_cps, seas, hol)
  ref <- cbind(morie_prphet_trend_matrix(pp_t, pp_cps),
               morie_prphet_fourier_terms(pp_t, 7, 2),
               as.numeric(pp_t == 4))
  expect_equal(unname(d$X), unname(ref), tolerance = 1e-12)
  expect_identical(d$cols, c("k", "m", "delta_0", "delta_1", "wk_cos1",
                             "wk_sin1", "wk_cos2", "wk_sin2", "holiday_x"))
  expect_identical(d$holiday.names, "x")
  d0 <- morie_prphet_design(pp_t, pp_cps)
  expect_identical(d0$cols, c("k", "m", "delta_0", "delta_1"))
  pd <- prophet_design(pp_t, pp_cps, seas, hol)
  expect_identical(pd$cols, d$cols)
  expect_equal(do.call(rbind, pd$X), unname(ref), tolerance = 1e-12)
})

pp_series <- function() {
  tt <- seq(0, 29)
  k <- 0.5
  d <- c(0, 1.5)
  cps <- c(10, 20)
  y <- morie_prphet_piecewise_trend(tt, k, 2, d, cps) +
    1.3 * sin(2 * pi * tt / 7) + 0.1 * cos(1.7 * tt)
  list(t = tt, y = y, cps = cps)
}

test_that("morie_prphet_fit satisfies the lasso KKT conditions", {
  s <- pp_series()
  seas <- list(list("wk", 7, 1))
  tau <- 0.05
  fit <- morie_prphet_fit(s$t, s$y, changepoints = s$cps, seasonalities = seas,
                          changepoint_prior = tau, ridge = 0)
  X <- morie_prphet_design(s$t, s$cps, seas)$X
  g <- as.numeric(crossprod(X, s$y - X %*% fit$beta))
  pen <- ifelse(grepl("^delta_", fit$columns), 1 / tau, 0)
  free <- pen == 0
  expect_lt(max(abs(g[free])), 1e-6)
  act <- !free & fit$beta != 0
  expect_equal(g[act], pen[act] * sign(fit$beta[act]), tolerance = 1e-6)
  expect_true(all(abs(g[!free & fit$beta == 0]) <= pen[!free & fit$beta == 0] + 1e-9))
  expect_equal(fit$fitted, as.numeric(X %*% fit$beta), tolerance = 1e-12)
  expect_equal(fit$residual, s$y - fit$fitted, tolerance = 1e-12)
  expect_equal(fit$sigma, sqrt(sum(fit$residual^2) / (30 - 6)), tolerance = 1e-12)
  expect_equal(fit$trend, morie_prphet_piecewise_trend(s$t, fit$k, fit$m,
                                                       fit$deltas, s$cps),
               tolerance = 1e-12)
  expect_identical(fit$n.active.changepoints, sum(fit$deltas != 0))
})

test_that("morie_prphet_fit with a vanishing penalty equals least squares", {
  s <- pp_series()
  seas <- list(list("wk", 7, 1))
  # well-conditioned case (time scaled to [0, 1], one changepoint) so the
  # 400-sweep coordinate descent reaches the least-squares optimum
  ts <- s$t / 29
  fit <- morie_prphet_fit(ts, s$y, changepoints = 0.5, changepoint_prior = 1e12,
                          ridge = 0)
  X <- morie_prphet_design(ts, 0.5)$X
  ols <- qr.solve(X, s$y)
  expect_equal(fit$beta, as.numeric(ols), tolerance = 1e-9)
  # default changepoint schedule: evenly spaced in the first 80 percent
  f2 <- morie_prphet_fit(s$t, s$y, n_changepoints = 3)
  expect_equal(f2$changepoints, 0 + (0.8 * 29) / 4 * 1:3, tolerance = 1e-12)
})

test_that("morie_prphet_fit validates its inputs", {
  expect_error(morie_prphet_fit(1:10, 1:9), "observations")
  expect_error(morie_prphet_fit(1:5, 1:5), "at least 8")
  expect_error(morie_prphet_fit(1:10, 1:10, changepoint_prior = 0), "positive")
})

test_that("predict functions evaluate the design at new times", {
  s <- pp_series()
  seas <- list(list("wk", 7, 1))
  hol <- list(h = c(3, 17))
  fit <- morie_prphet_fit(s$t, s$y, changepoints = s$cps, seasonalities = seas,
                          holidays = hol)
  tn <- c(30, 31.5, 17)
  Xn <- morie_prphet_design(tn, s$cps, seas, hol)$X
  ref <- as.numeric(Xn %*% fit$beta)
  expect_equal(morie_prphet_predict(fit, tn, seas, hol), ref, tolerance = 1e-12)
  expect_equal(prophet_predict(fit, tn, seas, hol), ref, tolerance = 1e-12)
  expect_error(morie_prphet_predict(fit, tn), "does not match")
  expect_error(prophet_predict(fit, tn, seas), "does not match")
})

test_that("prphet_cheatsheet is a single string naming the trend model", {
  cs <- prphet_cheatsheet()
  expect_type(cs, "character")
  expect_length(cs, 1L)
  expect_match(cs, "gamma_j = -s_j delta_j", fixed = TRUE)
})
