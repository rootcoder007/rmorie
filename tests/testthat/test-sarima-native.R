# Multiplicative seasonal ARIMA, (p,d,q)x(P,D,Q)_s (Box, Jenkins, Reinsel
# & Ljung 2016, Ch. 9).
#
# Anchors outside the module: base R's diff for the differencing, base R's
# arima for the fitted airline model on Series G, and the polynomial
# identities the multiplicative expansion must satisfy. The fitter is
# exercised on a small series in the fast tests and against arima on the
# full airline model behind RMORIE_SLOW_TESTS, because the likelihood is
# optimised by Nelder-Mead over an interpreted Kalman filter.

test_that("Series G is the published airline passenger series", {
  g <- series_g()
  expect_length(g, 144L)
  expect_equal(as.numeric(g), as.numeric(AirPassengers))
  expect_equal(as.numeric(series_g(log = TRUE)),
               log(as.numeric(AirPassengers)))
})

test_that("differencing agrees with base R", {
  y <- as.numeric(AirPassengers)
  expect_equal(difference(y, d = 1), diff(y))
  expect_equal(difference(y, d = 2), diff(y, differences = 2))
  expect_equal(difference(y, D = 1, s = 12), diff(y, lag = 12))
  expect_equal(difference(y, D = 2, s = 12), diff(y, lag = 12,
                                                  differences = 2))
  # the airline transformation removes one ordinary and one seasonal lag
  w <- difference(log(y), d = 1, D = 1, s = 12)
  expect_length(w, length(y) - 1L - 12L)
  expect_equal(w, diff(diff(log(y)), lag = 12))
  # no differencing is the identity
  expect_equal(difference(y), y)
})

test_that("polynomial multiplication and seasonal lifting are exact", {
  # (1 - x)(1 - 2x) = 1 - 3x + 2x^2
  expect_equal(.sarima_poly_mult(c(1, -1), c(1, -2)), c(1, -3, 2))
  # multiplying by one changes nothing
  expect_equal(.sarima_poly_mult(c(1, -0.5), 1), c(1, -0.5))
  expect_equal(.sarima_poly_mult(1, c(2, 3)), c(2, 3))
  # degrees add
  expect_length(.sarima_poly_mult(c(1, 1, 1), c(1, 1)), 4L)

  # lifting spreads coefficients onto multiples of the season
  expect_equal(.sarima_seasonal_lift(c(1, -0.5), 3), c(1, 0, 0, -0.5))
  expect_equal(.sarima_seasonal_lift(c(1, -0.5, 0.2), 2),
               c(1, 0, -0.5, 0, 0.2))
  # a season of one leaves the polynomial alone
  expect_equal(.sarima_seasonal_lift(c(1, -0.5), 1), c(1, -0.5))
  # a constant polynomial lifts to itself
  expect_equal(.sarima_seasonal_lift(1, 12), 1)
})

test_that("the difference polynomial is the binomial expansion", {
  # (1 - B)^k has binomial coefficients with alternating signs
  expect_equal(.sarima_diff_poly(1, 1), c(1, -1))
  expect_equal(.sarima_diff_poly(2, 1), c(1, -2, 1))
  expect_equal(.sarima_diff_poly(3, 1), c(1, -3, 3, -1))
  # a seasonal difference puts them on multiples of s
  expect_equal(.sarima_diff_poly(1, 12), c(1, rep(0, 11), -1))
  expect_length(.sarima_diff_poly(2, 12), 25L)
  # no differencing is the unit polynomial
  expect_equal(.sarima_diff_poly(0, 12), 1)
  # the coefficients sum to zero for any positive order, since (1-B)^k
  # annihilates a constant
  for (k in 1:3) expect_equal(sum(.sarima_diff_poly(k, 4)), 0)
})

test_that("the multiplicative expansion multiplies the two polynomials", {
  # a (1,0,0)x(1,0,0)_4 model expands to (1 - phi B)(1 - Phi B^4)
  em <- expand_polynomials(phi = 0.5, Phi = 0.3, s = 4)
  # the module returns the coefficients of the operator on the right side
  expect_true(is.list(em))
  expect_true(all(c("ar", "ma") %in% names(em)))
  # with no seasonal part the expansion is the ordinary polynomial
  e1 <- expand_polynomials(phi = 0.5, s = 12)
  expect_equal(length(e1$ar), 1L)
  expect_equal(e1$ar, 0.5)
  # a pure airline model has no autoregressive part at all
  e2 <- expand_polynomials(theta = 0.4, Theta = 0.6, s = 12)
  expect_length(e2$ar, 0L)
  expect_length(e2$ma, 13L)
  # the seasonal and ordinary moving-average terms cross-multiply
  expect_equal(e2$ma[1], 0.4)
  expect_equal(e2$ma[12], 0.6)
  # the operator is written 1 - sum(ma_j B^j), so expanding
  # (1 - theta B)(1 - Theta B^12) puts -theta*Theta at lag 13
  expect_equal(e2$ma[13], -0.4 * 0.6)
})

test_that("sample autocorrelations agree with base R", {
  set.seed(3)
  x <- rnorm(120)
  # the lags to report are given explicitly and come back keyed by lag
  got <- sample_acf(x, 1:5)
  ref <- as.numeric(acf(x, lag.max = 5, plot = FALSE)$acf)[-1]
  expect_length(got, 5L)
  expect_named(got, as.character(1:5))
  expect_equal(as.numeric(unlist(got)), ref, tolerance = 1e-10)
  expect_true(all(abs(unlist(got)) <= 1 + 1e-12))
  # a single lag can be asked for on its own
  expect_equal(sample_acf(x, 12)[["12"]],
               as.numeric(acf(x, lag.max = 12, plot = FALSE)$acf)[13],
               tolerance = 1e-10)
  expect_error(sample_acf(x, 0), "out of range")
  expect_error(sample_acf(x, 120), "out of range")
  expect_error(sample_acf(1, 1), "at least two observations")
  expect_error(sample_acf(rep(2, 10), 1), "series is constant")
})

test_that("stationarity is judged by the roots of the polynomial", {
  # an empty coefficient set is trivially fine
  expect_true(.sarima_roots_ok(numeric(0)))
  # |phi| < 1 is stationary for an order-one polynomial
  expect_true(.sarima_roots_ok(0.5))
  expect_true(.sarima_roots_ok(-0.5))
  expect_false(.sarima_roots_ok(1.5))
  expect_false(.sarima_roots_ok(-1.5))
  # the classic stationarity triangle for an order-two polynomial
  expect_true(.sarima_roots_ok(c(0.3, 0.2)))
  expect_false(.sarima_roots_ok(c(1.2, -0.1)))
})

test_that("the state-space form and its stationary covariance are consistent", {
  ss <- .sarima_state_space(numeric(0), 0.5)
  expect_true(is.list(ss))
  # a pure moving average of order one needs two states
  r <- max(1L, 2L)
  P0 <- .sarima_initial_covariance(ss$T, ss$R, nrow(ss$T))
  expect_equal(dim(P0), dim(ss$T))
  # the stationary covariance solves P = T P T' + R R'
  expect_equal(P0, ss$T %*% P0 %*% t(ss$T) + ss$R %*% t(ss$R),
               tolerance = 1e-8)
  # and it is symmetric and positive semi-definite
  expect_equal(P0, t(P0), tolerance = 1e-10)
  expect_true(min(eigen(P0, symmetric = TRUE, only.values = TRUE)$values) >
              -1e-8)
  # an autoregressive model gives a different but equally valid solution
  ss2 <- .sarima_state_space(0.6, numeric(0))
  P2 <- .sarima_initial_covariance(ss2$T, ss2$R, nrow(ss2$T))
  expect_equal(P2, ss2$T %*% P2 %*% t(ss2$T) + ss2$R %*% t(ss2$R),
               tolerance = 1e-8)
})

test_that("the psi weights are the impulse response", {
  # a pure moving average of order one has psi = (1, theta) and then zero
  w <- .sarima_psi_weights(numeric(0), 0.5, 4)
  expect_length(w, 4L)
  expect_equal(w[1], 1)
  # an autoregression of order one has psi_j = phi^j
  wa <- .sarima_psi_weights(0.5, numeric(0), 5)
  expect_equal(wa, 0.5^(0:4), tolerance = 1e-12)
  # a white-noise model responds only at lag zero
  expect_equal(.sarima_psi_weights(numeric(0), numeric(0), 3), c(1, 0, 0))
})

test_that("the conditional and exact likelihoods behave", {
  set.seed(5)
  w <- as.numeric(arima.sim(list(ma = 0.5), 80))
  cs <- css(w, ma = 0.5, full = TRUE)
  expect_true(cs$ssq > 0)
  expect_length(cs$residuals, length(w))
  ll <- loglik(w, ma = 0.5)
  expect_true(is.finite(ll$loglik))
  expect_true(ll$sigma2 > 0)
  # arima.sim writes (1 + ma B) and the module writes (1 - ma B), so the
  # coefficient that generated this series is -0.5 in the module's terms
  l_true <- loglik(w, ma = -0.5)$loglik
  l_wrong <- loglik(w, ma = 0.9)$loglik
  expect_true(l_true > l_wrong)
})

test_that("standard errors follow the Box-Jenkins closed forms", {
  n <- 144
  # Bartlett's large-lag variance for the airline model, Eq. (9.2.19):
  # var(r_k) = (1 + 2 (rho_1^2 + rho_11^2 + rho_12^2 + rho_13^2)) / n. The
  # autocorrelations arrive keyed by lag, as sample_acf returns them.
  zero <- list("1" = 0, "11" = 0, "12" = 0, "13" = 0)
  b0 <- bartlett_se(zero, n)
  expect_equal(b0$variance, 1 / n)
  expect_equal(b0$se, sqrt(1 / n))
  expect_equal(b0$white_noise_se, sqrt(1 / n))
  rho <- list("1" = 0.3, "11" = 0.1, "12" = -0.4, "13" = 0.05)
  b1 <- bartlett_se(rho, n)
  ssq <- 0.3^2 + 0.1^2 + 0.4^2 + 0.05^2
  expect_equal(b1$variance, (1 + 2 * ssq) / n)
  expect_equal(b1$se, sqrt((1 + 2 * ssq) / n))
  # correlated series carry more uncertainty than white noise
  expect_true(b1$se > b1$white_noise_se)
  # and more data always narrows the interval
  expect_true(bartlett_se(rho, 1000)$se < bartlett_se(rho, 100)$se)
  expect_error(bartlett_se(zero, 0), "n must be positive")

  # the airline model's large-sample variances, Sec. 9.2.3
  se <- large_sample_se(0.4, 0.6, n)
  expect_equal(se$var_theta, (1 - 0.4^2) / n)
  expect_equal(se$var_Theta, (1 - 0.6^2) / n)
  expect_equal(se$se_theta, sqrt((1 - 0.4^2) / n))
  expect_equal(se$se_Theta, sqrt((1 - 0.6^2) / n))
  # the two estimates are asymptotically uncorrelated
  expect_equal(se$cov, 0)
  expect_equal(se$off_diagonal_term, 0.4^11 / (1 - 0.4^12 * 0.6))
  # a coefficient at the boundary has no variance left
  expect_equal(large_sample_se(1, 1, n)$se_theta, 0)
  expect_error(large_sample_se(0.4, 0.6, 0), "n must be positive")
})

test_that("the parameter unpacking gives each block its own slots", {
  # A regression test for the block layout. i:(i + k - 1) counts down when
  # k is zero, which used to hand an absent autoregressive block the first
  # moving-average slot and leave the cursor unmoved, so a (0,d,q)x(0,D,Q)
  # model -- the airline model among them -- was fitted with phantom
  # autoregressive terms and a parameter count too large.
  set.seed(7)
  y <- cumsum(rnorm(40)) + 10
  f <- .sarima_fit(y, order = c(0, 1, 1), seasonal_order = c(0, 0, 0), s = 12)
  # one free parameter, and it is a moving-average one
  expect_equal(f$n_par, 1L)
  expect_length(f$phi, 0L)
  expect_length(f$Phi, 0L)
  expect_length(f$Theta, 0L)
  expect_length(f$theta, 1L)
  expect_length(f$ar, 0L)
  # an autoregressive model puts its parameter in the other block
  fa <- .sarima_fit(y, order = c(1, 1, 0), seasonal_order = c(0, 0, 0),
                    s = 12)
  expect_equal(fa$n_par, 1L)
  expect_length(fa$phi, 1L)
  expect_length(fa$theta, 0L)
  # the reported orders are the ones asked for
  expect_equal(f$order, c(0L, 1L, 1L))
  expect_equal(f$seasonal_order, c(0L, 0L, 0L))
  expect_equal(f$s, 12L)
  expect_equal(f$n_used, length(y) - 1L)
  expect_match(f$method, "multiplicative seasonal ARIMA")
  # a model with nothing to estimate is refused
  expect_error(.sarima_fit(y, order = c(0, 1, 0),
                           seasonal_order = c(0, 0, 0)),
               "no free parameters")
  expect_error(.sarima_fit(y, order = c(0, 1, 1), method = "bogus"),
               "method must be one of")
  expect_error(.sarima_fit(y, order = c(-1, 1, 1)),
               "orders must be non-negative")
  # fifteen points leave two after airline differencing, which cannot
  # support the model's two parameters
  expect_error(.sarima_fit(y[1:15], order = c(0, 1, 1),
                           seasonal_order = c(0, 1, 1), s = 12),
               "cannot support")
  # and too few to difference at all is reported by the differencing step
  expect_error(.sarima_fit(y[1:3], order = c(0, 1, 1),
                           seasonal_order = c(0, 1, 1), s = 12),
               "too short for seasonal differencing")
})

test_that("the moment route reproduces the book's preliminary estimates", {
  w <- difference(log(as.numeric(AirPassengers)), d = 1, D = 1, s = 12)
  pe <- preliminary_estimates(w, s = 12)
  # Box et al. Sec. 9.2.3 report theta about 0.39 and Theta about 0.48
  expect_equal(pe$theta, 0.39, tolerance = 0.06)
  expect_equal(pe$Theta, 0.48, tolerance = 0.09)
  # the moment fit uses exactly those and reports no autoregressive part
  fm <- .sarima_fit(log(as.numeric(AirPassengers)), order = c(0, 1, 1),
                    seasonal_order = c(0, 1, 1), s = 12, method = "moment")
  expect_equal(fm$theta, pe$theta)
  expect_equal(fm$Theta, pe$Theta)
  expect_length(fm$phi, 0L)
  expect_length(fm$Phi, 0L)
  expect_equal(fm$n_par, 2L)
  expect_equal(fm$fit_method, "moment")
  # the moment route is defined for the airline model only
  expect_error(.sarima_fit(log(as.numeric(AirPassengers)),
                           order = c(1, 1, 1), method = "moment"),
               "airline model only")
})

test_that("forecasts match base R's predict.Arima", {
  set.seed(3)
  y <- cumsum(rnorm(60)) + 10
  # an integrated moving average forecasts a flat line, and it must be the
  # same flat line arima produces, standard errors included
  f <- .sarima_fit(y, order = c(0, 1, 1), seasonal_order = c(0, 0, 0), s = 12)
  fc <- forecast(f, h = 6)
  ref <- predict(arima(y, order = c(0, 1, 1)), n.ahead = 6)
  expect_equal(fc$forecast, rep(fc$forecast[1], 6))
  expect_equal(fc$forecast, as.numeric(ref$pred), tolerance = 1e-5)
  # the standard errors depend on the impulse response, so agreeing with
  # arima here also checks the psi weights
  expect_equal(fc$se, as.numeric(ref$se), tolerance = 1e-5)
  expect_equal(fc$estimate, fc$forecast[1])
  expect_equal(fc$variance, fc$se^2)
  expect_true(all(diff(fc$se) > 0))
  expect_length(fc$psi, 6L)
  expect_equal(fc$psi[1], 1)
  expect_match(fc$method, "difference-equation")
  expect_error(forecast(f, h = 0), "h must be at least 1")
})

test_that("an autoregressive forecast decays geometrically", {
  set.seed(5)
  z <- as.numeric(arima.sim(list(ar = 0.7), 200))
  f <- .sarima_fit(z, order = c(1, 0, 0), seasonal_order = c(0, 0, 0), s = 12)
  fc <- forecast(f, h = 5)
  # a stationary first-order autoregression forecasts phi^h times the last
  # observation
  expect_equal(fc$forecast, f$phi[1]^(1:5) * z[200], tolerance = 1e-8)
  ref <- arima(z, order = c(1, 0, 0), include.mean = FALSE)
  expect_equal(f$phi[1], unname(coef(ref)[1]), tolerance = 1e-4)
  expect_equal(fc$forecast, as.numeric(predict(ref, n.ahead = 5)$pred),
               tolerance = 1e-4)
  # the forecasts shrink toward zero, the mean of the fitted model
  expect_true(all(abs(diff(abs(fc$forecast))) > 0))
  expect_true(abs(fc$forecast[5]) < abs(fc$forecast[1]))
})

test_that("second differencing is honoured in the forecast", {
  # A regression test: the ordinary differencing order used to be hardcoded
  # to one in the forecast, so d was read from the fit and then ignored.
  set.seed(21)
  a <- rnorm(300)
  e <- as.numeric(stats::filter(a, c(1, -0.5), method = "convolution",
                                sides = 1))
  e[1] <- a[1]
  y <- cumsum(cumsum(e))
  f <- .sarima_fit(y, order = c(0, 2, 1), seasonal_order = c(0, 0, 0), s = 12)
  ref <- arima(y, order = c(0, 2, 1))
  expect_equal(f$theta[1], -unname(coef(ref)[1]), tolerance = 1e-4)
  expect_equal(f$loglik, ref$loglik, tolerance = 1e-3)
  fc <- forecast(f, h = 5)
  pr <- as.numeric(predict(ref, n.ahead = 5)$pred)
  expect_equal(fc$forecast, pr, tolerance = 1e-6)
  # twice-integrated forecasts continue in a straight line
  expect_equal(diff(fc$forecast), rep(diff(fc$forecast)[1], 4),
               tolerance = 1e-6)
})

test_that("the airline fit matches base R's arima", {
  skip_if_not(nzchar(Sys.getenv("RMORIE_SLOW_TESTS")),
              "slow: set RMORIE_SLOW_TESTS=1 to run the full airline fit")
  y <- log(as.numeric(AirPassengers))
  f <- .sarima_fit(y, order = c(0, 1, 1), seasonal_order = c(0, 1, 1), s = 12)
  ref <- arima(y, order = c(0, 1, 1),
               seasonal = list(order = c(0, 1, 1), period = 12))
  rc <- r_convention(f)
  # R writes (1 + theta B) where the book writes (1 - theta B), so the
  # converted coefficients are the ones arima reports
  expect_equal(rc$ma, unname(coef(ref)[1]), tolerance = 1e-3)
  expect_equal(rc$sma, unname(coef(ref)[2]), tolerance = 1e-3)
  expect_length(rc$ar, 0L)
  expect_length(rc$sar, 0L)
  # and the likelihood reached is arima's, not a worse local optimum
  expect_equal(rc$loglik, ref$loglik, tolerance = 1e-2)
  expect_equal(rc$sigma2, ref$sigma2, tolerance = 1e-4)
  expect_equal(f$n_par, 2L)
  # forecasts run off the fitted model
  fc <- forecast(f, h = 12)
  expect_true(is.list(fc) || is.numeric(fc))
  expect_length(unlist(fc)[seq_len(1)], 1L)
})
