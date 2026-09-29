# Coverage for SampVar .. sarsa exports (spatial autoregressions, airline
# model, regression with ARIMA errors, SARSA). Every expectation is
# recomputed in the test body.

cov_s2_lattice <- function() {
  xy <- expand.grid(1:4, 1:4)
  D <- as.matrix(stats::dist(xy))
  W <- (D > 0 & D <= 1) * 1
  W / rowSums(W)
}

test_that("SampVar gives the n - 1 and n divisors", {
  x <- c(2, 4, 4, 5, 7, 9)
  r <- SampVar(x)
  expect_equal(r$sample_variance, stats::var(x), tolerance = 1e-12)
  expect_equal(r$population_variance, stats::var(x) * 5 / 6, tolerance = 1e-12)
  expect_error(SampVar(1), "n >= 2")
})

test_that("SAR lag, SAR error and SARAR maximise the spatialreg likelihoods", {
  skip_if_not_installed("spatialreg")
  skip_if_not_installed("spdep")
  W <- cov_s2_lattice()
  n <- 16
  x <- sin(1:n) + 0.1 * (1:n)
  e <- cos(3 * (1:n)) * 0.3
  y <- solve(diag(n) - 0.35 * W, 1 + 0.8 * x + e)
  lw <- spdep::mat2listw(W, style = "W")
  dat <- data.frame(y = y, x = x)
  la <- Sarlag(y, cbind(1, x), W)
  fl <- spatialreg::lagsarlm(y ~ x, data = dat, listw = lw)
  # both optimise the same concentrated likelihood; optimize() stops at 1e-10
  expect_equal(la$rho, unname(fl$rho), tolerance = 1e-6)
  expect_equal(la$estimate, unname(stats::coef(fl)[-1]), tolerance = 1e-6)
  expect_same_function(morie_spatial_ar_lag_model, Sarlag)
  ye <- 1 + 0.8 * x + solve(diag(n) - 0.4 * W, e)
  er <- Sarerr(ye, cbind(1, x), W)
  fe <- spatialreg::errorsarlm(y ~ x, data = data.frame(y = ye, x = x), listw = lw)
  expect_equal(er$lambda, unname(fe$lambda), tolerance = 1e-6)
  expect_equal(er$estimate, unname(stats::coef(fe)[-1]), tolerance = 1e-6)
  expect_same_function(morie_spatial_ar_error_model, Sarerr)
  mx <- Sarmix(y, cbind(1, x), W, W)
  expect_same_function(morie_spatial_ar_combined, Sarmix)
  fs <- spatialreg::sacsarlm(y ~ x, data = dat, listw = lw)
  expect_equal(mx$loglik, as.numeric(fs$LL), tolerance = 1e-6)
  A <- diag(n) - mx$rho * W
  B <- diag(n) - mx$lambda * W
  Xs <- B %*% cbind(1, x)
  expect_equal(mx$estimate, as.numeric(solve(crossprod(Xs), crossprod(Xs, B %*% A %*% y))), tolerance = 1e-10)
  expect_error(Sarmix(y[-1], cbind(1, x), W, W), "shape mismatch")
})

test_that("SarTobitGibbs without censoring is the SAR-lag Gibbs sampler", {
  W <- cov_s2_lattice()
  n <- 16
  x <- sin(1:n) + 0.1 * (1:n)
  y <- as.numeric(solve(diag(n) - 0.3 * W, 2 + 0.8 * x + cos(3 * (1:n)) * 0.3))
  X <- cbind(1, x)
  t1 <- SarTobitGibbs(y, X, W, ndraw = 30, burn_in = 10, seed = 5)
  g1 <- SpatialBayesGibbs(y, X, W, model = "lag", ndraw = 30, burn_in = 10, seed = 5)
  expect_equal(t1$beta_draws, g1$beta_draws)
  expect_equal(t1$rho_draws, g1$rho_draws)
  expect_equal(t1$beta, colMeans(t1$beta_draws), tolerance = 1e-12)
  expect_equal(t1$rho_sd, stats::sd(t1$rho_draws), tolerance = 1e-12)
  # censored outcomes are imputed below zero, the rest are kept
  yc <- pmax(y - 2.5, 0)
  t2 <- SarTobitGibbs(yc, X, W, ndraw = 30, burn_in = 10, seed = 5)
  expect_true(all(abs(t2$rho_draws) < 1))
  expect_equal(dim(t2$beta_draws), c(30L, 2L))
  expect_identical(SarTobitGibbs(yc, X, W, ndraw = 30, burn_in = 10, seed = 5)$rho_draws, t2$rho_draws)
})

test_that("airline-model autocorrelations and the MA(1) moment inversion", {
  th <- 0.4
  TH <- 0.6
  r <- airline_autocovariances(th, TH, sigma2 = 2)
  # (1 - th B)(1 - TH B^12) in R's + convention
  ma <- c(-th, rep(0, 10), -TH, th * TH)
  acf <- stats::ARMAacf(ma = ma, lag.max = 13)
  expect_equal(unlist(r$rho)[c("1", "11", "12", "13")], acf[c("1", "11", "12", "13")], tolerance = 1e-12,
               ignore_attr = TRUE)
  expect_equal(r$gamma[["0"]], 2 * (1 + sum(ma^2)), tolerance = 1e-12)
  expect_equal(r$rho_1, acf[["1"]] / 1, tolerance = 1e-12)
  for (rho in c(-0.45, -0.2, 0, 0.3)) {
    t0 <- moment_estimate(rho)
    expect_equal(-t0 / (1 + t0^2), rho, tolerance = 1e-12)
    expect_lte(abs(t0), 1)
  }
  expect_error(moment_estimate(0.6), "exceeds 0.5")
})

test_that("profile_beta is exact GLS on the Kalman innovations", {
  n <- 12
  t <- seq_len(n)
  wy <- c(0.3, -0.5, 0.8, 0.1, -0.2, 0.6, -0.9, 0.4, 0.2, -0.3, 0.7, -0.1) + 0.2 * t / n
  wx <- list(t / n, cos(t))
  X <- do.call(cbind, wx)
  # white noise: ordinary least squares
  r0 <- profile_beta(wy, wx)
  expect_equal(r0$beta, unname(stats::coef(stats::lm(wy ~ X - 1))), tolerance = 1e-10)
  expect_equal(r0$sum_log_f, 0, tolerance = 1e-12)
  # MA(1) in the Box-Jenkins convention w_t = e_t - 0.5 e_{t-1}
  S <- stats::toeplitz(c(1.25, -0.5, rep(0, n - 2)))
  Si <- solve(S)
  b <- solve(t(X) %*% Si %*% X, t(X) %*% Si %*% wy)
  r <- profile_beta(wy, wx, ma = 0.5)
  expect_equal(r$beta, as.numeric(b), tolerance = 1e-10)
  e <- wy - X %*% b
  expect_equal(r$ssq, sum(e * (Si %*% e)), tolerance = 1e-10)
  expect_equal(r$sum_log_f, as.numeric(determinant(S)$modulus), tolerance = 1e-10)
  rc <- profile_beta(wy, list(), ma = 0.5, filter = "conditional")
  expect_equal(rc$f, rep(1, n))
  expect_error(profile_beta(wy, list(rep(0, n))), "annihilated")
  expect_error(profile_beta(wy, list(1:3)), "must match")
  expect_error(profile_beta(wy, wx, filter = "smooth"), "filter must be")
})

test_that("regression with ARIMA errors and the step-wise order search", {
  n <- 40
  t <- seq_len(n)
  x <- sin(t / 3)
  e <- c(0.3, -0.5, 0.8, 0.1, -0.2, 0.6, -0.9, 0.4, 0.2, -0.3)
  y <- 2 * x + cumsum(rep(e, 4) + 0.5 * c(0, rep(e, 4)[-40]))
  f <- morie_sarimax(y, X = cbind(x), order = c(0, 1, 1), seasonal_order = c(0, 0, 0), s = 1,
                     include_constant = FALSE)
  a <- stats::arima(y, order = c(0, 1, 1), xreg = x, method = "ML")
  # Box-Jenkins theta is minus R's ma coefficient; both are ML optima
  # reached by different optimisers
  expect_equal(f$theta, -unname(stats::coef(a)["ma1"]), tolerance = 1e-3)
  expect_equal(f$beta, unname(stats::coef(a)["x"]), tolerance = 1e-3)
  expect_equal(f$loglik, a$loglik, tolerance = 1e-5)
  expect_equal(f$aic, -2 * f$loglik + 2 * f$n_par, tolerance = 1e-12)
  expect_same_function(sarimax, .sarimax_fit)
  sm <- starting_models(1, 0, 12)
  expect_equal(sm[[1]], list(c(2, 1, 2), c(1, 0, 1)))
  expect_equal(starting_models(0, 1, 1)[[4]], list(c(0, 0, 1), c(0, 1, 0)))
  ao <- auto_order(y, X = cbind(x), d = 1, max_steps = 3)
  starts <- vapply(ao$tried[1:4], function(z) if (isTRUE(z$rejected)) Inf else z$aic, 0)
  expect_lte(ao$aic, min(starts) + 1e-8)
  expect_equal(ao$aic, ao$fit$aic)
  expect_error(morie_sarimax(y, order = c(0, 2, 1), seasonal_order = c(0, 0, 0), s = 1,
                             include_constant = TRUE), "admitted only when")
  expect_error(morie_sarimax(y, method = "mom"), "method must be")
})

test_that("SARSA replays exactly on a deterministic chain", {
  S <- 4
  Pl <- matrix(0, S, S)
  Pr <- matrix(0, S, S)
  for (s in 1:S) {
    Pl[s, max(s - 1, 1)] <- 1
    Pr[s, min(s + 1, S)] <- 1
  }
  R <- matrix(0, S, 2)
  R[3, 2] <- 1
  R[1, 1] <- -0.1
  Q <- matrix(0, S, 2)
  steps <- 0
  greedy <- function(q) if (q[2] > q[1]) 2 else 1
  for (ep in 1:5) {
    s <- 1
    a <- greedy(Q[s, ])
    for (k in 1:20) {
      s2 <- if (a == 1) max(s - 1, 1) else min(s + 1, S)
      if (s2 == 4) {
        tg <- R[s, a]
      } else {
        a2 <- greedy(Q[s2, ])
        tg <- R[s, a] + 0.9 * Q[s2, a2]
      }
      Q[s, a] <- Q[s, a] + 0.5 * (tg - Q[s, a])
      steps <- steps + 1
      if (s2 == 4) break
      s <- s2
      a <- a2
    }
  }
  r1 <- morie_sarsa(list(Pl, Pr), R, 0.9, alpha = 0.5, epsilon = 0, n_episodes = 5, terminal = 3,
                    max_steps = 20)
  r2 <- Sarsa(list(Pl, Pr), R, 0.9, alpha = 0.5, epsilon = 0, n_episodes = 5, terminal = 3, max_steps = 20)
  expect_equal(r1$estimate, Q, tolerance = 1e-12)
  expect_equal(r2$estimate, Q, tolerance = 1e-12)
  expect_equal(r1$n_steps, steps)
  r3 <- morie_sarsa(list(Pl, Pr), R, 0.9, epsilon = 0.3, n_episodes = 4, terminal = 3, seed = 2)
  r4 <- Sarsa(list(Pl, Pr), R, 0.9, epsilon = 0.3, n_episodes = 4, terminal = 3, seed = 2)
  expect_equal(r3$estimate, r4$estimate, tolerance = 1e-12)
  expect_error(morie_sarsa(list(Pl, Pr), R, 0.9, start = 4), "start out of range")
  expect_error(Sarsa(list(Pl, Pr), R, 0.9, start = 9), "start out of range")
})
