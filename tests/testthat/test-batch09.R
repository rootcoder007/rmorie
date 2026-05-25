# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 09: hawkes_fit.R, heinz.R, hrzb1.R, hrzb2.R, hrzc1.R, hrzd1.R,
#           hrzi1.R, hrzi2.R, hrzk1.R, hrzk2.R, hrzk3.R, hrzm1.R,
#           hrzn1.R, hrzn2.R, hrzp1.R

test_that("morie_hawkes_fit fits an exponential-kernel Hawkes process", {
  set.seed(101)
  ev <- sort(cumsum(stats::rexp(60, rate = 3)))
  fit <- morie_hawkes_fit(ev, kernel = "exponential")
  expect_s3_class(fit, "morie_hawkes_fit")
  expect_named(fit$estimate, c("a0", "eta", "beta"))
  expect_equal(fit$kernel, "exponential")
  expect_equal(fit$n_events, length(ev))
  expect_true(is.finite(fit$loglik))
  expect_true(is.finite(fit$aic))
  expect_true(is.finite(fit$branching_ratio))
  expect_true(fit$branching_ratio >= 0)
  expect_true(fit$baseline_rate > 0)
  expect_true(is.logical(fit$converged))
  expect_true(fit$backend %in% c("cpp", "pure-R"))
  expect_true(fit$loglik >= fit$loglik_poisson - 1e-4)
})

test_that("morie_hawkes_fit honours the end_time argument", {
  set.seed(102)
  ev <- sort(cumsum(stats::rexp(40, rate = 2)))
  fit <- morie_hawkes_fit(ev,
    end_time = ev[length(ev)] + 5,
    kernel = "exponential"
  )
  expect_equal(fit$end_time, ev[length(ev)] + 5)
  expect_s3_class(fit, "morie_hawkes_fit")
})

test_that("morie_hawkes_fit supports the weibull / lomax / gamma kernels", {
  set.seed(103)
  ev <- sort(cumsum(stats::rexp(45, rate = 2)))
  for (kn in c("weibull", "lomax", "gamma")) {
    fit <- morie_hawkes_fit(ev, kernel = kn)
    expect_s3_class(fit, "morie_hawkes_fit")
    expect_equal(fit$kernel, kn)
    expect_length(fit$estimate, 4L)
    expect_true(is.finite(fit$loglik))
  }
})

test_that("morie_hawkes_fit rejects unsorted, NA and too-short inputs", {
  expect_error(morie_hawkes_fit(c(3, 1, 2)), "sorted")
  expect_error(morie_hawkes_fit(c(1, NA, 3)), "sorted")
  expect_error(morie_hawkes_fit(c(1)), "at least 2")
  expect_error(
    morie_hawkes_fit(c(1, 2, 3), end_time = 2),
    "end_time"
  )
})

test_that("print.morie_hawkes_fit returns its argument invisibly", {
  set.seed(104)
  ev <- sort(cumsum(stats::rexp(30, rate = 2)))
  fit <- morie_hawkes_fit(ev, kernel = "exponential")
  expect_output(print(fit), "morie Hawkes fit")
  expect_invisible(print(fit))
})

test_that("internal hawkes parameter helpers round-trip", {
  nm <- morie:::.hawkes_param_names("exponential")
  expect_equal(nm, c("a0", "eta", "beta"))
  expect_error(morie:::.hawkes_param_names("nope"), "unknown kernel")

  theta <- c(0.5, 0.3, 2.0)
  phi <- morie:::.hawkes_to_phi(theta)
  back <- morie:::.hawkes_to_theta(phi)
  expect_equal(back, theta, tolerance = 1e-8)
  expect_true(back[2] > 0 && back[2] < 1)
})

test_that("internal hawkes likelihood + start helpers behave", {
  set.seed(105)
  ev <- sort(cumsum(stats::rexp(20, rate = 2)))
  st <- morie:::.hawkes_start("exponential", ev, ev[length(ev)])
  expect_length(st, 3L)
  expect_true(all(is.finite(st)))

  pen <- morie:::.hawkes_nll_pureR(
    c(0, 1.5, 1), ev, ev[length(ev)],
    "exponential"
  )
  expect_equal(pen, 1e12)
  ok <- morie:::.hawkes_nll_pureR(
    c(-0.5, 0.3, 2), ev, ev[length(ev)],
    "exponential"
  )
  expect_true(is.finite(ok))

  expect_null(morie:::.hawkes_kernel_funs("exponential", c(0, 0.3, 0)))
  expect_true(is.list(morie:::.hawkes_kernel_funs(
    "gamma",
    c(0, 0.3, 1.5, 2)
  )))

  lp <- morie:::.hawkes_loglik_poisson(20, ev[length(ev)])
  expect_true(is.finite(lp))

  rs <- morie:::.hawkes_restarts(c(0, 0, 0))
  expect_length(rs, 5L)
  expect_true(all(vapply(rs, length, integer(1)) == 3L))
})

test_that("morie_heinz_he_initialization returns a length-fan_in vector by default", {
  res <- morie_heinz_he_initialization(8L)
  expect_type(res, "list")
  expect_named(res, c("W", "estimate", "mean", "std", "shape", "method"))
  expect_length(res$W, 8L)
  expect_equal(res$shape, 8L)
  expect_true(is.finite(res$mean))
  expect_true(is.finite(res$std))
  expect_match(res$method, "normal")
})

test_that("morie_heinz_he_initialization builds a matrix when fan_out given", {
  res <- morie_heinz_he_initialization(6L, fan_out = 4L)
  expect_true(is.matrix(res$W))
  expect_equal(dim(res$W), c(4L, 6L))
  expect_equal(res$shape, c(4L, 6L))
})

test_that("morie_heinz_he_initialization supports uniform mode", {
  res <- morie_heinz_he_initialization(10L, mode = "uniform")
  expect_length(res$W, 10L)
  limit <- sqrt(6 / 10)
  expect_true(all(res$W >= -limit & res$W <= limit))
  expect_match(res$method, "uniform")
})

test_that("morie_heinz_he_initialization is reproducible for a fixed seed", {
  a <- morie_heinz_he_initialization(12L, seed = 7L)
  b <- morie_heinz_he_initialization(12L, seed = 7L)
  expect_equal(a$W, b$W)
})

test_that("morie_heinz_he_initialization rejects bad fan_in and mode", {
  expect_error(morie_heinz_he_initialization(0L), "fan_in")
  expect_error(morie_heinz_he_initialization(-3L), "fan_in")
  expect_error(morie_heinz_he_initialization(5L, mode = "bogus"), "mode")
})

test_that("morie_heinz_he_initialization honours deterministic_seed", {
  res <- morie_heinz_he_initialization(8L, deterministic_seed = 3L)
  expect_length(res$W, 8L)
  expect_true(all(is.finite(res$W)))
})

test_that("morie_he_initialization alias matches morie_heinz_he_initialization", {
  expect_identical(morie_he_initialization, morie_heinz_he_initialization)
})

test_that("morie_horowitz_binary_response fits a maximum-score model", {
  set.seed(201)
  n <- 80
  X <- cbind(stats::rnorm(n), stats::rnorm(n))
  y <- as.numeric(X %*% c(1, 0.5) + stats::rnorm(n) > 0)
  res <- morie_horowitz_binary_response(X, y)
  expect_type(res, "list")
  expect_named(res, c("estimate", "se", "score", "n", "method", "warnings"))
  expect_length(res$estimate, 2L)
  expect_equal(res$n, n)
  expect_equal(sqrt(sum(res$estimate^2)), 1, tolerance = 1e-6)
  expect_true(is.finite(res$score))
  expect_match(res$method, "Manski")
})

test_that("morie_horowitz_binary_response returns NA on insufficient data", {
  res <- morie_horowitz_binary_response(stats::rnorm(6), c(0, 1, 0, 1, 0, 1))
  expect_true(all(is.na(res$estimate)))
  expect_match(res$method, "insufficient data")
})

test_that("morie_horowitz_binary_response alias is bound to hrzb1", {
  expect_identical(morie_horowitz_binary_response, morie:::hrzb1)
})

test_that("morie_horowitz_smoothed_maximum_score fits with the default bandwidth", {
  set.seed(202)
  n <- 70
  X <- cbind(stats::rnorm(n), stats::rnorm(n))
  y <- as.numeric(X %*% c(1, -0.6) + stats::rnorm(n) > 0)
  res <- morie_horowitz_smoothed_maximum_score(X, y)
  expect_named(res, c("estimate", "se", "bandwidth", "n", "method"))
  expect_length(res$estimate, 2L)
  expect_equal(sqrt(sum(res$estimate^2)), 1, tolerance = 1e-6)
  expect_true(res$bandwidth > 0)
  expect_equal(res$n, n)
})

test_that("morie_horowitz_smoothed_maximum_score accepts an explicit bandwidth", {
  set.seed(203)
  n <- 60
  X <- cbind(stats::rnorm(n), stats::rnorm(n))
  y <- as.numeric(X %*% c(1, 0.4) + stats::rnorm(n) > 0)
  res <- morie_horowitz_smoothed_maximum_score(X, y, bandwidth = 0.5)
  expect_equal(res$bandwidth, 0.5)
})

test_that("morie_horowitz_smoothed_maximum_score handles a vector covariate", {
  set.seed(204)
  x <- stats::rnorm(50)
  y <- as.numeric(x + stats::rnorm(50) > 0)
  res <- morie_horowitz_smoothed_maximum_score(x, y)
  expect_length(res$estimate, 1L)
})

test_that("morie_horowitz_smoothed_maximum_score returns NA on tiny data", {
  res <- morie_horowitz_smoothed_maximum_score(stats::rnorm(6), rep(c(0, 1), 3))
  expect_true(all(is.na(res$estimate)))
  expect_match(res$method, "insufficient data")
})

test_that("morie_horowitz_censored_regression fits a censored LAD model", {
  set.seed(205)
  n <- 80
  X <- cbind(1, stats::rnorm(n))
  ystar <- X %*% c(0.5, 1) + stats::rnorm(n)
  y <- pmax(as.numeric(ystar), 0)
  res <- morie_horowitz_censored_regression(X, y, censor = 0)
  expect_named(res, c(
    "estimate", "se", "n", "n_uncensored", "censor",
    "method"
  ))
  expect_length(res$estimate, 2L)
  expect_equal(res$n, n)
  expect_equal(res$censor, 0)
  expect_true(res$n_uncensored >= 0 && res$n_uncensored <= n)
  expect_match(res$method, "Powell")
})

test_that("morie_horowitz_censored_regression uses a custom censor threshold", {
  set.seed(206)
  n <- 70
  X <- cbind(1, stats::rnorm(n))
  y <- pmax(as.numeric(X %*% c(1, 0.8) + stats::rnorm(n)), 1)
  res <- morie_horowitz_censored_regression(X, y, censor = 1)
  expect_equal(res$censor, 1)
})

test_that("morie_horowitz_censored_regression returns NA on insufficient data", {
  res <- morie_horowitz_censored_regression(stats::rnorm(6), stats::rnorm(6))
  expect_true(all(is.na(res$estimate)))
  expect_match(res$method, "insufficient")
})

test_that("morie_horowitz_censored_regression flags too few uncensored obs", {
  set.seed(207)
  n <- 40
  X <- cbind(1, stats::rnorm(n))
  y <- rep(-5, n)
  res <- morie_horowitz_censored_regression(X, y, censor = 0)
  expect_true(all(is.na(res$estimate)))
  expect_match(res$method, "too few uncensored")
})

test_that("morie_horowitz_duration_model fits a Cox proportional-hazards model", {
  set.seed(208)
  n <- 80
  X <- cbind(stats::rnorm(n), stats::rnorm(n))
  t <- stats::rexp(n, rate = exp(X %*% c(0.5, -0.3)))
  event <- stats::rbinom(n, 1, 0.8)
  res <- morie_horowitz_duration_model(t, X, event)
  expect_named(res, c("estimate", "se", "n", "n_events", "method"))
  expect_length(res$estimate, 2L)
  expect_equal(res$n, n)
  expect_equal(res$n_events, sum(event))
  expect_true(all(is.finite(res$estimate)))
  expect_match(res$method, "Cox")
})

test_that("morie_horowitz_duration_model handles a single-covariate vector", {
  set.seed(209)
  n <- 60
  x <- stats::rnorm(n)
  t <- stats::rexp(n, rate = exp(0.4 * x))
  event <- rep(1, n)
  res <- morie_horowitz_duration_model(t, x, event)
  expect_length(res$estimate, 1L)
})

test_that("morie_horowitz_duration_model returns NA on insufficient data", {
  res <- morie_horowitz_duration_model(
    stats::rexp(6), stats::rnorm(6),
    rep(1, 6)
  )
  expect_true(all(is.na(res$estimate)))
  expect_match(res$method, "insufficient")
})

test_that("morie_horowitz_index_model fits a single-index model", {
  set.seed(210)
  n <- 70
  X <- cbind(stats::rnorm(n), stats::rnorm(n))
  y <- (X %*% c(1, 0.5))^2 + stats::rnorm(n, sd = 0.2)
  res <- morie_horowitz_index_model(X, y)
  expect_named(res, c(
    "estimate", "se", "bandwidth", "n", "loss",
    "method"
  ))
  expect_length(res$estimate, 2L)
  expect_equal(sqrt(sum(res$estimate^2)), 1, tolerance = 1e-6)
  expect_true(res$bandwidth > 0)
  expect_true(is.finite(res$loss))
  expect_match(res$method, "Ichimura")
})

test_that("morie_horowitz_index_model accepts an explicit bandwidth", {
  set.seed(211)
  n <- 60
  X <- cbind(stats::rnorm(n), stats::rnorm(n))
  y <- as.numeric(X %*% c(1, -0.3)) + stats::rnorm(n)
  res <- morie_horowitz_index_model(X, y, bandwidth = 0.7)
  expect_equal(res$bandwidth, 0.7)
})

test_that("morie_horowitz_index_model returns NA on insufficient data", {
  res <- morie_horowitz_index_model(stats::rnorm(6), stats::rnorm(6))
  expect_true(all(is.na(res$estimate)))
  expect_match(res$method, "insufficient")
})

test_that("morie_horowitz_average_derivative estimates an average derivative", {
  set.seed(212)
  n <- 80
  x <- stats::rnorm(n)
  y <- 2 * x + stats::rnorm(n, sd = 0.3)
  res <- morie_horowitz_average_derivative(x, y)
  expect_named(res, c("estimate", "se", "bandwidth", "n", "method"))
  expect_length(res$estimate, 1L)
  expect_true(is.finite(res$estimate))
  expect_true(res$bandwidth > 0)
  expect_equal(res$n, n)
  expect_match(res$method, "average derivative")
})

test_that("morie_horowitz_average_derivative handles a multi-column design", {
  set.seed(213)
  n <- 80
  X <- cbind(stats::rnorm(n), stats::rnorm(n))
  y <- as.numeric(X %*% c(1, -1)) + stats::rnorm(n, sd = 0.3)
  res <- morie_horowitz_average_derivative(X, y, bandwidth = 0.8)
  expect_length(res$estimate, 2L)
  expect_equal(res$bandwidth, 0.8)
})

test_that("morie_horowitz_average_derivative returns NA on insufficient data", {
  res <- morie_horowitz_average_derivative(stats::rnorm(10), stats::rnorm(10))
  expect_true(all(is.na(res$estimate)))
  expect_match(res$method, "insufficient")
})

test_that("morie_horowitz_kernel_density estimates at the sample points", {
  set.seed(214)
  x <- stats::rnorm(60)
  res <- morie_horowitz_kernel_density(x)
  expect_named(res, c(
    "estimate", "se", "bandwidth", "n", "kernel",
    "method"
  ))
  expect_length(res$estimate, length(x))
  expect_true(all(res$estimate >= 0))
  expect_true(res$bandwidth > 0)
  expect_equal(res$kernel, "gaussian")
})

test_that("morie_horowitz_kernel_density evaluates on a separate grid", {
  set.seed(215)
  samp <- stats::rnorm(80)
  grid <- seq(-2, 2, length.out = 11)
  res <- morie_horowitz_kernel_density(grid, sample = samp)
  expect_length(res$estimate, length(grid))
  expect_true(all(res$estimate >= 0))
})

test_that("morie_horowitz_kernel_density accepts an explicit bandwidth", {
  set.seed(216)
  res <- morie_horowitz_kernel_density(stats::rnorm(40), bandwidth = 0.5)
  expect_equal(res$bandwidth, 0.5)
})

test_that("morie_horowitz_kernel_density returns NA on a singleton sample", {
  res <- morie_horowitz_kernel_density(c(1.0))
  expect_true(is.na(res$estimate))
  expect_match(res$method, "insufficient")
})

test_that("morie_horowitz_kernel_regression fits an NW regression", {
  set.seed(217)
  x <- stats::rnorm(70)
  y <- sin(x) + stats::rnorm(70, sd = 0.2)
  res <- morie_horowitz_kernel_regression(x, y)
  expect_named(res, c("estimate", "se", "bandwidth", "n", "method"))
  expect_length(res$estimate, length(x))
  expect_true(all(is.finite(res$estimate)))
  expect_true(res$bandwidth > 0)
  expect_match(res$method, "Nadaraya-Watson")
})

test_that("morie_horowitz_kernel_regression evaluates on a custom grid", {
  set.seed(218)
  x <- stats::rnorm(60)
  y <- 2 * x + stats::rnorm(60, sd = 0.3)
  grid <- seq(-1, 1, length.out = 9)
  res <- morie_horowitz_kernel_regression(x, y, grid = grid, bandwidth = 0.4)
  expect_length(res$estimate, length(grid))
  expect_equal(res$bandwidth, 0.4)
})

test_that("morie_horowitz_kernel_regression returns NA on insufficient data", {
  res <- morie_horowitz_kernel_regression(c(1.0), c(2.0))
  expect_true(is.na(res$estimate))
  expect_match(res$method, "insufficient")
})

test_that("morie_horowitz_local_linear fits a local-linear regression", {
  set.seed(219)
  x <- stats::rnorm(70)
  y <- 1.5 * x + stats::rnorm(70, sd = 0.2)
  res <- morie_horowitz_local_linear(x, y)
  expect_named(res, c("estimate", "se", "bandwidth", "n", "method"))
  expect_length(res$estimate, length(x))
  expect_true(res$bandwidth > 0)
  expect_match(res$method, "Local-linear")
})

test_that("morie_horowitz_local_linear evaluates on a custom grid", {
  set.seed(220)
  x <- stats::rnorm(60)
  y <- x^2 + stats::rnorm(60, sd = 0.3)
  grid <- seq(-1, 1, length.out = 7)
  res <- morie_horowitz_local_linear(x, y, grid = grid, bandwidth = 0.5)
  expect_length(res$estimate, length(grid))
  expect_equal(res$bandwidth, 0.5)
})

test_that("morie_horowitz_local_linear returns NA on insufficient data", {
  res <- morie_horowitz_local_linear(c(1, 2), c(3, 4))
  expect_true(is.na(res$estimate))
  expect_match(res$method, "insufficient")
})

test_that("morie_horowitz_mixture_model fits a 2-component mixture by default", {
  set.seed(221)
  y <- c(stats::rnorm(40, -3), stats::rnorm(40, 3))
  res <- morie_horowitz_mixture_model(y)
  expect_named(res, c(
    "estimate", "log_likelihood", "n", "k", "iters",
    "method"
  ))
  expect_equal(res$k, 2L)
  expect_named(res$estimate, c("pi", "mu", "sigma"))
  expect_length(res$estimate$pi, 2L)
  expect_length(res$estimate$mu, 2L)
  expect_length(res$estimate$sigma, 2L)
  expect_equal(sum(res$estimate$pi), 1, tolerance = 1e-6)
  expect_true(all(res$estimate$sigma > 0))
  expect_true(res$iters >= 1L)
})

test_that("morie_horowitz_mixture_model supports a 3-component fit", {
  set.seed(222)
  y <- c(stats::rnorm(30, -4), stats::rnorm(30, 0), stats::rnorm(30, 4))
  res <- morie_horowitz_mixture_model(y, k = 3, maxit = 100, tol = 1e-5)
  expect_equal(res$k, 3L)
  expect_length(res$estimate$mu, 3L)
  expect_match(res$method, "3-component")
})

test_that("morie_horowitz_mixture_model returns NA on insufficient data", {
  res <- morie_horowitz_mixture_model(stats::rnorm(5))
  expect_true(is.na(res$estimate))
  expect_match(res$method, "insufficient")
})

test_that("morie_horowitz_nonparametric_iv fits an NPIV model", {
  set.seed(223)
  n <- 120
  z <- stats::rnorm(n)
  x <- 0.7 * z + stats::rnorm(n, sd = 0.5)
  y <- x + stats::rnorm(n, sd = 0.3)
  res <- morie_horowitz_nonparametric_iv(x, y, z, J = 4, .bootstrap = FALSE)
  expect_named(res, c(
    "estimate", "se", "grid", "J", "alpha", "n",
    "method"
  ))
  expect_length(res$estimate, length(res$grid))
  expect_equal(res$J, 4)
  expect_equal(res$n, n)
  expect_true(all(is.na(res$se)))
  expect_match(res$method, "Tikhonov")
})

test_that("morie_horowitz_nonparametric_iv bootstraps SEs when requested", {
  set.seed(224)
  n <- 80
  z <- stats::rnorm(n)
  x <- 0.6 * z + stats::rnorm(n, sd = 0.5)
  y <- x + stats::rnorm(n, sd = 0.3)
  grid <- seq(-1, 1, length.out = 5)
  res <- morie_horowitz_nonparametric_iv(x, y, z, J = 3, grid = grid)
  expect_length(res$estimate, length(grid))
  expect_length(res$se, length(grid))
  expect_true(all(is.finite(res$se)))
})

test_that("morie_horowitz_nonparametric_iv falls back to 2SLS for small n", {
  set.seed(225)
  n <- 30
  z <- stats::rnorm(n)
  x <- 0.5 * z + stats::rnorm(n, sd = 0.4)
  y <- x + stats::rnorm(n, sd = 0.3)
  res <- morie_horowitz_nonparametric_iv(x, y, z)
  expect_length(res$estimate, 1L)
  expect_match(res$method, "2SLS")
})

test_that("morie_horowitz_deconvolution estimates a density with laplace noise", {
  set.seed(226)
  y <- stats::rnorm(80) + stats::rexp(80) - stats::rexp(80)
  res <- morie_horowitz_deconvolution(y)
  expect_named(res, c(
    "estimate", "grid", "bandwidth", "sigma_u",
    "noise", "n", "method"
  ))
  expect_length(res$estimate, length(res$grid))
  expect_true(all(res$estimate >= 0))
  expect_true(res$bandwidth > 0)
  expect_equal(res$noise, "laplace")
  expect_match(res$method, "deconvolution")
})

test_that("morie_horowitz_deconvolution supports normal noise and custom args", {
  set.seed(227)
  y <- stats::rnorm(60, sd = 1.5)
  grid <- seq(-3, 3, length.out = 11)
  res <- morie_horowitz_deconvolution(y,
    sigma_u = 0.3, bandwidth = 1.2,
    grid = grid, noise = "normal"
  )
  expect_equal(res$noise, "normal")
  expect_equal(res$bandwidth, 1.2)
  expect_equal(res$sigma_u, 0.3)
  expect_length(res$estimate, length(grid))
})

test_that("morie_horowitz_deconvolution returns NA on insufficient data", {
  res <- morie_horowitz_deconvolution(stats::rnorm(10))
  expect_true(is.na(res$estimate))
  expect_match(res$method, "insufficient")
})

test_that("morie_horowitz_plr_estimator fits a partially-linear regression", {
  set.seed(228)
  n <- 80
  x <- stats::rnorm(n)
  z <- stats::rnorm(n)
  y <- 1.5 * x + sin(z) + stats::rnorm(n, sd = 0.2)
  res <- morie_horowitz_plr_estimator(x, y, z)
  expect_named(res, c("estimate", "se", "bandwidth", "n", "method"))
  expect_length(res$estimate, 1L)
  expect_true(is.finite(res$estimate))
  expect_true(res$bandwidth > 0)
  expect_equal(res$n, n)
  expect_match(res$method, "Robinson")
})

test_that("morie_horowitz_plr_estimator handles a multi-column parametric part", {
  set.seed(229)
  n <- 70
  X <- cbind(stats::rnorm(n), stats::rnorm(n))
  z <- stats::rnorm(n)
  y <- as.numeric(X %*% c(1, -0.5)) + cos(z) + stats::rnorm(n, sd = 0.2)
  res <- morie_horowitz_plr_estimator(X, y, z, bandwidth = 0.6)
  expect_length(res$estimate, 2L)
  expect_equal(res$bandwidth, 0.6)
})

test_that("morie_horowitz_plr_estimator returns NA on insufficient data", {
  res <- morie_horowitz_plr_estimator(
    stats::rnorm(4), stats::rnorm(4),
    stats::rnorm(4)
  )
  expect_true(is.na(res$estimate))
  expect_match(res$method, "insufficient")
})
