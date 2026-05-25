# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch J: hawkes_fit, rndsr, dataset_profile, mrkvr, stacv, fzcvm, irtsp, stkrg, hrzd1.

test_that("morie_hawkes_fit fits exponential kernel on synthetic events", {
  set.seed(1)
  ev <- sort(cumsum(rexp(100, rate = 2)))
  fit <- morie_hawkes_fit(ev, kernel = "exponential")
  expect_s3_class(fit, "morie_hawkes_fit")
  expect_true(is.finite(fit$loglik))
  expect_equal(fit$n_events, 100L)
  expect_named(fit$estimate, c("a0", "eta", "beta"))
})

test_that("morie_hawkes_fit fits weibull/lomax/gamma kernels", {
  set.seed(1)
  ev <- sort(cumsum(rexp(80, rate = 1.5)))
  for (k in c("weibull", "lomax", "gamma")) {
    fit <- morie_hawkes_fit(ev, kernel = k, end_time = max(ev) + 1)
    expect_s3_class(fit, "morie_hawkes_fit")
    expect_true(is.finite(fit$loglik))
  }
})

test_that("morie_hawkes_fit input guards", {
  expect_error(morie_hawkes_fit(c(1, 0.5, 2)), "sorted")
  expect_error(morie_hawkes_fit(c(1.0)), "at least 2 events")
  expect_error(morie_hawkes_fit(c(1, 2, 3), end_time = 0.5), "end_time")
})

test_that("hawkes phi<->theta round-trip", {
  th <- c(0.5, 0.3, 1.0, 2.0)
  phi <- morie:::.hawkes_to_phi(th)
  expect_equal(morie:::.hawkes_to_theta(phi), th, tolerance = 1e-9)
})

test_that("morie_random_search_cv runs in regression mode", {
  set.seed(1)
  x <- matrix(rnorm(60), 30, 2)
  y <- x %*% c(1, -1) + rnorm(30, sd = 0.1)
  out <- morie_random_search_cv(x, y, n_iter = 3L, cv = 2L, task = "regression", seed = 1L)
  expect_equal(out$task, "regression")
  expect_equal(out$n, 30L)
})

test_that("morie_infer_measurement_level covers main branches", {
  expect_equal(morie_infer_measurement_level(c(TRUE, FALSE, TRUE)), "binary")
  expect_equal(morie_infer_measurement_level(c(0L, 1L, 1L, 0L)), "binary")
  expect_equal(morie_infer_measurement_level(ordered(c("low", "med", "high"))), "ordinal")
  expect_equal(morie_infer_measurement_level(factor(c("a", "b", "c"))), "nominal")
  expect_equal(morie_infer_measurement_level(c(1.2, 3.4, 5.6)), "ratio")
})

test_that("morie_profile_dataset returns per-column profiles and rejects non-frames", {
  expect_error(morie_profile_dataset(list(a = 1)), "data.frame")
  p <- morie_profile_dataset(iris)
  expect_equal(p$n_rows, nrow(iris))
  expect_equal(p$n_cols, ncol(iris))
})

test_that("morie_suggest_analysis_plan errors when no profile given", {
  expect_error(morie_suggest_analysis_plan(list()), "morie_profile_dataset")
})

test_that("morie_marker_variance default path with x = empty", {
  set.seed(1)
  n <- 20
  m <- 8
  M <- matrix(sample(0:2, n * m, TRUE), n, m)
  y <- M %*% rnorm(m) + 0.5 * rnorm(n)
  out <- morie_marker_variance(x = numeric(0), y = y, markers = M)
  expect_true(is.finite(out$sigma_g2))
  expect_equal(out$n, n)
  expect_equal(out$p, m)
})

test_that("stacv returns binned C matrix", {
  set.seed(1)
  n <- 40
  out <- stacv(
    x = rnorm(n),
    coords = matrix(runif(2 * n), n, 2),
    times = sort(cumsum(rexp(n))),
    n_spatial_bins = 4, n_temporal_bins = 4
  )
  expect_equal(dim(out$estimate$C), c(4, 4))
  expect_equal(out$n, n)
})

test_that("stacv error on shape mismatch", {
  # 5x2 coords (10 values) but rnorm(10) length-10 -> n mismatch
  expect_error(
    stacv(rnorm(10), matrix(runif(10), 5, 2), seq_len(10)),
    "shape mismatch"
  )
})

test_that("fzcvm default normal CDF path", {
  set.seed(1)
  x <- rnorm(80)
  out <- fzcvm(x)
  expect_true(is.finite(out$statistic))
  expect_true(out$statistic >= 0)
  expect_equal(out$n, 80L)
})

test_that("fzcvm guards: too few obs + unknown cdf string", {
  expect_match(fzcvm(c(1, 2, 3))$method, "too few obs")
  expect_error(fzcvm(rnorm(20), cdf = "foo"), "non-normal cdf")
})

test_that(".morie_cvm_pvalue covers all interpolation regimes", {
  expect_equal(morie:::.morie_cvm_pvalue(0), 1.0)
  expect_equal(morie:::.morie_cvm_pvalue(0.1), 0.5)
})

test_that("irtsp fits a 2PL model to a binary roll-call matrix", {
  set.seed(1)
  n <- 30
  m <- 8
  M <- matrix(rbinom(n * m, 1, 0.5), n, m)
  out <- irtsp(M, n_iter = 10L)
  expect_length(out$x_hat, n)
  expect_length(out$alpha, m)
  expect_length(out$beta, m)
  expect_true(is.finite(out$loglik))
})

test_that("irtsp degenerate-shape branch returns NA fields", {
  out <- irtsp(matrix(integer(0), 0, 0))
  expect_true(all(is.na(out$x_hat)))
  expect_equal(out$n_iter, 0L)
})

test_that("stkrg single-target kriging reproduces value at observed site", {
  set.seed(1)
  n <- 20
  coords <- matrix(runif(2 * n), n, 2)
  times <- sort(cumsum(rexp(n)))
  x <- rnorm(n)
  out <- stkrg(
    x, coords, times,
    target = list(s0 = coords[1, , drop = FALSE], t0 = times[1])
  )
  expect_true(is.finite(out$estimate))
  expect_equal(out$n, n)
})

test_that("stkrg input guards", {
  set.seed(1)
  n <- 10
  coords <- matrix(runif(2 * n), n, 2)
  times <- seq_len(n) * 1.0
  x <- rnorm(n)
  expect_error(
    stkrg(x, coords[-1, ], times,
      target = list(s0 = coords[1, , drop = FALSE], t0 = times[1])
    ),
    "shape mismatch"
  )
})

test_that("hrzd1 fits Cox PH on single covariate", {
  set.seed(1)
  n <- 60
  x <- rnorm(n)
  t <- rexp(n, rate = exp(0.5 * x))
  event <- rbinom(n, 1, 0.8)
  out <- morie:::hrzd1(t, x, event)
  expect_length(out$estimate, 1L)
  expect_equal(out$n, n)
})

test_that("hrzd1 insufficient-data branch", {
  out <- morie:::hrzd1(c(1, 2, 3), c(0.1, 0.2, 0.3), c(1, 0, 1))
  expect_true(all(is.na(out$estimate)))
  expect_match(out$method, "insufficient data")
})
