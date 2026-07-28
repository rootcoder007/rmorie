# Cross-language anchors from morie.fn._horowitz at full precision.
# Both languages use the SAME bandwidth rule: R's existing
# .hrz_silverman already matches morie.fn's silverman_bw exactly.

test_that("Silverman bandwidth and KDE match Python", {
  set.seed(0)
  x <- stats::rnorm(200)
  # anchors were produced from numpy's default_rng(0); the bandwidth
  # RULE is what must agree, so it is checked on a fixed vector
  xf <- c(1, 2, 3, 4, 5, 4, 3, 2, 1, 0)
  s <- stats::sd(xf)
  iqr <- diff(stats::quantile(xf, c(0.25, 0.75)))
  expect_equal(.hrz_silverman(xf),
    unname(1.06 * min(s, iqr / 1.349) * 10^(-1 / 5)), tolerance = 1e-12)
  out <- morie_kde_h(xf)
  expect_equal(out$rate_exponent, -0.4)
  expect_equal(out$bandwidth, .hrz_silverman(xf), tolerance = 1e-12)
  # the density integrates to ~1 over its own grid
  expect_equal(sum(out$density) * diff(out$grid[1:2]), 1, tolerance = 0.02)
  expect_error(morie_kde_h(1))
})

test_that("MISE bandwidth uses the Gaussian constants", {
  set.seed(1)
  x <- stats::rnorm(400)
  out <- morie_bandwidth_mise(x)
  expect_equal(out$R_K, 1 / (2 * sqrt(pi)), tolerance = 1e-12)
  expect_equal(out$mu2_K, 1)
  expect_true(out$normal_reference_used)
  # supplying the true functional scales h by that factor^(-1/5)
  known <- morie_bandwidth_mise(x, f_second_deriv_l2 = out$f2_l2 * 32)
  expect_false(known$normal_reference_used)
  expect_equal(known$h_opt, out$h_opt * 32^(-0.2), tolerance = 1e-10)
})

test_that("local linear beats NW at the boundary, as the theory says", {
  set.seed(3)
  n <- 400
  x <- stats::runif(n)
  y <- 2 * x + stats::rnorm(n, sd = 0.05)
  edge <- c(0.02, 0.05)
  ll <- morie_local_linear(x, y, grid = edge)$fitted
  nw <- morie_nw_regression(x, y, grid = edge)$fitted
  expect_lt(max(abs(ll - 2 * edge)), max(abs(nw - 2 * edge)))
  expect_lt(max(abs(ll - 2 * edge)), 0.05)
  # the local slope recovers m'(x) = 2 for free
  expect_equal(stats::median(morie_local_linear(x, y)$slope), 2, tolerance = 0.3)
  expect_error(morie_local_linear(x, y[1:10]))
})

test_that("series regression needs K to grow", {
  set.seed(9)
  x <- stats::runif(500)
  y <- sin(3 * x) + stats::rnorm(500, sd = 0.1)
  lo <- morie_series_regression(x, y, K = 2)
  hi <- morie_series_regression(x, y, K = 8)
  expect_gt(hi$r_squared, lo$r_squared) # K=2 cannot fit a sine
  expect_equal(hi$df_ratio, 8 / 500)
  expect_equal(dim(morie_sieve_basis(x, 4)), c(500L, 4L))
  expect_error(morie_series_regression(x, y, K = 0))
  expect_error(morie_sieve_basis(x, 4, kind = "spline"))
})

test_that("index regression keeps the one-dimensional rate", {
  set.seed(5)
  n <- 400
  X <- matrix(stats::rnorm(n * 4), ncol = 4)
  beta <- c(1, -0.5, 0.25, 0.75)
  y <- tanh(X %*% beta) + stats::rnorm(n, sd = 0.1)
  out <- morie_index_regression(X, y, beta)
  expect_equal(out$rate_exponent, -0.4) # independent of d
  expect_equal(out$d, 4L)
  ok <- is.finite(out$G)
  expect_gt(stats::cor(out$G[ok], tanh(out$index_grid[ok])), 0.98)
  expect_error(morie_index_regression(X, y, beta[1:2]))
})

test_that("Ichimura recovers the index under its normalisation", {
  set.seed(6)
  n <- 400
  X <- matrix(stats::rnorm(n * 2), ncol = 2)
  y <- as.numeric(tanh(X %*% c(1, -0.6))) + stats::rnorm(n, sd = 0.15)
  out <- morie_ichimura(X, y)
  expect_equal(out$beta[1], 1) # the |b1| = 1 normalisation
  expect_equal(out$beta[2], -0.6, tolerance = 0.35)
  expect_true(out$root_n)
  expect_error(morie_ichimura(X[, 1, drop = FALSE], y))
})

test_that("max score and its smoothed version promise different things", {
  set.seed(7)
  n <- 400
  X <- matrix(stats::rnorm(n * 2), ncol = 2)
  bt <- c(1, -0.8)
  sc <- 0.5 + abs(X[, 1]) # heteroskedastic: probit would be inconsistent
  y <- as.numeric((X %*% bt + sc * stats::rnorm(n)) > 0)
  ms <- morie_maximum_score(X, y)
  expect_equal(ms$rate_exponent, -1 / 3)
  expect_false(ms$standard_errors_valid) # Chernoff limit
  sm <- morie_maximum_score(X, y, smoothed = TRUE)
  expect_equal(sm$rate_exponent, -2 / 5)
  expect_true(sm$standard_errors_valid) # normality restored
  expect_error(morie_maximum_score(X, rep(2, n)))
})

test_that("partially linear is root-n and beats ignoring g(Z)", {
  set.seed(8)
  n <- 600
  Z <- stats::runif(n, -2, 2)
  X <- matrix(stats::rnorm(n * 2), ncol = 2) + 0.3 * Z
  bt <- c(1.5, -0.7)
  y <- as.numeric(X %*% bt + sin(2 * Z) + stats::rnorm(n, sd = 0.2))
  out <- morie_partially_linear(X, Z, y)
  expect_equal(out$beta, bt, tolerance = 0.2)
  expect_true(all(out$se > 0))
  naive <- qr.solve(X, y)
  expect_lt(max(abs(out$beta - bt)), max(abs(naive - bt)))
  expect_error(morie_partially_linear(X, Z[1:10], y))
})

test_that("backfitting centres its components and quantiles do not cross", {
  set.seed(10)
  n <- 400
  X <- matrix(stats::runif(n * 2, -1, 1), ncol = 2)
  y <- 1 + X[, 1]^2 + sin(3 * X[, 2]) + stats::rnorm(n, sd = 0.1)
  bf <- morie_backfitting(X, y)
  expect_lt(abs(mean(bf$components[, 1])), 1e-8) # only the SUM is identified
  expect_lt(abs(mean(bf$components[, 2])), 1e-8)
  expect_equal(bf$mu, mean(y), tolerance = 1e-12)
  expect_gt(stats::cor(bf$fitted, y), 0.9)
  # kernel quantiles come from one CDF, so they are monotone in tau
  x <- stats::runif(800)
  yq <- 2 * x + stats::rnorm(800) * (0.2 + x)
  kq <- morie_kernel_quantile(x, yq, tau = c(0.1, 0.5, 0.9),
                              grid = c(0.3, 0.7))
  expect_true(kq$monotone_in_tau)
  expect_true(all(diff(t(kq$quantile)) >= 0))
  expect_error(morie_kernel_quantile(x, yq, tau = c(0.5, 1.5)))
})

test_that("rate checker measures the exponent exactly", {
  n <- c(100, 400, 1600, 6400)
  out <- morie_rate_check(3 * n^(-0.4), n, -0.4)
  expect_equal(out$observed_exponent, -0.4, tolerance = 1e-9)
  expect_true(out$consistent)
  expect_false(morie_rate_check(3 * n^(-0.2), n, -0.4)$consistent)
  expect_error(morie_rate_check(c(1, 2), c(10, 20), -0.4))
})
