# SPDX-License-Identifier: AGPL-3.0-or-later
# Native Engle-Granger / ADF / KPSS / Johansen versus urca.
#
# Tolerances: the native code performs the same least-squares and
# eigen computations as urca (lm.fit QR + chol2inv for the ADF t
# statistic, the same moment matrices and Cholesky-whitened eigenproblem
# for Johansen), so differences are pure floating-point rounding:
# observed max relative differences are ~1e-14 (ADF/KPSS) and exactly 0
# (Johansen).  1e-10 / 1e-8 leave ample headroom while still catching
# any change of lag selection, sample trimming or normalisation.

.sim_coint <- function(seed, n, P = 2L) {
  set.seed(seed)
  w <- cumsum(stats::rnorm(n))
  Y <- vapply(seq_len(P), function(j) {
    if (j <= 2L) (0.5 + j / 2) * w + stats::rnorm(n) else cumsum(stats::rnorm(n))
  }, numeric(n))
  colnames(Y) <- paste0("v", seq_len(P))
  Y
}

test_that("ADF helper reproduces urca::ur.df for all types and lag rules", {
  skip_if_not_installed("urca")
  cases <- expand.grid(type = c("none", "drift", "trend"),
                       sel = c("Fixed", "AIC", "BIC"), lags = c(0L, 1L, 4L, 8L),
                       stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    set.seed(100 + i)
    y <- cumsum(stats::rnorm(60 + 7 * i)) + stats::arima.sim(list(ar = 0.5), 60 + 7 * i)
    a <- rmorie:::.morie_urdf(y, cases$type[i], cases$lags[i], cases$sel[i])
    b <- urca::ur.df(y, type = cases$type[i], lags = cases$lags[i],
                     selectlags = cases$sel[i])
    expect_equal(a$statistic, unname(b@teststat[1]), tolerance = 1e-10)
    expect_equal(unname(a$cval), unname(b@cval[1, ]))
  }
})

test_that("morie_eg_coint matches urca ur.df(type = 'none', selectlags = 'AIC')", {
  skip_if_not_installed("urca")
  for (s in 1:6) {
    Y <- .sim_coint(s, n = c(40, 80, 120, 200, 300, 500)[s])
    r <- morie_eg_coint(Y[, 1], Y[, 2])
    n <- nrow(Y)
    u <- stats::residuals(stats::lm(Y[, 1] ~ Y[, 2]))
    b <- urca::ur.df(u, type = "none", lags = floor(12 * (n / 100)^0.25),
                     selectlags = "AIC")
    expect_equal(r$adf_statistic, unname(b@teststat[1]), tolerance = 1e-10)
    r2 <- morie_eg_coint(Y[, 1], Y[, 2], max_lag = 2)
    b2 <- urca::ur.df(u, type = "none", lags = 2, selectlags = "AIC")
    expect_equal(r2$adf_statistic, unname(b2@teststat[1]), tolerance = 1e-10)
  }
})

test_that("KPSS helper and morie_ts_stationarity match urca", {
  skip_if_not_installed("urca")
  for (s in 1:8) {
    set.seed(200 + s)
    n <- c(30, 60, 100, 150, 250, 400, 75, 500)[s]
    v <- if (s %% 2L) cumsum(stats::rnorm(n)) else
      as.numeric(stats::arima.sim(list(ar = 0.6), n))
    for (ty in c("mu", "tau")) for (lg in c("short", "long", "nil")) {
      a <- rmorie:::.morie_kpss(v, ty, lg)
      b <- urca::ur.kpss(v, type = ty, lags = lg)
      expect_equal(a$statistic, b@teststat, tolerance = 1e-10)
      expect_equal(unname(a$cval), unname(b@cval[1, ]))
    }
    st <- morie_ts_stationarity(v)
    adf <- urca::ur.df(v, type = "drift", selectlags = "AIC")
    kp <- urca::ur.kpss(v, type = "mu")
    expect_equal(st$adf$statistic, unname(adf@teststat[1]), tolerance = 1e-10)
    expect_equal(st$adf$crit_5pct, unname(adf@cval[1, "5pct"]))
    expect_equal(st$kpss$statistic, kp@teststat, tolerance = 1e-10)
    expect_equal(st$kpss$crit_5pct, unname(kp@cval[1, "5pct"]))
  }
})

test_that("morie_ts_stationarity never returns NA statistics", {
  set.seed(7)
  st <- morie_ts_stationarity(cumsum(stats::rnorm(80)))
  expect_true(is.finite(st$adf$statistic) && is.finite(st$kpss$statistic))
  expect_true(is.finite(st$adf$crit_5pct) && is.finite(st$kpss$crit_5pct))
})

test_that("native Johansen matches urca::ca.jo for every ecdet / spec / K", {
  skip_if_not_installed("urca")
  i <- 0L
  for (ec in c("none", "const", "trend")) for (sp in c("longrun", "transitory")) {
    i <- i + 1L
    Y <- .sim_coint(300 + i, n = 60 + 25 * i, P = 2L + i %% 3L)
    K <- 2L + i %% 3L
    a <- rmorie:::.morie_johansen_native(Y, K, ec, sp)
    bt <- urca::ca.jo(Y, type = "trace", ecdet = ec, K = K, spec = sp)
    be <- urca::ca.jo(Y, type = "eigen", ecdet = ec, K = K, spec = sp)
    expect_equal(a$lambda, bt@lambda, tolerance = 1e-8)
    expect_equal(a$trace, bt@teststat, tolerance = 1e-8)
    expect_equal(a$maxeig, be@teststat, tolerance = 1e-8)
    expect_equal(unname(a$V), unname(bt@V), tolerance = 1e-8)
    expect_equal(unname(a$W), unname(bt@W), tolerance = 1e-8)
    expect_equal(unname(a$PI), unname(bt@PI), tolerance = 1e-8)
    expect_equal(a$cval_trace, bt@cval)
    expect_equal(a$cval_eigen, be@cval)
  }
})

test_that("morie_johansen_cointegration matches ca.jo(K = max(k_ar_diff + 1, 2))", {
  skip_if_not_installed("urca")
  Y <- .sim_coint(41, n = 150, P = 3L)
  for (kd in 0:3) {
    r <- morie_johansen_cointegration(Y, k_ar_diff = kd)
    j <- urca::ca.jo(Y, type = "trace", ecdet = "none", K = max(kd + 1, 2))
    je <- urca::ca.jo(Y, type = "eigen", ecdet = "none", K = max(kd + 1, 2))
    expect_equal(r$eigenvalues, j@lambda, tolerance = 1e-8)
    expect_equal(r$trace_stat, j@teststat, tolerance = 1e-8)
    expect_equal(r$crit_values, j@cval)
    expect_equal(r$rank, sum(j@teststat > j@cval[, "5pct"]))
    jo <- attr(r, "johansen")
    expect_equal(jo$max_eigen_stat, je@teststat, tolerance = 1e-8)
    expect_equal(unname(jo$eigenvectors), unname(j@V), tolerance = 1e-8)
    expect_equal(jo$K, max(kd + 1, 2))
  }
})

test_that("Johansen and EG run on unnamed input without urca", {
  set.seed(5)
  w <- cumsum(stats::rnorm(100))
  Y <- cbind(w + stats::rnorm(100), 0.5 * w + stats::rnorm(100))
  r <- morie_johansen_cointegration(Y)
  expect_named(r, c("eigenvalues", "trace_stat", "crit_values", "rank",
                    "n", "k", "method"))
  expect_true(all(is.finite(r$trace_stat)))
  expect_true(is.finite(morie_eg_coint(Y[, 1], Y[, 2])$adf_statistic))
})
