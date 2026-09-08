# Cross-language anchors from morie.fn._garch on the same inputs,
# stored at full precision (testthat tolerance is relative).

.E <- c(0.5, -1.2, 0.3, 0.8, -0.4, 1.1, -0.9, 0.2, 0.6, -0.7)

test_that("recursions match the Python core and the Tsay equations", {
  g <- morie_garch_recursion(.E, list(omega = 0.05, alpha = 0.1, beta = 0.85), "garch")
  expect_equal(g[1:4], c(0.5481, 0.540885, 0.65375225, 0.6146894125), tolerance = 1e-10)
  # IGARCH, Tsay p.141: the two variance weights sum to exactly one
  i <- morie_garch_recursion(.E, list(omega = 0.01, beta = 0.9), "igarch")
  expect_equal(i[1:4], c(0.5481, 0.52829, 0.629461, 0.5855149), tolerance = 1e-10)
  v0 <- mean((.E - mean(.E))^2)  # population variance, as in the core
  expect_equal(i[2], 0.01 + 0.9 * v0 + 0.1 * 0.25, tolerance = 1e-12)
  # GJR eq. (3.34): gamma loads only on the negative lagged shock
  j <- morie_garch_recursion(
    .E, list(omega = 0.05, alpha = 0.05, gamma = 0.1, beta = 0.85), "gjr"
  )
  expect_equal(j[1:4], c(0.5481, 0.528385, 0.71512725, 0.6623581625), tolerance = 1e-10)
  expect_equal(j[2], 0.05 + 0.05 * 0.25 + 0.85 * v0, tolerance = 1e-12)
  expect_equal(j[3], 0.05 + 0.15 * 1.44 + 0.85 * j[2], tolerance = 1e-12)
  # APARCH with delta = 2 and gamma = 0 nests plain GARCH
  a <- morie_garch_recursion(
    .E, list(omega = 0.05, alpha = 0.1, gamma = 0, beta = 0.85, delta = 2), "aparch"
  )
  expect_equal(a, g, tolerance = 1e-10)
  expect_error(morie_garch_recursion(.E[1:3], list(omega = 1), "garch"))
})

test_that("garch_spec_fit recovers parameters and extends morie_garch_fit", {
  set.seed(7)
  n <- 1500
  e <- numeric(n)
  s2 <- 1
  for (t in seq_len(n)) {
    if (t > 1) s2 <- 0.05 + 0.1 * e[t - 1]^2 + 0.85 * s2
    e[t] <- sqrt(s2) * stats::rnorm(1)
  }
  f <- morie_garch_spec_fit(e, "garch")
  # absolute bounds: testthat's tolerance is RELATIVE, so tolerance =
  # 0.06 on a target of 0.1 would demand agreement to 0.006
  expect_lt(abs(f$params$alpha - 0.1), 0.06)
  expect_lt(abs(f$params$beta - 0.85), 0.1)
  expect_lt(f$persistence, 1)
  expect_true(all(f$sigma2 > 0))
  # IGARCH persistence is exactly one by construction
  expect_equal(morie_garch_spec_fit(e, "igarch")$persistence, 1)
  expect_true(all(morie_garch_spec_fit(e, "gjr")$sigma2 > 0))
  expect_error(morie_garch_spec_fit(e[1:10]))
  expect_error(morie_garch_spec_fit(rep(1, 100)))
})

test_that("BEKK keeps every conditional covariance positive definite", {
  set.seed(8)
  R <- matrix(stats::rnorm(600), ncol = 3)
  out <- morie_bekk_garch(R)
  expect_equal(dim(out$H), c(200L, 3L, 3L))
  for (t in c(1L, 50L, 200L)) {
    expect_true(all(eigen(out$H[t, , ], only.values = TRUE)$values > 0))
  }
  # variance targeting: the long-run covariance IS the sample one
  expect_equal(out$H_bar, stats::cov(scale(R, TRUE, FALSE)), tolerance = 1e-10)
  expect_true(out$persistence > 0 && out$persistence < 1)
  expect_error(morie_bekk_garch(R[, 1, drop = FALSE]))
})

test_that("VaR and expected shortfall match Python and order correctly", {
  v <- morie_garch_var_es(0, 1, 0.05)
  expect_equal(v$var, 1.6448536269514729, tolerance = 1e-12)
  expect_equal(v$es, 2.0627128075074253, tolerance = 1e-12)
  expect_gt(v$es, v$var) # ES always lies beyond VaR
  tt <- morie_garch_var_es(0, 1, 0.01, "t", 4)
  expect_equal(tt$var, 2.64949190678931, tolerance = 1e-8)
  expect_equal(tt$es, 5.22058419449222, tolerance = 1e-8)
  expect_gt(tt$var, morie_garch_var_es(0, 1, 0.01)$var) # fatter tail, wider VaR
  # VaR scales in sigma and shifts with mu
  expect_equal(morie_garch_var_es(0, 2, 0.05)$var, 2 * v$var)
  expect_equal(morie_garch_var_es(0.5, 1, 0.05)$var, v$var - 0.5)
  expect_error(morie_garch_var_es(0, -1))
  expect_error(morie_garch_var_es(0, 1, 1.5))
  expect_error(morie_garch_var_es(0, 1, 0.05, "t", nu = 1.5))
})

test_that("Holt extrapolates a linear trend and damping flattens it", {
  y <- seq_len(40) * 2 + 5
  out <- morie_holt_linear(y, horizon = 5)
  expect_equal(out$forecast, y[40] + seq_len(5) * 2, tolerance = 0.02)
  expect_lt(out$sse, 1e-2)
  expect_lt(
    morie_holt_linear(y, horizon = 30, damped = TRUE, phi = 0.9)$forecast[30],
    morie_holt_linear(y, horizon = 30)$forecast[30]
  )
  expect_error(morie_holt_linear(y, alpha = 1.5))
  expect_error(morie_holt_linear(y[1:3]))
})

test_that("Holt-Winters recovers the seasonal shape", {
  set.seed(9)
  m <- 12
  season <- c(3, 1, -2, -4, -1, 2, 5, 4, 1, -1, -3, -5)
  y <- rep(season, 8) + seq_len(96) * 0.5 + 10 + stats::rnorm(96, 0, 0.5)
  fc <- morie_holt_winters(y, m = m, horizon = 12)$forecast
  # the forecast carries the trend too, so detrend before comparing:
  # subtracting only the mean leaves a ramp that caps the correlation
  tt <- seq_len(12)
  fcd <- fc - stats::fitted(stats::lm(fc ~ tt))
  expect_gt(stats::cor(fcd, season), 0.9)
  expect_error(morie_holt_winters(y[1:10], m = 12))
  # multiplicative refuses non-positive data instead of returning Inf
  expect_error(morie_holt_winters(c(y, 0), m = 12, seasonal = "multiplicative"))
})

test_that("hierarchical reconciliation is coherent", {
  S <- rbind(c(1, 1), c(1, 0), c(0, 1))
  expect_equal(morie_reconcile_hierarchy(c(3, 4), S)$reconciled, c(7, 3, 4))
  # an incoherent base vector is repaired, and the aggregate is used
  rec <- morie_reconcile_hierarchy(NULL, S, base = c(10, 3, 4), method = "ols")$reconciled
  expect_equal(rec[1], rec[2] + rec[3], tolerance = 1e-10)
  expect_gt(rec[1], 7)
  set.seed(10)
  Rm <- matrix(stats::rnorm(150), ncol = 3) %*% diag(c(1, 5, 0.1))
  w <- morie_reconcile_hierarchy(NULL, S, base = c(10, 3, 4), method = "wls",
                                 residuals = Rm)$reconciled
  expect_equal(w[1], w[2] + w[3], tolerance = 1e-10)
  expect_error(morie_reconcile_hierarchy(NULL, S, method = "ols"))
  expect_error(morie_reconcile_hierarchy(1, S))
})

test_that("Aalen-Johansen rows are probabilities", {
  out <- morie_aalen_johansen(1:6, c(0, 0, 0, 1, 0, 1), c(1, 1, 2, 2, 1, 2), 3)
  expect_equal(dim(out$P), c(3L, 3L))
  expect_equal(rowSums(out$P), rep(1, 3), tolerance = 1e-12)
  expect_true(all(out$P >= -1e-12))
  for (dA in out$increments) expect_equal(rowSums(dA), rep(0, 3), tolerance = 1e-12)
  expect_error(morie_aalen_johansen(1:6, c(0, 0, 0, 1, 0, 1), c(1, 1, 2, 2, 1, 2), 2))
})
