# SPDX-License-Identifier: AGPL-3.0-or-later
# Kleibergen-Paap rk and Montiel Olea-Pflueger effective F: every expected
# value recomputed in the test body from its definition.

.ivw_data <- function(n = 600L, seed = 11L) {
  set.seed(seed)
  w <- rnorm(n); z1 <- rnorm(n); z2 <- rnorm(n); z3 <- rnorm(n); u <- rnorm(n)
  x1 <- 0.25 * z1 + 0.15 * z2 + 0.3 * w + u * (1 + abs(z1)) / 2
  x2 <- 0.2 * z3 - 0.1 * z1 + rnorm(n) + 0.3 * u
  data.frame(y = 0.5 * x1 + u + rnorm(n), x1, x2, z1, z2, z3, w,
             g = sample(1:40, n, TRUE))
}

test_that("with one endogenous regressor the KP rk statistic is the HC0 robust first-stage Wald", {
  d <- .ivw_data()
  n <- nrow(d)
  kp <- morie_iv_kleibergen_paap(d, "x1", c("z1", "z2", "z3"), "w")
  M <- cbind(1, d$w)
  xr <- qr.resid(qr(M), d$x1)
  Zr <- qr.resid(qr(M), as.matrix(d[, c("z1", "z2", "z3")]))
  b <- solve(crossprod(Zr), crossprod(Zr, xr))
  e <- as.numeric(xr - Zr %*% b)
  A <- solve(crossprod(Zr))
  wald <- as.numeric(t(b) %*% solve(A %*% crossprod(Zr * e) %*% A, b))
  expect_equal(kp$chi2_statistic, wald, tolerance = 1e-10)
  expect_equal(kp$statistic, wald / n * (n - 5) / 3, tolerance = 1e-10)
  expect_equal(kp$p_value, pchisq(wald, 3, lower.tail = FALSE), tolerance = 1e-10)
  expect_identical(kp$weak, kp$statistic < 10)
})

test_that("under iid errors KP is the classical F (one regressor) and Cragg-Donald (two)", {
  d <- .ivw_data()
  n <- nrow(d)
  f1 <- anova(lm(x1 ~ w, d), lm(x1 ~ w + z1 + z2 + z3, d))$F[2]
  expect_equal(morie_iv_kleibergen_paap(d, "x1", c("z1", "z2", "z3"), "w", vcov = "iid")$statistic,
               f1, tolerance = 1e-10)
  M <- cbind(1, d$w)
  r2 <- min(cancor(qr.resid(qr(M), as.matrix(d[, c("x1", "x2")])),
                   qr.resid(qr(M), as.matrix(d[, c("z1", "z2", "z3")])),
                   xcenter = FALSE, ycenter = FALSE)$cor)^2
  kp2 <- morie_iv_kleibergen_paap(d, c("x1", "x2"), c("z1", "z2", "z3"), "w", vcov = "iid")
  expect_equal(kp2$statistic, r2 / (1 - r2) * (n - 5) / 3, tolerance = 1e-10)
  expect_identical(kp2$df, 2L)
})

test_that("clustered KP sums scores within clusters and applies the (G-1)/G scaling", {
  d <- .ivw_data()
  n <- nrow(d)
  kp <- morie_iv_kleibergen_paap(d, "x1", c("z1", "z2"), vcov = "cluster", cluster = "g")
  M <- matrix(1, n, 1)
  xr <- qr.resid(qr(M), d$x1)
  Zr <- qr.resid(qr(M), as.matrix(d[, c("z1", "z2")]))
  b <- solve(crossprod(Zr), crossprod(Zr, xr))
  e <- as.numeric(xr - Zr %*% b)
  A <- solve(crossprod(Zr))
  Sg <- rowsum(Zr * e, d$g)
  wald <- as.numeric(t(b) %*% solve(A %*% crossprod(Sg) %*% A, b))
  G <- length(unique(d$g))
  expect_equal(kp$chi2_statistic, wald, tolerance = 1e-10)
  expect_equal(kp$statistic, wald / (n - 1) * (n - 3) * (G - 1) / G / 2, tolerance = 1e-10)
  expect_error(morie_iv_kleibergen_paap(d, "x1", "z1", vcov = "cluster"), "needs `cluster`")
  expect_error(morie_iv_kleibergen_paap(d, c("x1", "x2"), "z1"), "fewer excluded instruments")
})

test_that("the effective F is Y2'P_Z Y2 / tr(W2) on partialled, orthonormal instruments", {
  d <- .ivw_data()
  n <- nrow(d)
  m <- morie_iv_montiel_olea_pflueger(d, "y", "x1", c("z1", "z2", "z3"), "w")
  M <- cbind(1, d$w)
  xr <- qr.resid(qr(M), d$x1)
  Zr <- qr.resid(qr(M), as.matrix(d[, c("z1", "z2", "z3")]))
  # the same quantity in the pi' Q pi / tr(Sigma Q) form, Sigma the HC sandwich
  fit <- lm.fit(Zr, xr)
  e <- fit$residuals
  A <- solve(crossprod(Zr))
  Sigma <- A %*% crossprod(Zr * e) %*% A * n / (n - 3 - 1 - 1)
  pi <- fit$coefficients
  expect_equal(m$F_eff, as.numeric(t(pi) %*% crossprod(Zr) %*% pi) /
                 sum(diag(Sigma %*% crossprod(Zr))), tolerance = 1e-10)
  # homoskedastic: the ordinary first-stage F
  f1 <- anova(lm(x1 ~ w, d), lm(x1 ~ w + z1 + z2 + z3, d))$F[2]
  expect_equal(morie_iv_montiel_olea_pflueger(d, "y", "x1", c("z1", "z2", "z3"), "w",
                                              vcov = "iid")$F_eff, f1, tolerance = 1e-10)
})

test_that("Patnaik critical values: one instrument is chi2(1, 1/tau), K_eff from W2's eigenvalues", {
  d <- .ivw_data()
  m1 <- morie_iv_montiel_olea_pflueger(d, "y", "x1", "z1")
  expect_equal(m1$critical_values$simplified, qchisq(0.95, 1, 1 / c(0.05, 0.10, 0.20, 0.30)),
               tolerance = 1e-10)
  expect_equal(m1$critical_values$K_eff_simplified, rep(1, 4), tolerance = 1e-12)
  m <- morie_iv_montiel_olea_pflueger(d, "y", "x1", c("z1", "z2", "z3"), "w", tau = 0.10)
  # recompute the simplified value from the definition with W2 rebuilt here
  n <- nrow(d)
  M <- cbind(1, d$w)
  Q <- qr.Q(qr(qr.resid(qr(M), as.matrix(d[, c("z1", "z2", "z3")])))) * sqrt(n)
  e2 <- qr.resid(qr(Q), qr.resid(qr(M), d$x1))
  W2 <- crossprod(e2 * Q) / (n - 3 - 1 - 1)
  w <- eigen(W2, symmetric = TRUE)$values / sum(diag(W2))
  x <- 10
  Ke <- 2 * (1 + 2 * x) / (2 * sum(w^2) + 4 * x * max(w))
  expect_equal(m$critical_values$simplified, qchisq(0.95, Ke, Ke * x) / Ke, tolerance = 1e-10)
  # the worst-case TSLS bias bound is at most 1, so its critical value never exceeds the simplified
  expect_lte(m$B_tsls, 1 + 1e-12)
  expect_lte(m$critical_values$tsls, m$critical_values$simplified + 1e-10)
  expect_identical(unname(m$reject), m$F_eff > m$critical_values$tsls)
})

test_that("cluster and HAC effective F, and the one-regressor restriction", {
  d <- .ivw_data()
  n <- nrow(d)
  mc <- morie_iv_montiel_olea_pflueger(d, "y", "x1", c("z1", "z2"), vcov = "cluster", cluster = "g")
  M <- matrix(1, n, 1)
  Q <- qr.Q(qr(qr.resid(qr(M), as.matrix(d[, c("z1", "z2")])))) * sqrt(n)
  x2 <- qr.resid(qr(M), d$x1)
  e2 <- qr.resid(qr(Q), x2)
  W2 <- crossprod(rowsum(e2 * Q, d$g)) / (n - 2 - 1)
  G <- length(unique(d$g))
  expect_equal(mc$F_eff, (n / (n - 1)) * (G - 1) / G * sum(crossprod(Q, x2)^2) / n / sum(diag(W2)),
               tolerance = 1e-10)
  mh <- morie_iv_montiel_olea_pflueger(d, "y", "x1", c("z1", "z2"), vcov = "hac", lag = 2L)
  S <- e2 * Q
  Wh <- crossprod(S)
  for (j in 1:2) {
    Gj <- crossprod(S[(j + 1):n, ], S[1:(n - j), ])
    Wh <- Wh + (1 - j / 3) * (Gj + t(Gj))
  }
  expect_equal(mh$F_eff, sum(crossprod(Q, x2)^2) / n / (sum(diag(Wh)) / (n - 3)), tolerance = 1e-10)
  expect_error(morie_iv_montiel_olea_pflueger(d, "y", c("x1", "x2"), c("z1", "z2")),
               "one endogenous regressor")
})

test_that("Stock-Yogo outside its rows names the computed alternatives; 2SLS reports the effective F", {
  expect_error(morie_iv_stock_yogo(2, 5), "morie_iv_montiel_olea_pflueger")
  d <- .ivw_data()
  fit <- suppressWarnings(morie_iv_2sls(d, "y", "x1", c("z1", "z2", "z3", "w")))
  expect_true(is.na(fit$stock_yogo_10))
  mop <- morie_iv_montiel_olea_pflueger(d, "y", "x1", c("z1", "z2", "z3", "w"), tau = 0.10)
  expect_equal(fit$effective_F, mop$F_eff, tolerance = 1e-12)
  expect_equal(fit$mop_critical_10, mop$critical_values$tsls, tolerance = 1e-12)
})
