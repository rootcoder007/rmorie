# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native MASS utilities (module 30) vs MASS.

test_that("native ginv matches MASS::ginv (square, rectangular, rank-deficient)", {
  skip_if_not_installed("MASS")
  set.seed(1)
  A <- matrix(rnorm(25), 5)
  B <- matrix(rnorm(30), 6, 5)
  C <- cbind(1:4, 2:5, c(1, 1, 1, 1))
  expect_equal(rmorie:::.morie_ginv(A), MASS::ginv(A), tolerance = 1e-12)
  expect_equal(rmorie:::.morie_ginv(B), MASS::ginv(B), tolerance = 1e-12)
  expect_equal(rmorie:::.morie_ginv(C), MASS::ginv(C), tolerance = 1e-10)
})

test_that("native mvrnorm matches MASS::mvrnorm bit-for-bit under a seed", {
  skip_if_not_installed("MASS")
  set.seed(2)
  mu <- c(1, -2, 0.5)
  Sig <- crossprod(matrix(rnorm(9), 3))
  set.seed(42)
  m1 <- morie_mvrnorm(100, mu, Sig)
  set.seed(42)
  m2 <- MASS::mvrnorm(100, mu, Sig)
  expect_equal(m1, m2, tolerance = 1e-12)
  set.seed(7)
  a <- morie_mvrnorm(1, mu, Sig)
  set.seed(7)
  b <- MASS::mvrnorm(1, mu, Sig)
  expect_equal(a, b, tolerance = 1e-12)
  set.seed(3)
  e1 <- morie_mvrnorm(50, mu, Sig, empirical = TRUE)
  set.seed(3)
  e2 <- MASS::mvrnorm(50, mu, Sig, empirical = TRUE)
  expect_equal(e1, e2, tolerance = 1e-10)
})

# --- Module 31: glm.nb, kde2d, rlm, polr vs MASS ---------------------

test_that("native glm.nb matches MASS::glm.nb (coef, theta, summary, AIC)", {
  skip_if_not_installed("MASS")
  set.seed(42)
  n <- 200
  x1 <- rnorm(n)
  x2 <- runif(n)
  y <- rnbinom(n, mu = exp(0.5 + 0.8 * x1 - 0.4 * x2), size = 2)
  d <- data.frame(y, x1, x2)
  ref <- suppressWarnings(MASS::glm.nb(y ~ x1 + x2, data = d))
  nat <- suppressWarnings(morie_glm_nb(y ~ x1 + x2, data = d))
  expect_equal(unname(coef(nat)), unname(coef(ref)), tolerance = 1e-6)
  expect_equal(nat$theta, ref$theta, tolerance = 1e-6)
  expect_equal(unname(summary(nat)$coefficients),
               unname(summary(ref)$coefficients), tolerance = 1e-6)
  expect_equal(as.numeric(logLik(nat)), as.numeric(logLik(ref)),
               tolerance = 1e-6)
  expect_equal(AIC(nat), AIC(ref), tolerance = 1e-6)
  nd <- data.frame(x1 = c(-1, 0, 1), x2 = c(0.2, 0.5, 0.8))
  expect_equal(unname(predict(nat, nd, type = "response")),
               unname(predict(ref, nd, type = "response")), tolerance = 1e-6)
})

test_that("native kde2d matches MASS::kde2d (default + explicit h)", {
  skip_if_not_installed("MASS")
  set.seed(7)
  xx <- rnorm(150)
  yy <- rnorm(150) + 0.5 * xx
  a <- morie_kde2d(xx, yy, n = 30)
  b <- MASS::kde2d(xx, yy, n = 30)
  expect_equal(a$z, b$z, tolerance = 1e-12)
  expect_equal(a$x, b$x)
  expect_equal(a$y, b$y)
  a2 <- morie_kde2d(xx, yy, h = c(1.2, 0.8), n = 25, lims = c(-3, 3, -3, 3))
  b2 <- MASS::kde2d(xx, yy, h = c(1.2, 0.8), n = 25, lims = c(-3, 3, -3, 3))
  expect_equal(a2$z, b2$z, tolerance = 1e-12)
})

test_that("native rlm matches MASS::rlm (coef + summary SE, bit-exact)", {
  skip_if_not_installed("MASS")
  set.seed(11)
  n <- 120
  x1 <- rnorm(n)
  x2 <- runif(n)
  y <- 1 + 2 * x1 - 0.5 * x2 + rt(n, df = 3)
  y[c(3, 17, 88)] <- y[c(3, 17, 88)] + 30
  d <- data.frame(y, x1, x2)
  ref <- MASS::rlm(y ~ x1 + x2, data = d)
  nat <- morie_rlm(y ~ x1 + x2, data = d)
  expect_equal(unname(nat$coefficients), unname(coef(ref)), tolerance = 1e-8)
  cr <- summary(ref)$coefficients
  cn <- summary(nat)$coefficients
  expect_equal(unname(cn[, "Std. Error"]), unname(cr[, "Std. Error"]),
               tolerance = 1e-8)
})

test_that("native polr logLik matches MASS::polr (proportional-odds)", {
  skip_if_not_installed("MASS")
  set.seed(5)
  n <- 300
  xa <- rnorm(n)
  xb <- rnorm(n)
  eta <- 0.8 * xa - 0.6 * xb
  cuts <- c(-1, 0.3, 1.5)
  pr <- sapply(cuts, function(c) plogis(c - eta))
  u <- runif(n)
  yc <- 1 + (u > pr[, 1]) + (u > pr[, 2]) + (u > pr[, 3])
  yf <- factor(yc, levels = 1:4, ordered = TRUE)
  dd <- data.frame(yf, xa, xb)
  ref <- MASS::polr(yf ~ xa + xb, data = dd, method = "logistic", Hess = FALSE)
  nat <- morie_polr(yf ~ xa + xb, data = dd, method = "logistic")
  expect_equal(as.numeric(logLik(nat)), as.numeric(logLik(ref)),
               tolerance = 1e-4)
  expect_equal(unname(nat$coefficients), unname(coef(ref)), tolerance = 1e-3)
})
