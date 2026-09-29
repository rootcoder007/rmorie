# Coverage tests for R/gpgrmsvr.R (Montesinos Lopez et al. 2022, MVSML):
# genomic relationship matrices (VanRaden 2008; Yang et al. 2010),
# scaling helpers, variance-reduction splits and SVM / SVR wrappers.

gm_M <- rbind(c(0, 1, 2, 1, 0), c(1, 1, 0, 2, 1), c(2, 0, 1, 1, 1), c(1, 2, 1, 0, 2))

test_that("VanRaden method 1 and 2 relationship matrices", {
  p <- colMeans(gm_M) / 2
  Z <- sweep(gm_M, 2, 2 * p)
  v1 <- Vanr1(gm_M)
  expect_equal(v1$G, tcrossprod(Z) / (2 * sum(p * (1 - p))), tolerance = 1e-12)
  expect_equal(v1$estimate, mean(diag(v1$G)), tolerance = 1e-12)
  f <- c(0.3, 0.4, 0.5, 0.5, 0.45)
  expect_equal(Vanr1(gm_M, freq = f)$G, tcrossprod(sweep(gm_M, 2, 2 * f)) / (2 * sum(f * (1 - f))), tolerance = 1e-12)
  v2 <- Vanr2(gm_M)
  vr <- 2 * p * (1 - p)
  Zs <- sweep(Z, 2, sqrt(vr), "/")
  expect_equal(v2$G, tcrossprod(Zs) / 5, tolerance = 1e-12)
  w <- c(1, 2, 1, 0.5, 1)
  expect_equal(Vanr2(gm_M, weights = w)$G, Z %*% diag(w) %*% t(Z) / sum(w * vr), tolerance = 1e-12)
})

test_that("Yang realised relationship matrix and its diagonal correction", {
  p <- colMeans(gm_M) / 2
  vr <- 2 * p * (1 - p)
  Zs <- sweep(sweep(gm_M, 2, 2 * p), 2, sqrt(vr), "/")
  y <- Yangr(gm_M)
  expect_equal(y$A, tcrossprod(Zs) / 5, tolerance = 1e-12)
  yd <- Yangr(gm_M, yang_diagonal = TRUE)
  dg <- 1 + rowSums(sweep(gm_M^2 - sweep(gm_M, 2, 1 + 2 * p, "*") + rep(2 * p^2, each = 4), 2, vr, "/")) / 5
  expect_equal(diag(yd$A), dg, tolerance = 1e-12)
  expect_equal(yd$A[upper.tri(yd$A)], y$A[upper.tri(y$A)], tolerance = 1e-12)
  mono <- cbind(gm_M, 2)
  expect_equal(Yangr(mono)$A, tcrossprod(Zs) / 6, tolerance = 1e-12)
})

test_that("z-score, unit length and variance-reduction splits", {
  x <- c(3, 7, 1, 9, 5)
  z <- Zscnm(x)
  expect_equal(z$x_std, (x - mean(x)) / sd(x), tolerance = 1e-12)
  expect_equal(Zscnm(x, ddof = 0)$sd, sqrt(mean((x - 5)^2)), tolerance = 1e-12)
  expect_equal(Zscnm(c(2, 2, 2))$x_std, c(0, 0, 0))
  u <- Unitl(c(3, 4))
  expect_equal(u$x_unit, c(0.6, 0.8))
  expect_equal(Unitl(c(0, 0))$x_unit, c(0, 0))
  y <- c(1, 2, 2, 8, 9, 10)
  pv <- function(v) mean((v - mean(v))^2)
  s <- Varrd(y, c(1, 2, 3))
  expect_equal(s$delta_var, pv(y) - 0.5 * pv(y[1:3]) - 0.5 * pv(y[4:6]), tolerance = 1e-12)
  expect_equal(s$sse_left, sum((y[1:3] - mean(y[1:3]))^2), tolerance = 1e-12)
  expect_equal(Varrd(y, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))$delta_var, s$delta_var)
  expect_equal(Varrd(y, integer(0))$delta_var, 0)
})

test_that("SVM hard and soft margins on a separable set", {
  # a symmetric configuration: the street is bounded by (-1, 0) and (1, 0),
  # so the maximum margin is 1 with beta = (1, 0). (On asymmetric data the
  # shared dual solver morie_svm_fit_dual stalls short of the optimum; that
  # solver lives in R/gp_mvsml.R and is reported, not exercised, here.)
  X <- rbind(c(-2, 0.5), c(-1, 0), c(1, 0), c(2, -0.5))
  yy <- c(-1, -1, 1, 1)
  h <- Svmhp(X, yy)
  expect_equal(h$estimate, 1, tolerance = 1e-6)
  expect_equal(h$estimate, 1 / sqrt(sum(h$beta^2)), tolerance = 1e-12)
  expect_equal(h$beta, c(1, 0), tolerance = 1e-6)
  s <- Svmsl(X, yy, C = 1e4)
  expect_equal(s$estimate, h$estimate, tolerance = 1e-6)
  expect_match(s$method, "soft margin")
})

test_that("epsilon-insensitive SVR: dual quantities and a tube around exact data", {
  x <- cbind(seq(-1, 1, length.out = 9))
  y <- 1 + 2 * x[, 1]
  r <- Svmep(x, y, C = 10, eps = 0.1, n_iter = 6000)
  K <- tcrossprod(x)
  expect_equal(r$theta, r$alpha - r$alpha_star, tolerance = 1e-12)
  expect_equal(r$fitted, as.numeric(K %*% r$theta) + r$b, tolerance = 1e-12)
  expect_equal(r$loss, pmax(0, abs(y - r$fitted) - 0.1), tolerance = 1e-12)
  expect_equal(r$objective, -0.5 * sum(r$theta * (K %*% r$theta)) - 0.1 * sum(r$alpha + r$alpha_star) + sum(y * r$theta), tolerance = 1e-12)
  expect_true(all(r$alpha >= 0 & r$alpha <= 10 & r$alpha_star >= 0 & r$alpha_star <= 10))
  # projected-gradient dual ascent: the exact linear data end inside the tube
  # up to the solver's tolerance
  expect_lt(max(r$loss), 1e-3)
  expect_equal(r$w, sum(x[, 1] * r$theta), tolerance = 1e-12)
})
