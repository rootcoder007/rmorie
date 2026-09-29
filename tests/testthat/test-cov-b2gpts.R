# Coverage tests for R/b2gpts.R: Gaussian-process regression (Rasmussen
# and Williams 2006, ch. 2) and simple forecasting / hierarchical
# reconciliation (Hyndman and Athanasopoulos, FPP3).

gp_X <- cbind(c(0, 0.5, 1.1, 1.9, 2.4, 3.2), c(1, 0.2, -0.4, 0.3, 0.9, -1))
gp_y <- c(0.3, 0.8, 1.1, 0.4, -0.2, -0.9)
gp_T <- cbind(c(0.7, 2.8), c(0.1, 0.2))
se_k <- function(A, B, sf = 1.3, l = 0.8) {
  d2 <- outer(A[, 1], B[, 1], "-")^2 + outer(A[, 2], B[, 2], "-")^2
  sf^2 * exp(-d2 / (2 * l^2))
}

test_that("GP posterior mean, variance and marginal likelihood (eqs. 2.23-2.30)", {
  K <- se_k(gp_X, gp_X) + 0.2^2 * diag(6)
  Ks <- se_k(gp_T, gp_X)
  m <- as.numeric(Ks %*% solve(K, gp_y))
  v <- 1.3^2 - rowSums(Ks * t(solve(K, t(Ks))))
  L <- chol(K)
  ll <- -0.5 * sum(gp_y * solve(K, gp_y)) - sum(log(diag(L))) - 3 * log(2 * pi)
  g <- Gpreg(gp_X, gp_y, gp_T, kernel = c(1.3, 0.8), noise = 0.2)
  expect_equal(g$estimate, m, tolerance = 1e-12)
  expect_equal(g$variance, v, tolerance = 1e-12)
  expect_equal(g$loglik, ll, tolerance = 1e-12)
  p <- Gppost(gp_X, gp_y, gp_T, kernel = c(1.3, 0.8), noise = 0.2)
  expect_equal(p$estimate, m, tolerance = 1e-12)
  expect_equal(p$weights, as.numeric(solve(K, gp_y)), tolerance = 1e-12)
  gv <- Gpvar(gp_X, gp_T, kernel = c(1.3, 0.8), sigma2 = 0.04)
  expect_equal(gv$estimate, v, tolerance = 1e-12)
  expect_equal(gv$prior, rep(1.69, 2), tolerance = 1e-12)
  s <- Srfintp(gp_X, gp_y, gp_T, method = "kriging", kernel = c(1.3, 0.8), noise = 0.2)
  expect_equal(s$estimate, m, tolerance = 1e-12)
  expect_equal(s$method_used, "kriging")
  # a user kernel function is used as given
  lin <- function(a, b) sum(a * b) + 1
  gl <- Gpreg(gp_X, gp_y, gp_T, kernel = lin, noise = 0.3)
  Kl <- tcrossprod(gp_X) + 1 + 0.09 * diag(6)
  expect_equal(gl$estimate, as.numeric((tcrossprod(gp_T, gp_X) + 1) %*% solve(Kl, gp_y)), tolerance = 1e-12)
  expect_error(Gpreg(gp_X, gp_y[-1], gp_T), "one entry per row")
  expect_error(Gpreg(gp_X, gp_y, gp_T, kernel = c(1, 0)), "length-scale")
  expect_error(Srfintp(gp_X, gp_y, gp_T, method = "idw"), "gp' or 'kriging")
})

test_that("GP smoothing of parametric residuals", {
  pred <- 0.2 * gp_X[, 1]
  r <- gp_y - pred
  K0 <- se_k(gp_X, gp_X)
  K <- K0 + 0.25 * diag(6)
  g <- Gpresid(gp_X, gp_y, pred, kernel = c(1.3, 0.8), noise = 0.5)
  expect_equal(g$estimate, as.numeric(K0 %*% solve(K, r)), tolerance = 1e-12)
  expect_equal(g$fitted, pred + g$estimate, tolerance = 1e-12)
  expect_equal(g$residual, r)
  expect_error(Gpresid(gp_X, gp_y, pred[-1]), "one entry per row")
})

test_that("naive and seasonal naive forecasts", {
  y <- c(5, 7, 6, 9, 8, 10, 12, 11)
  n <- Naivefc(y, h = 3)
  expect_equal(n$estimate, rep(11, 3))
  expect_equal(n$residual_sd, sd(diff(y)), tolerance = 1e-12)
  expect_error(Naivefc(y, h = 0), "at least 1")
  s <- Snaivefc(y, m = 3, h = 5)
  expect_equal(s$estimate, c(10, 12, 11, 10, 12))
  expect_equal(s$residual_sd, sd(y[4:8] - y[1:5]), tolerance = 1e-12)
  expect_error(Snaivefc(y[1:2], m = 3), "full season")
})

test_that("bottom-up, top-down and middle-out reconciliation", {
  S <- rbind(c(1, 1, 1, 1), c(1, 1, 0, 0), c(0, 0, 1, 1), diag(4))
  b <- c(2, 3, 1.5, 4)
  bu <- Bottomup(b, S)
  expect_equal(bu$estimate, as.numeric(S %*% b))
  expect_equal(bu$total, 10.5)
  expect_error(Bottomup(b[-1], S), "one column")
  td <- Topdown(20, c(1, 3, 4))
  expect_equal(td$estimate, 20 * c(1, 3, 4) / 8)
  expect_equal(sum(td$estimate), 20)
  expect_error(Topdown(20, c(1, -1)), "non-negative")
  Sm <- rbind(c(1, 1), c(1, 0), c(0, 1), c(0.4, 0), c(0.6, 0))
  mo <- Middleout(c(10, 6), Sm)
  expect_equal(mo$estimate, c(16, 10, 6, 4, 6))
  expect_equal(mo$aggregated, c(16, 10, 6))
  expect_equal(mo$disaggregated, c(4, 6))
})
