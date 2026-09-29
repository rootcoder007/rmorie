n <- 16
P <- cbind((0:15) %% 4, (0:15) %/% 4)
X <- matrix((0.3 * (0:15)) %% 1.7)
y <- 1 + 2 * X[, 1] + 0.1 * P[, 1] + 0.05 * ((0:15) %% 3) + 0.2 * sin(0:15)
bw <- 2.5
D <- unname(as.matrix(stats::dist(P)))
Xa <- cbind(1, X)
Wi <- function(i) exp(-D[i, ]^2 / (2 * bw^2))
loc <- function(i, w = Wi(i)) solve(t(Xa) %*% (w * Xa), t(Xa * w))

test_that("coefficients, residuals, leverages and CV recompute", {
  B <- gwrcoef(y, X, P, bw, kernel = "gaussian")
  for (i in c(1, 6, 16)) expect_equal(B[i, ], as.vector(loc(i) %*% y), tolerance = 1e-12)
  fit <- vapply(1:n, function(i) sum(Xa[i, ] * (loc(i) %*% y)), 0)
  expect_equal(gwrres(y, X, P, bw, kernel = "gaussian"), y - fit, tolerance = 1e-12)
  S <- t(vapply(1:n, function(i) as.vector(Xa[i, ] %*% loc(i)), numeric(n)))
  expect_equal(gwrhat(y, X, P, bw, kernel = "gaussian"), diag(S), tolerance = 1e-12)
  cv <- sum(vapply(1:n, function(i) {
    w <- Wi(i)
    w[i] <- 0
    (y[i] - sum(Xa[i, ] * (loc(i, w) %*% y)))^2
  }, 0))
  expect_equal(gwrcv(y, X, P, bw, kernel = "gaussian"), cv, tolerance = 1e-12)
  s2 <- sum((y - fit)^2) / (n - 2 * sum(diag(S)) + sum(S^2))
  expect_equal(gwrstd(y, X, P, bw, kernel = "gaussian")[4, 2], sqrt(s2 * sum(loc(4)[2, ]^2)), tolerance = 1e-12)
  eo <- stats::lm.fit(Xa, y)$residuals
  expect_equal(gwrdlt(y, X, P, bw, kernel = "gaussian")$F4, sum((y - fit)^2) / sum(eo^2), tolerance = 1e-12)
})

test_that("Monte Carlo test, FWL and SUR recompute", {
  r <- gwrtst(y, X, P, bw, nsim = 19, seed = 4, kernel = "gaussian")
  B <- gwrcoef(y, X, P, bw, kernel = "gaussian")
  expect_equal(r$observed_variance, apply(B, 2, var), tolerance = 1e-12)
  expect_equal(r$p_values[2], 1 - (1 + sum(r$simulated_variance[, 2] < r$observed_variance[2])) / 20)
  X2 <- cbind(X, cos(0:15))
  f <- gwrfwl(y, matrix(P[, 1]), X2, P, bw, kernel = "gaussian")
  Z <- cbind(1, P[, 1], X2)
  for (i in c(1, 10)) {
    w <- Wi(i)
    expect_equal(f$betas[i, ], as.vector(solve(t(Z) %*% (w * Z), t(Z) %*% (w * y)))[3:4], tolerance = 1e-10)
  }
  y2 <- 0.5 - X[, 1] + 0.2 * P[, 2] + 0.3 * cos(2 * (0:15))
  s <- gwrsur(list(y, y2), X, P, bw, kernel = "gaussian")
  e1 <- y - rowSums(Xa * B)
  e2 <- y2 - rowSums(Xa * gwrcoef(y2, X, P, bw, kernel = "gaussian"))
  expect_equal(s$local_covariance[[7]][1, 2], sum(Wi(7) * e1 * e2) / sum(Wi(7)), tolerance = 1e-12)
})

test_that("GW Poisson and logistic reach the local-scoring fixed point", {
  P2 <- cbind((0:19) %% 5, (0:19) %/% 5)
  X2 <- matrix((0.37 * (0:19)) %% 1.3)
  X2a <- cbind(1, X2)
  D2 <- unname(as.matrix(stats::dist(P2)))
  yp <- ((0:19) * 7) %% 5 + (0:19) %/% 5
  r <- gwrpois(yp, X2, P2, 4, kernel = "gaussian")
  mu <- r$fitted
  z <- log(mu) + (yp - mu) / mu
  w <- exp(-D2[1, ]^2 / 32) * mu
  expect_equal(r$betas[1, ], as.vector(solve(t(X2a) %*% (w * X2a), t(X2a) %*% (w * z))), tolerance = 1e-6)
  expect_equal(r$loglik, sum(stats::dpois(yp, mu, log = TRUE)), tolerance = 1e-12)
  yb <- as.numeric(((0:19) * 7) %% 3 == 0)
  b <- gwrlgt(yb, X2, P2, 6, kernel = "gaussian")
  mu <- b$fitted
  z <- log(mu / (1 - mu)) + (yb - mu) / (mu * (1 - mu))
  w <- exp(-D2[1, ]^2 / 72) * mu * (1 - mu)
  expect_equal(b$betas[1, ], as.vector(solve(t(X2a) %*% (w * X2a), t(X2a) %*% (w * z))), tolerance = 1e-6)
})
