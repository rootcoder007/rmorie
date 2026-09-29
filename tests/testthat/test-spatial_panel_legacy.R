N <- 5
T <- 4
W <- 0.5 * (abs(outer(1:N, 1:N, "-")) %in% c(1, N - 1))
dim(W) <- c(N, N)
tid <- rep(0:(T - 1), each = N)
uid <- rep(0:(N - 1), T)
k <- 0:(N * T - 1)
X <- cbind(sin(1.3 * k) + 0.1 * k, cos(0.7 * k))
y <- 1 + 0.8 * X[, 1] - 0.5 * X[, 2] + 0.3 * cos(2.1 * k) + 0.2 * (k %% N)
wi <- function(v) as.vector(matrix(v, N, T) - rowMeans(matrix(v, N, T)))
lg <- function(v) as.vector(W %*% matrix(v, N, T))
cll <- function(rho, yt, Xt) {
  z <- yt - rho * lg(yt)
  e <- stats::lm.fit(Xt, z)$residuals
  -N * T / 2 * log(sum(e^2)) + T * as.numeric(determinant(diag(N) - rho * W)$modulus)
}
yt <- wi(y)
Xt <- apply(X, 2, wi)

test_that("ML panel estimators solve their score equations", {
  r <- sppfe(y, X, W, tid, uid)
  expect_lt(abs((cll(r$rho + 1e-5, yt, Xt) - cll(r$rho - 1e-5, yt, Xt)) / 2e-5), 1e-5)
  p <- c(8, 3, 17, 12, 1, 20, 5, 14, 9, 2, 19, 6, 11, 16, 4, 13, 7, 18, 10, 15)
  expect_equal(sppsar(y[p], X[p, ], W, tid[p], uid[p])$rho, r$rho, tolerance = 1e-14)
  d <- sppsdm(y, X, W, tid, uid)
  Xd <- cbind(Xt, apply(X, 2, function(v) wi(lg(v))))
  expect_lt(abs((cll(d$rho + 1e-5, yt, Xd) - cll(d$rho - 1e-5, yt, Xd)) / 2e-5), 1e-5)
  s <- sppsac(y, X, W, tid, uid)
  c2 <- function(p, q) {
    B <- function(v) v - q * lg(v)
    e <- stats::lm.fit(apply(Xt, 2, B), B(yt - p * lg(yt)))$residuals
    -N * T / 2 * log(sum(e^2)) + T * as.numeric(determinant(diag(N) - p * W)$modulus) +
      T * as.numeric(determinant(diag(N) - q * W)$modulus)
  }
  expect_lt(abs((c2(s$rho + 1e-5, s$lambda) - c2(s$rho - 1e-5, s$lambda)) / 2e-5), 1e-5)
  expect_lt(abs((c2(s$rho, s$lambda + 1e-5) - c2(s$rho, s$lambda - 1e-5)) / 2e-5), 1e-5)
  re <- sppre(y, X, W, tid, uid)
  expect_equal(re$sigma2_mu, (1 / re$phi^2 - 1) * re$sigma2 / T, tolerance = 1e-12)
})

test_that("between, CD, Moran, LM, Hausman, 2SLS, GM and bootstrap recompute", {
  ym <- tapply(y, uid, mean)
  Xm <- cbind(1, tapply(X[, 1], uid, mean), tapply(X[, 2], uid, mean))
  expect_equal(sppbe(y, X, uid)$coefficients, as.vector(solve(crossprod(Xm), crossprod(Xm, ym))), tolerance = 1e-10)
  e <- sin(3.1 * k)
  E <- matrix(e, N, T)
  D <- E - rowMeans(E)
  cr <- cor(t(E))
  expect_equal(sppcov(e, uid, tid)$statistic, sqrt(2 * T / (N * (N - 1))) * sum(cr[upper.tri(cr)]), tolerance = 1e-12)
  Wb <- kronecker(diag(T), W)
  d <- e - mean(e)
  expect_equal(sppres(e, W)$statistic, N * T / sum(Wb) * sum(d * (Wb %*% d)) / sum(d^2), tolerance = 1e-12)
  expect_equal(sppdiag(y, X, W, tid, uid)$RSerr, lmdiag(y, X, Wb)$RSerr, tolerance = 1e-10)
  expect_equal(spphaus(c(1, 0.5), c(0.8, 0.6), rbind(c(0.05, 0.01), c(0.01, 0.04)), diag(c(0.02, 0.03)))$statistic,
               5.5, tolerance = 1e-12)
  H <- cbind(Xt, apply(X, 2, function(v) wi(lg(v))), apply(X, 2, function(v) wi(lg(lg(v)))))
  Dm <- cbind(lg(yt), Xt)
  P <- H %*% solve(crossprod(H), t(H))
  expect_equal(sppiv(y, X, NULL, W, tid, uid)$coefficients, as.vector(solve(t(Dm) %*% P %*% Dm, t(Dm) %*% P %*% yt)),
               tolerance = 1e-10)
  g <- sppgmm(y, X, W, tid, uid)
  expect_gt(g$sigma2_nu, 0)
  expect_equal(g$sigma2_mu, (g$sigma2_1 - g$sigma2_nu) / T)
  b <- sppboot(y, X, W, tid, uid, B = 3, seed = 5)
  expect_equal(b$rho_se, stats::sd(b$rho_draws))
})
