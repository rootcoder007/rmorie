n <- 14
W <- matrix(0, n, n)
for (i in 0:(n - 1)) for (d in 1:2) {
  W[i + 1, (i + d) %% n + 1] <- 0.25
  W[i + 1, (i - d) %% n + 1] <- 0.25
}
ii <- 0:(n - 1)
X <- cbind(sin(ii * 1.3) + ii / 7, ((ii * 5) %% 7) / 3)
y <- 1 + 2 * X[, 1] - X[, 2] + ((ii * 3) %% 5 - 2) / 3 + 0.4 * cos(ii)
Xa <- cbind(1, X)
M <- diag(n) - Xa %*% solve(crossprod(Xa), t(Xa))
u <- as.vector(M %*% y)
s2 <- sum(u^2) / n
tt <- sum(diag(t(W) %*% W + W %*% W))
wxb <- as.vector(W %*% (y - u))
nJ <- (sum(wxb * (M %*% wxb)) + tt * s2) / s2
de <- sum(u * (W %*% u)) / s2
dl <- sum(u * (W %*% y)) / s2

test_that("Anselin tests recompute", {
  expect_equal(lmerr(y, X, W)$statistic, de^2 / tt, tolerance = 1e-10)
  expect_equal(lmlag(y, X, W)$statistic, dl^2 / nJ, tolerance = 1e-10)
  z <- (de - tt * dl / nJ) / sqrt(tt * (1 - tt / nJ))
  expect_equal(lmrerr(y, X, W)$statistic, z^2, tolerance = 1e-10)
  expect_equal(lmrerr2(y, X, W)$statistic, z, tolerance = 1e-10)
  expect_equal(lmrlag(y, X, W)$statistic, (dl - de)^2 / (nJ - tt), tolerance = 1e-10)
  sarma <- (dl - de)^2 / (nJ - tt) + de^2 / tt
  expect_equal(lmsarma(y, X, W)$statistic, sarma, tolerance = 1e-10)
  expect_equal(lmjoint(y, X, W)$p_value, exp(-sarma / 2), tolerance = 1e-10)
  expect_equal(lmdiag(y, X, W)$SARMA, sarma, tolerance = 1e-10)
})

test_that("Koley-Bera Durbin tests recompute", {
  WX <- W %*% X
  g <- as.vector(crossprod(WX, u))
  rswx <- sum(g * solve(t(WX) %*% M %*% WX, g)) / s2
  expect_equal(lmslx(y, X, W)$statistic, rswx, tolerance = 1e-10)
  G <- cbind(wxb, WX)
  J <- t(G) %*% M %*% G
  J[1, 1] <- J[1, 1] + tt * s2
  dd <- c(dl, g / s2)
  expect_equal(lmsdm(y, X, W)$statistic, sum(dd * solve(J, dd)) * s2, tolerance = 1e-10)
})

test_that("lmkp and sphet recompute", {
  WX <- W %*% X
  H <- cbind(Xa, WX[, 1], W %*% WX[, 1], WX[, 2], W %*% WX[, 2])
  Z <- cbind(W %*% y, Xa)
  P <- H %*% solve(crossprod(H), t(H))
  b <- solve(t(Z) %*% P %*% Z, t(Z) %*% P %*% y)
  e <- as.vector(y - Z %*% b)
  s0 <- sum(W)
  mi <- n / s0 * sum(e * (W %*% e)) / sum(e^2)
  g <- as.vector(t(Z) %*% t(W) %*% e)
  phi2 <- (tt + 4 / (sum(e^2) / n) * sum(g * solve(t(Z) %*% P %*% Z, g))) / ((s0 / n)^2 * n)
  expect_equal(lmkp(y, X, W)$statistic, n * mi^2 / phi2, tolerance = 1e-9)
  e2 <- u^2
  z2 <- as.vector(W %*% e2)
  f <- stats::lm.fit(cbind(1, z2), e2)
  expect_equal(sphet(u, W)$statistic, n * (1 - sum(f$residuals^2) / sum((e2 - mean(e2))^2)), tolerance = 1e-10)
})
