n <- 20
ii <- 0:19
P <- cbind(ii %% 5, ii %/% 5)
X <- cbind(sin(ii + 0.5), (0.3 * ii) %% 1.1 + 0.2)
y <- 1 + (1 + 0.2 * P[, 1]) * X[, 1] - X[, 2] + 0.1 * cos(3 * ii)
Xa <- cbind(1, X)
big <- rep(1e6, 3)

test_that("the OLS limit recomputes", {
  G <- solve(crossprod(Xa))
  b <- as.vector(G %*% crossprod(Xa, y))
  e <- as.vector(y - Xa %*% b)
  H <- Xa %*% G %*% t(Xa)
  r <- mgwrfit(y, X, P, big, kernel = "gaussian")
  expect_equal(r$betas[3, ], b, tolerance = 1e-7)
  expect_equal(r$trS, 3, tolerance = 1e-6)
  expect_equal(r$se[5, ], sqrt(sum(e^2) / (n - 3) * diag(G)), tolerance = 1e-7)
  expect_equal(mgwrhat(y, X, P, big, kernel = "gaussian"), diag(H), tolerance = 1e-7)
  expect_equal(mgwrcv(y, X, P, big, kernel = "gaussian"), sum((e / (1 - diag(H)))^2), tolerance = 1e-6)
  expect_equal(mgwrres(y, X, P, big, kernel = "gaussian"), e, tolerance = 1e-7)
  expect_equal(mgwrcof(y, X, P, big, kernel = "gaussian")[1, ], b, tolerance = 1e-7)
  expect_equal(mgwrstd(y, X, P, big, kernel = "gaussian")[1, 2], sqrt(sum(e^2) / (n - 3) * G[2, 2]), tolerance = 1e-7)
})

test_that("fixed point, selection and summaries recompute", {
  b <- c(6, 3, 8)
  r <- mgwrfit(y, X, P, b, kernel = "gaussian")
  D <- unname(as.matrix(stats::dist(P)))
  for (k in 1:3) {
    part <- r$residuals + Xa[, k] * r$betas[, k]
    w <- exp(-D[1, ]^2 / (2 * b[k]^2))
    expect_equal(r$betas[1, k], sum(w * Xa[, k] * part) / sum(w * Xa[, k]^2), tolerance = 1e-7)
  }
  bk <- mgwrbk(y, X, P, b, max_iter = 3, kernel = "gaussian")
  expect_equal(bk$iterations, 3)
  bw <- mgwrbw(y, X, P, kernel = "gaussian")
  expect_true(all(bw >= 1 - 1e-9 & bw <= 5 + 1e-9))
  ll <- -n / 2 * (log(2 * pi * r$rss / n) + 1)
  expect_equal(mgwraic(ll, r$trS, n)$statistic, r$aicc, tolerance = 1e-10)
  expect_equal(mgwrdg(ll, r$trS, n, k = 3)$alpha_adjusted, 0.05 * 3 / r$trS)
  expect_equal(mgwrsig(r$residuals, r$trS)$statistic, r$sigma2, tolerance = 1e-12)
  t <- mgwrtst(y, X, P, nsim = 9, bandwidths = b, seed = 2, kernel = "gaussian", threshold = 1e-12)
  expect_equal(t$observed_variance, apply(mgwrcof(y, X, P, b, kernel = "gaussian"), 2, stats::var), tolerance = 1e-9)
  expect_equal(t$p_values[1], 1 - (1 + sum(t$simulated_variance[, 1] < t$observed_variance[1])) / 10)
})
