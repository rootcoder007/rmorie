# Tests for StarModels: Pfeifer-Deutsch STARMA space-time models.

W1 <- matrix(c(0, 0.5, 0.5, 0, 0.5, 0, 0, 0.5, 0.5, 0, 0, 0.5, 0, 0.5, 0.5, 0), 4, byrow = TRUE)
W2 <- matrix(c(0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0), 4, byrow = TRUE)
W <- list(W1, W2)
X <- matrix(0, 12, 4)
for (t in 0:11) for (i in 0:3) X[t + 1, i + 1] <- (t * 7 + i * 3) %% 5 + 0.1 * ((t * i) %% 4) + 0.01 * t
T_ <- 12
N <- 4
lag_ <- function(z, order) if (order == 0) z else as.numeric(W[[order]] %*% z)
gamma_ <- function(z, l, h, s) {
  tot <- 0
  for (t in seq_len(T_ - s)) tot <- tot + sum(lag_(z[t, ], l) * lag_(z[t + s, ], h))
  tot / (N * (T_ - s))
}
Z <- X - mean(X)

test_that("StLag is the matrix product", {
  z <- c(1, 2, 4, 8)
  expect_equal(StLag(z, W, 1), c(3, 4.5, 4.5, 3))
  expect_equal(StLag(z, W, 2), c(8, 4, 2, 1))
  expect_equal(StLag(z, W, 0), z)
})

test_that("StAcf matches the definition", {
  r <- StAcf(X, W, max_lag_t = 3)
  expect_equal(r$acf[1, 1], 1)
  for (s in 0:3) for (l in 0:2) {
    want <- gamma_(Z, l, 0, s) / sqrt(gamma_(Z, l, l, 0) * gamma_(Z, 0, 0, 0))
    expect_equal(r$acf[s + 1, l + 1], want, tolerance = 1e-12)
  }
})

test_that("StPacf solves the first- and second-order Yule-Walker systems", {
  r <- StPacf(X, W, max_lag_t = 2, max_lag_s = 0)
  g0 <- gamma_(Z, 0, 0, 0)
  g1 <- gamma_(Z, 0, 0, 1)
  g2 <- gamma_(Z, 0, 0, 2)
  expect_equal(r$pacf[1, 1], g1 / g0, tolerance = 1e-12)
  expect_equal(r$pacf[2, 1], (g0 * g2 - g1 * g1) / (g0 * g0 - g1 * g1), tolerance = 1e-12)
})

test_that("StarFit is stacked least squares", {
  r <- StarFit(X, W, list(c(0, 1)))
  rows <- NULL
  y <- NULL
  for (t in 2:T_) {
    rows <- rbind(rows, cbind(lag_(Z[t - 1, ], 0), lag_(Z[t - 1, ], 1)))
    y <- c(y, Z[t, ])
  }
  beta <- as.numeric(solve(crossprod(rows), crossprod(rows, y)))
  expect_equal(unname(r$coefficients[, 3]), beta, tolerance = 1e-10)
  resid <- as.numeric(y - rows %*% beta)
  rss <- sum(resid^2)
  n <- length(y)
  expect_equal(r$rss, rss, tolerance = 1e-12)
  expect_equal(r$residuals[1, 1], resid[1], tolerance = 1e-10)
  expect_equal(r$sigma2, rss / (n - 2), tolerance = 1e-12)
  expect_equal(r$loglik, -0.5 * n * (log(2 * pi * rss / n) + 1), tolerance = 1e-12)
  expect_equal(r$aic, -2 * r$loglik + 4, tolerance = 1e-12)
})

test_that("StarmaFit reaches the least-squares optimum and nests the AR model", {
  ls <- StarFit(X, W, list(c(0, 1)))
  m <- StarmaFit(X, W, list(c(0, 1)), list())
  expect_lte(m$rss, ls$rss + 1e-9)
  expect_equal(unname(m$phi[, 3]), unname(ls$coefficients[, 3]), tolerance = 1e-6)
  m2 <- StarmaFit(X, W, list(c(0, 1)), list(0))
  expect_lte(m2$rss, m$rss + 1e-9)
  # residual recursion recomputed
  e <- matrix(0, T_, N)
  for (t in 2:T_) {
    pred <- numeric(N)
    for (a in seq_len(nrow(m2$phi))) pred <- pred + m2$phi[a, 3] * lag_(Z[t - m2$phi[a, 1], ], m2$phi[a, 2])
    for (a in seq_len(nrow(m2$theta))) if (t - m2$theta[a, 1] >= 2) pred <- pred - m2$theta[a, 3] * lag_(e[t - m2$theta[a, 1], ], m2$theta[a, 2])
    e[t, ] <- Z[t, ] - pred
  }
  expect_equal(unname(m2$residuals), e[2:T_, ], tolerance = 1e-12)
  expect_equal(m2$rss, sum(e[2:T_, ]^2), tolerance = 1e-12)
})

test_that("StarmaForecast one step by hand", {
  m <- StarmaFit(X, W, list(c(0, 1)), list(0))
  f <- StarmaForecast(m, X, W, 1)
  pred <- numeric(N)
  for (a in seq_len(nrow(m$phi))) pred <- pred + m$phi[a, 3] * lag_(Z[T_ + 1 - m$phi[a, 1], ], m$phi[a, 2])
  for (a in seq_len(nrow(m$theta))) pred <- pred - m$theta[a, 3] * lag_(m$residuals[T_ - 1 + 1 - m$theta[a, 1], ], m$theta[a, 2])
  expect_equal(as.numeric(f$forecast[1, ]), pred + mean(X), tolerance = 1e-12)
})

test_that("StPortmanteau is recomputed from the residual STACF", {
  m <- StarmaFit(X, W, list(c(0, 1)), list())
  q <- StPortmanteau(m$residuals, W, max_lag_t = 2, n_params = 2)
  a <- StAcf(m$residuals, W, max_lag_t = 2, center = FALSE)
  Q <- N * nrow(m$residuals) * sum(a$acf[2:3, ]^2)
  expect_equal(q$statistic, Q, tolerance = 1e-12)
  expect_equal(q$df, 4)
  expect_equal(q$p_value, stats::pchisq(Q, 4, lower.tail = FALSE), tolerance = 1e-12)
})
