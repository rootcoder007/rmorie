esl9_K <- function(x) {
  o <- order(x)
  u <- x[o]
  n <- length(u)
  h <- diff(u)
  Q <- matrix(0, n, n - 2)
  R <- matrix(0, n - 2, n - 2)
  for (j in 2:(n - 1)) {
    Q[j - 1, j - 1] <- 1 / h[j - 1]
    Q[j, j - 1] <- -1 / h[j - 1] - 1 / h[j]
    Q[j + 1, j - 1] <- 1 / h[j]
    R[j - 1, j - 1] <- (h[j - 1] + h[j]) / 3
    if (j < n - 1) R[j - 1, j] <- R[j, j - 1] <- h[j] / 6
  }
  K <- matrix(0, n, n)
  K[o, o] <- Q %*% solve(R, t(Q))
  K
}

test_that("backfitting reaches the penalised least-squares solution", {
  i <- 1:40
  X <- cbind(((7 * i) %% 41) / 41 * 3, ((11 * i) %% 43) / 43 * 2)
  y <- sin(2 * X[, 1]) + 0.5 * X[, 2]^2 + 0.2 * cos(9 * i)
  r <- morie_esl_gam(X, y, lambdas = c(0.05, 0.2))
  I <- diag(40)
  M <- rbind(cbind(I + 0.05 * esl9_K(X[, 1]), I), cbind(I, I + 0.2 * esl9_K(X[, 2])),
             c(rep(1, 40), rep(0, 40)), c(rep(0, 40), rep(1, 40)))
  f <- qr.solve(M, c(y - mean(y), y - mean(y), 0, 0), tol = 1e-14)
  expect_equal(as.numeric(r$partial_fits), f, tolerance = 1e-8)
  expect_equal(r$rss, 1.0146439575827859, tolerance = 1e-9)
  lin <- morie_esl_gam(X, y, smoother = "linear")
  expect_equal(lin$fitted, unname(fitted(lm(y ~ X))), tolerance = 1e-9)
})

test_that("local scoring satisfies the penalised-likelihood stationarity conditions", {
  i <- 1:40
  X <- cbind(((7 * i) %% 41) / 41 * 3, ((11 * i) %% 43) / 43 * 2)
  yb <- as.integer(sin(2 * X[, 1]) + X[, 2] - 1 + 0.8 * cos(9 * i) > 0)
  q <- morie_esl_gam(X, yb, g = "logit", lambdas = c(0.5, 0.5))
  expect_true(q$converged)
  res <- yb - q$fitted
  expect_lt(abs(sum(res)), 1e-8)
  for (j in 1:2) expect_lt(max(abs(res - 0.5 * esl9_K(X[, j]) %*% q$partial_fits[, j])), 1e-7)
})

test_that("local scoring pools tied inputs with their weights", {
  i <- 1:60
  X <- cbind(round(((7 * i) %% 41) / 41 * 3, 1), round(((11 * i) %% 43) / 43 * 2, 1))
  yb <- as.integer(sin(2 * X[, 1]) + X[, 2] - 1 + 0.8 * cos(9 * i) > 0)
  q <- morie_esl_gam(X, yb, g = "logit", lambdas = c(0.3, 0.3))
  res <- yb - q$fitted
  for (j in 1:2) {
    u <- sort(unique(X[, j]))
    fu <- q$partial_fits[match(u, X[, j]), j]
    agg <- as.numeric(tapply(res, match(X[, j], u), sum))
    expect_lt(max(abs(agg - 0.3 * esl9_K(u) %*% fu)), 1e-7)
  }
})
