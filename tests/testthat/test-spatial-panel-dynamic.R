# Tests for SpatialPanel: spatial panel data models.

N <- 8
T_ <- 5
A <- outer(0:7, 0:7, function(i, j) as.numeric(i != j & (abs(i - j) == 1 | (i * 3 + j * 5) %% 7 == 0)))
A <- pmax(A, t(A))
W <- A / rowSums(A)
X <- array(0, c(T_, N, 2))
for (t in 0:4) for (i in 0:7) X[t + 1, i + 1, ] <- c(sin(i + 2 * t), cos(i * t * 0.3) + 0.1 * i)
Y <- matrix(0, T_, N)
for (t in 0:4) for (i in 0:7) Y[t + 1, i + 1] <- 0.7 * X[t + 1, i + 1, 1] - 0.4 * X[t + 1, i + 1, 2] + 0.3 * sin(i * 1.7) + 0.2 * cos(t * i)

lag_ll <- function(rho) {
  wmean <- function(v) as.numeric(matrix(v, N) - rowMeans(matrix(v, N)))
  y <- as.numeric(t(Y))
  wy <- as.numeric(W %*% t(Y))
  Xc <- cbind(wmean(as.numeric(t(X[, , 1]))), wmean(as.numeric(t(X[, , 2]))))
  e <- stats::lm.fit(Xc, wmean(y) - rho * wmean(wy))$residuals
  -0.5 * N * T_ * log(sum(e^2)) + T_ * log(abs(det(diag(N) - rho * W)))
}

test_that("SpPanelFe lag maximises the concentrated likelihood", {
  r <- SpPanelFe(Y, X, W)
  expect_gte(lag_ll(r$rho), lag_ll(r$rho + 1e-4))
  expect_gte(lag_ll(r$rho), lag_ll(r$rho - 1e-4))
})

test_that("SpPanelRe and SpPanelDynamic", {
  r <- SpPanelRe(Y, X, W)
  expect_true(r$phi >= 0 && abs(r$rho) < 1)
  d <- SpPanelDynamic(Y, X, W)
  Xn <- array(0, c(T_ - 1, N, 4))
  for (t in 2:T_) Xn[t - 1, , ] <- cbind(Y[t - 1, ], as.numeric(W %*% Y[t - 1, ]), X[t, , 1], X[t, , 2])
  expect_equal(SpPanelFe(Y[-1, ], Xn, W)$beta, d$beta)
})
