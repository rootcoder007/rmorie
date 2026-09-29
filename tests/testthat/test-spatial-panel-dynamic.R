# Tests for SpatialPanelDynamic: the dynamic spatial panel is the FE spatial lag model on augmented regressors.

N <- 6
T_ <- 5
A <- outer(seq_len(N), seq_len(N), function(i, j) as.numeric(i != j & abs(i - j) <= 1))
W <- A / rowSums(A)
X <- array(0, c(T_, N, 2))
Y <- matrix(0, T_, N)
for (t in 0:(T_ - 1)) for (i in 0:(N - 1)) {
  X[t + 1, i + 1, ] <- c(sin(i + 2 * t), 0.3 * cos(i * t))
  Y[t + 1, i + 1] <- 0.6 * X[t + 1, i + 1, 1] - 0.4 * X[t + 1, i + 1, 2] + 0.5 * sin(i * 1.3) + 0.2 * cos(t + i)
}
augmented <- function(stl) {
  yv <- numeric((T_ - 1) * N)
  Xm <- matrix(0, (T_ - 1) * N, 2 + if (stl) 2 else 1)
  for (t in 2:T_) {
    wprev <- as.numeric(W %*% Y[t - 1, ])
    for (i in seq_len(N)) {
      k <- (t - 2) * N + i
      yv[k] <- Y[t, i]
      Xm[k, ] <- c(Y[t - 1, i], if (stl) wprev[i], X[t, i, ])
    }
  }
  list(y = yv, X = Xm)
}

test_that("SpPanelDynamic equals SpatialPanelMl on the augmented design", {
  for (stl in c(TRUE, FALSE)) {
    r <- SpPanelDynamic(Y, X, W, space_time_lag = stl)
    a <- augmented(stl)
    ref <- SpatialPanelMl(a$y, a$X, W, N, model = "lag", effects = "individual")
    expect_length(r$beta, if (stl) 4 else 3)
    expect_identical(r$beta, ref$coefficients)
    expect_identical(r$rho, ref$rho)
    expect_identical(r$loglik, ref$loglik)
    expect_equal(r$n_obs, N * (T_ - 1))
  }
})

test_that("effects and bounds pass through", {
  r <- SpPanelDynamic(Y, X, W, effects = "twoways", bounds = c(-0.5, 0.5))
  a <- augmented(TRUE)
  ref <- SpatialPanelMl(a$y, a$X, W, N, model = "lag", effects = "twoways", interval = c(-0.5, 0.5))
  expect_identical(r$rho, ref$rho)
  expect_identical(r$sigma2, ref$sigma2)
  expect_true(r$rho >= -0.5 && r$rho <= 0.5)
})
