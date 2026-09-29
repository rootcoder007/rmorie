# Cross tests: spatial panel models against splm (spml and spreml).

make_panel <- function() {
  N <- 15
  T_ <- 6
  A <- outer(0:14, 0:14, function(i, j) as.numeric(i != j & (abs(i - j) == 1 | (i * 3 + j * 5) %% 11 == 0)))
  A <- pmax(A, t(A))
  W <- A / rowSums(A)
  X <- array(0, c(T_, N, 2))
  for (t in 0:5) for (i in 0:14) X[t + 1, i + 1, ] <- c(sin(i + 2 * t), cos(i * t * 0.3) + 0.1 * i)
  y <- matrix(0, T_, N)
  for (t in 0:5) for (i in 0:14) {
    y[t + 1, i + 1] <- 0.7 * X[t + 1, i + 1, 1] - 0.4 * X[t + 1, i + 1, 2] + 0.3 * sin(i * 1.7) + 0.2 * cos(t * i)
  }
  df <- data.frame(id = rep(1:N, T_), time = rep(1:T_, each = N), y = as.numeric(t(y)),
                   x1 = as.numeric(t(X[, , 1])), x2 = as.numeric(t(X[, , 2])))
  list(W = W, X = X, y = y, df = df)
}

test_that("fixed-effects lag and error models equal splm::spml", {
  skip_if_not_installed("splm")
  skip_if_not_installed("spdep")
  p <- make_panel()
  lw <- spdep::mat2listw(p$W, style = "W")
  for (e in c("individual", "time", "twoways")) {
    ref <- splm::spml(y ~ x1 + x2, p$df, index = c("id", "time"), listw = lw, model = "within", effect = e, lag = TRUE,
                      spatial.error = "none")
    got <- SpPanelFe(p$y, p$X, p$W, effects = e, method = "splm")
    # splm maximises with optimize(), tol = sqrt(.Machine$double.eps)
    expect_equal(c(got$rho, got$beta), as.numeric(coef(ref)), tolerance = 1e-6)
    ref <- splm::spml(y ~ x1 + x2, p$df, index = c("id", "time"), listw = lw, model = "within", effect = e, lag = FALSE,
                      spatial.error = "b")
    got <- SpPanelFe(p$y, p$X, p$W, model = "error", effects = e)
    expect_equal(c(got$lambda_, got$beta), as.numeric(coef(ref)), tolerance = 1e-6)
    expect_equal(got$sigma2, ref$sigma2, tolerance = 1e-6)
  }
})

test_that("random-effects lag model equals splm::spreml", {
  skip_if_not_installed("splm")
  p <- make_panel()
  ref <- splm::spreml(y ~ x1 + x2, p$df, index = c("id", "time"), w = p$W, lag = TRUE, errors = "re")
  got <- SpPanelRe(p$y, p$X, p$W)
  expect_equal(c(got$intercept, got$beta), as.numeric(coef(ref)), tolerance = 1e-6)
  expect_equal(got$phi, as.numeric(ref$errcomp), tolerance = 1e-5)
  expect_equal(got$rho, as.numeric(ref$arcoef), tolerance = 1e-5)
  expect_equal(got$loglik, as.numeric(ref$logLik), tolerance = 1e-8)
})
