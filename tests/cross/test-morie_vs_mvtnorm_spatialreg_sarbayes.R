bm_se <- function(x, b = 40) {
  m <- matrix(x[seq_len(length(x) %/% b * b)], ncol = b)
  stats::sd(colMeans(m)) / sqrt(b)
}

test_that("SAR probit sampler reproduces the exact posterior (mvtnorm orthant probabilities)", {
  skip_if_not_installed("mvtnorm")
  W <- rbind(c(0, 1, 0, 0), c(0.5, 0, 0.5, 0), c(0, 0.5, 0, 0.5), c(0, 0, 1, 0))
  y <- c(1, 1, 0, 1)
  X <- matrix(1, 4, 1)
  bg <- seq(-4, 4, by = 0.05)
  rg <- seq(-0.99, 0.99, by = 0.02)
  lo <- ifelse(y > 0, 0, -Inf)
  hi <- ifelse(y > 0, Inf, 0)
  P <- matrix(0, length(bg), length(rg))
  set.seed(1)
  for (j in seq_along(rg)) {
    A <- diag(4) - rg[j] * W
    S <- solve(crossprod(A))
    a1 <- solve(A, rep(1, 4))
    for (i in seq_along(bg)) {
      P[i, j] <- stats::dnorm(bg[i]) * mvtnorm::pmvnorm(lower = lo, upper = hi, mean = a1 * bg[i], sigma = S)[1]
    }
  }
  P <- P / sum(P)
  o <- SarProbitGibbs(y, X, W, ndraw = 30000, burn_in = 1000, seed = 3, prior_var = 1)
  expect_lt(abs(o$beta - sum(P * bg)) / bm_se(o$beta_draws[, 1]), 5)
  expect_lt(abs(o$rho - sum(t(P) * rg)) / bm_se(o$rho_draws), 5)
})

test_that("Bayesian spatial lag equals spatialreg::spBreg_lag; Durbin rho equals the exact marginal posterior", {
  skip_if_not_installed("spatialreg")
  skip_if_not_installed("spdep")
  n <- 50
  u <- .morie_random_uniform(2 * n, seed = 3)
  D <- as.matrix(stats::dist(cbind(u[1:n], u[n + 1:n])))
  A <- matrix(0, n, n)
  for (i in 1:n) A[i, order(D[i, ])[2:5]] <- 1
  A <- pmax(A, t(A))
  W <- A / rowSums(A)
  z <- .morie_random_normal(3 * n, seed = 4)
  X <- cbind(1, z[1:n], z[n + 1:n])
  y <- as.vector(solve(diag(n) - 0.5 * W, X %*% c(0.3, 1, -0.8) + 0.5 * z[2 * n + 1:n]))
  df <- data.frame(y = y, x1 = X[, 2], x2 = X[, 3])
  lw <- suppressWarnings(spdep::mat2listw(W, style = "W"))
  set.seed(1)
  ref <- suppressWarnings(spatialreg::spBreg_lag(y ~ x1 + x2, data = df, listw = lw,
                                                 control = list(ndraw = 20000L, nomit = 2000L)))
  o <- SpatialBayesGibbs(y, X, W, "lag", ndraw = 4000, burn_in = 400, seed = 1)
  se <- c(apply(o$beta_draws, 2, bm_se), bm_se(o$rho_draws)) + apply(as.matrix(ref), 2, stats::sd)[1:4] / 50
  expect_true(all(abs(c(o$beta, o$rho) - colMeans(as.matrix(ref))[1:4]) / se < 5))
  # spBreg_lag(Durbin = TRUE) drifts from the flat-prior posterior; check rho against the marginal
  # posterior |I - rho W| (e'e)^(-(n-k)/2) integrated on a grid
  XX <- cbind(X, W %*% X[, -1])
  g <- seq(-0.99, 0.99, by = 0.001)
  lp <- vapply(g, function(r) {
    Ar <- diag(n) - r * W
    e <- Ar %*% y - XX %*% solve(crossprod(XX), crossprod(XX, Ar %*% y))
    as.numeric(determinant(Ar)$modulus) - (n - 5) / 2 * log(sum(e^2))
  }, numeric(1))
  w <- exp(lp - max(lp))
  d <- SpatialBayesGibbs(y, X, W, "durbin", ndraw = 4000, burn_in = 400, seed = 1)
  expect_lt(abs(d$rho - sum(g * w) / sum(w)) / bm_se(d$rho_draws), 5)
})
