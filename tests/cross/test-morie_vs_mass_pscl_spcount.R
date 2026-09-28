test_that("spatial count models equal glm, MASS::glm.nb and pscl::zeroinfl on the lagged design", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("pscl")
  side <- 6
  n <- side^2
  W <- matrix(0, n, n)
  for (i in 0:(n - 1)) {
    r <- i %/% side
    cc <- i %% side
    for (d in list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))) {
      rr <- r + d[1]
      c2 <- cc + d[2]
      if (rr >= 0 && rr < side && c2 >= 0 && c2 < side) W[i + 1, rr * side + c2 + 1] <- 1
    }
  }
  W <- W / rowSums(W)
  u <- .morie_random_uniform(3 * n, seed = 5, stream = 0)
  X <- cbind(1, 2 * u[1:n] - 1)
  y <- stats::qnbinom(u[(n + 1):(2 * n)], size = 2, mu = exp(1 + 0.7 * X[, 2]))
  y[u[(2 * n + 1):(3 * n)] < 0.2] <- 0
  Xt <- solve(diag(n) - 0.3 * W, X)
  p <- SarPoisson(y, X, W, rho_bounds = c(0.3, 0.3))
  expect_equal(p$coefficients, unname(coef(glm(y ~ Xt - 1, family = poisson))), tolerance = 1e-8)
  nb <- SarNegbin(y, X, W, rho_bounds = c(0.3, 0.3))
  ref <- MASS::glm.nb(y ~ Xt - 1)
  expect_equal(nb$coefficients, unname(coef(ref)), tolerance = 1e-5)
  expect_equal(nb$theta, ref$theta, tolerance = 1e-4)
  zp <- SarZip(y, X, W, rho_bounds = c(0.3, 0.3))
  zr <- pscl::zeroinfl(y ~ Xt - 1 | 1, dist = "poisson")
  expect_equal(c(zp$count_coefficients, zp$zero_coefficients), unname(coef(zr)), tolerance = 1e-4)
  expect_gte(zp$loglik, as.numeric(zr$loglik) - 1e-8)
  zn <- SarZinb(y, X, W, rho_bounds = c(0.3, 0.3))
  zb <- pscl::zeroinfl(y ~ Xt - 1 | 1, dist = "negbin")
  expect_equal(c(zn$count_coefficients, zn$zero_coefficients), unname(coef(zb)), tolerance = 1e-3)
  expect_gte(zn$loglik, as.numeric(zb$loglik) - 1e-6)
})
