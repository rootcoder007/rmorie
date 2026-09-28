test_that("serial LMM profile likelihood equals the Gaussian density at the GLS fit", {
  e <- .morie_random_normal(120, seed = 17)
  id <- rep(0:7, each = 4)
  tt <- rep(c(0, 1, 3, 4), 8) + 0.2 * e[id + 1]
  X <- cbind(1, tt, e[11:42])
  y <- as.vector(X %*% c(0.5, 0.3, 1)) + 1.2 * e[51 + id] + 0.7 * e[61:92]
  r <- LmmSerialLoglik(y, X, id, tt, 0.6, 0.9, 0.4, 0.3)
  ll <- 0
  for (s in 0:7) {
    rows <- which(id == s)
    V <- LmmSerialCovariance(tt[rows], 0.5 * r$sigma2, 0.75 * r$sigma2, 0.4, 0.25 * r$sigma2)
    res <- y[rows] - X[rows, ] %*% r$beta
    ll <- ll - 0.5 * (4 * log(2 * pi) + as.numeric(determinant(V)$modulus) + sum(res * solve(V, res)))
  }
  expect_equal(r$loglik, ll, tolerance = 1e-11)
  expect_equal(SerialCorrelation(c(0, 2), 0.3), c(1, exp(-0.6)), tolerance = 1e-15)
  expect_equal(SerialCorrelation(3, 0.5, "ar1"), 0.125, tolerance = 1e-15)
  f <- LmmSerialFit(y, X, id, tt)
  expect_true(f$converged)
  base <- LmmSerialLoglik(y, X, id, tt, f$sigma_b2, f$tau2, f$phi, f$nu2)$loglik
  expect_equal(base, f$loglik, tolerance = 1e-12)
  expect_lte(LmmSerialLoglik(y, X, id, tt, f$sigma_b2, f$tau2, f$phi * 1.05, f$nu2)$loglik, base + 1e-9)
})
