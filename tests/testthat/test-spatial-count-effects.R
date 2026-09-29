test_that("spatial count impacts recompute and match derivatives", {
  n <- 8
  W <- matrix(0, n, n)
  W[abs(row(W) - col(W)) %in% c(1, n - 1)] <- 0.5
  X <- cbind(1, c(0.3, -0.2, 0.8, 0.1, -0.5, 0.4, 0.9, -0.7), c(1.2, 0.4, -0.3, 0.8, 0, -1.1, 0.5, 0.2))
  b <- c(0.4, 0.6, -0.3)
  rho <- 0.35
  mu_of <- function(Xm) exp(as.vector(solve(diag(n) - rho * W, Xm %*% b)))
  r <- scpmf(b, rho, X, W)
  Ai <- solve(diag(n) - rho * W)
  mu <- mu_of(X)
  expect_equal(r$direct, sum(mu * diag(Ai)) / n * b, tolerance = 1e-12)
  expect_equal(r$total, sum(diag(mu) %*% Ai) / n * b, tolerance = 1e-12)
  h <- 1e-6
  e <- matrix(0, n, 3)
  e[, 2] <- h
  expect_equal((sum(mu_of(X + e)) - sum(mu_of(X - e))) / (2 * h) / n, r$total[2], tolerance = 1e-7)
  expect_identical(scnbmf(b, rho, X, W), r)
  expect_equal(max(abs(scpmf(b, 0, X, W)$indirect)), 0, tolerance = 1e-15)
  p <- scpprd(b, X, W, rho = rho)
  expect_equal(p$fitted, mu, tolerance = 1e-12)
  expect_equal(p$total, sum(mu), tolerance = 1e-12)
})

test_that("Wald test and Python parity", {
  w <- scpwld(0.5, 0.15, 0.2)
  expect_equal(w$statistic, 4, tolerance = 1e-12)
  expect_equal(w$p_value, 2 * stats::pnorm(-2), tolerance = 1e-12)
  expect_error(scpwld(0.3, 0))
  W <- rbind(c(0, 1, 0), c(0.5, 0, 0.5), c(0, 1, 0))
  X <- cbind(1, c(0.1, 0.4, 0.9))
  r <- scpmf(c(0.2, 0.5), 0.3, X, W)
  # Python doctest of morie.fn.scpmf, rounded to 10 digits
  expect_equal(round(c(r$direct[2], r$indirect[2], r$total[2]), 10), c(0.9972864709, 0.3400792661, 1.337365737))
})
