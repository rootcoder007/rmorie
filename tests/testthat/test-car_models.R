W4 <- rbind(c(0, 1, 0, 1), c(1, 0, 1, 0), c(0, 1, 0, 1), c(1, 0, 1, 0))
P4 <- rbind(c(0, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 0))

test_that("carvar, carjac, carsim and caricar recompute", {
  expect_equal(carvar(W4, 0.3, 1.7)$covariance, 1.7 * solve(diag(4) - 0.3 * W4), tolerance = 1e-14)
  expect_equal(carjac(P4, 0.35)$statistic, 0.5 * sum(log(1 - 0.35 * 2 * cos((1:4) * pi / 5))), tolerance = 1e-14)
  r <- carsim(W4, 0.3, 2, nsim = 3, seed = 7)
  Q <- (diag(4) - 0.3 * W4) / 2
  for (k in 1:3) {
    z <- .morie_random_normal(4, seed = 7, stream = k - 1)
    expect_equal(sum(r$draws[k, ] * (Q %*% r$draws[k, ])), sum(z^2), tolerance = 1e-12)
  }
  phi <- c(0.4, -0.1, 0.3, -0.6)
  pos <- 2 - 2 * cos((1:3) * pi / 4)
  ref <- -1.5 * log(2 * pi) + 1.5 * log(2.5) + 0.5 * sum(log(pos)) - 0.5 * 2.5 * sum(diff(phi)^2)
  expect_equal(caricar(phi, P4, tau = 2.5)$statistic, ref, tolerance = 1e-12)
})

test_that("cargmm and carres recompute", {
  n <- 8
  W <- 1 * (abs(outer(1:n, 1:n, "-")) == 1)
  y <- c(1, 2, 2.5, 4.5, 4, 3.2, 2.1, 2.9)
  e <- y - mean(y)
  expect_equal(cargmm(y, W)$statistic, sum(e * (W %*% e)) / sum(e * (W %*% W %*% e)), tolerance = 1e-12)
  expect_equal(carres(e, W)$statistic, n / sum(W) * sum(e * (W %*% e)) / sum(e^2), tolerance = 1e-12)
})

test_that("carbym is the REML optimum", {
  n <- 16
  ii <- 0:(n - 1)
  W <- outer(ii, ii, function(a, b) as.numeric(abs(a %/% 4 - b %/% 4) + abs(a %% 4 - b %% 4) == 1))
  y <- 0.5 * (ii %/% 4) + 0.4 * (ii %% 4) + 0.8 * sin(2.3 * ii)
  Q <- diag(rowSums(W)) - W
  J <- matrix(1 / n, n, n)
  Qp <- solve(Q + J) - J
  reml <- function(su, sv) {
    S <- su * Qp + sv * diag(n)
    Si <- solve(S)
    a <- sum(Si)
    mu <- sum(Si %*% y) / a
    r <- y - mu
    -0.5 * (as.numeric(determinant(S)$modulus) + log(a) - log(n) + sum(r * (Si %*% r)) + (n - 1) * log(2 * pi))
  }
  f <- carbym(y, W)
  expect_equal(reml(f$sigma2_spatial, f$sigma2_unstructured), f$reml_loglik, tolerance = 1e-10)
  o <- stats::optim(c(0.5, 0.5), function(p) -reml(p[1], p[2]), method = "L-BFGS-B", lower = 1e-6,
                    control = list(factr = 1))
  expect_equal(-o$value, f$reml_loglik, tolerance = 1e-8)
})
