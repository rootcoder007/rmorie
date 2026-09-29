n <- 10
W <- 1 * (abs(outer(1:n, 1:n, "-")) == 1)
W <- W / rowSums(W)
X <- cbind(1, c(2, -1, 0.1, 1.5, 0.6, -0.4, 0.9, -1.3, 0.2, 1.1), cos(0:9))
b <- c(-0.2, 0.9, 0.4)
rho <- 0.35
Si <- solve(diag(n) - rho * W)
eta <- as.vector(Si %*% X %*% b)

test_that("impacts and probabilities recompute", {
  d <- stats::dnorm(eta)
  r <- spprmf(b, rho, X, W)
  expect_equal(r$direct, mean(d * diag(Si)) * b[2:3], tolerance = 1e-12)
  expect_equal(r$total, mean(d * rowSums(Si)) * b[2:3], tolerance = 1e-12)
  expect_equal(sprmfdi(b, rho, X, W)$indirect, r$total - r$direct)
  l <- stats::dlogis(eta)
  expect_equal(splgtmf(b, rho, X, W)$total, mean(l * rowSums(Si)) * b[2:3], tolerance = 1e-12)
  expect_equal(spprprd(b, X, W, rho = rho), stats::pnorm(eta / sqrt(diag(tcrossprod(Si)))), tolerance = 1e-14)
})

test_that("GHK is exact for independent latents and the fit is an optimum", {
  y <- c(1, 0, 1, 1, 0, 0, 1, 0, 0, 1)
  X2 <- X[, 1:2]
  U <- matrix(stats::runif(40), 4)
  ref <- sum(stats::pnorm((2 * y - 1) * as.vector(X2 %*% c(-0.3, 0.8)), log.p = TRUE))
  expect_equal(.spbl_ghk(c(-0.3, 0.8, 0), y, X2, W, U), ref, tolerance = 1e-12)
  r <- spprml(y, X2, W, nsim = 10, seed = 3)
  U <- t(vapply(0:9, function(k) .morie_random_uniform(n, seed = 3, stream = k), numeric(n)))
  par <- c(r$coefficients, atanh(r$rho))
  expect_equal(.spbl_ghk(par, y, X2, W, U), r$loglik, tolerance = 1e-12)
  for (j in 1:3) expect_lte(.spbl_ghk(replace(par, j, par[j] + 1e-3), y, X2, W, U), r$loglik + 1e-12)
})

test_that("panel designs recompute", {
  Wp <- 0.5 * (abs(outer(1:6, 1:6, "-")) %in% c(1, 5))
  dim(Wp) <- c(6, 6)
  k <- 0:59
  Xp <- matrix(sin(1.7 * k) + 0.3 * cos(0.4 * k))
  yp <- as.numeric(Xp[, 1] + 0.8 * sin(3.3 * k + 1) + 0.3 * ((k %% 6) - 3) / 6 > 0)
  big <- kronecker(diag(10), Wp)
  xbar <- tapply(Xp[, 1], k %% 6, mean)
  ref <- SpatialProbitGmm(yp, cbind(1, Xp, xbar[k %% 6 + 1]), big)
  expect_equal(sptrx(yp, Xp, Wp, rep(0:5, 10))$rho, ref$rho, tolerance = 1e-12)
  D <- cbind(1, Xp, outer(k %% 6, 1:5, "==") * 1)
  ref2 <- SpatialProbitGmm(yp, D, big, Z = cbind(D, big %*% Xp))
  expect_equal(sptfx(yp, Xp, Wp, rep(0:5, 10))$rho, ref2$rho, tolerance = 1e-12)
  expect_equal(splgtml(c(1, 0, 1, 1, 0, 0, 1, 0, 0, 1), X[, 2], W)$rho,
               SpatialLogitGmm(c(1, 0, 1, 1, 0, 0, 1, 0, 0, 1), X[, 1:2], W)$rho)
})
