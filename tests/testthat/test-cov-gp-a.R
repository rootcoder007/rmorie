# Coverage for the Gaussian-process family (classification, composed and
# deep kernels, heteroscedastic, multitask, mixture of experts, Bayesian
# hyperparameter averaging, FITC, kernel ridge), the GPCM and GPD
# distributions and the MVSML FDA helpers; recomputed with solve().

se_k <- function(P, Q, ell = 1, v = 1) {
  P <- as.matrix(P)
  Q <- as.matrix(Q)
  D <- matrix(0, nrow(P), nrow(Q))
  for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q))) D[i, j] <- sum((P[i, ] - Q[j, ])^2)
  v * exp(-0.5 * D / ell^2)
}
gp_ll <- function(K, y) {
  -0.5 * sum(y * solve(K, y)) - 0.5 * as.numeric(determinant(K)$modulus) - 0.5 * length(y) * log(2 * pi)
}

X1 <- matrix(c(-2, -1.2, -0.3, 0.4, 1.1, 2.0), ncol = 1)
y1 <- c(0.3, 0.9, 0.2, -0.4, -1.0, -0.2)

test_that("Gpdkl, Gpregb and Krrdual are the GP posterior formulas", {
  Xt <- matrix(c(-1.5, 0.8), ncol = 1)
  r <- Gpdkl(X1, y1, Xt, lengthscale = 0.8, variance = 1.5, noise = 0.1)
  K <- se_k(X1, X1, 0.8, 1.5) + diag(0.1, 6)
  Ks <- se_k(Xt, X1, 0.8, 1.5)
  expect_equal(r$mean, as.numeric(Ks %*% solve(K, y1)), tolerance = 1e-9)
  expect_equal(r$variance, 1.5 - rowSums(Ks * t(solve(K, t(Ks)))), tolerance = 1e-9)
  expect_equal(r$loglik, gp_ll(K, y1), tolerance = 1e-9)
  nn <- list(W1 = matrix(c(0.5, -1), 1, 2), b1 = c(0.1, 0), W2 = matrix(c(1, 2), 2, 1))
  g <- function(P) tanh(P %*% nn$W1 + matrix(nn$b1, nrow(P), 2, byrow = TRUE)) %*% nn$W2
  rd <- Gpdkl(X1, y1, Xt, nn = nn, noise = 0.1)
  Kd <- se_k(g(X1), g(X1)) + diag(0.1, 6)
  expect_equal(rd$mean, as.numeric(se_k(g(Xt), g(X1)) %*% solve(Kd, y1)), tolerance = 1e-9)
  expect_error(Gpdkl(X1, y1, nn = list(W1 = matrix(1, 2, 2), W2 = matrix(1, 2, 1))), "one row per input")
  expect_error(Gpdkl(X1, y1, noise = -1), "non-negative")

  gb <- Gpregb(X1, y1, X_test = Xt, lengthscales = c(0.5, 1.5), noises = c(0.05, 0.2), variance = 1)
  grid <- expand.grid(s = c(0.05, 0.2), e = c(0.5, 1.5))
  mus <- vars <- matrix(0, 4, 2)
  lls <- numeric(4)
  for (g2 in 1:4) {
    Kg <- se_k(X1, X1, grid$e[g2]) + diag(grid$s[g2], 6)
    Ksg <- se_k(Xt, X1, grid$e[g2])
    lls[g2] <- gp_ll(Kg, y1)
    mus[g2, ] <- Ksg %*% solve(Kg, y1)
    vars[g2, ] <- 1 - rowSums(Ksg * t(solve(Kg, t(Ksg))))
  }
  w <- exp(lls - max(lls))
  w <- w / sum(w)
  expect_equal(gb$loglik, lls, tolerance = 1e-9)
  expect_equal(gb$mean, colSums(w * mus), tolerance = 1e-9)
  expect_equal(gb$variance, colSums(w * (vars + mus^2)) - colSums(w * mus)^2, tolerance = 1e-9)
  expect_error(Gpregb(X1, y1, noises = 0), "positive")

  kr <- Krrdual(X1, y1, Xt, lam = 0.3, gamma = 0.7)
  Kr <- exp(-0.7 * as.matrix(dist(X1))^2) + diag(0.3, 6)
  kst <- exp(-0.7 * outer(Xt[, 1], X1[, 1], "-")^2)
  expect_equal(kr$alpha, as.numeric(solve(Kr, y1)), tolerance = 1e-9)
  expect_equal(kr$pred, as.numeric(kst %*% solve(Kr, y1)), tolerance = 1e-9)
  expect_equal(kr$var, 1 - rowSums(kst * t(solve(Kr, t(kst)))), tolerance = 1e-9)
})

test_that("Gphtr alternates the mean GP and the log-noise GP", {
  Xt <- matrix(c(0, 1.5), ncol = 1)
  r <- Gphtr(X1, y1, Xt, lengthscale = 1, variance = 1, noise0 = 0.2, noise_lengthscale = 1.2, iters = 2)
  K <- se_k(X1, X1)
  Kn <- se_k(X1, X1, 1.2)
  nz <- rep(0.2, 6)
  for (k in 1:2) {
    fit <- K %*% solve(K + diag(nz), y1)
    z <- log(pmax((y1 - fit)^2, 1e-6))
    nz <- exp(mean(z) + as.numeric(Kn %*% solve(Kn + diag(0.25, 6), z - mean(z))))
  }
  M <- K + diag(nz)
  Ks <- se_k(Xt, X1)
  expect_equal(r$noise, nz, tolerance = 1e-9)
  expect_equal(r$mean, as.numeric(Ks %*% solve(M, y1)), tolerance = 1e-9)
  zb <- mean(log(nz))
  expect_equal(r$noise_test, exp(zb + as.numeric(se_k(Xt, X1, 1.2) %*% solve(Kn + diag(0.25, 6), log(nz) - zb))),
               tolerance = 1e-9)
  expect_equal(r$loglik, gp_ll(M, y1), tolerance = 1e-9)
  r0 <- Gphtr(X1, y1, Xt, noise0 = 0.3, iters = 0)
  expect_equal(r0$noise_test, c(0.3, 0.3))
  expect_error(Gphtr(X1, y1, noise0 = 0), "noise0")
})

test_that("Gpmlt uses the Kronecker task-input covariance", {
  Y <- rbind(y1, 0.5 * y1 + 0.1)
  B <- matrix(c(1, 0.6, 0.6, 1.2), 2)
  Xt <- matrix(c(0.1, -0.5), ncol = 1)
  r <- Gpmlt(X1, Y, Xt, task_cov = B, lengthscale = 0.9, noise = 0.05)
  K <- kronecker(B, se_k(X1, X1, 0.9)) + diag(0.05, 12)
  al <- solve(K, as.numeric(t(Y)))
  Ks <- kronecker(B, se_k(Xt, X1, 0.9))
  expect_equal(as.numeric(t(r$mean)), as.numeric(Ks %*% al), tolerance = 1e-9)
  expect_equal(r$loglik, gp_ll(K, as.numeric(t(Y))), tolerance = 1e-9)
  expect_error(Gpmlt(X1, Y, task_cov = diag(3)), "T x T")
  expect_error(Gpmlt(X1, Y[, 1:3]), "one observation per input")
})

test_that("Gpmoe gates block GP experts by distance to block centroids", {
  Xt <- matrix(c(-1, 0.5, 1.8), ncol = 1)
  r <- Gpmoe(X1, y1, Xt, K = 2, ell = 0.9, noise = 0.05)
  blocks <- list(1:3, 4:6)
  mu <- vv <- matrix(0, 2, 3)
  cen <- numeric(2)
  for (k in 1:2) {
    b <- blocks[[k]]
    cen[k] <- mean(X1[b, 1])
    Kb <- se_k(X1[b, , drop = FALSE], X1[b, , drop = FALSE], 0.9) + diag(0.05, 3)
    Ksb <- se_k(X1[b, , drop = FALSE], Xt, 0.9)
    mu[k, ] <- t(Ksb) %*% solve(Kb, y1[b])
    vv[k, ] <- 1 - colSums(Ksb * solve(Kb, Ksb))
  }
  G <- t(vapply(Xt[, 1], function(x) {
    d <- -(cen - x)^2
    exp(d - max(d)) / sum(exp(d - max(d)))
  }, numeric(2)))
  m <- rowSums(G * t(mu))
  expect_equal(r$gate, G, tolerance = 1e-12)
  expect_equal(r$mean, m, tolerance = 1e-9)
  expect_equal(r$var, rowSums(G * t(vv + mu^2)) - m^2, tolerance = 1e-9)
})

test_that("Fitcgp is the FITC predictive distribution", {
  Z <- matrix(c(-1.5, 0, 1.5), ncol = 1)
  Xt <- matrix(c(-0.7, 0.9), ncol = 1)
  r <- Fitcgp(X1, y1, Xt, inducing = Z, gamma = 0.5, sigma2 = 0.1)
  kf <- function(A, B) exp(-0.5 * outer(A[, 1], B[, 1], "-")^2)
  Kmm <- kf(Z, Z) + diag(1e-8, 3)
  Knm <- kf(X1, Z)
  lam <- pmax(1 - rowSums(Knm * t(solve(Kmm, t(Knm)))), 0)
  Li <- diag(1 / (lam + 0.1))
  A <- Kmm + t(Knm) %*% Li %*% Knm
  Ktm <- kf(Xt, Z)
  expect_equal(r$lam, lam, tolerance = 1e-9)
  expect_equal(r$pred, as.numeric(Ktm %*% solve(A, t(Knm) %*% Li %*% y1)), tolerance = 1e-9)
  expect_equal(r$var, 1 - rowSums(Ktm * t(solve(Kmm, t(Ktm)))) + rowSums(Ktm * t(solve(A, t(Ktm)))),
               tolerance = 1e-9)
  d <- Fitcgp(X1, y1, Xt, inducing = Z, gamma = 0.5, sigma2 = 0.1, kind = "dtc")
  expect_equal(d$lam, rep(0, 6))
})

test_that("Gpkern composes RBF and warped kernels", {
  X <- matrix(c(0.1, 0.8, 1.9, -0.4, 0.3, 1.0), 3)
  sp <- list(op = "prod", parts = list(list(type = "rbf", lengthscale = 0.7, variance = 2),
                                       list(type = "warp", lengthscale = 1.3)))
  r <- Gpkern(X, kernel_spec = sp)
  W <- cbind(sin(X[, 1]), cos(X[, 1]), sin(X[, 2]), cos(X[, 2]))
  K <- se_k(X, X, 0.7, 2) * se_k(W, W, 1.3)
  expect_equal(r$K, K, tolerance = 1e-12)
  expect_equal(r$min_eigenvalue, min(eigen(K, symmetric = TRUE)$values), tolerance = 1e-9)
  expect_equal(r$is_psd, 1L)
  s <- Gpkern(X, X[1:2, ], kernel_spec = list(op = "sum", parts = sp$parts))
  expect_equal(s$K, se_k(X, X[1:2, ], 0.7, 2) + se_k(W, W[1:2, ], 1.3), tolerance = 1e-12)
  expect_true(is.nan(s$min_eigenvalue))
  expect_equal(Gpkern(X)$K, se_k(X, X), tolerance = 1e-12)
  expect_error(Gpkern(X, kernel_spec = list(op = "max", parts = sp$parts)), "sum or prod")
  expect_error(Gpkern(X, kernel_spec = list(parts = list(list(type = "poly")))), "unknown kernel")
})

test_that("Gpcla reaches the Laplace mode and predicts with the averaged probit", {
  yb <- c(1, 1, 1, 0, 0, 0)
  Xt <- matrix(c(-1, 1.5), ncol = 1)
  r <- Gpcla(X1, yb, Xt, lengthscale = 1, variance = 2, iters = 40)
  K <- se_k(X1, X1, 1, 2)
  s <- ifelse(yb == 1, 1, -1)
  f <- r$f_mode
  g <- s * dnorm(s * f) / pnorm(s * f)
  expect_lt(max(abs(f - as.numeric(K %*% g))), 1e-9)
  rr <- dnorm(s * f) / pnorm(s * f)
  w <- rr^2 + s * f * rr
  Ks <- se_k(Xt, X1, 1, 2)
  mu <- as.numeric(Ks %*% solve(K, f))
  v <- 2 - rowSums(Ks * t(solve(K + diag(1 / w), t(Ks))))
  expect_equal(r$latent_mean, mu, tolerance = 1e-8)
  expect_equal(r$latent_var, v, tolerance = 1e-8)
  expect_equal(r$p, pnorm(mu / sqrt(1 + v)), tolerance = 1e-8)
  expect_equal(r$predicted, as.integer(r$p >= 0.5))
  expect_error(Gpcla(X1, c(1, 2, 0, 0, 1, 0)), "0 or 1")
})

test_that("Gpcgs starts at the prior (KL 0) with the Gauss-Hermite expected log-likelihood", {
  yb <- c(1, 1, 0, 1, 0, 0)
  r <- Gpcgs(X1, yb, M = 3, iters = 0, nodes = 9)
  B <- matrix(0, 9, 9)
  for (i in 1:8) B[i, i + 1] <- B[i + 1, i] <- sqrt(i / 2)
  e <- eigen(B, symmetric = TRUE)
  x <- e$values
  w <- sqrt(pi) * e$vectors[1, ]^2
  s <- ifelse(yb == 1, 1, -1)
  # with q(u) = p(u) every latent marginal is N(0, variance = 1)
  ell <- sum(vapply(s, function(si) sum(w * log(pnorm(si * sqrt(2) * x))) / sqrt(pi), 0))
  expect_equal(r$kl, 0, tolerance = 1e-9)
  expect_equal(r$elbo, ell, tolerance = 1e-9)
  expect_equal(r$p, rep(0.5, 6), tolerance = 1e-12)
  expect_length(r$elbo_path, 1)
  r2 <- Gpcgs(X1, yb, M = 2, iters = 2, nodes = 5)
  expect_length(r2$elbo_path, 3)
  expect_equal(r2$elbo, r2$elbo_path[3])
  expect_error(Gpcgs(X1, yb, M = 7), "M must")
})

test_that("Gpcm, GpdD and Gpdsh follow their definitions", {
  b <- c(-1, 0.2, 1.1)
  th <- c(-0.5, 0.3, 1.4)
  y <- c(0, 2, 1)
  r <- Gpcm(y, th, 1.3, b)
  P <- t(vapply(th, function(t) {
    z <- cumsum(1.3 * (t - b))
    exp(z) / sum(exp(z))
  }, numeric(3)))
  expect_equal(r$p_observed, P[cbind(1:3, y + 1)], tolerance = 1e-12)
  expect_equal(r$loglik, sum(log(P[cbind(1:3, y + 1)])), tolerance = 1e-12)
  expect_error(Gpcm(3, 0, 1, b), "category range")
  expect_error(Gpcm(0, 0, 0, b), "a must")

  gd <- GpdD(2, 0.3, x = c(0.5, 3), p = c(0.2, 0.9))
  expect_equal(gd$cdf, 1 - (1 + 0.3 * c(0.5, 3) / 2)^(-1 / 0.3), tolerance = 1e-12)
  expect_equal(gd$pdf, (1 + 0.3 * c(0.5, 3) / 2)^(-1 / 0.3 - 1) / 2, tolerance = 1e-12)
  expect_equal(gd$quantile, 2 * ((1 - c(0.2, 0.9))^-0.3 - 1) / 0.3, tolerance = 1e-12)
  expect_equal(gd$mean, 2 / 0.7, tolerance = 1e-12)
  expect_equal(gd$variance, 4 / (0.49 * 0.4), tolerance = 1e-12)
  ge <- GpdD(1.5, 0, x = 2)
  expect_equal(ge$cdf, pexp(2, 1 / 1.5), tolerance = 1e-12)
  gn <- GpdD(1, -0.5, x = c(1, 3))
  expect_equal(gn$upper_endpoint, 2)
  expect_equal(gn$cdf, c(1 - 0.5^2, 1), tolerance = 1e-12)
  expect_equal(GpdD(1, 0.6)$variance, Inf)
  expect_error(GpdD(0, 0.1), "sigma")

  v <- c(0.1, -0.2, 0.3, 0.0, 2.1, 1.8, 2.4, 2.0)
  ds <- Gpdsh(v, window = 3, tau = 1)
  kl <- vapply(3:5, function(s) {
    a <- v[(s - 2):s]
    bb <- v[(s + 1):(s + 3)]
    sa <- sd(a)
    sb <- sd(bb)
    log(sa / sb) + (sb^2 + (mean(bb) - mean(a))^2) / (2 * sa^2) - 0.5
  }, 0)
  expect_equal(ds$kl, kl, tolerance = 1e-12)
  expect_equal(ds$change_point, (3:5)[which.max(kl)])
  expect_equal(ds$n_shifts, sum(kl > 1))
  expect_error(Gpdsh(v, window = 5), "two windows")
})

test_that("the MVSML FDA helpers and the SVM intercept", {
  t <- c(0, 0.25, 0.5, 1)
  F1 <- morie_fda_basis_deriv(t, 4, p = 1, kind = "fourier")
  w1 <- 2 * pi
  expect_equal(F1[, 1], rep(0, 4))
  expect_equal(F1[, 2], w1 * cos(w1 * t), tolerance = 1e-12)
  expect_equal(F1[, 3], -w1 * sin(w1 * t), tolerance = 1e-12)
  expect_equal(F1[, 4], 2 * w1 * cos(2 * w1 * t), tolerance = 1e-12)
  F0 <- morie_fda_basis_deriv(t, 3, p = 0, period = 2)
  expect_equal(F0[, 2], sin(pi * t), tolerance = 1e-12)
  P2 <- morie_fda_basis_deriv(t, 4, p = 2, kind = "poly")
  expect_equal(P2[, 1:2], matrix(0, 4, 2))
  expect_equal(P2[, 4], 6 * t, tolerance = 1e-12)

  X <- matrix(1:6, 3)
  ei <- morie_fda_env_interaction(X, c("b", "a", "c"))
  expect_equal(ei$kept_levels, c("b", "c"))
  expect_equal(ei$X_EF, rbind(c(1, 4, 0, 0), c(0, 0, 0, 0), c(0, 0, 3, 6)))
  expect_equal(morie_fda_env_interaction(X, c(1, 1, 2), reference = FALSE)$n_columns, 4L)

  A <- matrix(c(1, 2, 3, 4), 2)
  B <- matrix(c(5, 6, 7, 8, 9, 10), 2)
  kr <- morie_khatri_rao_rows(A, B)
  expect_equal(kr[1, ], as.numeric(kronecker(A[1, ], B[1, ])))
  expect_equal(kr[2, ], as.numeric(kronecker(A[2, ], B[2, ])))

  Xs <- rbind(c(1, 1), c(2, 0), c(-1, -1), c(0, -2))
  ys <- c(1, 1, -1, -1)
  al <- c(0.25, 0, 0.25, 0)
  G <- Xs %*% t(Xs)
  b <- mean(c(1 - sum(al * ys * G[1, ]), -1 - sum(al * ys * G[3, ])))
  expect_equal(morie_svm_intercept(al, Xs, ys), b, tolerance = 1e-12)
  expect_equal(morie_svm_intercept(al, K = G + 1, y = ys), mean(c(1 - sum(al * ys * (G[1, ] + 1)),
                                                                   -1 - sum(al * ys * (G[3, ] + 1)))),
               tolerance = 1e-12)
  expect_equal(morie_svm_intercept(rep(0, 4), Xs, ys), 0)
})
