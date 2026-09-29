# Coverage for Horowitz (2009) single-index WNLS, quantile average
# derivatives, Klein-Spady, the smoothed maximum score interval, the
# Theorem 4.1 check, the transformation-model estimators, Tikhonov NPIV,
# parametric transformations and the Weibull-heterogeneity estimator.

gk <- function(u) exp(-0.5 * u^2) / sqrt(2 * pi)
set.seed(31)
Xs <- cbind(rnorm(25), rnorm(25))
zs <- as.numeric(Xs %*% c(1, 0.7))

test_that("Sindex reports the leave-one-out WNLS objective at its estimate", {
  y <- sin(zs) + rnorm(25, sd = 0.1)
  r <- Sindex(Xs, y, h = 0.5, niter = 4)
  z <- as.numeric(Xs %*% r$estimate)
  K <- gk(outer(z, z, "-") / 0.5)
  diag(K) <- 0
  den <- rowSums(K) / (25 * 0.5)
  gh <- (as.numeric(K %*% y) / (25 * 0.5)) / den
  keep <- den > 0.01 * mean(den)
  expect_equal(r$index, z, tolerance = 1e-12)
  expect_equal(r$ghat, gh, tolerance = 1e-12)
  expect_equal(r$objective, sum((y - gh)[keep]^2) / 25, tolerance = 1e-12)
  expect_equal(r$estimate[1], 1)
  expect_equal(r$se[1], 0)
  expect_equal(morie_horowitz_single_index_model(Xs, y, h = 0.5, niter = 4)$objective, r$objective)
  expect_error(Sindex(Xs[, 1], y), "at least two covariates")
})

test_that("Simquant normalises the average quantile derivative", {
  y <- zs + rnorm(25, sd = 0.3)
  r <- Simquant(Xs, y, alpha = 0.5, h = 0.8, hg = 0.5, niter = 15, ngrid = 4)
  expect_equal(r$estimate, r$delta / r$delta[1], tolerance = 1e-12)
  expect_equal(r$index, as.numeric(Xs %*% r$estimate), tolerance = 1e-12)
  expect_equal(r$grid, seq(min(r$index), max(r$index), length.out = 4), tolerance = 1e-12)
  expect_equal(morie_horowitz_sim_quantile(Xs, y, alpha = 0.5, h = 0.8, hg = 0.5, niter = 15, ngrid = 4)$delta, r$delta)
  expect_error(Simquant(Xs, y, alpha = 0), "alpha")
})

test_that("Spmlebin reports the trimmed Klein-Spady likelihood", {
  y <- as.numeric(zs + rnorm(25, sd = 0.5) > 0)
  r <- Spmlebin(Xs, y, h = 0.6, niter = 4)
  z <- as.numeric(Xs %*% r$estimate)
  K <- gk(outer(z, z, "-") / 0.6)
  diag(K) <- 0
  den <- rowSums(K)
  gh <- pmin(pmax(as.numeric(K %*% y) / den, 1e-4), 1 - 1e-4)
  keep <- den > 0.01 * mean(den)
  expect_equal(r$ghat, gh, tolerance = 1e-12)
  expect_equal(r$loglik, sum((y * log(gh) + (1 - y) * log(1 - gh))[keep]) / 25, tolerance = 1e-12)
  expect_equal(morie_horowitz_semipar_mle_binary(Xs, y, h = 0.6, niter = 4)$loglik, r$loglik)
  expect_error(Spmlebin(Xs, y + 2), "binary")
})

test_that("Smsciband builds the analytic smoothed-maximum-score interval", {
  y <- as.numeric(zs + rnorm(25, sd = 0.5) > 0)
  h <- 0.5
  hs <- 0.7
  r <- Smsciband(Xs, y, h = h, hstar = hs, alpha = 0.1, niter = 4)
  sg <- 2 * y - 1
  b <- r$estimate
  z <- as.numeric(Xs %*% b)
  x2 <- Xs[, 2]
  expect_equal(r$objective, sum(sg * pnorm(z / h)) / 25, tolerance = 1e-12)
  Dn <- sum(x2^2 * gk(z / h)^2) / (25 * h)
  # finite-difference Hessian of the smoothed score (eps 1e-5) against its analytic form
  Qn <- sum(x2^2 * sg * (-(z / h)) * gk(z / h)) / (25 * h^2)
  expect_equal(r$vcov[1, 1], Dn / Qn^2, tolerance = 1e-6)
  se <- sqrt(Dn / Qn^2 / (25 * h))
  expect_equal(r$se, se, tolerance = 1e-6)
  An <- sum(x2 * sg * gk(z / hs)) / (25 * hs^3)
  bh <- b[2] + 25^(-2 / 5) * An / Qn
  expect_equal(r$biascorrected[2], bh, tolerance = 1e-6)
  expect_equal(r$lower, bh - qnorm(0.95) * se, tolerance = 1e-6)
  nb <- Smsciband(Xs, y, h = h, hstar = hs, alpha = 0.1, niter = 4, biascorrect = FALSE)
  expect_equal(nb$lower, b[2] - qnorm(0.95) * nb$se, tolerance = 1e-12)
  expect_equal(morie_horowitz_sms_confidence(Xs, y, h = h, hstar = hs, niter = 4)$objective, r$objective)
  expect_error(Smsciband(Xs, y, alpha = 1), "alpha")
})

test_that("Binidmed checks rank, scale and support coverage", {
  set.seed(2)
  X <- cbind(runif(200, -2, 2), rnorm(200))
  r <- Binidmed(X, c(1, 0.5), ncell = 2, nbin = 4)
  cut <- median(X[, 2])
  cell <- as.numeric(X[, 2] >= cut)
  ed <- seq(min(X[, 1]), max(X[, 1]), length.out = 5)
  cov <- vapply(0:1, function(c) {
    v <- X[cell == c, 1]
    mean(vapply(1:4, function(k) any(if (k == 4) v >= ed[k] & v <= ed[k + 1] else v >= ed[k] & v < ed[k + 1]), TRUE))
  }, 0)
  expect_equal(r$coverage, min(cov), tolerance = 1e-12)
  expect_true(r$conda && r$condscale)
  expect_false(Binidmed(X, c(2, 0.5))$condscale)
  expect_false(Binidmed(cbind(X[, 1], X[, 1]), c(1, 1))$conda)
  expect_equal(morie_horowitz_thm4_1_id_median(X, c(1, 0.5), 2, 4)$coverage, r$coverage)
})

test_that("Hrztmod integrates the ratio of kernel derivatives; Hrztf estimates F on the residual grid", {
  set.seed(11)
  n <- 30
  X <- cbind(rnorm(n), rnorm(n))
  y <- exp(X %*% c(1, 0.5) + rnorm(n, sd = 0.5))[, 1]
  bw <- 0.7
  r <- Hrztmod(X, y, ny = 5, nz = 5, bandwidth = bw)
  ade <- numeric(2)
  for (i in 1:n) for (l in 1:n) if (i != l) {
    us <- (X[i, ] - X[l, ]) / bw
    ade <- ade + (-2) * y[i] * (-us) * prod(gk(us)) / ((n - 1) * bw^3) / n
  }
  beta <- ade / abs(ade[1])
  Z <- as.numeric(X %*% beta)
  y2 <- quantile(y, 0.1, names = FALSE)
  y1 <- quantile(y, 0.9, names = FALSE)
  za <- quantile(Z, 0.25, names = FALSE)
  zb <- quantile(Z, 0.75, names = FALSE)
  yg <- seq(y2, y1, length.out = 5)
  zg <- seq(za, zb, length.out = 5)
  wz <- rep((zb - za) / 4, 5)
  wz[c(1, 5)] <- wz[c(1, 5)] / 2
  inner <- vapply(yg, function(v) {
    s <- 0
    for (q in 1:5) {
      uu <- (Z - zg[q]) / bw
      kv <- gk(uu)
      dk <- uu / bw * kv
      ind <- y <= v
      A <- sum(ind * kv)
      B <- sum(kv)
      Gz <- (sum(ind * dk) * B - A * sum(dk)) / B^2
      Gy <- sum(gk((y - v) / bw) * kv) / (bw * B)
      s <- s + wz[q] / (zb - za) * Gy / Gz
    }
    s
  }, 0)
  T <- numeric(5)
  dv <- (y1 - y2) / 4
  for (k in 4:5) T[k] <- T[k - 1] - 0.5 * dv * (inner[k - 1] + inner[k])
  for (k in 2:1) T[k] <- T[k + 1] + 0.5 * dv * (inner[k] + inner[k + 1])
  expect_equal(r$beta_hat, beta, tolerance = 1e-9)
  expect_equal(r$y_grid, yg, tolerance = 1e-12)
  expect_equal(r$T_hat, T, tolerance = 1e-9)
  expect_equal(r$y0, yg[3], tolerance = 1e-12)
  expect_equal(morie_horowitz_transformation_model(X, y, ny = 5, nz = 5, bandwidth = bw)$T_hat, r$T_hat)
  expect_error(Hrztmod(X[1:9, ], y[1:9]), "at least 10")

  tf <- Hrztf(X, y, ny = 5, nz = 5, nu = 4, bandwidth = bw)
  Tn <- function(v) if (v < y2) -1e12 else if (v > y1) 1e12 else approx(yg, T, v)$y
  U <- vapply(y, Tn, 0) - Z
  fin <- U[abs(U) < 5e11]
  ug <- seq(min(fin), max(fin), length.out = 4)
  Fh <- vapply(ug, function(u) {
    inb <- Z > T[1] - u & Z <= T[5] - u
    sum(inb & U <= u) / sum(inb)
  }, 0)
  expect_equal(tf$u_grid, ug, tolerance = 1e-9)
  expect_equal(tf$F_hat, Fh, tolerance = 1e-12)
  expect_true(tf$A_le_B)
  expect_equal(morie_horowitz_both_nonpar_transform(X, y, ny = 5, nz = 5, nu = 4, bandwidth = bw)$F_hat, tf$F_hat)
  expect_error(Hrztf(X, y, nu = 2), "nu must")
})

test_that("Hrztiku is the Tikhonov-regularised leave-one-out NPIV solution", {
  set.seed(14)
  n <- 15
  w <- rnorm(n)
  x <- w + rnorm(n, sd = 0.4)
  y <- 2 * x + rnorm(n, sd = 0.2)
  r <- Hrztiku(x, y, w, bandwidth = 0.3, alpha = 0.01, grid = 6)
  u <- (rank(x) - 0.5) / n
  v <- (rank(w) - 0.5) / n
  z <- seq(0, 1, length.out = 6)
  wq <- rep(0.2, 6)
  wq[c(1, 6)] <- 0.1
  KX <- gk(outer(z, u, "-") / 0.3)
  KWo <- gk(outer(v, v, "-") / 0.3)
  KW <- gk(outer(z, v, "-") / 0.3)
  f <- KX %*% t(KW) / (n * 0.09)
  mass <- sum(outer(wq, wq) * f)
  S <- KX %*% t(KWo)
  Floo <- (S - KX * matrix(diag(KWo), 6, n, byrow = TRUE)) / ((n - 1) * 0.09 * mass)
  rh <- as.numeric(Floo %*% y) / n
  fx <- f / mass
  Th <- fx %*% (wq * t(fx))
  A <- t(Th * wq) + diag(0.01, 6)
  expect_equal(r$raw_mass, mass, tolerance = 1e-12)
  expect_equal(r$r_hat, rh, tolerance = 1e-12)
  expect_equal(r$g_hat, as.numeric(solve(A, rh)), tolerance = 1e-9)
  expect_equal(morie_horowitz_tikhonov_unknown_T(x, y, w, bandwidth = 0.3, alpha = 0.01, grid = 6)$g_hat, r$g_hat)
  expect_error(Hrztiku(x, y, w, alpha = 0), "ill-posed")
})

test_that("Hrztpar minimises the NL2SLS criterion over the transformation parameter", {
  set.seed(15)
  n <- 20
  X <- cbind(1, runif(n))
  y <- exp(0.3 + X[, 2] + rnorm(n, sd = 0.1))
  r <- Hrztpar(X, y, a_lo = -1, a_hi = 1, ngrid = 5, refine = 10)
  W <- cbind(X, X[, 2]^2)
  crit <- function(a) {
    Ty <- if (a == 0) log(y) else (y^a - 1) / a
    O <- solve(crossprod(W) + diag(1e-12, 3))
    M <- t(X) %*% W %*% O %*% t(W)
    b <- solve(M %*% X, M %*% Ty)
    g <- crossprod(W, Ty - X %*% b) / n
    list(v = as.numeric(t(g) %*% O %*% g), b = as.numeric(b))
  }
  cr <- crit(r$theta_hat)
  expect_equal(r$criterion, cr$v, tolerance = 1e-8)
  expect_equal(r$beta_hat, cr$b, tolerance = 1e-8)
  Ty <- (y^r$theta_hat - 1) / r$theta_hat
  expect_equal(r$resid, as.numeric(Ty - X %*% r$beta_hat), tolerance = 1e-12)
  bd <- Hrztpar(X, y, T_family = "bickel-doksum", a_lo = 0.2, a_hi = 1.5, ngrid = 5, refine = 5)
  expect_equal(bd$T_family, "bickel-doksum")
  expect_equal(morie_horowitz_parametric_T(X, y, a_lo = -1, a_hi = 1, ngrid = 5, refine = 10)$theta_hat, r$theta_hat)
  expect_error(Hrztpar(X, y, T_family = "yeo"), "T_family")
  expect_error(Hrztpar(X[, 1, drop = FALSE], y), "intercept and at least one covariate")
})

test_that("Hrzweib is Honore's two-order-statistic estimator", {
  set.seed(16)
  n <- 200
  X <- cbind(1, rnorm(n))
  t <- rweibull(n, 1.5, exp(0.2 * X[, 2]))
  ev <- rbinom(n, 1, 0.9)
  r <- Hrzweib(t, X, event = ev)
  yv <- t[ev == 1]
  m <- length(yv)
  s <- sort(yv)
  m1 <- round(m^0.4)
  m2 <- round(m^0.7)
  rho <- 1 - 0.5 * (m^-0.6 - m^-0.3) / (0.3 * log(m))
  a <- -rho * 0.3 * log(m) / (log(s[m1]) - log(s[m2]))
  expect_equal(c(r$m1, r$m2), as.integer(c(m1, m2)))
  expect_equal(r$alpha_hat, a, tolerance = 1e-12)
  g <- qr.solve(X[ev == 1, ], log(yv))
  expect_equal(r$gamma_hat, g, tolerance = 1e-9)
  expect_equal(r$beta_hat, a * g, tolerance = 1e-9)
  expect_equal(r$sigma2, (1 / (0.3 * log(m)))^2 * (m^0.6 - m^0.3) / m, tolerance = 1e-12)
  expect_equal(morie_horowitz_weibull_heterogeneity(t, X, event = ev)$alpha_hat, a, tolerance = 1e-12)
  expect_error(Hrzweib(t, X, delta1 = 0.2, delta2 = 0.3), "delta2 < delta1")
  expect_error(Hrzweib(t, X, mixing_dist = "gamma"), "nonparametric")
})
