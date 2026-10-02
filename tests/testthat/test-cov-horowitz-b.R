# Coverage for Horowitz (2009) multiple-index and NPIV models, the
# proportional-hazards family, PLR identification and quantile PLR, the
# single-index rate helpers, series NPIV and the identification checks;
# rebuilt from the defining equations in the test body.

gk <- function(u) exp(-0.5 * u^2) / sqrt(2 * pi)
u01 <- function(v) (rank(v) - 0.5) / length(v)

test_that("Multindex estimates block directions by average derivatives", {
  set.seed(2)
  n <- 20
  X <- cbind(rnorm(n), rnorm(n), rnorm(n))
  y <- (X[, 1] + 0.5 * X[, 2])^2 + X[, 3] + rnorm(n, sd = 0.1)
  r <- Multindex(X[, 1:2], y, blocks = list(1:2), x0 = X[, 3], h = 0.8, hg = 0.5, ngrid = 3)
  b0 <- qr.solve(matrix(X[, 3]), y)
  res <- y - X[, 3] * b0
  W <- gk(outer(X[, 1], X[, 1], "-") / 0.8) * gk(outer(X[, 2], X[, 2], "-") / 0.8)
  d <- vapply(1:2, function(j) -2 * sum(res * W * (-outer(X[, j], X[, j], "-") / 0.64)) / (n^2 * 0.64), 0)
  expect_equal(r$beta0, b0, tolerance = 1e-12)
  expect_equal(r$estimate[[1]], d / d[1], tolerance = 1e-12)
  z <- as.numeric(X[, 1:2] %*% (d / d[1]))
  K <- gk(outer(z, z, "-") / 0.5)
  gh <- as.numeric(K %*% res) / rowSums(K)
  expect_equal(r$ghat, gh, tolerance = 1e-12)
  expect_equal(r$rss, sum((res - gh)^2), tolerance = 1e-12)
  g <- seq(min(z), max(z), length.out = 3)
  expect_equal(r$ggrid, vapply(g, function(t) sum(gk((t - z) / 0.5) * res) / sum(gk((t - z) / 0.5)), 0),
               tolerance = 1e-12)
  expect_equal(morie_horowitz_multiple_index_model(X[, 1:2], y, list(1:2), h = 0.8, hg = 0.5)$rss,
               Multindex(X[, 1:2], y, list(1:2), h = 0.8, hg = 0.5)$rss)
  expect_error(Multindex(X, y, list(1, 2:3), ngrid = 3), "single index")
  expect_error(Multindex(X, y, list()), "at least one index block")
})

test_that("Hrznpiv is the spectral cut-off solution of T g = r on the grid", {
  set.seed(4)
  n <- 15
  w <- rnorm(n)
  x <- w + rnorm(n, sd = 0.5)
  y <- x + rnorm(n, sd = 0.2)
  r <- Hrznpiv(x, y, w, bandwidth = 0.3, grid = 7, tol = 1e-4)
  u <- u01(x)
  v <- u01(w)
  z <- seq(0, 1, length.out = 7)
  wq <- rep(1 / 6, 7)
  wq[c(1, 7)] <- 1 / 12
  KX <- gk(outer(z, u, "-") / 0.3)
  KW <- gk(outer(z, v, "-") / 0.3)
  f <- KX %*% t(KW) / (n * 0.09)
  mass <- sum(outer(wq, wq) * f)
  f <- f / mass
  fW <- as.numeric(wq %*% f)
  mW <- as.numeric(KW %*% y) / rowSums(KW)
  rh <- as.numeric(f %*% (wq * mW * fW))
  tau <- f %*% (wq * t(f))
  S <- outer(sqrt(wq), sqrt(wq)) * tau
  e <- eigen(S, symmetric = TRUE)
  g <- numeric(7)
  for (j in which(e$values > 1e-4 * e$values[1])) {
    phi <- e$vectors[, j] / sqrt(wq)
    g <- g + sum(wq * rh * phi) / e$values[j] * phi
  }
  expect_equal(r$raw_mass, mass, tolerance = 1e-12)
  expect_equal(r$r_hat, rh, tolerance = 1e-12)
  expect_equal(r$eigenvalues, e$values, tolerance = 1e-9)
  expect_equal(r$n_terms, sum(e$values > 1e-4 * e$values[1]))
  expect_equal(r$g_hat, g, tolerance = 1e-7)
  expect_equal(r$trace_T, sum(wq * diag(tau)), tolerance = 1e-12)
  expect_equal(morie_horowitz_npiv_model(x, y, w, bandwidth = 0.3, grid = 7, tol = 1e-4)$r_hat, rh, tolerance = 1e-12)
  expect_error(Hrznpiv(x, y, w, tol = 0), "tol")
  expect_error(Hrznpiv(x[1:2], y[1:2], w[1:2]), "at least 3")
})

test_that("Hrzph is Cox's partial likelihood with the exp(-x b) sign convention", {
  skip_if_not_installed("survival")
  set.seed(6)
  n <- 25
  X <- cbind(rnorm(n), rbinom(n, 1, 0.5))
  t <- rexp(n, exp(-X %*% c(0.5, -0.8)))
  ev <- rbinom(n, 1, 0.8)
  r <- Hrzph(t, X, event = ev)
  f <- survival::coxph(survival::Surv(t, ev) ~ X, ties = "breslow",
                       control = survival::coxph.control(eps = 1e-14, toler.chol = 1e-16, iter.max = 100))
  expect_equal(r$beta_hat, -unname(coef(f)), tolerance = 1e-8)
  expect_equal(r$se, unname(sqrt(diag(vcov(f)))), tolerance = 1e-7)
  et <- sort(t[ev == 1])
  bh <- vapply(et, function(s) sum(1 / vapply(et[et <= s], function(u) sum(exp(-X[t >= u, ] %*% r$beta_hat)), 0)), 0)
  expect_equal(r$Lambda0, bh, tolerance = 1e-9)
  expect_equal(r$event_times, et)
  expect_equal(morie_horowitz_proportional_hazards(t, X, event = ev)$beta_hat, r$beta_hat)
  expect_error(Hrzph(t, X[-1, ]), "different number of rows")
})

test_that("Hrzphd reports the interval gamma-frailty likelihood at its estimate", {
  set.seed(8)
  n <- 30
  x <- matrix(rnorm(n))
  j <- sample(1:3, n, TRUE)
  ev <- rbinom(n, 1, 0.85)
  r <- Hrzphd(j, x, event = ev, cycles = 6, gs_iter = 20)
  th <- r$theta_hat
  A <- c(0, r$Lambda0)
  w <- exp(-x[, 1] * r$beta_hat)
  S <- function(a) (1 + th * a * w)^(-1 / th)
  ll <- sum(ifelse(ev == 1, log(S(A[j]) - S(A[j + 1])), log(S(A[4]))))
  expect_equal(r$loglik, ll, tolerance = 1e-9)
  expect_equal(r$Lambda0, cumsum(r$h_j_hat), tolerance = 1e-12)
  expect_equal(sum(r$cell_probs), 1, tolerance = 1e-12)
  w1 <- w[1]
  expect_equal(r$cell_probs[4], (1 + th * A[4] * w1)^(-1 / th), tolerance = 1e-12)
  expect_equal(morie_horowitz_ph_discrete_obs(j, x, event = ev, cycles = 6, gs_iter = 20)$loglik, r$loglik)
  expect_error(Hrzphd(j + 0.5, x), "integers")
})

test_that("Hrzphv's baseline jumps are the M-step of its final frailty weights", {
  set.seed(10)
  n <- 20
  x <- matrix(rnorm(n))
  t <- rexp(n, exp(-0.5 * x[, 1]))
  ev <- rbinom(n, 1, 0.8)
  r <- Hrzphv(t, x, event = ev, em_iter = 3, cycles = 2, gs_iter = 15)
  w <- exp(-x[, 1] * r$beta_hat)
  et <- order(t)[ev[order(t)] == 1]
  jm <- vapply(et, function(i) 1 / sum(r$frailty[t >= t[i]] * w[t >= t[i]]), 0)
  expect_equal(r$h0_hat, jm, tolerance = 1e-12)
  expect_equal(r$Lambda0, cumsum(jm), tolerance = 1e-12)
  expect_equal(r$event_times, t[et])
  fx <- Hrzphv(t, x, event = ev, theta = 0.7, em_iter = 2, cycles = 1, gs_iter = 10)
  expect_equal(fx$theta_hat, 0.7)
  expect_equal(morie_horowitz_ph_heterogeneity(t, x, event = ev, theta = 0.7, em_iter = 2, cycles = 1, gs_iter = 10)$beta_hat,
               fx$beta_hat)
  expect_error(Hrzphv(t, x, frailty_dist = "lognormal"), "gamma")
})

test_that("Hrzphvnp composes the ADE direction, the sigma estimator and Hrztf", {
  set.seed(12)
  n <- 40
  X <- cbind(rnorm(n), rnorm(n))
  t <- rexp(n, exp(X %*% c(1, 0.5)))
  r <- Hrzphvnp(t, X, ny = 7, nz = 7, nq = 7, bandwidth = 0.6)
  tf <- Hrztf(X, t, ny = 7, nz = 7, bandwidth = 0.6)
  expect_equal(r$T_hat, tf$T_hat, tolerance = 1e-12)
  fac <- n^(-0.22 * (1 - 0.85))
  expect_equal(r$sigma, (r$sigma_y1 - fac * r$sigma_y2) / (1 - fac), tolerance = 1e-12)
  expect_equal(r$beta_hat, r$sigma * r$alpha_hat, tolerance = 1e-12)
  expect_equal(r$Lambda0_hat, exp(r$sigma * r$T_hat), tolerance = 1e-12)
  dv <- diff(r$y_grid)[1]
  Tp <- c((r$T_hat[2] - r$T_hat[1]) / dv, (r$T_hat[3:7] - r$T_hat[1:5]) / (2 * dv), (r$T_hat[7] - r$T_hat[6]) / dv)
  expect_equal(r$h0_hat, r$sigma * Tp * exp(r$sigma * r$T_hat), tolerance = 1e-9)
  ade <- numeric(2)
  for (i in 1:n) for (l in 1:n) if (i != l) {
    us <- (X[i, ] - X[l, ]) / 0.6
    ade <- ade + (-2) * t[i] * (-us) * prod(gk(us)) / ((n - 1) * 0.6^3) / n
  }
  expect_equal(r$alpha_hat, ade / abs(ade[1]), tolerance = 1e-9)
  expect_equal(morie_horowitz_ph_frailty_nonpar(t, X, ny = 7, nz = 7, nq = 7, bandwidth = 0.6)$sigma, r$sigma)
  expect_error(Hrzphvnp(t, X, q = 0.3), "q must")
  expect_error(Hrzphvnp(-t, X), "strictly positive")
  expect_error(Hrzphvnp(t[1:8], X[1:8, ]), "at least 10")
})

test_that("Plrident checks positive definiteness of the partialled-out design", {
  set.seed(1)
  n <- 20
  Z <- rnorm(n)
  X <- cbind(rnorm(n) + Z, rnorm(n))
  r <- Plrident(X, Z, h = 0.5)
  W <- gk(outer(Z, Z, "-") / 0.5)
  Xt <- X - W %*% X / rowSums(W)
  ev <- sort(eigen(crossprod(Xt) / n, symmetric = TRUE)$values)
  expect_equal(r$eigvals, ev, tolerance = 1e-12)
  expect_true(r$identified)
  expect_equal(r$condnum, ev[2] / ev[1], tolerance = 1e-12)
  r1 <- Plrident(cbind(1, X), Z, h = 0.5)
  expect_true(r1$hasintercept)
  expect_false(r1$identified)
  expect_equal(morie_horowitz_plr_identification(X, Z, h = 0.5)$mineig, r$mineig)
})

test_that("Hrzplrq reports the kernel-weighted quantile criterion at its estimate", {
  set.seed(3)
  n <- 16
  z <- runif(n)
  X <- cbind(rnorm(n))
  y <- 2 * X[, 1] + sin(3 * z) + rnorm(n, sd = 0.3)
  r <- Hrzplrq(X, y, z, bandwidth = 0.3, tau = 0.4, niter = 4)
  W <- gk(outer(z, z, "-") / 0.3)
  wq <- function(v, w) {
    o <- order(v)
    cw <- cumsum(w[o]) / sum(w)
    v[o][which(cw >= 0.4 - 1e-12)[1]]
  }
  res <- y - X %*% r$beta_tau
  g <- vapply(1:n, function(i) wq(res, W[i, ]), 0)
  expect_equal(r$g_tau_hat, g, tolerance = 1e-12)
  crit <- sum((colSums(X * (0.4 - (res - g <= 0))) / n)^2)
  expect_equal(r$criterion, crit, tolerance = 1e-12)
  expect_equal(morie_horowitz_plr_quantile(X, y, z, bandwidth = 0.3, tau = 0.4, niter = 4)$beta_tau, r$beta_tau)
  expect_error(Hrzplrq(X, y, z, tau = 1), "tau")
})

test_that("Simgrate, Simbrate and Hrzseriu", {
  set.seed(5)
  X <- cbind(rnorm(20), rnorm(20))
  y <- as.numeric(X %*% c(1, 1)) + rnorm(20)
  r <- Simgrate(X, y, c(1, 1), grid = c(-1, 0, 1), h = 0.6)
  z <- X %*% c(1, 1)
  K <- gk(outer(c(-1, 0, 1), as.numeric(z), "-") / 0.6)
  expect_equal(r$ghat, as.numeric(K %*% y) / rowSums(K), tolerance = 1e-12)
  expect_equal(r$density, rowSums(K) / (20 * 0.6), tolerance = 1e-12)
  expect_equal(r$rate, 20^-0.4, tolerance = 1e-12)
  expect_equal(morie_horowitz_rate_G_estimation(X, y, c(1, 1), grid = 0, h = 0.6)$ghat, r$ghat[2], tolerance = 1e-12)

  e <- c(0.5, 0.21, 0.09, 0.052)
  ns <- c(50, 200, 1000, 3000)
  b <- Simbrate(e, ns)
  f <- lm(log(e) ~ log(ns))
  expect_equal(b$exponent, unname(coef(f)[2]), tolerance = 1e-12)
  expect_equal(b$se, unname(summary(f)$coefficients[2, 2]), tolerance = 1e-12)
  expect_equal(b$rsq, summary(f)$r.squared, tolerance = 1e-12)
  expect_equal(morie_horowitz_rate_beta_estimation(e, ns)$gap, b$exponent + 0.5, tolerance = 1e-12)
  expect_error(Simbrate(e[1:2], ns[1:2]), "three")

  set.seed(6)
  w <- rnorm(25)
  x <- w + rnorm(25, sd = 0.5)
  yy <- x^2 + rnorm(25, sd = 0.1)
  s <- Hrzseriu(x, yy, w, K = 3)
  Psi <- outer(u01(w), 0:2, "^")
  Phi <- outer(u01(x), 0:2, "^")
  m <- solve(crossprod(Psi) + diag(1e-10, 3), crossprod(Psi, yy))
  C <- solve(crossprod(Psi) + diag(1e-10, 3), crossprod(Psi, Phi))
  be <- solve(crossprod(C) + diag(1e-10, 3), crossprod(C, m))
  expect_equal(s$m_hat, as.numeric(m), tolerance = 1e-9)
  expect_equal(s$Q, C, tolerance = 1e-9)
  expect_equal(s$g_hat, as.numeric(Phi %*% be), tolerance = 1e-8)
  sc <- Hrzseriu(x, yy, w, K = 3, basis = "cos")
  expect_equal(sc$basis, "cos")
  expect_equal(morie_horowitz_series_unknown_T(x, yy, w, K = 3)$beta, s$beta)
  expect_error(Hrzseriu(x, yy, w, K = 30), "must not exceed")
})

test_that("Simident and Simidentd check the Theorem 2.1 conditions and the (2.13) bounds", {
  set.seed(7)
  X <- cbind(rnorm(30), rnorm(30))
  y <- (X %*% c(1, 2))^2
  r <- Simident(X, c(1, 2), y = y)
  expect_true(r$condd && r$condc && r$condb)
  z <- as.numeric(X %*% c(1, 2))
  cuts <- quantile(z, seq(0, 1, 0.1))
  bins <- findInterval(z, cuts, rightmost.closed = TRUE, all.inside = TRUE)
  mm <- tapply(y, bins, mean)
  expect_equal(r$gspread, max(mm) - min(mm), tolerance = 1e-12)
  expect_false(Simident(X, c(2, 1))$condd)
  Xd <- cbind(rnorm(30), sample(1:3, 30, TRUE))
  expect_false(Simident(Xd, c(1, 1))$condb)
  expect_equal(morie_horowitz_sim_identification(X, c(1, 2))$rank, 2L)

  xs <- rbind(c(0, 0), c(1, -1), c(0.5, 1), c(2, 0.5))
  gv <- c(0.1, 0.3, 0.6, 0.9)
  d <- Simidentd(xs, gv, blim = 10)
  o <- order(gv)
  lo <- -10
  hi <- 10
  for (k in 1:3) {
    a <- xs[o[k], 2] - xs[o[k + 1], 2]
    c0 <- xs[o[k], 1] - xs[o[k + 1], 1]
    if (a > 0) hi <- min(hi, -c0 / a)
    if (a < 0) lo <- max(lo, -c0 / a)
  }
  expect_equal(d$lower, lo, tolerance = 1e-9)
  expect_equal(d$upper, hi, tolerance = 1e-9)
  expect_equal(d$width, hi - lo, tolerance = 1e-9)
  expect_equal(morie_horowitz_sim_id_discrete_x(xs, gv, blim = 10)$upper, d$upper)
  expect_true(is.na(Simidentd(xs[1, , drop = FALSE], 1)$lower))
})
