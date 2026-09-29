# Coverage for Shfflm .. Sirtdy exports. Every expectation is recomputed in
# the test body.

test_that("Shfflm gives the shuffle-model amplification bounds", {
  r <- Shfflm(1, 10000, 1e-6)
  e1 <- 2 * exp(2) * (exp(1) - 1) / 10000
  gen <- e1 * sqrt(2 * 10000 * log(1e6)) + 10000 * e1 * (exp(e1) - 1)
  ref <- exp(2) * (exp(1) - 1) * sqrt(8 * log(1e6) / 10000) + 6 * exp(4) * (exp(1) - 1)^2 / 10000
  expect_equal(r$epsilon_general, gen, tolerance = 1e-12)
  expect_equal(r$epsilon_refined, ref, tolerance = 1e-12)
  expect_equal(r$epsilon_simple, 12 * sqrt(log(1e6) / 10000), tolerance = 1e-12)
  expect_equal(r$refined_valid, as.numeric(1 <= log(2500) / 3))
  expect_error(Shfflm(0, 10, 0.1), "positive")
  expect_error(Shfflm(1, 1, 0.1), "exceed 1")
  expect_error(Shfflm(1, 10, 1), "delta must lie")
})

test_that("Shfrm with fixed theta is a fixed point of the gamma-frailty EM", {
  skip_if_not_installed("survival")
  tt <- c(2, 5, 3, 8, 6, 1, 7, 4, 9, 3.5, 6.5, 2.5)
  ev <- c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0)
  x <- c(0.5, -0.3, 1.1, -0.8, 0.2, 0.9, -0.5, 0.4, 1.3, -1.0, 0.0, 0.7)
  cl <- rep(1:4, each = 3)
  r <- Shfrm(tt, ev, cbind(x), cl, theta = 0.5)
  w <- unlist(r$frailty)[as.character(cl)]
  f <- survival::coxph(survival::Surv(tt, ev) ~ x + offset(log(w)), ties = "breslow",
                       control = survival::coxph.control(eps = 1e-10, iter.max = 100))
  expect_equal(unname(r$estimate), unname(stats::coef(f)), tolerance = 1e-7)
  eta <- r$estimate * x
  et <- sort(unique(tt[ev == 1]))
  dL <- vapply(et, function(s) sum(tt == s & ev == 1) / sum((w * exp(eta))[tt >= s]), 0)
  H <- vapply(tt, function(s) sum(dL[et <= s]), 0) * exp(eta)
  D <- tapply(ev, cl, sum)
  L <- tapply(H, cl, sum)
  expect_equal(unname(unlist(r$frailty)), as.numeric((D + 2) / (L + 2)), tolerance = 1e-8)
  expect_equal(r$baseline_cumhaz, cumsum(dL), tolerance = 1e-8)
  expect_equal(r$marginal_survivor, (1 + 0.5 * cumsum(dL))^(-2), tolerance = 1e-8)
  expect_equal(r$kendall_tau, 0.2)
  expect_error(Shfrm(tt, ev, cbind(x), rep(1, 12), 0.5), "at least 2 clusters")
  expect_error(Shfrm(tt, ev, cbind(x), 1:12, 0.5), "unidentifiable")
})

test_that("Schoenres matches survival's raw and scaled Schoenfeld residuals", {
  skip_if_not_installed("survival")
  tt <- c(2, 5, 3, 8, 6, 1, 7, 4, 9, 3.5)
  ev <- c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1)
  X <- cbind(c(0.5, -0.3, 1.1, -0.8, 0.2, 0.9, -0.5, 0.4, 1.3, -1.0), c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0))
  f <- survival::coxph(survival::Surv(tt, ev) ~ X, ties = "breslow")
  b <- unname(stats::coef(f))
  r <- Schoenres(tt, ev, X, beta = b)
  raw <- stats::residuals(f, type = "schoenfeld")
  expect_equal(do.call(rbind, r$residuals), unname(raw), tolerance = 1e-8)
  expect_equal(r$event_times, sort(tt[ev == 1]))
  # scaled residuals r* = d V^-1 r + beta, V the observed information
  V <- solve(f$var)
  sc <- do.call(rbind, r$residuals) %*% solve(V) * sum(ev) + matrix(b, sum(ev), 2, byrow = TRUE)
  expect_equal(do.call(rbind, r$scaled), unname(sc), tolerance = 1e-6)
  expect_equal(r$rho, apply(sc, 2, function(z) stats::cor(r$event_times, z)), tolerance = 1e-6)
})

test_that("network SI, SIS and SIR mean-field epidemics match an RK4 solve", {
  skip_if_not_installed("deSolve")
  A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
  p0 <- c(0.1, 0, 0, 0)
  tg <- seq(0, 10, by = 0.1)
  si <- deSolve::ode(p0, tg, function(t, x, q) list(0.4 * (1 - x) * (A %*% x)), NULL, method = "rk4")
  r1 <- Siepid(A, 0.4, p0, t_max = 10, dt = 0.1)
  expect_equal(r1$prevalence, unname(si[101, -1]), tolerance = 1e-10)
  expect_equal(r1$half_time, tg[which(rowMeans(si[, -1]) >= 0.5)[1]])
  sis <- deSolve::ode(p0, tg, function(t, x, q) list(-0.3 * x + 0.4 * (1 - x) * (A %*% x)), NULL, method = "rk4")
  r2 <- Sietrt(A, 0.4, 0.3, p0, t_max = 10, dt = 0.1)
  expect_equal(r2$prevalence, unname(sis[101, -1]), tolerance = 1e-10)
  k <- rowSums(A)
  expect_equal(r2$lambda_c, mean(k) / mean(k^2), tolerance = 1e-12)
  sir <- deSolve::ode(c(1 - p0, p0, rep(0, 4)), tg, function(t, y, q) {
    s <- y[1:4]
    i <- y[5:8]
    f <- 0.4 * s * (A %*% i)
    list(c(-f, f - 0.3 * i, 0.3 * i))
  }, NULL, method = "rk4")
  r3 <- Siaepi(A, 0.4, 0.3, p0, t_max = 10, dt = 0.1)
  expect_equal(c(r3$S, r3$I, r3$R), unname(sir[101, -1]), tolerance = 1e-10)
  expect_lt(r3$conservation_error, 1e-12)
  expect_error(Siaepi(A, 0.4, 0.3, p0[-1]), "one entry per node")
  expect_error(Siepid(A, -1, p0), "non-negative")
  expect_error(Sietrt(A, 0.4, 0.3, p0 + 2), "\\[0, 1\\]")
})

test_that("age-structured SIR and its next-generation R0", {
  skip_if_not_installed("deSolve")
  C <- rbind(c(2, 0.5), c(0.8, 1.2))
  N <- c(600, 400)
  r <- Sirtdy(c(590, 400), c(10, 0), c(0, 0), C, gamma = 0.25, t_max = 20, dt = 0.25)
  o <- deSolve::ode(c(590, 400, 10, 0, 0, 0), seq(0, 20, by = 0.25), function(t, y, q) {
    s <- y[1:2]
    i <- y[3:4]
    f <- s * as.numeric(C %*% (i / N))
    list(c(-f, f - 0.25 * i, 0.25 * i))
  }, NULL, method = "rk4")
  expect_equal(c(r$S, r$I, r$R), unname(o[81, -1]), tolerance = 1e-10)
  K <- c(590, 400) * C / (0.25 * matrix(N, 2, 2, byrow = TRUE))
  expect_equal(r$R0, max(Mod(eigen(K)$values)), tolerance = 1e-10)
  expect_error(Sirtdy(1, 1, 1, C, 0.2), "m x m")
  expect_error(Sirtdy(c(1, 1), c(0, 0), c(0, 0), C, -1), "gamma must be")
})

test_that("Sieg is Siegel's repeated-median line", {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  y <- c(2.1, 3.9, 6.2, 8.1, 30, 12.0, 13.8)
  r <- Sieg(x, y)
  inner <- vapply(1:7, function(i) stats::median((y[-i] - y[i]) / (x[-i] - x[i])), 0)
  b <- stats::median(inner)
  expect_equal(r$slope, b, tolerance = 1e-12)
  expect_equal(r$intercept, stats::median(y - b * x), tolerance = 1e-12)
  expect_equal(r$breakdown_point, 3 / 7)
  expect_error(Sieg(c(1, 1), c(1, 2)), "every x is identical")
  expect_error(Sieg(1, 1), "at least two")
})

test_that("Sigbasis, Siglip, single-step H and SSA", {
  X <- cbind(c(0.2, -1.1, 0.7, 1.5, -0.3, 0.9, -0.8, 0.1), c(1.0, 0.3, -0.6, 0.2, 1.4, -1.2, 0.5, -0.4))
  B <- matrix(c(1.5, -0.7, -0.4, 2.0), 2)
  H <- stats::plogis(X %*% B)
  y <- c(1.1, 0.2, -0.3, 0.8, 1.9, -1.0, 0.4, 0.0)
  sb <- Sigbasis(X, B, y = y)
  expect_equal(sb$h, H, tolerance = 1e-12)
  # normal equations versus QR differ by cond(H)^2 eps
  expect_equal(sb$theta, qr.solve(H, y), tolerance = 1e-9)
  expect_error(Sigbasis(X, B[1, , drop = FALSE]), "one row per column")
  Ie <- rbind(c(1, 0.2), c(-0.3, 1), c(0.5, 0.5))
  Te <- rbind(c(0.9, 0.1), c(-0.2, 1.1), c(1, -0.4))
  u <- function(M) M / sqrt(rowSums(M^2))
  lg <- 2 * u(Ie) %*% t(u(Te)) - 1
  Z <- 2 * diag(3) - 1
  sl <- Siglip(Ie, Te, t_prime = 2, bias = -1)
  expect_equal(sl$estimate, -sum(log(stats::plogis(Z * lg))) / 3, tolerance = 1e-12)
  expect_equal(sl$acc, mean(apply(lg, 1, which.max) == 1:3))
  A <- rbind(c(1, 0.5, 0.25, 0.5), c(0.5, 1, 0.5, 0.25), c(0.25, 0.5, 1, 0.125), c(0.5, 0.25, 0.125, 1))
  G <- rbind(c(1.05, 0.4), c(0.4, 0.95))
  s <- Singgw(A, G, c(2, 4), w = 0.1)
  g <- c(2, 4)
  o <- c(1, 3)
  Gw <- 0.9 * G + 0.1 * A[g, g]
  Bm <- solve(A[g, g]) %*% A[g, o]
  Hm <- matrix(0, 4, 4)
  Hm[g, g] <- Gw
  Hm[g, o] <- Gw %*% Bm
  Hm[o, g] <- t(Bm) %*% Gw
  Hm[o, o] <- A[o, o] + t(Bm) %*% (Gw - A[g, g]) %*% Bm
  expect_equal(s$estimate, Hm, tolerance = 1e-12)
  expect_equal(s$Hinv, solve(Hm), tolerance = 1e-9)
  expect_error(Singgw(A, G, c(2, 2)), "unique")
  expect_error(Singgw(A, G, c(2, 4), w = 1), "w must be")
  z <- c(1.2, 0.4, -0.3, 0.8, 1.9, 0.2, -0.5, 1.1, 0.7, -0.2)
  sa <- Singsd(z, window = 3)
  zc <- z - mean(z)
  cv <- vapply(0:2, function(j) sum(zc[1:(10 - j)] * zc[(1 + j):10]) / (10 - j), 0)
  Cm <- stats::toeplitz(cv)
  e <- eigen(Cm, symmetric = TRUE)
  expect_equal(sa$eigenvalues, e$values, tolerance = 1e-10)
  Xt <- t(sapply(1:8, function(t) zc[t:(t + 2)]))
  E1 <- e$vectors[, 1]
  R1m <- (Xt %*% E1) %*% t(E1)
  rec <- vapply(1:10, function(t) mean(R1m[cbind(t - (max(1, t - 7):min(3, t)) + 1, max(1, t - 7):min(3, t))]), 0)
  expect_equal(sa$reconstructed, rec, tolerance = 1e-10)
  expect_lt(sa$reconstruction_error, 1e-10)
  expect_error(Singsd(z, window = 10), "window must satisfy")
})

test_that("Sirstn replays the Gillespie direct method", {
  r <- Sirstn(20, 2, beta = 0.6, gamma = 0.3, T = 50, seed = 4)
  s <- 4
  u <- function() {
    s <<- (48271 * s) %% 2147483647
    s / 2147483647
  }
  S <- 20
  I <- 2
  R <- 0
  t <- 0
  ev <- 0
  repeat {
    a1 <- 0.6 * S * I / 22
    a2 <- 0.3 * I
    if (a1 + a2 <= 0) break
    tau <- -log(u()) / (a1 + a2)
    if (t + tau > 50) break
    t <- t + tau
    if (u() * (a1 + a2) < a1) {
      S <- S - 1
      I <- I + 1
    } else {
      I <- I - 1
      R <- R + 1
    }
    ev <- ev + 1
  }
  expect_equal(c(r$S, r$I, r$R), c(S, I, R))
  expect_equal(r$n_events, ev)
  expect_equal(r$R0, 2)
  expect_error(Sirstn(-1, 1, 1, 1, 1), "non-negative")
})
