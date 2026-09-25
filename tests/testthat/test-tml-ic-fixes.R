## Independent recomputations for Tmlmrk, Tmlpse and morie_tmlcou.

test_that("Tmlmrk: stationary value and martingale IC standard error", {
  u <- 12345
  lcg <- function() { u <<- (16807 * u) %% 2147483647; u / 2147483647 }
  s <- 0; S <- A <- R <- numeric(0)
  for (i in 1:400) {
    a <- if (lcg() < 0.4 + 0.2 * s) 1 else 0
    r <- 0.5 * s + a - 0.3 * s * a + (lcg() - 0.5)
    S <- c(S, s); A <- c(A, a); R <- c(R, r)
    if (lcg() < 0.7) s <- (s + 1 + a) %% 3
  }
  pol <- c(1, 0, 1); n <- length(S); si <- S + 1
  on <- A == pol[si]
  b <- vapply(1:3, function(k) sum(si == k & on) / sum(si == k), 0)
  rr <- vapply(1:3, function(k) mean(R[si == k & on]), 0)
  P <- matrix(0, 3, 3)
  for (i in seq_len(n - 1)) if (on[i]) P[si[i], si[i + 1]] <- P[si[i], si[i + 1]] + 1
  P <- P / rowSums(P)
  d <- rep(1 / 3, 3)
  for (i in 1:5000) d <- as.numeric(d %*% P)
  V <- sum(d * rr)
  ## h = sum_t P^t (r - V): convergent for an aperiodic chain, d'h = 0
  h <- numeric(3); term <- rr - V
  for (i in 1:5000) { h <- h + term; term <- as.numeric(P %*% term) }
  s0 <- si[-n]
  db <- tabulate(s0, 3) / (n - 1)
  D <- d[s0] / db[s0] * on[-n] / b[s0] * (R[-n] + h[si[-1]] - h[s0] - V)
  out <- Tmlmrk(S, A, R, pol)
  expect_equal(out$estimate, V, tolerance = 1e-9)
  expect_equal(out$se, sqrt(sum(D^2)) / (n - 1), tolerance = 1e-8)
  expect_equal(out$eps, 0, tolerance = 1e-12)
})

test_that("Tmlpse: simulated plug-in and delta-method standard error", {
  n <- 60; k <- 0:(n - 1)
  X <- matrix(sin(k), ncol = 1)
  D <- ifelse(cos(3 * k) > 0, 1, 0)
  M1 <- 0.5 * D + 0.3 * X[, 1] + 0.1 * sin(5 * k)
  M2 <- 0.4 * D + 0.6 * M1 + 0.1 * cos(7 * k)
  y <- 1 + 0.2 * D + 0.7 * M1 + 0.5 * M2 + 0.3 * X[, 1] + 0.2 * sin(11 * k)
  Z1 <- cbind(D, 1, X); Z2 <- cbind(D, 1, X, M1); Zy <- cbind(D, 1, X, M1, M2)
  inf <- function(Z, t) {
    f <- lm.fit(Z, t)
    list(b = f$coefficients, i = (Z * f$residuals) %*% solve(crossprod(Z) / n))
  }
  f1 <- inf(Z1, M1); f2 <- inf(Z2, M2); fy <- inf(Zy, y)
  sz <- c(length(f1$b), length(f2$b))
  for (path in list(c(1, 1), c(0, 0), c(1, 0), c(0, 1))) {
    psi <- function(th) {
      c1 <- th[1:sz[1]]; c2 <- th[sz[1] + 1:sz[2]]; cy <- th[-(1:sum(sz))]
      ch <- function(a1, a2) {
        m1 <- cbind(a1, 1, X) %*% c1
        m2 <- cbind(a2, 1, X, m1) %*% c2
        cbind(m1, m2)
      }
      mean(cbind(1, 1, X, ch(path[1], path[2])) %*% cy - cbind(0, 1, X, ch(0, 0)) %*% cy)
    }
    th <- c(f1$b, f2$b, fy$b)
    ## psi is affine in each coefficient, so central differences are exact
    gr <- vapply(seq_along(th), function(j) {
      e <- replace(numeric(length(th)), j, 1e-4)
      (psi(th + e) - psi(th - e)) / 2e-4
    }, 0)
    ic <- as.numeric(cbind(f1$i, f2$i, fy$i) %*% gr)
    out <- Tmlpse(y, D, cbind(M1, M2), X, path)
    expect_equal(out$estimate, psi(th), tolerance = 1e-9)
    expect_equal(out$se, sqrt(var(ic) / n), tolerance = 1e-8)
  }
})

test_that("morie_tmlcou: supplied fits are rescaled with the outcome", {
  n <- 50; k <- 0:(n - 1); W <- sin(1.9 * k)
  ex <- function(x) 1 / (1 + exp(-x))
  A <- ifelse(((31 * k + 3) %% 71 + 0.5) / 71 < ex(0.4 * W), 1, 0)
  Y <- as.numeric((13 * k + 5) %% 7 + 2 * A + (W > 0))
  g <- ex(0.1 + 0.5 * W); Q1 <- 4.5 + 0.8 * W; Q0 <- 3 + 0.8 * W
  lo <- min(Y); rg <- max(Y) - lo
  ys <- (Y - lo) / rg; q1 <- (Q1 - lo) / rg; q0 <- (Q0 - lo) / rg
  H <- A / g - (1 - A) / (1 - g); qa <- ifelse(A == 1, q1, q0)
  e <- uniroot(function(e) sum(H * (ys - ex(qlogis(qa) + e * H))),
               c(-50, 50), tol = 1e-14)$root
  q1s <- ex(qlogis(q1) + e / g); q0s <- ex(qlogis(q0) - e / (1 - g))
  ps <- mean(q1s - q0s)
  dd <- (H * (ys - ifelse(A == 1, q1s, q0s)) + q1s - q0s - ps) * rg
  out <- morie_tmlcou(Y, A, W, g = g, Q1 = Q1, Q0 = Q0)
  expect_equal(out$estimate, ps * rg, tolerance = 1e-9)
  expect_equal(out$se, sqrt(sum((dd - mean(dd))^2)) / n, tolerance = 1e-8)
})
