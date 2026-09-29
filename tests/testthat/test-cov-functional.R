# Coverage for the functional-data files (fanmd.R, fanva.R, fclust.R,
# fdwarp.R, fpca.R, freg.R, flmgcr.R, fnlm.R, fours.R, fhar.R, fnDist.R,
# Basisexp.R, basisR.R, facov.R): quantities are recomputed with base-R
# linear algebra, trapezoid weights and explicit dynamic programming.

fd_t <- seq(0, 1, length.out = 9)
fd_Y <- rbind(sin(2 * pi * fd_t), sin(2 * pi * fd_t) + 0.2, cos(2 * pi * fd_t),
              cos(2 * pi * fd_t) - 0.1, fd_t^2, fd_t^2 + 0.3)
trapw <- function(t) {
  h <- diff(t)
  c(h, 0) / 2 + c(0, h) / 2
}

test_that("Fanova is the Sobol decomposition on a midpoint grid", {
  f <- function(x) x[1] + x[2]^2 + 2 * x[1] * x[2]
  r <- Fanova(f, d = 2, grid = 4)
  p <- (0:3 + 0.5) / 4
  V <- outer(p, p, function(a, b) a + b^2 + 2 * a * b)
  f0 <- mean(V)
  m1 <- rowMeans(V) - f0
  m2 <- colMeans(V) - f0
  int <- V - outer(m1, m2, "+") - f0
  expect_equal(r$f0, f0, tolerance = 1e-12)
  expect_equal(r$D, mean((V - f0)^2), tolerance = 1e-12)
  expect_equal(r$D_main, c(mean(m1^2), mean(m2^2)), tolerance = 1e-12)
  expect_equal(r$D_int, mean(int^2), tolerance = 1e-12)
  expect_equal(r$closure, 1, tolerance = 1e-12)
  u <- Fanova(function(x) sum(x), input_dist = list(qnorm, qexp), d = 2, grid = 3)
  q <- (0:2 + 0.5) / 3
  expect_equal(u$f0, mean(outer(qnorm(q), qexp(q), "+")), tolerance = 1e-12)
})

test_that("Fanva is the pointwise and integrated functional ANOVA", {
  g <- c(1, 1, 2, 2, 3, 3)
  r <- Fanva(fd_Y, g)
  Fp <- vapply(1:9, function(j) {
    a <- anova(lm(fd_Y[, j] ~ factor(g)))
    a$`F value`[1]
  }, 0)
  ok <- is.finite(Fp)
  expect_equal(r$F[ok], Fp[ok], tolerance = 1e-9)
  w <- trapw(fd_t)
  expect_equal(r$ssb_int, sum(w * r$ssb), tolerance = 1e-12)
  expect_equal(r$estimate, (sum(w * r$ssb) / 2) / (sum(w * r$ssw) / 3), tolerance = 1e-12)
  one <- Fanva(fd_Y[, 3, drop = FALSE], g)
  expect_equal(one$estimate, Fp[3], tolerance = 1e-9)
  expect_error(Fanva(fd_Y[0, ], numeric(0)), "empty")
  expect_error(Fanva(fd_Y, g[-1]), "one label per curve")
  expect_error(Fanva(fd_Y, rep(1, 6)), "two groups")
  expect_error(Fanva(fd_Y[1:3, ], 1:3), "more curves than groups")
  expect_error(Fanva(fd_Y, g, t = 1:3), "must match")
})

test_that("Fdaclust runs k-means on basis coefficients", {
  B <- cbind(1, fd_t, fd_t^2)
  r <- Fdaclust(fd_Y, K = 2, basis = B)
  cf <- t(apply(fd_Y, 1, function(y) qr.solve(B, y)))
  expect_equal(r$coef, unname(cf), tolerance = 1e-10)
  ctr <- r$centers
  expect_equal(r$labels, apply(cf, 1, function(v) which.min(colSums((t(ctr) - v)^2))) - 1L)
  expect_equal(r$wss, sum((cf - ctr[r$labels + 1, ])^2), tolerance = 1e-10)
  raw <- Fdaclust(fd_Y, K = 3)
  expect_equal(raw$coef, fd_Y)
  expect_error(Fdaclust(fd_Y, K = 7), "at least K")
})

test_that("Fdwarp is dynamic time warping with an optional band", {
  a <- c(0, 1, 2, 3, 2, 1)
  b <- c(0, 0, 1, 2, 3, 3, 2)
  r <- Fdwarp(a, b)
  G <- matrix(Inf, 7, 8)
  G[1, 1] <- 0
  for (i in 1:6) for (j in 1:7) G[i + 1, j + 1] <- abs(a[i] - b[j]) + min(G[i, j + 1], G[i + 1, j], G[i, j])
  expect_equal(r$distance, G[7, 8], tolerance = 1e-12)
  expect_equal(r$normalized, G[7, 8] / r$path_length, tolerance = 1e-12)
  expect_equal(sum(abs(a[r$path[, 1] + 1] - b[r$path[, 2] + 1])), r$distance, tolerance = 1e-12)
  s <- Fdwarp(a, b, cost = "sq", window = 1)
  Gs <- matrix(Inf, 7, 8)
  Gs[1, 1] <- 0
  for (i in 1:6) for (j in 1:7) if (abs(i - j) <= 1) {
    Gs[i + 1, j + 1] <- (a[i] - b[j])^2 + min(Gs[i, j + 1], Gs[i + 1, j], Gs[i, j])
  }
  expect_equal(s$distance, Gs[7, 8], tolerance = 1e-12)
  expect_error(Fdwarp(numeric(0), b), "non-empty")
  expect_error(Fdwarp(a, b, cost = "x"), "abs or sq")
  expect_error(Fdwarp(a, b, window = -1), "non-negative")
})

test_that("Fpca diagonalises the trapezoid-weighted covariance", {
  r <- Fpca(fd_Y, 2)
  w <- trapw(fd_t)
  C <- sweep(fd_Y, 2, colMeans(fd_Y))
  V <- (crossprod(C) / 5) * sqrt(outer(w, w))
  e <- eigen(V, symmetric = TRUE)
  expect_equal(r$eigenvalues, e$values[1:2], tolerance = 1e-9)
  phi1 <- e$vectors[, 1] / sqrt(w)
  phi1 <- phi1 / sqrt(sum(w * phi1^2))
  expect_equal(abs(r$eigenfuncs[1, ]), abs(phi1), tolerance = 1e-8)
  expect_equal(abs(r$scores[, 1]), abs(as.numeric(C %*% (w * phi1))), tolerance = 1e-8)
  expect_equal(r$estimate, sum(e$values[1:2]) / sum(pmax(e$values, 0)), tolerance = 1e-9)
  expect_error(Fpca(fd_Y[1, , drop = FALSE], 1), "two curves")
  expect_error(Fpca(fd_Y[, 1, drop = FALSE], 1), "two grid points")
  expect_error(Fpca(fd_Y, 6), "between 1")
  expect_error(Fpca(fd_Y, 1, a = 1, b = 0), "positive width")
})

test_that("Freg finds the least-squares shift with parabolic refinement", {
  a <- sin(2 * pi * (0:19) / 20)
  b <- sin(2 * pi * ((0:19) - 3) / 20)
  r <- Freg(a, b, max_lag = 5)
  prof <- vapply(-5:5, function(d) {
    i <- max(0, -d):(min(20, 20 - d) - 1)
    mean((a[i + 1] - b[i + d + 1])^2)
  }, 0)
  expect_equal(r$profile, prof, tolerance = 1e-12)
  k <- which.min(prof)
  ref <- 0.5 * (prof[k - 1] - prof[k + 1]) / (prof[k - 1] - 2 * prof[k] + prof[k + 1])
  expect_equal(r$shift, (-5:5)[k])
  expect_equal(r$estimate, (-5:5)[k] + ref, tolerance = 1e-12)
  expect_error(Freg(numeric(0), numeric(0)), "empty")
  expect_error(Freg(a, b[-1]), "same length")
  expect_error(Freg(1:2, 1:2), "three sampling points")
  expect_error(Freg(a, b, max_lag = 0), "at least 1")
})

test_that("Fnlm fits the function-on-function tensor model", {
  X <- fd_Y
  Yf <- 0.5 * fd_Y[, 9:1] + 0.1
  Th <- cbind(1, fd_t)
  Et <- cbind(1, fd_t, fd_t^2)
  r <- Fnlm(X, Yf, Th, Et)
  w <- trapw(fd_t)
  Z <- X %*% (w * Th)
  M <- Yf %*% (w * Et)
  J <- crossprod(Et, w * Et)
  B <- t(solve(J, t(solve(crossprod(Z), crossprod(Z, M)))))
  expect_equal(r$Z, unname(Z), tolerance = 1e-12)
  expect_equal(r$B, unname(B), tolerance = 1e-9)
  expect_equal(r$fitted, unname(Z %*% B %*% t(Et)), tolerance = 1e-9)
  expect_equal(r$r2, 1 - sum((Yf - Z %*% B %*% t(Et))^2 %*% w) / sum(Yf^2 %*% w), tolerance = 1e-9)
  expect_error(Fnlm(X[0, ], Yf[0, ], Th, Et), "empty")
  expect_error(Fnlm(X, Yf[-1, ], Th, Et), "same number of curves")
  expect_error(Fnlm(X[, 1, drop = FALSE], Yf, Th[1, , drop = FALSE], Et), "two points")
  expect_error(Fnlm(X, Yf, Th[-1, ], Et), "one row per s point")
  expect_error(Fnlm(X, Yf, Th, Et[-1, ]), "one row per t point")
  expect_error(Fnlm(X, Yf, Th[, 0], Et), "no columns")
  expect_error(Fnlm(X[1, , drop = FALSE], Yf[1, , drop = FALSE], Th, Et), "at least K1")
  expect_error(Fnlm(X, Yf, Th, Et, s = 1:2), "columns of X")
  expect_error(Fnlm(X, Yf, Th, Et, t = 1:2), "columns of Y")
})

test_that("Fourier bases: Fours (orthonormal) and Fhar (unscaled)", {
  tt <- seq(0, 2, length.out = 7)
  f <- Fours(tt, 2)
  expect_equal(f$F[, 1], rep(1 / sqrt(2), 7), tolerance = 1e-12)
  expect_equal(f$F[, 4], sin(2 * pi * tt), tolerance = 1e-12)
  expect_equal(f$F[, 5], cos(2 * pi * tt), tolerance = 1e-12)
  expect_equal(f$F[, 2], sin(pi * tt), tolerance = 1e-12)
  fine <- seq(0, 2, length.out = 2001)
  Ff <- Fours(fine, 2)$F
  G <- crossprod(Ff * trapw(fine), Ff)
  # a 2001-point trapezoid rule integrates the products to about 1e-6
  expect_equal(G, diag(5), tolerance = 1e-5)
  h <- Fhar(tt, 2, period = 4)
  expect_equal(h$Phi, cbind(1, sin(pi / 2 * tt), cos(pi / 2 * tt), sin(pi * tt), cos(pi * tt)),
               tolerance = 1e-12)
  expect_equal(h$nbasis, 5L)
  expect_error(Fours(numeric(0), 1), "empty")
  expect_error(Fours(tt, -1), "non-negative")
  expect_error(Fours(tt, 1, period = 0), "positive")
  expect_error(Fhar(numeric(0), 1), "empty")
  expect_error(Fhar(tt, -1), "non-negative")
  expect_error(Fhar(c(1, 1), 1), "positive")
})

test_that("FnDist integrates the L1 and L2 distances", {
  f <- sin(2 * pi * fd_t)
  g <- cos(2 * pi * fd_t)
  r <- FnDist(f, g)
  w <- trapw(fd_t)
  expect_equal(r$l2sq, sum(w * (f - g)^2), tolerance = 1e-12)
  expect_equal(r$estimate, sqrt(sum(w * (f - g)^2)), tolerance = 1e-12)
  expect_equal(r$l1, sum(w * abs(f - g)), tolerance = 1e-12)
  expect_equal(r$sup, max(abs(f - g)))
  expect_equal(FnDist(f, f)$estimate, 0)
  expect_error(FnDist(numeric(0), numeric(0)), "empty")
  expect_error(FnDist(f, g[-1]), "same length")
  expect_error(FnDist(1, 1), "two sampling points")
  expect_error(FnDist(f, g, t = 1:3), "must match")
})

test_that("Basisexp and BasisR are least-squares basis expansions", {
  x <- seq(-1, 2, length.out = 12)
  y <- sin(x) + 0.3 * x^2 + 0.05 * cos(5 * x)
  for (kind in c("poly", "trig")) {
    r <- Basisexp(x, y, kind = kind, M = 2)
    H <- if (kind == "poly") cbind(1, x, x^2) else cbind(1, cos(x), sin(x), cos(2 * x), sin(2 * x))
    f <- lm.fit(H, y)
    expect_equal(r$theta, unname(f$coefficients), tolerance = 1e-9)
    expect_equal(r$r2, 1 - sum(f$residuals^2) / sum((y - mean(y))^2), tolerance = 1e-9)
  }
  sp <- Basisexp(x, y, kind = "spline", knots = c(0, 1))
  Hs <- cbind(1, x, pmax(x, 0), pmax(x - 1, 0))
  expect_equal(sp$theta, unname(lm.fit(Hs, y)$coefficients), tolerance = 1e-9)
  expect_error(Basisexp(numeric(0), numeric(0)), "empty")
  expect_error(Basisexp(x, y[-1]), "same length")
  expect_error(Basisexp(x, y, M = -1), "non-negative")
  expect_error(Basisexp(x, y, kind = "spline"), "needs knots")
  expect_error(Basisexp(x, y, kind = "x"), "kind must be")
  expect_error(Basisexp(x[1:2], y[1:2], M = 3), "fewer observations")
  P <- cbind(1, x, x^3)
  b <- BasisR(y, P)
  expect_equal(b$coef, unname(lm.fit(P, y)$coefficients), tolerance = 1e-9)
  expect_equal(b$sse, sum(lm.fit(P, y)$residuals^2), tolerance = 1e-9)
  expect_equal(b$df, 9L)
  expect_error(BasisR(numeric(0), P), "empty")
  expect_error(BasisR(y, P[-1, ]), "one row per observation")
  expect_error(BasisR(y, P[, 0]), "no columns")
})

test_that("Facov builds Lambda Lambda' + Psi", {
  L <- rbind(c(0.8, 0), c(0.5, 0.3), c(0.2, 0.9), c(0.6, -0.4))
  r <- Facov(4, 2, loadings = L, psi = c(0.2, 0.3, 0.1, 0.4))
  expect_equal(r$Sigma, tcrossprod(L) + diag(c(0.2, 0.3, 0.1, 0.4)), tolerance = 1e-12)
  expect_equal(r$n_params, 4 * 2 - 1 + 4)
  d <- Facov(3, 1)
  vdc <- function(i, b) {
    f <- 1
    s <- 0
    k <- i + 1
    while (k > 0) {
      f <- f / b
      s <- s + f * (k %% b)
      k <- k %/% b
    }
    s
  }
  Ld <- matrix(vapply(0:2, vdc, 0, b = 2) + 0.5, 3, 1)
  expect_equal(d$Sigma, tcrossprod(Ld) + diag(3), tolerance = 1e-12)
  expect_equal(Facov(3, 0)$Sigma, diag(3))
  expect_error(Facov(0, 0), "at least 1")
  expect_error(Facov(3, 4), "between 0 and n_env")
  expect_error(Facov(4, 2, loadings = L[-1, ]), "n_env by n_factors")
  expect_error(Facov(4, 2, loadings = L, psi = 1:3), "n_env entries")
  expect_error(Facov(4, 2, loadings = L, psi = c(-1, 1, 1, 1)), "non-negative")
})

test_that("Flmgcr adds tanh-gated cross-attention to the residual stream", {
  h <- rbind(c(0.2, -0.1, 0.5), c(0.4, 0.3, -0.2))
  v <- rbind(c(0.1, 0.7, -0.3), c(-0.5, 0.2, 0.4), c(0.3, 0.3, 0.3))
  r <- Flmgcr(h, v, gate = 0.8)
  s <- h %*% t(v) / sqrt(3)
  a <- exp(s - apply(s, 1, max))
  a <- a / rowSums(a)
  expect_equal(as.matrix(r$h_new), h + tanh(0.8) * a %*% v, tolerance = 1e-12)
  expect_equal(r$estimate, mean(h + tanh(0.8) * a %*% v), tolerance = 1e-12)
  expect_equal(as.matrix(Flmgcr(h, v, gate = 0)$h_new), h)
  expect_error(Flmgcr(matrix(numeric(0), 1, 0), v, 1), "no columns")
})
