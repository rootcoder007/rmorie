# Coverage for the Horvitz-Thompson variance, Huber weight/loss, HITS to
# convergence, Hardy-Weinberg, hypergeometric vs binomial, Horowitz-Manski
# bounds, the ICAR prior, the BLP calibration test, the functional
# high-throughput-phenotyping predictor and CV grid tuning.

test_that("Htvar1 is the Horvitz-Thompson variance", {
  y <- c(3, 5, 2, 8)
  p <- c(0.4, 0.5, 0.3, 0.8)
  P <- outer(p, p) * 0.95
  diag(P) <- p
  r <- Htvar1(y, p, P)
  v <- 0
  for (i in 1:4) for (j in 1:4) v <- v + (P[i, j] - p[i] * p[j]) * y[i] * y[j] / (p[i] * p[j] * P[i, j])
  expect_equal(r$variance, v, tolerance = 1e-12)
  expect_equal(r$total, sum(y / p), tolerance = 1e-12)
  expect_equal(r$se, sqrt(v), tolerance = 1e-12)
  expect_error(Htvar1(y, c(p[-1], 0), P), "\\(0, 1\\]")
  P0 <- P
  P0[1, 2] <- 0
  expect_error(Htvar1(y, p, P0), "not positive")
})

test_that("Huberw and Hubrho", {
  r <- c(-3, -1, 0.5, 1.345, 2.5)
  w <- Huberw(r)
  expect_equal(w$weights, pmin(1, 1.345 / abs(r)), tolerance = 1e-12)
  expect_equal(w$psi, pmax(-1.345, pmin(1.345, r)), tolerance = 1e-12)
  expect_equal(w$n_downweighted, 2L)
  h <- Hubrho(r, k = 1)
  rho <- ifelse(abs(r) <= 1, r^2 / 2, abs(r) - 0.5)
  expect_equal(h$loss, rho, tolerance = 1e-12)
  expect_equal(h$estimate, sum(rho), tolerance = 1e-12)
  expect_error(Huberw(1, k = 0), "positive")
  expect_error(Hubrho(numeric(0)), "empty")
})

test_that("Hubsa iterates HITS to the principal singular vectors", {
  A <- matrix(0, 4, 4)
  A[1, 2:3] <- 1
  A[2, 3] <- 1
  A[4, c(1, 3)] <- 1
  r <- Hubsa(rep(1, 4), A, tol = 1e-13)
  s <- svd(A)
  expect_true(r$converged)
  expect_equal(r$hubs, abs(s$u[, 1]), tolerance = 1e-9)
  expect_equal(r$authorities, abs(s$v[, 1]), tolerance = 1e-9)
  expect_equal(r$estimate, max(abs(s$u[, 1])), tolerance = 1e-9)
  expect_error(Hubsa(1:3, A), "different lengths")
  expect_error(Hubsa(rep(1, 4), A, tol = 0), "tol")
})

test_that("hwetst and morie_hardy_weinberg are the 1-df chi-square test", {
  g <- c(30, 50, 20)
  r <- hwetst(g)
  p <- (60 + 50) / 200
  e <- 100 * c(p^2, 2 * p * (1 - p), (1 - p)^2)
  x2 <- sum((g - e)^2 / e)
  expect_equal(r$statistic, x2, tolerance = 1e-12)
  expect_equal(r$p_value, pchisq(x2, 1, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(morie_hardy_weinberg(g)$expected, e, tolerance = 1e-12)
  expect_true(is.na(hwetst(c(0, 0, 0))$statistic))
})

test_that("HyperLim compares the hypergeometric and binomial pmfs", {
  r <- HyperLim(3, 10, 0.3, 50)
  expect_equal(r$hypergeometric, dhyper(3, 15, 35, 10), tolerance = 1e-12)
  expect_equal(r$binomial, dbinom(3, 10, 0.3), tolerance = 1e-12)
  expect_equal(r$abs_error, abs(dhyper(3, 15, 35, 10) - dbinom(3, 10, 0.3)), tolerance = 1e-12)
  expect_equal(HyperLim(9, 10, 0.1, 20)$hypergeometric, 0)
  expect_error(HyperLim(1, 30, 0.5, 20), "n <= N")
  expect_error(HyperLim(1, 3, 2, 20), "p in")
})

test_that("Hzbnds and morie_horowitz_manski_bounds", {
  y <- c(2, 5, NA, 3, NA, 4)
  R <- c(1, 1, 0, 1, 0, 1)
  yy <- ifelse(is.na(y), 0, y)
  r <- Hzbnds(yy, R, 0, 10)
  p <- 4 / 6
  m <- mean(c(2, 5, 3, 4))
  expect_equal(c(r$lower, r$upper), c(m * p, m * p + 10 * (1 - p)), tolerance = 1e-12)
  expect_equal(r$width, 10 * (1 - p), tolerance = 1e-12)
  expect_equal(morie_horowitz_manski_bounds(yy, R, 0, 10)$mar_estimate, m, tolerance = 1e-12)
  expect_error(Hzbnds(yy, rep(0, 6), 0, 1), "no observed")
  expect_error(Hzbnds(yy, R, 2, 1), "y_max")
  expect_error(Hzbnds(yy, c(R[-1], 2), 0, 1), "0 or 1")
})

test_that("Icarbm builds the ICAR precision and conditionals", {
  W <- matrix(c(0, 1, 1, 0,
                1, 0, 1, 0,
                1, 1, 0, 1,
                0, 0, 1, 0), 4, byrow = TRUE)
  u <- c(0.5, -0.2, 0.1, 0.9)
  r <- Icarbm(W, tau = 2, u = u)
  Q <- (diag(rowSums(W)) - W) / 4
  expect_equal(r$precision, Q, tolerance = 1e-12)
  expect_equal(r$conditional_mean, as.numeric(W %*% u) / rowSums(W), tolerance = 1e-12)
  expect_equal(r$conditional_var, 4 / rowSums(W), tolerance = 1e-12)
  expect_equal(r$pairwise_quadratic, sum(u * (Q %*% u)), tolerance = 1e-12)
  expect_equal(r$smallest_eigenvalue, 0, tolerance = 1e-9)
  expect_true(is.nan(Icarbm(W)$pairwise_quadratic))
  W2 <- W
  W2[1, 2] <- 2
  expect_error(Icarbm(W2), "symmetric")
  expect_error(Icarbm(W, tau = 0), "tau")
})

test_that("Htebias is the HC3 BLP calibration regression", {
  skip_if_not_installed("sandwich")
  set.seed(7)
  n <- 40
  D <- rbinom(n, 1, 0.5)
  tau <- runif(n, 0.5, 2)
  y <- 1 + tau * D + rnorm(n)
  r <- Htebias(y, D, tau)
  tgt <- y - mean(y)
  wr <- D - mean(D)
  X <- cbind(wr * mean(tau), wr * (tau - mean(tau)))
  f <- lm(tgt ~ 0 + X)
  V <- sandwich::vcovHC(f, type = "HC3")
  expect_equal(c(r$coef_mean, r$coef_differential), unname(coef(f)), tolerance = 1e-9)
  expect_equal(c(r$se_mean, r$se_differential), unname(sqrt(diag(V))), tolerance = 1e-9)
  t1 <- coef(f)[1] / sqrt(V[1, 1])
  expect_equal(r$p_mean, unname(pt(t1, n - 2, lower.tail = FALSE)), tolerance = 1e-9)
  expect_error(Htebias(y, D, rep(1, n)), "constant")
  expect_error(Htebias(y[1:3], D[1:3], tau[1:3]), "n >= 4")
})

test_that("Htpfn solves Henderson's equations on the functional design", {
  set.seed(5)
  n <- 8
  m <- 7
  M <- matrix(sample(0:2, n * 12, TRUE), n)
  tg <- seq(0, 1, length.out = m)
  Wf <- t(vapply(1:n, function(i) sin(2 * pi * tg) * rnorm(1) + cos(2 * pi * tg) * rnorm(1) + rnorm(m, sd = 0.1), numeric(m)))
  y <- rnorm(n, 2)
  r <- Htpfn(y, M, Wf, n_basis = 3, lam = 0.7, ridge = 0.1)
  Psi <- cbind(1, sqrt(2) * sin(2 * pi * tg), sqrt(2) * cos(2 * pi * tg))
  C <- t(solve(crossprod(Psi) + diag(1e-12, 3), t(Wf %*% Psi)))
  wq <- rep(1 / 6, m)
  wq[c(1, m)] <- 1 / 12
  Q <- t(Psi) %*% (wq * Psi)
  Xs <- cbind(1, C %*% Q)
  p <- colMeans(M) / 2
  Z <- sweep(M, 2, 2 * p)
  G <- Z %*% t(Z) / (2 * sum(p * (1 - p)))
  Gi <- solve(G + diag(0.1, n))
  MM <- rbind(cbind(crossprod(Xs), t(Xs)), cbind(Xs, 0.7 * Gi + diag(n)))
  sol <- solve(MM, c(crossprod(Xs, y), y))
  expect_equal(r$coefs, C, tolerance = 1e-9)
  expect_equal(r$Q, Q, tolerance = 1e-12)
  expect_equal(r$beta, sol[1:4], tolerance = 1e-8)
  expect_equal(r$g_hat, sol[5:12], tolerance = 1e-8)
  expect_equal(r$beta_func, as.numeric(Psi %*% sol[2:4]), tolerance = 1e-8)
  expect_equal(r$fitted, as.numeric(Xs %*% sol[1:4] + sol[5:12]), tolerance = 1e-8)
  expect_equal(r$bic, n * log(2 * pi * mean((y - r$fitted)^2)) + n + 4 * log(n), tolerance = 1e-8)
  cv <- 0
  for (i in 1:n) for (j in 1:m) {
    cj <- solve(crossprod(Psi[-j, ]) + diag(1e-12, 3), crossprod(Psi[-j, ], Wf[i, -j]))
    cv <- cv + (Wf[i, j] - sum(cj * Psi[j, ]))^2
  }
  expect_equal(r$cv1, cv, tolerance = 1e-9)
  expect_error(Htpfn(y, M, Wf, n_basis = 9), "n_basis")
  expect_error(Htpfn(y, M, Wf, lam = 0), "lam")
  expect_error(Htpfn(y[-1], M, Wf), "disagree")
})

test_that("Htprd scans the Cartesian grid with K-fold ridge CV", {
  set.seed(1)
  X <- matrix(rnorm(40), 20)
  y <- as.numeric(X %*% c(1, -0.5) + rnorm(20, sd = 0.3))
  cvr <- function(lam) {
    mean(vapply(0:3, function(f) {
      te <- which((0:19) %% 4 == f)
      b <- solve(crossprod(X[-te, ]) + diag(lam + 1e-12, 2), crossprod(X[-te, ], y[-te]))
      mean((y[te] - X[te, ] %*% b)^2)
    }, 0))
  }
  r <- Htprd(list(lam = c(0.01, 1, 10)), list(X, y), k = 4)
  sc <- vapply(c(0.01, 1, 10), cvr, 0)
  expect_equal(r$scores, sc, tolerance = 1e-9)
  expect_equal(r$best_params$lam, c(0.01, 1, 10)[which.min(sc)])
  g2 <- Htprd(list(a = 1:2, b = c("x", "y", "z")), list(X, y),
              fit_cv = function(X, y, K, p) p$a * match(p$b, c("z", "x", "y")), k = 2)
  expect_equal(g2$n, 6L)
  expect_equal(g2$best_params, list(a = 1L, b = "z"))
  expect_error(Htprd(list(), list(X, y)), "empty")
  expect_error(Htprd(list(lam = 1), list(X, y), k = 1), "k must")
})
