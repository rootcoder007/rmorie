# Coverage for eqmm.R, eqms.R, erstst.R, esatic.R, esumtv.R, emaxr.R,
# ekfF.R, empbnp.R and ferror.R: IRT linking constants, the DF-GLS
# statistic (lm and urca), EAP scoring on its trapezoid grid, effective
# resistance via the Laplacian pseudo-inverse, one random-effects EM
# step against the explicit GLS/BLUP formulas, the extended Kalman
# filter, empirical Bayes and renewal forecasting.

test_that("Eqmm and Eqms are mean-mean and mean-sigma linking", {
  aR <- c(1.2, 0.8, 1.5)
  bR <- c(-0.5, 0.3, 1.1)
  aF <- c(1.0, 0.9, 1.3)
  bF <- c(-0.8, 0.1, 0.9)
  m <- Eqmm(c(0, 1), aR, bR, aF, bF)
  A <- mean(aF) / mean(aR)
  expect_equal(c(m$A, m$B), c(A, mean(bR) - A * mean(bF)), tolerance = 1e-12)
  expect_equal(m$equated, A * c(0, 1) + mean(bR) - A * mean(bF), tolerance = 1e-12)
  expect_error(Eqmm(0, numeric(0), numeric(0), numeric(0), numeric(0)), "no common items")
  expect_error(Eqmm(0, aR, bR[-1], aF, bF), "same length")
  expect_error(Eqmm(0, -aR, bR, aF, bF), "positive")
  s <- Eqms(2, bR, bF)
  expect_equal(s$A, sd(bR) / sd(bF), tolerance = 1e-12)
  expect_equal(s$B, mean(bR) - s$A * mean(bF), tolerance = 1e-12)
  pop <- function(x) sqrt(mean((x - mean(x))^2))
  expect_equal(Eqms(2, bR, bF, ddof = 0)$A, pop(bR) / pop(bF), tolerance = 1e-12)
  expect_error(Eqms(0, 1, 1), "at least two")
  expect_error(Eqms(0, bR, bF[-1]), "same length")
  expect_error(Eqms(0, bR, bF, ddof = 2), "0 or 1")
  expect_error(Eqms(0, bR, c(1, 1, 1)), "zero spread")
})

test_that("Erstst is the Elliott-Rothenberg-Stock DF-GLS statistic", {
  y <- cumsum(c(0.5, -0.3, 0.8, 0.1, -0.6, 0.4, 0.9, -0.2, 0.3, -0.7, 0.6, 0.2, -0.4, 0.5,
                0.1, -0.3, 0.7, -0.1, 0.2, 0.4))
  n <- 20
  ref <- function(trend, p) {
    ab <- 1 + (if (trend) -13.5 else -7) / n
    Z <- if (trend) cbind(1, 1:n) else matrix(1, n, 1)
    yt <- c(y[1], y[-1] - ab * y[-n])
    Zt <- rbind(Z[1, ], Z[-1, , drop = FALSE] - ab * Z[-n, , drop = FALSE])
    yd <- as.numeric(y - Z %*% coef(lm(yt ~ Zt - 1)))
    dy <- diff(yd)
    idx <- (p + 1):(n - 1)
    lagm <- if (p > 0) sapply(1:p, function(j) dy[idx - j]) else NULL
    f <- lm(dy[idx] ~ 0 + cbind(yd[idx], lagm))
    unname(coef(summary(f))[1, 3])
  }
  expect_equal(Erstst(y, lags = 1)$statistic, ref(FALSE, 1), tolerance = 1e-9)
  expect_equal(Erstst(y, lags = 0)$statistic, ref(FALSE, 0), tolerance = 1e-9)
  expect_equal(Erstst(y, lags = 2, trend = TRUE)$statistic, ref(TRUE, 2), tolerance = 1e-9)
  expect_error(Erstst(y, lags = -1), "non-negative")
  expect_error(Erstst(1:3, lags = 1), "too short")
  skip_if_not_installed("urca")
  u <- urca::ur.ers(y, type = "DF-GLS", model = "constant", lag.max = 1)
  expect_equal(Erstst(y, lags = 1)$statistic, u@teststat[1], tolerance = 1e-9, ignore_attr = TRUE)
})

test_that("Eapinfo integrates the 4PL posterior on its grid", {
  it <- rbind(c(1.2, -0.5, 0.1, 0.95), c(0.8, 0.3, 0.2, 1), c(1.5, 1.0, 0, 0.9))
  x <- c(1, 0, 1)
  lik <- function(th) {
    L <- plogis(1.7 * sweep(outer(th, it[, 2], "-"), 2, it[, 1], "*"))
    P <- sweep(sweep(L, 2, it[, 4] - it[, 3], "*"), 2, it[, 3], "+")
    apply(ifelse(matrix(x, length(th), 3, byrow = TRUE) == 1, P, 1 - P), 1, prod)
  }
  r <- Eapinfo(it, x, D = 1.7, prior_mean = 0.2, prior_sd = 1.5, nqp = 41)
  g <- seq(-4, 4, length.out = 41)
  w <- c(0.5, rep(1, 39), 0.5)
  po <- lik(g) * exp(-0.5 * ((g - 0.2) / 1.5)^2)
  eap <- sum(w * g * po) / sum(w * po)
  expect_equal(r$estimate, eap, tolerance = 1e-12)
  expect_equal(r$se, sqrt(sum(w * g^2 * po) / sum(w * po) - eap^2), tolerance = 1e-12)
  e <- exp(1.7 * it[, 1] * (eap - it[, 2]))
  p <- it[, 3] + (it[, 4] - it[, 3]) * e / (1 + e)
  info <- 1.7^2 * it[, 1]^2 * (p - it[, 3])^2 * (it[, 4] - p)^2 / ((it[, 4] - it[, 3])^2 * p * (1 - p))
  expect_equal(r$item_information, info, tolerance = 1e-12)
  expect_equal(r$se_ml, 1 / sqrt(sum(info)), tolerance = 1e-12)
  fine <- Eapinfo(it, x, D = 1.7, prior_mean = 0.2, prior_sd = 1.5, lower = -9, upper = 9, nqp = 2001)
  num <- integrate(function(t) t * lik(t) * dnorm(t, 0.2, 1.5), -Inf, Inf, rel.tol = 1e-10)$value
  den <- integrate(function(t) lik(t) * dnorm(t, 0.2, 1.5), -Inf, Inf, rel.tol = 1e-10)$value
  # trapezoid rule with h = 0.009 on a smooth integrand: error near 1e-7
  expect_equal(fine$estimate, num / den, tolerance = 1e-6)
  expect_error(Eapinfo(it[0, , drop = FALSE], numeric(0)), "at least one item")
  expect_error(Eapinfo(it[, 1:3], x), "\\(a, b, c, d\\)")
  expect_error(Eapinfo(it, x[-1]), "one response per item")
  expect_error(Eapinfo(it, c(1, 2, 0)), "0 or 1")
  expect_error(Eapinfo(it, x, prior_sd = 0), "prior sd")
  expect_error(Eapinfo(it, x, nqp = 2), "three quadrature")
})

test_that("Esumtv is the effective resistance", {
  G <- rbind(c(0, 1, 2, 0), c(1, 0, 1, 1), c(2, 1, 0, 0), c(0, 1, 0, 0))
  L <- diag(rowSums(G)) - G
  Lp <- MASS::ginv(L)
  e <- c(1, 0, 0, -1)
  r <- Esumtv(G, 0, 3)
  expect_equal(r$resistance, sum(e * (Lp %*% e)), tolerance = 1e-12)
  expect_equal(c(r$degree_u, r$degree_v), c(3, 1))
  expect_equal(Esumtv(G, 1, 1)$resistance, 0)
  # series 1 + 1 ohm (conductance 1) gives 2
  expect_equal(Esumtv(rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)), 0, 2)$resistance, 2, tolerance = 1e-12)
  expect_error(Esumtv(matrix(0, 0, 0), 0, 0), "no nodes")
  expect_error(Esumtv(G[, 1:3], 0, 1), "square")
  expect_error(Esumtv(-G, 0, 1), "non-negative")
  expect_error(Esumtv(upper.tri(G) * G, 0, 1), "symmetric")
  expect_error(Esumtv(G, 0, 4), "valid node")
  expect_error(Esumtv(diag(0, 3), 0, 1), "not connected")
})

test_that("Emaxr takes one EM step from the GLS and BLUP formulas", {
  y <- c(2.1, 2.5, 1.9, 3.4, 3.8, 3.1, 3.6, 1.2, 1.5)
  X <- cbind(1, c(0.5, 1, 0, 1.5, 2, 0.8, 1.1, -0.5, 0))
  cl <- c("a", "a", "a", "b", "b", "b", "b", "c", "c")
  r <- Emaxr(y, X, cl, sigma2_u = 0.4, sigma2_e = 0.3)
  V <- 0.3 * diag(9) + 0.4 * outer(cl, cl, "==")
  Vi <- solve(V)
  bet <- as.numeric(solve(t(X) %*% Vi %*% X, t(X) %*% Vi %*% y))
  expect_equal(r$beta, bet, tolerance = 1e-10)
  res <- y - as.numeric(X %*% bet)
  nj <- c(3, 4, 2)
  uh <- 0.4 * as.numeric(tapply(res, cl, sum)) / (0.3 + nj * 0.4)
  vu <- 0.4 * 0.3 / (0.3 + nj * 0.4)
  expect_equal(r$u_hat, uh, tolerance = 1e-10)
  expect_equal(r$sigma2_u, mean(uh^2 + vu), tolerance = 1e-10)
  expect_equal(r$sigma2_e, (sum((res - rep(uh, nj))^2) + sum(nj * vu)) / 9, tolerance = 1e-10)
  fx <- Emaxr(y, X, cl, 0.4, 0, beta = c(1, 1))
  expect_equal(fx$u_hat, as.numeric(tapply(y - X %*% c(1, 1), cl, mean)), tolerance = 1e-12)
  expect_error(Emaxr(numeric(0), X, cl, 1, 1), "no observations")
  expect_error(Emaxr(y, X[-1, ], cl, 1, 1), "one row per observation")
  expect_error(Emaxr(y, X, cl[-1], 1, 1), "one label per observation")
  expect_error(Emaxr(y, X, cl, -1, 1), "sigma2_u")
  expect_error(Emaxr(y, X, cl, 1, 0), "sigma2_e")
  expect_error(Emaxr(y, X, cl, 1, 1, beta = 1), "one entry per column")
})

test_that("EkfF runs the extended Kalman recursion", {
  y <- c(0.9, 1.3, 0.7, 1.8)
  f <- function(x) c(0.9 * x[1] + 0.1 * sin(x[2]), x[2] + 0.2)
  Fj <- function(x) rbind(c(0.9, 0.1 * cos(x[2])), c(0, 1))
  h <- function(x) x[1]^2
  Hj <- function(x) c(2 * x[1], 0)
  Q <- diag(c(0.05, 0.01))
  x <- c(1, 0)
  P <- diag(2)
  ll <- 0
  for (t in 1:4) {
    xp <- f(x)
    Fk <- Fj(x)
    Pp <- Fk %*% P %*% t(Fk) + Q
    Hk <- Hj(xp)
    S <- as.numeric(t(Hk) %*% Pp %*% Hk) + 0.2
    K <- as.numeric(Pp %*% Hk) / S
    v <- y[t] - h(xp)
    x <- xp + K * v
    P <- Pp - S * outer(K, K)
    ll <- ll + dnorm(v, 0, sqrt(S), log = TRUE)
  }
  r <- EkfF(y, f, h, Fj, Hj, Q, 0.2, x0 = c(1, 0))
  expect_equal(r$state, x, tolerance = 1e-12)
  expect_equal(r$cov, as.numeric(t(P)), tolerance = 1e-12)
  expect_equal(r$loglik, ll, tolerance = 1e-12)
  lin <- EkfF(1, function(x) x, function(x) x, 1, 1, 1, 1)
  expect_equal(c(lin$state, lin$cov), c(2 / 3, 2 / 3), tolerance = 1e-12)
  expect_error(EkfF(numeric(0), f, h, Fj, Hj, Q, 1), "no observations")
  expect_error(EkfF(y, f, h, Fj, Hj, matrix(1, 2, 3), 1), "square")
  expect_error(EkfF(y, f, h, Fj, Hj, Q, 0), "R must be positive")
  expect_error(EkfF(y, f, h, Fj, Hj, Q, 1, x0 = 1), "x0 length")
  expect_error(EkfF(y, f, h, Fj, Hj, Q, 1, P0 = diag(3)), "d x d")
  expect_error(EkfF(y, f, h, Fj, function(x) 1, Q, 1, x0 = c(1, 0)), "length-d row")
})

test_that("Empbnp gives the Robbins and Tweedie estimates", {
  y <- c(0, 1, 1, 2, 0, 3, 1, 0, 2, 1)
  r <- Empbnp(y)
  N <- tabulate(y + 1, 5)
  th <- ifelse(N[1:4] > 0, (1:4) * N[2:5] / N[1:4], NaN)
  expect_equal(r$theta_hat, th, tolerance = 1e-12)
  expect_equal(r$estimate, mean(th[y + 1]), tolerance = 1e-12)
  expect_equal(r$counts, N[1:4])
  g <- c(0.3, -1.2, 0.8, 2.1, 0.5, -0.4)
  tw <- Empbnp(g, "Gaussian")
  h <- 1.06 * sd(g) * 6^(-0.2)
  K <- exp(-0.5 * (outer(g, g, "-") / h)^2)
  expect_equal(tw$theta_hat, g - rowSums(K * outer(g, g, "-")) / (h^2 * rowSums(K)), tolerance = 1e-12)
  expect_error(Empbnp(numeric(0)), "no observations")
  expect_error(Empbnp(y, "beta"), "poisson' or 'gaussian")
  expect_error(Empbnp(-y), "non-negative")
  expect_error(Empbnp(c(1, 1), "gaussian"), "zero spread")
})

test_that("Ferror forecasts by the renewal equation", {
  inc <- c(10, 12, 15, 20)
  w <- c(0.2, 0.5, 0.3)
  r <- Ferror(inc, Rt = c(1.2, 0.9), gen_int = w * 2)
  l1 <- sum(rev(inc)[1:3] * w)
  f1 <- 1.2 * l1
  l2 <- sum(c(f1, 20, 15) * w)
  expect_equal(r$lambda_, c(l1, l2), tolerance = 1e-12)
  expect_equal(r$forecast, c(f1, 0.9 * l2), tolerance = 1e-12)
  expect_equal(r$Rt_implied, 20 / sum(c(15, 12, 10) * w), tolerance = 1e-12)
  expect_equal(Ferror(5, 2, c(1, 1))$forecast, 5)
  expect_error(Ferror(numeric(0), 1, 1), "no observations")
  expect_error(Ferror(-inc, 1, 1), "non-negative")
  expect_error(Ferror(inc, 1, numeric(0)), "at least one lag")
  expect_error(Ferror(inc, 1, -1), "non-negative")
  expect_error(Ferror(inc, 1, 0), "all be zero")
  expect_error(Ferror(inc, numeric(0), 1), "at least one step")
  expect_error(Ferror(inc, -1, 1), "Rt must be non-negative")
})
