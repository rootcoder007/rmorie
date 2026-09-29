# Coverage for tsallen .. ucbb_native exports. Every expectation is
# recomputed in the test body.

test_that("Tsallis entropies, Tversky index and the two-coin variance", {
  y <- c(1, 2, 2, 3, 3, 3, 4, 1)
  p <- as.numeric(table(y)) / 8
  expect_equal(Tsallen(y, 2)$estimate, 1 - sum(p^2), tolerance = 1e-12)
  expect_equal(Tsallen(y, 1)$estimate, -sum(p * log(p)), tolerance = 1e-12)
  w <- c(2, 1, 1, 0)
  q <- w / 4
  expect_equal(Tsalls(w, 0.5)$estimate, (1 - sum(q^0.5)) / (0.5 - 1), tolerance = 1e-12)
  expect_equal(Tsalls(w, 1)$estimate, -sum(q[q > 0] * log(q[q > 0])), tolerance = 1e-12)
  a <- c(1, 0, 1, 1, 0, 1)
  b <- c(1, 1, 0, 1, 0, 0)
  tv <- Tvsbn(a, b, alpha = 0.3, beta = 0.7)
  expect_equal(tv$estimate, 2 / (2 + 0.3 * 2 + 0.7 * 1), tolerance = 1e-12)
  expect_true(is.nan(Tvsbn(c(0, 0), c(0, 0))$estimate))
  expect_equal(TwoCoinVar(), list(variance = 0.5, mean = 1))
})

test_that("Tukey biweight location, rho/psi and regression", {
  y <- c(2.1, 1.9, 2.3, 2.0, 8.5, 2.2, 1.8, 2.4, -3.0)
  r <- Tukeyw(y, c = 4.685, n_iter = 20)
  mu <- stats::median(y)
  s <- stats::mad(y, constant = 1 / stats::qnorm(0.75))
  for (i in 1:20) {
    u <- (y - mu) / (4.685 * s)
    w <- ifelse(abs(u) < 1, (1 - u^2)^2, 0)
    mu <- sum(w * y) / sum(w)
  }
  expect_equal(r$scale, s, tolerance = 1e-12)
  expect_equal(r$estimate, mu, tolerance = 1e-12)
  expect_equal(r$weights, w, tolerance = 1e-12)
  expect_equal(Tukeyw(rep(1, 3))$scale, 1)
  v <- c(-6, -2, 0.5, 3, 5)
  k <- Tukrho(v, c = 4)
  u <- v / 4
  ins <- abs(u) <= 1
  expect_equal(k$rho, ifelse(ins, 16 / 6 * (1 - (1 - u^2)^3), 16 / 6), tolerance = 1e-12)
  expect_equal(k$psi, ifelse(ins, v * (1 - u^2)^2, 0), tolerance = 1e-12)
  X <- cbind(1, c(0.5, 1.2, 2.2, 3.1, 4.0, 5.3, 6.1, 7.2))
  yy <- c(1.1, 2.0, 2.4, 4.5, 4.1, 12.0, 6.3, 7.0)
  rr <- Tukrr(X, yy, n_iter = 25)
  res <- stats::lm.fit(X, yy)$residuals
  for (i in 1:25) {
    s <- stats::median(abs(res - stats::median(res))) / stats::qnorm(0.75)
    u <- res / (4.685 * s)
    w <- ifelse(abs(u) < 1, (1 - u^2)^2, 0)
    f <- stats::lm.wfit(X, yy, w)
    res <- yy - as.numeric(X %*% f$coefficients)
  }
  expect_equal(rr$estimate, unname(f$coefficients), tolerance = 1e-9)
  expect_equal(rr$weights, w, tolerance = 1e-9)
  expect_lt(rr$weights[6], 0.5)
})

test_that("morie_ttsAn flags seasonal-hybrid ESD anomalies", {
  x <- 10 + 3 * sin(2 * pi * (1:48) / 12) + c(0.2, -0.1, 0.3, -0.2)[rep(1:4, 12)]
  x[c(15, 33)] <- x[c(15, 33)] + c(9, -8)
  r <- morie_ttsAn(x, 12, k = 4)
  S <- morie_stl_decompose(x, 12, s_window = 7)$seasonal
  R <- x - S - stats::median(x)
  expect_equal(r$residual, R, tolerance = 1e-12)
  vals <- R
  idx <- seq_along(R)
  st <- lam <- rem <- numeric(0)
  for (i in 1:4) {
    ctr <- stats::median(vals)
    sc <- stats::mad(vals, center = ctr)
    dv <- abs(vals - ctr)
    b <- which.max(dv)
    st <- c(st, dv[b] / sc)
    tq <- stats::qt(1 - 0.05 / (2 * (48 - i + 1)), 48 - i - 1)
    lam <- c(lam, (48 - i) * tq / sqrt((48 - i - 1 + tq^2) * (48 - i + 1)))
    rem <- c(rem, idx[b])
    vals <- vals[-b]
    idx <- idx[-b]
  }
  expect_equal(r$statistics, st, tolerance = 1e-10)
  expect_equal(r$critical_values, lam, tolerance = 1e-10)
  nk <- max(c(0, which(st > lam)))
  expect_equal(r$anomalies, sort(rem[seq_len(nk)]))
  expect_true(all(c(15, 33) %in% r$anomalies))
  expect_equal(morie_t_quantile(0.975, 7), stats::qt(0.975, 7))
  expect_error(morie_t_quantile(1, 3), "p in \\(0,1\\)")
  pos <- morie_ttsAn(x, 12, k = 2, direction = "pos")
  expect_true(15 %in% pos$anomalies)
})

test_that("Dprime solves the two-locus EM and scales D", {
  g1 <- c(2, 2, 1, 1, 0, 2, 1, 2, 1, 0, 2, 2)
  g2 <- c(2, 1, 1, 2, 0, 2, 1, 2, 0, 1, 2, 1)
  r <- Dprime(g1, g2)
  n <- 12
  pA <- sum(g1) / 24
  pB <- sum(g2) / 24
  tab <- table(factor(g1, 0:2), factor(g2, 0:2))
  nAB <- 2 * tab[3, 3] + tab[3, 2] + tab[2, 3]
  dh <- tab[2, 2]
  p <- r$pAB
  wt <- p * (1 - pA - pB + p) / (p * (1 - pA - pB + p) + (pA - p) * (pB - p))
  expect_equal(p, (nAB + dh * wt) / 24, tolerance = 1e-10)
  D <- p - pA * pB
  dmax <- if (D > 0) min(pA * (1 - pB), (1 - pA) * pB) else max(-pA * pB, -(1 - pA) * (1 - pB))
  expect_equal(r$estimate, abs(D) / abs(dmax), tolerance = 1e-10)
  expect_equal(r$r2, D^2 / (pA * pB * (1 - pA) * (1 - pB)), tolerance = 1e-10)
  expect_error(Dprime(1:3, 1:2), "same length")
  expect_error(Dprime(c(1, 3), c(1, 2)), "at least 2 complete")
})

test_that("Twostg solves the Cheng-Wei-Ying estimating equation", {
  skip_if_not_installed("survival")
  t <- c(2.1, 5.0, 3.2, 8.1, 1.4, 7.3, 4.2, 6.0, 2.8, 3.9)
  d <- c(1, 0, 1, 1, 1, 0, 1, 1, 0, 1)
  x <- c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.0, -0.5, 1.4, 0.2)
  km <- survival::survfit(survival::Surv(t, 1 - d) ~ 1)
  Gm <- vapply(t, function(s) {
    k <- km$time < s
    if (any(k)) utils::tail(km$surv[k], 1) else 1
  }, 0)
  score <- function(b, xi) {
    U <- 0
    for (i in 1:10) for (j in 1:10) if (i != j) {
      z <- x[i] - x[j]
      U <- U + z * ((d[j] == 1 && t[i] >= t[j]) / Gm[j]^2 - xi(z * b))
    }
    U
  }
  ph <- Twostg(t, d, x)
  expect_equal(score(ph$estimate, function(s) 1 / (1 + exp(s))), 0, tolerance = 1e-8)
  expect_gt(ph$se, 0)
  xi_po <- function(s) stats::integrate(function(u) stats::plogis(-(u + s)) * stats::dlogis(u), -Inf, Inf, rel.tol = 1e-12)$value
  po <- Twostg(t, d, x, error = "po")
  # Simpson's rule on [-40, 40] with 4000 panels against adaptive quadrature
  expect_equal(score(po$estimate, xi_po), 0, tolerance = 1e-7)
  expect_error(Twostg(t, d, x, error = "aft"), "'ph' or 'po'")
})

test_that("morie_ucbb follows UCB1", {
  x <- rbind(c(1, 0, 0), c(0, 1, 0), c(1, 0, 1), c(1, 1, 0), c(0, 1, 1), c(1, 0, 0), c(1, 1, 1), c(0, 0, 1))
  r <- morie_ucbb(x)
  cnt <- sm <- numeric(3)
  act <- numeric(8)
  for (t in 1:8) {
    j <- if (t <= 3) t else which.max(sm / cnt + sqrt(2 * log(t - 1) / cnt))
    cnt[j] <- cnt[j] + 1
    sm[j] <- sm[j] + x[t, j]
    act[t] <- j - 1
  }
  expect_equal(r$actions, act)
  expect_equal(r$means, sm / cnt, tolerance = 1e-12)
  expect_equal(r$index, sm / cnt + sqrt(2 * log(8) / cnt), tolerance = 1e-12)
  expect_equal(r$estimate, which.max(sm / cnt) - 1)
  expect_error(morie_ucbb(x, T = 2), "at least K = 3")
  expect_error(morie_ucbb(x, T = 9), "only 8 rows")
})
