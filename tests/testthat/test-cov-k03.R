# Coverage for the k03 shelf: ARDL bounds test, MCMC running traces,
# SV quasi-likelihood, 1-Wasserstein distance, Wide & Deep, the Hajek
# survey mean, spatial weights, Zivot-Andrews, zCDP accounting and the
# z-transform wrapper; recomputed with lm(), survey, urca and base R.

test_that("Ardlmd is the PSS F and t test on the conditional ECM", {
  set.seed(1)
  n <- 60
  x <- cumsum(rnorm(n))
  y <- numeric(n)
  for (t in 2:n) y[t] <- 0.5 * y[t - 1] + 0.4 * x[t] + rnorm(1, sd = 0.3)
  r <- Ardlmd(y, x, p = 2, q = 1, case = 3)
  dy <- diff(y)
  dx <- diff(x)
  rows <- 2:(n - 1)
  d <- data.frame(dy = dy[rows], yl = y[rows], xl = x[rows], dyl = dy[rows - 1], dx0 = dx[rows])
  fu <- lm(dy ~ yl + xl + dyl + dx0, data = d)
  fr <- lm(dy ~ dyl + dx0, data = d)
  F <- ((sum(resid(fr)^2) - sum(resid(fu)^2)) / 2) / (sum(resid(fu)^2) / df.residual(fu))
  expect_equal(r$f_statistic, F, tolerance = 1e-9)
  expect_equal(r$t_statistic, unname(summary(fu)$coefficients["yl", 3]), tolerance = 1e-9)
  expect_equal(r$pi_yy, unname(coef(fu)["yl"]), tolerance = 1e-9)
  expect_equal(r$df_den, df.residual(fu))
  r5 <- Ardlmd(y, x, p = 1, q = 1, case = 5)
  r1 <- 1:(n - 1)
  f5 <- lm(dy[r1] ~ I(r1 + 1) + y[r1] + x[r1] + dx[r1])
  expect_equal(r5$pi_yy, unname(coef(f5)[3]), tolerance = 1e-9)
  expect_error(Ardlmd(y, x, case = 2), "only cases 3 and 5")
  expect_error(Ardlmd(y, x, p = 0), "at least 1")
})

test_that("Baytrace tracks running means and quantiles", {
  ch <- cbind(c(1, 3, 2, 5, 4), c(0, -1, 2, 1, 1))
  r <- Baytrace(ch, probs = c(0.25, 0.5))
  expect_equal(r$running_mean[[1]], cumsum(ch[, 1]) / 1:5, tolerance = 1e-12)
  expect_equal(r$bands[[2]][[1]], vapply(1:5, function(t) quantile(ch[1:t, 2], 0.25, names = FALSE), 0),
               tolerance = 1e-12)
  expect_equal(r$final_mean, colMeans(ch), tolerance = 1e-12)
  expect_equal(Baytrace(1:4)$n_chains, 1L)
  expect_error(Baytrace(1:3, probs = 2), "probs")
})

test_that("Volsv maximises the Harvey-Ruiz-Shephard Kalman quasi-likelihood", {
  set.seed(2)
  n <- 80
  h <- numeric(n)
  h[1] <- rnorm(1, 0, 0.5)
  for (t in 2:n) h[t] <- 0.9 * h[t - 1] + rnorm(1, 0, 0.3)
  r <- exp(h / 2) * rnorm(n)
  v <- Volsv(r, sweeps = 5)
  y <- log(r^2 + 1e-8)
  qll <- function(mu, phi, s2) {
    a <- 0
    p <- s2 / (1 - phi^2)
    ll <- 0
    for (t in 1:n) {
      e <- y[t] - (mu - 1.2703628454614782) - a
      f <- p + pi^2 / 2
      ll <- ll - 0.5 * (log(2 * pi * f) + e^2 / f)
      k <- p / f
      a <- phi * (a + k * e)
      p <- phi^2 * (p - k * p) + s2
    }
    ll
  }
  expect_equal(v$ll, qll(v$mu, v$phi, v$sigma_eta^2), tolerance = 1e-9)
  ls <- log(v$sigma_eta)
  expect_gte(v$ll, qll(v$mu, v$phi, exp(2 * (ls + 1e-4))) - 1e-9)
  expect_gte(v$ll, qll(v$mu, v$phi, exp(2 * (ls - 1e-4))) - 1e-9)
  expect_error(Volsv(r[1:5]), "ten observations")
  expect_error(Volsv(r, init = c(0, 1, 1)), "phi")
})

test_that("Wassdt is the area between CDFs", {
  x <- c(0.3, 1.2, 2.5, 0.9)
  y <- c(1.1, 0.2, 3.0, 2.2)
  expect_equal(Wassdt(x, y)$distance, mean(abs(sort(x) - sort(y))), tolerance = 1e-12)
  y2 <- c(0.5, 2.0)
  g <- sort(c(x, y2))
  area <- sum(vapply(1:5, function(i) abs(mean(x <= g[i]) - mean(y2 <= g[i])) * (g[i + 1] - g[i]), 0))
  expect_equal(Wassdt(x, y2)$distance, area, tolerance = 1e-12)
  s <- c(0, 1, 3, 4)
  p <- c(1, 2, 1, 0)
  q <- c(0, 1, 1, 2)
  expect_equal(Wassdt(p, q, s)$distance,
               sum(abs(cumsum(p / 4) - cumsum(q / 4))[1:3] * diff(s)), tolerance = 1e-12)
  expect_error(Wassdt(p, q, c(0, 2, 1, 3)), "strictly increasing")
  expect_error(Wassdt(numeric(0), 1), "non-empty")
})

test_that("WideD takes one exact gradient step from zero output weights", {
  set.seed(3)
  n <- 10
  Xw <- cbind(rbinom(n, 1, 0.5), rbinom(n, 1, 0.5))
  Xd <- matrix(rnorm(n * 3), n)
  y <- rbinom(n, 1, 0.5)
  r <- WideD(Xw, Xd, y, hidden = c(4, 2), epochs = 1, lr = 0.2, seed = 5, crosses = list(c(0, 1)))
  Xc <- cbind(Xw, Xw[, 1] * Xw[, 2])
  W <- r$hidden_weights
  fwd <- function(x) {
    a <- x
    for (l in seq_along(W)) a <- pmax(vapply(seq_along(W[[l]]), function(u) sum(W[[l]][[u]] * a) + r$hidden_bias[[l]][u], 0), 0)
    a
  }
  top <- t(apply(Xd, 1, fwd))
  res <- 0.5 - y
  expect_equal(r$loss, log(2), tolerance = 1e-12)
  expect_equal(r$bias, -0.2 * mean(res), tolerance = 1e-12)
  expect_equal(r$coef_wide, -0.2 * colMeans(res * Xc), tolerance = 1e-12)
  expect_equal(r$coef_deep, -0.2 * colMeans(res * top), tolerance = 1e-12)
  z <- r$bias + Xc %*% r$coef_wide + top %*% r$coef_deep
  expect_equal(r$fitted, as.numeric(plogis(z)), tolerance = 1e-12)
  expect_equal(r$n_wide, 3L)
  expect_error(WideD(Xw, Xd, y + 1), "binary")
  expect_error(WideD(Xw, Xd, y, crosses = list(c(0, 5))), "cross indices")
})

test_that("Wmeansr matches survey::svymean with-replacement linearisation", {
  y <- c(3, 7, 2, 9, 4, 6)
  w <- c(10, 20, 15, 5, 30, 20)
  r <- Wmeansr(y, w)
  m <- sum(w * y) / sum(w)
  expect_equal(r$estimate, m, tolerance = 1e-12)
  expect_equal(r$se, sqrt(6 / 5 * sum(w^2 * (y - m)^2)) / sum(w), tolerance = 1e-12)
  skip_if_not_installed("survey")
  des <- survey::svydesign(ids = ~1, weights = ~w, data = data.frame(y = y, w = w))
  sm <- survey::svymean(~y, des)
  expect_equal(r$se, as.numeric(survey::SE(sm)), tolerance = 1e-9)
  expect_error(Wmeansr(y, -w), "non-negative")
  expect_error(Wmeansr(1, 1), "two sampled units")
})

test_that("Wmtwgt builds distance, kNN and inverse-distance weights", {
  P <- rbind(c(0, 0), c(1, 0), c(0, 2), c(3, 3))
  D <- unname(as.matrix(dist(P)))
  r <- Wmtwgt(P, "distance", 2.1)
  B <- (D > 0 & D <= 2.1) * 1
  expect_equal(r$weights, B / pmax(rowSums(B), 1), tolerance = 1e-12)
  expect_equal(r$n_islands, sum(rowSums(B) == 0))
  k <- Wmtwgt(P, "knn", 1, row_standardize = FALSE)
  nn <- apply(D + diag(Inf, 4), 1, which.min)
  expect_equal(k$weights, (outer(1:4, 1:4, function(i, j) nn[i] == j)) * 1)
  iv <- Wmtwgt(P, "inverse", 10, alpha = 2)
  I <- ifelse(D > 0, D^-2, 0)
  expect_equal(iv$weights, I / rowSums(I), tolerance = 1e-12)
  expect_equal(iv$pct_nonzero, 100 * 12 / 16, tolerance = 1e-12)
  expect_error(Wmtwgt(P, "knn", 4), "k must lie")
})

test_that("Zaurts matches urca::ur.za", {
  skip_if_not_installed("urca")
  set.seed(4)
  x <- cumsum(rnorm(60)) + c(rep(0, 30), rep(3, 30))
  for (m in c("intercept", "trend", "both")) {
    r <- Zaurts(x, model = m, lags = 1)
    z <- urca::ur.za(x, model = m, lag = 1)
    expect_equal(r$statistic, unname(z@teststat), tolerance = 1e-9)
    expect_equal(r$break_point, as.integer(z@bpoint))
  }
  expect_error(Zaurts(x, lags = -1), "non-negative")
})

test_that("Zcdp composes rho and converts to (eps, delta)", {
  r <- Zcdp(c(0.1, 0.05, 0.2), rho = 0.5, delta = 1e-5, sensitivity = 2)
  expect_equal(r$rho_total, 0.35, tolerance = 1e-12)
  expect_equal(r$epsilon, 0.35 + 2 * sqrt(0.35 * log(1e5)), tolerance = 1e-12)
  expect_equal(r$sigma, 2 / sqrt(1), tolerance = 1e-12)
  expect_true(r$within_budget)
  expect_error(Zcdp(-1, 1), "non-negative")
  expect_error(Zcdp(1, 1, delta = 1), "delta")
})

test_that("Zfm evaluates the z-transform through Ztrans", {
  x <- c(1, -0.5, 0.25)
  z <- c(2, 1i)
  r <- Zfm(x, z, n0 = 1)
  expect_equal(r$X, vapply(z, function(zz) sum(x * zz^-(1:3)), 0i), tolerance = 1e-12)
  expect_equal(r$n, 1:3)
  expect_error(Zfm(x, NULL), "z must be given")
})
