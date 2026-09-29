# Coverage for pnie .. polkn exports (mediation, Poisson family, point
# process summaries, roll calls, polynomial kernel). Every expectation is
# recomputed in the test body.

test_that("Pnie is the pure natural indirect effect of the VanderWeele model", {
  x <- c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0)
  cc <- c(0.5, 1.2, -0.3, 0.8, 0.1, 1.5, -0.6, 0.9, 0.4, -0.2)
  m <- 0.5 + 0.8 * x + 0.3 * cc + c(0.1, -0.2, 0.05, 0.15, -0.1, 0.2, -0.05, 0, 0.1, -0.1)
  y <- 1 + 0.4 * x + 0.9 * m + 0.2 * x * m + 0.5 * cc +
    c(0.05, 0.1, -0.1, 0.0, 0.2, -0.15, 0.1, -0.05, 0.0, 0.1)
  r <- Pnie(x, m, y, C = cc, a = 1, astar = 0)
  b <- stats::coef(stats::lm(m ~ x + cc))
  th <- stats::coef(stats::lm(y ~ x + m + I(x * m) + cc))
  # the helper solves the normal equations with a 1e-10 ridge, which moves
  # the coefficients by about 1e-9 relative
  expect_equal(r$pnie, unname(th[3] * b[2] + th[4] * b[2] * 0), tolerance = 1e-8)
  expect_equal(r$tnie, unname(th[3] * b[2] + th[4] * b[2] * 1), tolerance = 1e-8)
  cb <- mean(cc)
  expect_equal(r$pnde, unname(th[2] + th[4] * (b[1] + b[3] * cb)), tolerance = 1e-8)
  expect_equal(r$te, r$pnde + r$tnie, tolerance = 1e-12)
  expect_equal(r$estimate, r$pnie)
  expect_error(Pnie(x, m[-1], y), "same length")
  expect_error(Pnie(x[1:3], m[1:3], y[1:3]), "too few observations")
})

test_that("Poislo is the Poisson negative log-likelihood kernel", {
  Y <- matrix(c(0, 2, 1, 3, 5, 0), 3)
  H <- matrix(c(0.5, 1.8, 1.2, 2.5, 4.0, 0.3), 3)
  r <- Poislo(Y, H)
  expect_equal(r$loss, sum(H - Y * log(H)), tolerance = 1e-12)
  expect_equal(r$loss, -sum(stats::dpois(Y, H, log = TRUE)) - sum(lgamma(Y + 1)), tolerance = 1e-12)
  expect_equal(r$mean_loss, r$loss / 6, tolerance = 1e-12)
  expect_error(Poislo(Y, -H), "strictly positive")
  expect_error(Poislo(Y, H[1:2, ]), "same shape")
})

test_that("AbramsonIntensity uses square-root-law adaptive bandwidths", {
  P <- rbind(c(0.1, 0.2), c(0.3, 0.1), c(0.2, 0.4), c(0.8, 0.9), c(0.5, 0.5))
  at <- rbind(c(0.2, 0.2), c(0.7, 0.8))
  gk <- function(d2, h) exp(-d2 / (2 * h^2)) / (2 * pi * h^2)
  D2 <- as.matrix(stats::dist(P))^2
  pilot <- rowSums(gk(D2, 0.3))
  h <- 0.3 * pmin(sqrt(exp(mean(log(pilot))) / pilot), 5)
  lam <- vapply(1:2, function(k) sum(gk(colSums((t(P) - at[k, ])^2), h)), 0)
  r <- AbramsonIntensity(P, at, 0.3)
  expect_equal(r$pilot, pilot, tolerance = 1e-12)
  expect_equal(r$bandwidths, h, tolerance = 1e-12)
  expect_equal(r$intensity, lam, tolerance = 1e-12)
  # the trim caps the bandwidth ratio
  expect_true(all(AbramsonIntensity(P, at, 0.3, trim = 1)$bandwidths <= 0.3 + 1e-15))
})

test_that("StJFunction is (1 - G) / (1 - F) for space-time nearest events", {
  P <- rbind(c(0.1, 0.2, 0.5), c(0.3, 0.1, 1.5), c(0.25, 0.3, 1.0), c(0.8, 0.9, 3.0), c(0.6, 0.5, 2.2))
  rv <- c(0.15, 0.3)
  tv <- c(0.6, 1.2)
  r <- StJFunction(P, rv, tv, c(0, 1, 0, 1), c(0, 4), n_grid = 4, n_time = 3)
  G <- outer(rv, tv, Vectorize(function(rr, tt) {
    mean(vapply(1:5, function(i) {
      any(vapply(setdiff(1:5, i), function(j) sqrt(sum((P[i, 1:2] - P[j, 1:2])^2)) <= rr &&
                   abs(P[i, 3] - P[j, 3]) <= tt, TRUE))
    }, TRUE))
  }))
  g <- expand.grid(i = 0:3, j = 0:3, k = 0:2)
  ref <- cbind((g$i + 0.5) / 4, (g$j + 0.5) / 4, (g$k + 0.5) * 4 / 3)
  F <- outer(rv, tv, Vectorize(function(rr, tt) {
    mean(apply(ref, 1, function(q) any(sqrt((P[, 1] - q[1])^2 + (P[, 2] - q[2])^2) <= rr &
                                         abs(P[, 3] - q[3]) <= tt)))
  }))
  expect_equal(r$G, G, tolerance = 1e-12)
  expect_equal(r$F, F, tolerance = 1e-12)
  expect_equal(r$J, (1 - G) / (1 - F), tolerance = 1e-12)
})

test_that("Poispen is ridge-penalised Poisson IRLS", {
  X <- cbind(c(0.2, 1.1, -0.5, 0.8, 1.9, -1.0, 0.3, 0.6, 1.4, -0.2),
             c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0))
  y <- c(1, 3, 0, 2, 6, 0, 1, 2, 4, 1)
  r <- Poispen(X, y, lam = 0.5)
  Z <- cbind(1, X)
  mu <- exp(as.numeric(Z %*% r$beta))
  # stationarity of l(beta) - lam/2 |beta_-1|^2
  expect_equal(as.numeric(crossprod(Z, y - mu)), c(0, 0.5 * r$beta[2:3]), tolerance = 1e-9)
  expect_equal(r$loglik, sum(stats::dpois(y, mu, log = TRUE)), tolerance = 1e-10)
  r0 <- Poispen(X, y, lam = 0)
  expect_equal(r0$beta, unname(stats::coef(stats::glm(y ~ X, family = stats::poisson(),
                                                      control = list(epsilon = 1e-14)))),
               tolerance = 1e-8)
  # lasso variant: the returned beta is a fixed point of its thresholded step
  rl <- Poispen(X, y, lam = 0.2, penalty = "lasso", n_iter = 200)
  eta <- as.numeric(Z %*% rl$beta)
  w <- exp(eta)
  z <- eta + (y - w) / w
  nb <- as.numeric(solve(crossprod(Z, Z * w) + diag(c(0, 0.2, 0.2)), crossprod(Z, w * z)))
  nb[2:3] <- sign(nb[2:3]) * pmax(abs(nb[2:3]) - 0.2, 0)
  expect_equal(rl$beta, nb, tolerance = 1e-8)
})

test_that("Poisdev gives the Poisson deviance and Pearson statistics", {
  y <- c(0, 2, 5, 1, 3, 4)
  lam <- c(0.5, 1.8, 4.2, 1.5, 2.6, 3.9)
  r <- Poisdev(y, lam, p = 2)
  dres <- stats::poisson()$dev.resids(y, lam, rep(1, 6))
  expect_equal(r$deviance, sum(dres), tolerance = 1e-12)
  expect_equal(r$pearson_chisq, sum((y - lam)^2 / lam), tolerance = 1e-12)
  expect_equal(r$resid_sum, sum(y - lam), tolerance = 1e-12)
  expect_equal(r$D_nointercept, r$deviance + 2 * sum(y - lam), tolerance = 1e-12)
  expect_equal(r$df, 3L)
  expect_equal(r$pvalue, stats::pchisq(sum(dres), 3, lower.tail = FALSE), tolerance = 1e-12)
  expect_null(Poisdev(y, lam)$df)
  expect_error(Poisdev(y, lam, p = 5), "at least 1")
  expect_error(Poisdev(c(-1, 1), c(1, 1)), "non-negative")
  expect_error(Poisdev(y, -lam), "finite and positive")
  expect_error(Poisdev(y, lam[-1]), "same length")
})

test_that("the Poisson closed-form helpers agree with dpois", {
  expect_equal(PoisPmf(3, 2.5)$probability, stats::dpois(3, 2.5), tolerance = 1e-12)
  expect_equal(PoisPmf(0, 0)$probability, 1)
  expect_error(PoisPmf(1.5, 1), "integer >= 0")
  expect_error(PoisPmf(1, -1), ">= 0")
  expect_equal(PoisZero(4)$p_zero, exp(-4), tolerance = 1e-12)
  expect_equal(PoisMean(3.7)$mean, 3.7, tolerance = 1e-12)
  expect_equal(PoisVar(3.7)$variance, 3.7, tolerance = 1e-11)
  expect_equal(PoisMode(3.5)$mode, 3L)
  expect_equal(PoisMode(3.5)$p_mode, stats::dpois(3, 3.5), tolerance = 1e-12)
  # integer mean: a - 1 and a tie; the function reports a - 1
  expect_equal(PoisMode(4)$mode, 3L)
  expect_error(PoisMode(0), "> 0")
  expect_equal(PoisNorm(2, terms = 5)$normalization, stats::ppois(4, 2), tolerance = 1e-12)
  expect_equal(PoisSum1(2, kmax = 5)$total, stats::ppois(4, 2), tolerance = 1e-12)
  expect_equal(PoisSum1(2)$error, abs(stats::ppois(199, 2) - 1), tolerance = 1e-12)
  expect_error(PoisNorm(2, terms = 0), "integer >= 1")
  expect_error(PoisSum1(2, kmax = 1.5), "integer >= 1")
  pk <- PoisPeak(40, 0.1)
  expect_equal(pk$k, 4L)
  expect_equal(pk$PP, stats::dpois(4, 4), tolerance = 1e-12)
  expect_error(PoisPeak(10, 2), "p in \\[0, 1\\]")
  g <- PoisGauss(5, 4)
  expect_equal(g$PG, stats::dnorm(5, 4, 2), tolerance = 1e-12)
  expect_equal(g$exact, stats::dpois(5, 4), tolerance = 1e-12)
  expect_equal(PoisGauss(-1, 4)$exact, 0)
  expect_error(PoisGauss(1, 0), "> 0")
  s <- PoisStirl(6, 5)
  expect_equal(s$exact, stats::dpois(6, 5), tolerance = 1e-12)
  expect_equal(s$approx, 5^6 * exp(-5) / (sqrt(2 * pi * 6) * (6 / exp(1))^6), tolerance = 1e-12)
  expect_error(PoisStirl(0, 1), "integer >= 1")
  sc <- PoisStirC(1.3, 5)
  expect_equal(sc$k, 6L)
  expect_equal(sc$approx, s$approx)
  expect_error(PoisStirC(-10, 5), "k >= 1")
})

test_that("Poispred is the gamma-Poisson conjugate update", {
  y <- c(2, 0, 3, 1, 4)
  e <- c(1, 0.5, 2, 1, 1.5)
  r <- Poispred(y, 2, 1.5, exposure = e)
  ap <- 2 + 10
  bp <- 1.5 + 6
  expect_equal(c(r$alpha_post, r$beta_post), c(ap, bp))
  expect_equal(r$rate_var, ap / bp^2, tolerance = 1e-12)
  # the negative-binomial predictive with size ap and prob bp / (bp + 1)
  nbv <- ap * (1 / (bp + 1)) / (bp / (bp + 1))^2
  expect_equal(r$pred_var, nbv, tolerance = 1e-12)
  expect_equal(r$overdispersion, 1 + 1 / bp, tolerance = 1e-12)
  expect_equal(Poispred(y, 2, 1.5)$beta_post, 6.5)
  expect_error(Poispred(y, 0, 1), "alpha > 0")
  expect_error(Poispred(c(-1, 2), 1, 1), "non-negative")
  expect_error(Poispred(y, 1, 1, exposure = e[-1]), "one entry per observation")
  expect_error(Poispred(y, 1, 1, exposure = -e), "positive")
  expect_error(Poispred(numeric(0), 1, 1), "at least one")
})

test_that("RollcallSummary counts yeas, nays and participation", {
  M <- rbind(c(1, 0, NA, 1), c(1, 1, 0, NA), c(0, 1, 0, 1), c(1, NA, 0, 0))
  r <- RollcallSummary(M)
  ye <- apply(M, 2, function(v) sum(v == 1, na.rm = TRUE))
  na <- apply(M, 2, function(v) sum(v == 0, na.rm = TRUE))
  expect_equal(r$yeas, ye)
  expect_equal(r$nays, na)
  expect_equal(r$minority_share, pmin(ye, na) / (ye + na), tolerance = 1e-12)
  expect_equal(r$margin, abs(ye - na))
  expect_equal(r$participation, c(3, 3, 4, 3))
  expect_true(is.nan(RollcallSummary(cbind(c(NA, NA)))$minority_share))
})

test_that("Polykern is (gamma x'z + coef0)^degree", {
  X <- rbind(c(1, 0.5), c(-0.3, 2), c(0.7, -1))
  Z <- rbind(c(0.2, 0.1), c(1, 1))
  r <- Polykern(X, degree = 3, gamma = 0.4, coef0 = 2, Z = Z)
  expect_equal(r$K, (0.4 * X %*% t(Z) + 2)^3, tolerance = 1e-12)
  d <- Polykern(X)
  expect_equal(d$K, (X %*% t(X) / 2 + 1)^2, tolerance = 1e-12)
  expect_equal(d$gamma, 0.5)
  expect_equal(c(r$n, r$m), c(3L, 2L))
})
