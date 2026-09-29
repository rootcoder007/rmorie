# Coverage for interaction analysis, functional integrals, barrier LP and
# NLP solvers, four-way and interventional mediation, replicate-weight
# and survey IPW, forest AIPW, Tukey fences, impulse responses, weighted
# least squares, the IRT response models, MH DIF and the three-parameter
# IBP; recomputed with base R and reference packages.

test_that("Intanl is IP-weighted least squares with a sandwich SE", {
  skip_if_not_installed("sandwich")
  set.seed(1)
  n <- 40
  a <- rbinom(n, 1, 0.5)
  v <- rnorm(n)
  y <- 1 + a + 0.5 * v * a + v + rnorm(n)
  w <- runif(n, 0.5, 2)
  r <- Intanl(y, a, v, w)
  f <- lm(y ~ a + I(v * a) + v, weights = w)
  V <- sandwich::vcovHC(f, type = "HC0")
  expect_equal(c(r$beta0, r$beta_a, r$beta_av, r$beta_v), unname(coef(f)), tolerance = 1e-9)
  expect_equal(c(r$se0, r$se_a, r$se, r$se_v), unname(sqrt(diag(V))), tolerance = 1e-9)
  expect_equal(Intanl(y, a, v, NULL)$beta_av, unname(coef(lm(y ~ a + I(v * a) + v))[3]), tolerance = 1e-9)
  expect_error(Intanl(y, a[-1], v, w), "different lengths")
  expect_error(Intanl(y, a, v, -w), "non-negative")
})

test_that("Intf integrates a basis expansion by the trapezoid rule", {
  t <- c(0, 0.2, 0.5, 0.9, 1)
  B <- cbind(1, t, t^2)
  r <- Intf(c(2, -1, 3), B, t)
  tr <- function(v) sum(diff(t) * (head(v, -1) + tail(v, -1)) / 2)
  ints <- apply(B, 2, tr)
  expect_equal(r$basis_integrals, unname(ints), tolerance = 1e-12)
  expect_equal(r$estimate, sum(c(2, -1, 3) * ints), tolerance = 1e-12)
  expect_equal(Intf(1, matrix(c(0, 1, 2)))$estimate, 1, tolerance = 1e-12)
  expect_error(Intf(1:2, B), "one entry per basis column")
})

test_that("Intlpa follows the log-barrier central path to within its duality bound", {
  cv <- c(-1, -1)
  A <- rbind(c(1, 2), c(3, 1))
  b <- c(4, 6)
  r <- Intlpa(cv, A, b, x0 = c(0.5, 0.5), tau = 1e-4, iters = 200)
  expect_lte(r$newton_decrement / 2, 1e-14)
  expect_lte(r$objective - (-2.8), r$duality_bound + 1e-12)
  expect_gte(r$objective, -2.8)
  expect_equal(r$duality_bound, 4e-4, tolerance = 1e-12)
  expect_error(Intlpa(cv, A, b, x0 = c(5, 5)), "strictly feasible")
  expect_error(Intlpa(cv, A, b[1], x0 = c(0.5, 0.5)), "row counts")
})

test_that("Intmd4 and Intvse decompose the effect from the two linear models", {
  set.seed(2)
  n <- 60
  X <- rbinom(n, 1, 0.5)
  Cc <- rnorm(n)
  M <- 0.5 + 0.8 * X + 0.3 * Cc + rnorm(n)
  Y <- 1 + 0.4 * X + 0.6 * M + 0.5 * X * M + 0.2 * Cc + rnorm(n)
  th <- unname(coef(lm(Y ~ X + M + I(X * M) + Cc)))
  be <- unname(coef(lm(M ~ X + Cc)))
  bc <- be[1] + be[3] * mean(Cc)
  cde <- th[2] + th[4] * 0.5
  intref <- th[4] * (bc - 0.5)
  intmed <- th[4] * be[2]
  pie <- th[3] * be[2]
  r <- Intmd4(X, M, Y, Cc, a = 1, astar = 0, m = 0.5)
  expect_equal(c(r$cde, r$intref, r$intmed, r$pie), c(cde, intref, intmed, pie), tolerance = 1e-9)
  expect_equal(r$estimate, cde + intref + intmed + pie, tolerance = 1e-9)
  iv <- Intvse(Y, X, M, Cc, a = 1, astar = 0)
  expect_equal(iv$ide, th[2] + th[4] * bc, tolerance = 1e-9)
  expect_equal(iv$iie, (th[3] + th[4]) * be[2], tolerance = 1e-9)
  te0 <- th[2] + th[4] * bc + th[4] * be[2] + th[3] * be[2]
  expect_equal(iv$estimate, te0, tolerance = 1e-9)
  expect_equal(iv$check, 0, tolerance = 1e-9)
})

test_that("Ipferd and Ipwsrv are Hajek differences with JK1 / linearised variances", {
  y <- c(3, 5, 2, 7, 4, 6, 1, 8)
  D <- c(1, 1, 0, 1, 0, 0, 1, 0)
  w <- c(1, 2, 1, 1.5, 2, 1, 1, 0.5)
  R <- cbind(w * c(0, 2, 1, 1, 1, 1, 1, 1), w * c(2, 0, 1, 1, 1, 1, 1, 1), w * c(1, 1, 1, 1, 0, 2, 1, 1))
  hj <- function(ww) sum(ww[D == 1] * y[D == 1]) / sum(ww[D == 1]) - sum(ww[D == 0] * y[D == 0]) / sum(ww[D == 0])
  r <- Ipferd(y, D, w, R)
  th <- hj(w)
  reps <- apply(R, 2, hj)
  expect_equal(r$estimate, th, tolerance = 1e-12)
  expect_equal(r$variance, 2 / 3 * sum((reps - th)^2), tolerance = 1e-12)
  expect_equal(Ipferd(y, D, w, R, scale = 1)$variance, sum((reps - th)^2), tolerance = 1e-12)
  expect_error(Ipferd(y, D, w, R[, 1, drop = FALSE]), "two replicates")

  pi <- c(0.6, 0.5, 0.4, 0.7, 0.3, 0.5, 0.6, 0.4)
  s <- Ipwsrv(y, D, w, pi)
  ww <- w * ifelse(D == 1, 1 / pi, 1 / (1 - pi))
  m1 <- sum(ww[D == 1] * y[D == 1]) / sum(ww[D == 1])
  m0 <- sum(ww[D == 0] * y[D == 0]) / sum(ww[D == 0])
  expect_equal(s$estimate, m1 - m0, tolerance = 1e-12)
  expect_equal(s$var1, sum((ww[D == 1] * (y[D == 1] - m1))^2) / sum(ww[D == 1])^2, tolerance = 1e-12)
  expect_equal(s$ess, sum(ww)^2 / sum(ww^2), tolerance = 1e-12)
  expect_error(Ipwsrv(y, D, w, c(pi[-1], 1)), "strictly in")
})

test_that("Ipfsfa drives the barrier to the constrained optimum", {
  f <- function(x) (x[1] - 2)^2 + (x[2] - 1)^2
  g <- function(x) x[1] + x[2] - 2
  r <- Ipfsfa(f, list(g), c(0, 0), mu0 = 1, outer = 10, inner = 40)
  # after ten outer steps the barrier weight is 0.2^10, so x sits within
  # about mu / (2 * distance) of the KKT point (1.5, 0.5)
  expect_equal(r$x, c(1.5, 0.5), tolerance = 1e-5)
  expect_lt(r$max_violation, 0)
  expect_equal(r$objective, f(r$x), tolerance = 1e-12)
  expect_error(Ipfsfa(f, list(g), c(2, 2)), "strictly feasible")
  expect_error(Ipfsfa(f, list(g), c(0, 0), mu0 = 0), "mu0")
})

test_that("morie_ipwgrf averages the AIPW scores of its nuisances", {
  set.seed(3)
  n <- 80
  X <- matrix(rnorm(n * 2), n)
  W <- rep(c(0, 0, 1, 1), 20)
  y <- X[, 1] + W + rnorm(n)
  r <- morie_ipwgrf(y, W, X, n_folds = 2, n_trees = 3, min_leaf = 2)
  e <- pmin(pmax(r$propensity, 0.02), 0.98)
  g <- r$mu1 - r$mu0 + W * (y - r$mu1) / e - (1 - W) * (y - r$mu0) / (1 - e)
  expect_equal(r$scores, g, tolerance = 1e-12)
  expect_equal(r$ate, mean(g), tolerance = 1e-12)
  expect_equal(r$se, sd(g) / sqrt(n), tolerance = 1e-12)
  expect_equal(r$ci, mean(g) + c(-1, 1) * qnorm(0.975) * sd(g) / sqrt(n), tolerance = 1e-12)
  b <- morie_ipwgrf(y, W, X, n_folds = 2, n_trees = 3, min_leaf = 2, break_outcome = TRUE, break_propensity = TRUE)
  pb <- mean(W)
  expect_equal(b$ate, mean(W * (y - mean(y)) / pb - (1 - W) * (y - mean(y)) / (1 - pb)), tolerance = 1e-12)
  expect_error(morie_ipwgrf(y[1:50], W[1:50], X[1:50, ]), "at least 60")
  expect_error(morie_ipwgrf(y, W + 1, X), "binary")
})

test_that("IpwSn tilts the IP weights by exp(-lambda Y)", {
  set.seed(4)
  n <- 50
  x <- rnorm(n)
  C <- rbinom(n, 1, plogis(0.3 + x))
  Y <- 2 + x + rnorm(n)
  r <- IpwSn(Y, x, C, c(-0.5, 0, 0.5))
  pi <- fitted(glm(C ~ x, family = binomial))
  mu <- vapply(c(-0.5, 0, 0.5), function(L) {
    u <- exp(-L * Y[C == 1]) / pi[C == 1]
    sum(u * Y[C == 1]) / sum(u)
  }, 0)
  expect_equal(r$mu, mu, tolerance = 1e-9)
  expect_equal(r$estimate, mu[2], tolerance = 1e-9)
  expect_equal(r$range, max(mu) - min(mu), tolerance = 1e-9)
  expect_error(IpwSn(Y, x, rep(0, n), 0), "no observed")
  expect_error(IpwSn(Y, x, C, numeric(0)), "empty")
})

test_that("IqrA uses Tukey's hinges", {
  x <- c(2, 4, 4, 5, 6, 7, 8, 9, 30, -12)
  r <- IqrA(x)
  h <- fivenum(x)[c(2, 4)]
  expect_equal(c(r$lower, r$upper), c(h[1] - 1.5 * diff(h), h[2] + 1.5 * diff(h)), tolerance = 1e-12)
  expect_equal(r$flags, as.numeric(x < r$lower | x > r$upper))
  expect_equal(r$estimate, mean(r$flags), tolerance = 1e-12)
  expect_equal(IqrA(1:9, k = 3)$iqr, diff(fivenum(1:9)[c(2, 4)]), tolerance = 1e-12)
})

test_that("Irfun is the Cholesky-orthogonalised impulse response of a VAR", {
  A1 <- matrix(c(0.5, 0.1, 0.2, 0.3), 2)
  A2 <- matrix(c(-0.1, 0, 0.05, 0.1), 2)
  B <- cbind(c(0.1, -0.2), A1, A2)
  S <- matrix(c(1, 0.3, 0.3, 0.5), 2)
  r <- Irfun(B, S, horizon = 4, shock_var = 1)
  Phi <- list(diag(2), A1)
  for (h in 2:4) Phi[[h + 1]] <- A1 %*% Phi[[h]] + A2 %*% Phi[[h - 1]]
  L <- t(chol(S))
  irf <- t(vapply(Phi, function(P) (P %*% L)[, 2], numeric(2)))
  expect_equal(r$irf, irf, tolerance = 1e-12)
  expect_equal(r$estimate, irf[2, 2], tolerance = 1e-12)
  expect_equal(r$chol, L, tolerance = 1e-12)
  expect_error(Irfun(B, S, shock_var = 2), "shock_var")
  expect_error(Irfun(B, S[1, , drop = FALSE]), "m by m")
})

test_that("Irlsfn is one weighted least-squares step with Huber next weights", {
  set.seed(5)
  x <- rnorm(20)
  y <- 1 + 2 * x + rnorm(20)
  y[3] <- 15
  w <- runif(20, 0.5, 1.5)
  r <- Irlsfn(y, x, w)
  f <- lm(y ~ x, weights = w)
  expect_equal(r$coef, unname(coef(f)), tolerance = 1e-9)
  expect_equal(r$se_coef, unname(summary(f)$coefficients[, 2]), tolerance = 1e-9)
  res <- y - fitted(f)
  s <- mad(res)
  expect_equal(r$scale, s, tolerance = 1e-9)
  expect_equal(r$next_weights, unname(pmin(1, 1.345 / abs(res / s))), tolerance = 1e-9)
  expect_error(Irlsfn(y[1:2], cbind(x, x)[1:2, ], NULL), "more observations")
})

test_that("the IRT response models", {
  y <- c(1, 0, 1, 1)
  th <- c(-1, 0.2, 1.5, 0.3)
  r1 <- Irt1pl(y, th, 0.4)
  p1 <- plogis(th - 0.4)
  expect_equal(r1$p, p1, tolerance = 1e-12)
  expect_equal(r1$loglik, sum(dbinom(y, 1, p1, log = TRUE)), tolerance = 1e-12)
  expect_equal(r1$information, p1 * (1 - p1), tolerance = 1e-12)
  r4 <- Irt4pl(y, th, 1.3, 0.2, c = 0.1, d = 0.95)
  p4 <- 0.1 + 0.85 * plogis(1.3 * (th - 0.2))
  expect_equal(r4$p, p4, tolerance = 1e-12)
  expect_equal(r4$loglik, sum(dbinom(y, 1, p4, log = TRUE)), tolerance = 1e-12)
  expect_error(Irt4pl(y, th, 1, 0, c = 0.5, d = 0.4), "c < d")
  expect_error(Irt1pl(c(2, 0, 1, 1), th, 0), "0 or 1")
  yy <- c(0, 2, 1)
  tt <- c(-0.3, 1.2, 0.4)
  expect_equal(Irtgpc(yy, tt, 1.1, c(-0.5, 0.3, 1))$loglik, Gpcm(yy, tt, 1.1, c(-0.5, 0.3, 1))$loglik)
  expect_equal(Irtgrm(yy, tt, 1.1, c(-0.5, 0.8))$p_observed, Grmsam(yy, tt, 1.1, c(-0.5, 0.8))$p_observed)
  pc <- Irtprc(yy, tt, c(-0.4, 0.9))
  P <- t(vapply(tt, function(t) {
    z <- cumsum(t - c(0, -0.4, 0.9))
    exp(z) / sum(exp(z))
  }, numeric(3)))
  expect_equal(pc$p_observed, P[cbind(1:3, yy + 1)], tolerance = 1e-12)
  expect_error(Irtprc(3, 0, c(1, 2)), "category range")
})

test_that("Irtmh1 matches stats::mantelhaen.test with continuity correction", {
  set.seed(6)
  n <- 120
  g <- rep(0:1, each = 60)
  s <- sample(1:4, n, TRUE)
  X <- cbind(rbinom(n, 1, plogis(s - 2.5 + 0.8 * g)), rbinom(n, 1, plogis(s - 2.5)))
  r <- Irtmh1(X, g, s)
  for (j in 1:2) {
    tb <- table(factor(g, 0:1), factor(X[, j], c(1, 0)), s)
    mh <- mantelhaen.test(tb, correct = TRUE)
    expect_equal(r$odds_ratio[j], unname(mh$estimate), tolerance = 1e-12)
    expect_equal(r$chisq[j], unname(mh$statistic), tolerance = 1e-12)
    expect_equal(r$p_value[j], mh$p.value, tolerance = 1e-9)
    expect_equal(r$delta[j], -2.35 * log(unname(mh$estimate)), tolerance = 1e-12)
  }
  expect_equal(r$flagged, as.integer(r$ets_class != "A"))
  expect_error(Irtmh1(X, g + 1, s), "0/1")
})

test_that("Ibp3par sums the expected new features of the three-parameter IBP", {
  r <- Ibp3par(6, sigma = 0.3, alpha = 2, c = 1.5)
  nd <- vapply(1:6, function(i) 2 * gamma(2.5) * gamma(i - 1 + 1.8) / (gamma(i + 1.5) * gamma(1.8)), 0)
  expect_equal(r$new_dishes, nd, tolerance = 1e-12)
  expect_equal(r$K_path, cumsum(nd), tolerance = 1e-12)
  expect_equal(r$one_param, sum(2 * 1.5 / (0:5 + 1.5)), tolerance = 1e-12)
  expect_equal(Ibp3par(c(0.1, 0.2, 0.3), sigma = 0, alpha = 1, c = 1)$K_n, sum(1 / 1:3), tolerance = 1e-12)
})
