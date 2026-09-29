# Coverage for qbcfgs .. quanrg exports. Every expectation is recomputed in
# the test body.

test_that("quantile-balanced strata, SMD and the stratified weighting", {
  e <- c(0.3, 0.1, 0.8, 0.5, 0.2, 0.9, 0.4, 0.6)
  s <- morie_qbcfgs_strata(e, 3)
  expect_equal(s[order(e)], floor((0:7) * 3 / 8))
  x <- c(1.2, 0.5, 2.3, 1.8, 0.9, 2.7, 1.1, 2.0)
  d <- c(0, 0, 1, 1, 0, 1, 0, 1)
  w <- c(1, 2, 1, 0.5, 1, 2, 1, 1)
  sm <- function(x, d, w) {
    m1 <- stats::weighted.mean(x[d == 1], w[d == 1])
    m0 <- stats::weighted.mean(x[d == 0], w[d == 0])
    v1 <- sum(w[d == 1] * (x[d == 1] - m1)^2) / sum(w[d == 1])
    v0 <- sum(w[d == 0] * (x[d == 0] - m0)^2) / sum(w[d == 0])
    (m1 - m0) / sqrt((v1 + v0) / 2)
  }
  expect_equal(morie_qbcfgs_smd(x, d, w), sm(x, d, w), tolerance = 1e-12)
  expect_equal(morie_qbcfgs_smd(x, d), sm(x, d, rep(1, 8)), tolerance = 1e-12)
  expect_true(is.nan(morie_qbcfgs_smd(x, rep(1, 8))))
  expect_identical(morie_qbcfgs_smd(c(1, 1, 2, 2), c(1, 0, 1, 0)), 0)
  n <- 24
  X <- cbind(seq(0, 1, length.out = n), rep(c(0.2, 0.8, 0.5), 8))
  D <- rep(c(1, 0, 0, 1, 1, 0), 4)
  y <- 1 + 2 * D + X[, 1] + c(0.1, -0.1, 0.05, 0, 0.2, -0.05)
  r <- morie_qbcfgs(y, D, X, n_strata = 3, seed = 4)
  ep <- r$propensity
  expect_true(all(ep >= 0.01 & ep <= 0.99))
  st <- morie_qbcfgs_strata(ep, 3)
  expect_equal(r$stratum, st)
  raw <- ifelse(D == 1, 1 / ep, 1 / (1 - ep))
  ww <- raw * ave(raw, st, FUN = function(v) length(v) / sum(v))
  expect_equal(r$weight_value, ww, tolerance = 1e-12)
  eff <- vapply(0:2, function(k) {
    i <- st == k
    if (!any(i & D == 1) || !any(i & D == 0)) return(NaN)
    stats::weighted.mean(y[i & D == 1], ww[i & D == 1]) - stats::weighted.mean(y[i & D == 0], ww[i & D == 0])
  }, 0)
  expect_equal(r$stratum_effect, eff, tolerance = 1e-12)
  live <- !is.nan(eff)
  sz <- tabulate(st + 1, 3)
  expect_equal(r$estimate, sum(eff[live] * sz[live]) / sum(sz[live]), tolerance = 1e-12)
  expect_equal(r$smd_after, c(sm(X[, 1], D, ww), sm(X[, 2], D, ww)), tolerance = 1e-12)
  expect_equal(r$focal_stratum, 1L)
  ra <- morie_qbcfgs(y, D, X, n_strata = 3, seed = 4, weight = "att")
  rw <- ifelse(D == 1, 1, ra$propensity / (1 - ra$propensity))
  expect_equal(ra$weight_value, rw * ave(rw, ra$stratum, FUN = function(v) length(v) / sum(v)),
               tolerance = 1e-12)
  expect_match(morie_qbcfgs_cheatsheet(), "ate, att")
  expect_error(morie_qbcfgs(y, D, X, weight = "atc"), "weight must be one of")
  expect_error(morie_qbcfgs(y, D, X, n_strata = 1), "two strata")
  expect_error(morie_qbcfgs(y[1:10], D[1:10], X[1:10, ], n_strata = 3), "four observations per stratum")
  expect_error(morie_qbcfgs(y, D, X, quantile = 1), "strictly inside")
  expect_error(morie_qbcfgs(y, D, X, clip = 0.5), "clip must lie")
})

test_that("Achlioptas projections: dimension bound, coin matrix and embedding", {
  td <- target_dimension(100, 0.2, beta = 1.5)
  k0 <- (4 + 2 * 1.5) * log(100) / (0.2^2 / 2 - 0.2^3 / 3)
  expect_equal(td$k0, k0, tolerance = 1e-12)
  expect_equal(td$k, as.integer(ceiling(k0)))
  expect_equal(td$failure_probability, 100^-1.5, tolerance = 1e-12)
  expect_error(target_dimension(1, 0.2), "two points")
  expect_error(target_dimension(10, 1), "epsilon must lie")
  expect_error(target_dimension(10, 0.5, beta = 0), "beta must be positive")
  e <- .ghc_rng(9)
  u <- .ghc_unif(e, 12L)
  R <- projection_matrix(3, 4, seed = 9)
  expect_equal(do.call(rbind, R), matrix(ifelse(u < 0.5, 1, -1), 3, 4, byrow = TRUE))
  Rs <- projection_matrix(3, 4, distribution = "sparse", seed = 9)
  expect_equal(do.call(rbind, Rs), matrix(ifelse(u < 1 / 6, sqrt(3), ifelse(u > 5 / 6, -sqrt(3), 0)), 3, 4,
                                          byrow = TRUE))
  A <- list(c(1, 2, 3), c(-1, 0, 2), c(0.5, -1, 1))
  r <- morie_qjlcrn(A, 4, seed = 9)
  Am <- do.call(rbind, A)
  Rm <- do.call(rbind, R)
  expect_equal(do.call(rbind, r$embedding), Am %*% Rm / 2, tolerance = 1e-12)
  expect_equal(r$nonzero_fraction, 1)
  expect_same_function(johnson_lindenstrauss, morie_qjlcrn)
  expect_error(projection_matrix(3, 4, distribution = "gauss"), "distribution must be one of")
  expect_error(projection_matrix(0, 4), "dimensions must be positive")
  expect_error(morie_qjlcrn(list(c(1, 2), 1), 2), "every point needs")
  expect_error(morie_qjlcrn(list(), 2), "no points")
})

test_that("tabular Q-learning replays exactly on a deterministic chain", {
  # states 0..3, action 0 = left, 1 = right, state 3 terminal with reward 1 on entry
  S <- 4
  Pl <- matrix(0, S, S)
  Pr <- matrix(0, S, S)
  for (s in 1:S) {
    Pl[s, max(s - 1, 1)] <- 1
    Pr[s, min(s + 1, S)] <- 1
  }
  R <- matrix(0, S, 2)
  R[3, 2] <- 1
  Q <- matrix(0, S, 2)
  steps <- 0
  for (ep in 1:6) {
    s <- 1
    for (k in 1:20) {
      if (s == 4) break
      a <- if (Q[s, 2] > Q[s, 1]) 2 else 1
      s2 <- if (a == 1) max(s - 1, 1) else min(s + 1, S)
      nxt <- if (s2 == 4) 0 else max(Q[s2, ])
      Q[s, a] <- Q[s, a] + 0.5 * (R[s, a] + 0.9 * nxt - Q[s, a])
      steps <- steps + 1
      s <- s2
    }
  }
  r1 <- morie_qlearn(list(Pl, Pr), R, 0.9, alpha = 0.5, epsilon = 0, n_episodes = 6, terminal = 3,
                     max_steps = 20)
  r2 <- Qlearn(list(Pl, Pr), R, 0.9, alpha = 0.5, epsilon = 0, n_episodes = 6, terminal = 3,
               max_steps = 20)
  expect_equal(r1$estimate, Q, tolerance = 1e-12)
  expect_equal(r2$estimate, Q, tolerance = 1e-12)
  expect_equal(r1$n_steps, steps)
  expect_equal(r1$policy, apply(Q, 1, function(q) if (q[2] > q[1]) 1 else 0))
  expect_equal(r2$v, apply(Q, 1, max), tolerance = 1e-12)
  # exploration consumes the stream identically in both implementations
  r3 <- morie_qlearn(list(Pl, Pr), R, 0.9, epsilon = 0.4, n_episodes = 5, terminal = 3, seed = 3)
  r4 <- Qlearn(list(Pl, Pr), R, 0.9, epsilon = 0.4, n_episodes = 5, terminal = 3, seed = 3)
  expect_equal(r3$estimate, r4$estimate, tolerance = 1e-12)
  expect_error(morie_qlearn(list(Pl, Pr), R, 0.9, start = 5), "start out of range")
  expect_error(Qlearn(list(Pl, Pr), R, 0.9, start = -1), "start out of range")
  expect_error(Qlearn(list(Pl * 2, Pr), R, 0.9), "does not sum to 1")
})

test_that("morie_qlrtst is the Quandt-Andrews sup-Wald statistic", {
  y <- c(1.0, 1.2, 0.9, 1.1, 1.3, 0.8, 1.0, 1.1, 2.9, 3.1, 3.0, 2.8, 3.2, 3.1, 2.9, 3.0, 3.3, 2.7, 3.0, 3.1)
  n <- 20
  r <- morie_qlrtst(y, trim = 0.15)
  ssr <- function(v) sum((v - mean(v))^2)
  s0 <- ssr(y)
  lo <- max(2, ceiling(0.15 * n))
  hi <- min(n - 2, floor(0.85 * n))
  W <- vapply(lo:hi, function(k) {
    s1 <- ssr(y[1:k]) + ssr(y[(k + 1):n])
    (n - 2) * (s0 - s1) / s1
  }, 0)
  expect_equal(r$f_path, W, tolerance = 1e-10)
  expect_equal(r$statistic, max(W), tolerance = 1e-10)
  expect_equal(r$breakpoint, (lo:hi)[which.max(W)])
  # Hansen (1997) Table 2, m = 1, trim 0.15: (-0.99, 1.02, 3.0)
  z <- -0.99 + 1.02 * max(W)
  expect_equal(r$p_value, stats::pchisq(z, 3, lower.tail = FALSE), tolerance = 1e-10)
  x <- seq_len(n)
  rx <- morie_qlrtst(y, X = x, trim = 0.25)
  ssr2 <- function(i) sum(stats::residuals(stats::lm(y[i] ~ x[i]))^2)
  k0 <- rx$breakpoint
  s1 <- ssr2(1:k0) + ssr2((k0 + 1):n)
  expect_equal(rx$statistic, (n - 4) * (ssr2(1:n) - s1) / s1, tolerance = 1e-9)
  expect_error(morie_qlrtst(y, trim = 0.2), "trim must be one of")
  expect_error(morie_qlrtst(y[1:3], trim = 0.35), "too short")
  expect_error(morie_qlrtst(y, X = x[-1]), "one row per observation")
})

test_that("morie_qmDS maps quantiles through the piecewise-linear CDFs", {
  obs <- c(2.1, 3.5, 1.2, 4.8, 2.9, 3.3)
  mod <- c(1.0, 2.2, 1.7, 3.1, 2.5)
  x <- c(0.5, 1.5, 2.3, 2.9, 3.5)
  r <- morie_qmDS(x, obs, mod)
  pr <- stats::approx(sort(mod), (0:4) / 4, x, rule = 2)$y
  expect_equal(r$probs, pr, tolerance = 1e-12)
  expect_equal(r$estimate, stats::quantile(obs, pr, type = 7, names = FALSE), tolerance = 1e-12)
  # a pure shift is recovered and self-mapping is the identity
  expect_equal(morie_qmDS(mod, mod + 1, mod)$estimate, mod + 1, tolerance = 1e-12)
  expect_equal(morie_qmDS(3, obs, 7)$probs, 0.5)
  expect_error(morie_qmDS(x, numeric(0), mod), "non-empty")
})

test_that("pinball losses and their quantile minimiser", {
  y <- c(1.2, 3.4, 2.2, 5.0, 0.8)
  p <- c(1.0, 3.0, 2.5, 4.0, 1.5)
  u <- y - p
  r <- morie_qrF(y, p, theta = 0.3)
  expect_equal(r$losses, pmax(0.3 * u, (0.3 - 1) * u), tolerance = 1e-12)
  expect_equal(r$estimate, mean(pmax(0.3 * u, -0.7 * u)), tolerance = 1e-12)
  expect_same_function(morie_qrf, morie_qrF)
  q <- Qrf(y, p, 0.3)
  expect_equal(q$scores, r$losses, tolerance = 1e-12)
  # a scalar forecast: the tau-quantile of the sample minimises the mean score
  grid <- seq(0, 6, by = 0.01)
  best <- grid[which.min(vapply(grid, function(g) Qrf(y, g, 0.3)$estimate, 0))]
  expect_equal(best, sort(y)[2], tolerance = 1e-12)
  expect_error(morie_qrF(y, p, theta = 1), "theta must be in")
  expect_error(morie_qrF(y, p[-1]), "paired")
  expect_error(Qrf(y, p, 0), "tau must be in")
  expect_error(Qrf(y, p[-1], 0.5), "equal length")
})

test_that("qsarh fits the parabolic Hansch equation", {
  lp <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)
  sg <- c(0.1, -0.2, 0.3, 0.0, 0.2, -0.1, 0.4, 0.1)
  y <- 4 + 1.6 * lp - 0.4 * lp^2 + 0.8 * sg + c(0.05, -0.03, 0.02, 0, -0.04, 0.03, -0.01, 0.02)
  r <- qsarh(y, lp, sigma = sg)
  f <- stats::lm(y ~ lp + I(lp^2) + sg)
  expect_equal(as.numeric(r$coefficients), unname(stats::coef(f)), tolerance = 1e-9)
  expect_equal(r$r2, summary(f)$r.squared, tolerance = 1e-10)
  b <- stats::coef(f)
  expect_equal(r$logp0, unname(b[2] / (2 * -b[3])), tolerance = 1e-9)
  expect_equal(r$rho, unname(b[4]), tolerance = 1e-9)
  lin <- qsarh(y, lp, parabolic = FALSE)
  expect_true(is.na(lin$logp0))
  expect_equal(lin$names, c("k", "logP"))
  expect_same_function(morie_hansch_qsar, qsarh)
})

test_that("morie_quanrg finds the exact regression quantile", {
  x <- c(0.3, 1.2, 2.5, 3.1, 4.0, 4.4, 5.9, 6.2, 7.5)
  y <- c(1.4, 1.2, 2.9, 2.0, 3.5, 3.1, 4.4, 3.2, 5.1)
  r <- morie_quanrg(y, x, theta = 0.35)
  loss <- function(b) {
    e <- y - b[1] - b[2] * x
    sum(pmax(0.35 * e, -0.65 * e))
  }
  expect_equal(r$objective, loss(r$coefficients), tolerance = 1e-12)
  expect_equal(r$n_bases_checked, choose(9, 2))
  skip_if_not_installed("quantreg")
  q <- quantreg::rq(y ~ x, tau = 0.35)
  expect_equal(r$objective, loss(stats::coef(q)), tolerance = 1e-10)
  expect_equal(morie_quanrg(y)$coefficients, sort(y)[5], tolerance = 1e-12)
  expect_error(morie_quanrg(y, x, theta = 0), "theta must be in")
  expect_error(morie_quanrg(y, cbind(x, x^2, x^3)), "at most 3")
  expect_error(morie_quanrg(y[1:2], x[1:2]), "more observations")
})
