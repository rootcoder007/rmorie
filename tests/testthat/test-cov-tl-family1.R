# Coverage for timesf_native .. tlsieve_native exports. Every expectation
# is recomputed in the test body.

test_that("morie_timesf rolls a patched predictor forward", {
  pred <- function(pat) {
    last <- pat[[length(pat)]]
    c(mean(last), last[length(last)] + 1)
  }
  h <- c(1, 4, 2, 6, 3)
  r <- morie_timesf(h, pred, horizon = 5, input_patch_len = 2, output_patch_len = 2)
  ctx <- h
  out <- numeric(0)
  for (s in 1:3) {
    pad <- (2 - length(ctx) %% 2) %% 2
    v <- c(rep(0, pad), ctx)
    last <- v[(length(v) - 1):length(v)]
    nx <- c(mean(last), last[2] + 1)
    out <- c(out, nx)
    ctx <- c(ctx, nx)
  }
  expect_equal(r$forecast, out[1:5], tolerance = 1e-12)
  expect_equal(r$steps, 3L)
  expect_equal(r$context_grew_to, 11L)
  expect_error(morie_timesf(h, function(p) 1, 3, 2, 2), "returned 1 values")
  expect_error(morie_timesf(h, pred, 0, 2, 2), "horizon must be at least 1")
})

tip_data <- function() {
  list(y = c(5.1, 6.3, NA, 7.0, 4.8, NA, 6.9, 5.5, NA, 7.4, 4.2, 6.1),
       D = c(1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0))
}

test_that("morie_tipsne tips a deterministic delta-adjusted ANCOVA", {
  d <- tip_data()
  obs <- !is.na(d$y)
  f <- stats::lm(d$y[obs] ~ d$D[obs])
  mu <- stats::coef(f)[1] + stats::coef(f)[2] * d$D
  dts <- c(0, -1, -2, -3, -4)
  pv <- vapply(dts, function(dt) {
    comp <- ifelse(obs, d$y, mu + ifelse(d$D == 1, dt, 0))
    s <- summary(stats::lm(comp ~ d$D))$coefficients
    2 * stats::pt(-abs(s[2, 3]), 10)
  }, 0)
  r <- morie_tipsne(d$y, d$D, delta_treat = dts, mi = "deterministic")
  expect_equal(vapply(r$grid, function(g) g$p, 0), pv, tolerance = 1e-10)
  comp0 <- ifelse(obs, d$y, mu)
  s0 <- summary(stats::lm(comp0 ~ d$D))$coefficients
  expect_equal(r$estimate, s0[2, 1], tolerance = 1e-10)
  expect_equal(r$se, s0[2, 2], tolerance = 1e-10)
  expect_equal(r$df, 10)
  k <- which(diff(pv < 0.05) != 0)[1]
  if (!is.na(k)) {
    tp <- dts[k] + (0.05 - pv[k]) / (pv[k + 1] - pv[k]) * (dts[k + 1] - dts[k])
    expect_equal(r$tipping_points[[1]]$tipping_point, tp, tolerance = 1e-10)
    expect_true(r$tipped)
  }
  expect_equal(r$pooled_sd, stats::sd(d$y[obs]), tolerance = 1e-12)
  expect_equal(r$n_missing_treat, 1L)
  expect_error(morie_tipsne(d$y, d$D, mi = "hotdeck"), "mi must be one of")
  expect_error(morie_tipsne(d$y, d$D, pooling = "mean"), "pooling must be one of")
  expect_error(morie_tipsne(d$y, d$D, missing_indicator = rep(0, 12)), "observed where y is missing")
  expect_match(morie_tipsne_cheatsheet(), "deterministic")
})

test_that("morie_tipsne pools improper imputations by Rubin's rules", {
  d <- tip_data()
  obs <- !is.na(d$y)
  f <- stats::lm(d$y[obs] ~ d$D[obs])
  mu <- stats::coef(f)[1] + stats::coef(f)[2] * d$D
  sdv <- summary(f)$sigma
  e <- .ghc_rng(5)
  est <- vv <- numeric(3)
  for (m in 1:3) {
    comp <- d$y
    for (i in which(!obs)) comp[i] <- mu[i] + sdv * .ghc_norm(e, 1L)
    s <- summary(stats::lm(comp ~ d$D))$coefficients
    est[m] <- s[2, 1]
    vv[m] <- s[2, 2]^2
  }
  qb <- mean(est)
  B <- stats::var(est)
  Tt <- mean(vv) + 4 / 3 * B
  rr <- 4 / 3 * B / mean(vv)
  df <- 2 * (1 + 1 / rr)^2
  r <- morie_tipsne(d$y, d$D, delta_treat = 0, mi = "improper", n_imputations = 3, seed = 5)
  expect_equal(r$estimate, qb, tolerance = 1e-10)
  expect_equal(r$se, sqrt(Tt), tolerance = 1e-10)
  expect_equal(r$df, df, tolerance = 1e-10)
  # the incomplete-beta t tail loses a few digits at df ~ 1e5
  expect_equal(r$p, 2 * stats::pt(-abs(qb / sqrt(Tt)), df), tolerance = 1e-8)
  br <- morie_tipsne(d$y, d$D, delta_treat = 0, mi = "improper", n_imputations = 3, seed = 5,
                     pooling = "barnard_rubin")
  g <- 4 / 3 * B / Tt
  dfo <- 11 / 13 * 10 * (1 - g)
  expect_equal(br$df, 1 / (1 / df + 1 / dfo), tolerance = 1e-10)
  pr <- morie_tipsne(d$y, d$D, delta_treat = 0, n_imputations = 3, seed = 5)
  expect_equal(pr$m, 3L)
  expect_gt(pr$mar$between, 0)
})

test_that("morie_tlbandt replays its adaptive design", {
  W <- matrix(c(0.1, 0.5, 0.9, 0.3, 0.7, 0.2), ncol = 1)
  y1 <- c(1, 0, 1, 1, 0, 1)
  y0 <- c(0, 1, 0, 0, 1, 0)
  blip <- function(h) if (length(h) %% 2) 0.4 else -0.2
  r <- morie_tlbandt(W, y1, y0, blip, delta = 0.2, seed = 3, burn_in = 2)
  u <- .ghc_unif(.ghc_rng(3), 6)
  g <- c(0.5, 0.5, vapply(3:6, function(t) if ((t - 1) %% 2) 0.8 else 0.2, 0))
  A <- as.numeric(u < g)
  expect_equal(r$g, g)
  expect_equal(r$A, A)
  expect_equal(r$Y, ifelse(A == 1, y1, y0))
  gr <- morie_tlbandt(W, y1, y0, blip, greedy = TRUE, seed = 3, burn_in = 2)
  expect_equal(gr$g[3:6], c(0, 1, 0, 1))
  expect_error(morie_tlbandt(W, y1[-1], y0, blip), "differ in length")
  expect_error(morie_tlbandt(W, y1, y0, blip, delta = 0.6, burn_in = 0), "delta must lie")
})

test_that("Tldepu and Tldepl are empirical chi(u)", {
  x <- c(0.3, 1.2, -0.5, 2.2, 0.8, -1.1, 1.7, 0.1, 2.9, -0.2)
  y <- c(0.1, 1.5, -0.2, 2.5, 0.4, -0.9, 1.1, 0.6, 3.1, -0.6)
  chi <- function(a, b, u) {
    ra <- rank(a) / 11
    rb <- rank(b) / 11
    j <- min(max(mean(ra < u & rb < u), 1 / 20), 1 - 1 / 20)
    min(max(2 - log(j) / log(u), 0), 1)
  }
  expect_equal(Tldepu(x, y, 0.8)$estimate, chi(x, y, 0.8), tolerance = 1e-12)
  expect_equal(Tldepl(x, y, 0.8)$estimate, chi(-x, -y, 0.8), tolerance = 1e-12)
  expect_match(Tldepl(x, y)$method, "lower tail")
  expect_error(Tldepu(x[1:3], y[1:3]), "n >= 4")
})

test_that("morie_tlhoest adds the second-order U-statistic", {
  O <- c(0.4, 1.1, -0.3, 0.9)
  D1 <- c(0.2, -0.1, 0.05, 0.15)
  k <- function(a, b) a * b - 0.1
  r <- morie_tlhoest(1.5, D1, k, O)
  pair <- outer(O, O) - 0.1
  so <- (sum(pair) - sum(diag(pair))) / 12
  expect_equal(r$first_order, 1.5 + mean(D1), tolerance = 1e-12)
  expect_equal(r$second_order_correction, so, tolerance = 1e-12)
  expect_equal(r$estimate, 1.5 + mean(D1) + so, tolerance = 1e-12)
  expect_equal(r$n_pairs, 12L)
  expect_error(morie_tlhoest(1, 0.1, k, O), "at least 2 observations")
  q <- morie_tlhoest_rate_requirement(3, n = 400)
  expect_equal(q$required_rate_per_nuisance, 0.125)
  expect_equal(q$error_at_that_rate, 400^-0.125, tolerance = 1e-12)
  expect_error(morie_tlhoest_rate_requirement(0), "at least 1")
  expect_equal(morie_tlhoest_remainder_order(2)$remainder_order, 3L)
  expect_match(morie_tlhoest_cheatsheet(), "THIRD")
})

test_that("morie_tloilr and morie_tlsieve", {
  q1 <- c(0.6, 0.4, 0.9, 0.3, 0.8)
  q0 <- c(0.5, 0.45, 0.2, 0.25, 0.1)
  r <- morie_tloilr(q1, q0, kappa = 0.4)
  B <- q1 - q0
  tau <- max(0, sort(B)[ceiling(0.6 * 5)])
  rule <- B > tau
  expect_equal(r$tau, tau, tolerance = 1e-12)
  expect_equal(r$value, mean(ifelse(rule, q1, q0)), tolerance = 1e-12)
  expect_equal(r$unconstrained_value, mean(pmax(q1, q0)), tolerance = 1e-12)
  expect_equal(r$treated_fraction, mean(rule))
  expect_true(r$binding)
  un <- morie_tloilr(q1, q0, kappa = 1)
  expect_equal(un$tau, 0)
  expect_equal(un$cost_of_constraint, 0, tolerance = 1e-12)
  expect_error(morie_tloilr(q1, q0, 0), "kappa must lie")
  s <- morie_tlsieve(c(0.01, 0.03), c(0.02, 0.05), c(0.015, 0.04), c(0.02, 0))
  expect_equal(s$ve_matched, 1 - c(0.01, 0.03) / c(0.02, 0.05), tolerance = 1e-12)
  expect_true(is.nan(s$ve_mismatched[2]))
  expect_equal(s$sieve_effect[1], (1 - 0.5) - (1 - 0.75), tolerance = 1e-12)
  expect_error(morie_tlsieve(1, 1, c(1, 2), c(1, 2)), "differ in length")
})
