# Coverage for the LLM model list and probe cache, LMC covariances, the
# log-normal frailty Cox model, local-shift sensitivity, local DP,
# local polynomial regression, Huber Proposal 2, LOWESS, odds ratios,
# PSIS weights and PPS sampling; recomputed with base R, MASS and stats.

test_that("morie_llm_list_freeapi_models falls back offline and morie_llm_probe_freeapi honours its cache", {
  m <- morie_llm_list_freeapi_models()
  expect_true(all(c("model", "family", "size", "label", "alias") %in% names(m)))
  expect_true(nrow(m) >= 1)
  expect_false(any(duplicated(m$alias)))
  old <- options(morie.llm.freeapi_cached = TRUE)
  on.exit(options(old), add = TRUE)
  expect_true(morie_llm_probe_freeapi())
  options(morie.llm.freeapi_cached = FALSE)
  expect_false(morie_llm_probe_freeapi())
})

test_that("LmcCovariance sums B_s rho_s(h)", {
  B1 <- matrix(c(1, 0.6, 0.6, 1.5), 2)
  B2 <- matrix(c(0.3, 0.1, 0.1, 0.2), 2)
  lmc <- list(list(model = "Exp", range = 2, B = B1), list(model = "Sph", range = 3, B = B2),
              list(model = "Nug", B = diag(c(0.05, 0.02))))
  h <- 1.2
  expect_equal(LmcCovariance(h, lmc), B1 * exp(-0.6) + B2 * (1 - 1.5 * 0.4 + 0.5 * 0.4^3), tolerance = 1e-12)
  expect_equal(LmcCovariance(0, lmc), B1 + B2 + diag(c(0.05, 0.02)), tolerance = 1e-12)
  expect_equal(LmcCovariance(-h, lmc[1]), B1 * exp(-0.6), tolerance = 1e-12)
  expect_error(LmcCovariance(1, list(list(model = "Exp", B = matrix(c(1, 0, 1, 1), 2)))), "symmetric")
})

test_that("Lnfrm reaches its penalised-likelihood and REML fixed points", {
  set.seed(1)
  q <- 8
  cl <- rep(1:q, each = 8)
  b <- rnorm(q, 0, 1.2)
  x <- rnorm(64)
  t <- rexp(64, exp(0.7 * x + b[cl]))
  e <- rbinom(64, 1, 0.85)
  r <- Lnfrm(t, e, x, cl)
  expect_lt(r$n_outer, 50)
  Z <- cbind(x, outer(cl, 1:q, "==") * 1)
  th <- c(r$estimate, unlist(r$frailty))
  eta <- as.numeric(Z %*% th)
  U <- numeric(1 + q)
  I <- matrix(0, 1 + q, 1 + q)
  for (tk in sort(unique(t[e == 1]))) {
    D <- which(t == tk & e == 1)
    R <- which(t >= tk)
    w <- exp(eta[R])
    xb <- colSums(Z[R, , drop = FALSE] * w) / sum(w)
    U <- U + colSums(Z[D, , drop = FALSE]) - length(D) * xb
    I <- I + length(D) * (crossprod(Z[R, , drop = FALSE] * w, Z[R, , drop = FALSE]) / sum(w) - tcrossprod(xb))
  }
  P <- diag(c(0, rep(1 / r$sigma2, q)))
  expect_lt(max(abs(U - P %*% th)), 1e-8)
  cv <- solve(I + P)
  expect_equal(r$se, sqrt(cv[1, 1]), tolerance = 1e-8)
  s2 <- (sum(th[-1]^2) + sum(diag(cv)[-1])) / q
  # the outer REML update stops once sigma2 moves by less than 1e-7 relative
  expect_equal(r$sigma2, s2, tolerance = 1e-6)
  expect_error(Lnfrm(t, e, x, rep(1, 36)), "at least 2 clusters")
})

test_that("localS and morie_local_shift take the largest IF slope", {
  IF <- c(0, 0.5, 2.5, 2.7, 3.0)
  x <- c(0, 1, 2, 4, 5)
  r <- localS(IF, x)
  sl <- outer(1:5, 1:5, Vectorize(function(i, j) if (j > i) abs(IF[j] - IF[i]) / abs(x[j] - x[i]) else -Inf))
  expect_equal(r$lambda_star, max(sl), tolerance = 1e-12)
  expect_equal(c(r$i, r$j), as.integer(which(sl == max(sl), arr.ind = TRUE)[1, ] - 1))
  expect_equal(morie_local_shift(IF)$lambda_star, max(abs(diff(IF))), tolerance = 1e-12)
  expect_true(is.na(localS(1)$estimate))
})

test_that("morie_locdp and Locdp apply randomized response", {
  x <- c(1, 0, 1, 1, 0, 0, 1, 0)
  r <- morie_locdp(x, epsilon = 0.8, seed = 5)
  p <- 1 / (1 + exp(0.8))
  u <- .ghc_unif(.ghc_rng(5), 8)
  expect_equal(r$y, ifelse(u < p, 1 - x, x))
  expect_equal(r$p, p, tolerance = 1e-12)
  expect_equal(morie_locdp(c(TRUE, FALSE), 1)$n, 2L)
  expect_error(morie_locdp(x, -1), "non-negative")
  expect_match(morie_locdp_cheatsheet(), "randomized response")
  lc <- Locdp(x, epsilon = 1.2)
  ref <- Rrand(x, epsilon = 1.2)
  expect_equal(lc$released, ref$released)
  q <- 1 / (1 + exp(1.2))
  expect_equal(lc$estimate, (mean(lc$released) - q) / (1 - 2 * q), tolerance = 1e-12)
})

test_that("morie_locp is kernel-weighted local polynomial least squares", {
  x <- c(0.1, 0.4, 0.5, 0.9, 1.3, 1.6, 2.0)
  y <- sin(x) + c(0.02, -0.01, 0.03, 0, -0.02, 0.01, 0.02)
  r <- morie_locp(x, y, x0 = c(0.5, 1.5), degree = 2, bandwidth = 1)
  for (k in 1:2) {
    w <- pmax(1 - abs(x - r$x0[k])^3, 0)^3 * (abs(x - r$x0[k]) < 1)
    f <- lm(y ~ I(x - r$x0[k]) + I((x - r$x0[k])^2), weights = w)
    expect_equal(r$fitted[k], unname(coef(f)[1]), tolerance = 1e-9)
    expect_equal(r$slope[k], unname(coef(f)[2]), tolerance = 1e-9)
  }
  g <- morie_locp(x, y, x0 = 1, degree = 0, bandwidth = 0.5, kernel = "gaussian")
  w <- exp(-0.5 * ((x - 1) / 0.5)^2)
  expect_equal(g$fitted, sum(w * y) / sum(w), tolerance = 1e-12)
  expect_true(is.nan(g$slope))
  expect_error(morie_locp(x, y, kernel = "box"), "kernel must be")
})

test_that("locS and morie_locS iterate Huber's Proposal 2", {
  x <- c(2.1, 1.8, 2.5, 2.2, 1.9, 7.5, 2.0, 2.3, -3, 2.4)
  r <- locS(x, k = 1.5)
  # same start (median, mad), update and stopping rule as MASS::hubers at tol 1e-6 and 30 sweeps
  h <- MASS::hubers(x, k = 1.5)
  expect_equal(c(r$estimate, r$scale), c(h$mu, h$s), tolerance = 1e-12)
  yy <- pmin(pmax(r$mu_refined - 1.5 * r$scale_refined, x), r$mu_refined + 1.5 * r$scale_refined)
  expect_equal(r$mu_refined, mean(yy), tolerance = 1e-12)
  th <- 2 * pnorm(1.5) - 1
  expect_equal(r$beta, th + 2.25 * (1 - th) - 3 * dnorm(1.5), tolerance = 1e-12)
  expect_equal(morie_locS(rep(1, 5))$scale, 0)
})

test_that("Loess and morie_loess reproduce stats::lowess", {
  set.seed(2)
  x <- sort(runif(30, 0, 10))
  y <- sin(x) + rnorm(30, sd = 0.3)
  y[c(5, 20)] <- y[c(5, 20)] + 3
  r <- Loess(x, y, span = 0.5, iterations = 3)
  lw <- lowess(x, y, f = 0.5, iter = 3, delta = 0)
  expect_equal(r$fitted, lw$y, tolerance = 1e-12)
  expect_equal(morie_loess(rev(x), rev(y), span = 0.4, iterations = 0)$fitted,
               lowess(x, y, f = 0.4, iter = 0, delta = 0)$y, tolerance = 1e-12)
})

test_that("Logor scales the log odds ratio", {
  r <- Logor(0.3, a = 1, b = 4, se = 0.1, level = 0.9)
  expect_equal(r$or, exp(0.9), tolerance = 1e-12)
  expect_equal(c(r$ci_low, r$ci_high), exp(0.9 + c(-1, 1) * qnorm(0.95) * 0.3), tolerance = 1e-12)
  expect_null(Logor(0.3)$se_logor)
  expect_error(Logor(0.3, se = -1), "positive")
})

test_that("Loopr normalises the Pareto-smoothed weights", {
  set.seed(3)
  L <- matrix(rnorm(100 * 2, -1, 0.5), 100)
  r <- Loopr(L)
  expect_equal(colSums(r$weights), c(1, 1), tolerance = 1e-12)
  expect_equal(r$k, Khatd(L)$k, tolerance = 1e-12)
  # the unsmoothed part of each column keeps the raw importance-ratio ordering
  w <- exp(-L[, 1] - max(-L[, 1]))
  low <- order(w)[1:79]
  expect_equal(r$weights[low, 1] / sum(r$weights[low, 1]), w[low] / sum(w[low]), tolerance = 1e-12)
})

test_that("Ppssamp draws PPS-with-replacement and forms the Hansen-Hurwitz estimator", {
  z <- c(2, 5, 1, 8, 4)
  y <- c(20, 55, 9, 90, 38)
  r <- Ppssamp(z, y, 6, seed = 3)
  p <- z / sum(z)
  s <- 3
  idx <- integer(6)
  for (t in 1:6) {
    s <- (48271 * s) %% 2147483647
    idx[t] <- which(s / 2147483647 <= cumsum(p))[1]
  }
  expect_equal(r$index, idx)
  rr <- y[idx] / p[idx]
  expect_equal(r$estimate, mean(rr), tolerance = 1e-12)
  expect_equal(r$se, sqrt(var(rr) / 6), tolerance = 1e-12)
  expect_error(Ppssamp(-z, y, 3), "strictly positive")
})
