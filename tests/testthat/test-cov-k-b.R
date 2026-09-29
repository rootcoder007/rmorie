# Coverage for the binned KL chain diagnostic, the delegating Kalman
# filter/smoother, smoothed KL, konfound, Kaplan-Meier bands, Kronecker
# graphs, the kriging family, Krippendorff's alpha and kernel ridge
# regression; recomputed with solve(), survival, mvtnorm and base R.

test_that("Klmcmc bins the chain against the target", {
  x <- c(0.1, 0.4, 0.45, 0.7, 0.9, 0.95, 0.2, 0.6)
  r <- Klmcmc(x, function(m) dnorm(m, 0.5, 0.3), bins = 4, lo = 0, hi = 1)
  p <- tabulate(pmin(floor(x / 0.25), 3) + 1, 4) / 8
  q <- dnorm((1:4 - 0.5) / 4, 0.5, 0.3)
  q <- q / sum(q)
  expect_equal(r$p, p, tolerance = 1e-12)
  expect_equal(r$kl, sum(ifelse(p > 0, p * log(p / q), 0)), tolerance = 1e-12)
  z <- Klmcmc(x, c(1, 1, 0, 1), bins = 4, lo = 0, hi = 1)
  expect_equal(z$kl, Inf)
  expect_equal(z$unsupported_bins, 1L)
  expect_error(Klmcmc(x, 1, bins = 4), "one value per bin")
  expect_error(Klmcmc(1, 1), "two draws")
})

test_that("Klmflt and Klmsmh reproduce Kalmf and KalmS", {
  set.seed(1)
  y <- cumsum(rnorm(12))
  mod <- list(F = matrix(1), H = matrix(1), Q = matrix(0.3), R = matrix(0.5), x0 = 0, P0 = matrix(2))
  f <- Klmflt(y, mod)
  ref <- Kalmf(y, mod$F, mod$H, mod$Q, mod$R, mod$x0, mod$P0)
  expect_equal(f$loglik, ref$loglik, tolerance = 1e-12)
  expect_equal(unlist(f$state), unlist(ref$state), tolerance = 1e-12)
  s <- Klmsmh(y, mod, f)
  xs <- unlist(f$state)
  Ps <- unlist(f$cov)
  xp <- unlist(f$predicted)
  Pp <- unlist(f$predicted_cov)
  sm <- xs
  for (t in 11:1) sm[t] <- xs[t] + Ps[t] / (Pp[t + 1] + 1e-12) * (sm[t + 1] - xp[t + 1])
  expect_equal(unlist(s$smoothed), sm, tolerance = 1e-12)
  expect_equal(unlist(s$smoothed), unlist(KalmS(y, mod$F, mod$H, mod$Q, mod$R, mod$x0, mod$P0)$smoothed), tolerance = 1e-12)
  expect_error(Klmflt(y, list(F = 1)), "missing entry H")
  expect_error(Klmsmh(y, mod, list(state = 1)), "missing entry cov")
})

test_that("Klmsm1 is add-eps smoothed KL", {
  p <- c(3, 0, 5, 2)
  q <- c(1, 4, 0, 5)
  r <- Klmsm1(p, q, 0.5)
  P <- (p + 0.5) / 12
  Q <- (q + 0.5) / 12
  expect_equal(r$kl_pq, sum(P * log(P / Q)), tolerance = 1e-12)
  expect_equal(r$symmetric_kl, sum(P * log(P / Q)) + sum(Q * log(Q / P)), tolerance = 1e-12)
  expect_equal(c(r$zeros_p, r$zeros_q), c(1, 1))
  expect_error(Klmsm1(p, q, 0), "eps")
})

test_that("Konfound gives percent bias to invalidate and the ITCV", {
  r <- Konfound(0.5, 0.15, 100, 0.05, n_covariates = 3)
  df <- 94
  tc <- qt(0.975, df)
  frac <- 1 - tc * 0.15 / 0.5
  expect_equal(r$pct_bias, 100 * frac, tolerance = 1e-12)
  expect_equal(r$rir, 100 * frac, tolerance = 1e-12)
  t <- 0.5 / 0.15
  rxy <- t / sqrt(t^2 + df)
  rc <- tc / sqrt(tc^2 + df)
  expect_equal(r$itcv, (rxy - rc) / (1 - rc), tolerance = 1e-12)
  expect_equal(r$r_cv, sqrt((rxy - rc) / (1 - rc)), tolerance = 1e-12)
  ns <- Konfound(0.1, 0.15, 100, 0.05)
  expect_equal(ns$significant, 0)
  expect_lt(ns$itcv, 0)
  expect_error(Konfound(0, 1, 10, 0.05), "exactly zero")
  expect_error(Konfound(1, 1, 3, 0.05), "degrees of freedom")
})

test_that("Kpmnci and Kpmsmp give Greenwood and Hall-Wellner bands", {
  skip_if_not_installed("survival")
  tm <- c(2, 3, 3, 5, 6, 8, 9, 11, 12, 14)
  ev <- c(1, 1, 0, 1, 1, 0, 1, 1, 0, 1)
  km <- survival::survfit(survival::Surv(tm, ev) ~ 1, conf.type = "plain")
  ok <- km$n.event > 0
  fit <- list(time = km$time[ok], n_risk = km$n.risk[ok], n_event = km$n.event[ok])
  r <- Kpmnci(fit, 0.05)
  expect_equal(r$surv, km$surv[ok], tolerance = 1e-12)
  expect_equal(r$se, (km$std.err * km$surv)[ok], tolerance = 1e-9)
  expect_equal(r$lower, pmax(km$surv[ok] - qnorm(0.975) * r$se, 0), tolerance = 1e-12)
  b <- Kpmsmp(fit, 0.05)
  h <- uniroot(function(h) 2 * sum((-1)^(0:99) * exp(-2 * (1:100)^2 * h^2)) - 0.05, c(0.5, 3), tol = 1e-14)$root
  expect_equal(b$h, h, tolerance = 1e-9)
  sig2 <- cumsum(fit$n_event / (fit$n_risk * (fit$n_risk - fit$n_event)))
  expect_equal(b$half_width, h / sqrt(10) * (1 + 10 * sig2) * r$surv, tolerance = 1e-9)
  expect_error(Kpmnci(fit, 1), "alpha")
  expect_error(Kpmnci(list(time = 1, n_risk = 1, n_event = 2), 0.05), "between 0 and n_risk")
})

test_that("Krfgrp is the Kronecker power of the seed", {
  T <- matrix(c(0.9, 0.5, 0.4, 0.1), 2)
  r <- Krfgrp(T, 3)
  P <- kronecker(kronecker(T, T), T)
  expect_equal(r$P, as.numeric(t(P)), tolerance = 1e-12)
  expect_equal(r$expected_edges, sum(T)^3, tolerance = 1e-12)
  expect_equal(r$max_degree, max(rowSums(P)), tolerance = 1e-12)
  expect_equal(r$expected_self_loops, sum(diag(P)), tolerance = 1e-12)
  expect_error(Krfgrp(T * 2, 2), "\\[0, 1\\]")
  expect_error(Krfgrp(T, 13), "4096")
})

test_that("Krig, KrigCl, Krigsm and morie_krpkrg_ordinary_kriging solve the OK system", {
  P <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1.5, 1.2), c(0.3, 0.8))
  z <- c(1, 2, 1.5, 3, 1.8)
  Q <- rbind(c(0.5, 0.5), c(1, 1), c(0, 0))
  sph <- function(h, c0, s, a) ifelse(h == 0, 0, ifelse(h <= a, c0 + (s - c0) * (1.5 * h / a - 0.5 * (h / a)^3), s))
  D <- as.matrix(dist(P))
  G <- rbind(cbind(sph(D, 0.1, 1, 1.5), 1), c(rep(1, 5), 0))
  pr <- vs <- numeric(3)
  for (k in 1:3) {
    g0 <- sph(sqrt(colSums((t(P) - Q[k, ])^2)), 0.1, 1, 1.5)
    s <- solve(G, c(g0, 1))
    pr[k] <- sum(s[1:5] * z)
    vs[k] <- sum(s[1:5] * g0) + s[6]
  }
  r <- Krig(P, z, Q, nugget = 0.1, sill = 1, range_ = 1.5)
  expect_equal(r$predictions, pr, tolerance = 1e-9)
  expect_equal(r$variances, vs, tolerance = 1e-9)
  expect_equal(r$predictions[3], 1, tolerance = 1e-9)
  expect_equal(KrigCl(P, z, Q, 0.1, 1, 1.5)$predictions, pr, tolerance = 1e-9)
  expect_equal(Krigsm(P, z, Q, 0.1, 1, 1.5)$variances, vs, tolerance = 1e-9)
  expect_error(Krig(P, z[-1], Q), "same length")
  k2 <- morie_krpkrg_ordinary_kriging(P, z, Q, model = "exponential", nugget = 0.1, sill = 1, rng = 1.5)
  ex <- function(h) ifelse(h > 0, 0.1 + 0.9 * (1 - exp(-3 * h / 1.5)), 0)
  G2 <- rbind(cbind(ex(D), 1), c(rep(1, 5), 0))
  p2 <- vapply(1:3, function(k) {
    g0 <- ex(sqrt(colSums((t(P) - Q[k, ])^2)))
    sum(solve(G2, c(g0, 1))[1:5] * z)
  }, 0)
  expect_equal(k2$prediction, p2, tolerance = 1e-9)
  expect_equal(k2$variance[3], 0)
  expect_error(morie_krpkrg_ordinary_kriging(P, z, Q, model = "linear"), "model must be")
  expect_error(morie_krpkrg_ordinary_kriging(P, z, Q, nugget = 2, sill = 1), "below the nugget")
})

test_that("FixedRankKriging is the Woodbury form of the rank-r predictor", {
  set.seed(2)
  P <- cbind(runif(12), runif(12))
  z <- rnorm(12, 3)
  Cn <- rbind(c(0.25, 0.25), c(0.75, 0.25), c(0.5, 0.75))
  K <- matrix(c(1, 0.3, 0.1, 0.3, 1, 0.2, 0.1, 0.2, 1), 3)
  Q <- rbind(c(0.5, 0.5), c(0.1, 0.9))
  r <- FixedRankKriging(z, P, Q, Cn, radius = 0.8, K = K, sigma2_eps = 0.2, sigma2_xi = 0.05)
  bis <- function(q) {
    d <- sqrt(colSums((t(Cn) - q)^2))
    ifelse(d < 0.8, (1 - (d / 0.8)^2)^2, 0)
  }
  S <- t(apply(P, 1, bis))
  Sig <- S %*% K %*% t(S) + diag(0.25, 12)
  mu <- mean(z)
  for (k in 1:2) {
    s0 <- bis(Q[k, ])
    c0 <- S %*% K %*% s0
    expect_equal(r$prediction[k], mu + sum(c0 * solve(Sig, z - mu)), tolerance = 1e-9)
    expect_equal(r$mspe[k], sum(s0 * K %*% s0) + 0.05 - sum(c0 * solve(Sig, c0)), tolerance = 1e-9)
  }
  expect_equal(r$basis, S, tolerance = 1e-12)
})

test_that("GaKriging and EmpiricalBayesianKriging krige with the variograms they report", {
  skip_if_not_installed("mvtnorm")
  set.seed(3)
  P <- cbind(runif(20), runif(20))
  z <- sin(3 * P[, 1]) + cos(2 * P[, 2]) + rnorm(20, sd = 0.1)
  Q <- rbind(c(0.5, 0.5), c(0.2, 0.8))
  g <- GaKriging(z, P, Q, pop = 8, generations = 5, seed = 2)
  ev <- EmpiricalVariogramBins(z, P)
  f <- g$fit
  sse <- sum(ev$np / ev$dist^2 * (ev$gamma - f$nugget - f$psill * (1 - exp(-ev$dist / f$range)))^2)
  expect_equal(f$sse, sse, tolerance = 1e-9)
  mdl <- list(model = "Exp", psill = f$psill, range = f$range, nugget = f$nugget)
  expect_equal(g$prediction, Krige(z, P, Q, mdl)$prediction, tolerance = 1e-12)
  e <- EmpiricalBayesianKriging(z, P, Q, nsim = 4, seed = 5)
  expect_equal(sum(e$weights), 1, tolerance = 1e-12)
  lls <- vapply(e$variograms, function(v) {
    m <- list(model = "Exp", psill = v$psill, range = v$range, nugget = v$nugget)
    C <- outer(1:20, 1:20, Vectorize(function(i, j) KrigingCovariance(sqrt(sum((P[i, ] - P[j, ])^2)), m)))
    mvtnorm::dmvnorm(z, rep(mean(z), 20), C, log = TRUE)
  }, 0)
  w <- exp(lls - max(lls))
  expect_equal(e$weights, w / sum(w), tolerance = 1e-9)
  pk <- vapply(e$variograms, function(v) Krige(z, P, Q, list(model = "Exp", psill = v$psill, range = v$range,
                                                               nugget = v$nugget))$prediction, numeric(2))
  expect_equal(e$prediction, as.numeric(pk %*% (w / sum(w))), tolerance = 1e-9)
})

test_that("Krpalp matches the pairwise definition of Krippendorff's alpha", {
  M <- rbind(c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
             c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, 3),
             c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, NA),
             c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA))
  alpha <- function(delta) {
    units <- lapply(seq_len(ncol(M)), function(u) M[!is.na(M[, u]), u])
    units <- units[lengths(units) >= 2]
    all <- unlist(units)
    n <- length(all)
    Do <- sum(vapply(units, function(v) {
      m <- length(v)
      sum(outer(v, v, delta)) / (m - 1)
    }, 0)) / n
    De <- sum(outer(all, all, delta)) / (n * (n - 1))
    1 - Do / De
  }
  expect_equal(Krpalp(M, "nominal")$alpha, alpha(function(a, b) (a != b) * 1), tolerance = 1e-12)
  expect_equal(Krpalp(M, "interval")$alpha, alpha(function(a, b) (a - b)^2), tolerance = 1e-12)
  expect_equal(Krpalp(M, "ratio")$alpha, alpha(function(a, b) ((a - b) / (a + b))^2), tolerance = 1e-12)
  expect_true(is.finite(Krpalp(M, "ordinal")$alpha))
  expect_error(Krpalp(M, "cardinal"), "level must be")
})

test_that("Krreg and KrrFDA are kernel ridge regression", {
  x <- c(0.1, 0.5, 0.9, 1.4, 2.0, 2.6)
  y <- sin(x) + c(0.05, -0.02, 0.03, -0.04, 0.02, 0.01)
  r <- Krreg(x, y, x_eval = c(0.3, 1.7), bandwidth = 0.5, penalty = 0.2)
  K <- dnorm(outer(x, x, "-") / 0.5)
  a <- solve(K + diag(0.2, 6), y)
  expect_equal(r$alpha, as.numeric(a), tolerance = 1e-9)
  expect_equal(r$y_hat, as.numeric(dnorm(outer(c(0.3, 1.7), x, "-") / 0.5) %*% a), tolerance = 1e-9)
  e <- Krreg(x, y, bandwidth = 0.8, penalty = 1, kernel = "epanechnikov")
  Ke <- pmax(0.75 * (1 - (outer(x, x, "-") / 0.8)^2), 0)
  expect_equal(e$y_hat, as.numeric(Ke %*% solve(Ke + diag(6), y)), tolerance = 1e-9)
  bw <- 1.06 * min(sd(x), IQR(x) / 1.34) * 6^-0.2
  expect_equal(Krreg(x, y)$bandwidth, bw, tolerance = 1e-12)
  expect_equal(KrrFDA(x, y, lam = 0.2, bandwidth = 0.5, x_eval = c(0.3, 1.7))$y_hat, r$y_hat, tolerance = 1e-12)
  expect_error(Krreg(x, y, penalty = 0), "penalty")
  expect_error(Krreg(x, y, kernel = "cosine"), "unknown kernel")
})
