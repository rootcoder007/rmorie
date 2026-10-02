# Coverage for the Ghosal and van der Vaart (2017) demonstrations of
# chapters 2, 3, 6, 8, 12, 13 and 14.  Seeded ones replay the same .ghc
# stream in the test and recompute the statistic with base R.

test_that("the sec. 12.2-12.4 simulations replay the stream", {
  e <- .ghc_rng(3)
  dv <- vapply(1:20, function(i) {
    cnt <- sum(.ghc_unif(e, 100) <= 0.3)
    10 * ((2 * 0.3 + cnt) / 102 - 0.3)
  }, 0)
  r <- Ghosaldpbvm(100, 2, 20, 3)
  expect_equal(r$estimate, var(dv), tolerance = 1e-12)
  expect_equal(r$gap, abs(var(dv) - 0.21), tolerance = 1e-12)
  expect_error(Ghosaldpbvm(n_sim = 1), "n_sim")
  expect_error(Ghosaldpbvm(alpha = 0), "alpha")

  e <- .ghc_rng(5)
  d <- sort(.ghc_unif(e, 50))
  ks <- sqrt(50) * max(abs(1:50 / 50 - d), abs(0:49 / 50 - d))
  expect_equal(Ghosalstrongapxdp(50, 5)$estimate, ks, tolerance = 1e-12)
  expect_error(Ghosalstrongapxdp(0), "positive")

  e <- .ghc_rng(2)
  dv <- vapply(1:15, function(i) 8 * ((1 + sum(.ghc_unif(e, 64))) / 66 - 0.5), 0)
  expect_equal(Ghosalsemiparabvm(64, 2, 15, 2)$estimate, var(dv), tolerance = 1e-12)
  expect_error(Ghosalsemiparabvm(n = 0), "positive")

  x <- c(0.3, 1.2, -0.5, 2.2, 0.9)
  ef <- Ghosaleffinflfn(x, 1)
  Ft <- mean(x <= 1)
  expect_equal(ef$estimate, mean((as.numeric(x <= 1) - Ft)^2), tolerance = 1e-12)
  expect_equal(ef$estimate, Ft * (1 - Ft), tolerance = 1e-12)
  expect_true(ef$matches_bernoulli_var)
  expect_error(Ghosaleffinflfn(numeric(0), 1), "non-empty")

  I <- matrix(c(2, 0.4, 0.4, 1), 2)
  g <- c(1, -2)
  expect_equal(Ghosalsemiparaeff(g, I)$estimate, sum(g * solve(I, g)), tolerance = 1e-12)
  expect_error(Ghosalsemiparaeff(g, diag(3)), "square")

  s <- Ghosalstrictsbvm(TRUE, 0.01, 0.2, tol = 0.05)
  expect_equal(s$estimate, 1 - 0.21, tolerance = 1e-12)
  expect_equal(s$conditions, c(TRUE, TRUE, FALSE))
  expect_false(s$bvm_holds)
  expect_true(Ghosalstrictsbvm()$bvm_holds)
  expect_error(Ghosalstrictsbvm(tol = 0), "tol")

  wf <- Ghosalwnfullbvm(c(0.5, -0.2), n = 100, prior_var = 10)
  expect_equal(wf$estimate, max(abs(10 / (10 + 0.01) * c(0.5, -0.2) - c(0.5, -0.2))), tolerance = 1e-12)
  expect_false(wf$mean_matches_Y)
  expect_true(Ghosalwnfullbvm()$mean_matches_Y)
  expect_error(Ghosalwnfullbvm(prior_var = 0), "prior_var")
})

test_that("the Cox partial-likelihood grid demonstrations replay the stream", {
  npll <- function(b, tm, z) {
    o <- order(tm)
    sum(vapply(seq_along(o), function(i) log(sum(exp(b * z[tm >= tm[o[i]]]))) - b * z[o[i]], 0))
  }
  e <- .ghc_rng(4)
  n <- 40
  z <- as.numeric((1:n - 1) %% 2 == 0)
  tm <- -log(pmax(.ghc_unif(e, n), 1e-12)) / exp(0.8 * z)
  grid <- 0.8 - 1 + 2 * (0:50) / 50
  v <- vapply(grid, npll, 0, tm = tm, z = z)
  cb <- Ghosalcoxbvmsp(0.8, n, 4)
  expect_equal(cb$estimate, grid[which.min(v)], tolerance = 1e-12)
  expect_error(Ghosalcoxbvmsp(n = 0), "positive")

  e <- .ghc_rng(4)
  tm <- -log(pmax(.ghc_unif(e, n), 1e-12)) / exp(0.6 * z)
  grid <- 0.6 - 1.5 + 3 * (0:60) / 60
  lw <- vapply(grid, function(b) -0.5 * (b / 2)^2 - npll(b, tm, z), 0)
  w <- exp(lw - max(lw))
  expect_equal(Ghosalcoxpost(0.6, n, 2, 4)$estimate, sum(grid * w) / sum(w), tolerance = 1e-9)
  expect_error(Ghosalcoxpost(prior_sd = 0), "prior_sd")
})

test_that("Ghosalcoxbvm matches survival::coxph", {
  skip_if_not_installed("survival")
  X <- cbind(c(0.5, -1, 1.2, 0.3, -0.7, 2, 0.1, -0.2, 1.5, -1.3),
             c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0))
  tm <- c(2.1, 5.3, 0.7, 3.3, 8.1, 0.4, 4.4, 6.2, 1.1, 9.5)
  ev <- c(1, 1, 1, 0, 1, 1, 0, 1, 1, 1)
  r <- Ghosalcoxbvm(X, time = tm, event = ev)
  cf <- survival::coxph(survival::Surv(tm, ev) ~ X, ties = "breslow",
                        control = survival::coxph.control(eps = 1e-14, toler.chol = 1e-16, iter.max = 100))
  expect_equal(r$beta, unname(coef(cf)), tolerance = 1e-8)
  expect_equal(r$se, unname(sqrt(diag(vcov(cf)))), tolerance = 1e-8)
  expect_equal(r$posterior_normal, dnorm(r$beta_grid, r$beta[1], r$se[1]), tolerance = 1e-12)
  expect_equal(r$n_events, 8)
  expect_error(Ghosalcoxbvm(X), "time is required")
  expect_error(Ghosalcoxbvm(X, time = tm, event = rep(2, 10)), "binary")
  expect_error(Ghosalcoxbvm(X[1:4, ], time = tm[1:4]), "at least 5")
})

test_that("the chapter 13 survival constructions", {
  tm <- c(0.5, 1.2, 0.3, 2.0, 0.9)
  ev <- c(1, 0, 1, 1, 1)
  o <- order(tm)
  s <- 1
  s2 <- 1
  risk <- 5
  for (i in o) {
    if (tm[i] > 1.5) break
    if (ev[i] == 1) {
      s <- s * (2 * exp(-tm[i]) + risk - 1) / (2 * exp(-tm[i]) + risk)
      s2 <- s2 * (risk - 1) / risk
    }
    risk <- risk - 1
  }
  expect_equal(Ghosalsurvdppost(tm, ev, 1.5, alpha = 2)$estimate, s, tolerance = 1e-12)
  expect_equal(Ghosalbbcensored(tm, ev, 1.5)$estimate, s2, tolerance = 1e-12)
  expect_error(Ghosalsurvdppost(tm, ev[-1], 1), "same length")
  expect_error(Ghosalbbcensored(numeric(0), numeric(0), 1), "non-empty")

  x <- c(1, 2, 2, 3, 4, 5)
  evk <- c(1, 1, 0, 1, 0, 1)
  u <- sort(unique(x))
  km <- cumprod(vapply(u, function(t) 1 - sum(x == t & evk == 1) / sum(x >= t), 0))
  wt <- 6 / (6 + 0.5)
  dk <- Ghosalsurvdpkm(x, evk, alpha = 0.5, g0_rate = 0.4)
  expect_equal(dk$survival_km, km, tolerance = 1e-12)
  expect_equal(dk$survival_dp, wt * km + (1 - wt) * exp(-0.4 * u), tolerance = 1e-12)
  dk2 <- Ghosalsurvdpkm(x)
  expect_equal(dk2$survival_dp, (6 / 7) * cumprod(1 - c(1, 2, 1, 1, 1) / c(6, 5, 3, 2, 1)) +
                 (1 / 7) * exp(-u / mean(x)), tolerance = 1e-12)
  expect_error(Ghosalsurvdpkm(-1:1), "non-negative")
  expect_error(Ghosalsurvdpkm(x, alpha = 0), "alpha")

  e <- .ghc_rng(3)
  errs <- numeric(2)
  for (k in 1:2) {
    n <- c(20, 40)[k]
    uu <- .ghc_unif(e, 2 * n)
    xx <- -log(pmax(uu[seq(1, 2 * n, 2)], 1e-12))
    cz <- 3 * uu[seq(2, 2 * n, 2)]
    tt <- pmin(xx, cz)
    dd <- xx <= cz
    s <- 1
    risk <- n
    for (i in order(tt)) {
      if (tt[i] > 1) break
      if (dd[i]) s <- s * (risk - 1 + 2 * exp(-tt[i])) / (risk + 2 * exp(-tt[i]))
      risk <- risk - 1
    }
    errs[k] <- abs(s - exp(-1))
  }
  expect_equal(Ghosalntrconsist(c(20, 40), 3)$err_by_n, errs, tolerance = 1e-12)
  expect_error(Ghosalntrconsist(10), "two sample sizes")

  e <- .ghc_rng(6)
  F1 <- 1 - exp(-1)
  dv <- vapply(1:10, function(i) sqrt(30) * ((2 * F1 + sum(-log(pmax(.ghc_unif(e, 30), 1e-12)) <= 1)) / 32 - F1), 0)
  expect_equal(Ghosalntrbvm(30, 10, 6)$estimate, var(dv), tolerance = 1e-12)
  expect_error(Ghosalntrbvm(n_sim = 1), "n_sim")

  e <- .ghc_rng(8)
  xs <- vapply(1:25, function(i) -log(max(.ghc_unif(e, 1), 1e-12)), 0)
  br <- seq(0, 1.8, by = 0.3)
  dd <- ee <- numeric(6)
  for (b in 1:6) {
    dd[b] <- sum(xs > br[b] & xs < br[b + 1])
    ee[b] <- sum(pmin(pmax(xs - br[b], 0), 0.3))
  }
  hz <- pmax((dd + 0.5) / (ee + 0.5), 1e-6)
  sh <- Ghosalsmhazgp(25, 8)
  expect_equal(sh$hazard_by_bin, hz, tolerance = 1e-12)
  expect_equal(sh$estimate, mean(abs(hz - 1)), tolerance = 1e-12)

  cm <- Ghosalcoxmodel(0.4, c(0.5, 2), t = 3)
  expect_equal(cm$cum_hazards, 3 * exp(0.4 * c(0.5, 2)), tolerance = 1e-12)
  expect_equal(cm$estimate, exp(0.4 * 1.5), tolerance = 1e-12)
  expect_false(cm$proportional)
  expect_true(Ghosalcoxmodel()$proportional)
  expect_error(Ghosalcoxmodel(z = 1), "exactly two")
})

test_that("the beta-process and NTR constructions", {
  g <- c(0.2, 0.5, 0.9)
  e <- .ghc_rng(2)
  H <- cumsum(vapply(diff(c(0, g)), function(d) .ghc_beta1(e, max(3 * d, 1e-8), max(3 * (1 - d), 1e-8)), 0))
  bp <- Ghosalbetaprocdef(g, c = 3, seed = 2)
  expect_equal(bp$cum_hazard, H, tolerance = 1e-12)
  expect_true(bp$nondecreasing)
  expect_error(Ghosalbetaprocdef(numeric(0)), "non-empty")

  e <- .ghc_rng(1)
  h0 <- c(0.2, 0.5)
  m <- numeric(2)
  for (it in 1:2000) for (k in 1:2) m[k] <- m[k] + .ghc_beta1(e, 2 * h0[k], 2 * (1 - h0[k])) / 2000
  bd <- Ghosalbpdiscrete(h0, 2, 1)
  expect_equal(bd$mean_by_time, m, tolerance = 1e-12)
  expect_equal(bd$prior_mean_gap, max(abs(m - h0)), tolerance = 1e-12)
  expect_error(Ghosalbpdiscrete(1), "strictly between")

  u <- (1:1000 - 0.5) / 1000
  expect_equal(Ghosalbpcont(3, 2, 1000)$estimate, sum(3 * (1 - u)^2 / 1000) * 2, tolerance = 1e-12)
  expect_lt(Ghosalbpcont(3, 2, 1000)$gap, 1e-5)
  expect_error(Ghosalbpcont(0), "c must")

  e <- .ghc_rng(4)
  tau <- J <- numeric(5)
  for (k in 1:5) {
    tau[k] <- .ghc_unif(e, 1) * 2
    J[k] <- .ghc_beta1(e, 1, 2) / 5 * 2 * 5
  }
  pg <- Ghosalbppathgen(2, 2, 5, 4)
  expect_equal(pg$estimate, sum(J), tolerance = 1e-12)
  expect_true(pg$pure_jump_nondecreasing)
  expect_error(Ghosalbppathgen(n_jumps = 0), "n_jumps")

  mx <- Ghosalmixbp(c(1, 3), weights = c(1, 3), t = 2)
  expect_equal(mx$estimate, sum(c(0.25, 0.75) * c(1, 3) * 2), tolerance = 1e-12)
  expect_equal(Ghosalmixbp()$estimate, mean(c(0.5, 1, 2)), tolerance = 1e-12)
  expect_error(Ghosalmixbp(1, weights = c(1, 2)), "same length")

  nt <- Ghosalntrdef(c(0.1, 0.4, 0.2))
  expect_equal(nt$F_path, 1 - exp(-cumsum(c(0.1, 0.4, 0.2))), tolerance = 1e-12)
  expect_error(Ghosalntrdef(-1), "nonnegative")

  lv <- Ghosalntrlevy(c(0.5, 2), c(1, 0.3))
  expect_equal(lv$exponent, (1 - exp(-0.5)) + 0.3 * (1 - exp(-2)), tolerance = 1e-12)
  expect_equal(lv$estimate, exp(-lv$exponent), tolerance = 1e-12)
  expect_error(Ghosalntrlevy(1, c(1, 2)), "same length")
})

test_that("the chapter 2 prior constructions replay the stream", {
  x <- c(0.1, 0.4, 0.7, 0.95)
  e <- .ghc_rng(3)
  z <- .ghc_norm(e, 5) * (1:5)^-2
  rb <- Ghosalrandombasisexpansion(x, 5, 3, decay = 2)
  expect_equal(rb$f, as.numeric(cos(pi * outer(x, 1:5)) %*% z), tolerance = 1e-12)
  expect_error(Ghosalrandombasisexpansion(numeric(0)), "non-empty")

  K <- 2 * exp(-0.5 * outer(x, x, "-")^2 / 0.3^2) + diag(1e-10, 4)
  e <- .ghc_rng(7)
  f <- as.numeric(t(chol(K)) %*% .ghc_norm(e, 4))
  gp <- Ghosalgppriordef(x, 0.3, 2, 7)
  expect_equal(gp$f, f, tolerance = 1e-12)
  expect_error(Ghosalgppriordef(x, length = 0), "length")

  xs <- c(0.9, 0.1, 0.5)
  w <- Ghosalgppriordef(sort(xs), 0.5, seed = 2)$f
  inc <- Ghosalgpincreasingprior(xs, 0.5, 2)
  expect_equal(inc$F, c(0, cumsum(0.5 * (exp(w[-1]) + exp(w[-3])) * diff(sort(xs)))), tolerance = 1e-12)
  expect_true(inc$increasing)

  gx <- seq(0, 1, by = 0.25)
  ex <- exp(sin(3 * gx))
  Z <- sum(0.5 * (ex[-1] + ex[-5]) * 0.25)
  el <- Ghosalexplink(gx)
  expect_equal(el$density, ex / Z, tolerance = 1e-12)
  expect_equal(el$estimate, ex[3] / Z, tolerance = 1e-12)
  expect_error(Ghosalexplink(1), "two grid points")

  e <- .ghc_rng(5)
  gg <- vapply(1:4, function(i) .ghc_gamma1(e, 2, 1), 0)
  p <- gg / sum(gg)
  hp <- Ghosalhistogramprior(c(0.1, 0.3, 0.99, 1.2), K = 4, alpha = 2, seed = 5)
  expect_equal(hp$density, c(p[1], p[2], p[4], 0) * 4, tolerance = 1e-12)
  expect_error(Ghosalhistogramprior(0.5, alpha = 0), "alpha")

  e <- .ghc_rng(9)
  gg <- vapply(1:3, function(i) .ghc_gamma1(e, 1, 1), 0)
  th <- .ghc_unif(e, 3)
  mb <- Ghosalmixturebasisprior(x, 3, 9, bandwidth = 0.2)
  expect_equal(mb$density, as.numeric(outer(x, th, function(a, b) dnorm(a, b, 0.2)) %*% (gg / sum(gg))),
               tolerance = 1e-12)
  expect_error(Ghosalmixturebasisprior(x, bandwidth = 0), "bandwidth")

  bf <- Ghosalbernsteinfeller(c(0.2, 0.6, 1.3), K = 6)
  Bk <- function(u) sum(((0:6) / 6)^2 * dbinom(0:6, 6, u))
  expect_equal(bf$F_K, vapply(c(0.2, 0.6, 1), Bk, 0), tolerance = 1e-12)
  expect_equal(bf$sup_error, max(abs(vapply(c(0.2, 0.6, 1), Bk, 0) - c(0.04, 0.36, 1))), tolerance = 1e-12)
  expect_error(Ghosalbernsteinfeller(0.5, K = 0), "at least 1")
})

test_that("the chapter 2 GP regressions solve their normal equations or MAP conditions", {
  x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  y <- c(0.2, 0.8, 1.1, 0.7, 0.1)
  Kx <- 1.5 * exp(-0.5 * outer(x, x, "-")^2 / 0.4^2)
  nr <- Ghosalnpnormalreg(x, y, 0.4, 1.5, 0.1)
  fh <- as.numeric(Kx %*% solve(Kx + diag(0.1, 5), y))
  expect_equal(nr$fitted, fh, tolerance = 1e-12)
  expect_equal(nr$sse, sum((fh - y)^2), tolerance = 1e-12)
  expect_error(Ghosalnpnormalreg(x, y, sigma2 = 0), "sigma2")

  cnt <- c(0, 2, 5, 3, 1)
  pr <- Ghosalnppoissonreg(x, cnt, 0.5, 1)
  K <- exp(-0.5 * outer(x, x, "-")^2 / 0.5^2) + diag(1e-8, 5)
  # MAP stationarity: f = K (y - exp(f)); Newton stops at a step below 1e-8
  expect_lt(max(abs(pr$f - as.numeric(K %*% (cnt - exp(pr$f))))), 1e-6)
  expect_equal(pr$intensity, exp(pr$f), tolerance = 1e-12)
  expect_error(Ghosalnppoissonreg(x, -cnt), "non-negative")

  yb <- c(0, 0, 1, 1, 0)
  bn <- Ghosalnpbinaryreg(x, yb, 0.7, 2)
  K2 <- 2 * exp(-0.5 * outer(x, x, "-")^2 / 0.7^2) + diag(1e-8, 5)
  s <- ifelse(yb > 0.5, dnorm(bn$f) / pnorm(bn$f), -dnorm(bn$f) / pnorm(-bn$f))
  # MAP stationarity: f = K s(f), same stopping rule as above
  expect_lt(max(abs(bn$f - as.numeric(K2 %*% s))), 1e-6)
  expect_equal(bn$prob, pnorm(bn$f), tolerance = 1e-12)
  expect_error(Ghosalnpbinaryreg(x, yb[-1]), "same length")
})

test_that("the chapter 3, 6, 8 and 14 checks", {
  mp <- Momprior(c(1, 0.5, 1 / 3))
  expect_equal(mp$differences[[2]], c(0.5, 1 / 6), tolerance = 1e-12)
  expect_equal(mp$differences[[3]], 1 / 3, tolerance = 1e-12)
  expect_equal(mp$feasible, 1)
  bad <- Momprior(c(1, 0.5, 0.6))
  expect_equal(bad$n_violations, 1)
  expect_equal(bad$min_difference, 0.5 - 0.6, tolerance = 1e-12)
  expect_equal(bad$feasible, 0)
  expect_error(Momprior(c(2, 1)), "m_0")

  sp <- Sepcons(0.2, 3, 12)
  expect_equal(sp$rate, -log(0.2) / 3, tolerance = 1e-12)
  expect_equal(sp$bound, 0.2^4, tolerance = 1e-12)
  expect_error(Sepcons(1, 1, 1), "delta")

  d <- c(0.5, 0.3, 0.2, 0.1, 0.05)
  mc <- Martcons(d, variances = c(1, 1, 2, 2, 3))
  expect_equal(mc$cesaro, cumsum(d) / 1:5, tolerance = 1e-12)
  expect_equal(mc$tail_mean, mean(d[3:5]), tolerance = 1e-12)
  expect_equal(mc$lemma652_sum, sum(c(1, 1, 2, 2, 3) / (1:5)^2), tolerance = 1e-12)
  expect_true(is.nan(Martcons(d)$lemma652_sum))
  expect_error(Martcons(1.5), "lie in")
  expect_error(Martcons(d, variances = 1), "same length")

  kl <- Kldsupp(0.1, 0.2, 0.5, 10)
  expect_equal(kl$margin, 0.3, tolerance = 1e-12)
  expect_equal(kl$bound, exp(-3) / 0.1, tolerance = 1e-12)
  expect_equal(Kldsupp(0.1, 0.6, 0.5, 10)$holds, 0)
  expect_error(Kldsupp(0, 0.1, 0.1, 1), "prior mass")

  tc <- Testcond(0.01, 5, 1e-6, 0.3, 0.5, 100, 2)
  neb <- 100 * 0.09
  expect_equal(tc$slack_prior, log(0.01) + 2 * neb, tolerance = 1e-12)
  expect_equal(tc$slack_entropy, 100 * 0.25 - 5, tolerance = 1e-12)
  expect_equal(tc$slack_sieve, -6 * neb - log(1e-6), tolerance = 1e-12)
  expect_equal(tc$holds, as.numeric(tc$slack_prior >= 0 && tc$slack_entropy >= 0 && tc$slack_sieve >= 0))
  expect_equal(Testcond(0.5, 1, 0, 0.1, 0.2, 10, 1)$slack_sieve, Inf)
  expect_error(Testcond(0.5, 1, 0.1, 0.3, 0.2, 10, 1), "at least eps_bar")

  pd <- Poisdir(0.3, 1.5, 3, n = 6)
  ev <- 0.7 / (2.5 + (0:2) * 0.3)
  expect_equal(pd$expected_stick, ev, tolerance = 1e-12)
  expect_equal(pd$weights, ev * c(1, cumprod(1 - ev)[1:2]), tolerance = 1e-12)
  expect_equal(pd$Vnk, (1.5 + 0.3) * (1.5 + 0.6) / prod(2.5 + 0:4), tolerance = 1e-12)
  expect_true(is.nan(Poisdir(0, 1, 2)$Vnk))
  expect_error(Poisdir(1, 1, 1), "sigma")
  expect_error(Poisdir(0.2, 1, 5, n = 3), "cannot exceed")

  xv <- c(0.1, 0.5, 0.55, 0.9, 1.4)
  tf <- Tfcells(xv, c(0.4, 1))
  expect_equal(tf$N_epsilon, 3L)
  expect_equal(tf$proportion, 3 / 5, tolerance = 1e-12)
  tl <- Tfcells(xv, list(c(0, 0.5), function(v) v > 1), n = 4)
  expect_equal(tl$N_epsilon, c(1L, 0L))
  expect_equal(tl$n, 4)
  expect_error(Tfcells(xv, c(0, 1), n = 9), "n must lie")
})
