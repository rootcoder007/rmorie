# Coverage for the Ghosal and van der Vaart (2017) demonstrations of the
# appendix, chapter 1, chapter 10 and chapter 11 (plus sec. 12.1, 12.4.2,
# 12.5).  Seeded ones replay the same .ghc stream in the test and recompute
# the statistic with base R.

logev_k <- function(y, n, K) sum(dnorm(y, 0, sqrt(1 / n + (seq_along(y) <= K)), log = TRUE))

test_that("Exptest and Lecam are the closed-form bounds", {
  r <- Exptest(0.1, 0.01, 10)
  expect_equal(c(r$rate_null, r$rate_alt), -log(c(0.1, 0.01)) / 10, tolerance = 1e-12)
  expect_equal(r$rate, -log(0.1) / 10, tolerance = 1e-12)
  expect_equal(r$bound, 0.1, tolerance = 1e-12)
  expect_equal(Exptest(1, 0.5, 3)$exponential, 0)
  expect_error(Exptest(0, 0.5, 3), "error probabilities")
  expect_error(Exptest(0.5, 0.5, 0), "n must")
  l <- Lecam(0.1, 0.2, 0.5, 0.05)
  expect_equal(l$bound, 0.1 + 0.2 + 0.05 / 0.5, tolerance = 1e-12)
  expect_equal(l$informative, 1)
  expect_equal(Lecam(0.6, 0.5, 1, 0)$informative, 0)
  expect_error(Lecam(2, 0, 1, 0), "dtv")
  expect_error(Lecam(0.1, 0.1, 0, 0), "prior mass")
})

test_that("the chapter 1 grid posteriors are normalised likelihood times prior", {
  th <- seq(-2, 3, by = 0.25)
  lw <- -0.5 * (1 - th)^2 - 0.5 * th^2
  post <- exp(lw) / sum(exp(lw))
  a <- Ghosalbayesruleinfinite(th)
  expect_equal(a$posterior, post, tolerance = 1e-12)
  expect_equal(a$estimate, sum(th * post), tolerance = 1e-12)
  ll <- function(t) dpois(3, exp(t), log = TRUE)
  lp <- function(t) dnorm(t, 0, 2, log = TRUE)
  b <- Ghosalbayesruleinfinite(th, ll, lp)
  w <- exp(dpois(3, exp(th), log = TRUE) + dnorm(th, 0, 2, log = TRUE))
  expect_equal(b$posterior, w / sum(w), tolerance = 1e-12)
  expect_error(Ghosalbayesruleinfinite(numeric(0)), "non-empty")

  c1 <- Ghosalabsolutecontinuity(th)
  expect_equal(c1$log_marginal, log(mean(exp(lw))), tolerance = 1e-12)
  expect_equal(c1$estimate, sum(th * post), tolerance = 1e-12)
  expect_error(Ghosalabsolutecontinuity(numeric(0)), "non-empty")

  d <- c(0.8, 1.2, 1)
  lw3 <- vapply(th, function(t) sum(-0.5 * (d - t)^2) - 0.5 * t^2, 0)
  u <- Ghosalpriorposteriorupdate(th)
  expect_equal(u$posterior, exp(lw3) / sum(exp(lw3)), tolerance = 1e-12)
  expect_lt(u$sequential_batch_gap, 1e-12)
  u2 <- Ghosalpriorposteriorupdate(th, data = c(2, 2.5))
  lw4 <- vapply(th, function(t) sum(-0.5 * (c(2, 2.5) - t)^2) - 0.5 * t^2, 0)
  expect_equal(u2$estimate, sum(th * exp(lw4)) / sum(exp(lw4)), tolerance = 1e-12)
  expect_error(Ghosalpriorposteriorupdate(th, data = numeric(0)), "data")
})

test_that("the normal-means model-selection demonstrations recompute from the stream", {
  y <- c(1.1, 0.9, 0.05, -0.1, 0.02)
  lg <- vapply(0:4, function(K) logev_k(y, 100, K) - K * log(100), 0)
  a <- Ghosaladaptthm(y = y, n = 100, K_max = 4)
  expect_equal(a$model_posterior, exp(lg - max(lg)) / sum(exp(lg - max(lg))), tolerance = 1e-12)
  expect_equal(a$estimate, which.max(lg) - 1)
  e <- .ghc_rng(7)
  yy <- ifelse(1:6 <= 2, 1, 0) + .ghc_norm(e, 6) / sqrt(50)
  lg2 <- vapply(0:6, function(K) logev_k(yy, 50, K) - 0.5 * K * log(50), 0)
  a2 <- Ghosaladaptthm(n = 50, K_true = 2, lam = 0.5, K_max = 6, seed = 7)
  expect_equal(a2$model_posterior, exp(lg2 - max(lg2)) / sum(exp(lg2 - max(lg2))), tolerance = 1e-12)
  expect_error(Ghosaladaptthm(K_max = 0), "K_max")

  e <- .ghc_rng(3)
  y2 <- 0.7 + .ghc_norm(e, 2) / sqrt(400)
  m <- Ghosalmodselbic(TRUE, n = 400, seed = 3)
  expect_equal(m$estimate, logev_k(y2, 400, 2) - logev_k(y2, 400, 0), tolerance = 1e-12)
  expect_true(m$supports_H1)
  expect_error(Ghosalmodselbic(n = 0), "positive")

  e <- .ghc_rng(9)
  y6 <- c(1, 0, 0, 0, 0, 0) + .ghc_norm(e, 6) / sqrt(200)
  l0 <- logev_k(y6, 200, 1) + log(0.3)
  l1 <- logev_k(y6, 200, 6) + log(0.7)
  expect_equal(Ghosaltwomodeladp(200, 1, 0.3, 9)$estimate, 1 / (1 + exp(l1 - l0)), tolerance = 1e-12)
  expect_error(Ghosaltwomodeladp(pi0 = 1), "pi0")

  e <- .ghc_rng(5)
  y8 <- ifelse(1:8 <= 3, 0.9, 0) + .ghc_norm(e, 8) / sqrt(300)
  lg8 <- vapply(0:8, function(K) logev_k(y8, 300, K) - 0.5 * K * log(300), 0)
  p8 <- exp(lg8 - max(lg8)) / sum(exp(lg8 - max(lg8)))
  rs <- Ghosalrndseriespr(K_true = 3, n = 300, K_max = 8, seed = 5)
  expect_equal(rs$K_posterior, p8, tolerance = 1e-12)
  expect_equal(rs$estimate, sum(0:8 * p8), tolerance = 1e-12)
  expect_error(Ghosalrndseriespr(n = 1), "exceed 1")

  e <- .ghc_rng(11)
  ns <- c(50, 500)
  risk <- numeric(2)
  for (i in 1:2) {
    yv <- ifelse(1:10 <= 2, 0.8, 0) + .ghc_norm(e, 10) / sqrt(ns[i])
    lg <- vapply(0:10, function(K) logev_k(yv, ns[i], K) - K * log(ns[i]), 0)
    kh <- which.max(lg) - 1
    risk[i] <- kh / (ns[i] + 1) + max(2 - kh, 0) * 0.64
  }
  pr <- Ghosalparamrate(2, ns, 1, 11)
  expect_equal(pr$risk_by_n, risk, tolerance = 1e-12)
  expect_equal(pr$estimate, log(risk[1] / risk[2]) / log(10), tolerance = 1e-12)
  expect_error(Ghosalparamrate(ns = 10), "two sample sizes")
})

test_that("Ghosalwnadapt, Ptnulltst, Ghosalparamnpbf and Ghosalunivweights", {
  y <- c(0.8, -0.05, 0.01)
  n <- 100
  l1 <- dnorm(y, 0, sqrt(1 / n + 2), log = TRUE) + log(0.3)
  l0 <- dnorm(y, 0, sqrt(1 / n), log = TRUE) + log(0.7)
  w <- Ghosalwnadapt(y = y, n = n, pi_incl = 0.3, tau2 = 2)
  expect_equal(w$inclusion_probs, 1 / (1 + exp(l0 - l1)), tolerance = 1e-12)
  expect_error(Ghosalwnadapt(tau2 = 0), "tau2")

  p <- Ptnulltst(-10, -11.5, lam = 0.4)
  expect_equal(p$log_bayes_factor, 1.5, tolerance = 1e-12)
  expect_equal(p$posterior_null, 0.6 * exp(-10) / (0.6 * exp(-10) + 0.4 * exp(-11.5)), tolerance = 1e-12)
  expect_error(Ptnulltst(0, 0, lam = 1), "lam")

  e <- .ghc_rng(2)
  u <- .ghc_unif(e, 300)
  cnt <- tabulate(findInterval(u, c(0.4, 0.7, 0.9), left.open = TRUE) + 1, 4)
  b <- Ghosalparamnpbf(300, FALSE, 2)
  l1 <- lgamma(4) - lgamma(304) + sum(lgamma(1 + cnt))
  expect_equal(b$estimate, l1 - 300 * log(0.25), tolerance = 1e-9)
  expect_error(Ghosalparamnpbf(0), "positive")

  k <- 1:30
  pis <- exp(-2 * k * log(50))
  pis <- pis / sum(pis)
  tr <- pis * 50^(0.5 * k)
  uw <- Ghosalunivweights(n = 50, c = 2, K_max = 30, eps_scale = 0.5)
  expect_equal(uw$estimate, sum(tr), tolerance = 1e-12)
  expect_equal(uw$partial_sums, cumsum(tr)[c(10, 30)], tolerance = 1e-12)
  expect_true(uw$converges)
  expect_error(Ghosalunivweights(n = 1), "exceed 1")
})

test_that("the chapter 10 series regressions replay their fixed iterations", {
  e <- .ghc_rng(4)
  n <- 40
  S <- matrix(0, n, 4)
  ys <- numeric(n)
  for (i in 1:n) {
    S[i, ] <- .ghc_norm(e, 4)
    ys[i] <- sum(c(1, -0.5, 0.25, 0) * S[i, ]) + 0.2 * .ghc_norm(e, 1)
  }
  bh <- solve(crossprod(S) + diag(4), crossprod(S, ys))
  f <- Ghosalfuncreg(n = n, K = 4, seed = 4)
  expect_equal(f$beta_hat, as.numeric(bh), tolerance = 1e-9)
  expect_equal(f$estimate, max(abs(as.numeric(bh) - c(1, -0.5, 0.25, 0))), tolerance = 1e-9)
  expect_error(Ghosalfuncreg(K = 3), "K must equal 4")

  e <- .ghc_rng(6)
  n <- 50
  xs <- (1:n - 0.5) / n
  f0 <- sin(2 * pi * xs)
  ys <- f0 + 0.4 * .ghc_norm(e, n)
  best <- -Inf
  for (K in 1:8) {
    P <- sqrt(2) * cos(outer(xs, 1:K) * pi)
    cf <- colSums(ys * P) / (n + 1)
    ev <- -0.5 * n * log(sum((ys - P %*% cf)^2) / n) - 0.5 * K * log(n)
    if (ev > best) {
      best <- ev
      kh <- K
      risk <- mean((P %*% cf - f0)^2)
    }
  }
  fr <- Ghosalfrsreg(n, 6)
  expect_equal(fr$K_hat, kh)
  expect_equal(fr$estimate, risk, tolerance = 1e-12)
  expect_error(Ghosalfrsreg(1), "at least 2")

  e <- .ghc_rng(8)
  n <- 60
  xs <- .ghc_unif(e, n)
  yb <- as.numeric(.ghc_unif(e, n) < plogis(4 * (xs - 0.5)))
  P <- cbind(1, sqrt(2) * cos(pi * xs), sqrt(2) * cos(2 * pi * xs))
  beta <- numeric(3)
  for (it in 1:80) beta <- beta + 0.08 * colSums(P * (yb - plogis(P %*% beta)[, 1])) / n - 0.001 * beta
  fit <- function(x) plogis(sum(beta * c(1, sqrt(2) * cos(pi * x), sqrt(2) * cos(2 * pi * x))))
  br <- Ghosalfrsbinreg(n, 3, 8)
  expect_equal(br$p_low_high, c(fit(0.1), fit(0.9)), tolerance = 1e-12)
  expect_equal(br$estimate, abs(fit(0.5) - 0.5), tolerance = 1e-12)
  expect_error(Ghosalfrsbinreg(K = 0), "K must")

  e <- .ghc_rng(12)
  n <- 30
  xs <- .ghc_unif(e, n)
  ys <- numeric(n)
  for (i in 1:n) {
    L <- exp(-exp(1 + 0.8 * cos(pi * xs[i])))
    k <- 0
    p <- 1
    repeat {
      p <- p * .ghc_unif(e, 1)
      if (p <= L) break
      k <- k + 1
    }
    ys[i] <- k
  }
  P <- cbind(1, sqrt(2) * cos(pi * xs))
  beta <- numeric(2)
  for (it in 1:200) {
    mu <- exp(pmin(P %*% beta, 5))[, 1]
    beta <- beta + 0.02 * colSums(P * (ys - mu)) / n - 0.0005 * beta
  }
  xg <- (1:20 - 0.5) / 20
  pr <- Ghosalfrspoireg(n, 2, 12)
  expect_equal(pr$beta, beta, tolerance = 1e-12)
  expect_equal(pr$estimate, mean(abs(beta[1] + beta[2] * sqrt(2) * cos(pi * xg) - 1 - 0.8 * cos(pi * xg))),
               tolerance = 1e-12)
  expect_error(Ghosalfrspoireg(0), "positive")
})

test_that("Ghosalfrsdensity averages normalised exponentiated cosine series", {
  x <- c(0.1, 0.35, 0.4, 0.5, 0.55, 0.8, 0.9)
  g <- seq(0.1, 0.9, length.out = 9)
  r <- Ghosalfrsdensity(x, grid = g, K = 2, seed = 3, n_draws = 4)
  z <- (g - 0.1) / 0.8
  zx <- (x - 0.1) / 0.8
  e <- .ghc_rng(3)
  dens <- numeric(9)
  trap <- function(a, b) sum(diff(a) * (head(b, -1) + tail(b, -1)) / 2)
  for (it in 1:4) {
    beta <- colMeans(cos(pi * outer(zx, 1:2))) * 2 + .ghc_norm(e, 2, 0, 0.3)
    psi <- cos(pi * outer(z, 1:2)) %*% beta
    f <- exp(psi - max(psi))[, 1]
    dens <- dens + f / trap(g, f)
  }
  expect_equal(r$density, dens / 4, tolerance = 1e-12)
  expect_equal(r$mass, 1, tolerance = 1e-12)
  expect_equal(r$rate, 7^(-1 / 3), tolerance = 1e-12)
  ad <- Ghosalfrsdensity(x, n_draws = 5, s = 2)
  expect_true(ad$adaptive)
  expect_true(ad$K_drawn_mean >= 1 && ad$K_drawn_mean <= 2)
  expect_equal(ad$rate, 7^(-2 / 5), tolerance = 1e-12)
  expect_error(Ghosalfrsdensity(1:4), "at least 5")
  expect_error(Ghosalfrsdensity(rep(1, 6)), "zero spread")
})

test_that("the chapter 11 closed forms", {
  S <- matrix(c(2, 0.5, 0.3, 1), 2)
  a <- c(1, -1)
  b <- c(0.5, 2)
  r <- Ghosalgpdefrkhs(S, a, b)
  expect_equal(r$estimate, sum(a * (S %*% b)), tolerance = 1e-12)
  expect_equal(r$h, as.numeric(S %*% a), tolerance = 1e-12)
  expect_equal(r$reproducing_gap, max(abs(as.numeric(t(S) %*% a) - as.numeric(S %*% a))), tolerance = 1e-12)
  expect_error(Ghosalgpdefrkhs(S, a, 1), "same length")

  k <- 1:10
  ss <- cumsum(k^-2 * 2 * cos(k * pi * 0.2) * cos(k * pi * 0.6))
  sg <- Ghosalseriesgp(0.2, 0.6, 10)
  expect_equal(sg$estimate, ss[10], tolerance = 1e-12)
  expect_equal(sg$partial_sums, ss[c(5, 10)], tolerance = 1e-12)
  expect_error(Ghosalseriesgp(n_terms = 2), "at least 3")

  rg <- Ghosalrescalgp(c(3, 1, 0.5), h = 0.4)
  expect_equal(rg$correlation_by_length, exp(-(0.4 / c(3, 1, 0.5))^2), tolerance = 1e-12)
  expect_true(rg$roughens_as_l_shrinks)
  expect_error(Ghosalrescalgp(0), "positive")

  sf <- Ghosalselfsimgp(0.3, 2, 0.5)
  expect_equal(sf$estimate, 2^0.6, tolerance = 1e-12)
  expect_lt(sf$gap, 1e-12)
  expect_error(Ghosalselfsimgp(H = 1), "H must")

  cr <- Ghosalgpcrtthm(1.5, 1000)
  expect_equal(cr$estimate, 1000^(-1 / 3.5), tolerance = 1e-12)
  expect_lt(cr$balance_gap, 1e-12)
  expect_error(Ghosalgpcrtthm(-3), "exceed -2")

  mm <- 500^(-2 / 5)
  expect_equal(Ghosalgpdenscrt(1:500, s = 2, kernel = "matern")$rate, mm, tolerance = 1e-12)
  expect_equal(Ghosalgpdenscrt(1, s = 2, n = 500, kernel = "rescaled_se")$rate,
               mm * log(500)^(3 / 5), tolerance = 1e-12)
  expect_equal(Ghosalgpdenscrt(1, s = 2, n = 500)$rate, log(500)^-2, tolerance = 1e-12)
  expect_error(Ghosalgpdenscrt(1, n = 50, kernel = "x"), "kernel must")
  expect_error(Ghosalgpdenscrt(1), "at least 2")

  bc <- Ghosalgpbinregcrt(3, 2, c(10, 100))
  expect_equal(bc$rate_by_n, c(10, 100)^(-3 / 8), tolerance = 1e-12)
  expect_error(Ghosalgpbinregcrt(0), "s must")

  G <- outer(c(0.2, 0.5, 1), c(0.2, 0.5, 1), function(s, t) 0.5 * (s^1.2 + t^1.2 - abs(s - t)^1.2))
  fb <- Ghosalfbmprior(0.6, c(0.2, 0.5, 1))
  expect_equal(fb$kernel, G, tolerance = 1e-12)
  expect_lt(fb$var_gap, 1e-12)
  expect_equal(fb$positive_definite, all(vapply(1:3, function(i) det(G[1:i, 1:i, drop = FALSE]), 0) > 0))
  expect_error(Ghosalfbmprior(ts = -1), "positive")

  sp <- Ghosalstatgpspec(0.8, 4000, 30)
  expect_equal(sp$estimate, exp(-0.64), tolerance = 1e-10)
  expect_error(Ghosalstatgpspec(lam_max = 0), "lam_max")
})

test_that("the chapter 11 simulations replay the stream", {
  e <- .ghc_rng(5)
  ss <- st <- 0
  for (it in 1:6) {
    w <- cumsum(.ghc_norm(e, 8) / sqrt(8))
    st <- st + w[2] * w[4] / 6
    ss <- ss + w[2]^2 / 6
  }
  bm <- Ghosalbmprior(8, 6, 5)
  expect_equal(bm$estimate, st, tolerance = 1e-12)
  expect_equal(bm$var_gap, abs(ss - 0.25), tolerance = 1e-12)
  expect_error(Ghosalbmprior(3), "n_grid")

  e <- .ghc_rng(2)
  mid <- (1:8 - 0.5) / 8
  v1 <- v2 <- 0
  for (it in 1:4) {
    dB <- .ghc_norm(e, 8) / sqrt(8)
    r1 <- sum((0.25 - mid[1:2])^0.5 * dB[1:2]) / gamma(1.5)
    r2 <- sum((1 - mid)^0.5 * dB) / gamma(1.5)
    v1 <- v1 + r1^2 / 4
    v2 <- v2 + r2^2 / 4
  }
  rl <- Ghosalrlprocess(1, 8, 4, 2)
  expect_equal(rl$estimate, log(v2 / v1) / log(4), tolerance = 1e-12)
  expect_error(Ghosalrlprocess(0), "alpha")

  f0 <- c(3, -1, 0.2)
  lam <- c(1, 0.5, 0.1)
  e <- .ghc_rng(1)
  hits <- 0
  for (it in 1:200) {
    z <- .ghc_norm(e, 3)
    if (sqrt(sum(lam * z^2)) < 0.5) hits <- hits + 1
  }
  hn2 <- 9 / 1 + 1 / 0.5
  rk <- Ghosalrkhsnorm(f0, lam, 0.5, n_sim = 200, seed = 1)
  expect_equal(rk$decentering_norm2, hn2, tolerance = 1e-12)
  expect_equal(rk$small_ball_exponent, -log(max(hits, 1) / 200), tolerance = 1e-12)
  expect_equal(rk$estimate, 0.5 * hn2 + rk$small_ball_exponent, tolerance = 1e-12)
  expect_error(Ghosalrkhsnorm(1, 0, 1), "positive")

  e <- .ghc_rng(3)
  n <- 20
  xs <- (1:n - 0.5) / n
  ys <- sin(2 * pi * xs / 1.5) + 0.1 * .ghc_norm(e, n)
  lev <- vapply(c(0.1, 0.3), function(l) {
    K <- exp(-0.5 * outer(xs, xs, "-")^2 / l^2) + diag(0.01 + 1e-8, n)
    -0.5 * sum(solve(K, ys) * ys) - 0.5 * as.numeric(determinant(K)$modulus)
  }, 0)
  ad <- Ghosalgpadaptthm(n, 0.3, c(0.1, 0.3), 0.1, 3)
  expect_equal(ad$log_evidence, lev, tolerance = 1e-9)
  expect_equal(ad$estimate, c(0.1, 0.3)[which.max(lev)])
  expect_error(Ghosalgpadaptthm(l_grid = -1), "positive")
})

test_that("Ghosalgplaplace and Ghosalepgp replay their fixed-point iterations", {
  x <- c(0.1, 0.3, 0.5, 0.8)
  y <- c(0, 1, 0, 1)
  K <- exp(-0.5 * outer(x, x, "-")^2 / 0.4^2) + diag(1e-8, 4)
  f <- numeric(4)
  for (it in 1:100) f <- f + 0.3 * (K %*% (y - plogis(f)) - f)[, 1]
  p <- plogis(f)
  la <- Ghosalgplaplace(x, y, length = 0.4)
  expect_equal(la$mode_probs, p, tolerance = 1e-12)
  expect_equal(la$laplace_var_site0, solve(solve(K) + diag(p * (1 - p)))[1, 1], tolerance = 1e-9)
  expect_error(Ghosalgplaplace(x, y[-1]), "same length")

  mu <- numeric(4)
  for (it in 1:60) {
    pp <- plogis(mu)
    Lam <- pmax(pp * (1 - pp), 1e-4)
    mu <- mu + 0.3 * (K %*% (y - pp))[, 1]
  }
  ep <- Ghosalepgp(x, y, length = 0.4)
  expect_equal(ep$site_precisions, Lam, tolerance = 1e-12)
  expect_equal(ep$estimate, plogis(mu[4]), tolerance = 1e-12)
  expect_equal(ep$ep_var_site0, solve(solve(K) + diag(Lam))[1, 1], tolerance = 1e-9)
  expect_true(Ghosalepgp()$separates)
  expect_error(Ghosalepgp(x, y, length = 0), "positive")
})

test_that("the sec. 12.1, 12.4.2 and 12.5 simulations replay the stream", {
  e <- .ghc_rng(4)
  S <- sum(.ghc_unif(e, 300) < 0.4)
  mle <- S / 300
  sd <- sqrt(mle * (1 - mle) / 300)
  lo <- max(mle - 6 * sd, 1e-9)
  hi <- min(mle + 6 * sd, 1 - 1e-9)
  t <- lo + (hi - lo) * (1:2000 - 0.5) / 2000
  tv <- sum(0.5 * abs(dbeta(t, 1 + S, 301 - S) - dnorm(t, mle, sd))) * (hi - lo) / 2000
  expect_equal(Ghosalinfdimbvm(0.4, 300, 4)$estimate, tv, tolerance = 1e-9)
  expect_error(Ghosalinfdimbvm(1), "theta0")

  e <- .ghc_rng(2)
  shrink <- 50 / (50 + 1 / 100)
  dv <- vapply(1:40, function(i) {
    y <- c(0.3, -0.4) + .ghc_norm(e, 2) / 10
    10 * sum(c(0.6, 0.8) * (shrink * y - c(0.3, -0.4)))
  }, 0)
  wl <- Ghosalwnlinbvm(c(0.6, 0.8), 100, 50, 40, 2)
  expect_equal(wl$estimate, var(dv), tolerance = 1e-12)
  expect_equal(wl$norm2_L, 1, tolerance = 1e-12)
  expect_error(Ghosalwnlinbvm(1), "length 2")

  # documented rule: level 0.9 uses z = 1.6449, any other level 1.96
  for (lev in c(0.9, 0.95)) {
    e <- .ghc_rng(6)
    z <- if (lev == 0.9) 1.6448536269514722 else 1.96
    hits <- 0
    for (i in 1:30) {
      S <- sum(.ghc_unif(e, 50) < 0.5)
      a <- 1 + S
      b <- 51 - S
      m <- a / (a + b)
      s <- sqrt(a * b / ((a + b)^2 * (a + b + 1)))
      hits <- hits + (abs(m - 0.5) <= z * s)
    }
    cs <- Ghosalcredsetcov(0.5, 50, lev, 30, 6)
    expect_equal(cs$estimate, hits / 30, tolerance = 1e-12)
  }
  expect_error(Ghosalcredsetcov(theta0 = 1), "theta0")
})
