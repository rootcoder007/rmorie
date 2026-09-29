# Coverage for ucbb .. vaeCF exports. Every expectation is recomputed in the
# test body.

test_that("Ucbb follows UCB1 on a reward table", {
  x <- rbind(c(1, 0, 0), c(0, 1, 0), c(1, 0, 1), c(1, 1, 0), c(0, 1, 1), c(1, 0, 0), c(1, 1, 1), c(0, 0, 1))
  r <- Ucbb(x, T = 7)
  cnt <- sm <- numeric(3)
  for (t in 1:7) {
    j <- if (t <= 3) t else which.max(sm / cnt + sqrt(2 * log(t - 1) / cnt))
    cnt[j] <- cnt[j] + 1
    sm[j] <- sm[j] + x[t, j]
  }
  expect_equal(r$counts, cnt)
  expect_equal(r$index, sm / cnt + sqrt(2 * log(7) / cnt), tolerance = 1e-12)
  expect_equal(r$total_reward, sum(sm))
  expect_error(Ucbb(x, T = 2), "at least K = 3")
  expect_match(morie_ukfF_cheatsheet(), "2n\\+1 sigma points")
})

test_that("morie_ukrig solves the universal kriging system", {
  P <- cbind(c(0, 1, 2, 0.5, 1.5, 2.5), c(0, 0.5, 0, 1.2, 1, 1.4))
  z <- c(1.2, 0.4, -0.3, 0.8, 0.1, 0.6)
  tg <- rbind(c(1, 1), c(2, 0.5))
  for (mod in c("exponential", "gaussian", "spherical")) {
    r <- morie_ukrig(z, P, tg, model = mod, nugget = 0.1, sill = 1.2, range_ = 1.5)
    cf <- function(h) {
      base <- switch(mod, exponential = exp(-h / 1.5), gaussian = exp(-h^2 / 1.5^2),
                     spherical = ifelse(h <= 1.5, 1 - 1.5 * h / 1.5 + 0.5 * (h / 1.5)^3, 0))
      1.1 * base + 0.1 * (h == 0)
    }
    D <- as.matrix(stats::dist(P))
    Fm <- cbind(1, P)
    K <- rbind(cbind(cf(D), Fm), cbind(t(Fm), matrix(0, 3, 3)))
    for (k in 1:2) {
      rhs <- c(cf(sqrt(colSums((t(P) - tg[k, ])^2))), 1, tg[k, ])
      sol <- solve(K, rhs)
      expect_equal(r$estimate[k], sum(sol[1:6] * z), tolerance = 1e-10)
      expect_equal(r$se[k], sqrt(max(1.2 - sum(sol * rhs), 0)), tolerance = 1e-10)
    }
  }
  ex <- morie_ukrig(z, P, P[3, , drop = FALSE], trend_order = 0)
  expect_equal(ex$estimate, z[3], tolerance = 1e-10)
  expect_equal(ex$se, 0, tolerance = 1e-6)
  q2 <- morie_ukrig(z, P, c(1, 1), trend_order = 2, model = "gaussian", nugget = 0.05)
  expect_true(is.finite(q2$estimate))
  expect_error(morie_ukrig(z, P, c(1, 1), trend_order = 3), "trend_order must be")
  expect_error(morie_ukrig(z, P, c(1, 1), model = "cubic"), "unknown model")
  expect_error(morie_ukrig(z[-1], P, c(1, 1)), "coords rows must match")
})

test_that("Unobts maximises the concentrated local-level likelihood", {
  y <- c(10.2, 10.8, 11.5, 11.1, 12.3, 12.9, 12.4, 13.8, 14.1, 13.9, 15.0, 15.6)
  ll_level <- function(q) {
    a <- 0
    P <- 1e10
    v <- f <- numeric(12)
    lev <- numeric(12)
    for (t in 1:12) {
      Pp <- P + q
      f[t] <- Pp + 1
      v[t] <- y[t] - a
      K <- Pp / f[t]
      a <- a + K * v[t]
      P <- Pp - K * Pp
      lev[t] <- a
    }
    s2 <- sum(v[-1]^2 / f[-1]) / 11
    list(ll = -0.5 * 11 * (log(2 * pi) + 1 + log(s2)) - 0.5 * sum(log(f[-1])), s2 = s2, lev = lev)
  }
  grid <- c(0.05, 0.5, 2)
  lls <- vapply(grid, function(q) ll_level(q)$ll, 0)
  r <- Unobts(y, ratio_grid = grid)
  b <- ll_level(grid[which.max(lls)])
  # the diffuse prior 1e10 is carried through the first update
  expect_equal(r$loglik, b$ll, tolerance = 1e-8)
  expect_equal(r$ratios, grid[which.max(lls)])
  expect_equal(r$sigma2, b$s2, tolerance = 1e-8)
  expect_equal(r$level, b$lev, tolerance = 1e-8)
  expect_equal(r$aic, -2 * b$ll + 4, tolerance = 1e-8)
  s <- Unobts(y, components = c("trend", "seasonal"), period = 4, ratio_grid = c(0, 0.1))
  expect_equal(s$d, 5L)
  expect_equal(s$irregular, y - s$level - s$seasonal, tolerance = 1e-12)
  expect_error(Unobts(y, components = "cycle"), "unknown component")
  expect_error(Unobts(y, components = "seasonal", period = 1), "at least 2")
  expect_error(Unobts(y[1:2], components = "trend"), "shorter than the state dimension")
})

test_that("Vaccthresh is 1 - 1/R0 scaled by efficacy", {
  r <- Vaccthresh(c(1.5, 3, 12), efficacy = 0.9)
  pc <- 1 - 1 / c(1.5, 3, 12)
  expect_equal(r$threshold, pc, tolerance = 1e-12)
  expect_equal(r$coverage, pc / 0.9, tolerance = 1e-12)
  expect_equal(r$feasible, pc / 0.9 <= 1)
  expect_error(Vaccthresh(2, efficacy = 0), "efficacy must be")
  expect_error(Vaccthresh(-1), "R0 must be positive")
})

test_that("Vaean scores reconstruction probability under the PPCA optimum", {
  X <- cbind(c(1.0, 2.1, 2.9, 4.2, 5.0, 3.1), c(0.8, 2.0, 3.2, 3.9, 5.1, 9.0), c(0.1, -0.2, 0.3, 0.0, -0.1, 0.2))
  r <- Vaean(X, latent_dim = 1, n_samples = 4, alpha = 0.2)
  cen <- colMeans(X)
  C <- crossprod(sweep(X, 2, cen)) / 6
  e <- eigen(C, symmetric = TRUE)
  W <- e$vectors[, 1, drop = FALSE]
  s <- sqrt(mean(e$values[2:3]))
  Xc <- sweep(X, 2, cen)
  rec <- Xc %*% W %*% t(W)
  lrp <- -0.5 * rowSums(log(2 * pi * s^2) + (Xc - rec)^2 / s^2)
  expect_equal(abs(r$W), abs(W), tolerance = 1e-9)
  expect_equal(r$decoder_scale, s, tolerance = 1e-9)
  expect_equal(r$log_rp, lrp, tolerance = 1e-9)
  cut <- stats::quantile(lrp, 0.2, type = 7, names = FALSE)
  expect_equal(r$threshold, cut, tolerance = 1e-9)
  expect_equal(r$anomaly, as.numeric(lrp < cut))
  g <- Vaean(X, vae = list(W = W, center = cen), decoder_scale = 0.5, threshold = -5)
  expect_equal(g$log_rp, -0.5 * rowSums(log(2 * pi * 0.25) + (Xc - rec)^2 / 0.25), tolerance = 1e-9)
  expect_error(Vaean(X, latent_dim = 4), "latent_dim must lie")
  expect_error(Vaean(X, alpha = 2), "alpha must lie")
})

test_that("Vaeber combines the Gaussian KL with Monte Carlo reconstruction", {
  X <- rbind(c(0.5, 1.0, -0.3), c(1.2, -0.4, 0.8))
  enc <- list(mu = rbind(c(0.2, -0.1), c(0.5, 0.3)), logvar = rbind(c(-1, -0.5), c(-0.2, -2)))
  dec <- list(W = rbind(c(1, 0.5, -0.2), c(-0.3, 0.8, 0.4)), b = c(0.1, 0, -0.1))
  r <- Vaeber(X, encoder = enc, decoder = dec, n_samples = 5, decoder_scale = 0.7)
  sg <- exp(0.5 * enc$logvar)
  kl <- 0.5 * rowSums(enc$mu^2 + sg^2 - 1 - enc$logvar)
  expect_equal(r$kl_per_point, kl, tolerance = 1e-12)
  mean_r <- sweep(enc$mu %*% dec$W, 2, dec$b, "+")
  q <- sg^2 %*% dec$W^2
  ana <- -0.5 * rowSums(log(2 * pi * 0.49) + ((X - mean_r)^2 + q) / 0.49)
  expect_equal(r$recon_analytic, mean(ana), tolerance = 1e-12)
  eps <- .vitdraw(5, 2, 0 + 2 * 3 * 2 + 2 * 3, 1)
  mc <- vapply(1:2, function(i) mean(vapply(1:5, function(l) {
    z <- enc$mu[i, ] + sg[i, ] * eps[l, ]
    rr <- as.numeric(z %*% dec$W) + dec$b
    -0.5 * sum(log(2 * pi * 0.49) + (X[i, ] - rr)^2 / 0.49)
  }, 0)), 0)
  expect_equal(r$recon_per_point, mc, tolerance = 1e-12)
  expect_equal(r$elbo, mean(mc) - mean(kl), tolerance = 1e-12)
  d0 <- Vaeber(X, n_samples = 3)
  expect_equal(d0$latent_dim, 2L)
  expect_equal(d0$elbo, d0$recon - d0$kl, tolerance = 1e-12)
  expect_error(Vaeber(X, decoder_scale = 0), "decoder_scale must be positive")
  expect_error(Vaeber(X, encoder = list(mu = enc$mu[1, , drop = FALSE], logvar = enc$logvar)), "one row per observation")
})

test_that("Vaecf ranks items and scores recall and NDCG", {
  R <- rbind(c(3, 0, 1, 0, 2), c(0, 1, 0, 4, 0), c(1, 1, 0, 0, 0))
  r <- Vaecf(R, K = 2, latent_dim = 2, beta = 0.3, n_samples = 4)
  sg <- exp(0.5 * r$logvar)
  kl <- 0.5 * rowSums(r$mu^2 + sg^2 - 1 - r$logvar)
  expect_equal(r$kl_per_user, kl, tolerance = 1e-12)
  expect_equal(r$elbo, r$loglik - 0.3 * mean(kl), tolerance = 1e-12)
  rel <- R > 0
  rc <- nd <- numeric(3)
  for (u in 1:3) {
    top <- r$ranking[u, 1:2]
    h <- rel[u, top]
    den <- min(2, sum(rel[u, ]))
    rc[u] <- sum(h) / den
    nd[u] <- sum(h / log2(1:2 + 1)) / sum(1 / log2(seq_len(den) + 1))
  }
  expect_equal(r$recall_per_user, rc, tolerance = 1e-12)
  expect_equal(r$ndcg_per_user, nd, tolerance = 1e-12)
  Xn <- t(apply(log1p(R), 1, function(v) v / sqrt(sum(v^2))))
  expect_equal(r$mu, Xn %*% .vitdraw(5, 2, 0, 1 / sqrt(5)), tolerance = 1e-12)
  expect_error(Vaecf(-R), "non-negative")
  expect_error(Vaecf(R, K = 6), "K must lie")
  expect_error(Vaecf(R, relevance = R[1:2, ]), "same shape")
})
