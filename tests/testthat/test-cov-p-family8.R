# Coverage for prgxnt .. propMd exports. Every expectation is recomputed in
# the test body.

test_that("Perplex is exp of the per-token cross-entropy", {
  lp <- log(c(0.25, 0.5, 0.1, 0.8))
  r <- Perplex(lp)
  h <- -mean(lp)
  expect_equal(r$perplexity, exp(h), tolerance = 1e-12)
  expect_equal(r$cross_entropy_bits, h / log(2), tolerance = 1e-12)
  expect_equal(Perplex(lp, N = 8)$cross_entropy_nats, -sum(lp) / 8, tolerance = 1e-12)
  # a uniform model over V symbols has perplexity V
  expect_equal(Perplex(rep(log(1 / 7), 5))$perplexity, 7, tolerance = 1e-12)
  expect_error(Perplex(c(-1, 0.5)), "non-positive")
  expect_error(Perplex(numeric(0)), "at least one")
  expect_error(Perplex(lp, N = 0), "N must be positive")
})

test_that("morie_tv_denoise_1d reaches the TV-denoising optimum", {
  # two samples: the closed form either merges or shrinks by lam
  r <- morie_tv_denoise_1d(c(1, 4), lam = 0.5, max_iter = 20000)
  # O(1/N) primal-dual convergence: 1e-8 after 20000 iterations
  expect_equal(r$x, c(1.5, 3.5), tolerance = 1e-8)
  r2 <- morie_tv_denoise_1d(c(1, 1.6), lam = 0.5, max_iter = 20000)
  expect_equal(r2$x, c(1.3, 1.3), tolerance = 1e-8)
  b <- c(0.2, 0.1, 2.3, 2.0, 2.2, -0.5, -0.4)
  r3 <- morie_tv_denoise_1d(b, lam = 0.3, max_iter = 20000)
  dx <- diff(r3$x)
  # optimality: x = b - D'y with |y| <= lam and y = lam sign(Dx) off the flat pieces
  Dty <- c(-r3$y[1], -diff(r3$y), r3$y[6])
  expect_equal(r3$x, b - Dty, tolerance = 1e-8)
  expect_true(all(abs(r3$y) <= 0.3 + 1e-12))
  expect_equal(r3$y[abs(dx) > 1e-6], 0.3 * sign(dx[abs(dx) > 1e-6]), tolerance = 1e-8)
  expect_equal(r3$objective, 0.5 * sum((r3$x - b)^2) + 0.3 * sum(abs(dx)), tolerance = 1e-12)
  expect_error(morie_tv_denoise_1d(1), "two samples")
  expect_error(morie_tv_denoise_1d(b, lam = -1), "non-negative")
})

test_that("morie_primal enforces the step condition and estimates ||K||", {
  A <- matrix(c(2, 1, 0, 1, 3, 1), 2)
  K <- function(x) as.numeric(A %*% x)
  Kt <- function(y) as.numeric(t(A) %*% y)
  # least squares min 0.5 |Ax - b|^2 written as F(Kx) with F*(y) = 0.5|y|^2 + <b, y>
  b <- c(1, 2)
  pf <- function(y, s) (y - s * b) / (1 + s)
  pg <- function(x, t) x
  r <- morie_primal(K, Kt, pf, pg, c(0, 0, 0), c(0, 0), max_iter = 20000, tol = 1e-13)
  expect_equal(r$norm_K, sqrt(max(eigen(crossprod(A))$values)), tolerance = 1e-10)
  expect_equal(r$tau, 1 / r$norm_K, tolerance = 1e-12)
  # G = 0 leaves the minimum-residual solutions; A has full row rank so Ax = b
  expect_equal(as.numeric(A %*% r$x), b, tolerance = 1e-6)
  expect_error(morie_primal(K, Kt, pf, pg, c(0, 0, 0), c(0, 0), tau = 1, sigma = 1, norm_K = 2),
               "Theorem 1")
  expect_error(morie_primal(K, Kt, pf, pg, c(0, 0, 0), c(0, 0), tau = -1, sigma = 1, norm_K = 1),
               "must be positive")
  expect_same_function(morie_primal_dual, morie_primal)
})

test_that("Prnkpg is PageRank on the adjacency matrix", {
  G <- matrix(c(0, 1, 1, 0,
                1, 0, 0, 1,
                0, 1, 0, 1,
                1, 0, 0, 0), 4, byrow = TRUE)
  r <- Prnkpg(G, damping = 0.9, n_iter = 400)
  P <- G / rowSums(G)
  pr <- solve(diag(4) - 0.9 * t(P), rep(0.1 / 4, 4))
  expect_equal(r$pr, pr, tolerance = 1e-10)
})

test_that("RotatedGrid inverts the rotated-pole transformation", {
  rot <- function(rlon, rlat, plon, plat) {
    t0 <- (90 - plat) * pi / 180
    p <- c(cos(rlat * pi / 180) * cos(rlon * pi / 180), cos(rlat * pi / 180) * sin(rlon * pi / 180),
           sin(rlat * pi / 180))
    Ry <- matrix(c(cos(t0), 0, -sin(t0), 0, 1, 0, sin(t0), 0, cos(t0)), 3, byrow = TRUE)
    q <- as.numeric(Ry %*% p)
    lon <- (atan2(q[2], q[1]) * 180 / pi + 180 + plon + 180) %% 360 - 180
    c(lon, asin(q[3]) * 180 / pi)
  }
  rl <- c(-10, 0, 15)
  rt <- c(-5, 20)
  g <- RotatedGrid(rl, rt, pole_lon = -162, pole_lat = 39.25)
  for (i in 1:2) for (j in 1:3) {
    v <- rot(rl[j], rt[i], -162, 39.25)
    expect_equal(c(g$lon[i, j], g$lat[i, j]), v, tolerance = 1e-10)
  }
  # the unrotated pole is the identity
  id <- RotatedGrid(rl, rt, pole_lon = -180, pole_lat = 90)
  expect_equal(id$lon, matrix(rl, 2, 3, byrow = TRUE), tolerance = 1e-10)
  expect_equal(id$lat, matrix(rt, 2, 3), tolerance = 1e-10)
})

test_that("Propal and Propalloc allocate proportionally to stratum size", {
  Nh <- c(120, 300, 80)
  r <- Propal(500, Nh, 60)
  al <- 60 * Nh / 500
  expect_equal(r$allocation, al, tolerance = 1e-12)
  # unit S_h: V = sum N_h (N_h - n_h) / n_h
  expect_equal(r$variance, sum(Nh * (Nh - al) / al), tolerance = 1e-12)
  expect_equal(r$A, (500 - 60) / 60 * 500, tolerance = 1e-12)
  expect_equal(r$fraction, 60 / 500)
  expect_error(Propal(501, Nh, 60), "does not equal")
  expect_error(Propal(-1, Nh, 60), "must be positive")
  p <- Propalloc(c(7, 11, 5), 10)
  ex <- 10 * c(7, 11, 5) / 23
  fl <- floor(ex)
  k <- order(-(ex - fl))[seq_len(10 - sum(fl))]
  fl[k] <- fl[k] + 1
  expect_equal(p$nh, as.integer(fl))
  expect_equal(p$nh_exact, ex, tolerance = 1e-12)
  expect_equal(sum(p$nh), 10L)
  expect_error(Propalloc(c(1, 0), 3), "positive")
  expect_error(Propalloc(numeric(0), 3), "at least one stratum")
  expect_error(Propalloc(c(1, 2), -1), "non-negative")
})

test_that("Prophet components add back to the fitted values", {
  tt <- 1:30
  y <- 5 + 0.2 * tt + 1.5 * sin(2 * pi * tt / 7) + 0.8 * cos(2 * pi * tt / 7) +
    ifelse(tt %in% c(10, 20), 3, 0) + c(0.1, -0.2, 0.15, 0, -0.1, 0.2, -0.05, 0.1, -0.15, 0.05)
  seas <- list(list("weekly", 7, 2))
  hol <- list(peak = c(10, 20))
  r <- morie_prophe(tt, y, seasonalities = seas, holidays = hol, n_changepoints = 3)
  fit <- morie_prphet_fit(tt, y, seasonalities = seas, holidays = hol, n_changepoints = 3)
  cf <- fit$coef
  wk <- cf$weekly_cos1 * cos(2 * pi * tt / 7) + cf$weekly_sin1 * sin(2 * pi * tt / 7) +
    cf$weekly_cos2 * cos(4 * pi * tt / 7) + cf$weekly_sin2 * sin(4 * pi * tt / 7)
  expect_equal(r$components$weekly, wk, tolerance = 1e-12)
  expect_equal(r$components$holidays, cf$holiday_peak * (tt %in% c(10, 20)), tolerance = 1e-12)
  expect_equal(r$components$trend, fit$trend, tolerance = 1e-12)
  expect_equal(r$total, fit$fitted, tolerance = 1e-10)
  expect_true(r$reconstructs)
  expect_equal(r$component_names, c("holidays", "trend", "weekly"))
  sh <- prophe_component_shares(r$components)
  sds <- vapply(r$components, stats::sd, 0)
  expect_equal(unlist(sh$sd), sds, tolerance = 1e-12)
  expect_equal(unlist(sh$relative), sds / sum(sds), tolerance = 1e-12)
  expect_equal(sh$ranked, names(sds)[order(-sds)])
  z <- prophe_component_shares(list(a = 1, b = c(2, 2)))
  expect_equal(unlist(z$relative), c(a = 0, b = 0))
  expect_match(prophe_cheatsheet(), "DECOMPOSITION")
  expect_same_function(facebook_prophet, prophe_additive_components)
})

test_that("PropMd is NIE / TE with a sign-agreement flag", {
  r <- PropMd(0.3, 0.9)
  expect_equal(r$estimate, 0.25, tolerance = 1e-12)
  expect_equal(r$te, 1.2, tolerance = 1e-12)
  expect_equal(r$same_sign, 1)
  expect_equal(PropMd(-0.2, 0.5)$same_sign, 0)
  expect_true(is.nan(PropMd(0.5, -0.5)$estimate))
})
