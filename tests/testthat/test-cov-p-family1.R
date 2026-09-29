# Coverage for Outms .. paligi exports. Every expectation is recomputed in
# the test body.

test_that("Outms applies the mean/SD outlier rule", {
  x <- c(2.1, 2.5, 1.9, 2.2, 2.4, 9.0, 2.0, -3.0)
  r <- Outms(x)
  dis <- abs(x - mean(x)) / stats::sd(x)
  expect_equal(r$dis, dis, tolerance = 1e-12)
  expect_equal(r$which, which(dis > 2))
  expect_equal(r$n_out, sum(dis > 2))
  expect_equal(c(r$center, r$scale), c(mean(x), stats::sd(x)), tolerance = 1e-12)
  expect_equal(Outms(x, crit = 1)$which, which(dis > 1))
  expect_error(Outms(1), "at least 2")
  expect_error(Outms(c(1, NA)), "missing")
  expect_error(Outms(x, crit = -1), "crit must be positive")
  expect_error(Outms(c(3, 3, 3)), "standard deviation is zero")
})

test_that("Ovbnk is Oster's proportional-selection bound", {
  r <- Ovbnk(0.5, 0.4, 0.2, 0.3, R_max = 0.6, delta = 1.2)
  sc <- (0.6 - 0.3) / (0.3 - 0.2)
  bias <- 1.2 * (0.5 - 0.4) * sc
  expect_equal(r$bias, bias, tolerance = 1e-12)
  expect_equal(r$beta_star, 0.4 - bias, tolerance = 1e-12)
  expect_equal(r$delta_star, 0.4 * (0.3 - 0.2) / ((0.5 - 0.4) * (0.6 - 0.3)), tolerance = 1e-12)
  # delta = delta_star drives beta_star to zero
  z <- Ovbnk(0.5, 0.4, 0.2, 0.3, R_max = 0.6, delta = r$delta_star)
  expect_equal(z$beta_star, 0, tolerance = 1e-12)
  expect_equal(c(r$bound_lower, r$bound_upper), sort(c(0.4 - bias, 0.4)), tolerance = 1e-12)
  expect_equal(r$sign_stable, as.numeric((0.4 - bias) * 0.4 > 0))
  expect_identical(Ovbnk(0.4, 0.4, 0.2, 0.3)$delta_star, Inf)
  expect_error(Ovbnk(0.5, 0.4, 0.3, 0.2), "R_long must exceed")
  expect_error(Ovbnk(0.5, 0.4, 0.2, 0.3, R_max = 0.25), "at least R_long")
  expect_error(Ovbnk(0.5, 0.4, 0.2, 0.3, R_max = 1.5), "lie in \\[0, 1\\]")
})

test_that("morie_over reports the common-support range and the Smirnov test", {
  pt <- c(0.31, 0.45, 0.52, 0.66, 0.71, 0.83, 0.9)
  pc <- c(0.12, 0.2, 0.28, 0.35, 0.47, 0.58, 0.61, 0.74)
  r <- morie_over(pt, pc)
  lo <- max(min(pt), min(pc))
  hi <- min(max(pt), max(pc))
  expect_equal(r$overlap_range, c(lo, hi))
  out <- sum(pt < lo | pt > hi) + sum(pc < lo | pc > hi)
  expect_equal(r$pct_trimmed, 100 * out / 15, tolerance = 1e-12)
  kt <- stats::ks.test(pt, pc, exact = TRUE)
  expect_equal(r$ks_stat, unname(kt$statistic), tolerance = 1e-12)
  expect_equal(r$ks_p, kt$p.value, tolerance = 1e-12)
  # ties: asymptotic Kolmogorov series with Stephens' correction
  a <- c(0.2, 0.3, 0.3, 0.5)
  b <- c(0.3, 0.6, 0.7)
  rt <- morie_over(a, b)
  Fa <- stats::ecdf(a)
  Fb <- stats::ecdf(b)
  u <- sort(unique(c(a, b)))
  D <- max(abs(Fa(u) - Fb(u)))
  en <- 12 / 7
  lam <- D * (sqrt(en) + 0.12 + 0.11 / sqrt(en))
  j <- 1:100
  expect_equal(rt$ks_stat, D, tolerance = 1e-12)
  expect_equal(rt$ks_p, min(1, max(0, sum(2 * (-1)^(j - 1) * exp(-2 * j^2 * lam^2)))),
               tolerance = 1e-12)
  # disjoint supports: empty overlap
  expect_equal(morie_over(c(0.8, 0.9), c(0.1, 0.2))$overlap_range, c(0, 0))
  expect_error(morie_over(numeric(0), pc), "non-empty")
})

test_that("Owltrn runs Pegasos on the outcome-weighted hinge loss", {
  W <- cbind(c(0.2, -1.0, 0.5, 1.3, -0.4, 0.9, -0.7, 0.1), c(1, 0, 0, 1, 1, 0, 1, 0))
  D <- c(1, 0, 1, 1, 0, 0, 1, 0)
  y <- c(2.0, -0.5, 1.5, 3.0, 0.2, 0.8, 1.1, -1.0)
  lam <- 0.05
  r <- Owltrn(y, D, W, lam = lam, n_iter = 300)
  X <- cbind(1, W)
  pt <- mean(D)
  pv <- ifelse(D == 1, pt, 1 - pt)
  ys <- y + 1
  w <- ys / pv
  w <- w / mean(w)
  lab <- 2 * D - 1
  b <- numeric(3)
  for (t in 1:300) {
    g <- c(0, lam * b[2:3])
    act <- lab * as.numeric(X %*% b) < 1
    g <- g - colSums((w * lab)[act] * X[act, , drop = FALSE]) / 8
    b <- b - g / (lam * t)
  }
  expect_equal(r$beta, b, tolerance = 1e-10)
  f <- as.numeric(X %*% b)
  expect_equal(r$hinge, mean(w * pmax(0, 1 - lab * f)) + lam * sum(b[2:3]^2), tolerance = 1e-10)
  rule <- as.numeric(f > 0)
  expect_equal(r$rule, rule)
  m <- rule == D
  expect_equal(r$value, sum(ys * m / pv) / sum(m / pv), tolerance = 1e-12)
  expect_equal(r$shift, 1)
  expect_error(Owltrn(y, D + 1, W), "binary")
  expect_error(Owltrn(y, D, W, lam = 0), "lam must be positive")
  expect_error(Owltrn(y, D, W, n_iter = 0), "at least 1")
  expect_error(Owltrn(y, rep(0, 8), W), "both treatments")
  expect_error(Owltrn(y, D, W, pi = rep(2, 8)), "pi must lie")
})

test_that("Paa averages equal-width (possibly fractional) segments", {
  x <- c(1, 4, 2, 8, 5, 7, 3)
  for (N in c(1, 2, 3, 7)) {
    # expand every sample N times so each segment covers exactly n points
    up <- rep(x, each = N)
    want <- vapply(seq_len(N), function(i) mean(up[((i - 1) * 7 + 1):(i * 7)]), 0)
    expect_equal(Paa(x, N)$paa, want, tolerance = 1e-12)
  }
  expect_error(Paa(x, 8), "1 <= N <= n")
  expect_error(Paa(numeric(0), 1), "empty")
})

test_that("morie_pace_local_linear and _2d are kernel-weighted local linear fits", {
  t <- c(0, 0.1, 0.25, 0.4, 0.5, 0.65, 0.8, 0.9, 1)
  y <- c(1, 1.3, 1.1, 1.8, 2.0, 2.4, 2.2, 2.9, 3.1)
  at <- c(0.2, 0.55, 0.95)
  for (k in c("epan", "gauss")) {
    got <- morie_pace_local_linear(t, y, at, 0.3, kernel = k)
    want <- vapply(at, function(t0) {
      u <- (t - t0) / 0.3
      w <- if (k == "epan") pmax(0.75 * (1 - u^2), 0) else exp(-u^2 / 2)
      unname(stats::lm.wfit(cbind(1, t - t0)[w > 0, ], y[w > 0], w[w > 0])$coefficients[1])
    }, 0)
    expect_equal(got, want, tolerance = 1e-10)
  }
  expect_error(morie_pace_local_linear(t, y, 5, 0.3), "no data")
  expect_error(morie_pace_local_linear(t, y, at, 0), "bandwidth must be positive")
  expect_error(morie_pace_local_linear(t, y, at, 0.3, kernel = "box"), "epan or gauss")
  s <- rep(c(0, 0.3, 0.6, 1), 3)
  tt <- rep(c(0, 0.5, 1), each = 4)
  z <- s + 2 * tt + c(0.1, -0.1, 0.05, 0, 0.2, -0.05, 0.1, -0.2, 0, 0.1, -0.1, 0.05)
  g <- morie_pace_local_linear_2d(s, tt, z, c(0.3, 0.6), c(0.5), 0.8)
  want <- vapply(c(0.3, 0.6), function(s0) {
    w <- pmax(0.75 * (1 - ((s - s0) / 0.8)^2), 0) * pmax(0.75 * (1 - ((tt - 0.5) / 0.8)^2), 0)
    k <- w > 0
    unname(stats::lm.wfit(cbind(1, s - s0, tt - 0.5)[k, ], z[k], w[k])$coefficients[1])
  }, 0)
  # the function solves the normal equations with a 1e-10 ridge
  expect_equal(as.numeric(g), want, tolerance = 1e-8)
  expect_error(morie_pace_local_linear_2d(s, tt, z, 0.3, 0.5, -1), "bandwidth must be positive")
})

test_that("morie_pace returns orthonormal eigenfunctions and consistent fits", {
  Y <- list(c(1.0, 1.6, 2.1), c(0.4, 0.9, 1.7, 2.0), c(1.5, 2.2), c(0.8, 1.1, 1.9))
  tv <- list(c(0, 0.4, 0.9), c(0.1, 0.3, 0.6, 1), c(0.2, 0.8), c(0, 0.5, 0.7))
  r <- morie_pace(Y, tv, K = 2, n_grid = 11, bw_mu = 0.35, bw_cov = 0.6)
  gr <- seq(0, 1, length.out = 11)
  dt <- 0.1
  expect_equal(r$grid, gr, tolerance = 1e-12)
  expect_equal(r$mean, morie_pace_local_linear(unlist(tv), unlist(Y), gr, 0.35), tolerance = 1e-12)
  for (j in 1:2) expect_equal(sum(r$eigenfunctions[[j]]^2) * dt, 1, tolerance = 1e-10)
  expect_equal(sum(r$eigenfunctions[[1]] * r$eigenfunctions[[2]]) * dt, 0, tolerance = 1e-10)
  expect_true(all(diff(r$fve) >= 0) && r$fve[2] <= 1 + 1e-12)
  for (i in 1:4) {
    fit <- r$mean + r$scores[[i]][1] * r$eigenfunctions[[1]] + r$scores[[i]][2] * r$eigenfunctions[[2]]
    expect_equal(r$fitted[[i]], fit, tolerance = 1e-12)
  }
  # without shrinkage the scores are trapezoid integrals of the centred curve
  r2 <- morie_pace(Y, tv, K = 1, n_grid = 11, bw_mu = 0.35, bw_cov = 0.6, shrink = FALSE)
  ph <- stats::approxfun(gr, r2$eigenfunctions[[1]])
  mu <- stats::approxfun(gr, r2$mean)
  cen <- Y[[2]] - mu(tv[[2]])
  f <- cen * ph(tv[[2]])
  expect_equal(r2$scores[[2]], sum(diff(tv[[2]]) * (f[-1] + f[-4]) / 2), tolerance = 1e-12)
  expect_error(morie_pace(list(), list()), "no subjects")
  expect_error(morie_pace(Y, tv[1:3]), "time vectors")
  expect_error(morie_pace(list(1, 2, 3), list(0, 0.5, 1)), "no off-diagonal pairs")
  expect_error(morie_pace(Y, tv, n_grid = 2), "n_grid must be at least 3")
})

test_that("Pagehink accumulates the Page-Hinkley statistic", {
  x <- c(1, 1.2, 0.9, 1.1, 1, 3, 3.2, 2.9, 3.1)
  ph <- function(x, sgn, delta) {
    m <- cumsum(sgn * (x - cumsum(x) / seq_along(x)) - delta)
    m - cummin(m)
  }
  r <- Pagehink(x, threshold = 1.5, delta = 0.01)
  p <- ph(x, 1, 0.01)
  expect_equal(r$ph, p, tolerance = 1e-12)
  expect_equal(r$changepoint, which(p > 1.5)[1])
  expect_true(r$detected)
  rd <- Pagehink(rev(x), threshold = 1.5, direction = "decrease")
  expect_equal(rd$ph, ph(rev(x), -1, 0.005), tolerance = 1e-12)
  expect_false(Pagehink(x, threshold = 100)$detected)
  expect_error(Pagehink(numeric(0), 1), "empty stream")
  expect_error(Pagehink(x, 1, direction = "up"), "increase' or 'decrease")
})

test_that("Pagrk is PageRank with uniform teleport and dangling redistribution", {
  A <- matrix(c(0, 1, 1, 0,
                0, 0, 1, 0,
                1, 0, 0, 0,
                0, 0, 0, 0), 4, byrow = TRUE)
  r <- Pagrk(A, alpha = 0.85, n_iter = 200)
  P <- A / ifelse(rowSums(A) > 0, rowSums(A), 1)
  P[rowSums(A) == 0, ] <- 1 / 4
  G <- 0.85 * P + 0.15 / 4
  e <- eigen(t(G))
  pr <- Re(e$vectors[, 1])
  pr <- pr / sum(pr)
  expect_equal(r$pr, pr, tolerance = 1e-10)
  expect_equal(r$top, which.max(pr) - 1L)
  expect_equal(sum(r$pr), 1, tolerance = 1e-12)
})

test_that("Paligi is ALiBi attention with fixed linear distance penalties", {
  Q <- matrix(c(0.2, -0.5, 1.0, 0.3, 0.7, -0.2), 3)
  K <- matrix(c(0.5, 0.1, -0.4, 0.9, 0.2, 0.6), 3)
  V <- matrix(c(1, 2, 3, -1, 0, 1), 3)
  sm <- function(M) exp(M - apply(M, 1, max)) / rowSums(exp(M - apply(M, 1, max)))
  Bm <- -0.5 * abs(outer(1:3, 1:3, "-"))
  W <- sm(Q %*% t(K) / sqrt(2) + Bm)
  r <- Paligi(Q = Q, K = K, V = V, slopes = 0.5)
  expect_equal(r$weights, W, tolerance = 1e-12)
  expect_equal(r$output, W %*% V, tolerance = 1e-12)
  Bc <- Bm
  Bc[upper.tri(Bc)] <- -Inf
  rc <- Paligi(Q = Q, K = K, V = V, slopes = c(0.5, 0.25), causal = TRUE)
  Wc <- sm(Q %*% t(K) / sqrt(2) + Bc)
  expect_equal(rc$output[[1]], Wc %*% V, tolerance = 1e-12)
  expect_equal(rc$weights[1, ], c(1, 0, 0))
  expect_error(Paligi(Q = Q, K = K), "all required")
  expect_error(Paligi(Q = Q, K = K[, 1, drop = FALSE], V = V), "key dimension")
})
