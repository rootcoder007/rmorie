# Coverage for rgemgf .. ripk exports (Rangayyan EMG/VMG/VAG summaries,
# functional regression, ridge, Ripley F/G/K). Every expectation is
# recomputed in the test body.

test_that("rgemgf relates EMG summaries to force plateaus", {
  f <- c(0, 0, 5, 5, 5, 5, 0, 0, 10, 10, 10, 10, 10, 0, 0, 15, 15, 15, 15, 0)
  e <- c(1, -2, 30, -45, 52, -38, 2, -1, 80, -95, 110, -70, 88, 0, 1, 160, -150, 175, -140, 3)
  r <- rgemgf(e, f, fs = 100, turn_threshold = 50)
  ivs <- list(c(2, 6), c(8, 13), c(15, 19))
  expect_equal(r$intervals, ivs)
  seg <- lapply(ivs, function(v) (v[1] + 1):v[2])
  lev <- vapply(seg, function(i) mean(f[i]), 0)
  rms <- vapply(seg, function(i) sqrt(mean(e[i]^2)), 0)
  zc <- vapply(seg, function(i) {
    s <- ifelse(e[i] >= 0, 1, -1)
    sum(diff(s) != 0) / (length(i) - 1) * 100
  }, 0)
  expect_equal(r$force_levels, lev)
  expect_equal(r$rms, rms, tolerance = 1e-12)
  expect_equal(r$zcr, zc, tolerance = 1e-12)
  expect_equal(r$tcr, vapply(seg, function(i) TurnsCount(e[i], threshold = 50)$turns / (length(i) / 100), 0),
               tolerance = 1e-12)
  # eq. (5.28) is the squared Pearson correlation
  expect_equal(r$r2_rms, stats::cor(lev, rms)^2, tolerance = 1e-12)
  expect_equal(c(r$slope_rms, r$intercept_rms), rev(unname(stats::coef(stats::lm(rms ~ lev)))), tolerance = 1e-10)
  w <- rgemgf(e, f, fs = 100, window = 4, turn_threshold = 50)
  expect_equal(w$short_time_rms, Rms(e, window = 4)$short_time)
  expect_error(rgemgf(e, -f, 100), "no positive excursion")
  expect_error(rgemgf(e, f[-1], 100), "same length")
  expect_error(rgemgf(e, f, 0), "fs must be positive")
})

test_that("rgemgfd and rgisint work per held contraction level", {
  f <- rep(c(0, 2, 4, 6), each = 8)
  e <- c(rep(0.1, 8), 1:8, 2 * (1:8), 3 * (1:8))
  # a straight-line EMG segment has Higuchi dimension exactly 1
  d <- rgemgfd(e, f, fs = 8, kmax = 3)
  expect_equal(d$levels, c(2, 4, 6))
  expect_equal(d$fd, c(1, 1, 1), tolerance = 1e-12)
  expect_equal(d$intervals, list(c(8, 16), c(16, 24), c(24, 32)))
  e2 <- e + rep(c(0, 0.5, -0.3, 0.8, -0.6, 0.2, 0.4, -0.1), 4)
  d2 <- rgemgfd(e2, f, fs = 8, kmax = 3)
  hig <- function(x, kmax) {
    N <- length(x)
    L <- vapply(1:kmax, function(k) {
      mean(vapply(1:k, function(m) {
        idx <- seq(m, N, by = k)
        sum(abs(diff(x[idx]))) / k * (N - 1) / (k * (length(idx) - 1))
      }, 0))
    }, 0)
    unname(stats::coef(stats::lm(log(L) ~ log(1 / (1:kmax))))[2])
  }
  expect_equal(d2$fd, vapply(1:3, function(q) hig(e2[(8 * q + 1):(8 * q + 8)], 3), 0), tolerance = 1e-10)
  expect_error(rgemgfd(e, f, fs = 2), "too low")
  expect_error(rgemgfd(e, rep(0, 32), fs = 8), "two usable")
  g <- rgisint(e2, f, fs = 8)
  expect_equal(g$levels, c(2, 4, 6))
  expect_equal(g$rms, vapply(1:3, function(q) sqrt(mean(e2[(8 * q + 1):(8 * q + 8)]^2)), 0), tolerance = 1e-12)
  expect_equal(g$durations, rep(1, 3))
  expect_equal(g$r2, stats::cor(g$levels, g$rms)^2, tolerance = 1e-12)
  expect_error(rgisint(e2, rep(c(0, 2), each = 16), 8), "at least two held")
})

test_that("rgkneejt flags runs of the RMS envelope above the friction ratio", {
  x <- c(0.1, -0.1, 0.12, -0.08, 2.0, -1.8, 2.2, 0.1, -0.1, 0.09, -0.1, 1.9, -2.1, 0.1, -0.12, 0.1)
  r <- rgkneejt(x, fs = 100, force = 1.5, window = 2)
  env <- vapply(seq_along(x), function(i) sqrt(mean(x[max(1, i - 1):i]^2)), 0)
  thr <- 1.5 * stats::median(env)
  sl <- as.integer(env > thr)
  rl <- rle(sl)
  starts <- cumsum(c(1, rl$lengths))[seq_along(rl$lengths)]
  on <- starts[rl$values == 1] - 1
  expect_equal(r$envelope, env, tolerance = 1e-12)
  expect_equal(r$threshold, thr, tolerance = 1e-12)
  expect_equal(r$slip, sl)
  expect_equal(r$onsets, as.integer(on))
  expect_equal(r$burst_rate, length(on) / (16 / 100), tolerance = 1e-12)
  expect_equal(r$mean_interval, mean(diff(on)) / 100, tolerance = 1e-12)
  expect_equal(rgkneejt(x, fs = 1000)$window, 10L)
  expect_error(rgkneejt(x, 100, force = 0.5), "at least 1")
  expect_error(rgkneejt(1:2, 100), "three samples")
})

test_that("rgvmg reports RMS, zero crossings and the periodogram summaries", {
  tt <- (0:63) / 256
  x <- sin(2 * pi * 20 * tt) + 0.5 * sin(2 * pi * 60 * tt) + 0.1 * cos(2 * pi * 110 * tt)
  r <- rgvmg(x, fs = 256, band = c(10, 50))
  P <- Mod(stats::fft(x))^2 / 64
  P <- P[1:33]
  fr <- (0:32) * 256 / 64
  expect_equal(r$rms, sqrt(mean(x^2)), tolerance = 1e-12)
  expect_equal(r$psd, P, tolerance = 1e-10)
  expect_equal(r$mean_frequency, sum(fr * P) / sum(P), tolerance = 1e-10)
  expect_equal(r$median_frequency, fr[which(cumsum(P) >= 0.5 * sum(P))[1]])
  expect_equal(r$band_power_fraction, sum(P[fr >= 10 & fr <= 50]) / sum(P), tolerance = 1e-10)
  s <- ifelse(x >= 0, 1, -1)
  expect_equal(r$zcr, sum(diff(s) != 0) / 63 * 256, tolerance = 1e-12)
  expect_true(is.na(rgvmg(rep(0, 8), 10)$mean_frequency))
  expect_error(rgvmg(x, 256, band = c(50, 10)), "increasing")
  expect_error(rgvmg(1, 10), "two samples")
})

test_that("morie_rgs is functional linear regression on trapezoid-weighted FPCs", {
  tg <- seq(0, 1, length.out = 6)
  X <- rbind(sin(tg), cos(tg), tg^2, 1 - tg, sin(2 * tg), exp(-tg), tg * (1 - tg))
  y <- c(0.5, 1.2, 0.3, 0.9, 0.7, 1.0, 0.2)
  r <- morie_rgs_functional_regression(X, y, basis = 3)
  w <- c(0.1, rep(0.2, 4), 0.1)
  Xc <- sweep(X, 2, colMeans(X))
  C <- crossprod(Xc) / 7
  e <- eigen(outer(sqrt(w), sqrt(w)) * C, symmetric = TRUE)
  phi <- e$vectors[, 1:3] / sqrt(w)
  sc <- Xc %*% (phi * w)
  b <- colSums(sc * (y - mean(y))) / 7 / e$values[1:3]
  # beta and the fit are invariant to the eigenvector signs
  expect_equal(r$beta, as.numeric(phi %*% b), tolerance = 1e-10)
  expect_equal(r$fitted, mean(y) + as.numeric(sc %*% b), tolerance = 1e-10)
  expect_equal(r$eigenvalues, e$values[1:3], tolerance = 1e-10)
  expect_equal(r$r_squared, 1 - sum((y - r$fitted)^2) / sum((y - mean(y))^2), tolerance = 1e-12)
  ra <- morie_rgs_functional_regression(X, y)
  expect_equal(ra$k, min(which(cumsum(e$values) / sum(e$values) >= 0.99)))
  expect_same_function(morie_rgs, morie_rgs_functional_regression)
  expect_error(morie_rgs_functional_regression(X, y[-1]), "responses")
  expect_error(morie_rgs_functional_regression(X[1, , drop = FALSE], 1), "at least two curves")
  expect_error(morie_rgs_functional_regression(X, y, basis = 0), "at least 1")
  expect_error(morie_rgs_functional_regression(X, y, basis = matrix(1, 4, 2)), "rows for a grid")
})

test_that("ridge objective, closed form and lm.ridge scaling", {
  X <- cbind(1:10, c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9))
  y <- c(1.2, 2.3, 2.9, 4.1, 5.2, 5.8, 7.3, 8.1, 8.9, 10.2)
  b <- c(0.3, 0.6, 0.35)
  o <- Ridgeobj(X, y, b, 2)
  rss <- sum((y - cbind(1, X) %*% b)^2)
  expect_equal(o$rss, rss, tolerance = 1e-12)
  expect_equal(o$prss, rss + 2 * sum(b[2:3]^2), tolerance = 1e-12)
  expect_equal(Ridgeobj(X, y, b[2:3], 2, add_intercept = FALSE)$penalty, 2 * sum(b[2:3]^2), tolerance = 1e-12)
  expect_error(Ridgeobj(X, y, b, -1), "non-negative")
  expect_error(Ridgeobj(X, y, b[1:2], 1), "one entry per column")
  s <- Ridgesol(X, y, 2)
  Z <- cbind(1, X)
  bs <- solve(crossprod(Z) + diag(c(0, 2, 2)), crossprod(Z, y))
  expect_equal(s$beta, as.numeric(bs), tolerance = 1e-10)
  expect_equal(s$prss, sum((y - Z %*% bs)^2) + 2 * sum(bs[2:3]^2), tolerance = 1e-10)
  expect_error(Ridgesol(X, y, -1), "non-negative")
  r <- ridgrg(X, y, 0.5)
  skip_if_not_installed("MASS")
  m <- MASS::lm.ridge(y ~ X, lambda = 0.5)
  expect_equal(c(r$intercept, r$coefficients), unname(stats::coef(m)), tolerance = 1e-10)
  # MASS reports RSS / (n - df)^2; the Golub-Heath-Wahba GCV is n times that
  expect_equal(r$gcv, 10 * unname(m$GCV), tolerance = 1e-10)
  expect_same_function(morie_ridgrg, ridgrg)
})

test_that("Ripley F, G and K functions", {
  P <- cbind(c(0.12, 0.35, 0.8, 0.52, 0.9, 0.22, 0.61, 0.44, 0.7, 0.3),
             c(0.2, 0.75, 0.4, 0.5, 0.95, 0.44, 0.13, 0.28, 0.66, 0.9))
  win <- c(0, 1, 0, 1)
  rs <- c(0.05, 0.1, 0.2)
  D <- as.matrix(stats::dist(P))
  diag(D) <- Inf
  nn <- apply(D, 1, min)
  bd <- pmin(P[, 1], 1 - P[, 1], P[, 2], 1 - P[, 2])
  g <- morie_ripley_g_function(P, win, rs)
  expect_equal(g$g, vapply(rs, function(h) mean(nn <= h), 0))
  expect_equal(g$g_border, vapply(rs, function(h) sum(bd > h & nn <= h) / sum(bd > h), 0), tolerance = 1e-12)
  expect_equal(g$mean_nn, mean(nn), tolerance = 1e-12)
  u <- expand.grid(b = (1:20 - 0.5) / 20, a = (1:20 - 0.5) / 20)
  U <- cbind(u$a, u$b)
  dm <- apply(U, 1, function(q) min(sqrt((P[, 1] - q[1])^2 + (P[, 2] - q[2])^2)))
  bm <- pmin(U[, 1], 1 - U[, 1], U[, 2], 1 - U[, 2])
  f <- RipF(P, win, rs)
  expect_equal(f$f, vapply(rs, function(h) mean(dm <= h), 0))
  expect_equal(f$f_border, vapply(rs, function(h) sum(bm > h & dm <= h) / sum(bm > h), 0), tolerance = 1e-12)
  expect_same_function(morie_ripley_f_function, RipF)
  k <- morie_ripley_k_function(P, win, rs)
  diag(D) <- 0
  expect_equal(k$k_border, vapply(rs, function(h) sum((D <= h & D > 0)[bd > h, ]) / (10 * sum(bd > h)), 0),
               tolerance = 1e-12)
  tr <- outer(1:10, 1:10, Vectorize(function(i, j) 1 / ((1 - abs(P[i, 1] - P[j, 1])) * (1 - abs(P[i, 2] - P[j, 2])))))
  expect_equal(k$k_trans, vapply(rs, function(h) sum(tr[D <= h & D > 0]) / 100, 0), tolerance = 1e-12)
  skip_if_not_installed("spatstat.geom")
  skip_if_not_installed("spatstat.explore")
  X <- spatstat.geom::ppp(P[, 1], P[, 2], window = spatstat.geom::owin(c(0, 1), c(0, 1)))
  ew <- spatstat.explore::edge.Ripley(X, D)
  expect_equal(k$k, vapply(rs, function(h) sum(ew[D <= h & D > 0]) / 100, 0), tolerance = 1e-10)
  expect_equal(k$l, sqrt(k$k / pi), tolerance = 1e-12)
  expect_error(morie_ripley_k_function(P[1, , drop = FALSE], win, rs), "at least 2")
  expect_error(RipF(P, c(0, 1, 0), rs), "window")
  expect_error(morie_ripley_g_function(P + 2, win, rs), "inside")
  expect_error(RipF(P, win, -1), "non-negative")
})
