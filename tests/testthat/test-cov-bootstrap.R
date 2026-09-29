# Coverage for the bootstrap files (bt*.R). The resampling streams are
# rebuilt in the test from the documented generators (Park-Miller 48271
# for .t1_lcg, 16807 for the pairs/quantile/test routines, the radical
# inverse for the quasi-random counts, SplitMix64 for the *_native ones)
# and every interval, standard error and p-value is recomputed from them.

pm_m <- 2147483647
lcg_ref <- function(seed) {
  s <- seed %% pm_m
  if (s <= 0) s <- 1
  list(unif = function() {
    s <<- (48271 * s) %% pm_m
    s / pm_m
  })
}
pm16807_ref <- function(seed, n) {
  s <- seed %% pm_m
  if (s <= 0) s <- s + pm_m - 1
  function() {
    s <<- (16807 * s) %% pm_m
    min(floor((s - 1) / (pm_m - 1) * n), n - 1) + 1
  }
}
q7 <- function(v, p) unname(stats::quantile(v, p, type = 7))
idx_ref <- function(g, n, m = n) {
  vapply(seq_len(m), function(i) min(floor(g$unif() * n), n - 1) + 1, numeric(1))
}
ridge_ls <- function(X, y) {
  as.numeric(solve(crossprod(X) + diag(1e-10, ncol(X)), crossprod(X, y)))
}
radinv <- function(i, base) {
  f <- 1
  r <- 0
  k <- i + 1
  while (k > 0) {
    f <- f / base
    r <- r + f * (k %% base)
    k <- k %/% base
  }
  r
}
counts_ref <- function(n, B, rng) {
  pr <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)
  out <- matrix(0, B, n)
  for (b in seq_len(B)) {
    for (i in seq_len(n)) {
      j <- min(floor(radinv(b - 1, pr[rng - 2 + i]) * n) + 1, n)
      out[b, j] <- out[b, j] + 1
    }
  }
  out
}
bx <- c(2.1, 3.7, 1.4, 5.2, 4.4, 2.9, 6.1, 3.3, 4.8, 2.2, 5.5, 3.9)
bX <- cbind(1, c(0.5, 1.2, -0.3, 2.1, 1.7, 0.1, 2.8, 0.9, 1.9, -0.6, 2.4, 1.1))
by <- as.numeric(bX %*% c(1, 2)) + c(0.3, -0.2, 0.5, -0.4, 0.1, 0.2, -0.3, 0.6, -0.5, 0.1, 0.2, -0.1)

test_that("percentile-type intervals from given replicates", {
  tb <- c(1.2, 0.8, 1.9, 1.4, 1.1, 0.7, 1.6, 1.3, 2.2, 0.9)
  r <- Btbasic(1.25, tb, alpha = 0.1)
  expect_equal(r$lo, 2.5 - q7(tb, 0.95), tolerance = 1e-12)
  expect_equal(r$hi, 2.5 - q7(tb, 0.05), tolerance = 1e-12)
  expect_error(Btbasic(1, numeric(0)), "no bootstrap")
  expect_error(Btbasic(1, tb, alpha = 1), "strictly")
  nr <- Btnorm(1.25, tb, alpha = 0.1)
  bias <- mean(tb) - 1.25
  expect_equal(nr$lo, 1.25 - bias - qnorm(0.95) * sd(tb), tolerance = 1e-12)
  expect_equal(nr$hi, 1.25 - bias + qnorm(0.95) * sd(tb), tolerance = 1e-12)
  d <- tb - mean(tb)
  expect_equal(nr$skew, mean(d^3) / mean(d^2)^1.5, tolerance = 1e-12)
  expect_error(Btnorm(1, 1), "at least two")
  expect_error(Btnorm(1, tb, alpha = 0), "strictly")
  st <- Btstud(1.25, 0.3, tb - 1.3, alpha = 0.1)
  expect_equal(st$lo, 1.25 - q7(tb - 1.3, 0.95) * 0.3, tolerance = 1e-12)
  expect_equal(st$hi, 1.25 - q7(tb - 1.3, 0.05) * 0.3, tolerance = 1e-12)
  expect_error(Btstud(1, 0, tb), "positive")
  expect_error(Btstud(1, 1, numeric(0)), "no studentised")
  expect_error(Btstud(1, 1, tb, alpha = 2), "strictly")
  pc <- Bootpct(tb, alpha = 0.1, theta_hat = 1.25)
  z0 <- qnorm(mean(tb < 1.25))
  expect_equal(c(pc$lo, pc$hi), c(q7(tb, 0.05), q7(tb, 0.95)), tolerance = 1e-12)
  expect_equal(pc$z0, z0, tolerance = 1e-12)
  expect_equal(pc$bc_lo, q7(tb, pnorm(2 * z0 + qnorm(0.05))), tolerance = 1e-12)
  expect_equal(pc$bc_hi, q7(tb, pnorm(2 * z0 + qnorm(0.95))), tolerance = 1e-12)
  expect_equal(Bootpct(tb, theta_hat = 0)$z0, qnorm(0.05), tolerance = 1e-12)
})

test_that("Btbca follows Davison-Hinkley (5.21)-(5.27)", {
  x <- bx
  t0 <- mean(x)
  tb <- t0 + c(-0.5, 0.3, 0.1, -0.2, 0.6, -0.1, 0.4, -0.3, 0.2, 0.05, -0.45, 0.35)
  r <- Btbca(t0, tb, x, mean, alpha = 0.1)
  w <- qnorm(sum(tb <= t0) / 13)
  l <- 11 * (t0 - vapply(1:12, function(j) mean(x[-j]), numeric(1)))
  acc <- sum(l^3) / (6 * sum(l^2)^1.5)
  alo <- pnorm(w + (w + qnorm(0.05)) / (1 - acc * (w + qnorm(0.05))))
  ahi <- pnorm(w + (w + qnorm(0.95)) / (1 - acc * (w + qnorm(0.95))))
  expect_equal(r$z0, w, tolerance = 1e-12)
  expect_equal(r$accel, acc, tolerance = 1e-12)
  expect_equal(c(r$alpha_lo, r$alpha_hi), c(alo, ahi), tolerance = 1e-12)
  expect_equal(c(r$lo, r$hi), c(q7(tb, alo), q7(tb, ahi)), tolerance = 1e-12)
  sv <- sort(tb)
  ord <- function(rr) {
    if (rr <= 1) return(sv[1])
    if (rr >= 12) return(sv[12])
    sv[floor(rr)] + (rr - floor(rr)) * (sv[floor(rr) + 1] - sv[floor(rr)])
  }
  expect_equal(r$lo_order, ord(13 * alo), tolerance = 1e-12)
  expect_equal(r$hi_order, ord(13 * ahi), tolerance = 1e-12)
  expect_error(Btbca(1, numeric(0), x, mean), "no bootstrap")
  expect_error(Btbca(1, tb, 1, mean), "two observations")
  expect_error(Btbca(1, tb, x, mean, alpha = 1), "strictly")
})

test_that("Btbg averages or votes over bagged predictions", {
  M <- rbind(c(1, 2, 0), c(3, 2, 1), c(2, 5, 1), c(2, 2, 0))
  r <- Btbg(M)
  expect_equal(r$y_pred, colMeans(M), tolerance = 1e-12)
  expect_equal(r$vote_share, apply(M, 2, sd), tolerance = 1e-12)
  cl <- Btbg(M, kind = "classification")
  expect_equal(cl$y_pred, c(2, 2, 0))
  expect_equal(cl$vote_share, c(0.5, 0.75, 0.5))
  expect_equal(Btbg(M[1, , drop = FALSE])$vote_share, c(0, 0, 0))
  expect_error(Btbg(M, X_new = 1:2), "case count")
  expect_error(Btbg(M, kind = "x"), "regression or classification")
})

test_that("Btdir and Btbayes use uniform-gap Dirichlet weights", {
  g <- lcg_ref(5)
  W <- lapply(1:4, function(b) diff(c(0, sort(c(g$unif(), g$unif())), 1)))
  r <- Btdir(3, B = 4, rng = 5)
  expect_equal(r$W, W, tolerance = 1e-12)
  expect_equal(r$w_mean, 1 / 3, tolerance = 1e-12)
  expect_equal(r$w_var, mean(unlist(W)^2) - 1 / 9, tolerance = 1e-12)
  expect_lt(r$rowsum_max_err, 1e-12)
  expect_equal(Btdir(1, B = 2)$W, list(1, 1))
  expect_error(Btdir(0), "at least 1")
  x <- bx[1:5]
  g <- lcg_ref(3)
  th <- vapply(1:6, function(b) sum(diff(c(0, sort(vapply(1:4, function(i) g$unif(), 0)), 1)) * x), 0)
  bb <- Btbayes(x, B = 6, seed = 3)
  expect_equal(bb$theta_b, th, tolerance = 1e-12)
  expect_equal(bb$se, sd(th), tolerance = 1e-12)
  expect_equal(c(bb$lo, bb$hi), c(q7(th, 0.025), q7(th, 0.975)), tolerance = 1e-12)
  expect_equal(bb$var_closed, sum((x - mean(x))^2) / 30, tolerance = 1e-12)
  expect_true(is.nan(Btbayes(x, stat = function(x, w) max(w), B = 3)$var_closed))
  expect_error(Btbayes(numeric(0)), "at least one")
  expect_error(Btbayes(x, B = 1), "two replicates")
})

test_that("Btblen follows Politis-White (2004)", {
  x <- sin(1:40 / 3) + c(0.2, -0.1, 0.4, -0.3)[(1:40) %% 4 + 1]
  n <- 40
  r <- Btblen(x)
  xb <- mean(x)
  R <- vapply(0:39, function(k) sum((x[1:(n - k)] - xb) * (x[(1 + k):n] - xb)) / n, 0)
  kn <- 5
  thr <- 2 * sqrt(log10(n) / n)
  mh <- NA
  for (m in 1:min(ceiling(sqrt(n)) + kn, 39)) {
    kk <- m + seq_len(kn)
    kk <- kk[kk <= 39]
    if (all(abs(R[kk + 1] / R[1]) < thr)) {
      mh <- m
      break
    }
  }
  if (is.na(mh)) mh <- min(ceiling(sqrt(n)) + kn, 39)
  M <- min(2 * mh, 39)
  lam <- function(t) ifelse(abs(t) <= 0.5, 1, ifelse(abs(t) <= 1, 2 * (1 - abs(t)), 0))
  k <- -M:M
  G <- sum(lam(k / M) * abs(k) * R[abs(k) + 1])
  g0 <- sum(lam(k / M) * R[abs(k) + 1])
  gh <- function(w) vapply(w, function(u) sum(lam(k / M) * R[abs(k) + 1] * cos(u * k)), 0)
  integ <- stats::integrate(function(w) (1 + cos(w)) * gh(w)^2, -pi, pi,
                            rel.tol = 1e-12)$value
  dsb <- 4 * g0^2 + 2 / pi * integ
  expect_equal(r$m_hat, as.integer(mh))
  expect_equal(r$G_hat, G, tolerance = 1e-12)
  expect_equal(r$g0, g0, tolerance = 1e-12)
  # the periodic trapezoid rule on 2000 panels is spectrally accurate
  expect_equal(r$D_sb, dsb, tolerance = 1e-9)
  expect_equal(r$b_cb, (2 * G^2 / (4 / 3 * g0^2))^(1 / 3) * n^(1 / 3), tolerance = 1e-12)
  expect_equal(r$ell, as.integer(min(max(floor(r$b_cb + 0.5), 1), n)))
  expect_equal(Btblen(x, method = "stationary")$ell,
               as.integer(min(max(floor(r$b_sb + 0.5), 1), n)))
  expect_error(Btblen(1:3), "four")
  expect_error(Btblen(x, method = "x"), "circular")
  expect_error(Btblen(x, c = 0), "positive")
  expect_error(Btblen(rep(1, 6)), "zero variance")
})

test_that("Btmbb and Btcbb rebuild moving and circular blocks", {
  x <- bx
  n <- 12
  ell <- 3
  blocks <- function(seed, B, circ) {
    g <- lcg_ref(seed)
    st <- if (circ) n else n - ell + 1
    vapply(1:B, function(b) {
      smp <- unlist(lapply(1:4, function(j) {
        s <- min(floor(g$unif() * st), st - 1)
        x[((s + 0:(ell - 1)) %% n) + 1]
      }))
      median(smp[1:n])
    }, 0)
  }
  r <- Btmbb(x, block_len = 3, stat = median, B = 7, seed = 2, alpha = 0.2)
  th <- blocks(2, 7, FALSE)
  expect_equal(r$theta_b, th, tolerance = 1e-12)
  expect_equal(r$se, sd(th), tolerance = 1e-12)
  expect_equal(c(r$lo, r$hi), c(q7(th, 0.1), q7(th, 0.9)), tolerance = 1e-12)
  expect_equal(r$var_iid, sum((x - mean(x))^2) / n^2, tolerance = 1e-12)
  expect_equal(r$n_starts, 10L)
  expect_equal(Btmbb(x, B = 3)$block_len, 2L)
  expect_error(Btmbb(1), "two observations")
  expect_error(Btmbb(x, block_len = 13), "1..n")
  expect_error(Btmbb(x, B = 1), "two replicates")
  expect_error(Btmbb(x, alpha = 1), "strictly")
  cr <- Btcbb(x, block_len = 3, stat = median, B = 7, seed = 2, alpha = 0.2)
  expect_equal(cr$theta_b, blocks(2, 7, TRUE), tolerance = 1e-12)
  expect_equal(cr$ebar_star, mean(x), tolerance = 1e-12)
  expect_error(Btcbb(1), "two observations")
  expect_error(Btcbb(x, block_len = 0), "1..n")
  expect_error(Btcbb(x, B = 1), "two replicates")
  expect_error(Btcbb(x, alpha = 0), "strictly")
})

test_that("Btcicor, Btciqua and Btht use Park-Miller 16807 indices", {
  x <- bx
  y <- bx^1.3 + c(0.5, -0.3, 0.2, 0.1, -0.4, 0.3, -0.2, 0.6, -0.1, 0.2, -0.5, 0.4)
  nx <- pm16807_ref(4, 12)
  zs <- vapply(1:9, function(b) {
    id <- vapply(1:12, function(i) nx(), 0)
    atanh(cor(x[id], y[id]))
  }, 0)
  r <- Btcicor(x, y, B = 9, alpha = 0.2, seed = 4)
  expect_equal(c(r$lo_z, r$hi_z), c(q7(zs, 0.1), q7(zs, 0.9)), tolerance = 1e-12)
  expect_equal(c(r$lo, r$hi), tanh(c(q7(zs, 0.1), q7(zs, 0.9))), tolerance = 1e-12)
  expect_equal(r$z_se, sd(zs), tolerance = 1e-12)
  expect_equal(r$lo_normal, tanh(atanh(cor(x, y)) - qnorm(0.9) * sd(zs)), tolerance = 1e-12)
  expect_error(Btcicor(numeric(0), 1), "empty")
  expect_error(Btcicor(x, y[-1]), "different lengths")
  expect_error(Btcicor(1:2, 1:2), "three pairs")
  expect_error(Btcicor(x, y, alpha = 1), "strictly")
  expect_error(Btcicor(x, y, B = 0), "at least one")
  nq <- pm16807_ref(6, 12)
  reps <- vapply(1:8, function(b) q7(x[vapply(1:12, function(i) nq(), 0)], 0.3), 0)
  q <- Btciqua(x, tau = 0.3, B = 8, alpha = 0.2, seed = 6)
  expect_equal(q$theta_b, reps, tolerance = 1e-12)
  expect_equal(c(q$lo, q$hi), c(q7(reps, 0.1), q7(reps, 0.9)), tolerance = 1e-12)
  expect_equal(q$q_hat, q7(x, 0.3), tolerance = 1e-12)
  expect_error(Btciqua(numeric(0)), "empty")
  expect_error(Btciqua(x, tau = 2), "\\[0, 1\\]")
  expect_error(Btciqua(x, alpha = 0), "strictly")
  expect_error(Btciqua(x, B = 0), "at least one")
  nh <- pm16807_ref(9, 12)
  sh <- x - mean(x) + 3
  tb <- vapply(1:11, function(b) mean(sh[vapply(1:12, function(i) nh(), 0)]), 0)
  h <- Btht(x, theta0 = 3, B = 11, seed = 9)
  pge <- (1 + sum(tb >= mean(x))) / 12
  ple <- (1 + sum(tb <= mean(x))) / 12
  expect_equal(h$T_b, tb, tolerance = 1e-12)
  expect_equal(h$p, min(1, 2 * min(pge, ple)), tolerance = 1e-12)
  expect_equal(Btht(x, theta0 = 3, stat = median, B = 3)$T_hat, median(x))
  expect_error(Btht(numeric(0)), "empty")
  expect_error(Btht(x, B = 0), "at least one")
})

test_that("quasi-random multinomial counts: Btmult, Btvarm, Btcimed", {
  cs <- counts_ref(5, 6, 2)
  m <- Btmult(5, B = 6)
  expect_equal(m$counts, cs)
  expect_equal(m$W, cs / 5)
  expect_equal(unique(rowSums(m$counts)), 5)
  ex <- Btmult(3, exhaustive = TRUE)
  g <- as.matrix(expand.grid(1:3, 1:3, 1:3))
  expect_equal(nrow(ex$counts), 27L)
  expect_equal(sort(apply(ex$counts, 1, paste, collapse = "")),
               sort(apply(t(apply(g, 1, tabulate, nbins = 3)), 1, paste, collapse = "")))
  expect_error(Btmult(0), "at least 1")
  expect_error(Btmult(3, B = 0), "at least 1")
  expect_error(Btmult(3, rng = 1), "base")
  expect_error(Btmult(7, exhaustive = TRUE), "capped")
  x <- c(2, 7, 1, 4)
  v <- Btvarm(x, B = 6, rng = 3)
  mb <- as.numeric(counts_ref(4, 6, 3) %*% x) / 4
  expect_equal(v$mean_b, mb, tolerance = 1e-12)
  expect_equal(v$var_b, mean((mb - mean(mb))^2), tolerance = 1e-12)
  expect_equal(Btvarm(x, exhaustive = TRUE)$var_b, mean((x - mean(x))^2) / 4,
               tolerance = 1e-12)
  expect_error(Btvarm(numeric(0)), "empty")
  expect_error(Btvarm(x, B = 0), "at least 1")
  expect_error(Btvarm(x, rng = 0), "base")
  cm <- Btcimed(c(3, 1, 2), exhaustive = TRUE, alpha = 0.2)
  meds <- apply(g, 1, function(i) median(c(3, 1, 2)[i]))
  expect_equal(sort(cm$medians), sort(meds))
  expect_equal(c(cm$lo, cm$hi), c(q7(meds, 0.1), q7(meds, 0.9)), tolerance = 1e-12)
  expect_equal(cm$estimate, 2)
  cq <- Btcimed(x, B = 6, rng = 3)
  expect_equal(cq$medians, apply(counts_ref(4, 6, 3), 1, function(k) median(rep(x, k))),
               tolerance = 1e-12)
  expect_error(Btcimed(numeric(0)), "empty")
  expect_error(Btcimed(x, alpha = 1), "strictly")
  expect_error(Btcimed(x, B = 0), "at least 1")
  expect_error(Btcimed(x, rng = 1), "base")
})

test_that("Btjkab averages replicates that omit each observation", {
  tb <- c(1.1, 0.9, 1.4, 1.2)
  idx <- list(c(0, 1, 1), c(2, 2, 0), c(1, 2, 2), c(0, 0, 0))
  r <- Btjkab(c(5, 6, 7), tb, idx)
  tm <- c(mean(tb[3]), mean(tb[c(2, 4)]), mean(tb[c(1, 4)]))
  expect_equal(r$theta_minus, tm, tolerance = 1e-12)
  expect_equal(r$infl_i, tm - mean(tb), tolerance = 1e-12)
  expect_equal(r$n_out, c(1L, 2L, 2L))
  expect_true(is.na(Btjkab(c(5, 6), tb[1:2], list(c(0, 1), c(1, 0)))$infl_i[1]))
  expect_error(Btjkab(numeric(0), tb, idx), "empty")
  expect_error(Btjkab(1:3, numeric(0), list()), "no bootstrap")
  expect_error(Btjkab(1:3, tb, idx[1:2]), "different lengths")
  expect_error(Btjkab(1:3, tb[1], list(5)), "out of range")
})

test_that("Btmoutn, Btsubs and Btsubrho draw m-out-of-n resamples", {
  x <- bx
  g <- lcg_ref(8)
  th <- vapply(1:9, function(b) mean(x[idx_ref(g, 12, 4)]), 0)
  r <- Btmoutn(x, m = 4, B = 9, seed = 8, alpha = 0.2)
  rr <- sqrt(4 / 12)
  expect_equal(r$theta_b, th, tolerance = 1e-12)
  expect_equal(r$se, rr * sd(th), tolerance = 1e-12)
  expect_equal(r$lo, mean(x) + rr * (q7(th, 0.1) - mean(x)), tolerance = 1e-12)
  expect_equal(Btmoutn(x, B = 3)$m, 3L)
  expect_error(Btmoutn(1), "two observations")
  expect_error(Btmoutn(x, m = 13), "1..n")
  expect_error(Btmoutn(x, B = 1), "two replicates")
  expect_error(Btmoutn(x, alpha = 1), "strictly")
  g <- lcg_ref(8)
  sub <- vapply(1:9, function(b) {
    p <- 0:11
    for (i in 1:4) {
      k <- min((i - 1) + floor(g$unif() * (12 - (i - 1))), 11)
      p[c(i, k + 1)] <- p[c(k + 1, i)]
    }
    mean(x[p[1:4] + 1])
  }, 0)
  s <- Btsubs(x, m = 4, B = 9, seed = 8, alpha = 0.2)
  roots <- 2 * (sub - mean(x))
  expect_equal(s$theta_b, sub, tolerance = 1e-12)
  expect_equal(s$lo, mean(x) - q7(roots, 0.9) / sqrt(12), tolerance = 1e-12)
  expect_equal(s$se, sd(roots) / sqrt(12), tolerance = 1e-12)
  expect_equal(s$var_closed, var(x) / 4 * 8 / 12, tolerance = 1e-12)
  expect_error(Btsubs(1), "two observations")
  expect_error(Btsubs(x, m = 0), "1..n")
  expect_error(Btsubs(x, B = 1), "two subsamples")
  expect_error(Btsubs(x, alpha = 2), "strictly")
  grid <- c(12, 6, 3)
  laws <- lapply(grid, function(m) {
    g <- lcg_ref(2)
    vapply(1:10, function(b) sqrt(m) * (mean(x[idx_ref(g, 12, m)]) - mean(x)), 0)
  })
  vol <- vapply(1:2, function(i) unname(suppressWarnings(
    stats::ks.test(laws[[i]], laws[[i + 1]])$statistic)), 0)
  sr <- Btsubrho(x, m_grid = grid, B = 10, seed = 2)
  expect_equal(sr$vol_curve, vol, tolerance = 1e-12)
  expect_equal(sr$m_star, grid[which.min(vol)])
  expect_equal(sr$se_star, sd(laws[[which.min(vol)]]) / sqrt(12), tolerance = 1e-12)
  dg <- Btsubrho(x, B = 3, q = 0.5)
  expect_equal(dg$m_grid, c(12L, 6L, 3L, 2L))
  expect_error(Btsubrho(1), "two observations")
  expect_error(Btsubrho(x, B = 1), "two replicates")
  expect_error(Btsubrho(x, q = 1), "strictly")
  expect_error(Btsubrho(x, m_grid = 4), "two grid values")
  expect_error(Btsubrho(x, m_grid = c(4, 20)), "1..n")
})

test_that("Btsbb, Btsmth and Btparm replay the 48271 stream", {
  x <- bx
  g <- lcg_ref(3)
  runs <- 0
  th <- vapply(1:5, function(b) {
    smp <- numeric(12)
    j <- 0
    for (t in 1:12) {
      if (t == 1 || g$unif() < 0.25) {
        runs <<- runs + 1
        j <- min(floor(g$unif() * 12), 11)
      } else {
        j <- (j + 1) %% 12
      }
      smp[t] <- x[j + 1]
    }
    mean(smp)
  }, 0)
  r <- Btsbb(x, p = 0.25, B = 5, seed = 3)
  expect_equal(r$theta_b, th, tolerance = 1e-12)
  expect_equal(r$n_runs, runs)
  expect_equal(r$mean_block, 60 / runs, tolerance = 1e-12)
  expect_equal(r$exp_runs, 5 * (1 + 11 * 0.25), tolerance = 1e-12)
  expect_error(Btsbb(1), "two observations")
  expect_error(Btsbb(x, p = 0), "\\(0, 1\\]")
  expect_error(Btsbb(x, B = 1), "two replicates")
  expect_error(Btsbb(x, alpha = 1), "strictly")
  g <- lcg_ref(4)
  sm <- vapply(1:5, function(b) mean(vapply(1:12, function(i) {
    j <- min(floor(g$unif() * 12), 11)
    x[j + 1] + 0.3 * qnorm(g$unif())
  }, 0)), 0)
  s <- Btsmth(x, h = 0.3, B = 5, seed = 4)
  expect_equal(s$theta_b, sm, tolerance = 1e-12)
  expect_equal(s$var_closed, (mean((x - mean(x))^2) + 0.09) / 12, tolerance = 1e-12)
  iq <- q7(x, 0.75) - q7(x, 0.25)
  expect_equal(Btsmth(x, B = 2)$h, 0.9 * min(sd(x), iq / 1.34) * 12^(-0.2), tolerance = 1e-12)
  expect_error(Btsmth(1), "two observations")
  expect_error(Btsmth(x, B = 1), "two replicates")
  expect_error(Btsmth(x, alpha = 0), "strictly")
  expect_error(Btsmth(x, h = -1), "non-negative")
  g <- lcg_ref(6)
  pr <- vapply(1:4, function(b) mean(2 + 0.5 * qnorm(vapply(1:5, function(i) g$unif(), 0))), 0)
  p <- Btparm(c(2, 0.5), B = 4, n = 5, seed = 6)
  expect_equal(p$theta_b, pr, tolerance = 1e-12)
  expect_equal(p$var_closed, 0.25 / 5, tolerance = 1e-12)
  cu <- Btparm(3, rvs_fn = function(th, n, g) rep(th * g$unif(), n), B = 3, n = 2, seed = 6)
  g <- lcg_ref(6)
  expect_equal(cu$theta_b, 3 * c(g$unif(), g$unif(), g$unif()), tolerance = 1e-12)
  expect_true(is.nan(cu$var_closed))
  expect_error(Btparm(c(0, 1)), "n \\(the simulated")
  expect_error(Btparm(c(0, 1), n = 0), "at least 1")
  expect_error(Btparm(c(0, 1), n = 3, B = 1), "two replicates")
  expect_error(Btparm(c(0, 1), n = 3, alpha = 1), "strictly")
  expect_error(Btparm(0, n = 3), "\\(mu, sigma\\)")
  expect_error(Btparm(c(0, -1), n = 3), "non-negative")
})

test_that("regression bootstraps: pairs, residual, wild and wild-cluster", {
  bh <- ridge_ls(bX, by)
  g <- lcg_ref(5)
  pr <- lapply(1:6, function(b) {
    id <- idx_ref(g, 12)
    ridge_ls(bX[id, ], by[id])
  })
  p <- Btpair(bX, by, B = 6, seed = 5, alpha = 0.2)
  expect_equal(p$beta_hat, bh, tolerance = 1e-10)
  expect_equal(p$beta_b, pr, tolerance = 1e-10)
  c2 <- vapply(pr, `[`, 0, 2)
  expect_equal(p$se[2], sd(c2), tolerance = 1e-10)
  expect_equal(c(p$lo[2], p$hi[2]), c(q7(c2, 0.1), q7(c2, 0.9)), tolerance = 1e-10)
  expect_error(Btpair(bX, by[-1]), "different lengths")
  expect_error(Btpair(bX[1:2, ], by[1:2]), "more rows")
  expect_error(Btpair(bX, by, B = 1), "two replicates")
  expect_error(Btpair(bX, by, alpha = 0), "strictly")
  fit <- as.numeric(bX %*% bh)
  e <- by - fit
  xtxi <- solve(crossprod(bX) + diag(1e-10, 2))
  hh <- rowSums((bX %*% xtxi) * bX)
  er <- e / sqrt(1 - hh)
  er <- er - mean(er)
  g <- lcg_ref(7)
  rr <- lapply(1:5, function(b) ridge_ls(bX, fit + er[idx_ref(g, 12)]))
  r <- Btres(bX, by, B = 5, seed = 7, rescale = TRUE)
  expect_equal(r$resid, er, tolerance = 1e-10)
  expect_equal(r$beta_b, rr, tolerance = 1e-10)
  expect_equal(r$var_closed, mean(er^2) * diag(xtxi), tolerance = 1e-10)
  expect_equal(Btres(bX, by, B = 2)$resid, e - mean(e), tolerance = 1e-10)
  expect_error(Btres(bX, by[-1]), "different lengths")
  expect_error(Btres(bX[1:2, ], by[1:2]), "more rows")
  expect_error(Btres(bX, by, B = 1), "two replicates")
  expect_error(Btres(bX, by, alpha = 1), "strictly")
  r5 <- sqrt(5)
  mam <- function(u) if (u < (r5 + 1) / (2 * r5)) (1 - r5) / 2 else (1 + r5) / 2
  g <- lcg_ref(2)
  vs <- numeric(0)
  wr <- lapply(1:4, function(b) {
    v <- vapply(1:12, function(i) mam(g$unif()), 0)
    vs <<- c(vs, v)
    ridge_ls(bX, fit + e * v)
  })
  w <- Btwild(bX, by, B = 4, seed = 2)
  expect_equal(w$beta_b, wr, tolerance = 1e-10)
  expect_equal(c(w$v_mean, w$v_m3), c(mean(vs), mean(vs^3)), tolerance = 1e-12)
  hc0 <- diag(xtxi %*% crossprod(bX * e) %*% xtxi)
  expect_equal(w$var_hc0, hc0, tolerance = 1e-10)
  g <- lcg_ref(2)
  rw <- Btwild(bX, by, B = 2, seed = 2, weights = "rademacher")
  v1 <- vapply(1:12, function(i) if (g$unif() < 0.5) 1 else -1, 0)
  expect_equal(rw$beta_b[[1]], ridge_ls(bX, fit + e * v1), tolerance = 1e-10)
  expect_error(Btwild(bX, by, weights = "x"), "mammen")
  expect_error(Btwild(bX, by[-1]), "different lengths")
  expect_error(Btwild(bX[1:2, ], by[1:2]), "more rows")
  expect_error(Btwild(bX, by, B = 1), "two replicates")
  expect_error(Btwild(bX, by, alpha = 0), "strictly")
  cl <- rep(c("a", "b", "c", "d"), each = 3)
  crve <- function(res, corr) {
    meat <- Reduce(`+`, lapply(unique(cl), function(k) {
      s <- crossprod(bX[cl == k, ], res[cl == k])
      s %*% t(s)
    }))
    cc <- if (corr) (4 / 3) * (11 / 10) else 1
    diag(xtxi %*% (meat * cc) %*% xtxi)
  }
  wc <- Btwldcl(bX, by, cl, B = 5, seed = 3)
  expect_equal(wc$vcov_cluster, crve(e, TRUE), tolerance = 1e-10)
  expect_equal(wc$vcov_cluster0, crve(e, FALSE), tolerance = 1e-10)
  expect_equal(wc$w, bh[2] / sqrt(crve(e, TRUE)[2]), tolerance = 1e-10)
  g <- lcg_ref(3)
  wb <- vapply(1:5, function(b) {
    v <- vapply(1:4, function(k) if (g$unif() < 0.5) 1 else -1, 0)
    ys <- fit + e * v[match(cl, unique(cl))]
    bb <- ridge_ls(bX, ys)
    (bb[2] - bh[2]) / sqrt(crve(ys - as.numeric(bX %*% bb), TRUE)[2])
  }, 0)
  expect_equal(wc$w_b, wb, tolerance = 1e-9)
  expect_equal(wc$p_value, (sum(abs(wb) >= abs(wc$w)) + 1) / 6, tolerance = 1e-12)
  expect_error(Btwldcl(bX, by, cl[-1]), "different lengths")
  expect_error(Btwldcl(bX[1:2, ], by[1:2], cl[1:2]), "more rows")
  expect_error(Btwldcl(bX, by, cl, B = 1), "two replicates")
  expect_error(Btwldcl(bX, by, cl, coef = 2), "out of range")
  expect_error(Btwldcl(bX, by, cl, alpha = 1), "strictly")
  expect_error(Btwldcl(bX, by, rep("a", 12)), "two clusters")
})

test_that("Btnpqr fits the check-loss quantile regression", {
  r <- Btnpqr(bX, by, tau = 0.3, B = 4, seed = 2)
  loss <- function(b) {
    u <- by - as.numeric(bX %*% b)
    sum(u * (0.3 - (u < 0)))
  }
  # the LP optimum passes through two observations: enumerate all pairs
  best <- Inf
  for (i in 1:11) for (j in (i + 1):12) {
    b <- solve(bX[c(i, j), ], by[c(i, j)])
    best <- min(best, loss(b))
  }
  expect_equal(r$loss, loss(r$beta_hat), tolerance = 1e-12)
  expect_equal(r$loss, best, tolerance = 1e-12)
  c1 <- vapply(r$beta_b, `[`, 0, 1)
  expect_equal(r$se[1], sd(c1), tolerance = 1e-12)
  expect_equal(r$lo[1], q7(c1, 0.025), tolerance = 1e-12)
  expect_error(Btnpqr(bX, by[-1]), "different lengths")
  expect_error(Btnpqr(bX[1:2, ], by[1:2]), "more rows")
  expect_error(Btnpqr(bX, by, tau = 1), "strictly")
  expect_error(Btnpqr(bX, by, B = 1), "two replicates")
  expect_error(Btnpqr(bX, by, alpha = 1), "strictly")
})

test_that("Btsieve fits an AR sieve by AIC and resamples it", {
  x <- c(bx, rev(bx), bx * 0.5 + 1)
  n <- 36
  pm <- floor(10 * log10(n))
  z <- x - mean(x)
  y <- z[(pm + 1):n]
  aics <- vapply(0:pm, function(p) {
    if (p == 0) return((n - pm) * log(mean(y^2)))
    X <- vapply(1:p, function(k) z[(pm + 1 - k):(n - k)], numeric(n - pm))
    res <- y - X %*% ridge_ls(X, y)
    (n - pm) * log(mean(res^2)) + 2 * p
  }, 0)
  r <- Btsieve(x, B = 3)
  expect_equal(r$order, which.min(aics) - 1L)
  expect_equal(r$aic, min(aics), tolerance = 1e-9)
  cu <- Btsieve(x, fit_fn = function(v) list(model = NULL, resid = v - mean(v)),
                rvs_fn = function(model, res, n, g) rep(g$unif(), n), B = 4, seed = 9)
  g <- lcg_ref(9)
  th <- c(g$unif(), g$unif(), g$unif(), g$unif())
  expect_equal(cu$theta_b, th, tolerance = 1e-12)
  expect_equal(cu$se, sd(th), tolerance = 1e-12)
  expect_equal(cu$sigma2, mean((x - mean(x))^2), tolerance = 1e-12)
  expect_error(Btsieve(1:3), "four")
  expect_error(Btsieve(x, B = 1), "two replicates")
  expect_error(Btsieve(x, alpha = 0), "strictly")
  expect_error(Btsieve(x, p_max = -1), "non-negative")
})

test_that("Btvinf is the numerically perturbed influence function", {
  x <- c(2, 5, 3, 9, 1)
  r <- Btvinf(x, "mean")
  expect_equal(r$infl, x - mean(x), tolerance = 1e-9)
  expect_equal(r$estimate, sum((x - mean(x))^2) / 25, tolerance = 1e-9)
  f <- function(v, w) sum(w * v^2)
  rf <- Btvinf(x, f, eps = 0.01)
  expect_equal(rf$infl, (x^2 - mean(x^2)), tolerance = 1e-9)
  expect_error(Btvinf(numeric(0)), "empty")
  expect_error(Btvinf(x, eps = 1), "strictly")
  expect_error(Btvinf(x, "mode"), "estimator must be")
})

test_that("SplitMix64 bootstraps: calibration, double, iterated, AR sieve", {
  x <- bx
  n <- 12
  e <- .ghc_rng(4)
  tb <- vapply(1:40, function(b) {
    xb <- x[pmin(floor(.ghc_unif(e, n) * n), n - 1) + 1]
    sqrt(n) * (mean(xb) - mean(x)) / sd(xb)
  }, 0)
  r <- morie_btcalib(x, alpha = 0.1, B = 40, seed = 4)
  ap <- sort(pnorm(abs(tb), lower.tail = FALSE))[4]
  expect_equal(r$alpha_prime, ap, tolerance = 1e-12)
  expect_equal(r$upper - mean(x), qnorm(1 - ap) * sd(x) / sqrt(n), tolerance = 1e-12)
  expect_lt(r$identity_gap, 1e-9)
  expect_error(morie_btcalib(1:4), "five")
  expect_error(morie_btcalib(x, alpha = 1), "\\(0, 1\\)")
  rec <- new.env()
  rec$s <- list()
  st <- function(v) {
    rec$s[[length(rec$s) + 1]] <- v
    median(v)
  }
  d <- morie_btdbl(x, statistic = st, alpha = 0.2, B_outer = 5, B_inner = 4, seed = 2)
  smp <- rec$s[-1]
  roots <- numeric(5)
  pre <- numeric(5)
  for (b in 1:5) {
    tbb <- median(smp[[(b - 1) * 5 + 1]])
    roots[b] <- sqrt(n) * abs(tbb - median(x))
    inner <- vapply(smp[(b - 1) * 5 + 2:5], median, 0)
    pre[b] <- mean(sqrt(n) * abs(inner - tbb) <= roots[b])
  }
  c1 <- sort(pre)[4]
  expect_equal(d$c_level, c1, tolerance = 1e-12)
  expect_equal(d$critical_root, sort(roots)[max(ceiling(c1 * 5), 1)], tolerance = 1e-12)
  expect_equal(d$upper - d$estimate, d$critical_root / sqrt(n), tolerance = 1e-12)
  expect_error(morie_btdbl(1:4), "five")
  expect_error(morie_btdbl(x, alpha = 0), "\\(0, 1\\)")
  ts <- function(s, c0) {
    sdv <- sd(s)
    if (sdv <= 0) 0 else sqrt(n) * abs(mean(s) - c0) / sdv
  }
  e <- .ghc_rng(3)
  rs <- function(v) v[pmin(floor(.ghc_unif(e, n) * n), n - 1) + 1]
  nx <- x - mean(x) + 3
  ot <- numeric(6)
  pp <- numeric(6)
  for (b in 1:6) {
    xb <- rs(nx)
    ot[b] <- ts(xb, 3)
    nb <- xb - mean(xb) + 3
    pp[b] <- mean(vapply(1:5, function(k) ts(rs(nb), 3), 0) <= ot[b])
  }
  it <- morie_btiseq(x, mu0 = 3, B_outer = 6, B_inner = 5, seed = 3)
  h <- mean(ot <= ts(x, 3))
  expect_equal(it$statistic, ts(x, 3), tolerance = 1e-12)
  expect_equal(it$p_boot, 1 - h, tolerance = 1e-12)
  expect_equal(it$p_iterated, mean(pp >= h), tolerance = 1e-12)
  expect_error(morie_btiseq(1:4), "five")
  y <- c(bx, rev(bx)) + 0.1 * (1:24)
  ar <- morie_btarsv(y, p = 2, B = 3, burn = 5, seed = 1)
  yc <- y - mean(y)
  gam <- vapply(0:2, function(k) sum(yc[1:(24 - k)] * yc[(1 + k):24]) / 24, 0)
  phi <- solve(stats::toeplitz(gam[1:2]), gam[2:3])
  expect_equal(ar$phi, phi, tolerance = 1e-12)
  expect_equal(ar$sigma2, gam[1] - sum(phi * gam[2:3]), tolerance = 1e-12)
  res <- vapply(3:24, function(t) yc[t] - sum(phi * yc[(t - 1):(t - 2)]), 0)
  res <- res - mean(res)
  e <- .ghc_rng(1)
  reps <- vapply(1:3, function(b) {
    state <- c(0, 0)
    out <- numeric(0)
    for (t in 1:29) {
      v <- sum(phi * state) + res[min(floor(.ghc_unif(e, 1) * 22), 21) + 1]
      state <- c(v, state[1])
      if (t > 5) out <- c(out, v + mean(y))
    }
    mean(out)
  }, 0)
  expect_equal(ar$replicates, reps, tolerance = 1e-12)
  expect_equal(ar$se, sd(reps), tolerance = 1e-12)
  expect_true(morie_btarsv(y, B = 2)$p >= 1L)
  expect_error(morie_btarsv(1:10), "20")
  expect_error(morie_btarsv(y, p = 12), "out of range")
})
