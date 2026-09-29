# Coverage for the partial-identification bound files (bnd*.R, bd*.R):
# every bound is recomputed from the Manski / Manski-Pepper / Imbens-
# Manski / Andrews-Soares formula in base R.

bd_y <- c(3, 7, 2, 9, 5, 4, 8, 6, 1, 10)
bd_d <- c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1)
bd_x <- c("a", "a", "b", "b", "a", "b", "a", "b", "a", "b")
q1 <- function(v, p) unname(stats::quantile(v, p, type = 1))

test_that("worst-case (no-assumption) bounds on a mean and an ATE", {
  r <- Bndmsg(bd_y, bd_d, 0, 12)
  p <- mean(bd_d)
  m <- mean(bd_y[bd_d == 1])
  expect_equal(c(r$lower, r$upper), c(m * p, m * p + 12 * (1 - p)), tolerance = 1e-12)
  expect_equal(r$width, 12 * (1 - p), tolerance = 1e-12)
  expect_same_function(morie_bound_missing_outcome, Bndmsg)
  expect_equal(Bndmsg(1:3, c(0, 0, 0), 0, 5)$upper, 5)
  expect_error(Bndmsg(numeric(0), numeric(0), 0, 1), "empty")
  expect_error(Bndmsg(1:3, 1:2, 0, 1), "different lengths")
  expect_error(Bndmsg(1:3, c(1, 1, 1), 2, 1), "at least y_min")
  expect_error(Bndmsg(1:3, c(1, 2, 1), 0, 5), "0 or 1")
  t <- morie_bdtrns(c(2, 4, 6), p_obs = 0.6, y_lo = 0, y_hi = 10)
  expect_equal(c(t$lower, t$upper), c(4 * 0.6, 4 * 0.6 + 4), tolerance = 1e-12)
  expect_error(morie_bdtrns(numeric(0), 0.5, 0, 1), "at least one")
  expect_error(morie_bdtrns(1, 2, 0, 1), "\\[0, 1\\]")
  expect_error(morie_bdtrns(1, 0.5, 2, 1), "y_hi")
  expect_error(morie_bdtrns(5, 0.5, 0, 1), "violate")
  e <- morie_bndest(bd_y, bd_d == 1, support = c(0, 12))
  expect_equal(c(e$lower, e$upper), c(m * p, m * p + 12 * (1 - p)), tolerance = 1e-12)
  expect_false(e$identified)
  a <- morie_bndest(bd_y, TRUE, support = c(0, 12), treatment = bd_d)
  m0 <- mean(bd_y[bd_d == 0])
  expect_equal(a$ate_lower, m * p - (m0 * (1 - p) + 12 * p), tolerance = 1e-12)
  expect_equal(a$ate_width, 12, tolerance = 1e-12)
  expect_true(a$contains_zero)
  expect_error(morie_bndest(bd_y, bd_d == 1, support = c(5, 1)), "K0 < K1")
  expect_error(morie_bndest(bd_y, TRUE, support = c(0, 1)), "observed has")
  expect_error(morie_bndest(bd_y, TRUE, c(0, 12), treatment = 1), "treatment has")
  expect_error(morie_bndest(bd_y, TRUE, c(0, 12), treatment = bd_d + 1), "binary")
  expect_error(morie_bndest(bd_y, bd_d == 1, support = c(0, 5)), "outside the declared")
})

test_that("naive, positive-only and negative-only treatment bounds", {
  p1 <- mean(bd_d)
  m1 <- mean(bd_y[bd_d == 1])
  m0 <- mean(bd_y[bd_d == 0])
  n <- Bndnvg(bd_y, bd_d)
  expect_equal(c(n$lower, n$upper), c(m1 - 10, m1 - 1), tolerance = 1e-12)
  expect_error(Bndnvg(1:3, c(0, 0, 0)), "no treated")
  expect_error(Bndnvg(1:3, c(0, 2, 0)), "0/1")
  pp <- Bndpos(bd_y, bd_d, y_max = 12)
  expect_equal(pp$upper, (m1 * p1 + 12 * (1 - p1)) - (m0 * (1 - p1) + 1 * p1),
               tolerance = 1e-12)
  expect_equal(pp$lower, 0)
  expect_error(Bndpos(bd_y, bd_d, y_max = 5), "below max")
  ng <- Bndngt(bd_y, bd_d, y_min = 0)
  expect_equal(ng$lower, (m1 * p1) - (m0 * (1 - p1) + 10 * p1), tolerance = 1e-12)
  expect_error(Bndngt(bd_y, bd_d, y_min = 2), "above min")
  expect_error(Bndngt(numeric(0), numeric(0), 0), "empty")
  expect_error(Bndngt(1:2, 1, 0), "same length")
})

test_that("Manski-Pepper MTR, MTS and MIV bounds", {
  z <- c(1, 2, 2, 3, 1, 3, 2, 1, 3, 2)
  r <- Mtrbound(bd_y, z, d = 2, ymin = 0, ymax = 12)
  expect_equal(r$lower, mean(ifelse(z <= 2, bd_y, 0)), tolerance = 1e-12)
  expect_equal(r$upper, mean(ifelse(z >= 2, bd_y, 12)), tolerance = 1e-12)
  expect_error(Mtrbound(bd_y, z[-1], 2, 0, 12), "same length")
  expect_error(Mtrbound(numeric(0), numeric(0), 2, 0, 1), "at least one")
  expect_error(Mtrbound(bd_y, z, 2, 5, 1), "must not exceed")
  expect_error(Mtrbound(bd_y, z, 2, 0, 5), "\\[ymin, ymax\\]")
  s <- Mtsbound(bd_y, z, d = 2, ymin = 0, ymax = 12)
  cm <- mean(bd_y[z == 2])
  expect_equal(s$upper, mean(z <= 2) * cm + mean(z > 2) * 12, tolerance = 1e-12)
  expect_equal(s$lower, mean(z < 2) * 0 + mean(z >= 2) * cm, tolerance = 1e-12)
  expect_error(Mtsbound(bd_y, z, d = 5, 0, 12), "no unit")
  expect_error(Mtsbound(bd_y, z[-1], 2, 0, 12), "same length")
  expect_error(Mtsbound(bd_y, z, 2, 13, 12), "must not exceed")
  mv <- Mivbound(lower = c(2, 1, 4), upper = c(9, 6, 8), prob = c(1, 2, 1))
  expect_equal(mv$lowerv, c(2, 2, 4))
  expect_equal(mv$upperv, c(6, 6, 8))
  expect_equal(c(mv$lower, mv$upper), c(sum(c(2, 2, 4) * c(0.25, 0.5, 0.25)),
                                        sum(c(6, 6, 8) * c(0.25, 0.5, 0.25))), tolerance = 1e-12)
  expect_error(Mivbound(1:2, 1:3, 1:2), "same length")
  expect_error(Mivbound(numeric(0), numeric(0), numeric(0)), "at least one")
  expect_error(Mivbound(1, 2, -1), "non-negative")
  expect_error(Mivbound(1, 2, 0), "all be zero")
  for (f in list(Bndapp, morie_bndapp)) {
    b <- f(bd_y, z)
    lev <- 1:3
    sh <- vapply(lev, function(g) mean(z == g), 0)
    mu <- vapply(lev, function(g) mean(bd_y[z == g]), 0)
    lo <- vapply(lev, function(k) sum((sh * mu)[lev < k]) + mu[k] * sum(sh[lev >= k]), 0)
    hi <- vapply(lev, function(k) sum((sh * mu)[lev > k]) + mu[k] * sum(sh[lev <= k]), 0)
    expect_equal(b$lower, lo, tolerance = 1e-12)
    expect_equal(b$upper, hi, tolerance = 1e-12)
    expect_equal(b$ate_upper, hi[3] - lo[1], tolerance = 1e-12)
    expect_equal(f(bd_y, z, t1 = 2, t0 = 1)$ate_upper, hi[2] - lo[1], tolerance = 1e-12)
    expect_error(f(numeric(0), numeric(0)), "empty")
    expect_error(f(bd_y, z[-1]), "same length")
    expect_error(f(bd_y, z, t1 = 5), "realized levels")
    expect_error(f(bd_y, z, t1 = 1, t0 = 2), "t1 > t0")
  }
})

test_that("moment-inequality set estimates: GMS and criterion sets", {
  g <- Gmsbound(mbar = c(0.2, -0.5, 0.05), sigma = c(1, 2, 0.5), n = 100)
  t <- 10 * c(0.2, -0.5, 0.05) / c(1, 2, 0.5)
  k <- sqrt(log(100))
  keep <- t / k > -1
  expect_equal(g$S, sum(pmax(t, 0)[keep]^2), tolerance = 1e-12)
  expect_equal(g$retained, as.integer(keep))
  expect_equal(Gmsbound(c(0.2, -0.5), c(1, 1), 100, kappa = 10)$nretained, 2L)
  expect_error(Gmsbound(1, 1:2, 10), "same length")
  expect_error(Gmsbound(1, 0, 10), "strictly positive")
  expect_error(Gmsbound(1, 1, 1), "exceed 1")
  expect_error(Gmsbound(1, 1, 10, kappa = 0), "kappa")
  M <- rbind(c(0.1, -0.2), c(0.3, 0.4), c(-0.1, -0.3))
  q <- Qcritset(M, se = c(1, 2), n = 50, cutoff = 2)
  Q <- rowSums(pmax(sweep(M, 2, c(1, 2), "/"), 0)^2)
  expect_equal(q$Q, Q, tolerance = 1e-12)
  expect_equal(q$inset, as.integer(50 * Q <= 2))
  expect_equal(q$argmin, which.min(Q) - 1L)
  expect_equal(Qcritset(M)$nin, 1L)
  expect_equal(Qcritset(M, se = matrix(1, 3, 2))$Q, rowSums(pmax(M, 0)^2), tolerance = 1e-12)
  expect_equal(Qcritset(M, se = matrix(c(1, 2), 2, 1))$Q, Q, tolerance = 1e-12)
  expect_error(Qcritset(M, se = 1:3), "length J")
  expect_error(Qcritset(M, se = c(0, 1)), "strictly positive")
  expect_error(Qcritset(M, n = 0), "positive")
})

test_that("Misspecbd adds the worst-case bias c ||s|| to the z interval", {
  r <- Misspecbd(1.5, c(3, 4), c = 0.2, se = 0.3, conf = 0.9)
  hw <- 0.2 * 5 + qnorm(0.95) * 0.3
  expect_equal(c(r$lower, r$upper), 1.5 + c(-hw, hw), tolerance = 1e-12)
  expect_equal(r$worstgamma, 0.2 * c(3, 4) / 5, tolerance = 1e-12)
  expect_equal(Misspecbd(0, c(0, 0), 1, 1)$worstgamma, c(0, 0))
  expect_error(Misspecbd(0, 1, -1, 1), "non-negative")
  expect_error(Misspecbd(0, 1, 1, -1), "non-negative")
})

test_that("Bndfre and Bndinf: Imbens-Manski inference", {
  lo <- c(1.2, 0.8, 1.1, 1.4, 0.9, 1)
  hi <- c(2.1, 2.5, 1.9, 2.2, 2.6, 2.3)
  r <- Bndfre(lo, hi, alpha = 0.1)
  sl <- sd(lo)
  su <- sd(hi)
  sh <- sqrt(6) * (mean(hi) - mean(lo)) / max(sl, su)
  cv <- stats::uniroot(function(c) pnorm(c + sh) - pnorm(-c) - 0.9,
                       c(qnorm(0.9), qnorm(0.95)), tol = 1e-14)$root
  if (pnorm(qnorm(0.9) + sh) - pnorm(-qnorm(0.9)) >= 0.9) cv <- qnorm(0.9)
  expect_equal(r$c, cv, tolerance = 1e-9)
  expect_equal(r$lower, mean(lo) - cv * sl / sqrt(6), tolerance = 1e-9)
  expect_equal(r$upper, mean(hi) + cv * su / sqrt(6), tolerance = 1e-9)
  expect_error(Bndfre(1, 2), "two replicates")
  expect_error(Bndfre(lo, hi[-1]), "same length")
  expect_error(Bndfre(hi, lo), "below mean lower")
  mm <- cbind(lo, hi)
  b <- Bndinf(seq(0, 3, by = 0.25), mm, alpha = 0.1)
  z <- qnorm(0.9)
  expect_equal(b$lower, mean(lo) - z * sl / sqrt(6), tolerance = 1e-12)
  expect_equal(b$upper, mean(hi) + z * su / sqrt(6), tolerance = 1e-12)
  crit <- function(th) max(sqrt(6) * (mean(lo) - th) / sl, 0)^2 +
    max(sqrt(6) * (th - mean(hi)) / su, 0)^2
  qs <- vapply(seq(0, 3, by = 0.25), crit, 0)
  ins <- seq(0, 3, by = 0.25)[qs <= z^2]
  expect_equal(c(b$grid_lower, b$grid_upper), range(ins), tolerance = 1e-12)
  expect_equal(b$n_in_set, length(ins))
  expect_true(is.na(Bndinf(100, mm)$grid_lower))
  expect_error(Bndinf(numeric(0), mm), "empty")
  expect_error(Bndinf(1, mm, alpha = 1), "\\(0, 1\\)")
  expect_error(Bndinf(1, mm[1, , drop = FALSE]), "two observations")
  expect_error(Bndinf(1, cbind(mm, 1)), "two columns")
  expect_error(Bndinf(1, mm[, 2:1]), "below yL")
})

test_that("Bndlmm intersects the min-max bounds", {
  L <- cbind(c(1, 1.2, 0.9, 1.1), c(0.5, 0.7, 0.6, 0.8))
  U <- cbind(c(3, 3.3, 2.9, 3.1), c(2.5, 2.8, 2.7, 2.6), c(4, 4, 4.1, 3.9))
  r <- Bndlmm(L, U)
  expect_equal(c(r$lower, r$upper), c(max(colMeans(L)), min(colMeans(U))), tolerance = 1e-12)
  expect_equal(r$lower_pc, max(colMeans(L) - qnorm(1 - 0.25) * apply(L, 2, sd) / 2),
               tolerance = 1e-12)
  expect_equal(r$upper_pc, min(colMeans(U) + qnorm(1 - 0.5 / 3) * apply(U, 2, sd) / 2),
               tolerance = 1e-12)
  expect_error(Bndlmm(L[1, , drop = FALSE], U[1, , drop = FALSE]), "two observations")
  expect_error(Bndlmm(L, U[1:3, ]), "same number of rows")
})

test_that("Bndlpm reproduces the Balke-Pearl linear programme", {
  skip_if_not_installed("rcdd")
  y <- c(1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1)
  D <- c(1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0)
  Z <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0)
  r <- Bndlpm(y, D, Z)
  A <- matrix(0, 8, 16)
  b <- numeric(8)
  row <- 0
  for (z in 0:1) for (d in 0:1) for (a in 0:1) {
    row <- row + 1
    for (j in 1:4) {
      dz <- c(0, 0, 1, 1)[j] * (1 - z) + c(0, 1, 0, 1)[j] * z
      if (dz != d) next
      for (k in 1:4) {
        yk <- c(0, 0, 1, 1)[k] * (1 - d) + c(0, 1, 0, 1)[k] * d
        if (yk == a) A[row, (j - 1) * 4 + k] <- 1
      }
    }
    b[row] <- sum(Z == z & D == d & y == a) / sum(Z == z)
  }
  cv <- rep(c(0, 1, -1, 0), 4)
  h <- rcdd::makeH(rbind(diag(16), -diag(16)), c(rep(1, 16), rep(0, 16)), A, b)
  expect_equal(r$lower, rcdd::lpcdd(h, cv, minimize = TRUE)$optimal.value, tolerance = 1e-9)
  expect_equal(r$upper, rcdd::lpcdd(h, cv, minimize = FALSE)$optimal.value, tolerance = 1e-9)
  expect_equal(r$feasible, 1)
  expect_error(Bndlpm(numeric(0), numeric(0), numeric(0)), "empty")
  expect_error(Bndlpm(y, D[-1], Z), "same length")
  expect_error(Bndlpm(y + 1, D, Z), "0/1")
  expect_error(Bndlpm(y, D, rep(1, 12)), "only one value")
  expect_error(Bndlpm(y, D, Z, moment_eqs = matrix(0, 1, 5)), "17 entries")
})

test_that("Bndmoq, Bndnpr, Bndtfm, Bndsbs, Bndvld", {
  a <- 0.5
  band <- function(ys, ds) {
    obs <- ys[ds == 1]
    p1 <- length(obs) / length(ys)
    c(if (p1 > 1 - a) q1(obs, 1 - (1 - a) / p1) else min(bd_y),
      if (p1 >= a) q1(obs, a / p1) else max(bd_y))
  }
  r <- Bndmoq(bd_y, bd_d, bd_x, quantile = a)
  expect_equal(c(r$lower, r$upper), band(bd_y, bd_d), tolerance = 1e-12)
  wa <- diff(band(bd_y[bd_x == "a"], bd_d[bd_x == "a"]))
  wb <- diff(band(bd_y[bd_x == "b"], bd_d[bd_x == "b"]))
  expect_equal(r$max_width, max(wa, wb), tolerance = 1e-12)
  expect_error(Bndmoq(bd_y, bd_d, 1:3, 0.5), "one value per unit")
  expect_error(Bndmoq(bd_y, bd_d, bd_x, 1), "\\(0, 1\\)")
  xn <- c(0.1, 0.5, 0.9, 1.3, 0.2, 1.1, 0.4, 0.8, 1.5, 0.6)
  np <- Bndnpr(bd_y, bd_d, xn, bw = 0.4)
  arm <- vapply(1:10, function(i) {
    k <- exp(-0.5 * ((xn[i] - xn) / 0.4)^2)
    p1 <- sum(k * bd_d) / sum(k)
    m1 <- sum(k * bd_d * bd_y) / sum(k * bd_d)
    m0 <- sum(k * (1 - bd_d) * bd_y) / sum(k * (1 - bd_d))
    c(m1 * p1 + 1 * (1 - p1) - (m0 * (1 - p1) + 10 * p1),
      m1 * p1 + 10 * (1 - p1) - (m0 * (1 - p1) + 1 * p1))
  }, numeric(2))
  expect_equal(c(np$lower, np$upper), rowMeans(arm), tolerance = 1e-12)
  expect_error(Bndnpr(bd_y, bd_d, 1:3, 1), "one value per unit")
  expect_error(Bndnpr(bd_y, bd_d, xn, 0), "positive")
  tv <- log(bd_y)
  tf <- Bndtfm(bd_y, bd_d, bd_x, tv)
  wc <- function(s) {
    tt <- tv[s]
    dd <- bd_d[s]
    p1 <- mean(dd)
    m1 <- mean(tt[dd == 1])
    m0 <- if (any(dd == 0)) mean(tt[dd == 0]) else 0
    c(m1 * p1 + 0 * (1 - p1) - (m0 * (1 - p1) + log(10) * p1),
      m1 * p1 + log(10) * (1 - p1) - (m0 * (1 - p1) + 0 * p1)) * mean(s)
  }
  expect_equal(c(tf$lower, tf$upper), wc(bd_x == "a") + wc(bd_x == "b"), tolerance = 1e-12)
  expect_equal(tf$gap, 0, tolerance = 1e-12)
  expect_error(Bndtfm(bd_y, bd_d, bd_x, tv[-1]), "one value per unit")
  expect_error(Bndtfm(bd_y, bd_d, bd_x[-1], tv), "one value per unit")
  expect_error(Bndtfm(bd_y, bd_d, bd_x, -tv), "not monotone")
  M <- cbind(c(1, 3, 2), c(5, 5, 9), c(0, 1, 0.5))
  sb <- Bndsbs(M, c(2, 0))
  expect_equal(c(sb$lower, sb$upper), c(0, 1))
  expect_equal(c(sb$total_width, sb$max_width), c(3, 2))
  expect_error(Bndsbs(matrix(numeric(0), 0, 2), 0), "empty")
  expect_error(Bndsbs(M, integer(0)), "empty")
  expect_error(Bndsbs(M, 3), "out of range")
  v <- Bndvld(lower = c(1, 2), upper = c(5, 4), theta_0 = 4.5)
  expect_equal(c(v$lower, v$upper, v$covers, v$reject, v$refuted), c(2, 4, 0, 1, 0))
  expect_equal(Bndvld(c(3, 1), c(2, 5), 0, H0 = 0)$refuted, 1)
  expect_equal(Bndvld(c(3, 1), c(2, 5), 0, H0 = 0)$reject, 0)
  expect_error(Bndvld(numeric(0), numeric(0), 1), "empty")
  expect_error(Bndvld(1:2, 1, 1), "same length")
})
