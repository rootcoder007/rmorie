# Coverage for SearchTheory .. Sfbnds exports. Every expectation is
# recomputed in the test body.

test_that("SweepWidth integrates the lateral range curve", {
  x <- seq(-3, 3, by = 0.5)
  p <- LateralRange(x, W = 2)
  expect_equal(SweepWidth(x, p), sum(diff(x) * (head(p, -1) + tail(p, -1)) / 2), tolerance = 1e-12)
  # the definite-range curve of width W sweeps exactly W
  xd <- seq(-2, 2, by = 0.001)
  expect_equal(SweepWidth(xd, LateralRange(xd, W = 2, model = "definite")), 2, tolerance = 2e-3)
})

test_that("SEIR and SEIRA match a classical RK4 solve", {
  skip_if_not_installed("deSolve")
  r <- Seirep(990, 5, 5, 0, beta = 0.5, sigma = 0.2, gamma = 0.1, t_max = 60, dt = 0.5)
  f <- function(t, y, p) {
    N <- sum(y)
    inf <- 0.5 * y[1] * y[3] / N
    list(c(-inf, inf - 0.2 * y[2], 0.2 * y[2] - 0.1 * y[3], 0.1 * y[3]))
  }
  o <- deSolve::ode(c(990, 5, 5, 0), seq(0, 60, by = 0.5), f, NULL, method = "rk4")
  expect_equal(c(r$S, r$E, r$I, r$R), unname(o[121, 2:5]), tolerance = 1e-10)
  expect_equal(r$peak_I, max(o[, 4]), tolerance = 1e-10)
  expect_equal(r$peak_time, unname(o[which.max(o[, 4]), 1]))
  expect_equal(r$R0, 5)
  expect_lt(r$conservation_error, 1e-9)
  expect_error(Seirep(-1, 0, 1, 0, 1, 1, 1), "non-negative")
  expect_error(Seirep(1, 0, 1, 0, 1, 1, 1, dt = 0), "dt > 0")
  pa <- c(0.6, 0.25, 0.1, 0.7, 0.5, 0.2)
  a <- Seiarp(990, 5, 5, 0, 0, pa, t_max = 40, dt = 0.5)
  g <- function(t, y, p) {
    N <- sum(y)
    inf <- y[1] * (0.6 * y[3] + 0.5 * 0.6 * y[4]) / N
    list(c(-inf, inf - 0.25 * y[2], 0.7 * 0.25 * y[2] - 0.1 * y[3], 0.3 * 0.25 * y[2] - 0.2 * y[4],
           0.1 * y[3] + 0.2 * y[4]))
  }
  oa <- deSolve::ode(c(990, 5, 5, 0, 0), seq(0, 40, by = 0.5), g, NULL, method = "rk4")
  expect_equal(c(a$S, a$E, a$I, a$A, a$R), unname(oa[81, 2:6]), tolerance = 1e-10)
  expect_equal(a$R0, 0.6 * (0.7 / 0.1 + 0.5 * 0.3 / 0.2), tolerance = 1e-12)
  expect_error(Seiarp(1, 0, 1, 0, 0, pa[1:5]), "params must be")
  expect_error(Seiarp(1, 0, 1, 0, 0, replace(pa, 4, 2)), "p must lie")
})

test_that("Predacc is the Pearson prediction accuracy", {
  y <- c(1.2, 2.3, 2.9, 4.1, 5.2)
  yh <- c(1.0, 2.6, 3.1, 3.8, 5.5)
  r <- Predacc(y, yh)
  expect_equal(r$accuracy, stats::cor(y, yh), tolerance = 1e-12)
  expect_equal(r$r2, stats::cor(y, yh)^2, tolerance = 1e-12)
  expect_error(Predacc(y, yh[-1]), "same length")
  expect_error(Predacc(1, 1), "at least two")
  expect_error(Predacc(y, rep(1, 5)), "both vary")
})

cov_s5_ehh <- function(H, core, idx) {
  L <- ncol(H)
  vapply(seq_len(L), function(j) {
    lo <- min(j, core)
    hi <- max(j, core)
    k <- apply(H[idx, lo:hi, drop = FALSE], 1, paste, collapse = "")
    tb <- table(k)
    sum(tb * (tb - 1)) / (length(idx) * (length(idx) - 1))
  }, 0)
}

cov_s5_ihh <- function(pos, e, core, me) {
  side <- function(js) {
    a <- 0
    pp <- pos[core]
    pe <- e[core]
    for (j in js) {
      a <- a + abs(pos[j] - pp) * (e[j] + pe) / 2
      pp <- pos[j]
      pe <- e[j]
      if (e[j] < me) break
    }
    a
  }
  L <- length(pos)
  (if (core > 1) side((core - 1):1) else 0) + (if (core < L) side((core + 1):L) else 0)
}

test_that("iHS and XP-EHH integrate EHH decay curves", {
  H <- rbind(c(0, 1, 1, 0, 1, 1), c(0, 1, 1, 0, 1, 0), c(1, 1, 1, 0, 1, 1), c(1, 1, 1, 0, 0, 1),
             c(0, 0, 1, 1, 0, 1), c(1, 0, 1, 1, 0, 0), c(0, 0, 0, 1, 1, 0), c(1, 0, 0, 0, 0, 1))
  pos <- c(0, 10, 25, 30, 42, 60)
  core <- 3
  a <- which(H[, core] == 0)
  d <- which(H[, core] == 1)
  r <- Ihstst(H, core - 1, positions = pos)
  ia <- cov_s5_ihh(pos, cov_s5_ehh(H, core, a), core, 0.05)
  id <- cov_s5_ihh(pos, cov_s5_ehh(H, core, d), core, 0.05)
  expect_equal(c(r$ihh_a, r$ihh_d), c(ia, id), tolerance = 1e-12)
  expect_equal(r$ihs_unstandardized, log(ia / id), tolerance = 1e-12)
  expect_equal(r$daf, length(d) / 8)
  expect_equal(Ihstst(H, core - 1, positions = pos, standardize = c(0.1, 2))$estimate, (log(ia / id) - 0.1) / 2,
               tolerance = 1e-12)
  expect_error(Ihstst(H, core - 1, positions = pos, standardize = c(0, 0)), "sd must be positive")
  HB <- H[c(2, 1, 4, 3, 8, 7, 6, 5), c(1, 2, 3, 4, 6, 5)]
  x <- Xpehh1(H, HB, core - 1, positions = pos)
  IA <- cov_s5_ihh(pos, cov_s5_ehh(H, core, 1:8), core, 0.05)
  IB <- cov_s5_ihh(pos, cov_s5_ehh(HB, core, 1:8), core, 0.05)
  expect_equal(c(x$I_A, x$I_B), c(IA, IB), tolerance = 1e-12)
  expect_equal(x$xpehh_unstandardized, log(IA / IB), tolerance = 1e-12)
  expect_error(Xpehh1(H, HB[, 1:5], core - 1), "same SNPs")
})

test_that("Ibdmtx estimates PLINK IBD proportions", {
  G <- rbind(c(0, 1, 2, 1, 0, 2, 1, 1, 0, 2),
             c(0, 1, 2, 1, 0, 2, 1, 1, 0, 2),
             c(2, 1, 0, 1, 2, 0, 1, 1, 2, 0),
             c(1, 2, 1, 0, 1, 1, 2, 0, 1, 1))
  r <- Ibdmtx(G)
  # rows 1 and 2 are identical: IBS2 at every SNP gives pihat 1
  expect_equal(r$estimate[1, 2], 1)
  expect_equal(c(r$Z0[1, 2], r$Z1[1, 2], r$Z2[1, 2]), c(0, 0, 1))
  expect_equal(r$ibs_counts[[1]], c(1, 2, 0, 0, 10))
  ibs <- 2 - abs(G[1, ] - G[3, ])
  expect_equal(r$ibs_counts[[2]][3:5], as.numeric(tabulate(ibs + 1, 3)))
  expect_equal(r$estimate, t(r$estimate))
  expect_true(all(r$Z0 + r$Z1 + r$Z2 > 1 - 1e-12 & r$Z0 + r$Z1 + r$Z2 < 1 + 1e-12))
  expect_equal(r$estimate[upper.tri(r$estimate)], (0.5 * r$Z1 + r$Z2)[upper.tri(r$Z1)], tolerance = 1e-12)
  expect_error(Ibdmtx(G[1, , drop = FALSE]), "at least 2")
})

test_that("Semsro gives RMR and SRMR", {
  S <- matrix(c(4, 1.2, 0.8, 1.2, 2.5, 0.6, 0.8, 0.6, 1.8), 3)
  G <- matrix(c(3.9, 1.0, 0.9, 1.0, 2.6, 0.5, 0.9, 0.5, 1.7), 3)
  r <- Semsro(S, G)
  E <- S - G
  lt <- lower.tri(E, diag = TRUE)
  Z <- E / sqrt(outer(diag(S), diag(S)))
  expect_equal(r$rmr, sqrt(mean(E[lt]^2)), tolerance = 1e-12)
  expect_equal(r$srmr, sqrt(mean(Z[lt]^2)), tolerance = 1e-12)
  expect_equal(r$max_abs_standardised, max(abs(Z[lt])), tolerance = 1e-12)
  expect_equal(r$srmr_acceptable, as.numeric(r$srmr <= 0.08))
  expect_error(Semsro(S[1:2, ], G), "square")
  expect_error(Semsro(S, G[1:2, 1:2]), "same order")
  S2 <- S
  S2[1, 2] <- 5
  expect_error(Semsro(S2, G), "not symmetric")
})

test_that("SgsCrossValidation summarises leave-one-out SGS realisations", {
  P <- cbind(c(0, 1, 2, 0.5, 1.5), c(0, 0.5, 0, 1.5, 1))
  z <- c(1.2, 0.4, -0.3, 0.8, 0.1)
  mod <- list(model = "Exp", psill = 1, range = 1.5)
  r <- SgsCrossValidation(P, z, mod, nsim = 20, seed = 3, level = 0.8)
  err <- numeric(5)
  inside <- 0
  for (i in 1:5) {
    v <- SgsSimulate(P[-i, ], z[-i], P[i, , drop = FALSE], mod, nsim = 20, seed = 3 + i - 1)$realizations[, 1]
    q <- stats::quantile(v, c(0.1, 0.9), type = 7, names = FALSE)
    err[i] <- z[i] - mean(v)
    inside <- inside + (q[1] <= z[i] && z[i] <= q[2])
  }
  expect_equal(r$etype_error, err, tolerance = 1e-12)
  expect_equal(r$rmse, sqrt(mean(err^2)), tolerance = 1e-12)
  expect_equal(r$coverage, inside / 5)
  # one conditioning datum: every draw is the simple-kriging mean plus scaled noise
  one <- SgsSimulate(P[1, , drop = FALSE], z[1], P[2, , drop = FALSE], mod, nsim = 4, seed = 2)$realizations[, 1]
  c1 <- exp(-sqrt(1.25) / 1.5)
  e <- vapply(0:3, function(s) .morie_random_normal(1, seed = 2, stream = 3 * s + 1), 0)
  expect_equal(one, c1 * z[1] + sqrt(1 - c1^2) * e, tolerance = 1e-12)
})

test_that("ServR measures serendipity of a recommendation list", {
  r <- ServR(c(1, 2, 3, 4, 5), c(1, 2, 6), c(2, 4, 5, 7))
  expect_equal(r$unexpectedness, 3 / 5)
  expect_equal(r$serendipity, 2 / 5)
  expect_equal(r$precision, 3 / 5)
  expect_equal(r$recall, 3 / 4)
  expect_equal(c(r$tp, r$fp, r$fn, r$tn), c(3, 2, 1, 1))
  expect_error(ServR(numeric(0), 1, 1), "non-empty")
})

test_that("Sfbnds equal the linear-programming bounds on the ACE", {
  skip_if_not_installed("lpSolve")
  z <- rep(c(0, 1), each = 20)
  d <- c(rep(c(0, 0, 0, 1), 5), rep(c(1, 1, 1, 0), 5))
  y <- c(rep(c(0, 1, 0, 1, 1), 4), rep(c(1, 1, 0, 1, 0), 4))
  r <- Sfbnds(y, d, z)
  # response types: compliance c in {never, complier, defier, always} and
  # outcome type r in {never, helped, hurt, always}
  dz <- function(cc, zz) switch(cc, 0, zz, 1 - zz, 1)
  yd <- function(rr, dd) switch(rr, 0, dd, 1 - dd, 1)
  types <- expand.grid(cc = 1:4, rr = 1:4)
  A <- NULL
  b <- NULL
  for (zz in 0:1) for (dd in 0:1) for (yy in 0:1) {
    A <- rbind(A, as.numeric(vapply(seq_len(16), function(k) {
      dk <- dz(types$cc[k], zz)
      dk == dd && yd(types$rr[k], dk) == yy
    }, TRUE)))
    b <- c(b, mean(y[z == zz] == yy & d[z == zz] == dd))
  }
  A <- rbind(A, rep(1, 16))
  b <- c(b, 1)
  ace <- vapply(types$rr, function(rr) yd(rr, 1) - yd(rr, 0), 0)
  lo <- lpSolve::lp("min", ace, A, rep("=", nrow(A)), b)$objval
  hi <- lpSolve::lp("max", ace, A, rep("=", nrow(A)), b)$objval
  expect_equal(c(r$lower, r$upper), c(lo, hi), tolerance = 1e-9)
  pz1 <- mean(d[z == 1])
  pz0 <- mean(d[z == 0])
  expect_equal(r$late, (mean(y[z == 1]) - mean(y[z == 0])) / (pz1 - pz0), tolerance = 1e-12)
  expect_error(Sfbnds(y, d, rep(0, 40)), "both instrument arms")
  expect_error(Sfbnds(y + 1, d, z), "binary")
  expect_error(Sfbnds(y[-1], d, z), "same length")
})
