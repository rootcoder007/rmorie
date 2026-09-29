# Coverage for the doubly-robust DiD family (Dr*.R, dr*.R, dropr.R,
# dr_forest_native.R, droPDSI/droSPI): every estimate is rebuilt from a
# reference DR-DiD (Sant'Anna and Zhao 2020, eq. 2.6: logit propensity by
# glm, OLS outcome model on the controls, normalised weights).

drdid_ref <- function(dy, d, X = NULL) {
  n <- length(dy)
  Z <- if (is.null(X)) matrix(1, n, 1) else cbind(1, X)
  g <- stats::glm.fit(Z, d, family = stats::binomial(),
                      control = list(epsilon = 1e-14, maxit = 100))
  p <- pmin(pmax(g$fitted.values, 1e-12), 1 - 1e-12)
  Z0 <- Z[d < 0.5, , drop = FALSE]
  b0 <- solve(crossprod(Z0) + diag(1e-10, ncol(Z)), crossprod(Z0, dy[d < 0.5]))
  mu0 <- as.numeric(Z %*% b0)
  w1 <- d / sum(d)
  w0 <- p * (1 - d) / (1 - p)
  w0 <- w0 / sum(w0)
  tau <- sum((w1 - w0) * (dy - mu0))
  inf <- n * (w1 - w0) * (dy - mu0) - tau
  list(tau = tau, inf = inf, se = sqrt(sum(inf^2)) / n, pi = p, mu0 = mu0, w1 = w1, w0 = w0)
}
mammen_ref <- function(i) {
  f <- 1
  r <- 0
  k <- i + 1
  while (k > 0) {
    f <- f / 2
    r <- r + f * (k %% 2)
    k <- k %/% 2
  }
  r5 <- sqrt(5)
  if (r < (r5 + 1) / (2 * r5)) (1 - r5) / 2 else (1 + r5) / 2
}
q7 <- function(v, p) unname(stats::quantile(v, p, type = 7))
dr_x <- c(0.3, -1.2, 0.8, 1.5, -0.4, 0.9, -0.7, 1.1, 0.2, -1.5, 0.6, -0.1, 1.3, -0.9)
dr_d <- c(1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0)
dr_dy <- 0.5 + 1.2 * dr_d + 0.8 * dr_x + c(0.2, -0.3, 0.1, 0.4, -0.2, 0.3, -0.1,
                                          0.25, -0.35, 0.15, -0.05, 0.1, -0.2, 0.3)

test_that("the reference DR-DiD matches DRDID::drdid_panel", {
  skip_if_not_installed("DRDID")
  r <- DRDID::drdid_panel(y1 = dr_dy, y0 = rep(0, 14), D = dr_d,
                          covariates = cbind(1, dr_x), boot = FALSE)
  expect_equal(drdid_ref(dr_dy, dr_d, dr_x)$tau, r$ATT, tolerance = 1e-8)
})

test_that("Drclt, Drbsze, Drnpc, Drvst and Drweights on one cross-section", {
  ref <- drdid_ref(dr_dy, dr_d, dr_x)
  cl <- rep(c("p", "q", "r", "s", "t", "u", "v"), 2)
  r <- Drclt(dr_dy, dr_d, dr_x, cluster = cl)
  v <- sum(tapply(ref$inf, cl, sum)^2) * (7 / 6) * (13 / 12) / 14^2
  expect_equal(r$estimate, ref$tau, tolerance = 1e-9)
  expect_equal(r$se, sqrt(v), tolerance = 1e-9)
  expect_equal(r$se_iid, ref$se, tolerance = 1e-9)
  expect_equal(Drclt(dr_dy, dr_d)$dof_adj, (14 / 13) * (13 / 13), tolerance = 1e-12)
  expect_error(Drclt(numeric(0), numeric(0)), "empty")
  expect_error(Drclt(dr_dy, dr_d[-1]), "same length")
  expect_error(Drclt(dr_dy, dr_d, cluster = 1:3), "same length")
  b <- Drbsze(dr_dy, dr_d, dr_x, alpha = 0.1)
  z <- qnorm(0.95)
  df <- 5
  tq <- z + (z^3 + z) / 4 / df + (5 * z^5 + 16 * z^3 + 3 * z) / 96 / df^2 +
    (3 * z^7 + 19 * z^5 + 17 * z^3 - 15 * z) / 384 / df^3 +
    (79 * z^9 + 776 * z^7 + 1482 * z^5 - 1920 * z^3 - 945 * z) / 92160 / df^4
  sec <- ref$se * sqrt(14 / 12)
  expect_equal(b$crit_t, tq, tolerance = 1e-12)
  # the four-term Cornish-Fisher t quantile is within 1e-3 of qt at df = 5
  expect_lt(abs(b$crit_t - qt(0.95, 5)), 1e-3)
  expect_equal(c(b$ci_lo, b$ci_hi), ref$tau + c(-1, 1) * tq * sec, tolerance = 1e-9)
  expect_error(Drbsze(numeric(0), numeric(0)), "empty")
  expect_error(Drbsze(dr_dy, dr_d[-1]), "same length")
  expect_error(Drbsze(dr_dy, dr_d, alpha = 1), "strictly")
  expect_error(Drbsze(dr_dy, rep(1, 14)), "both treated")
  yneg <- dr_x^2 + 0.1 * dr_d
  np <- Drnpc(dr_dy, yneg, dr_d, dr_x)
  rn <- drdid_ref(yneg, dr_d, dr_x)
  expect_equal(c(np$tau_neg, np$se_neg), c(rn$tau, rn$se), tolerance = 1e-9)
  expect_equal(np$falsified, as.numeric(abs(rn$tau / rn$se) > qnorm(0.975)))
  expect_equal(np$tau_adj, ref$tau - rn$tau, tolerance = 1e-9)
  expect_error(Drnpc(numeric(0), numeric(0), numeric(0)), "empty")
  expect_error(Drnpc(dr_dy, yneg[-1], dr_d), "same length")
  expect_error(Drnpc(dr_dy, yneg, dr_d, alpha = 0), "strictly")
  expect_error(Drnpc(dr_dy, yneg, rep(0, 14)), "both treated")
  vs <- Drvst(dr_dy, dr_d, dr_x, q = 0.8)
  cap <- q7(ref$w0[dr_d == 0], 0.8)
  w0 <- ifelse(dr_d == 1 | ref$w0 <= cap, ref$w0, cap)
  w0 <- w0 / sum(w0)
  ws <- ref$w1 - w0
  ess <- function(w) sum(abs(w))^2 / sum(w^2)
  expect_equal(vs$cap, cap, tolerance = 1e-9)
  expect_equal(vs$estimate, sum(ws * (dr_dy - ref$mu0)), tolerance = 1e-9)
  expect_equal(vs$ess_ratio, ess(ws) / ess(ref$w1 - ref$w0), tolerance = 1e-9)
  expect_error(Drvst(numeric(0), numeric(0)), "empty")
  expect_error(Drvst(dr_dy, dr_d[-1]), "same length")
  expect_error(Drvst(dr_dy, dr_d, q = 0), "\\(0, 1\\]")
  expect_error(Drvst(dr_dy, rep(1, 14)), "both treated")
  y0 <- rep(0.2, 14)
  w <- Drweights(dr_dy + y0, dr_d, dr_x, y0 = y0)
  ed <- mean(dr_d)
  expect_equal(w$tau_reg, sum(dr_d * (dr_dy - ref$mu0)) / (14 * ed), tolerance = 1e-9)
  expect_equal(w$tau_ipw, sum((dr_d - ref$pi) / (ed * (1 - ref$pi)) * dr_dy) / 14,
               tolerance = 1e-9)
  expect_equal(w$tau_dr, ref$tau, tolerance = 1e-9)
})

test_that("Drbnk, Drctf, Drhtg, Drspa, Drspr, Drrct", {
  e <- c(0.4, 0.5, 0.45, 0.6, 0.3, 0.55, 0.5, 0.35, 0.4, 0.5, 0.65, 0.45, 0.5, 0.4)
  r <- Drbnk(dr_dy, dr_d, dr_x, pi_t = e)
  Z <- cbind(1, dr_x)
  b1 <- qr.solve(Z[dr_d == 1, ], dr_dy[dr_d == 1])
  b0 <- qr.solve(Z[dr_d == 0, ], dr_dy[dr_d == 0])
  m1 <- as.numeric(Z %*% b1)
  m0 <- as.numeric(Z %*% b0)
  psi <- m1 - m0 + dr_d * (dr_dy - m1) / e - (1 - dr_d) * (dr_dy - m0) / (1 - e)
  h <- sqrt(e / 14)
  est <- sum(h * psi) / sum(h)
  expect_equal(r$h, h, tolerance = 1e-12)
  expect_equal(r$estimate, est, tolerance = 1e-9)
  expect_equal(r$se, sqrt(sum((h * (psi - est))^2)) / sum(h), tolerance = 1e-9)
  expect_equal(Drbnk(dr_dy, dr_d)$aipw_unweighted,
               mean(dr_dy[dr_d == 1]) - mean(dr_dy[dr_d == 0]), tolerance = 1e-9)
  expect_error(Drbnk(numeric(0), numeric(0)), "empty")
  expect_error(Drbnk(dr_dy, dr_d[-1]), "same length")
  expect_error(Drbnk(dr_dy, rep(0, 14)), "both arms")
  expect_error(Drbnk(dr_dy, dr_d, pi_t = 0.5), "same length")
  expect_error(Drbnk(dr_dy, dr_d, pi_t = rep(1, 14)), "strictly inside")
  dose <- dr_d
  dose[c(3, 8, 11, 13)] <- 2
  ct <- Drctf(dr_dy, dose, dr_x)
  a <- vapply(c(1, 2), function(k) {
    idx <- c(which(dose == k), which(dose == 0))
    drdid_ref(dr_dy[idx], as.numeric(dose[idx] > 0), dr_x[idx])$tau
  }, 0)
  expect_equal(ct$att, a, tolerance = 1e-9)
  expect_equal(ct$acrt, a[2] - a[1], tolerance = 1e-9)
  nd <- c(sum(dose == 1), sum(dose == 2))
  expect_equal(ct$estimate, sum(nd * a) / sum(nd), tolerance = 1e-9)
  expect_error(Drctf(numeric(0), numeric(0)), "empty")
  expect_error(Drctf(dr_dy, dose[-1]), "same length")
  expect_error(Drctf(dr_dy, -dose), "non-negative")
  expect_error(Drctf(dr_dy, dose + 1), "no zero-dose")
  expect_error(Drctf(dr_dy, dose * 0), "no treated")
  st <- rep(c("a", "b"), 7)
  ht <- Drhtg(dr_dy, dr_d, NULL, strata = st)
  ca <- vapply(c("a", "b"), function(s) {
    i <- st == s
    mean(dr_dy[i & dr_d == 1]) - mean(dr_dy[i & dr_d == 0])
  }, 0)
  expect_equal(ht$catt, unname(ca), tolerance = 1e-9)
  expect_equal(ht$estimate, mean(ca), tolerance = 1e-9)
  expect_equal(ht$hetero_var, mean((ca - mean(ca))^2), tolerance = 1e-9)
  expect_equal(ht$pooled, mean(dr_dy[dr_d == 1]) - mean(dr_dy[dr_d == 0]), tolerance = 1e-9)
  expect_true(is.nan(Drhtg(dr_dy, dr_d, strata = c(rep("z", 2), rep("y", 12)))$catt[2]))
  expect_error(Drhtg(numeric(0), numeric(0)), "empty")
  expect_error(Drhtg(dr_dy, dr_d[-1]), "same length")
  expect_error(Drhtg(dr_dy, dr_d, strata = 1), "same length")
  W <- outer(1:14, 1:14, function(i, j) as.numeric(abs(i - j) == 1))
  sp <- Drspa(dr_dy, dr_d, dr_x, W_neighbors = W)
  wd <- as.numeric(W %*% dr_d) / rowSums(W)
  expect_equal(sp$estimate, drdid_ref(dr_dy, dr_d, cbind(dr_x, wd))$tau, tolerance = 1e-9)
  expect_equal(sp$tau_nospatial, drdid_ref(dr_dy, dr_d, dr_x)$tau, tolerance = 1e-9)
  expect_equal(sp$wd_sd, sd(wd), tolerance = 1e-12)
  expect_equal(Drspa(dr_dy, dr_d)$spatial_shift, 0)
  expect_equal(Drspa(dr_dy, dr_d, W_neighbors = W)$estimate,
               drdid_ref(dr_dy, dr_d, matrix(wd))$tau, tolerance = 1e-9)
  expect_error(Drspa(numeric(0), numeric(0)), "empty")
  expect_error(Drspa(dr_dy, dr_d[-1]), "same length")
  expect_error(Drspa(dr_dy, dr_d, X = 1:3), "one row per unit")
  expect_error(Drspa(dr_dy, dr_d, W_neighbors = diag(3)), "n x n")
  expect_error(Drspa(dr_dy, dr_d, W_neighbors = -W), "non-negative")
  ex <- c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1)
  sr <- Drspr(dr_dy, dr_d, NULL, exposure = ex)
  ctrl <- dr_d == 0 & ex == 0
  spl <- dr_d == 0 & ex == 1
  expect_equal(sr$att_direct, mean(dr_dy[dr_d == 1]) - mean(dr_dy[ctrl]), tolerance = 1e-9)
  expect_equal(sr$att_spillover, mean(dr_dy[spl]) - mean(dr_dy[ctrl]), tolerance = 1e-9)
  expect_true(is.nan(Drspr(dr_dy, dr_d)$att_spillover))
  expect_error(Drspr(numeric(0), numeric(0)), "empty")
  expect_error(Drspr(dr_dy, dr_d[-1]), "same length")
  expect_error(Drspr(dr_dy, dr_d, exposure = 1), "same length")
  G <- rep(c(1, 0), each = 7)
  dd <- c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0)
  yr <- dr_dy + 0.3 * dr_x^2
  rc <- Drrct(dr_dy, yr, dd, dr_x, G = G)
  Wm <- cbind(1, dd, dr_x)
  bs <- qr.solve(Wm[G == 1, ], yr[G == 1])
  al <- yr[G == 0] - as.numeric(Wm[G == 0, ] %*% bs)
  be <- qr.solve(cbind(Wm[G == 0, ], al), dr_dy[G == 0])
  expect_equal(rc$tau_secondary, unname(bs[2]), tolerance = 1e-8)
  expect_equal(rc$tau_esc, unname(be[2]), tolerance = 1e-8)
  expect_equal(rc$tau_naive, unname(qr.solve(Wm[G == 0, ], dr_dy[G == 0])[2]), tolerance = 1e-8)
  expect_error(Drrct(numeric(0), numeric(0), numeric(0)), "empty")
  expect_error(Drrct(dr_dy, yr[-1], dd), "same length")
  expect_error(Drrct(dr_dy, yr, dd, G = 1), "same length")
  expect_error(Drrct(dr_dy, yr, dd, G = rep(1, 14)), "both an experimental")
  expect_error(Drrct(dr_dy, yr, rep(c(1, 0), c(7, 7)), G = G), "both arms")
})

test_that("Drdidboot, Drdidblock and Drdidsplit", {
  ref <- drdid_ref(dr_dy, dr_d, dr_x)
  b <- Drdidboot(dr_dy, dr_d, dr_x, B = 9, alpha = 0.1)
  bt <- vapply(0:8, function(bb) ref$tau + sum(vapply(1:14, function(i)
    mammen_ref(bb * 14 + i - 1), 0) * ref$inf) / 14, 0)
  expect_equal(b$boot, bt, tolerance = 1e-9)
  se <- (q7(bt, 0.75) - q7(bt, 0.25)) / (qnorm(0.75) - qnorm(0.25))
  expect_equal(b$se, se, tolerance = 1e-9)
  expect_equal(b$ci_lo, ref$tau - qnorm(0.95) * se, tolerance = 1e-9)
  expect_equal(Drdidboot(dr_dy + 1, dr_d, dr_x, B = 3, y0 = rep(1, 14))$estimate,
               ref$tau, tolerance = 1e-9)
  cl <- rep(c("p", "q", "r", "s", "t", "u", "v"), 2)
  bk <- Drdidblock(dr_dy, dr_d, X = dr_x, clusters = cl, time = rep(1:2, 7), B = 7)
  gi <- match(cl, unique(cl))
  bb <- vapply(0:6, function(k) {
    w <- vapply(0:6, function(g) mammen_ref(k * 7 + g), 0)
    ref$tau + sum(w[gi] * ref$inf) / 14
  }, 0)
  expect_equal(bk$boot, bb, tolerance = 1e-9)
  expect_equal(bk$se, sd(bb), tolerance = 1e-9)
  expect_equal(bk$n_periods, 2L)
  expect_equal(sum(names(bk) == "n_periods"), 1L)
  expect_equal(Drdidblock(dr_dy, dr_d, unit = cl, B = 2)$n_clusters, 7L)
  expect_error(Drdidblock(dr_dy, dr_d, time = 1:3), "one entry per observation")
  sp <- Drdidsplit(dr_dy, dr_d, K = 3)
  fold <- (seq_len(14) - 1) %% 3
  ft <- vapply(0:2, function(f) {
    tr <- fold != f
    te <- fold == f
    p <- mean(dr_d[tr])
    mu <- mean(dr_dy[tr & dr_d == 0])
    dt <- dr_d[te]
    w0 <- p * (1 - dt) / (1 - p)
    sum((dt / sum(dt) - w0 / sum(w0)) * (dr_dy[te] - mu))
  }, 0)
  expect_equal(sp$fold_tau, ft, tolerance = 1e-9)
  expect_equal(sp$fold_n, c(5L, 5L, 4L))
  expect_equal(sp$estimate, sum(c(5, 5, 4) * ft) / 14, tolerance = 1e-9)
  expect_equal(sp$full_tau, mean(dr_dy[dr_d == 1]) - mean(dr_dy[dr_d == 0]), tolerance = 1e-9)
  # a fold whose covariate separates D must not turn the weights into NaN
  sx <- Drdidsplit(dr_dy, dr_d, dr_x, K = 2)
  expect_true(is.finite(sx$estimate))
})

dp_unit <- rep(c("a", "b", "c", "d", "e", "f", "g"), each = 5)
dp_time <- rep(1:5, 7)
dp_coh <- rep(c(3, 3, 4, 0, 0, 0, 4), each = 5)
dp_y <- rep(c(1, 2, 1.5, 0.5, 1.2, 0.8, 2.2), each = 5) + 0.4 * dp_time +
  ifelse(dp_coh > 0 & dp_time >= dp_coh, 1 + dp_time - dp_coh, 0) +
  0.1 * sin(seq_along(dp_time))
dp_D <- as.numeric(dp_coh > 0 & dp_time >= dp_coh)
cell_did <- function(cc, t1, ctrl_coh = 0) {
  y1 <- dp_y[dp_time == t1]
  y0 <- dp_y[dp_time == cc - 1]
  g <- dp_coh[dp_time == t1]
  dy <- y1 - y0
  mean(dy[g == cc]) - mean(dy[g == ctrl_coh])
}

test_that("Drsta and Drcef aggregate cohort-time DR-DiD cells", {
  s <- Drsta(dp_y, dp_D, dp_unit, dp_time, dp_coh)
  expect_equal(s$event_time, -3:2)
  att3 <- vapply(-3:2, function(e) if (3 + e >= 1 && 3 + e <= 5) cell_did(3, 3 + e) else NaN, 0)
  att4 <- vapply(-3:2, function(e) if (4 + e >= 1 && 4 + e <= 5) cell_did(4, 4 + e) else NaN, 0)
  expect_equal(s$att, c(att3, att4), tolerance = 1e-9)
  post <- c(2 * att3[4:6], 2 * att4[4:5])
  expect_equal(s$estimate, sum(post) / 10, tolerance = 1e-9)
  expect_error(Drsta(numeric(0), 0, "a", 1, 1), "empty")
  expect_error(Drsta(dp_y, dp_D, dp_unit[-1], dp_time, dp_coh), "same length")
  expect_error(Drsta(dp_y, dp_D, dp_unit, dp_time, rep(0, 35)), "no treated cohort")
  expect_error(Drsta(dp_y, dp_D, dp_unit, dp_time, rep(3, 35)), "never-treated")
  cf <- Drcef(dp_y, dp_D, dp_unit, dp_time, dp_coh)
  th <- vapply(cf$event_time, function(e) {
    ok <- c(3, 4) + e <= 5 & c(3, 4) + e >= 1
    sz <- c(2, 2)[ok]
    v <- vapply(c(3, 4)[ok], function(cc) cell_did(cc, cc + e), 0)
    sum(sz / sum(sz) * v)
  }, 0)
  expect_equal(cf$event_time, -3:2)
  expect_equal(cf$theta, th, tolerance = 1e-9)
  expect_equal(cf$estimate, mean(th[cf$event_time >= 0]), tolerance = 1e-9)
  expect_error(Drcef(numeric(0), 0, "a", 1, 1), "empty")
  expect_error(Drcef(dp_y, dp_D, dp_unit[-1], dp_time, dp_coh), "same length")
  expect_error(Drcef(dp_y, dp_D, dp_unit, dp_time, rep(0, 35)), "no treated cohort")
  expect_error(Drcef(dp_y, dp_D, dp_unit, dp_time, rep(3, 35)), "never-treated")
})

test_that("Drlp1 and Drdiddyn build local-projection and event-time cells", {
  lp <- Drlp1(dp_y, dp_D, dp_unit, dp_time, horizon = 1)
  key <- paste(dp_unit, dp_time)
  cells <- function(h) {
    dy <- numeric(0)
    lab <- numeric(0)
    for (j in 2:5) {
      if (j + h > 5) next
      for (u in unique(dp_unit)) {
        ic <- match(paste(u, j), key)
        ip <- match(paste(u, j - 1), key)
        ih <- match(paste(u, j + h), key)
        new <- dp_D[ic] == 1 && dp_D[ip] == 0
        if (!(new || dp_D[ih] == 0)) next
        dy <- c(dy, dp_y[ih] - dp_y[ip])
        lab <- c(lab, as.numeric(new))
      }
    }
    mean(dy[lab == 1]) - mean(dy[lab == 0])
  }
  expect_equal(lp$beta, c(cells(0), cells(1)), tolerance = 1e-9)
  expect_error(Drlp1(numeric(0), 0, "a", 1), "empty")
  expect_error(Drlp1(dp_y, dp_D[-1], dp_unit, dp_time), "same length")
  expect_error(Drlp1(dp_y, dp_D, dp_unit, dp_time, horizon = -1), "non-negative")
  dy <- Drdiddyn(dp_y, dp_D, dp_unit, dp_time, dp_coh, horizon = 1)
  ev <- function(e) {
    tr <- c()
    ct <- c()
    for (u in unique(dp_unit)) {
      g <- dp_coh[dp_unit == u][1]
      yy <- dp_y[dp_unit == u]
      if (g > 0) {
        if (g + e >= 1 && g + e <= 5) tr <- c(tr, yy[g + e] - yy[g - 1])
      } else {
        for (g2 in c(3, 4)) if (g2 + e <= 5) ct <- c(ct, yy[g2 + e] - yy[g2 - 1])
      }
    }
    mean(tr) - mean(ct)
  }
  expect_equal(dy$att, c(ev(-1), ev(0), ev(1)), tolerance = 1e-9)
  expect_equal(dy$estimate, mean(c(ev(0), ev(1))), tolerance = 1e-9)
})

test_that("Dropmask is inverted dropout", {
  r <- Dropmask(c(1, 2, 3, 4), c(1, 0, 1, 1), rate = 0.25)
  expect_equal(r$activation, c(1, 0, 3, 4) / 0.75, tolerance = 1e-12)
  expect_equal(c(r$kept, r$dropped), c(3, 1))
  expect_error(Dropmask(1:2, 1, 0.1), "same length")
  expect_error(Dropmask(1, 1, 1), "\\[0, 1\\)")
  expect_error(Dropmask(1, 0.5, 0.1), "0 or 1")
})

test_that("overlap-weighted DR, placebo DR-DiD and the loss forest", {
  X <- cbind(dr_x, dr_x^2)
  ps <- c(0.4, 0.3, 0.6, 0.7, 0.35, 0.5, 0.45, 0.55, 0.4, 0.3, 0.6, 0.5, 0.65, 0.4)
  r <- morie_dr_overlap_weighted(dr_dy, dr_d, dr_x, ps = ps, n_folds = 2, seed = 3)
  fold <- integer(14)
  withr::with_seed(3, fold[sample.int(14)] <- (0:13) %% 2)
  A <- cbind(1, dr_x)
  mu1 <- mu0 <- numeric(14)
  for (f in 0:1) {
    te <- fold == f
    for (l in c(1, 0)) {
      m <- !te & dr_d == l
      pr <- if (sum(m) > 2) as.numeric(A[te, ] %*% qr.solve(A[m, ], dr_dy[m])) else
        rep(mean(dr_dy[m]), sum(te))
      if (l == 1) mu1[te] <- pr else mu0[te] <- pr
    }
  }
  h <- ps * (1 - ps)
  psi <- mu1 - mu0 + dr_d * (dr_dy - mu1) / ps - (1 - dr_d) * (dr_dy - mu0) / (1 - ps)
  ate <- sum(h * psi) / sum(h)
  expect_equal(r$mu1, mu1, tolerance = 1e-10)
  expect_equal(r$ate, ate, tolerance = 1e-10)
  expect_equal(r$se, sd(h * (psi - ate) / (sum(h) / 14)) / sqrt(14), tolerance = 1e-10)
  g <- morie_dr_overlap_weighted(dr_dy, dr_d, X, seed = 1)
  expect_true(all(g$propensity >= 1e-4 & g$propensity <= 1 - 1e-4))
  hh <- g$propensity * (1 - g$propensity)
  ps2 <- g$mu1 - g$mu0 + dr_d * (dr_dy - g$mu1) / g$propensity -
    (1 - dr_d) * (dr_dy - g$mu0) / (1 - g$propensity)
  expect_equal(g$ate, sum(hh * ps2) / sum(hh), tolerance = 1e-12)
  expect_error(morie_dr_overlap_weighted(dr_dy[-1], dr_d, dr_x), "agree")
  expect_error(morie_dr_overlap_weighted(dr_dy, dr_d + 1, dr_x), "0/1")
  expect_error(morie_dr_overlap_weighted(dr_dy, dr_d, dr_x, n_folds = 1), "at least 2")
  pl <- morie_placebo_dr_did(rep(0, 14), dr_dy, dr_d, dr_x, ps = ps, seed = 3)
  expect_equal(pl$placebo_effect, ate, tolerance = 1e-10)
  expect_equal(pl$p_value, 2 * pnorm(-abs(ate / r$se)), tolerance = 1e-10)
  expect_equal(pl$min_detectable, 2.8 * r$se, tolerance = 1e-10)
  expect_error(morie_placebo_dr_did(1:2, 1:3, dr_d, dr_x), "same length")
  set <- 1:40
  xf <- cbind(sin(set), cos(set / 3))
  yf <- xf[, 1] + (set %% 2) * (1 + xf[, 2]) + 0.1 * cos(set)
  lf <- morie_egregious_loss_forest(yf, set %% 2, xf, n_trees = 10L, min_leaf = 3L,
                                    max_depth = 3L, seed = 1L)
  ok <- is.finite(lf$cate)
  expect_equal(lf$ate, mean(lf$cate[ok]), tolerance = 1e-12)
  expect_equal(lf$se, sd(lf$cate[ok]) / sqrt(sum(ok)), tolerance = 1e-12)
  expect_equal(lf$n_leaves, length(lf$leaf_sizes))
  expect_error(morie_egregious_loss_forest(yf[-1], set %% 2, xf), "rows but y")
  expect_error(morie_egregious_loss_forest(yf, set %% 2, xf, imbalance_penalty = -1),
               "non-negative")
})

test_that("Palmer PDSI water balance and SPI", {
  P <- c(80, 40, 10, 5, 60, 90, 120, 30, 20, 70, 15, 55, 35, 95, 25)
  PE <- c(50, 60, 70, 80, 60, 40, 30, 50, 60, 40, 70, 45, 55, 35, 65)
  r <- morie_droPDSI_palmer_pdsi(P, PE, awc = 60)
  expect_same_function(morie_droPDSI, morie_droPDSI_palmer_pdsi)
  ss <- 25.4
  su <- 34.6
  et <- rr <- ro <- ll <- numeric(15)
  for (i in 1:15) {
    if (P[i] >= PE[i]) {
      ex <- P[i] - PE[i]
      a <- min(25.4 - ss, ex)
      b <- min(34.6 - su, ex - a)
      ss <- ss + a
      su <- su + b
      et[i] <- PE[i]
      rr[i] <- a + b
      ro[i] <- ex - a - b
    } else {
      need <- PE[i] - P[i]
      a <- min(ss, need)
      b <- min(su, (need - a) * su / 60)
      ss <- ss - a
      su <- su - b
      et[i] <- P[i] + a + b
      ll[i] <- a + b
    }
  }
  expect_equal(r$evapotranspiration, et, tolerance = 1e-12)
  expect_equal(r$recharge, rr, tolerance = 1e-12)
  expect_equal(r$runoff, ro, tolerance = 1e-12)
  expect_equal(r$loss, ll, tolerance = 1e-12)
  expect_equal(r$alpha, sum(et) / sum(PE), tolerance = 1e-12)
  expect_equal(r$pdsi, as.numeric(stats::filter(r$z_index / 3, 0.897, method = "recursive")),
               tolerance = 1e-12)
  expect_equal(r$departure, P - r$cafec_precip, tolerance = 1e-12)
  expect_error(morie_droPDSI_palmer_pdsi(numeric(0), numeric(0)), "empty")
  expect_error(morie_droPDSI_palmer_pdsi(1:3, 1:2), "PET values")
  expect_error(morie_droPDSI_palmer_pdsi(1:3, 1:3, awc = 0), "positive")
  expect_error(morie_droPDSI_palmer_pdsi(1:3, 1:3, month = 1:2), "month labels")
  pr <- c(12, 0, 30, 8, 22, 15, 40, 5, 18, 26, 0, 33, 14, 9, 21, 28, 6, 17)
  s <- morie_droSPI(pr, scale = 2, by_month = FALSE)
  tot <- c(NA, stats::filter(pr, c(1, 1), sides = 1)[-1])
  pos <- tot[-1][tot[-1] > 0]
  A <- log(mean(pos)) - mean(log(pos))
  al <- (1 + sqrt(1 + 4 * A / 3)) / (4 * A)
  expect_equal(unlist(s$totals), tot)
  expect_equal(s$params$pooled[2:3], c(al, mean(pos) / al), tolerance = 1e-12)
  h <- pgamma(tot[5], shape = al, scale = mean(pos) / al)
  # Abramowitz-Stegun 26.2.23 is accurate to 4.5e-4
  expect_lt(abs(s$spi[[5]] - qnorm(h)), 4.5e-4)
  expect_same_function(spi, morie_droSPI)
  expect_same_function(standardized_precipitation_index, morie_droSPI)
  expect_same_function(morie_drospi, morie_droSPI)
  expect_error(morie_droSPI(-pr), "non-negative")
  expect_error(morie_droSPI(pr[1:4], scale = 2), "too short")
  expect_error(morie_droSPI(rep(0, 12), scale = 1, by_month = FALSE), "three positive")
})
