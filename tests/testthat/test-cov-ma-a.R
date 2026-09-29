# Coverage for the meta-analysis shelf part 1 (Baujat, Bayesian RE,
# Begg, centred meta-regression, cumulative MA, MAD tools, MAE, fixed
# effect, funnel, Freeman-Tukey, fail-safe N, Galbraith, binomial-normal
# GLMM, Glass's delta, Hedges' g); checked against metafor and formulas.

yi <- c(0.32, 0.15, 0.61, -0.05, 0.42, 0.28, 0.51)
vi <- c(0.04, 0.02, 0.09, 0.05, 0.03, 0.06, 0.08)

test_that("Baujat, mafix and Mafnpr", {
  b <- Baujat(yi, vi)
  w <- 1 / vi
  th <- sum(w * yi) / sum(w)
  expect_equal(b$x, w * (yi - th)^2, tolerance = 1e-12)
  loo <- vapply(1:7, function(i) sum(w[-i] * yi[-i]) / sum(w[-i]), 0)
  expect_equal(b$y, (th - loo)^2 * (sum(w) - w), tolerance = 1e-12)
  skip_if_not_installed("metafor")
  f <- metafor::rma(yi, vi, method = "FE")
  r <- mafix(yi, vi, level = 0.9)
  expect_equal(c(r$estimate, r$se, r$Q), c(f$b[1], f$se, f$QE), tolerance = 1e-9)
  expect_equal(r$ci_lower, th - qnorm(0.95) * sqrt(1 / sum(w)), tolerance = 1e-12)
  expect_equal(r$p_Q, f$QEp, tolerance = 1e-9)
  expect_equal(morie_mafix(yi, vi)$weights, w / sum(w), tolerance = 1e-12)
  fp <- Mafnpr(yi, sqrt(vi))
  expect_equal(fp$ci_hi, th + qnorm(0.975) * sqrt(vi), tolerance = 1e-12)
  expect_error(Mafnpr(yi, -vi), "strictly positive")
})

test_that("mabay and morie_mabay give the conjugate RE posterior", {
  skip_if_not_installed("metafor")
  yh <- c(0.1, 0.8, -0.3, 0.6, 1.1)
  vh <- c(0.04, 0.02, 0.09, 0.05, 0.03)
  f <- metafor::rma(yh, vh, method = "DL")
  expect_gt(f$tau2, 0)
  r <- mabay(yh, vh)
  expect_equal(c(r$tau2, r$estimate, r$se), c(f$tau2, f$b[1], f$se), tolerance = 1e-9)
  t2 <- f$tau2
  prec <- 1 / vh + 1 / t2
  expect_equal(r$theta_mean, (yh / vh + r$estimate / t2) / prec, tolerance = 1e-9)
  expect_equal(r$shrinkage, 1 - (1 / prec) / vh, tolerance = 1e-9)
  z <- morie_mabay(yi, vi, tau2 = 0)
  expect_equal(z$theta_mean, rep(sum(yi / vi) / sum(1 / vi), 7), tolerance = 1e-12)
})

test_that("Beggtest is Kendall's tau between standardised effects and variances", {
  r <- Beggtest(yi, vi)
  w <- 1 / vi
  th <- sum(w * yi) / sum(w)
  ys <- (yi - th) / sqrt(vi - 1 / sum(w))
  expect_equal(r$tau, cor(ys, vi, method = "kendall"), tolerance = 1e-12)
  S <- sum(sign(outer(ys, ys, "-")) * sign(outer(vi, vi, "-"))) / 2
  z <- S / sqrt(7 * 6 * 19 / 18)
  expect_equal(r$statistic, z, tolerance = 1e-12)
  expect_equal(r$p_value, 2 * pnorm(-abs(z)), tolerance = 1e-12)
  expect_error(Beggtest(yi[1:2], vi[1:2]), "at least 3")
})

test_that("mac3 and morie_mac3 fit the DL meta-regression on centred moderators", {
  skip_if_not_installed("metafor")
  x <- c(1, 3, 2, 5, 4, 2, 6)
  r <- mac3(yi, vi, x)
  xc <- x - sum(x / vi) / sum(1 / vi)
  f <- metafor::rma(yi, vi, mods = ~xc, method = "DL")
  expect_equal(r$tau2_resid, f$tau2, tolerance = 1e-9)
  expect_equal(r$coefficients, as.numeric(f$b), tolerance = 1e-9)
  expect_equal(r$se, f$se, tolerance = 1e-9)
  expect_equal(r$QE, f$QE, tolerance = 1e-9)
  u <- morie_mac3(yi, vi, x, weighted = FALSE)
  expect_equal(u$centers, mean(x))
})

test_that("macum and morie_macum accumulate DL fits", {
  skip_if_not_installed("metafor")
  ord <- c(3, 1, 2, 7, 5, 4, 6)
  r <- macum(yi, vi, order = ord)
  o <- order(ord)
  est <- vapply(1:7, function(j) {
    if (j == 1) return(yi[o[1]])
    as.numeric(metafor::rma(yi[o[1:j]], vi[o[1:j]], method = "DL")$b)
  }, 0)
  expect_equal(r$cumulative, est, tolerance = 1e-9)
  expect_equal(r$order_index, o - 1L)
  expect_equal(morie_macum(yi, vi)$estimate, as.numeric(metafor::rma(yi, vi, method = "DL")$b), tolerance = 1e-9)
})

test_that("madAd, madMov, madsc and Maetst", {
  x <- c(2.1, 2.4, 1.9, 2.2, 8.5, 2.0, 2.3, -4)
  a <- madAd(x)
  m <- median(x)
  md <- median(abs(x - m))
  expect_equal(a$scores, 0.6745 * (x - m) / md, tolerance = 1e-12)
  expect_equal(a$n_outliers, sum(abs(0.6745 * (x - m) / md) > 3.5))
  expect_equal(morie_madAd(rep(1, 3))$estimate, 0)
  mv <- madMov(x, 4)
  expect_equal(mv$values, vapply(1:5, function(j) mad(x[j:(j + 3)]), 0), tolerance = 1e-12)
  expect_equal(morie_madMov(x, 9)$values, numeric(0))
  expect_equal(madsc(x)$estimate, mad(x), tolerance = 1e-12)
  expect_equal(morie_madsc(x, center = 0)$raw_mad, median(abs(x)), tolerance = 1e-12)
  expect_equal(Maetst(c(1, 2, 3), c(1.5, 2, 2))$mae, 0.5, tolerance = 1e-12)
  expect_error(Maetst(1:2, 1), "same length")
})

test_that("Mafrt, Mafrti, Mafsn, Magal, Magsd and mahg", {
  x <- c(3, 10, 0, 18)
  n <- c(20, 25, 15, 20)
  ft <- Mafrt(x, n)
  expect_equal(ft$ft, asin(sqrt(x / (n + 1))) + asin(sqrt((x + 1) / (n + 1))), tolerance = 1e-12)
  skip_if_not_installed("metafor")
  es <- metafor::escalc("PFT", xi = x, ni = n)
  expect_equal(ft$ft, 2 * as.numeric(es$yi), tolerance = 1e-12)
  expect_equal(ft$var, 4 * as.numeric(es$vi), tolerance = 1e-12)
  bt <- Mafrti(ft$ft, 20)
  s <- sin(ft$ft)
  p <- 0.5 * (1 - sign(cos(ft$ft)) * sqrt(pmax(1 - (s + (s - 1 / s) / 20)^2, 0)))
  expect_equal(bt$p, pmin(pmax(p, 0), 1), tolerance = 1e-12)
  expect_error(Mafrti(1, 0), "positive")

  z <- c(1.2, 2.1, 0.4, 1.8)
  fs <- Mafsn(z, alpha = 0.05)
  expect_equal(fs$Nfs, sum(z)^2 / qnorm(0.95)^2 - 4, tolerance = 1e-12)
  g <- Magal(yi, sqrt(vi))
  zz <- yi / sqrt(vi)
  xx <- 1 / sqrt(vi)
  expect_equal(g$slope, unname(coef(lm(zz ~ 0 + xx))), tolerance = 1e-12)

  gd <- Magsd(10, 8, 4, 20, 25)
  expect_equal(gd$delta, 0.5, tolerance = 1e-12)
  expect_equal(gd$var, 45 / 500 + 0.25 / 48, tolerance = 1e-12)
  expect_error(Magsd(1, 1, 0, 2, 2), "positive")
  h <- mahg(10, 8, 4, 5, 20, 25)
  sp <- sqrt((19 * 16 + 24 * 25) / 43)
  J <- exp(lgamma(21.5) - 0.5 * log(21.5) - lgamma(21))
  expect_equal(h$estimate, J * 2 / sp, tolerance = 1e-12)
  esd <- metafor::escalc("SMD", m1i = 10, m2i = 8, sd1i = 4, sd2i = 5, n1i = 20, n2i = 25)
  expect_equal(h$estimate, as.numeric(esd$yi), tolerance = 1e-9)
  expect_equal(morie_ma_hedges_g(10, 8, 4, 5, 20, 25)$J_approx, 1 - 3 / 171, tolerance = 1e-12)
})

test_that("magpa and morie_magpa maximise the Gauss-Hermite binomial-normal likelihood", {
  x <- c(3, 7, 2, 10, 5)
  n <- c(20, 25, 18, 30, 22)
  r <- magpa(x, n, quad = 15)
  B <- matrix(0, 15, 15)
  for (i in 1:14) B[i, i + 1] <- B[i + 1, i] <- sqrt(i / 2)
  e <- eigen(B, symmetric = TRUE)
  gx <- e$values
  gw <- sqrt(pi) * e$vectors[1, ]^2
  ll <- function(mu, s) sum(vapply(1:5, function(i) {
    p <- plogis(mu + s * sqrt(2) * gx)
    log(sum(gw * dbinom(x[i], n[i], p)) / sqrt(pi))
  }, 0))
  expect_equal(r$loglik, ll(r$logit_mu, r$sigma), tolerance = 1e-9)
  # golden sections to ~1e-14 in mu and sigma: nudges cannot raise the likelihood
  for (d in c(-1e-3, 1e-3)) {
    expect_lte(ll(r$logit_mu + d, r$sigma), r$loglik + 1e-9)
    expect_lte(ll(r$logit_mu, r$sigma + d), r$loglik + 1e-9)
  }
  expect_equal(r$estimate, plogis(r$logit_mu), tolerance = 1e-12)
  expect_equal(morie_magpa(x, n, quad = 15)$sigma, r$sigma)
})
