# Coverage for the meta-analysis shelf part 3 (Orwin, Peto, correlated
# g, risk difference, meta-regression and R^2, DL, prediction interval,
# H^2, two-step tau^2, trim and fill, three-level MA), conditional
# logistic regression, Matern kernels, the matrix profile and four marine
# indices; checked against metafor, survival and base R.

yh <- c(0.1, 0.8, -0.3, 0.6, 1.1, 0.4, 0.2)
vh <- c(0.04, 0.02, 0.09, 0.05, 0.03, 0.06, 0.05)

test_that("Maorw, Mapeto, Marba and mard", {
  o <- Maorw(0.5, 0.2, 0, 10)
  expect_equal(o$Nfs, 10 * 0.3 / 0.2, tolerance = 1e-12)
  expect_error(Maorw(0.5, 0.2, 0.2, 10), "must differ")
  a <- c(12, 5, 2, 8)
  b <- c(38, 45, 20, 32)
  c <- c(6, 9, 3, 4)
  d <- c(44, 41, 17, 36)
  skip_if_not_installed("metafor")
  pe <- metafor::rma.peto(ai = a, bi = b, ci = c, di = d)
  p <- Mapeto(a, b, c, d)
  expect_equal(p$log_OR, unname(pe$b[1]), tolerance = 1e-9)
  expect_equal(p$se_log, pe$se, tolerance = 1e-9)
  mb <- Marba(0.4, 30, 0.6)
  J <- 1 - 3 / (4 * 29 - 1)
  expect_equal(mb$var_g, J^2 * (2 * 0.4 / 30 + 0.16 / 58), tolerance = 1e-12)
  expect_error(Marba(1, 30, 2), "rho")
  rd <- mard(a, b, c, d)
  es <- metafor::escalc("RD", ai = a, bi = b, ci = c, di = d)
  expect_equal(rd$estimate, as.numeric(es$yi), tolerance = 1e-12)
  expect_equal(morie_mard(a, b, c, d)$variance, as.numeric(es$vi), tolerance = 1e-12)
})

test_that("marndm, matau2pi, math2, matr and Mareg/marpct against metafor", {
  skip_if_not_installed("metafor")
  f <- metafor::rma(yh, vh, method = "DL")
  r <- marndm(yh, vh)
  expect_equal(c(r$estimate, r$se, r$tau2, r$Q), c(f$b[1], f$se, f$tau2, f$QE), tolerance = 1e-9)
  expect_equal(morie_marndm(yh, vh)$p_Q, f$QEp, tolerance = 1e-9)
  pi <- matau2pi(yh, vh)
  sp <- sqrt(f$tau2 + f$se^2)
  expect_equal(c(pi$pi_lower, pi$pi_upper), f$b[1] + c(-1, 1) * qt(0.975, 5) * sp, tolerance = 1e-9)
  expect_equal(morie_matau2pi(yh, vh)$pi_upper_km1, f$b[1] + qt(0.975, 6) * sp, tolerance = 1e-9)
  h <- math2(yh, vh)
  expect_equal(h$estimate, f$QE / 6, tolerance = 1e-9)
  expect_equal(morie_math2(yh, vh)$H, sqrt(f$QE / 6), tolerance = 1e-9)
  mt <- matr(yh, vh)
  the <- max(0, var(yh) - mean(vh))
  a <- 1 / (vh + the)
  yb <- sum(a * yh) / sum(a)
  t2 <- max(0, (sum(a * (yh - yb)^2) - sum(a * vh) + sum(a^2 * vh) / sum(a)) / (sum(a) - sum(a^2) / sum(a)))
  expect_equal(c(mt$tau2_he, mt$estimate), c(the, t2), tolerance = 1e-12)
  expect_equal(morie_matr(yh, vh)$mu, sum(yh / (vh + t2)) / sum(1 / (vh + t2)), tolerance = 1e-12)
  x <- c(1, 3, 2, 5, 4, 2, 6)
  fm <- metafor::rma(yh, vh, mods = ~x, method = "DL")
  mr <- Mareg(yh, vh, cbind(1, x))
  expect_equal(c(mr$tau2, mr$beta, mr$se), c(fm$tau2, as.numeric(fm$b), fm$se), tolerance = 1e-9)
  expect_equal(mr$QE, fm$QE, tolerance = 1e-9)
  expect_equal(mr$R2, max(0, 1 - fm$tau2 / f$tau2), tolerance = 1e-9)
  rp <- marpct(yh, vh, x)
  expect_equal(rp$estimate, 100 * max(0, (f$tau2 - fm$tau2) / f$tau2), tolerance = 1e-9)
  expect_equal(morie_marpct(yh, vh, x)$coefficients, as.numeric(fm$b), tolerance = 1e-9)
  expect_error(Mareg(yh, vh, matrix(1, 7, 8)), "more moderators")
})

test_that("Matrim matches metafor's L0 trim and fill on a fixed-effect model", {
  skip_if_not_installed("metafor")
  y <- c(0.8, 0.6, 0.55, 0.7, 0.4, 0.9, 1.1, 0.5, 0.35, 0.65)
  v <- c(0.05, 0.03, 0.02, 0.06, 0.01, 0.08, 0.12, 0.02, 0.01, 0.04)
  r <- Matrim(y, v, side = "left")
  tf <- metafor::trimfill(metafor::rma(y, v, method = "FE"), side = "left", estimator = "L0")
  expect_equal(r$k_filled, as.integer(tf$k0))
  expect_equal(r$estimate, as.numeric(tf$b), tolerance = 1e-9)
})

test_that("matrl and morie_matrl match metafor's three-level ML fit", {
  skip_if_not_installed("metafor")
  set.seed(2)
  cl <- rep(1:5, c(3, 2, 4, 3, 2))
  u <- rnorm(5, 0, 0.3)
  vi <- runif(14, 0.01, 0.05)
  yi <- 0.3 + u[cl] + rnorm(14, 0, 0.2) + rnorm(14, 0, sqrt(vi))
  r <- matrl(yi, vi, cl)
  f <- metafor::rma.mv(yi, vi, random = ~ 1 | cl / obs, data = data.frame(cl = cl, obs = seq_along(yi)),
                       method = "ML", control = list(rel.tol = 1e-12))
  # nested golden sections vs rma.mv's optimiser: agreement to ~1e-6 in the variances
  expect_equal(c(r$tau2_level3, r$tau2_level2), f$sigma2, tolerance = 1e-5)
  expect_equal(r$estimate, as.numeric(f$b), tolerance = 1e-6)
  expect_equal(r$loglik, as.numeric(logLik(f)), tolerance = 1e-8)
  expect_equal(morie_matrl(yi, vi, cl)$n_clusters, 5L)
})

test_that("Matccd maximises the conditional likelihood of matched sets", {
  set.seed(3)
  sets <- rep(1:12, each = 3)
  case <- rep(c(1, 0, 0), 12)
  x <- rnorm(36) + 0.8 * case
  r <- Matccd(case, 1 - case, sets, x)
  cll <- function(b) sum(vapply(1:12, function(s) {
    i <- which(sets == s)
    b * x[i][case[i] == 1] - log(sum(exp(b * x[i])))
  }, 0))
  opt <- optimize(cll, c(-5, 5), maximum = TRUE, tol = 1e-12)
  expect_equal(r$log_or, opt$maximum, tolerance = 1e-7)
  expect_equal(r$loglik, cll(r$log_or), tolerance = 1e-12)
  h <- 1e-4
  info <- -(cll(r$log_or + h) - 2 * cll(r$log_or) + cll(r$log_or - h)) / h^2
  # central second difference with h = 1e-4 against the analytic information
  expect_equal(r$information, info, tolerance = 1e-6)
  expect_equal(r$se, 1 / sqrt(r$information), tolerance = 1e-12)
  expect_error(Matccd(rep(0, 36), NULL, sets, x), "exactly one case")
  expect_error(Matccd(case, case, sets, x), "complement")
})

test_that("MatnK and Maternvg use the Matern correlation", {
  d <- c(0, 0.3, 1, 2.5)
  r <- MatnK(d, nu = 1.5, rho = 0.8, sigma2 = 2)
  z <- sqrt(3) * d / 0.8
  expect_equal(r$k, ifelse(d == 0, 2, 2 * 2^(1 - 1.5) / gamma(1.5) * z^1.5 * besselK(z, 1.5)), tolerance = 1e-9)
  expect_equal(r$k[-1], 2 * (1 + z[-1]) * exp(-z[-1]), tolerance = 1e-9)
  expect_error(MatnK(d, 0, 1), "smoothness")
  g <- Maternvg(c(0, 0.5, 2), c0 = 0.1, c = 1, a = 0.7, nu = 0.5)
  expect_equal(g$gamma, c(0, 0.1 + 1 - exp(-c(0.5, 2) / 0.7)), tolerance = 1e-9)
  expect_equal(g$exponential, g$gamma, tolerance = 1e-9)
  expect_equal(g$sill, 1.1)
})

test_that("morie_matrxP is the brute-force z-normalised matrix profile", {
  set.seed(4)
  x <- sin(seq(0, 8 * pi, length.out = 50)) + rnorm(50, sd = 0.1)
  x[30:33] <- x[30:33] + 1.5
  m <- 5
  r <- morie_matrxP(x, m)
  zn <- function(s) (s - mean(s)) / sqrt(mean((s - mean(s))^2))
  S <- lapply(1:46, function(i) zn(x[i:(i + 4)]))
  P <- vapply(1:46, function(i) {
    j <- which(abs(1:46 - i) > 2)
    min(vapply(j, function(k) sqrt(sum((S[[i]] - S[[k]])^2)), 0))
  }, 0)
  expect_equal(r$profile, P, tolerance = 1e-9)
  expect_equal(r$discord, which.max(P))
  expect_equal(r$motif_distance, min(P), tolerance = 1e-9)
  expect_error(morie_matrxP(x, 30), "window")
})

test_that("the marine indices follow their formulas", {
  fai <- FloatingAlgaeIndex(0.02, 0.05, 0.01)
  expect_equal(fai, 0.05 - (0.02 + (0.01 - 0.02) * (859 - 645) / (1240 - 645)), tolerance = 1e-12)
  expect_equal(MangroveVegetationIndex(0.1, 0.4, 0.2), 3, tolerance = 1e-12)
  expect_equal(SplitWindowSst(290, 288, c(1, 0.9, 0.1, 0.5), zenith = 30),
               1 + 0.9 * 290 + 0.1 * 2 * 290 + 0.5 * 2 * (1 / cos(pi / 6) - 1), tolerance = 1e-12)
  chl <- c(0.5, 2)
  tot <- ifelse(chl < 1, 38 * chl^0.425, 40.2 * chl^0.507)
  zeu <- 200 * tot^-0.293
  zeu <- ifelse(zeu <= 102, 568.2 * tot^-0.746, zeu)
  t <- 15
  pb <- sum(c(1.2956, 2.749e-1, 6.17e-2, -2.05e-2, 2.462e-3, -1.348e-4, 3.4132e-6, -3.27e-8) * t^(0:7))
  expect_equal(VgpmProduction(chl, 40, t, 12), pb * chl * 12 * 0.66125 * 40 / 44.1 * zeu, tolerance = 1e-12)
  expect_equal(VgpmProduction(1, 40, 30, 12), 4 * 1 * 12 * 0.66125 * 40 / 44.1 * (568.2 * 40.2^-0.746), tolerance = 1e-12)
})
