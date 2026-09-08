# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the bootstrap/jackknife and volatility
# shelves. The deterministic pieces (jackknife, Parkinson,
# Garman-Klass, the aggregates, the noise decomposition) are anchored
# to full-precision Python values on a shared LCG fixture; the
# resamplers use the language's own RNG and are tested against their
# closed-form oracles.

btv_fixture <- function(n, s = 444) {
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[i] <- (s + 0.5) / 4294967296
  }
  stats::qnorm(u)
}

test_that("the fixture matches the one Python anchored against", {
  expect_equal(btv_fixture(3), c(-0.23232898534945803, 0.1282859067496912,
                                 0.9541916468574952), tolerance = 1e-12)
})

test_that("the jackknife matches Python and its closed forms", {
  z <- btv_fixture(600)
  j <- morie_bt_jackknife(z[1:80], mean)
  expect_equal(j$variance, 0.012647738599577907, tolerance = 1e-12)
  expect_equal(j$estimate, -0.042419150758233645, tolerance = 1e-12)
  # exact identity: jackknife variance of the mean IS s^2/n
  expect_equal(j$variance, stats::var(z[1:80]) / 80, tolerance = 1e-12)
  # pseudovalues of the mean ARE the observations
  expect_equal(j$pseudovalues, z[1:80], tolerance = 1e-9)
  jv <- morie_bt_jackknife(z[1:120], function(d) mean((d - mean(d))^2))
  expect_equal(jv$bias, -0.007358868008955133, tolerance = 1e-10)
  expect_equal(jv$corrected, 0.8830641610768679, tolerance = 1e-10)
  # the jackknife removes the MLE variance's -s^2/n bias EXACTLY
  expect_equal(jv$corrected, stats::var(z[1:120]), tolerance = 1e-9)
  expect_error(morie_bt_jackknife(1:2, mean), "at least 3")
})

test_that("the IID bootstrap hits the closed-form SE of the mean", {
  z <- btv_fixture(200, 7)
  o <- morie_bt_iid(2 * z + 3, mean, B = 800, seed = 1)
  expect_equal(o$se, 2 / sqrt(200), tolerance = 0.15)
  expect_equal(o$estimate, mean(2 * z + 3), tolerance = 1e-12)
  expect_true(o$ci_percentile[1] < 3 & 3 < o$ci_percentile[2])
  v <- morie_bt_var(o$replicates)
  expect_equal(v$se, o$se, tolerance = 1e-12)
  expect_error(morie_bt_var(1), "at least 2")
  expect_error(morie_bt_var(c(1, NA)), "finite")
})

test_that("the bootstrap bias correction points the right way", {
  z <- 2 * btv_fixture(200, 11)
  mlvar <- function(d) mean((d - mean(d))^2)
  b <- morie_bt_iid(z, mlvar, B = 4000, seed = 2)
  o <- morie_bt_bias(b$estimate, b$replicates)
  expect_lt(o$bias, 0)
  expect_gt(o$corrected, o$estimate)         # moves UP
  expect_equal(o$corrected, 2 * o$estimate - o$mean_replicate,
               tolerance = 1e-12)
  mc_sd <- sqrt(2 * stats::var(z)^2 / 200 / 4000)
  expect_equal(o$bias, -stats::var(z) / 200,
               tolerance = (3 * mc_sd + 2 / 200^1.5) / abs(stats::var(z) / 200))
})

test_that("the .632 alias reproduces the book's numbers", {
  o <- morie_bt_632(0, 0.5, gamma = 0.5)
  expect_equal(o$err_632, 0.316, tolerance = 1e-12)
  expect_equal(o$err_632_plus, 0.5, tolerance = 1e-12)
  expect_equal(o$alias_of, "morie_esl_oob_632")
})

test_that("the out-of-bag error is honest with the 0.368 fraction", {
  z <- btv_fixture(400, 13)
  X <- matrix(z[1:240], ncol = 2)
  y <- X[, 1] - X[, 2] + 0.5 * z[241:360]
  fit <- function(Xa, ya) qr.coef(qr(cbind(1, Xa)), ya)
  prd <- function(b, Xn) as.numeric(cbind(1, Xn) %*% b)
  o <- morie_bt_oob(X, y, fit, prd, B = 100, seed = 3)
  expect_gt(o$err_oob, o$err_apparent)
  expect_equal(o$oob_fraction, 1 - 0.632, tolerance = 0.04)
  expect_equal(o$n_dropped, 0L)
})

test_that("the ratio CI covers and pairing matters", {
  z <- btv_fixture(1200, 17)
  a <- 2 + 0.3 * z[1:300]
  b <- 1 + 0.2 * z[301:600]
  o <- morie_bt_ci_ratio(a, b, B = 800, seed = 1)
  expect_true(o$ci[1] < 2 & 2 < o$ci[2])
  expect_equal(o$ratio, mean(a) / mean(b), tolerance = 1e-12)
  common <- z[601:1000]
  x2 <- 2 + common + 0.05 * z[801:1200]
  y2 <- 1 + 0.5 * common + 0.05 * z[1:400]
  paired <- morie_bt_ci_ratio(x2, y2, B = 600, paired = TRUE, seed = 3)
  indep <- morie_bt_ci_ratio(x2, y2, B = 600, paired = FALSE, seed = 3)
  expect_lt(paired$se, indep$se)
  expect_error(morie_bt_ci_ratio(x2[1:10], y2, paired = TRUE),
               "equal-length")
  expect_error(morie_bt_ci_ratio(a, b, B = 10), "at least 100")
})

btv_bars <- function() {
  z <- btv_fixture(600)
  steps <- matrix(0.02 / sqrt(10) * z[1:500], nrow = 50, byrow = TRUE)
  O <- H <- L <- C <- numeric(50)
  p <- 0
  for (b in 1:50) {
    path <- p + cumsum(steps[b, ])
    O[b] <- p
    C[b] <- path[10]
    H[b] <- max(p, max(path))
    L[b] <- min(p, min(path))
    p <- path[10]
  }
  list(O = exp(O), H = exp(H), L = exp(L), C = exp(C))
}

test_that("Parkinson and Garman-Klass match Python on the fixture bars", {
  b <- btv_bars()
  pk <- morie_vol_parkinson(b$H, b$L)
  expect_equal(pk$variance, 0.00025642953706023387, tolerance = 1e-12)
  expect_equal(pk$sigma, 0.01601341740729423, tolerance = 1e-12)
  expect_equal(pk$constant, 1 / (4 * log(2)), tolerance = 1e-12)
  gk <- morie_vol_garman_klass(b$O, b$H, b$L, b$C)
  expect_equal(gk$variance, 0.0002286731914975862, tolerance = 1e-12)
  expect_equal(gk$range_term, 0.00035548682125118627, tolerance = 1e-12)
  expect_equal(gk$openclose_term, 0.00012681362975360008, tolerance = 1e-12)
  # the open-close term is genuinely SUBTRACTED
  expect_equal(gk$variance, gk$range_term - gk$openclose_term,
               tolerance = 1e-12)
  expect_error(morie_vol_parkinson(c(1, 1), c(1.1, 0.9)),
               "high must be at least low")
  expect_error(morie_vol_garman_klass(c(2, 1), c(1.5, 1.2), c(0.9, 0.8),
                                      c(1, 1)), "low <= open")
})

test_that("the volatility aggregates match Python and their inequality", {
  z <- btv_fixture(600)
  s <- abs(z[1:20]) + 0.5
  h <- morie_vol_harmonic(s)
  expect_equal(h$harmonic, 0.9421923185702868, tolerance = 1e-12)
  expect_equal(h$geometric, 1.0206985285612034, tolerance = 1e-12)
  expect_equal(h$arithmetic, 1.1147648861426485, tolerance = 1e-12)
  expect_equal(h$rms, 1.2172952201001181, tolerance = 1e-12)
  expect_true(h$inequality_holds)
  expect_true(h$harmonic < h$geometric & h$geometric < h$arithmetic &
                h$arithmetic < h$rms)
  expect_error(morie_vol_harmonic(c(0.1, 0)), "positive")
})

test_that("the noise decomposition matches Python", {
  z <- btv_fixture(600)
  # Python's z[100:601] on a 600-vector truncates to elements
  # 101..600 (1-based): 500 values, giving 500 noise increments
  r <- 0.001 * z[1:500] + diff(c(0, 0.0005 * z[101:600]))
  o <- morie_vol_noise(r, K = 20)
  expect_equal(o$noise_variance, 7.524715805472461e-07, tolerance = 1e-12)
  expect_equal(o$rv_all, 0.0007524715805472461, tolerance = 1e-12)
  expect_equal(o$iv_two_scale, 0.0003743675237242522, tolerance = 1e-10)
  expect_equal(o$rv_subsampled, 0.00041056140674857475, tolerance = 1e-10)
  # the identity noise_variance = rv_all / (2n)
  expect_equal(o$noise_variance, o$rv_all / (2 * 500), tolerance = 1e-15)
  expect_error(morie_vol_noise(r[1:10]), "at least 30")
  expect_error(morie_vol_noise(r, K = 1), "K must lie")
})

test_that("the resamplers do not leak the global RNG stream", {
  z <- btv_fixture(100, 23)
  set.seed(2468)
  before <- stats::runif(3)
  set.seed(2468)
  invisible(morie_bt_iid(z, mean, B = 50, seed = 1))
  invisible(morie_bt_ci_ratio(z + 3, z + 2, B = 100, seed = 1))
  expect_equal(stats::runif(3), before, tolerance = 1e-12)
})
