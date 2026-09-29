# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/timeRS_native.R (Koren 2010, timeSVD++ biases). The
# drift dev_u(t) = sign(t - t_u) |t - t_u|^beta, the item time bins,
# the prediction of eq. (8) and the SGD updates are recomputed from
# the paper's formulas.

.tr_R <- rbind(c(0, 0, 10, 4), c(0, 1, 100, 3), c(1, 0, 40, 5),
               c(1, 1, 200, 2), c(2, 1, 75, 4), c(0, 0, 150, 5))

.tr_sgd <- function(R, nu, ni, bin_days, n_bins, epochs, lr, reg, beta) {
  mu <- mean(R[, 4])
  tu <- tapply(R[, 3], R[, 1], mean)
  bu <- al <- numeric(nu)
  bi <- numeric(ni)
  bins <- matrix(0, ni, n_bins)
  hist <- numeric(epochs)
  for (ep in seq_len(epochs)) {
    se <- 0
    for (k in seq_len(nrow(R))) {
      u <- R[k, 1] + 1
      i <- R[k, 2] + 1
      d <- R[k, 3] - tu[[as.character(R[k, 1])]]
      dev <- sign(d) * abs(d)^beta
      b <- min(floor(R[k, 3] / bin_days), n_bins - 1) + 1
      e <- R[k, 4] - (mu + bu[u] + al[u] * dev + bi[i] + bins[i, b])
      se <- se + e^2
      bu[u] <- bu[u] + lr * (e - reg * bu[u])
      al[u] <- al[u] + lr * (e * dev - reg * al[u])
      bi[i] <- bi[i] + lr * (e - reg * bi[i])
      bins[i, b] <- bins[i, b] + lr * (e - reg * bins[i, b])
    }
    hist[ep] <- sqrt(se / nrow(R))
  }
  list(mu = mu, bu = bu, al = al, bi = bi, bins = bins, hist = hist, tu = tu)
}

test_that("deviation is signed and concave; time_bin clamps to the last bin", {
  expect_equal(morie_timeRS_deviation(110, 10), 100^0.4, tolerance = 1e-12)
  expect_equal(morie_timeRS_deviation(10, 110), -(100^0.4), tolerance = 1e-12)
  expect_identical(morie_timeRS_deviation(5, 5), 0)
  expect_equal(morie_timeRS_deviation(12, 3, beta = 0.5), 3, tolerance = 1e-12)
  expect_error(morie_timeRS_deviation(1, 0, beta = 0), "beta must be positive")
  expect_identical(morie_timeRS_time_bin(139), 1L)
  expect_identical(morie_timeRS_time_bin(140), 2L)
  expect_identical(morie_timeRS_time_bin(-5), 0L)
  expect_identical(morie_timeRS_time_bin(1e6, 70, 30), 29L)
  expect_error(morie_timeRS_time_bin(1, bin_days = 0), "bin width")
})

test_that("user and item biases add the drift, per-day and bin terms", {
  u <- morie_timeRS_user_bias(0.2, 0.05, 30, 5, per_day = list("30" = -0.1))
  expect_equal(u$bias, 0.2 + 0.05 * 25^0.4 - 0.1, tolerance = 1e-12)
  expect_equal(u$per_day, -0.1)
  expect_equal(morie_timeRS_user_bias(0.2, 0.05, 31, 5, per_day = list("30" = -0.1))$per_day, 0)
  i1 <- morie_timeRS_item_bias(0.3, c(0.1, -0.2, 0.4), 150)
  expect_equal(i1$bias, 0.3 + 0.4, tolerance = 1e-12)
  expect_identical(i1$bin, 2L)
  # a bin beyond the stored ones contributes nothing
  expect_equal(morie_timeRS_item_bias(0.3, c(0.1, -0.2), 150)$bias, 0.3)
  p <- morie_timeRS_predict_time(3.5, 0.2, 0.05, 5, 0.3, c(0.1, -0.2, 0.4), 30,
                                 p_u = c(1, 2), q_i = c(0.5, -0.25))
  expect_equal(p$prediction, 3.5 + 0.2 + 0.05 * 25^0.4 + 0.3 + 0.1 + 0, tolerance = 1e-12)
  expect_identical(p$bin, 0L)
  p2 <- morie_timeRS_predict_time(3.5, 0, 0, 30, 0, c(0, 0), 30, p_u = c(1, 3), q_i = c(2, 1))
  expect_equal(p2$prediction, 3.5 + 5)
  expect_error(morie_timeRS_predict_time(3.5, 0, 0, 30, 0, 0, 30, p_u = 1:2, q_i = 1), "differ in width")
})

test_that("fit_time_bias and its aliases run the eq. (8) SGD", {
  ex <- .tr_sgd(.tr_R, 3, 2, 70, 4, 5, 0.05, 0.02, 0.4)
  for (fn in list(morie_timeRS, morie_timeRS_fit_time_bias, morie_timeRS_timesvd, morie_timeRS_timesvdpp)) {
    f <- fn(.tr_R, 3, 2, bin_days = 70, n_bins = 4, epochs = 5, lr = 0.05)
    expect_equal(f$mu, ex$mu, tolerance = 1e-12)
    expect_equal(f$rmse_history, ex$hist, tolerance = 1e-12)
    expect_equal(f$b_user, ex$bu, tolerance = 1e-12)
    expect_equal(f$alpha_user, ex$al, tolerance = 1e-12)
    expect_equal(f$b_item, ex$bi, tolerance = 1e-12)
    expect_equal(f$item_bins, ex$bins, tolerance = 1e-12)
    expect_equal(unname(f$t_user), as.numeric(ex$tu), tolerance = 1e-12)
    expect_equal(f$estimate, ex$hist[5])
  }
  # lr = 0 leaves every bias at zero: RMSE of the global mean
  f0 <- morie_timeRS(.tr_R, 3, 2, epochs = 2, lr = 0)
  expect_equal(f0$rmse, sqrt(mean((.tr_R[, 4] - mean(.tr_R[, 4]))^2)), tolerance = 1e-12)
  # list input is accepted in the same (u, i, t, r) layout
  fl <- morie_timeRS(lapply(seq_len(nrow(.tr_R)), function(k) .tr_R[k, ]), 3, 2,
                     n_bins = 4, epochs = 5, lr = 0.05)
  expect_equal(fl$rmse_history, ex$hist, tolerance = 1e-12)
  expect_error(morie_timeRS(list(), 1, 1), "no ratings")
})

test_that("morie_timeRS_cheatsheet states the drift formula", {
  s <- morie_timeRS_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "sign(t - t_u)|t - t_u|^0.4", fixed = TRUE)
})
