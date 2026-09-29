# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tsbF_native.R (Teunter, Syntetos & Babai 2011 TSB;
# Croston 1972; Syntetos & Boylan 2005 SBA; Syntetos et al. 2005
# ADI / CV^2 classes). The recursions are re-run in the test from
# their published update equations.

.tb_y <- c(0, 3, 0, 0, 5, 0, 2, 0, 0, 0, 4, 1)

.tb_tsb <- function(y, a, b, z, p) {
  f <- numeric(length(y))
  for (t in seq_along(y)) {
    p <- p + b * ((y[t] > 0) - p)
    if (y[t] > 0) z <- z + a * (y[t] - z)
    f[t] <- p * z
  }
  f
}
.tb_cro <- function(y, a, z, x) {
  f <- numeric(length(y))
  q <- 0
  for (t in seq_along(y)) {
    q <- q + 1
    if (y[t] > 0) {
      z <- z + a * (y[t] - z)
      x <- x + a * (q - x)
      q <- 0
    }
    f[t] <- z / x
  }
  f
}

test_that("TSB updates probability every period and size on demand", {
  pos <- .tb_y[.tb_y > 0]
  f <- morie_tsbF_tsb_forecast(.tb_y, 0.2, 0.1, horizon = 3)
  ex <- .tb_tsb(.tb_y, 0.2, 0.1, mean(pos), length(pos) / 12)
  expect_equal(f$fitted, ex, tolerance = 1e-12)
  expect_equal(f$forecast, rep(ex[12], 3), tolerance = 1e-12)
  expect_equal(f$p_init, 5 / 12)
  expect_equal(f$z_final * f$p_final, ex[12], tolerance = 1e-12)
  k <- morie_tsbF_tsb_forecast(.tb_y, 0.2, 0.1, init = "known", z0 = 2, p0 = 0.25, burn_in = 4)
  exk <- .tb_tsb(.tb_y, 0.2, 0.1, 2, 0.25)
  expect_equal(k$fitted_full, exk, tolerance = 1e-12)
  expect_equal(k$fitted, exk[5:12], tolerance = 1e-12)
  h <- morie_tsbF_tsb_forecast(.tb_y, 0.2, 0.1, init = "heuristic")
  expect_equal(h$fitted, .tb_tsb(.tb_y, 0.2, 0.1, 3, 5 / 12), tolerance = 1e-12)
  expect_error(morie_tsbF_tsb_forecast(1, 0.1, 0.1), "at least 2")
  expect_error(morie_tsbF_tsb_forecast(.tb_y, 0, 0.1), "alpha must be")
  expect_error(morie_tsbF_tsb_forecast(.tb_y, 0.1, 1.5), "beta must be")
  expect_error(morie_tsbF_tsb_forecast(rep(0, 5)), "no positive demand")
  expect_error(morie_tsbF_tsb_forecast(.tb_y, init = "zero"), "init must be one of")
  expect_error(morie_tsbF_tsb_forecast(.tb_y, init = "known", z0 = 1), "needs z0")
  expect_error(morie_tsbF_tsb_forecast(.tb_y, init = "known", z0 = -1, p0 = 0.5), "z0 must be positive")
  expect_error(morie_tsbF_tsb_forecast(.tb_y, burn_in = 12), "discards the whole series")
  expect_error(morie_tsbF_tsb_forecast(.tb_y, burn_in = -1), "non-negative")
})

test_that("Croston forecasts z / x; SBA deflates by 1 - alpha / 2", {
  pos <- .tb_y[.tb_y > 0]
  c1 <- morie_tsbF_croston_forecast(.tb_y, 0.3, horizon = 2)
  ex <- .tb_cro(.tb_y, 0.3, mean(pos), 12 / 5)
  expect_equal(c1$fitted, ex, tolerance = 1e-12)
  expect_equal(c1$forecast, rep(ex[12], 2), tolerance = 1e-12)
  # heuristic: first size, mean gap between demands (3, 2, 4, 1)
  ch <- morie_tsbF_croston_forecast(.tb_y, 0.3, init = "heuristic")
  expect_equal(ch$x_init, 2.5)
  expect_equal(ch$fitted, .tb_cro(.tb_y, 0.3, 3, 2.5), tolerance = 1e-12)
  ck <- morie_tsbF_croston_forecast(.tb_y, 0.3, init = "known", z0 = 4, x0 = 3)
  expect_equal(ck$fitted, .tb_cro(.tb_y, 0.3, 4, 3), tolerance = 1e-12)
  expect_error(morie_tsbF_croston_forecast(.tb_y, init = "known", z0 = 4, x0 = 0.5), "x0 must be at least 1")
  expect_error(morie_tsbF_croston_forecast(.tb_y, alpha = 2), "alpha must be")
  s <- morie_tsbF_sba_forecast(.tb_y, 0.3)
  expect_equal(s$fitted, ex * 0.85, tolerance = 1e-12)
  expect_equal(s$deflator, 0.85)
})

test_that("demand classification uses ADI and squared CV of positive demands", {
  cl <- morie_tsbF_demand_classification(.tb_y)
  pos <- .tb_y[.tb_y > 0]
  expect_equal(cl$adi, 12 / 5)
  expect_equal(cl$cv2, (sd(pos) / mean(pos))^2, tolerance = 1e-12)
  expect_identical(cl$class, if (cl$cv2 <= 0.49) "intermittent" else "lumpy")
  expect_identical(morie_tsbF_demand_classification(c(5, 5, 6, 5))$class, "smooth")
  expect_identical(morie_tsbF_demand_classification(c(1, 20, 1, 30))$class, "erratic")
  expect_identical(morie_tsbF_demand_classification(c(0, 1, 0, 0, 30, 0))$class, "lumpy")
  expect_error(morie_tsbF_demand_classification(c(0, 1, 0)), "at least 2 positive")
})

test_that("intermittent_forecast and morie_tsbF dispatch by method", {
  for (fn in list(morie_tsbF_intermittent_forecast, morie_tsbF)) {
    expect_equal(fn(.tb_y, "tsb", 0.2, 0.1)$forecast, morie_tsbF_tsb_forecast(.tb_y, 0.2, 0.1)$forecast)
    expect_equal(fn(.tb_y, "croston", 0.3)$forecast, morie_tsbF_croston_forecast(.tb_y, 0.3)$forecast)
    expect_equal(fn(.tb_y, "sba", 0.3)$forecast, morie_tsbF_sba_forecast(.tb_y, 0.3)$forecast)
    expect_error(fn(.tb_y, "ses"), "method must be one of")
  }
})

test_that("morie_tsbF_cheatsheet states the SBA deflator", {
  expect_match(morie_tsbF_cheatsheet(), "(1 - alpha/2)", fixed = TRUE)
})
