# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/surepi_native.R (EARS, Hutwagner, Thompson, Seeman &
# Treadwell 2003). C1 / C2 are z-scores of the day's count against a
# seven-day moving baseline (lag 1 or 3), with sd floored at
# sigma_floor; C3 accumulates three consecutive C2 terms. The CUSUM
# of eq. (4) and the 4253H smoother of eq. (5) are re-run in the test.

.se_y <- c(3, 5, 4, 6, 2, 4, 5, 3, 4, 6, 5, 4, 15, 18, 5, 4, 4, 4, 4, 4, 4, 4)

.se_z <- function(y, lag, floor = 1) {
  n <- length(y)
  out <- rep(NA_real_, n)
  for (t in seq_len(n)) {
    lo <- t - lag - 6
    if (lo < 1) next
    w <- y[lo:(t - lag)]
    out[t] <- (y[t] - mean(w)) / max(sd(w), floor)
  }
  out
}

test_that("C1 and C2 are z-scores against the lag-1 and lag-3 baselines", {
  c1 <- surepi_c1_mild(.se_y)
  expect_equal(c1$statistic, .se_z(.se_y, 1), tolerance = 1e-12)
  expect_identical(c1$flag, .se_z(.se_y, 1) > 3)
  expect_equal(c1$n_flagged, sum(.se_z(.se_y, 1) > 3, na.rm = TRUE))
  c2 <- surepi_c2_medium(.se_y, threshold = 2)
  expect_equal(c2$statistic, .se_z(.se_y, 3), tolerance = 1e-12)
  expect_equal(c2$n_evaluable, sum(!is.na(.se_z(.se_y, 3))))
  # the near-flat tail has sd below the floor: sigma_floor replaces it
  expect_equal(surepi_ears_detect(.se_y, "C1", sigma_floor = 0.5)$statistic, .se_z(.se_y, 1, 0.5), tolerance = 1e-12)
  expect_equal(surepi_ears_detect(.se_y, "C2", sigma_floor = 2)$statistic, .se_z(.se_y, 3, 2), tolerance = 1e-12)
  for (fn in list(surepi_ears_detect, surepi_earssignal, surepi_surveillance_signal, morie_surepi)) {
    expect_equal(fn(.se_y, "C2")$statistic, c2$statistic)
  }
  expect_error(surepi_ears_detect(.se_y, "C4"), "method must be one of")
  expect_error(surepi_ears_detect(-.se_y), "non-negative")
  expect_error(surepi_ears_detect(.se_y, sigma_floor = 0), "sigma_floor must be positive")
  expect_error(surepi_ears_detect(.se_y[1:9], "C2"), "more than 9 days")
})

test_that("C3 sums the current and two previous C2 terms", {
  z <- .se_z(.se_y, 3)
  ex <- rep(NA_real_, length(z))
  for (t in 3:length(z)) if (all(!is.na(z[t - 0:2]))) ex[t] <- sum(z[t - 0:2])
  c3 <- surepi_c3_ultra(.se_y)
  expect_equal(c3$statistic, ex, tolerance = 1e-12)
  expect_identical(c3$flag, ex > 2)
})

test_that("salmonella_cusum is S_t = max(0, S_{t-1} + (y - mu - k sigma) / sigma)", {
  mu <- rep(4, 22)
  mu[13:22] <- 5
  r <- surepi_salmonella_cusum(.se_y, mu, 1.5, k_shift = 0.5, decision = 2, min_count = 6)
  S <- 0
  ex <- numeric(22)
  for (t in 1:22) {
    S <- max(0, S + (.se_y[t] - (mu[t] + 0.75)) / 1.5)
    ex[t] <- S
  }
  expect_equal(r$cusum, ex, tolerance = 1e-12)
  expect_identical(r$flag, ex >= 2 & .se_y >= 6)
  expect_equal(surepi_salmonella_cusum(.se_y, 4, 2)$cusum[1], 0)
  expect_error(surepi_salmonella_cusum(.se_y, 1:3, 1), "match the series length")
  expect_error(surepi_salmonella_cusum(.se_y, 4, 0), "positive everywhere")
})

test_that("compound_smoothing applies running medians 4, 2, 5, 3 then Hanning", {
  v <- c(4, 7, 3, 9, 5, 6, 12, 4, 5, 8, 6, 7)
  rm_ <- function(x, w) {
    n <- length(x)
    h <- w %/% 2
    out <- x
    for (i in 1:n) {
      lo <- i - h
      hi <- if (w %% 2 == 0) i + h - 1 else i + h
      if (lo >= 1 && hi <= n) out[i] <- median(x[lo:hi])
    }
    out
  }
  s <- Reduce(rm_, c(4, 2, 5, 3), v)
  n <- length(s)
  s <- c(s[1], 0.25 * s[1:(n - 2)] + 0.5 * s[2:(n - 1)] + 0.25 * s[3:n], s[n])
  r <- surepi_compound_smoothing(v, current = 11)
  expect_equal(r$smoothed, s, tolerance = 1e-12)
  expect_equal(r$sigma, sd(v - s), tolerance = 1e-12)
  expect_equal(r$threshold, s[n] + 2 * sd(v - s), tolerance = 1e-12)
  expect_identical(r$flag, 11 > s[n] + 2 * sd(v - s))
  expect_error(surepi_compound_smoothing(1:5, 3), "too short")
})

test_that("surepi_cheatsheet gives the baselines", {
  expect_match(surepi_cheatsheet(), "C2 = t-9..t-3", fixed = TRUE)
})
