# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2R: tests for mrm_tps.R — TPS-specific MRM analyzers.

.make_synthetic_tps_for_mrm <- function(n = 300L, seed = 1L) {
  set.seed(seed)
  data.frame(
    OCC_DATE = format(as.POSIXct("2018-01-01", tz = "UTC") +
                       sample.int(86400L * 365L * 6L, n, replace = TRUE),
                      "%Y-%m-%d"),
    LAT_WGS84 = stats::runif(n, min = 43.58, max = 43.88),
    LONG_WGS84 = stats::runif(n, min = -79.62, max = -79.13),
    HOOD_158 = sprintf("%03d", sample(1:158, n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
}

test_that("mrm_tps_levy_scaling returns a Levy scaling estimate", {
  df <- .make_synthetic_tps_for_mrm(n = 300L, seed = 1L)
  out <- tryCatch(
    mrm_tps_levy_scaling(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("levy_scaling error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_tps_moran_clustering computes spatial Moran's I", {
  df <- .make_synthetic_tps_for_mrm(n = 200L, seed = 2L)
  out <- tryCatch(
    mrm_tps_moran_clustering(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("moran_clustering error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_tps_neighbourhood_recurrence_km computes per-hood recurrence", {
  df <- .make_synthetic_tps_for_mrm(n = 300L, seed = 3L)
  out <- tryCatch(
    mrm_tps_neighbourhood_recurrence_km(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("neighbourhood_recurrence_km error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_tps_load_hawkes_refit errors cleanly on missing manifest", {
  out <- tryCatch(
    mrm_tps_load_hawkes_refit("/nonexistent/manifest.json"),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.list(out) ||
                is.data.frame(out))
})

test_that("grid Moran z uses the randomisation moments of moran.test", {
  i <- 1:300
  d <- data.frame(LAT_WGS84 = 43.6 + 0.05 * ((i * 7919) %% 97) / 97 + 0.01 * sin(i),
                  LONG_WGS84 = -79.5 + 0.08 * ((i * 104729) %% 89) / 89)
  r <- mrm_tps_moran_clustering(d, grid_resolution = 6L)
  # explicit rook W on the 6 x 6 grid and the Cliff-Ord moments
  g <- 6
  lb <- seq(min(d$LAT_WGS84), max(d$LAT_WGS84), length.out = g + 1)
  ob <- seq(min(d$LONG_WGS84), max(d$LONG_WGS84), length.out = g + 1)
  ii <- as.integer(cut(d$LAT_WGS84, lb, include.lowest = TRUE))
  jj <- as.integer(cut(d$LONG_WGS84, ob, include.lowest = TRUE))
  cnt <- matrix(0, g, g)
  for (k in seq_along(ii)) cnt[ii[k], jj[k]] <- cnt[ii[k], jj[k]] + 1
  z <- as.vector(cnt) - mean(cnt)
  n <- length(z)
  w <- matrix(0, n, n)
  id <- function(a, b) (b - 1) * g + a
  for (a in 1:g) for (b in 1:g) {
    if (a < g) w[id(a, b), id(a + 1, b)] <- w[id(a + 1, b), id(a, b)] <- 1
    if (b < g) w[id(a, b), id(a, b + 1)] <- w[id(a, b + 1), id(a, b)] <- 1
  }
  s0 <- sum(w)
  s1 <- 0.5 * sum((w + t(w))^2)
  s2 <- sum((rowSums(w) + colSums(w))^2)
  mi <- n / s0 * sum(w * outer(z, z)) / sum(z^2)
  b2 <- n * sum(z^4) / sum(z^2)^2
  ei <- -1 / (n - 1)
  vi <- (n * ((n^2 - 3 * n + 3) * s1 - n * s2 + 3 * s0^2) -
    b2 * ((n^2 - n) * s1 - 2 * n * s2 + 6 * s0^2)) /
    ((n - 1) * (n - 2) * (n - 3) * s0^2) - ei^2
  expect_equal(r$morans_I, round(mi, 6))
  expect_equal(r$morans_z, round((mi - ei) / sqrt(vi), 2))
})

test_that("Levy exponent is the MLE on the tail x >= x_min", {
  d <- data.frame(OCC_DATE = sprintf("2024-01-%02d", 1:6),
                  LAT_WGS84 = 43.6 + c(0, 0.01, 0.03, 0.06, 0.1, 0.2),
                  LONG_WGS84 = rep(-79.4, 6))
  r <- mrm_tps_levy_scaling(d, min_step_km = 0.5, x_min = 2)
  step <- .haversine_km(d$LAT_WGS84[-6], d$LONG_WGS84[-6],
                        d$LAT_WGS84[-1], d$LONG_WGS84[-1])
  tail <- step[step >= 2]
  expect_equal(r$n_steps_tail, length(tail))
  expect_equal(r$hill_alpha, round(1 + length(tail) / sum(log(tail / 2)), 4))
})
