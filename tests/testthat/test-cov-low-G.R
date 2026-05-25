# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch G: rgeeg, spqkv, sptau, ksr10, ukrig, vrgm, nstat, rgstf, mrm_kulldorff, mrm_tps.

test_that("rgeeg returns named bands with default args", {
  set.seed(1)
  fs <- 128
  t <- seq(0, 4, length.out = 512)
  x <- sin(2 * pi * 10 * t) + 0.2 * rnorm(length(t))
  r <- rgeeg(x, fs = fs)
  expect_named(r, c("absolute", "relative", "total_power", "freqs", "psd"))
  expect_true(r$total_power > 0)
})

test_that("rgeeg honours custom bands and nperseg", {
  set.seed(1)
  x <- rnorm(1024)
  r <- rgeeg(
    x,
    fs = 100,
    bands = list(low = c(1, 5), high = c(20, 40)),
    nperseg = 64
  )
  expect_named(r$absolute, c("low", "high"))
})

test_that("sparse_attention works with integer length input", {
  set.seed(1)
  r <- morie:::sparse_attention(16L, window = 2L, stride = 4L)
  expect_equal(dim(r$tensor), c(16, 16))
  expect_equal(r$method, "sparse-attention")
})

test_that("sptau canonical chain returns I ~ 0.5", {
  set.seed(1)
  x <- c(1, 2, 3, 4, 5)
  n <- 5
  W <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    W[i, i + 1] <- 1
    W[i + 1, i] <- 1
  }
  r <- sptau(x, W)
  expect_equal(r$statistic, 0.5, tolerance = 1e-8)
  expect_equal(r$n, 5L)
})

test_that("sptau errors on bad-shape weight matrix", {
  expect_error(sptau(1:5, matrix(0, 4, 4)), "n-by-n")
})

test_that("ksr10 estimates near median for symmetric data", {
  set.seed(1)
  x <- rnorm(200)
  r <- morie_ksr10_kosorok_m_estimator(x)
  expect_equal(r$n, 200L)
  expect_true(abs(r$estimate) < 0.3)
  expect_true(r$se > 0)
})

test_that("ukrig canonical line example ~ 3.5", {
  set.seed(1)
  r <- ukrig(c(1, 2, 3, 4, 5), matrix(0:4, ncol = 1),
    matrix(2.5, 1, 1),
    trend_order = 1
  )
  expect_equal(r$estimate, 3.5, tolerance = 0.5)
  expect_equal(r$n, 5L)
})

test_that("ukrig works with gaussian and spherical models", {
  set.seed(1)
  coords <- matrix(runif(20), 10, 2)
  tgt <- matrix(c(0.5, 0.5), 1, 2)
  rg <- ukrig(rnorm(10), coords, tgt, model = "gaussian")
  rs <- ukrig(rnorm(10), coords, tgt, model = "spherical")
  expect_true(is.finite(rg$estimate))
  expect_true(is.finite(rs$estimate))
})

test_that("vrgm canonical line example returns valid bins", {
  set.seed(1)
  r <- vrgm(c(1, 2, 3, 4, 5), matrix(0:4, ncol = 1),
    n_bins = 4, max_dist = 4
  )
  expect_named(r$estimate, c("bins", "gamma", "n_pairs"))
  expect_length(r$estimate$bins, 4)
  expect_equal(r$n, 5L)
})

test_that("vrgm errors on shape mismatch and n<2", {
  expect_error(vrgm(rnorm(5), matrix(0, 4, 2)), "coords rows")
  expect_error(vrgm(1, matrix(0, 1, 1)), "at least 2")
})

test_that("nstat returns valid covariance matrix on default bw", {
  set.seed(1)
  coords <- matrix(runif(40), 20, 2)
  r <- nstat(rnorm(20), coords)
  expect_equal(dim(r$estimate$C_matrix), c(20, 20))
  expect_equal(r$n, 20L)
})

test_that("nstat coerces bandwidth<=0 to 1", {
  set.seed(1)
  coords <- matrix(runif(20), 10, 2)
  r <- nstat(rnorm(10), coords, bandwidth = -1)
  expect_equal(r$estimate$bandwidth, 1)
})

test_that("rgstf returns expected spectrogram shape (hann default)", {
  set.seed(1)
  t <- seq(0, 4, length.out = 512)
  x <- sin(2 * pi * 10 * t)
  r <- rgstf(x, fs = 100, nperseg = 64)
  expect_equal(nrow(r$Sxx), 64 %/% 2 + 1)
  expect_equal(r$nperseg, 64L)
  expect_equal(r$fs, 100)
})

test_that("rgstf supports hamming and boxcar windows", {
  set.seed(1)
  x <- rnorm(256)
  rh <- rgstf(x, fs = 50, nperseg = 32, window = "hamming")
  rb <- rgstf(x, fs = 50, nperseg = 32, window = "boxcar")
  expect_equal(dim(rh$Sxx), dim(rb$Sxx))
})

test_that("mrm_tps_kulldorff_scan returns empty df when n<100", {
  set.seed(1)
  df <- data.frame(
    OCC_DATE = format(as.Date("2020-01-01") + 0:49, "%m/%d/%Y 12:00:00 AM"),
    LAT_WGS84 = 43.6 + rnorm(50, sd = 0.01),
    LONG_WGS84 = -79.4 + rnorm(50, sd = 0.01)
  )
  out <- mrm_tps_kulldorff_scan(df, n_permutations = 5L, n_centers = 5L)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
})

test_that(".poisson_lrt returns 0 on degenerate inputs", {
  expect_equal(morie:::.poisson_lrt(0, 0, 1, 10), 0)
  expect_equal(morie:::.poisson_lrt(5, 5, 5, 5), 0)
})

test_that(".haversine_km_mat returns 0 for identical points", {
  expect_equal(morie:::.haversine_km_mat(43.6, -79.4, 43.6, -79.4), 0)
  expect_true(morie:::.haversine_km_mat(43.6, -79.4, 43.7, -79.4) > 0)
})

test_that("mrm_tps_levy_scaling returns NA on tiny data", {
  one <- data.frame(OCC_DATE = "2020-01-01", LAT_WGS84 = 43.6, LONG_WGS84 = -79.4)
  r1 <- mrm_tps_levy_scaling(one)
  expect_equal(r1$n_events, 1L)
  expect_true(is.na(r1$hill_alpha))
})

test_that("mrm_tps_moran_clustering returns NA stub for n<10", {
  set.seed(1)
  df <- data.frame(
    LAT_WGS84 = 43.6 + rnorm(5, sd = 0.01),
    LONG_WGS84 = -79.4 + rnorm(5, sd = 0.01)
  )
  r <- mrm_tps_moran_clustering(df)
  expect_true(is.na(r$morans_I))
  expect_equal(r$dbscan_n_clusters, 0L)
})
