# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: RemoteSensing.R vs RStoolbox, MASS::qda and stats::kmeans.

rs_scene <- function(seed, nr = 12, nc = 10, k = 7, scale = 1) {
  U <- .morie_random_uniform(nr * nc * (k + 3), seed = seed, stream = 0)
  base <- matrix(U[seq_len(nr * nc)], nr, nc)
  lapply(seq_len(k), function(b) scale * (0.2 + 0.5 * base + 0.3 * matrix(U[b * nr * nc + seq_len(nr * nc)], nr, nc)) * (0.6 + b / 10))
}
rs_rast <- function(G, names = paste0("b", seq_along(G))) {
  r <- terra::rast(lapply(G, terra::rast))
  terra::ext(r) <- c(0, ncol(G[[1]]) * 30, 0, nrow(G[[1]]) * 30)
  terra::crs(r) <- "EPSG:32617"
  if (!is.null(names)) names(r) <- names
  r
}
rs_mat <- function(r, i) {
  if (is.list(r) && !inherits(r, "SpatRaster")) {
    r <- r[[i]]
    i <- 1
  }
  rs_mat1(r, i)
}
rs_mat1 <- function(r, i) matrix(terra::values(r[[i]]), terra::nrow(r), terra::ncol(r), byrow = TRUE)

test_that("TasseledCap, BandPca, SpectralAngleClassify and EstimateHaze equal RStoolbox", {
  skip_if_not_installed("RStoolbox")
  G <- rs_scene(3, k = 6)
  r <- rs_rast(G)
  tc <- RStoolbox::tasseledCap(r, "landsat8oli")
  m <- TasseledCap(G, "landsat8oli")
  for (i in 1:3) expect_equal(m[[i]], rs_mat(tc, i), tolerance = 1e-12)
  for (spca in c(FALSE, TRUE)) {
    p <- RStoolbox::rasterPCA(r, spca = spca)
    q <- BandPca(G, spca = spca)
    for (i in 1:6) expect_equal(abs(q$scores[[i]]), abs(rs_mat(p$map, i)), tolerance = 1e-8, info = paste(spca, i))
  }
  em <- rbind(sapply(G, function(g) g[1, 1]), sapply(G, function(g) g[5, 5]), sapply(G, function(g) g[12, 10]))
  colnames(em) <- names(r)  # sam() drops unnamed columns via colnames(em) != "ID"
  s <- RStoolbox::sam(r, em)
  expect_equal(SpectralAngleClassify(G, em)$classes, rs_mat(s, 1))
  dn <- round(G[[1]] * 90)
  for (ms in c(FALSE, TRUE)) {
    expect_equal(EstimateHaze(dn, 0.05, ms), unname(RStoolbox::estimateHaze(rs_rast(list(dn)), hazeBands = 1,
                                                                             darkProp = 0.05, maxSlope = ms)))
  }
})

test_that("TopographicCorrection equals RStoolbox::topCor for every method", {
  skip_if_not_installed("RStoolbox")
  G <- rs_scene(5, k = 3)
  U <- .morie_random_uniform(240, seed = 9, stream = 1)
  slope <- matrix(0.6 * U[1:120], 12, 10)
  aspect <- matrix(2 * pi * U[121:240], 12, 10)
  r <- rs_rast(G)
  dem <- rs_rast(list(slope, aspect), c("slope", "aspect"))
  for (meth in c("cos", "avgcos", "minnaert", "C", "stat")) {
    ref <- RStoolbox::topCor(r, dem = dem, solarAngles = c(2.1, 0.7), method = meth)
    m <- TopographicCorrection(G, slope, aspect, sun_azimuth = 2.1, sun_zenith = 0.7, method = meth)
    for (i in 1:3) expect_equal(m$bands[[i]], rs_mat(ref, i), tolerance = 1e-9, info = meth)
  }
})

test_that("RadiometricCorrection equals RStoolbox::radCor (apref, sdos, dos, costz)", {
  skip_if_not_installed("RStoolbox")
  db <- asNamespace("RStoolbox")$.LANDSATdb$LANDSAT8$OLI_TIRS
  bn <- paste0("B", 1:7, "_dn")
  G <- lapply(rs_scene(7, k = 7, scale = 12000), round)
  r <- rs_rast(G, bn)
  gain <- c(0.0126, 0.0129, 0.0119, 0.0100, 0.0061, 0.0015, 0.0005)
  off <- -5 * gain
  meta <- RStoolbox::ImageMetaData(sat = "LANDSAT8", sen = "OLI_TIRS", az = 150, selv = 42, esd = 1.01, bands = bn,
                                   calrad = data.frame(gain = gain, offset = off, row.names = bn), radRes = 16)
  esun <- db[bn, "esun"]
  wl <- db[bn, "centerWavl"]
  ref <- RStoolbox::radCor(r, meta, method = "apref")
  m <- RadiometricCorrection(G, gain, off, method = "apref", esun = esun, sun_elevation = 42, distance = 1.01)
  for (i in 1:7) expect_equal(m$bands[[i]], rs_mat(ref, i), tolerance = 1e-10)
  ref <- suppressWarnings(RStoolbox::radCor(r, meta, method = "sdos", hazeValues = c(B1_dn = 3000, B2_dn = 2600),
                                            hazeBands = c("B1_dn", "B2_dn")))
  m <- RadiometricCorrection(G, gain, off, method = "sdos", esun = esun, sun_elevation = 42, distance = 1.01,
                             haze_dn = c(3000, 2600, 0, 0, 0, 0, 0))
  for (i in 1:7) expect_equal(m$bands[[i]], rs_mat(ref, i), tolerance = 1e-10)
  for (meth in c("dos", "costz")) for (atm in c("clear", "hazy")) {
    ref <- suppressWarnings(RStoolbox::radCor(r, meta, method = meth, hazeValues = c(B1_dn = 3000),
                                              hazeBands = "B1_dn", atmosphere = atm))
    m <- RadiometricCorrection(G, gain, off, method = meth, esun = esun, sun_elevation = 42, distance = 1.01,
                               haze_dn = 3000, haze_band = 1, wavelengths = wl, atmosphere = atm)
    for (i in 1:7) expect_equal(m$bands[[i]], rs_mat(ref, i), tolerance = 1e-10, info = paste(meth, atm))
  }
})

test_that("CloudMask, PanSharpen (Brovey), GaussianMlClassify and KmeansClassify equal their references", {
  skip_if_not_installed("RStoolbox")
  G <- rs_scene(11, k = 2)
  x <- rs_rast(G, c("B1_sre", "B6_sre"))
  cm <- RStoolbox::cloudMask(x, threshold = 0.1, blue = "B1_sre", tir = "B6_sre")
  m <- CloudMask(G[[1]], G[[2]], threshold = 0.1)
  expect_equal(m$ndtci, rs_mat(cm, "NDTCI"), tolerance = 1e-12)
  expect_equal(m$mask, !is.na(rs_mat(cm, "CMASK")))
  lo <- rs_scene(13, nr = 5, nc = 4, k = 3)
  pan <- matrix(.morie_random_uniform(80, seed = 2, stream = 0) + 0.5, 10, 8)
  rl <- rs_rast(lo, c("r", "g", "b"))
  rp <- terra::rast(pan)
  terra::ext(rp) <- terra::ext(rl)
  terra::crs(rp) <- terra::crs(rl)
  ref <- RStoolbox::panSharpen(rl, rp, r = 1, g = 2, b = 3, method = "brovey")
  up <- lapply(lo, function(g) kronecker(g, matrix(1, 2, 2)))
  m <- PanSharpen(up, pan, "brovey")
  # terra::resample returns float32 values, so agreement is at float32 precision
  for (i in 1:3) expect_equal(m$bands[[i]], rs_mat(ref, i), tolerance = 1e-6)
  skip_if_not_installed("MASS")
  tr <- rbind(cbind(.morie_random_normal(30, 1, 0), .morie_random_normal(30, 1, 1)),
              cbind(2 + .morie_random_normal(30, 1, 2), 1 + 0.5 * .morie_random_normal(30, 1, 3)))
  lab <- rep(c("a", "b"), each = 30)
  B <- list(matrix(seq(-1, 3, length.out = 20), 4), matrix(seq(2, -1, length.out = 20), 4))
  q <- MASS::qda(tr, lab)
  pq <- predict(q, cbind(as.vector(t(B[[1]])), as.vector(t(B[[2]]))))
  g <- GaussianMlClassify(B, tr, lab)
  expect_equal(as.vector(t(g$classes)), as.character(pq$class))
  expect_equal(unname(g$posterior), unname(pq$posterior), tolerance = 1e-10)
  P <- cbind(as.vector(t(G[[1]])), as.vector(t(G[[2]])))
  C0 <- P[c(1, 50, 100), ]
  km <- stats::kmeans(P, centers = C0, algorithm = "Lloyd", iter.max = 100)
  k <- KmeansClassify(G, C0)
  expect_equal(as.vector(t(k$classes)), unname(km$cluster))
  expect_equal(k$centers, unname(km$centers), tolerance = 1e-12)
})
