# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: PatchStructure / ClassStructure / LandscapeInformation vs landscapemetrics.

lsx_land <- function(seed, nr = 14, nc = 17, k = 3) {
  U <- .morie_random_uniform(nr * nc, seed = seed, stream = 0)
  matrix(1 + floor(k * U), nr, nc, byrow = TRUE)
}

lsx_rast <- function(m, res = 1) {
  r <- terra::rast(m)
  terra::ext(r) <- c(0, ncol(m) * res, 0, nrow(m) * res)
  terra::crs(r) <- "EPSG:3857"
  r
}

test_that("PatchStructure equals landscapemetrics frac, gyrate, circle, contig, core and cai", {
  skip_if_not_installed("landscapemetrics")
  skip_if_not_installed("terra")
  for (seed in c(3, 11)) for (res in c(1, 30)) for (dirs in c(4, 8)) {
    m <- lsx_land(seed)
    r <- lsx_rast(m, res)
    P <- PatchStructure(m, res = res, directions = dirs, circle_method = "landscapemetrics")
    for (met in c("frac", "gyrate", "circle", "contig", "core", "cai")) {
      ref <- get(paste0("lsm_p_", met), asNamespace("landscapemetrics"))(r, directions = dirs)
      expect_equal(P$patch_class, ref$class)
      expect_equal(P[[met]], ref$value, tolerance = 1e-9, info = paste(met, seed, res, dirs))
    }
    Pd <- PatchStructure(m, res = res, directions = dirs, edge_depth = 2, consider_boundary = TRUE)
    ref <- landscapemetrics::lsm_p_core(r, directions = dirs, edge_depth = 2, consider_boundary = TRUE)
    expect_equal(Pd$core, ref$value, tolerance = 1e-9)
  }
})

test_that("ClassStructure means, tca, cpland and pafrac equal landscapemetrics", {
  skip_if_not_installed("landscapemetrics")
  skip_if_not_installed("terra")
  m <- lsx_land(5, 30, 30, 2)
  r <- lsx_rast(m, 10)
  C <- ClassStructure(m, res = 10, directions = 4, circle_method = "landscapemetrics")
  expect_true(all(is.finite(C$pafrac)))
  for (met in c("frac_mn", "gyrate_mn", "circle_mn", "contig_mn", "core_mn", "cai_mn", "tca", "cpland", "pafrac")) {
    ref <- get(paste0("lsm_c_", met), asNamespace("landscapemetrics"))(r, directions = 4)
    expect_equal(C[[met]], ref$value, tolerance = 1e-9, info = met)
  }
  P <- landscapemetrics::lsm_p_frac(r, directions = 4)
  A <- landscapemetrics::lsm_p_area(r, directions = 4)
  am <- vapply(C$class, function(k) weighted.mean(P$value[P$class == k], A$value[A$class == k]), 0)
  expect_equal(C$frac_am, am, tolerance = 1e-12)
})

test_that("LandscapeInformation equals landscapemetrics ent, joinent, condent, mutinf, relmutinf", {
  skip_if_not_installed("landscapemetrics")
  skip_if_not_installed("terra")
  for (seed in c(2, 9)) {
    m <- lsx_land(seed, 20, 25, 4)
    m[3, 4] <- NA
    r <- lsx_rast(m)
    I <- LandscapeInformation(m)
    for (met in c("ent", "joinent", "condent", "mutinf", "relmutinf")) {
      ref <- get(paste0("lsm_l_", met), asNamespace("landscapemetrics"))(r)
      expect_equal(I[[met]], ref$value, tolerance = 1e-12, info = met)
    }
  }
})

test_that("exact circle is never smaller than the landscapemetrics circle", {
  m <- lsx_land(3)
  ex <- PatchStructure(m, directions = 8)
  lm <- PatchStructure(m, directions = 8, circle_method = "landscapemetrics")
  expect_true(all(ex$circle >= lm$circle - 1e-12))
  expect_true(any(ex$circle > lm$circle + 1e-6))
})
