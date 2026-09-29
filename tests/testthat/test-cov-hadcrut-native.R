# Coverage for the HadCRUT5 blend (Morice et al. 2021): land/SST blending
# weights with the sea-ice and 25% land-floor rules, the cell blend and
# its variance, cos-latitude area means (hemispheric, global area, 2:1
# land ratio), coverage error, and the combined uncertainty and
# ensemble interval of the headline estimate.

test_that("blending weights follow the land-fraction, sea-ice and floor rules", {
  expect_equal(morie_hadcrut_weights(0.6), c(0.6, 0.4))
  expect_equal(morie_hadcrut_weights(0.1), c(0.25, 0.75))
  expect_equal(morie_hadcrut_weights(0.1, rule = "area"), c(0.1, 0.9))
  expect_equal(morie_hadcrut_weights(0.2, sea_ice = 0.5), c(0.2 + 0.8 * 0.5, 1 - 0.6))
  expect_equal(morie_hadcrut_weights(0.2, sea_ice = 0.1, rule = "area"), c(0.2, 0.8))
  expect_equal(morie_hadcrut_weights(0.5, has_sst = FALSE), c(1, 0))
  expect_equal(morie_hadcrut_weights(0.5, has_land = FALSE), c(0, 1))
  expect_equal(morie_hadcrut_weights(0.5, has_land = FALSE, has_sst = FALSE), c(0, 0))
  expect_equal(morie_hadcrut_weights(0.5, rule = "land_only"), c(1, 0))
  expect_equal(morie_hadcrut_weights(0.5, rule = "sst_only", has_sst = FALSE), c(0, 0))
  expect_error(morie_hadcrut_weights(1.2), "land_fraction must lie")
  expect_error(morie_hadcrut_weights(0.5, sea_ice = -1), "sea_ice must lie")
  expect_error(morie_hadcrut_weights(0.5, rule = "gistemp"), "rule must be one of")
})

.g <- function() {
  T <- rbind(c(0.5, NA, 1), c(0.2, 0.3, NA), c(NA, NA, NA), c(1.1, 0.9, 0.7))
  S <- rbind(c(0.4, 0.6, NA), c(0.1, NA, 0.2), c(0.3, NA, 0.5), c(NA, 0.8, 0.6))
  LF <- rbind(c(0.5, 0, 1), c(0.3, 0.9, 0), c(0, 0, 0), c(1, 0.4, 0.2))
  list(T = T, S = S, LF = LF)
}

test_that("the cell blend weights land and SST anomalies and propagates variance", {
  g <- .g()
  b <- morie_hadcrut_blend(g$T, g$S, g$LF, T_var = matrix(0.04, 4, 3), sst_var = matrix(0.01, 4, 3))
  expect_equal(b$anomaly[1, 1], 0.5 * 0.5 + 0.5 * 0.4, tolerance = 1e-12)
  expect_equal(b$anomaly[1, 2], 0.6)
  expect_equal(b$anomaly[4, 3], 0.25 * 0.7 + 0.75 * 0.6, tolerance = 1e-12)
  expect_true(is.na(b$anomaly[3, 2]))
  expect_equal(b$variance[2, 1], 0.3^2 * 0.04 + 0.7^2 * 0.01, tolerance = 1e-12)
  expect_identical(b$observed, !is.na(g$T) | !is.na(g$S))
  expect_equal(b$land_weight[2, 2], 1)
})

test_that("area means weight cells by cos(latitude)", {
  G <- rbind(c(1, NA), c(2, 3), c(4, 5), c(NA, 6))
  lat <- -90 + (1:4 - 0.5) * 45
  w <- cos(lat * pi / 180)
  W <- matrix(w, 4, 2)
  S <- sum((W * G)[1:2, ], na.rm = TRUE) / sum(W[1:2, ][!is.na(G[1:2, ])])
  N <- sum((W * G)[3:4, ], na.rm = TRUE) / sum(W[3:4, ][!is.na(G[3:4, ])])
  h <- morie_hadcrut_area_mean(G)
  expect_equal(c(h$south, h$north), c(S, N), tolerance = 1e-12)
  expect_equal(h$mean, (S + N) / 2, tolerance = 1e-12)
  expect_equal(morie_hadcrut_area_mean(G, "land_ratio")$mean, 2 / 3 * N + S / 3, tolerance = 1e-12)
  expect_equal(morie_hadcrut_area_mean(G, "area")$mean, sum(W * G, na.rm = TRUE) / sum(W[!is.na(G)]), tolerance = 1e-12)
  V <- matrix(0.1, 4, 2)
  hv <- morie_hadcrut_area_mean(G, "area", var = V)
  expect_equal(hv$var, sum((W^2 * V)[!is.na(G)]) / sum(W[!is.na(G)])^2, tolerance = 1e-12)
  G2 <- G
  G2[3:4, ] <- NA
  expect_equal(morie_hadcrut_area_mean(G2)$mean, S, tolerance = 1e-12)
  expect_true(is.na(morie_hadcrut_area_mean(matrix(NA_real_, 2, 2))$mean))
  expect_error(morie_hadcrut_area_mean(G, "zonal"), "route must be one of")
  seen <- !is.na(G)
  R <- matrix(1:8, 4)
  Rm <- R
  Rm[!seen] <- NA
  expect_equal(morie_hadcrut_coverage_error(R, seen), morie_hadcrut_area_mean(Rm)$mean - morie_hadcrut_area_mean(R)$mean, tolerance = 1e-12)
})

test_that("the headline combines uncorrelated, ensemble and coverage uncertainty", {
  g <- .g()
  ens <- lapply(1:5, function(k) g$T * 0 + k / 10)
  ref <- list(matrix(1:12, 4), matrix(12:1, 4))
  r <- morie_hadcrut(g$T, g$S, g$LF, T_var = matrix(0.04, 4, 3), sst_var = matrix(0.01, 4, 3),
                     ensemble = ens, reference = ref)
  b <- morie_hadcrut_blend(g$T, g$S, g$LF, T_var = matrix(0.04, 4, 3), sst_var = matrix(0.01, 4, 3))
  a <- morie_hadcrut_area_mean(b$anomaly, var = b$variance)
  expect_equal(r$estimate, a$mean, tolerance = 1e-12)
  expect_equal(r$se_uncorrelated, sqrt(a$var), tolerance = 1e-12)
  em <- vapply(ens, function(x) morie_hadcrut_area_mean(x)$mean, 1)
  expect_equal(r$se_correlated, stats::sd(em), tolerance = 1e-12)
  ce <- vapply(ref, function(x) morie_hadcrut_coverage_error(x, b$observed), 1)
  expect_equal(r$se_coverage, sqrt(mean(ce^2)), tolerance = 1e-12)
  se <- sqrt(a$var + stats::var(em) + mean(ce^2))
  expect_equal(r$se, se, tolerance = 1e-12)
  expect_equal(r$ci_upper, a$mean + stats::qnorm(0.975) * se, tolerance = 1e-9)
  lat <- -90 + (1:4 - 0.5) * 45
  w <- matrix(cos(lat * pi / 180), 4, 3)
  expect_equal(r$coverage, sum(w[b$observed]) / sum(w), tolerance = 1e-12)
  e <- morie_hadcrut(g$T, g$S, g$LF, ensemble = ens, interval = "ensemble", level = 0.6)
  s <- sort(em)
  expect_equal(c(e$ci_lower, e$ci_upper), s[c(ceiling(0.2 * 5), ceiling(0.8 * 5))])
  n <- morie_hadcrut(g$T, g$S)
  expect_null(n$se)
  expect_null(n$ci_lower)
  expect_error(morie_hadcrut(g$T[1, , drop = FALSE], g$S[1, , drop = FALSE]), "two latitude bands")
  expect_error(morie_hadcrut(g$T, g$S[, 1:2]), "rectangular grid")
  expect_error(morie_hadcrut(g$T, g$S, level = 1), "strictly inside")
  expect_error(morie_hadcrut(g$T, g$S, interval = "bootstrap"), "interval must be one of")
  expect_match(morie_hadcrut_cheatsheet(), "HadCRUT5")
})
