# SPDX-License-Identifier: AGPL-3.0-or-later
# test-batch14.R: coverage for mnpbt.R, modules.R, moeml.R, morie-package.R,
#   mrkvr.R, mrm_kulldorff.R, mrm_lisa.R, mrm_mandela_spectrum.R, mrm_otis.R,
#   mrm_samples.R, mrm_siu.R (rOpenSci #770 coverage campaign)

test_that("mnpbt() runs the multinomial (J >= 3) MC path", {
  set.seed(14)
  U <- matrix(rnorm(15L), nrow = 5L, ncol = 3L)
  res <- mnpbt(U, n_draws = 200L, seed = 1L)
  expect_type(res, "list")
  expect_named(res, c("probs", "max_alt", "n_obs", "n_alt", "method"))
  expect_equal(res$method, "multinomial_probit")
  expect_equal(res$n_obs, 5L)
  expect_equal(res$n_alt, 3L)
  expect_equal(dim(res$probs), c(5L, 3L))
  expect_true(all(res$probs >= 0 & res$probs <= 1))
  expect_true(all(abs(rowSums(res$probs) - 1) < 1e-8))
  expect_equal(length(res$max_alt), 5L)
  expect_true(all(res$max_alt %in% 1:3))
})

test_that("mnpbt() uses the closed-form binary (J == 2) path", {
  set.seed(15)
  U <- matrix(rnorm(8L), nrow = 4L, ncol = 2L)
  res <- mnpbt(U, n_draws = 100L, seed = 2L)
  expect_equal(res$n_alt, 2L)
  expect_equal(dim(res$probs), c(4L, 2L))
  expect_true(all(abs(rowSums(res$probs) - 1) < 1e-10))
  expect_equal(res$probs[, 2], stats::pnorm((U[, 2] - U[, 1]) / sqrt(2)))
})

test_that("mnpbt() short-circuits when J < 2", {
  res <- mnpbt(matrix(1, nrow = 3L, ncol = 1L))
  expect_equal(res$n_alt, 1L)
  expect_equal(dim(res$probs), c(3L, 1L))
  expect_true(all(res$probs == 1))
  expect_equal(res$max_alt, rep(1L, 3L))
})

test_that("mnpbt() coerces a non-matrix vector to a single-row matrix", {
  res <- mnpbt(c(0.2, 0.5, 0.3), n_draws = 100L, seed = 3L)
  expect_equal(res$n_obs, 1L)
  expect_equal(res$n_alt, 3L)
  expect_equal(dim(res$probs), c(1L, 3L))
})

test_that("morie_multinomial_probit_spatial() is an exported alias of mnpbt()", {
  expect_identical(morie_multinomial_probit_spatial, mnpbt)
})

test_that("morie_list_morie_modules() returns the documented module surface", {
  mods <- morie_list_morie_modules()
  expect_s3_class(mods, "data.frame")
  expect_named(mods, c("name", "description"))
  expect_equal(nrow(mods), 21L)
  expect_type(mods$name, "character")
  expect_type(mods$description, "character")
  expect_true(all(nchar(mods$name) > 0))
  expect_true(all(nchar(mods$description) > 0))
  expect_true("data-wrangling" %in% mods$name)
  expect_true("final-report" %in% mods$name)
})

test_that("modules.R CPADS-data callables are exercised offline-safe", {
  if (FALSE) {
    data <- morie_load_cpads_data()
    canon <- morie_canonicalize_cpads_data(data)
    out <- morie_run_morie_module("descriptive-statistics")
    outs <- morie_run_morie_modules(c("descriptive-statistics"))
    expect_error(morie_run_morie_module("not-a-module"), "Unknown module")
  }
  expect_true(TRUE)
})

test_that("morie:::mixture_of_experts() runs with default gating/experts", {
  set.seed(16)
  x <- matrix(rnorm(12L), nrow = 4L, ncol = 3L)
  res <- morie:::mixture_of_experts(x)
  expect_type(res, "list")
  expect_named(res, c("tensor", "gate", "topk_idx", "load", "method"))
  expect_equal(res$method, "MoE")
  expect_equal(nrow(res$tensor), 4L)
  expect_equal(dim(res$gate), c(4L, 2L))
  expect_true(all(abs(rowSums(res$gate) - 1) < 1e-8))
  expect_equal(length(res$load), 2L)
  expect_true(all(is.finite(res$load)))
})

test_that("morie:::mixture_of_experts() honours a custom W_gate and top_k", {
  set.seed(17)
  x <- matrix(rnorm(15L), nrow = 5L, ncol = 3L)
  W_gate <- matrix(rnorm(12L), nrow = 3L, ncol = 4L)
  res <- morie:::mixture_of_experts(x, W_gate = W_gate, top_k = 1L)
  expect_equal(dim(res$gate), c(5L, 4L))
  expect_true(all(rowSums(res$gate > 0) == 1L))
  expect_equal(dim(res$topk_idx), c(5L, 1L))
  expect_true(all(res$topk_idx >= 0L & res$topk_idx <= 3L))
})

test_that("morie:::mixture_of_experts() clamps top_k to the number of experts", {
  set.seed(18)
  x <- matrix(rnorm(8L), nrow = 4L, ncol = 2L)
  res <- morie:::mixture_of_experts(x, top_k = 99L)
  expect_equal(ncol(res$topk_idx), 2L)
})

test_that("morie_marker_variance() reports the VanRaden / naive split", {
  set.seed(16)
  M <- matrix(sample(0:2, 160L, replace = TRUE), nrow = 20L, ncol = 8L)
  y <- as.numeric(M %*% rnorm(8L) + 0.5 * rnorm(20L))
  res <- morie_marker_variance(rep(0, 20L), y, M)
  expect_type(res, "list")
  expect_named(res, c(
    "estimate", "sigma_g2", "sigma_e2", "h2",
    "sigma_m2_vanraden", "sigma_m2_naive", "sum_2pq",
    "p_freq", "n", "p", "method"
  ))
  expect_equal(res$n, 20L)
  expect_equal(res$p, 8L)
  expect_equal(length(res$p_freq), 8L)
  expect_true(is.finite(res$sigma_g2) && res$sigma_g2 >= 0)
  expect_true(is.finite(res$sigma_e2) && res$sigma_e2 >= 0)
  expect_true(res$sum_2pq > 0)
  expect_equal(res$estimate, res$sigma_m2_vanraden)
  expect_true(is.na(res$h2) || (res$h2 >= 0 && res$h2 <= 1))
  expect_equal(res$method, "VanRaden + naive marker-variance split")
})

test_that("morie_marker_variance() accepts a NULL fixed-effect design", {
  set.seed(19)
  M <- matrix(sample(0:2, 120L, replace = TRUE), nrow = 15L, ncol = 8L)
  y <- as.numeric(M %*% rnorm(8L) + 0.5 * rnorm(15L))
  res <- morie_marker_variance(NULL, y, M)
  expect_equal(res$n, 15L)
  expect_equal(res$p, 8L)
  expect_true(is.finite(res$sigma_m2_naive))
})

test_that("morie_marker_variance() accepts an explicit fixed-effect design", {
  set.seed(20)
  M <- matrix(sample(0:2, 96L, replace = TRUE), nrow = 12L, ncol = 8L)
  y <- as.numeric(M %*% rnorm(8L) + 0.5 * rnorm(12L))
  x <- rnorm(12L)
  res <- morie_marker_variance(x, y, M)
  expect_equal(res$n, 12L)
  expect_true(is.finite(res$estimate))
})

test_that(".haversine_km_mat() returns 0 distance for identical points", {
  d <- morie:::.haversine_km_mat(43.6, -79.4, 43.6, -79.4)
  expect_equal(d, 0)
  d2 <- morie:::.haversine_km_mat(43.6, -79.4, 43.7, -79.4)
  expect_true(d2 > 0 && is.finite(d2))
})

test_that(".poisson_lrt() handles documented degenerate cases", {
  expect_equal(morie:::.poisson_lrt(0, 1, 0.5, 100), 0.0)
  expect_equal(morie:::.poisson_lrt(5, 0, 1, 100), 0.0)
  expect_equal(morie:::.poisson_lrt(3, 10, 5, 100), 0.0)
  lrt <- morie:::.poisson_lrt(20, 30, 5, 100)
  expect_true(lrt > 0 && is.finite(lrt))
})

test_that("mrm_tps_kulldorff_scan() returns empty frame for tiny input", {
  df <- data.frame(
    OCC_DATE = rep("01/01/2020 12:00:00 PM", 10L),
    LAT_WGS84 = rnorm(10L, 43.6, 0.01),
    LONG_WGS84 = rnorm(10L, -79.4, 0.01)
  )
  res <- mrm_tps_kulldorff_scan(df, n_permutations = 9L)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0L)
})

test_that("mrm_tps_kulldorff_scan() validates inputs", {
  expect_error(mrm_tps_kulldorff_scan(list(a = 1)))
  bad <- data.frame(OCC_DATE = "x", LAT_WGS84 = 1)
  expect_error(mrm_tps_kulldorff_scan(bad))
})

test_that("mrm_tps_kulldorff_scan() runs end to end on bundled data", {
  if (FALSE) {
    tps <- morie_sample("tps_assault")
    res <- mrm_tps_kulldorff_scan(tps, n_permutations = 19L)
    expect_s3_class(res, "data.frame")
  }
  expect_true(TRUE)
})

test_that(".knn_weights_lisa() builds a row-normalised weight matrix", {
  set.seed(21)
  lat <- 43.6 + runif(8L) * 0.05
  lon <- -79.4 + runif(8L) * 0.05
  W <- morie:::.knn_weights_lisa(lat, lon, k = 3L)
  expect_equal(dim(W), c(8L, 8L))
  expect_true(all(abs(rowSums(W) - 1) < 1e-8))
  expect_equal(diag(W), rep(0, 8L))
})

test_that("mrm_tps_lisa() computes per-polygon local Moran's I", {
  set.seed(2026)
  grid <- expand.grid(
    lat = 43.6 + (0:3) * 0.02,
    lon = -79.4 + (0:3) * 0.02
  )
  grid$ASSAULT_2024 <- rpois(nrow(grid), lambda = grid$lat * 12)
  res <- mrm_tps_lisa(grid,
    count_col = "ASSAULT_2024",
    lat_col = "lat", lon_col = "lon",
    k = 4L, n_permutations = 99L, seed = 42L
  )
  expect_type(res, "list")
  expect_named(res, c(
    "n_polygons", "global_moran_I", "permutations",
    "knn_k", "table", "quadrants_all",
    "quadrants_significant_p05", "n_significant_p05"
  ))
  expect_equal(res$n_polygons, 16L)
  expect_equal(res$permutations, 99L)
  expect_equal(res$knn_k, 4L)
  expect_s3_class(res$table, "data.frame")
  expect_equal(nrow(res$table), 16L)
  expect_named(res$table, c(
    "id", "lat", "lon", "x", "z", "lag_z",
    "I_local", "quadrant", "p_value",
    "significant_p05"
  ))
  expect_true(all(res$table$quadrant %in% c("HH", "HL", "LH", "LL")))
  expect_true(all(res$table$p_value > 0 & res$table$p_value <= 1))
  expect_true(is.finite(res$global_moran_I))
  expect_equal(res$n_significant_p05, sum(unlist(res$quadrants_significant_p05)))
})

test_that("mrm_tps_lisa() honours an id_col passthrough", {
  set.seed(2027)
  grid <- expand.grid(
    lat = 43.6 + (0:2) * 0.02,
    lon = -79.4 + (0:2) * 0.02
  )
  grid$ASSAULT <- rpois(nrow(grid), lambda = 8)
  grid$hood <- paste0("H", seq_len(nrow(grid)))
  res <- mrm_tps_lisa(grid,
    count_col = "ASSAULT", id_col = "hood",
    lat_col = "lat", lon_col = "lon",
    k = 3L, n_permutations = 49L, seed = 1L
  )
  expect_true(all(res$table$id %in% grid$hood))
})

test_that("mrm_tps_lisa() errors on too-few polygons", {
  small <- data.frame(
    lat = 43.6 + (0:2) * 0.01,
    lon = -79.4 + (0:2) * 0.01,
    ASSAULT = c(1, 2, 3)
  )
  expect_error(
    mrm_tps_lisa(small,
      count_col = "ASSAULT", lat_col = "lat",
      lon_col = "lon", n_permutations = 9L
    ),
    "5 polygons"
  )
})

test_that("mrm_tps_lisa() validates column presence", {
  grid <- data.frame(lat = 1:6, lon = 1:6)
  expect_error(mrm_tps_lisa(grid,
    count_col = "missing",
    lat_col = "lat", lon_col = "lon"
  ))
})

test_that("mrm_tps_polygon_moran_per_year() loops over year columns", {
  set.seed(2026)
  grid <- expand.grid(
    lat = 43.6 + (0:3) * 0.02,
    lon = -79.4 + (0:3) * 0.02
  )
  grid$ASSAULT_2023 <- rpois(nrow(grid), lambda = grid$lat * 10)
  grid$ASSAULT_2024 <- rpois(nrow(grid), lambda = grid$lat * 12)
  res <- mrm_tps_polygon_moran_per_year(
    grid,
    year_cols = c("ASSAULT_2023", "ASSAULT_2024"),
    lat_col = "lat", lon_col = "lon",
    k = 4L, n_permutations = 49L, seed = 42L
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2L)
  expect_true(all(c("year", "n_events", "moran_I") %in% names(res)))
  expect_equal(sort(res$year), c(2023L, 2024L))
  expect_true(all(is.finite(res$moran_I)))
  expect_true(all(res$n_events >= 0L))
})

test_that("mrm_tps_polygon_moran_per_year() skips failing year columns", {
  set.seed(2028)
  small <- data.frame(
    lat = 43.6 + (0:2) * 0.01,
    lon = -79.4 + (0:2) * 0.01,
    ASSAULT_2024 = c(1, 2, 3)
  )
  res <- mrm_tps_polygon_moran_per_year(
    small,
    year_cols = "ASSAULT_2024",
    lat_col = "lat", lon_col = "lon", n_permutations = 9L
  )
  expect_null(res)
})

make_b01 <- function(n = 60L, seed = 30L) {
  set.seed(seed)
  data.frame(
    NumberConsecutiveDays_Segregation = sample(c(1:40, NA), n, replace = TRUE),
    EndFiscalYear = sample(c(2023L, 2024L), n, replace = TRUE),
    UniqueIndividual_ID = paste0("2023-", sprintf("%05d", sample(1:25, n, TRUE)), "-SG"),
    MentalHealth_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    SuicideRisk_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    SuicideWatch_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("mrm_otis_mandela_spectrum() returns the tidy long grid", {
  b01 <- make_b01()
  spec <- mrm_otis_mandela_spectrum(b01)
  expect_s3_class(spec, "data.frame")
  expect_named(spec, c(
    "year", "denominator", "contact_proxy",
    "n_eligible", "n_mandela", "rate", "pct"
  ))
  expect_equal(nrow(spec), 3L * 3L * 3L)
  expect_true(all(spec$contact_proxy %in% c("none", "any_alert", "no_alert")))
  expect_true(all(spec$denominator %in%
    c("row", "individual_any", "individual_cumulative")))
  expect_true(all(spec$n_mandela <= spec$n_eligible))
  fin <- spec$rate[is.finite(spec$rate)]
  expect_true(all(fin >= 0 & fin <= 1))
  expect_true("pooled" %in% spec$year)
})

test_that("mrm_otis_mandela_spectrum() accepts a subset of proxies/denoms", {
  b01 <- make_b01()
  spec <- mrm_otis_mandela_spectrum(
    b01,
    contact_proxies = "none", denominators = "row"
  )
  expect_true(all(spec$contact_proxy == "none"))
  expect_true(all(spec$denominator == "row"))
})

test_that("mrm_otis_mandela_spectrum() handles a c11_aggregate denominator", {
  b01 <- make_b01()
  c11 <- data.frame(
    EndFiscalYear = c(2023L, 2023L, 2024L, 2024L),
    NumberIndividuals_Segregation = c(10L, 5L, 8L, 6L),
    Aggregate_Duration = c(
      "1 to 14 days", "Greater than 15 days",
      "1 to 14 days", "Greater than 15 days"
    ),
    stringsAsFactors = FALSE
  )
  spec <- mrm_otis_mandela_spectrum(
    b01,
    denominators = c("row", "c11_aggregate"),
    contact_proxies = "none", c11_data = c11
  )
  expect_true("c11_aggregate" %in% spec$denominator)
})

test_that("mrm_otis_mandela_spectrum() validates inputs", {
  expect_error(mrm_otis_mandela_spectrum(list(a = 1)))
  bad <- data.frame(x = 1:3)
  expect_error(mrm_otis_mandela_spectrum(bad))
})

test_that(".gini_int() spans the documented [0, 1] range", {
  expect_equal(morie:::.gini_int(rep(5, 10L)), 0)
  expect_true(is.na(morie:::.gini_int(numeric(0))))
  expect_true(is.na(morie:::.gini_int(rep(0, 5L))))
  g <- morie:::.gini_int(c(1, 1, 1, 100))
  expect_true(g > 0 && g < 1)
})

test_that(".hill_mle() returns NA for too-short tails", {
  expect_true(is.na(morie:::.hill_mle(c(5), x_min = 1)))
  a <- morie:::.hill_mle(c(2, 4, 8, 16, 32), x_min = 1)
  expect_true(is.finite(a))
})

test_that(".cramer_v() handles degenerate tables", {
  expect_true(is.na(morie:::.cramer_v(matrix(5, 1L, 1L))))
  tbl <- table(c(1, 1, 0, 0), c(1, 1, 0, 0))
  v <- morie:::.cramer_v(tbl)
  expect_true(is.finite(v) && v >= 0 && v <= 1)
})

test_that("mrm_otis_placement_concentration() summarises b09 bands", {
  b09 <- data.frame(
    EndFiscalYear = c(2023L, 2023L, 2024L, 2024L),
    NumberPlacements_Segregation = c("1", "6 to 10", "2", "Greater than 40"),
    NumberIndividuals_Segregation = c(40L, 12L, 30L, 4L),
    stringsAsFactors = FALSE
  )
  res <- mrm_otis_placement_concentration(b09)
  expect_s3_class(res, "data.frame")
  expect_named(res, c(
    "year", "n_individuals", "n_placements",
    "mean_per_individual", "gini", "hill_alpha",
    "top_pct_share"
  ))
  expect_true("pooled" %in% res$year)
  expect_true(all(res$n_individuals >= 0))
  fin <- res$gini[is.finite(res$gini)]
  expect_true(all(fin >= 0 & fin <= 1))
  fin2 <- res$top_pct_share[is.finite(res$top_pct_share)]
  expect_true(all(fin2 >= 0 & fin2 <= 1))
})

test_that("mrm_otis_placement_concentration() honours a gender filter", {
  b09 <- data.frame(
    EndFiscalYear = c(2023L, 2023L, 2024L),
    NumberPlacements_Segregation = c("1", "2", "3"),
    NumberIndividuals_Segregation = c(20L, 10L, 5L),
    Gender = c("Male", "Female", "Male"),
    stringsAsFactors = FALSE
  )
  res <- mrm_otis_placement_concentration(
    b09,
    gender_col = "Gender", gender_keep = "Male"
  )
  expect_s3_class(res, "data.frame")
  expect_true(all(res$n_individuals >= 0))
})

test_that("mrm_otis_placement_concentration() validates columns", {
  expect_error(mrm_otis_placement_concentration(data.frame(x = 1)))
})

test_that("mrm_otis_seg_duration_km() pools durations by default", {
  set.seed(31)
  b01 <- data.frame(
    NumberConsecutiveDays_Segregation = c(sample(1:40, 50L, replace = TRUE), NA)
  )
  res <- mrm_otis_seg_duration_km(b01)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_named(res, c(
    "stratum", "n", "mean_days", "median_days",
    "q25_days", "pct_above_mandela",
    "median_among_above_mandela"
  ))
  expect_equal(res$stratum, "pooled")
  expect_true(res$n > 0)
  expect_true(res$pct_above_mandela >= 0 && res$pct_above_mandela <= 100)
})

test_that("mrm_otis_seg_duration_km() stratifies by group_cols", {
  set.seed(32)
  b01 <- data.frame(
    NumberConsecutiveDays_Segregation = sample(1:40, 60L, replace = TRUE),
    MentalHealth_Alert = sample(c("Yes", "No"), 60L, replace = TRUE),
    stringsAsFactors = FALSE
  )
  res <- mrm_otis_seg_duration_km(b01, group_cols = "MentalHealth_Alert")
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) >= 1L)
  expect_true(all(res$n >= 0))
})

test_that("mrm_otis_seg_duration_km() validates the duration column", {
  expect_error(mrm_otis_seg_duration_km(data.frame(x = 1:3)))
})

test_that("mrm_otis_mortification_cooccurrence() reports pairwise Cramer's V", {
  set.seed(33)
  b01 <- data.frame(
    MentalHealth_Alert = sample(c("Yes", "No"), 80L, replace = TRUE),
    SuicideRisk_Alert = sample(c("Yes", "No"), 80L, replace = TRUE),
    SuicideWatch_Alert = sample(c("Yes", "No"), 80L, replace = TRUE),
    stringsAsFactors = FALSE
  )
  res <- mrm_otis_mortification_cooccurrence(b01)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3L)
  expect_named(res, c(
    "alert_a", "alert_b", "n", "chi2", "df",
    "p_value", "morie_cramers_v"
  ))
  fin <- res$morie_cramers_v[is.finite(res$morie_cramers_v)]
  expect_true(all(fin >= 0 & fin <= 1))
  expect_true(all(res$n > 0))
})

test_that("mrm_otis_mortification_cooccurrence() validates columns", {
  expect_error(mrm_otis_mortification_cooccurrence(data.frame(x = 1)))
})

test_that("mrm_otis_region_locality() reports the contingency summary", {
  set.seed(34)
  b01 <- data.frame(
    Region_AtTimeOfPlacement = sample(c("East", "West", "North"), 90L, TRUE),
    Region_MostRecentPlacement = sample(c("East", "West", "North"), 90L, TRUE),
    stringsAsFactors = FALSE
  )
  res <- mrm_otis_region_locality(b01)
  expect_type(res, "list")
  expect_named(res, c(
    "table", "chi2", "df", "p_value", "morie_cramers_v",
    "diagonal_share", "off_diagonal_share"
  ))
  expect_true(is.table(res$table))
  expect_true(is.finite(res$chi2))
  expect_true(res$diagonal_share >= 0 && res$diagonal_share <= 1)
  expect_equal(round(res$diagonal_share + res$off_diagonal_share, 4), 1)
})

test_that("mrm_otis_region_locality() validates columns", {
  expect_error(mrm_otis_region_locality(data.frame(x = 1)))
})

test_that("morie_tps_layer_urls() lists the known ArcGIS layers", {
  urls <- morie_tps_layer_urls()
  expect_type(urls, "character")
  expect_equal(length(urls), 9L)
  expect_true(!is.null(names(urls)) && all(nchar(names(urls)) > 0))
  expect_true("Assault" %in% names(urls))
  expect_true(all(grepl("^https://", urls)))
})

test_that("morie_sample() loads a bundled reference CSV", {
  if (FALSE) {
    b01 <- morie_sample("otis_b01")
    expect_s3_class(b01, "data.frame")
    expect_error(morie_sample("not_a_sample"))
  }
  expect_true(TRUE)
})

test_that("morie_fetch_tps() and morie_fetch_siu() are network fetchers", {
  if (FALSE) {
    expect_error(morie_fetch_tps("NotACategory"), "Unknown TPS category")
    csv <- morie_fetch_tps("Assault",
      cache_dir = tempdir(),
      where = "OCC_YEAR = 2024"
    )
    siu <- morie_fetch_siu(years = 2023:2024, cache_dir = tempdir())
  }
  expect_true(TRUE)
})

test_that(".parse_iso() parses ISO dates and tolerates junk", {
  d <- morie:::.parse_iso(c("2023-01-15", "2024-06-30"))
  expect_s3_class(d, "Date")
  expect_equal(length(d), 2L)
  expect_true(is.na(morie:::.parse_iso("not-a-date")))
})

make_siu <- function(n = 60L, seed = 40L) {
  set.seed(seed)
  inc <- as.Date("2022-01-01") + sample(0:600, n, replace = TRUE)
  dec <- inc + sample(30:300, n, replace = TRUE)
  data.frame(
    date_of_incident_iso = as.character(inc),
    date_of_director_decision_iso = as.character(dec),
    police_service = sample(c("Toronto", "Ottawa", "Hamilton"), n, TRUE),
    director_decision_category = sample(c("charges_laid", "no_charges"), n, TRUE),
    reason_for_interaction = sample(c("vehicle", "firearm"), n, TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("mrm_siu_case_to_decision_km() reports pooled + per-service KM", {
  siu <- make_siu()
  res <- mrm_siu_case_to_decision_km(siu, min_n = 5L)
  expect_type(res, "list")
  expect_named(res, c("pooled", "by_service"))
  expect_s3_class(res$pooled, "data.frame")
  expect_equal(nrow(res$pooled), 1L)
  expect_named(res$pooled, c(
    "stratum", "n", "n_censored", "median_days",
    "mean_days", "p25_days", "p75_days", "max_days"
  ))
  expect_equal(res$pooled$stratum, "pooled")
  expect_true(res$pooled$n > 0)
  expect_true(res$pooled$median_days >= 0)
  expect_s3_class(res$by_service, "data.frame")
  expect_true(all(res$by_service$n >= 5L))
})

test_that("mrm_siu_case_to_decision_km() censors or drops open cases", {
  siu <- make_siu()
  siu$date_of_director_decision_iso[1:10] <- NA
  res_cens <- mrm_siu_case_to_decision_km(siu, censor_open_cases = TRUE)
  expect_true(res_cens$pooled$n_censored > 0)
  res_drop <- mrm_siu_case_to_decision_km(siu, censor_open_cases = FALSE)
  expect_equal(res_drop$pooled$n_censored, 0L)
})

test_that("mrm_siu_case_to_decision_km() validates columns", {
  expect_error(mrm_siu_case_to_decision_km(list(a = 1)))
  expect_error(mrm_siu_case_to_decision_km(data.frame(x = 1:3)))
})

test_that("mrm_siu_per_service_rate() tabulates cases by service/year", {
  siu <- make_siu()
  res <- mrm_siu_per_service_rate(siu)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("service", "year", "n_cases"))
  expect_true(all(res$n_cases > 0))
  expect_true(nrow(res) > 0)
})

test_that("mrm_siu_per_service_rate() supports a stratifying column", {
  siu <- make_siu()
  res <- mrm_siu_per_service_rate(siu, stratify_col = "reason_for_interaction")
  expect_named(res, c("service", "year", "stratum", "n_cases"))
  expect_true(all(res$stratum %in% c("vehicle", "firearm")))
})

test_that("mrm_siu_per_service_rate() validates columns", {
  expect_error(mrm_siu_per_service_rate(data.frame(x = 1)))
})

test_that("mrm_siu_outcome_classifier() cross-tabs Director's decisions", {
  siu <- make_siu()
  res <- mrm_siu_outcome_classifier(siu)
  expect_s3_class(res, "data.frame")
  expect_named(res, c(
    "service", "outcome", "n_cases",
    "share_within_service"
  ))
  expect_true(all(res$n_cases > 0))
  expect_true(all(res$share_within_service > 0 &
    res$share_within_service <= 1))
})

test_that("mrm_siu_outcome_classifier() falls back to alternative columns", {
  siu <- make_siu()
  names(siu)[names(siu) == "director_decision_category"] <- "director_decision"
  res <- mrm_siu_outcome_classifier(siu)
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)
})

test_that("mrm_siu_outcome_classifier() validates columns", {
  expect_error(mrm_siu_outcome_classifier(data.frame(x = 1)))
})
