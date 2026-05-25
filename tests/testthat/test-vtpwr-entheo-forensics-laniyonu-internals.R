# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2VV: tests for internal helpers across tps_spatial_advanced.R,
# vtpwr.R, entheo_analysis.R, laniyonu_gentrification_policing.R,
# laniyonu_actuarial_risk_disparity.R, ingest_forensics.R,
# sensitivity.R, and arsau.R.

# ========================================================== tps_spatial_advanced.R

test_that(".tps_adv_result builds a morie_tps_spatial_advanced_result list", {
  out <- morie:::.tps_adv_result(title = "Test", call = "demo",
                                   summary_lines = list(N = 5))
  expect_type(out, "list")
  expect_s3_class(out, "morie_tps_spatial_advanced_result")
  expect_s3_class(out, "morie_rich_result")
})

test_that(".tps_coords drops NA + (0,0) rows + returns a 2-col matrix", {
  df <- data.frame(
    lat = c(43.6, NA, 43.7, 0, 43.8),
    lon = c(-79.4, -79.5, -79.3, 0, -79.2))
  out <- morie:::.tps_coords(df, "lat", "lon")
  expect_true(is.matrix(out))
  expect_equal(ncol(out), 2L)
  expect_equal(nrow(out), 3L)
})

test_that(".tps_coords returns 0-row matrix on missing columns", {
  out <- morie:::.tps_coords(data.frame(a = 1:3), "lat", "lon")
  expect_true(is.matrix(out))
  expect_equal(nrow(out), 0L)
})

test_that(".tps_haversine_km returns 0 on coincident + ~504 km YYZ<->YUL", {
  expect_equal(
    morie:::.tps_haversine_km(43.6532, -79.3832, 43.6532, -79.3832),
    0, tolerance = 1e-8)
  d <- morie:::.tps_haversine_km(43.6532, -79.3832, 45.5017, -73.5673)
  expect_true(abs(d - 504) < 5)
})

test_that(".tps_knn_idx returns n x k integer matrix without self-edges", {
  set.seed(1L)
  coords <- matrix(stats::runif(20L), 10L, 2L)
  idx <- morie:::.tps_knn_idx(coords, k = 3L)
  expect_true(is.matrix(idx))
  expect_equal(dim(idx), c(10L, 3L))
  for (i in seq_len(10L)) expect_false(i %in% idx[i, ])
})

# ===================================================================== vtpwr.R

test_that(".vtpwr_perms enumerates all permutations of length n", {
  out <- morie:::.vtpwr_perms(c(1L, 2L, 3L))
  expect_equal(nrow(out), 6L)
  expect_equal(ncol(out), 3L)
  expect_equal(sort(apply(out, 1, paste, collapse = ",")),
               sort(c("1,2,3", "1,3,2", "2,1,3",
                      "2,3,1", "3,1,2", "3,2,1")))
})

test_that(".vtpwr_perms is length-1 identity", {
  expect_equal(morie:::.vtpwr_perms(c(5L)), matrix(5L, 1L, 1L))
})

test_that(".vtpwr_pivot finds the pivotal player in an ordering", {
  # 3 players weights c(4, 3, 2); quota = 5. Ordering (1, 2, 3):
  # cum 0->4 (no), 4->7 (>=5, prev<5) -> player 2 pivotal.
  expect_equal(morie:::.vtpwr_pivot(c(1L, 2L, 3L),
                                      w = c(4, 3, 2), quota = 5),
               2L)
})

test_that(".vtpwr_swing_increment marks coalition-changing players", {
  # weights (4, 3, 2); quota = 5. Coalition = {1, 2}; tot_in = 7.
  # player 1 in coalition: tot_in=7 >= 5 and (7-4)=3 < 5 -> swing.
  # player 2 in coalition: tot_in=7 >= 5 and (7-3)=4 < 5 -> swing.
  mask <- c(TRUE, TRUE, FALSE)
  inc <- morie:::.vtpwr_swing_increment(mask, tot_in = 7,
                                          w = c(4, 3, 2),
                                          quota = 5, n = 3L)
  expect_equal(inc, c(1, 1, 0))
})

test_that(".vtpwr_exact returns banzhaf+shapley summing close to 1", {
  out <- morie:::.vtpwr_exact(w = c(4, 3, 2), quota = 5, n = 3L)
  expect_named(out, c("banzhaf", "shapley_shubik", "quota",
                      "weights", "method"))
  expect_equal(sum(out$shapley_shubik), 1, tolerance = 1e-10)
  expect_equal(sum(out$banzhaf), 1, tolerance = 1e-10)
})

test_that(".vtpwr_mc returns same shape as .vtpwr_exact", {
  out <- morie:::.vtpwr_mc(w = c(4, 3, 2), quota = 5, n = 3L)
  expect_named(out, c("banzhaf", "shapley_shubik", "quota",
                      "weights", "method"))
  expect_length(out$banzhaf, 3L)
  expect_length(out$shapley_shubik, 3L)
})

# =========================================================== entheo_analysis.R

test_that(".entheo_extract_pair handles record list + plain pair", {
  rec <- list(eeg = list(data_dmt = 1:5, data_pcb = 6:10),
              fmri = list(data_dmt = 11:15, data_pcb = 16:20))
  out <- morie:::.entheo_extract_pair(rec, NULL)
  expect_equal(out$e_dmt, 1:5)
  expect_equal(out$f_dmt, 11:15)
  out2 <- morie:::.entheo_extract_pair(c(1, 2, 3), c(4, 5, 6))
  expect_equal(out2$e_dmt, c(1, 2, 3))
})

test_that(".entheo_envelope returns vector of same length on a vec", {
  set.seed(2L); x <- stats::rnorm(20L)
  out <- morie:::.entheo_envelope(x)
  expect_length(out, 20L)
  # Middle values should be non-NA (filter sides = 2).
  expect_true(all(is.finite(out[3:18])))
})

test_that(".entheo_align trims to min length + handles scalar case", {
  out <- morie:::.entheo_align(c(1, 2, 3, 4, 5), c(10, 20, 30))
  expect_length(out$e, 3L)
  expect_length(out$f, 3L)
})

test_that(".entheo_binding_per_frame returns vector of length n", {
  set.seed(3L)
  eeg <- stats::rnorm(50L); fmri <- stats::rnorm(50L)
  out <- morie:::.entheo_binding_per_frame(eeg, fmri)
  expect_length(out, 50L)
  expect_true(all(is.finite(out)))
})

# =============================================== laniyonu_gentrification_policing.R

test_that(".lan_gp_placeholder_W returns n x n row-stochastic matrix", {
  set.seed(4L)
  crime <- stats::runif(15L, 0, 100)
  W <- morie:::.lan_gp_placeholder_W(crime, k = 3L)
  expect_true(is.matrix(W))
  expect_equal(dim(W), c(15L, 15L))
  # Row sums = 1 since each row has exactly k neighbours weighted 1/k
  # then normalised.
  expect_equal(unname(rowSums(W)), rep(1, 15L), tolerance = 1e-10)
})

test_that(".lan_gp_morans_i returns NA on degenerate input", {
  expect_true(is.na(morie:::.lan_gp_morans_i(c(1), matrix(0, 1, 1))))
  expect_true(is.na(morie:::.lan_gp_morans_i(rep(5, 5),
                                               matrix(0, 5, 5))))
})

test_that(".lan_gp_morans_i returns finite I on real residuals", {
  set.seed(5L)
  resid <- stats::rnorm(20L)
  W <- morie:::.lan_gp_placeholder_W(stats::runif(20L), k = 3L)
  I <- morie:::.lan_gp_morans_i(resid, W)
  expect_true(is.numeric(I) && is.finite(I))
})

test_that(".lan_gp_result builds a morie_laniyonu_gp_result list", {
  out <- morie:::.lan_gp_result(
    year = 2020L, n_tracts = 100L,
    rho = 0.3, moran_i_ols = 0.15,
    decompositions = list(),
    gent_distribution = c(eligible_no_change = 30L,
                          gentrified = 20L))
  expect_type(out, "list")
  expect_s3_class(out, "morie_laniyonu_gp_result")
  expect_s3_class(out, "morie_rich_result")
})

# =============================================== laniyonu_actuarial_risk_disparity.R

test_that(".lan_ard_result is a list builder", {
  out <- morie:::.lan_ard_result(title = "T", call = "demo",
                                   interpretation = "x")
  expect_type(out, "list")
})

test_that(".lan_ord_levels_to_int maps levels to 1..K", {
  out <- morie:::.lan_ord_levels_to_int(
    c("low", "high", "med"),
    levels_ = c("low", "med", "high"))
  expect_equal(out, c(1L, 3L, 2L))
})

# =============================================================== ingest_forensics.R

test_that(".morie_forensics_require_fbi_key returns explicit api_key arg", {
  expect_equal(
    morie:::.morie_forensics_require_fbi_key(api_key = "abc-123"),
    "abc-123")
})

test_that(".morie_forensics_require_fbi_key errors when no key supplied", {
  old <- Sys.getenv("MORIE_FBI_CDE_API_KEY", unset = NA)
  Sys.unsetenv("MORIE_FBI_CDE_API_KEY")
  on.exit(
    if (!is.na(old)) Sys.setenv(MORIE_FBI_CDE_API_KEY = old),
    add = TRUE)
  expect_error(
    morie:::.morie_forensics_require_fbi_key(NULL),
    regexp = "API key")
})

test_that(".morie_forensics_flatten_nibrs flattens nested rec to list", {
  rec <- list(case = "C-001",
              location = list(state = "CA", city = "LA"),
              charges = list("assault", "theft"))
  out <- morie:::.morie_forensics_flatten_nibrs(rec)
  expect_type(out, "list")
  expect_equal(out$case, "C-001")
  expect_equal(out$location.state, "CA")
  expect_equal(out$location.city, "LA")
  expect_equal(out$charges, "assault;theft")
})

test_that(".morie_forensics_rows_to_df returns 0-row frame on empty input", {
  out <- morie:::.morie_forensics_rows_to_df(list())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that(".morie_forensics_flatten_nist flattens keyword + publisher", {
  rec <- list(keyword = c("crime", "stats"),
              publisher = list(name = "FBI"),
              license = "CC0")
  out <- morie:::.morie_forensics_flatten_nist(rec)
  expect_type(out, "list")
  # Keyword joined.
  expect_match(out$keyword, "crime")
})

# ================================================================ sensitivity.R

test_that(".ovb_result builds an OVB result with the canonical fields", {
  out <- morie:::.ovb_result(
    estimate = 0.5, se = 0.1, rv_q = 0.15, rv_qa = 0.12,
    partial_r2_treatment = 0.05,
    benchmark_bounds = list(lower = 0.4, upper = 0.6),
    interpretation = "Estimate is robust")
  expect_type(out, "list")
  expect_s3_class(out, "morie_ovb")
})

# ======================================================================= arsau.R

test_that(".morie_env returns env var trimmed", {
  Sys.setenv(MORIE_TEST_ENV = "  hello  ")
  on.exit(Sys.unsetenv("MORIE_TEST_ENV"), add = TRUE)
  expect_equal(morie:::.morie_env("MORIE_TEST_ENV"), "hello")
})

test_that(".morie_env returns NULL on unset / whitespace-only", {
  Sys.unsetenv("MORIE_TEST_UNSET")
  expect_null(morie:::.morie_env("MORIE_TEST_UNSET"))
  Sys.setenv(MORIE_TEST_WS = "   ")
  on.exit(Sys.unsetenv("MORIE_TEST_WS"), add = TRUE)
  expect_null(morie:::.morie_env("MORIE_TEST_WS"))
})
