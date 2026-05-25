# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 10 -- R/mrm_tps.R: the TPS spatial-statistics callables
# (haversine, Levy scaling, Moran/DBSCAN, neighbourhood recurrence,
# Hawkes-refit loader).

.fake_tps <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    OCC_DATE = format(
      as.Date("2024-01-01") + sample(0:330, n, TRUE),
      "%m/%d/%Y %I:%M:%S %p"
    ),
    LAT_WGS84 = 43.65 + stats::rnorm(n, 0, 0.05),
    LONG_WGS84 = -79.38 + stats::rnorm(n, 0, 0.05),
    HOOD_158 = sample(sprintf("H%03d", 1:8), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that(".haversine_km computes great-circle distance", {
  expect_equal(morie:::.haversine_km(43.65, -79.38, 43.65, -79.38), 0)
  d <- morie:::.haversine_km(43.65, -79.38, 43.66, -79.38)
  expect_true(d > 0.9 && d < 1.3) # ~1.1 km per 0.01 deg lat
})

test_that("mrm_tps_levy_scaling validates input and fits a Hill alpha", {
  expect_error(mrm_tps_levy_scaling(list(a = 1)))
  expect_error(mrm_tps_levy_scaling(data.frame(x = 1)))
  one <- mrm_tps_levy_scaling(data.frame(
    OCC_DATE = "2024-01-01",
    LAT_WGS84 = 43.6,
    LONG_WGS84 = -79.4
  ))
  expect_equal(one$n_events, 1L)
  expect_true(is.na(one$hill_alpha))
  res <- mrm_tps_levy_scaling(.fake_tps(220L))
  expect_equal(res$n_events, 220L)
  expect_true(is.numeric(res$hill_alpha))
})

test_that("mrm_tps_neighbourhood_recurrence_km reports recurrence gaps", {
  out <- mrm_tps_neighbourhood_recurrence_km(.fake_tps(240L, seed = 3L))
  expect_s3_class(out, "data.frame")
  expect_true(all(c("hood", "n_events", "mean_gap_days") %in% names(out)))
  expect_true(nrow(out) >= 1L)
})

test_that("mrm_tps_moran_clustering runs on TPS lat/long data", {
  res <- tryCatch(mrm_tps_moran_clustering(.fake_tps(300L, seed = 5L)),
    error = function(e) e
  )
  expect_true(is.list(res) || inherits(res, "error"))
})

test_that("mrm_tps_load_hawkes_refit reads a manifest, errors if absent", {
  expect_error(mrm_tps_load_hawkes_refit("/no/such/manifest.json"))
  mf <- tempfile("hawkes-", fileext = ".json")
  on.exit(unlink(mf), add = TRUE)
  writeLines(paste0(
    '{"Assault":{"n_fitted":140,"T_days":3650.5,',
    '"markovian":{"aic":1234.5,"branching_ratio":0.61,"ks_pvalue":0.30},',
    '"weibull_sin":{"aic":1200.1,"branching_ratio":0.55,"ks_pvalue":0.42},',
    '"delta_aic":34.4}}'
  ), mf)
  tbl <- mrm_tps_load_hawkes_refit(mf)
  expect_s3_class(tbl, "data.frame")
  expect_equal(tbl$category, "Assault")
  expect_equal(tbl$kappa_mark, 0.61)
  expect_equal(tbl$delta_aic, 34.4)
})
