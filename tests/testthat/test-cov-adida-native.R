# Coverage tests for R/adida_native.R (Nikolopoulos et al. 2011):
# bucket aggregation, disaggregation, the TSB base forecast, ADIDA and
# temporal combination across levels.

ad_y <- c(0, 3, 0, 0, 5, 0, 2, 0, 0, 0, 4, 1, 0, 0, 6)

tsb <- function(y, a = 0.1, b = 0.05) {
  pos <- y[y > 0]
  z <- if (length(pos)) pos[1] else 0
  p <- length(pos) / length(y)
  for (v in y) {
    if (v > 0) {
      z <- (1 - a) * z + a * v
      p <- (1 - b) * p + b
    } else {
      p <- (1 - b) * p
    }
  }
  z * p
}

test_that("zero fraction, buckets and disaggregation", {
  expect_equal(zero_fraction(ad_y), 9 / 15)
  expect_error(zero_fraction(numeric(0)), "empty series")
  # non-overlapping buckets drop the oldest remainder
  expect_equal(aggregate_buckets(ad_y, 4), colSums(matrix(ad_y[4:15], 4)))
  expect_equal(aggregate_buckets(ad_y, 3, overlapping = TRUE), ad_y[1:13] + ad_y[2:14] + ad_y[3:15])
  expect_error(aggregate_buckets(ad_y, 0), "at least 1")
  expect_error(aggregate_buckets(ad_y, 16), "exceeds the 15 observations")
  expect_equal(disaggregate(6, 3), c(2, 2, 2))
  expect_equal(disaggregate(6, 3, c(1, 2, 3)), c(1, 2, 3))
  expect_error(disaggregate(6, 0), "at least 1")
  expect_error(disaggregate(6, 3, 1:2), "2 weights for a bucket of 3")
  expect_error(disaggregate(6, 2, c(-1, 2)), "non-negative")
  expect_error(disaggregate(6, 2, c(0, 0)), "sums to zero")
})

test_that("TSB base forecast and the ADIDA pipeline", {
  expect_equal(intermittent_forecast(ad_y)$forecast, tsb(ad_y), tolerance = 1e-12)
  expect_equal(intermittent_forecast(ad_y, alpha = 0.3, beta = 0.2)$forecast, tsb(ad_y, 0.3, 0.2), tolerance = 1e-12)
  expect_equal(intermittent_forecast(c(0, 0, 0))$forecast, 0)
  expect_error(intermittent_forecast(ad_y, method = "croston"), "not implemented")
  expect_error(intermittent_forecast(ad_y, horizon = 2), "horizon = 1")
  r <- morie_adida(ad_y, 3, horizon = 5)
  agg <- aggregate_buckets(ad_y, 3)
  fa <- tsb(agg)
  expect_equal(r$aggregate_forecast, fa, tolerance = 1e-12)
  expect_equal(r$forecast, rep(fa / 3, 5), tolerance = 1e-12)
  expect_equal(r$zero_fraction_aggregated, mean(agg <= 0))
  expect_true(r$disaggregation_sums_back)
  expect_null(r$lead_time_demand)
  p <- morie_adida(ad_y, 3, horizon = 4, profile = c(1, 1, 2))
  expect_equal(p$forecast, fa * c(0.25, 0.25, 0.5, 0.25), tolerance = 1e-12)
  lt <- morie_adida(ad_y, 99, lead_time = 5)
  expect_equal(lt$m, 5L)
  expect_equal(lt$lead_time_demand, tsb(aggregate_buckets(ad_y, 5)), tolerance = 1e-12)
  o <- morie_adida(ad_y, 3, overlapping = TRUE)
  expect_equal(o$n_buckets, 13L)
  expect_error(morie_adida(ad_y, 8), "leaves only 1 aggregated points")
  expect_identical(adida, morie_adida)
  expect_identical(adidaforecast, morie_adida)
})

test_that("temporal combination averages the level forecasts", {
  r <- temporal_combination(ad_y, c(1, 3, 5), horizon = 3)
  per <- lapply(c(1, 3, 5), function(m) morie_adida(ad_y, m, horizon = 3)$forecast)
  expect_equal(r$per_level, per)
  expect_equal(r$forecast, (per[[1]] + per[[2]] + per[[3]]) / 3, tolerance = 1e-12)
  expect_equal(r$spread, diff(range(vapply(per, `[`, 0, 1))))
  w <- temporal_combination(ad_y, c(1, 3), weights = c(1, 3))
  expect_equal(w$weights, c(0.25, 0.75))
  expect_equal(w$forecast, 0.25 * per[[1]][1] + 0.75 * per[[2]][1], tolerance = 1e-12)
  expect_error(temporal_combination(ad_y, 3), "at least 2 levels")
  expect_error(temporal_combination(ad_y, c(1, 3), weights = 1), "1 weights for 2 levels")
  expect_error(temporal_combination(ad_y, c(1, 3), weights = c(0, 0)), "sum to zero")
  expect_match(.adida_cheatsheet(), "LEAD TIME")
})
