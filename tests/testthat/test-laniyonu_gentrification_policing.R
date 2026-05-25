# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/laniyonu_gentrification_policing.R

set.seed(2026L)

mk_gp_data <- function(n_tracts = 40L, years = c(2010L, 2011L, 2012L)) {
  tracts <- sprintf("T%03d", seq_len(n_tracts))
  grid <- expand.grid(tract_id = tracts, year = years, stringsAsFactors = FALSE)
  # Per-tract baseline + follow values
  base_inc <- setNames(runif(n_tracts, 30000, 70000), tracts)
  base_rent <- setNames(runif(n_tracts, 700, 1500), tracts)
  base_coll <- setNames(runif(n_tracts, 0.05, 0.45), tracts)
  follow_inc <- base_inc * runif(n_tracts, 0.95, 1.4)
  follow_rent <- base_rent * runif(n_tracts, 0.9, 1.5)
  follow_coll <- pmin(base_coll * runif(n_tracts, 0.95, 1.5), 0.95)

  grid$median_inc_2000 <- base_inc[grid$tract_id]
  grid$median_inc_2014 <- follow_inc[grid$tract_id]
  grid$median_rent_2000 <- base_rent[grid$tract_id]
  grid$median_rent_2014 <- follow_rent[grid$tract_id]
  grid$pct_ba_2000 <- base_coll[grid$tract_id]
  grid$pct_ba_2014 <- follow_coll[grid$tract_id]
  grid$population <- sample(800:5000, nrow(grid), replace = TRUE)
  grid$stops <- rpois(nrow(grid), lambda = 30)
  grid$felony_count <- rpois(nrow(grid), lambda = 10)
  grid$calls_311_omp <- rpois(nrow(grid), lambda = 40)
  grid$pct_black <- runif(nrow(grid), 0.05, 0.7)
  grid
}

test_that("morie_laniyonu_gentrification_policing runs across years (lite mode)", {
  df <- mk_gp_data()
  res <- suppressWarnings(
    morie_laniyonu_gentrification_policing(df = df, log_outcome = TRUE)
  )
  expect_true(is.list(res))
  expect_true(length(res) >= 1L)
  expect_s3_class(res[[1]], "morie_laniyonu_gp_result")
  expect_true(is.numeric(res[[1]]$rho))
})

test_that("morie_laniyonu_gentrification_policing with additional_controls", {
  df <- mk_gp_data()
  res <- suppressWarnings(
    morie_laniyonu_gentrification_policing(
      df = df, additional_controls = c("pct_black"),
      log_outcome = FALSE
    )
  )
  expect_true(length(res) >= 1L)
})

test_that("morie_laniyonu_gentrification_policing with fitted SDM params", {
  df <- mk_gp_data(n_tracts = 25L, years = c(2010L))
  # Need to count how many design columns to size betas:
  # gentrification has 2 dummies + crime_col + demand_col = 4
  res <- suppressWarnings(
    morie_laniyonu_gentrification_policing(
      df = df, years = c(2010L),
      fitted_rho = 0.3,
      fitted_beta_direct = rep(0.1, 4L),
      fitted_beta_spatial = rep(0.05, 4L)
    )
  )
  expect_true(length(res) >= 1L)
  expect_equal(res[[1]]$rho, 0.3)
})

test_that("morie_laniyonu_gentrification_policing with named fitted lists", {
  df <- mk_gp_data(n_tracts = 25L, years = c(2010L, 2011L))
  res <- suppressWarnings(
    morie_laniyonu_gentrification_policing(
      df = df, years = c(2010L, 2011L),
      fitted_rho = list(`2010` = 0.2, `2011` = 0.3),
      fitted_beta_direct = list(`2010` = rep(0.1, 4L),
                                `2011` = rep(0.05, 4L)),
      fitted_beta_spatial = list(`2010` = rep(0.02, 4L),
                                  `2011` = rep(0.03, 4L))
    )
  )
  expect_equal(length(res), 2L)
})

test_that("morie_laniyonu_gentrification_policing with weight_matrix passes through", {
  df <- mk_gp_data(n_tracts = 25L, years = c(2010L))
  tracts <- unique(df$tract_id)
  n <- length(tracts)
  W <- matrix(1 / (n - 1), n, n)
  diag(W) <- 0
  res <- suppressWarnings(
    morie_laniyonu_gentrification_policing(
      df = df, years = c(2010L),
      weight_matrix = W, weight_matrix_kind = "knn"
    )
  )
  expect_true(length(res) >= 1L)
})

test_that("morie_laniyonu_gentrification_policing warns + skips small years", {
  df <- mk_gp_data(n_tracts = 5L, years = c(2010L))
  expect_warning(
    morie_laniyonu_gentrification_policing(df = df)
  )
})

test_that("print.morie_laniyonu_gp_result emits header", {
  df <- mk_gp_data(n_tracts = 25L, years = c(2010L))
  res <- suppressWarnings(
    morie_laniyonu_gentrification_policing(df = df, years = c(2010L))
  )
  out <- capture.output(print(res[[1]]))
  expect_true(any(nchar(out) > 0))
})
