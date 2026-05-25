# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Synthetic DGP that mimics the OTIS (T, Y, X) shape
# ---------------------------------------------------------------------------

set.seed(1)

make_otis_df <- function(n = 400, tau = 0.4, seed = 1) {
  set.seed(seed)
  x <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.5 * x))
  y <- tau * d + 0.7 * x + rnorm(n, sd = 0.5)
  data.frame(d = d, y = y, x = x,
             clust = sample.int(20, n, replace = TRUE))
}

# Minimal placement-level frame so the OTIS pair-builders can run.
make_otis_placement_df <- function(n_persons = 40, seed = 1) {
  set.seed(seed)
  rows <- list()
  for (i in seq_len(n_persons)) {
    n_rows <- sample(1:3, 1)
    for (k in seq_len(n_rows)) {
      rows[[length(rows) + 1L]] <- data.frame(
        UniqueIndividual_ID = sprintf("id%04d", i),
        EndFiscalYear = sample(2018:2022, 1),
        Gender = sample(c("M", "F"), 1),
        Age_Category = sample(c("18-24", "25-34", "35-44"), 1),
        Region_AtTimeOfPlacement = sample(c("Central", "East", "West"), 1),
        Region_MostRecentPlacement = sample(c("Central", "East", "West"), 1),
        MentalHealth_Alert = sample(0:1, 1, prob = c(0.6, 0.4)),
        SuicideRisk_Alert = sample(0:1, 1, prob = c(0.7, 0.3)),
        SuicideWatch_Alert = sample(0:1, 1, prob = c(0.85, 0.15)),
        Number_Of_Placements = sample(1:5, 1),
        NumberConsecutiveDays_Segregation = sample(0:25, 1),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# morie_otis_ipw_ate
# ---------------------------------------------------------------------------

test_that("morie_otis_ipw_ate returns expected fields", {
  df <- make_otis_df(n = 400, tau = 0.4)
  res <- morie_otis_ipw_ate(df, treatment = "d", outcome = "y",
                            covariates = "x")
  expect_s3_class(res, "morie_causal_estimate")
  expect_identical(res$estimator, "IPW")
  expect_true(all(c("ate", "ate_se", "ate_pval", "ate_ci95",
                    "n", "n_treated", "p_treat", "notes") %in%
                    names(res)))
  expect_length(res$ate_ci95, 2L)
  expect_true(is.finite(res$ate) && is.finite(res$ate_se))
  expect_equal(res$n, 400L)
})

test_that("morie_otis_ipw_ate recovers tau = 0.4 on simple DGP", {
  df <- make_otis_df(n = 1000, tau = 0.4, seed = 2)
  res <- morie_otis_ipw_ate(df, "d", "y", "x")
  expect_equal(res$ate, 0.4, tolerance = 0.15)
})

test_that("morie_otis_ipw_ate drops rows with NA covariate", {
  df <- make_otis_df(n = 200)
  df$x[1:10] <- NA
  res <- morie_otis_ipw_ate(df, "d", "y", "x")
  expect_equal(res$n, 190L)
})


# ---------------------------------------------------------------------------
# morie_otis_aipw_ate
# ---------------------------------------------------------------------------

test_that("morie_otis_aipw_ate returns expected fields", {
  df <- make_otis_df(n = 400)
  res <- morie_otis_aipw_ate(df, "d", "y", "x", n_folds = 3L)
  expect_s3_class(res, "morie_causal_estimate")
  expect_identical(res$estimator, "AIPW")
  expect_true(is.finite(res$ate))
  expect_true(is.finite(res$ate_se))
})

test_that("morie_otis_aipw_ate recovers tau on simple DGP", {
  df <- make_otis_df(n = 1000, tau = 0.4, seed = 3)
  res <- morie_otis_aipw_ate(df, "d", "y", "x", n_folds = 3L, seed = 3)
  expect_equal(res$ate, 0.4, tolerance = 0.15)
})


# ---------------------------------------------------------------------------
# morie_otis_irm_dml
# ---------------------------------------------------------------------------

test_that("morie_otis_irm_dml returns ATE / ATTE / ATC fields", {
  df <- make_otis_df(n = 400)
  res <- morie_otis_irm_dml(df, "d", "y", "x", n_folds = 3L)
  expect_true(all(c("ate", "ate_se", "ate_pval", "ate_ci95",
                    "atte", "atte_se", "atc", "atc_se",
                    "n", "n_treated", "p_treat", "se_kind") %in%
                    names(res)))
  expect_true(is.finite(res$ate))
  expect_identical(res$se_kind, "iid")
})

test_that("morie_otis_irm_dml supports one-way cluster SE", {
  df <- make_otis_df(n = 400)
  res <- morie_otis_irm_dml(df, "d", "y", "x", n_folds = 3L,
                            cluster_cols = "clust")
  expect_true(grepl("cluster:clust", res$se_kind, fixed = TRUE))
  expect_true(is.finite(res$ate_se))
})


# ---------------------------------------------------------------------------
# morie_otis_classify_mandela_combo
# ---------------------------------------------------------------------------

test_that("morie_otis_classify_mandela_combo encodes 8 alert states", {
  # no alerts -> a8 / compliant
  r0 <- morie_otis_classify_mandela_combo(0, 0, 0)
  expect_equal(r0$combo, 0L)
  expect_equal(r0$combo_label, "a8")
  expect_equal(r0$alert_count, 0L)
  expect_equal(r0$mandela_category, "compliant")

  # MH only -> a1
  r1 <- morie_otis_classify_mandela_combo(1, 0, 0)
  expect_equal(r1$combo, 4L)
  expect_equal(r1$combo_label, "a1")
  expect_equal(r1$alert_count, 1L)

  # all three -> a7
  r7 <- morie_otis_classify_mandela_combo(1, 1, 1)
  expect_equal(r7$combo, 7L)
  expect_equal(r7$combo_label, "a7")
})

test_that("morie_otis_classify_mandela_combo days > 15 = torture", {
  r <- morie_otis_classify_mandela_combo(0, 0, 0, days = 20,
                                          hours_per_day = 22)
  expect_true(r$mandela_category %in% c("torture",
                                          "prolonged_solitary"))
})

test_that("morie_otis_classify_mandela_combo days = 0 = compliant", {
  r <- morie_otis_classify_mandela_combo(0, 0, 0, days = 0)
  expect_equal(r$mandela_category, "compliant")
})


# ---------------------------------------------------------------------------
# morie_otis_make_pair_alert_to_volatility_*
# ---------------------------------------------------------------------------

test_that("morie_otis_make_pair_alert_to_volatility_ruhela returns pair list", {
  df <- make_otis_placement_df(n_persons = 30)
  pr <- morie_otis_make_pair_alert_to_volatility_ruhela(df)
  expect_true(all(c("data", "T", "Y", "covariates") %in% names(pr)))
  expect_identical(pr$T, "T_high_ac")
  expect_identical(pr$Y, "Y_vm_count")
  expect_true(pr$T %in% colnames(pr$data))
  expect_true(pr$Y %in% colnames(pr$data))
})

test_that("morie_otis_make_pair_alert_to_volatility_naive returns binary outcome", {
  df <- make_otis_placement_df(n_persons = 30)
  pr <- morie_otis_make_pair_alert_to_volatility_naive(df)
  expect_identical(pr$T, "T_high_ac")
  expect_identical(pr$Y, "Y_vm_any")
  expect_true(all(pr$data[[pr$Y]] %in% c(0L, 1L)))
})

test_that("morie_otis_make_pair_alert_to_volatility_all returns both formulations", {
  df <- make_otis_placement_df(n_persons = 30)
  res <- morie_otis_make_pair_alert_to_volatility_all(df)
  expect_true(all(c("ruhela", "naive") %in% names(res)))
})


# ---------------------------------------------------------------------------
# morie_otis_make_pair_a / b / c
# ---------------------------------------------------------------------------

test_that("morie_otis_make_pair_a returns binary T_a and Y_a", {
  df <- make_otis_placement_df(n_persons = 30)
  pr <- morie_otis_make_pair_a(df)
  expect_identical(pr$T, "T_a")
  expect_identical(pr$Y, "Y_a")
  expect_true(all(pr$data$T_a %in% c(0L, 1L)))
  expect_true(all(pr$data$Y_a %in% c(0L, 1L)))
})

test_that("morie_otis_make_pair_b returns binary T_b and Y_b", {
  df <- make_otis_placement_df(n_persons = 30)
  pr <- morie_otis_make_pair_b(df)
  expect_identical(pr$T, "T_b")
  expect_identical(pr$Y, "Y_b")
  expect_true(all(pr$data$T_b %in% c(0L, 1L)))
})

test_that("morie_otis_make_pair_c returns T_c binary and Y_c numeric winsorised", {
  df <- make_otis_placement_df(n_persons = 30)
  pr <- morie_otis_make_pair_c(df)
  expect_identical(pr$T, "T_c")
  expect_identical(pr$Y, "Y_c")
  expect_true(all(pr$data$T_c %in% c(0L, 1L)))
  expect_true(is.numeric(pr$data$Y_c))
  expect_true(min(pr$data$Y_c, na.rm = TRUE) >= 0)
})
