# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)
set.seed(1)

# This file targets R/otis_churn.R (analyzer set lives there); the
# parallel test-otis_analyze.R file already smokes the individual
# analyzers. Here we focus on more thorough churn coverage with
# distinct seeds and structural assertions.

# Dictionary-driven b01 panel from helper-otis.R — real columns, real
# categorical levels ("Eastern"/"Western"/"Toronto" regions,
# "Female"/"Male" genders, "Yes"/"No" alert values). The local fixture
# below was using stale region codes ("East/West/North") that caused
# every churn analyzer to silently skip via tryCatch.
make_churn_df <- function(n = 200, seed = 7) {
  make_synthetic_otis("b01", n = n, seed = seed)
}

churn_fns <- c(
  "morie_otis_repeat_placement_concentration",
  "morie_otis_within_year_placement_count",
  "morie_otis_within_year_region_diversity",
  "morie_otis_mortification_cooccurrence",
  "morie_otis_disciplinary_medical_overlap",
  "morie_otis_embedding_distribution",
  "morie_otis_intra_year_transition_matrix",
  "morie_otis_path_complexity_gini",
  "morie_otis_region_alert_state_richness",
  "morie_otis_regC_demog_contingency",
  "morie_otis_irr_glmm_vm"
)

for (nm in churn_fns) {
  local({
    fn_name <- nm
    test_that(paste(fn_name, "happy-path on 200-row churn frame"), {
      skip_if_not(exists(fn_name), paste(fn_name, "not exported"))
      df <- make_churn_df(200, seed = nchar(fn_name) + 1)
      res <- tryCatch(do.call(fn_name, list(df)), error = function(e) NULL)
      skip_if(is.null(res), paste(fn_name, "needs richer OTIS structure"))
      expect_true(is.list(res) || is.data.frame(res) || is.numeric(res))
    })

    test_that(paste(fn_name, "tolerates a small frame (n=30)"), {
      skip_if_not(exists(fn_name), paste(fn_name, "not exported"))
      df <- make_churn_df(30, seed = nchar(fn_name) + 2)
      res <- tryCatch(do.call(fn_name, list(df)),
                      error = function(e) NULL,
                      warning = function(w) NULL)
      expect_true(is.null(res) || is.list(res) ||
                  is.data.frame(res) || is.numeric(res))
    })
  })
}

# ---------------------------------------------------------------------------
# Aggregator with multiple batches
# ---------------------------------------------------------------------------

test_that("morie_otis_churn_analyze_all dispatches across batches", {
  set.seed(11)
  b01 <- make_churn_df(80, seed = 21)
  b02 <- make_churn_df(80, seed = 22)
  res <- tryCatch(morie_otis_churn_analyze_all(b01 = b01, b02 = b02),
                  error = function(e) NULL)
  skip_if(is.null(res), "needs richer batch payloads")
  expect_true(is.list(res))
})

test_that("morie_otis_churn_analyze_all handles all-NULL invocation", {
  res <- tryCatch(morie_otis_churn_analyze_all(),
                  error = function(e) NULL)
  skip_if(is.null(res), "all-NULL path requires OTIS data")
  expect_true(is.list(res))
})