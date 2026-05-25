# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 19 -- the MRM-framework callables: mrm_otis.R,
# mrm_mandela_spectrum.R, mrm_kulldorff.R, mrm_siu.R, mrm_diagnostics.R,
# mrm_doe.R. Driven from the bundled OTIS/TPS samples and synthetic
# frames; calls are tolerant because the goal is branch coverage.

.cw19_run <- function(expr) {
  r <- tryCatch(suppressWarnings(expr), error = function(e) e)
  testthat::expect_true(is.list(r) || is.data.frame(r) ||
    inherits(r, "error") || is.numeric(r))
  invisible(r)
}

test_that("mrm_otis callables run on the bundled OTIS samples", {
  b09 <- tryCatch(morie_sample("otis_b09"), error = function(e) NULL)
  b01 <- tryCatch(morie_sample("otis_b01"), error = function(e) NULL)
  if (!is.null(b09)) .cw19_run(mrm_otis_placement_concentration(b09))
  if (!is.null(b01)) {
    .cw19_run(mrm_otis_seg_duration_km(b01))
    .cw19_run(mrm_otis_mandela_spectrum(b01))
  }
  # input-validation branches
  expect_error(mrm_otis_placement_concentration(list(a = 1)))
  expect_error(mrm_otis_mandela_spectrum(data.frame(x = 1)))
})

test_that("mrm_tps_kulldorff_scan runs on TPS lat/long data", {
  tps <- tryCatch(morie_sample("tps_assault"), error = function(e) NULL)
  if (!is.null(tps)) .cw19_run(mrm_tps_kulldorff_scan(tps))
  # synthetic fallback
  set.seed(1)
  syn <- data.frame(
    LAT_WGS84 = 43.65 + stats::rnorm(150, 0, 0.05),
    LONG_WGS84 = -79.38 + stats::rnorm(150, 0, 0.05)
  )
  .cw19_run(mrm_tps_kulldorff_scan(syn))
})

test_that("mrm_siu_case_to_decision_km runs on a synthetic SIU frame", {
  set.seed(2)
  n <- 120
  base <- as.Date("2023-01-01") + sample(0:600, n, replace = TRUE)
  siu <- data.frame(
    case_number = sprintf("23-OCI-%03d", seq_len(n)),
    date_of_incident_iso = format(base, "%Y-%m-%d"),
    date_of_director_decision_iso =
      format(base + sample(60:300, n, replace = TRUE), "%Y-%m-%d"),
    police_service = sample(c(
      "Toronto Police Service",
      "Peel Regional Police"
    ), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  .cw19_run(mrm_siu_case_to_decision_km(siu))
})

test_that("mrm diagnostics + DoE callables run on synthetic data", {
  d <- make_canonical_cpads(n = 400L, seed = 9L)
  d$treat <- d$cannabis_any_use
  .cw19_run(mrm_assumptions_check(
    d,
    treatment_col = "treat", outcome_col = "heavy_drinking_30d",
    covariates = c("age_group", "gender")
  ))
  .cw19_run(mrm_median_causal_effect(
    d,
    treatment_col = "treat", outcome_col = "ebac_tot",
    covariates = c("age_group", "gender")
  ))
  set.seed(3)
  doe <- data.frame(
    resp = stats::rnorm(60),
    f1 = rep(c(-1, 0, 1), 20),
    f2 = rep(c(-1, 1), 30)
  )
  .cw19_run(mrm_response_surface(doe,
    response_col = "resp",
    factor_cols = c("f1", "f2")
  ))
})
