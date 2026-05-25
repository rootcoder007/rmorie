# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch C: quntf, mrm_siu, rglyp, ghcon, ghsve.

# ==== quntf.R ====
test_that("quntf returns full structure on normal data with default taus", {
  set.seed(1)
  x <- rnorm(200)
  out <- morie:::quntf(x)
  expect_type(out, "list")
  expect_named(out, c("taus", "quantiles", "se", "bandwidth", "estimate", "n", "method"))
  expect_length(out$taus, 5L)
  expect_length(out$quantiles, 5L)
  expect_gt(out$bandwidth, 0)
  expect_equal(out$n, 200L)
})

test_that("quntf returns the n<2 short-circuit list", {
  set.seed(1)
  out <- morie:::quntf(numeric(0))
  expect_true(is.na(out$estimate))
  expect_equal(out$n, 0L)
  expect_match(out$method, "n<2")
  out1 <- morie:::quntf(3.14)
  expect_equal(out1$n, 1L)
  expect_match(out1$method, "n<2")
})

test_that("quntf handles zero-IQR (constant or near-constant) input", {
  set.seed(1)
  x <- rep(2.5, 50)
  out <- morie:::quntf(x, taus = c(0.25, 0.5, 0.75))
  expect_length(out$quantiles, 3L)
  expect_true(all(out$quantiles == 2.5))
})

# ==== mrm_siu.R ====
.make_siu_df <- function(n = 60, with_decision = TRUE, censor_some = TRUE) {
  set.seed(1)
  services <- rep(
    c(
      "Toronto Police Service", "Ottawa Police Service",
      "Ontario Provincial Police", "Peel Regional Police"
    ),
    length.out = n
  )
  inc <- as.Date("2022-01-01") + sample.int(800, n, replace = TRUE)
  gap <- sample(30:400, n, replace = TRUE)
  dec <- inc + gap
  if (censor_some) {
    dec[seq_len(round(n * 0.15))] <- NA
  }
  if (!with_decision) dec <- rep(NA, n)
  data.frame(
    case_number = sprintf("OCC-%05d", seq_len(n)),
    date_of_incident_iso = format(inc, "%Y-%m-%d"),
    date_of_director_decision_iso = ifelse(is.na(dec), NA_character_,
      format(dec, "%Y-%m-%d")
    ),
    police_service = services,
    director_decision_category = sample(
      c("charges_laid", "no_charges", "no_jurisdiction"),
      n,
      replace = TRUE
    ),
    reason_for_interaction = sample(c("vehicle", "firearm", "custody"),
      n,
      replace = TRUE
    ),
    stringsAsFactors = FALSE
  )
}

test_that("mrm_siu_case_to_decision_km returns pooled + by_service tables", {
  set.seed(1)
  siu <- .make_siu_df()
  res <- mrm_siu_case_to_decision_km(siu, min_n = 2L)
  expect_named(res, c("pooled", "by_service"))
  expect_s3_class(res$pooled, "data.frame")
  expect_equal(res$pooled$stratum, "pooled")
  expect_gt(res$pooled$n, 0L)
})

test_that("mrm_siu_per_service_rate tabulates service x year cases", {
  set.seed(1)
  siu <- .make_siu_df()
  out <- mrm_siu_per_service_rate(siu)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("service", "year", "n_cases"))
  expect_true(all(out$n_cases > 0))
})

test_that("mrm_siu_outcome_classifier returns counts and shares", {
  set.seed(1)
  siu <- .make_siu_df()
  out <- mrm_siu_outcome_classifier(siu)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("service", "outcome", "n_cases", "share_within_service"))
  expect_true(all(out$share_within_service >= 0 &
    out$share_within_service <= 1))
})

# ==== rglyp.R ====
test_that("rglyp returns lyapunov + divergence_curve for a Gaussian series", {
  set.seed(1)
  out <- rglyp(rnorm(300), m = 3L, tau = 1L, max_t = 20L)
  expect_named(out, c("lyapunov", "divergence_curve", "t"))
  expect_length(out$divergence_curve, 20L)
  expect_length(out$t, 20L)
  expect_true(is.numeric(out$lyapunov))
})

test_that("rglyp errors when series is too short to embed", {
  set.seed(1)
  expect_error(rglyp(rnorm(5), m = 3L, tau = 1L),
    "too short",
    ignore.case = TRUE
  )
})

# ==== ghcon.R ====
test_that("morie_ghosal_posterior_consistency returns full structure on Gaussian data", {
  set.seed(1)
  out <- morie_ghosal_posterior_consistency(rnorm(80), K = 50, seed = 1)
  expect_named(out, c(
    "estimate", "ks_mean", "ks_se", "schwartz_bound",
    "n", "eps", "method"
  ))
  expect_gte(out$estimate, 0)
  expect_lte(out$estimate, 1)
  expect_gt(out$ks_mean, 0)
  expect_gte(out$ks_se, 0)
  expect_equal(out$n, 80L)
})

test_that("morie_ghosal_posterior_consistency short-circuits on empty input", {
  out <- morie_ghosal_posterior_consistency(numeric(0))
  expect_true(is.na(out$estimate))
  expect_equal(out$n, 0L)
})

# ==== ghsve.R ====
test_that("morie_ghosal_sieve_prior returns a Bernstein-sieve fit on Gaussian data", {
  set.seed(1)
  out <- morie_ghosal_sieve_prior(rnorm(80))
  expect_named(out, c("estimate", "log_lik_per_obs", "weights", "K", "n", "method"))
  expect_true(is.finite(out$estimate))
  expect_true(is.finite(out$log_lik_per_obs))
  expect_gte(out$K, 2L)
  expect_length(out$weights, out$K)
  expect_equal(sum(out$weights), 1, tolerance = 1e-6)
  expect_equal(out$n, 80L)
})

test_that("morie_ghosal_sieve_prior short-circuits when n < 3", {
  set.seed(1)
  out <- morie_ghosal_sieve_prior(c(1.1, 2.2))
  expect_true(is.na(out$estimate))
  expect_equal(out$n, 2L)
  expect_match(out$method, "n<3")
})

test_that("morie_ghosal_sieve_prior honours an explicit K", {
  set.seed(1)
  out <- morie_ghosal_sieve_prior(rnorm(60), K = 6L)
  expect_equal(out$K, 6L)
  expect_length(out$weights, 6L)
})
