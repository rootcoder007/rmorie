# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 25 -- five surgical targets:
#   * R/entheo_analysis.R -- the EEG/fMRI envelope/align/binding/SAN
#     internal helpers (matrix paths + the down-binning branch).
#   * R/hawkes_fit.R      -- the weibull/lomax/gamma kernel families,
#     the pure-R negative-log-likelihood, the full fitter and printer.
#   * R/mrm_otis.R        -- placement-concentration "Greater than" band
#     and the empty-year / empty-stratum returns.
#   * R/frns_predpol.R    -- non-finite-row drop warnings + calibration
#     and score-disparity interpretation branches.
#   * R/dccmd.R           -- morie_dcc_multivariate_garch end to end.

# ---- entheo_analysis.R ----------------------------------------------------

test_that("entheo EEG/fMRI helpers run on matrix and vector inputs", {
  set.seed(1)
  eeg <- matrix(rnorm(3 * 48), nrow = 3)
  fmri <- matrix(rnorm(3 * 12), nrow = 3) # fewer fMRI frames
  env <- morie:::.entheo_envelope(eeg)
  expect_true(is.matrix(env))
  expect_true(is.numeric(morie:::.entheo_envelope(rnorm(40))))
  al <- morie:::.entheo_align(eeg, fmri) # triggers down-binning
  expect_equal(length(al$e), length(al$f))
  b <- morie:::.entheo_binding_per_frame(eeg, fmri)
  expect_true(is.numeric(b))
  s <- morie:::.entheo_san_per_frame(eeg, fmri)
  expect_true(is.numeric(s))
})

# ---- hawkes_fit.R ---------------------------------------------------------

test_that(".hawkes_kernel_funs builds every kernel family", {
  expect_type(morie:::.hawkes_kernel_funs("exponential", c(0, 0.5, 1.2)), "list")
  expect_type(morie:::.hawkes_kernel_funs("weibull", c(0, 0.5, 1.5, 2)), "list")
  expect_type(morie:::.hawkes_kernel_funs("lomax", c(0, 0.5, 2.5, 1)), "list")
  expect_type(morie:::.hawkes_kernel_funs("gamma", c(0, 0.5, 2, 1)), "list")
  # degenerate parameters -> NULL
  expect_null(morie:::.hawkes_kernel_funs("weibull", c(0, 0.5, 0, 1)))
})

test_that(".hawkes_nll_pureR returns a finite negative log-likelihood", {
  set.seed(2)
  times <- sort(cumsum(rexp(40, rate = 1)))
  nll <- morie:::.hawkes_nll_pureR(
    c(log(0.5), 0.3, 1.0),
    times, max(times), "exponential"
  )
  expect_true(is.finite(nll))
  expect_equal(
    morie:::.hawkes_nll_pureR(
      c(log(0.5), 5, 1),
      times, max(times), "exponential"
    ),
    1e12
  ) # eta out of range -> guard
})

test_that("morie_hawkes_fit fits and prints a Hawkes model", {
  set.seed(3)
  ev <- cumsum(rexp(120, rate = 2))
  fit <- morie_hawkes_fit(ev, kernel = "exponential")
  expect_s3_class(fit, "morie_hawkes_fit")
  expect_output(print(fit), "logLik")
})

# ---- mrm_otis.R -----------------------------------------------------------

test_that("mrm_otis_placement_concentration handles bands and empty years", {
  d <- data.frame(
    EndFiscalYear = c(rep(2023, 3), rep(2024, 3), 2099),
    NumberPlacements_Segregation = c(
      "1", "2 to 5", "Greater than 10",
      "1", "2 to 5", "Greater than 10", "1"
    ),
    NumberIndividuals_Segregation = c(40, 12, 3, 35, 9, 2, 0),
    stringsAsFactors = FALSE
  )
  res <- mrm_otis_placement_concentration(d)
  expect_s3_class(res, "data.frame")
  expect_true("2099" %in% as.character(res$year)) # the all-zero year row
})

test_that("mrm_otis_seg_duration_km handles an all-missing stratum", {
  d <- data.frame(
    NumberConsecutiveDays_Segregation = c(20, 5, 30, NA, NA, NA),
    MentalHealth_Alert = c("Y", "Y", "Y", "N", "N", "N"),
    stringsAsFactors = FALSE
  )
  res <- mrm_otis_seg_duration_km(d, group_cols = "MentalHealth_Alert")
  expect_s3_class(res, "data.frame")
  expect_true(any(res$n == 0)) # the all-NA "N" stratum
})

# ---- frns_predpol.R -------------------------------------------------------

test_that("morie_predpol_calibration_audit drops non-finite rows and interprets", {
  set.seed(4)
  areas <- paste0("A", 1:14)
  risk <- c(runif(13), NA) # one non-finite row
  outcome <- runif(14)
  grp <- rep(c("x", "y"), 7)
  res <- morie_predpol_calibration_audit(areas, risk, outcome, grp)
  expect_true(any(grepl("non-finite", res$warnings)))
  expect_true(is.character(res$interpretation))
})

test_that("morie_predpol_score_disparity drops NaNs, needs two groups, runs ANOVA", {
  expect_error(
    morie_predpol_score_disparity(c(1, 2, 3), c("a", "a", "a")),
    "two groups"
  )
  set.seed(5)
  score <- c(rnorm(20, 3), rnorm(20, 5), NaN)
  grp <- c(rep("low", 20), rep("high", 20), "low")
  res <- morie_predpol_score_disparity(score, grp)
  expect_true(any(grepl("non-finite", res$warnings)))
  expect_true(is.finite(res$spread))
})

# ---- dccmd.R --------------------------------------------------------------

test_that("morie_dcc_multivariate_garch fits a multivariate GARCH model", {
  set.seed(6)
  X <- matrix(rnorm(150 * 3, sd = 0.4), ncol = 3) # >=100 rows: no rugarch notice
  res <- morie_dcc_multivariate_garch(X)
  expect_true(is.list(res))
  expect_true(all(c("unconditional_correlation", "n", "k") %in% names(res)))
  expect_error(
    morie_dcc_multivariate_garch(matrix(rnorm(20), ncol = 2)),
    "n>=30"
  )
})
