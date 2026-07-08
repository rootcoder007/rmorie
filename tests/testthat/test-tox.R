# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("morie_tox_matrix_schema is a typed zero-row template (no fabricated rows)", {
  s <- morie_tox_matrix_schema()
  expect_s3_class(s, "data.frame")
  expect_identical(nrow(s), 0L)
  expect_true(all(c("case_id", "analyte", "matrix", "conc", "lod", "loq") %in%
                    names(s)))
  roles <- attr(s, "role")
  expect_identical(unname(roles[["conc"]]), "measurement")
  expect_identical(unname(roles[["matrix"]]), "matrix")
})

test_that("calibration recovers a known linear curve and inverse-predicts", {
  set.seed(42)
  conc <- c(0.05, 0.1, 0.5, 1, 5, 10)
  # response ~ 1000 * conc, with negligible measurement noise
  response <- 1000 * conc + stats::rnorm(length(conc), 0, 1)
  cal <- morie_tox_calibration(conc, response, weights = "none",
                               response_unknown = 250)
  expect_equal(cal$slope, 1000, tolerance = 1e-3)
  expect_equal(cal$intercept, 0, tolerance = 1)
  expect_gt(cal$r_squared, 0.999)
  expect_equal(cal$conc_hat, 0.25, tolerance = 1e-2)
  expect_identical(cal$flag, "quantifiable")
})

test_that("calibration flags below-LOD and below-LOQ correctly", {
  set.seed(1)
  conc <- c(0.1, 0.5, 1, 5, 10, 50)
  response <- 1000 * conc + stats::rnorm(length(conc), 0, 20)
  cal <- morie_tox_calibration(conc, response, weights = "1/x^2")
  expect_true(cal$lod > 0 && cal$loq > cal$lod)
  below <- morie_tox_calibration(conc, response, weights = "1/x^2",
                                 response_unknown = 1000 * cal$lod * 0.5)
  expect_identical(below$flag, "below_lod")
})

test_that("calibration rejects bad input", {
  expect_error(morie_tox_calibration(c(1, 2), c(1, 2)), "at least 3")
  expect_error(morie_tox_calibration(c(0, 1, 2), c(1, 2, 3)), "must be > 0")
  expect_error(
    morie_tox_calibration(c(1, 2, 3), c(1, 2, 3), weights = "bogus"),
    "must be"
  )
})

test_that("PMR ratio classifies redistribution by C/P threshold", {
  expect_identical(morie_tox_pmr_ratio(1.0, 1.0)$redistribution, "minimal")
  expect_identical(morie_tox_pmr_ratio(1.8, 1.0)$redistribution, "modest")
  expect_identical(morie_tox_pmr_ratio(4.0, 1.0)$redistribution, "significant")
  expect_equal(morie_tox_pmr_ratio(2.4, 0.8)$cp_ratio, 3.0, tolerance = 1e-9)
  expect_error(morie_tox_pmr_ratio(1, 0), "> 0")
  expect_error(morie_tox_pmr_ratio(c(1, 2), 1), "scalars")
})

test_that("antemortem LR favours H1 when marker matches the antemortem model", {
  res <- morie_tox_antemortem_lr(
    marker = 2.0,
    antemortem = list(mean = 2.0, sd = 0.5),
    postmortem = list(mean = 0.1, sd = 0.3)
  )
  expect_gt(res$lr, 1)
  expect_true(grepl("antemortem", res$interpretation))
  # Symmetry: a near-zero marker should favour the postmortem artefact (LR < 1)
  res2 <- morie_tox_antemortem_lr(
    marker = 0.1,
    antemortem = list(mean = 2.0, sd = 0.5),
    postmortem = list(mean = 0.1, sd = 0.3)
  )
  expect_lt(res2$lr, 1)
})

test_that("antemortem LR validates the hypothesis models", {
  expect_error(
    morie_tox_antemortem_lr(1, list(mean = 1), list(mean = 0, sd = 1)),
    "mean="
  )
})

test_that("matrix reliability ranks protected matrices above diluting blood", {
  r <- morie_tox_matrix_reliability(submersion_days = 14, decomp_stage = 3)
  expect_s3_class(r, "data.frame")
  expect_identical(r$rank, seq_len(nrow(r)))
  # vitreous must outrank central blood under submersion
  expect_lt(which(r$matrix == "vitreous_humour"),
            which(r$matrix == "central_blood"))
  # submersion penalises diluting matrices more than protected ones
  dry <- morie_tox_matrix_reliability(matrix = "peripheral_blood")
  wet <- morie_tox_matrix_reliability(matrix = "peripheral_blood",
                                      submersion_days = 30)
  expect_lt(wet$reliability, dry$reliability)
  expect_error(morie_tox_matrix_reliability(matrix = "plasma"), "unknown matrix")
})

test_that("left-censor imputation substitutes below-LOD entries", {
  out <- morie_tox_left_censor_impute(c(0.4, NA, 0.9, 0.02), lod = 0.05)
  expect_equal(out$imputed, c(0.4, 0.025, 0.9, 0.025))
  expect_identical(out$censored, c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(out$fraction_censored, 0.5)
  expect_equal(
    morie_tox_left_censor_impute(c(NA), lod = 0.1, method = "sqrt2")$imputed,
    0.1 / sqrt(2)
  )
  expect_error(morie_tox_left_censor_impute(1, lod = 0), "> 0")
  expect_error(morie_tox_left_censor_impute(1, lod = 1, method = "x"), "must be")
})

test_that("ethanol congener adjudication reads the discriminating signals", {
  expect_identical(
    morie_tox_ethanol_congeners(ethanol = 1.2, n_propanol = 0.08)$verdict,
    "postmortem_production"
  )
  expect_identical(
    morie_tox_ethanol_congeners(ethanol = 1.2, etg = 3.5)$verdict,
    "antemortem"
  )
  expect_identical(
    morie_tox_ethanol_congeners(ethanol = 1.2)$verdict,
    "indeterminate"
  )
  # EtG (antemortem) takes precedence even if congeners present
  expect_identical(
    morie_tox_ethanol_congeners(ethanol = 1.2, n_propanol = 0.1, etg = 2)$verdict,
    "antemortem"
  )
  expect_error(morie_tox_ethanol_congeners(-1), ">= 0")
})
