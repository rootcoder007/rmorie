# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for batch 05: entheo_data, entheo_preprocess, ewtma, extvm, fast,
# flsha, frns_metrics, frns_predpol, frns_temporal, fwpas, fzbrd, fzcvm,
# fzedg, fzhdc, fzhok.

test_that("load_dmt_imaging returns synthetic fallback structure", {
  res <- morie:::load_dmt_imaging(
    subject_id = 1L,
    root = tempfile("no_such_root_")
  )
  expect_true(is.list(res))
  expect_named(res, c(
    "records", "root", "synthetic", "subject_ids",
    "warnings"
  ))
  expect_true(res$synthetic)
  expect_true(is.na(res$root))
  expect_length(res$records, 1L)
  expect_true(length(res$warnings) >= 1L)
})

test_that("load_dmt_imaging synthetic record has eeg/fmri/behavioural", {
  res <- morie:::load_dmt_imaging(
    subject_id = 3L,
    root = tempfile("missing_")
  )
  rec <- res$records[[1]]
  expect_true(is.list(rec))
  expect_true(is.matrix(rec$eeg$data_dmt))
  expect_true(is.matrix(rec$eeg$data_pcb))
  expect_true(is.matrix(rec$fmri$data_dmt))
  expect_equal(nrow(rec$eeg$data_dmt), 32L)
  expect_true(all(is.finite(rec$fmri$motion_fd_mm)))
  expect_true(rec$.synthetic)
})

test_that("load_dmt_imaging handles NULL subject_id (all subjects)", {
  res <- morie:::load_dmt_imaging(
    subject_id = NULL,
    root = tempfile("absent_")
  )
  expect_true(length(res$records) >= 1L)
  expect_true(is.character(res$subject_ids))
  expect_true(all(nchar(res$subject_ids) == 2L))
})

test_that("load_dmt_imaging accepts multiple subject ids", {
  res <- morie:::load_dmt_imaging(
    subject_id = c(1L, 2L),
    root = tempfile("absent_")
  )
  expect_length(res$records, 2L)
  expect_length(res$subject_ids, 2L)
})

.batch05_record <- function(seed = 1L, n_chan = 8L, n_tp = 64L,
                            n_parcels = 12L) {
  set.seed(seed)
  list(
    subject_id = "01",
    eeg = list(
      sfreq = 250,
      channels = sprintf("E%02d", seq_len(n_chan)),
      data_dmt = matrix(stats::rnorm(n_chan * n_tp), n_chan, n_tp),
      data_pcb = matrix(stats::rnorm(n_chan * n_tp), n_chan, n_tp)
    ),
    fmri = list(
      tr = 2.0, n_parcels = n_parcels,
      data_dmt = matrix(stats::rnorm(n_parcels * n_tp), n_parcels, n_tp),
      data_pcb = matrix(stats::rnorm(n_parcels * n_tp), n_parcels, n_tp),
      motion_fd_mm = stats::runif(n_tp, 0, 0.6)
    ),
    behavioural = list()
  )
}

test_that("preprocess_eeg returns cleaned record with expected names", {
  rec <- .batch05_record()
  res <- morie:::preprocess_eeg(rec)
  expect_true(is.list(res))
  expect_named(res, c(
    "record", "n_bad", "sfreq", "bandpass", "notch",
    "asr_threshold", "n_channels", "warnings",
    "interpretation"
  ))
  expect_true(is.matrix(res$record$eeg$data_dmt))
  expect_equal(res$sfreq, 250)
  expect_gte(res$n_bad, 0L)
  expect_equal(res$n_channels, 8L)
  expect_type(res$interpretation, "character")
})

test_that("preprocess_eeg respects custom bandpass/notch/threshold", {
  rec <- .batch05_record(seed = 2L)
  res <- morie:::preprocess_eeg(rec,
    bandpass = c(2, 30), notch = 50,
    asr_threshold = 5
  )
  expect_equal(res$bandpass, c(2, 30))
  expect_equal(res$notch, 50)
  expect_equal(res$asr_threshold, 5)
  expect_gte(res$n_bad, 0L)
})

test_that("preprocess_eeg warns when eeg matrices absent", {
  rec <- .batch05_record()
  rec$eeg$data_dmt <- NULL
  rec$eeg$data_pcb <- NULL
  res <- morie:::preprocess_eeg(rec)
  expect_true(length(res$warnings) >= 1L)
  expect_equal(res$n_bad, 0L)
})

test_that("preprocess_fmri returns cleaned record with expected names", {
  rec <- .batch05_record()
  res <- morie:::preprocess_fmri(rec)
  expect_true(is.list(res))
  expect_named(res, c(
    "record", "n_scrubbed", "motion_threshold_mm",
    "n_noise_components", "n_parcels", "warnings",
    "interpretation"
  ))
  expect_true(is.matrix(res$record$fmri$data_dmt))
  expect_gte(res$n_scrubbed, 0L)
  expect_equal(res$n_parcels, 12L)
})

test_that("preprocess_fmri respects custom threshold and component count", {
  rec <- .batch05_record(seed = 3L)
  res <- morie:::preprocess_fmri(rec,
    motion_threshold_mm = 0.1,
    n_noise_components = 2L
  )
  expect_equal(res$motion_threshold_mm, 0.1)
  expect_equal(res$n_noise_components, 2L)
  expect_gte(res$n_scrubbed, 0L)
})

test_that("preprocess_fmri warns when motion absent and matrices missing", {
  rec <- .batch05_record()
  rec$fmri$motion_fd_mm <- NULL
  res1 <- morie:::preprocess_fmri(rec)
  expect_true(length(res1$warnings) >= 1L)

  rec2 <- .batch05_record()
  rec2$fmri$data_dmt <- NULL
  rec2$fmri$data_pcb <- NULL
  res2 <- morie:::preprocess_fmri(rec2)
  expect_true(length(res2$warnings) >= 1L)
  expect_equal(res2$n_scrubbed, 0L)
})

test_that("morie_ewma_volatility returns the documented structure", {
  set.seed(10)
  x <- stats::rnorm(200)
  res <- morie_ewma_volatility(x)
  expect_true(is.list(res))
  expect_named(res, c(
    "conditional_variance", "conditional_volatility",
    "lambda", "n", "last_variance", "last_volatility",
    "method"
  ))
  expect_length(res$conditional_variance, 200L)
  expect_length(res$conditional_volatility, 200L)
  expect_true(all(is.finite(res$conditional_variance)))
  expect_true(all(res$conditional_variance >= 0))
  expect_equal(res$n, 200L)
  expect_equal(res$lambda, 0.94)
  expect_equal(res$last_volatility, sqrt(res$last_variance))
})

test_that("morie_ewma_volatility honours a custom lambda", {
  set.seed(11)
  x <- stats::rnorm(50)
  res <- morie_ewma_volatility(x, lambda = 0.8)
  expect_equal(res$lambda, 0.8)
  expect_true(all(is.finite(res$conditional_volatility)))
})

test_that("morie_ewma_volatility errors on bad input", {
  expect_error(morie_ewma_volatility(1))
  expect_error(morie_ewma_volatility(stats::rnorm(10), lambda = 0))
  expect_error(morie_ewma_volatility(stats::rnorm(10), lambda = 1))
})

test_that("morie_extreme_value_gev fits a GEV and returns SEs", {
  set.seed(20)
  x <- stats::rnorm(300, mean = 10, sd = 2)
  res <- morie_extreme_value_gev(x)
  expect_true(is.list(res))
  expect_true(all(c(
    "mu", "sigma", "xi", "se_mu", "se_sigma", "se_xi",
    "loglik", "estimate", "se", "n", "method"
  ) %in%
    names(res)))
  expect_true(is.finite(res$mu))
  expect_true(is.finite(res$sigma))
  expect_true(res$sigma > 0)
  expect_equal(res$n, 300L)
  expect_equal(res$estimate, res$mu)
})

test_that("morie_extreme_value_gev returns NA path for too-few obs", {
  res <- morie_extreme_value_gev(c(1, 2, 3))
  expect_true(is.list(res))
  expect_true(is.na(res$estimate))
  expect_equal(res$n, 3L)
})

test_that("morie_fast_available returns a logical scalar", {
  res <- morie_fast_available()
  expect_type(res, "logical")
  expect_length(res, 1L)
  expect_false(is.na(res))
})

test_that("internal fast kernels match base-R results", {
  set.seed(30)
  x <- stats::rnorm(40)
  y <- stats::rnorm(40)
  expect_equal(morie:::morie_normal_pdf(x, 0, 1), dnorm(x, 0, 1),
    tolerance = 1e-8
  )
  expect_equal(morie:::morie_mean(x), mean(x), tolerance = 1e-8)
  expect_equal(morie:::morie_var(x), stats::var(x), tolerance = 1e-8)
  expect_equal(morie:::morie_var(x, ddof = 0),
    sum((x - mean(x))^2) / length(x),
    tolerance = 1e-8
  )
  expect_equal(morie:::morie_cor_pearson(x, y),
    suppressWarnings(stats::cor(x, y)),
    tolerance = 1e-8
  )
})

test_that("internal morie_var handles degenerate ddof", {
  res <- morie:::morie_var(c(1), ddof = 1)
  expect_true(is.na(res))
})

test_that("flash_attention self-attention returns expected shape", {
  set.seed(40)
  Q <- matrix(stats::rnorm(10 * 4), 10, 4)
  res <- morie:::flash_attention(Q)
  expect_true(is.list(res))
  expect_named(res, c("tensor", "block_size", "method"))
  expect_true(is.matrix(res$tensor))
  expect_equal(dim(res$tensor), c(10L, 4L))
  expect_true(all(is.finite(res$tensor)))
})

test_that("flash_attention accepts separate K, V and a mask", {
  set.seed(41)
  Q <- matrix(stats::rnorm(6 * 3), 6, 3)
  K <- matrix(stats::rnorm(8 * 3), 8, 3)
  V <- matrix(stats::rnorm(8 * 3), 8, 3)
  mask <- matrix(0, 6, 8)
  res <- morie:::flash_attention(Q,
    K = K, V = V, block_size = 4L,
    mask = mask
  )
  expect_equal(dim(res$tensor), c(6L, 3L))
  expect_true(all(is.finite(res$tensor)))
  expect_equal(res$block_size, 4L)
})

test_that("morie_fairness_disparate_impact detects adverse impact", {
  pred <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
  race <- c(rep("A", 5), rep("B", 5))
  res <- morie_fairness_disparate_impact(pred, race, privileged = "A")
  expect_true(is.list(res))
  expect_true(all(c(
    "value", "ratios", "rates", "privileged",
    "adverse_impact", "threshold", "warnings",
    "interpretation"
  ) %in% names(res)))
  expect_true(res$adverse_impact)
  expect_lt(res$value, 0.8)
  expect_equal(res$privileged, "A")
  expect_equal(res$threshold, 0.8)
})

test_that("morie_fairness_disparate_impact infers privileged group with warning", {
  pred <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
  race <- c(rep("A", 5), rep("B", 5))
  res <- morie_fairness_disparate_impact(pred, race)
  expect_true(length(res$warnings) >= 1L)
  expect_true(res$privileged %in% c("A", "B"))
})

test_that("morie_fairness_disparate_impact errors on bad inputs", {
  expect_error(morie_fairness_disparate_impact(c(1, 0), c("A", "A")))
  expect_error(morie_fairness_disparate_impact(c(1, 0, 1),
    c("A", "B", "B"),
    privileged = "Z"
  ))
  expect_error(morie_fairness_disparate_impact(c(1, 0, 1), c("A", "B")))
})

test_that("morie_fairness_demographic_parity reports the parity gap", {
  pred <- c(1, 1, 1, 1, 0, 0, 0, 1, 0, 0)
  race <- c(rep("A", 5), rep("B", 5))
  res <- morie_fairness_demographic_parity(pred, race, privileged = "A")
  expect_true(is.list(res))
  expect_true(all(c(
    "value", "gaps", "rates", "privileged", "warnings",
    "interpretation"
  ) %in% names(res)))
  expect_true(is.finite(res$value))
  expect_equal(res$privileged, "A")
})

test_that("morie_fairness_equalized_odds flags a TPR/FPR violation", {
  truth <- c(1, 0, 1, 0, 1, 0, 1, 0)
  pred <- c(1, 0, 1, 0, 1, 1, 0, 1)
  race <- c(rep("A", 4), rep("B", 4))
  res <- morie_fairness_equalized_odds(truth, pred, race, privileged = "A")
  expect_true(is.list(res))
  expect_true(all(c(
    "value", "tpr_gaps", "fpr_gaps", "rates",
    "privileged", "violation", "warnings",
    "interpretation"
  ) %in% names(res)))
  expect_type(res$violation, "logical")
})

test_that("morie_fairness_equalized_odds errors on mismatched lengths", {
  expect_error(morie_fairness_equalized_odds(
    c(1, 0), c(1, 0, 1),
    c("A", "B", "A")
  ))
})

test_that("morie_fairness_average_odds_difference returns AOD breakdown", {
  truth <- c(1, 0, 1, 0, 1, 0, 1, 0)
  pred <- c(1, 0, 1, 0, 1, 1, 0, 1)
  race <- c(rep("A", 4), rep("B", 4))
  res <- morie_fairness_average_odds_difference(truth, pred, race,
    privileged = "A"
  )
  expect_true(is.list(res))
  expect_true(all(c(
    "value", "average_odds_difference", "rates",
    "privileged", "warnings", "interpretation"
  ) %in%
    names(res)))
  expect_true(is.list(res$average_odds_difference))
})

test_that("morie_fairness_gini ranges from 0 to near 1", {
  eq <- morie_fairness_gini(c(5, 5, 5, 5))
  expect_true(is.list(eq))
  expect_true(all(c(
    "value", "gini", "per_group", "warnings",
    "interpretation"
  ) %in% names(eq)))
  expect_equal(eq$value, 0)

  conc <- morie_fairness_gini(c(0, 0, 0, 100))
  expect_gt(conc$value, 0.5)
  expect_lte(conc$value, 1)
})

test_that("morie_fairness_gini supports per-group breakdown and negatives", {
  res <- morie_fairness_gini(c(1, 2, 3, 4, 5, 6),
    group = c("A", "A", "A", "B", "B", "B")
  )
  expect_true(is.list(res$per_group))
  expect_true(all(c("A", "B") %in% names(res$per_group)))

  neg <- morie_fairness_gini(c(-1, 2, 3))
  expect_true(length(neg$warnings) >= 1L)
})

test_that("morie_fairness_gini errors on empty input", {
  expect_error(morie_fairness_gini(numeric(0)))
})

test_that("morie_fairness_bias_amplification returns composite score", {
  pred <- c(1, 1, 1, 1, 0, 0, 0, 0)
  race <- c(rep("A", 4), rep("B", 4))
  res <- morie_fairness_bias_amplification(pred, race, privileged = "A")
  expect_true(is.list(res))
  expect_true(all(c(
    "value", "bias_amplification_score",
    "demographic_parity_gap", "gini", "rates",
    "privileged", "warnings", "interpretation"
  ) %in%
    names(res)))
  expect_true(is.finite(res$value))
  expect_equal(res$value, res$bias_amplification_score)
})

test_that("morie_predpol_aggregate_areas rolls records up per area", {
  agg <- morie_predpol_aggregate_areas(
    area = c("a", "a", "b", "b"), risk = c(10, 20, 30, 40),
    outcome = c(1, 0, 1, 1)
  )
  expect_true(is.list(agg))
  expect_named(agg, c(
    "areas", "mean_risk", "outcome_rate", "group",
    "n_records"
  ))
  expect_equal(agg$areas, c("a", "b"))
  expect_equal(agg$mean_risk, c(15, 35))
  expect_equal(agg$outcome_rate, c(0.5, 1.0))
  expect_equal(agg$n_records, c(2L, 2L))
  expect_null(agg$group)
})

test_that("morie_predpol_aggregate_areas handles group and named population", {
  agg <- morie_predpol_aggregate_areas(
    area = c("a", "a", "b", "b"), risk = c(10, 20, 30, 40),
    outcome = c(2, 1, 5, 4),
    group = c("X", "X", "Y", "Y"),
    population = c(a = 10000, b = 20000)
  )
  expect_equal(agg$group, c("X", "Y"))
  expect_true(all(is.finite(agg$outcome_rate)))
})

test_that("morie_predpol_aggregate_areas accepts per-record population", {
  agg <- morie_predpol_aggregate_areas(
    area = c("a", "a", "b", "b"), risk = c(10, 20, 30, 40),
    outcome = c(2, 1, 5, 4),
    population = c(5000, 5000, 8000, 8000)
  )
  expect_true(all(is.finite(agg$outcome_rate)))
})

test_that("morie_predpol_aggregate_areas errors on misaligned inputs", {
  expect_error(morie_predpol_aggregate_areas(
    c("a", "b"), c(1, 2, 3),
    c(1, 0)
  ))
  expect_error(morie_predpol_aggregate_areas(c("a", "b"), c(1, 2), c(1, 0),
    group = c("X")
  ))
  expect_error(morie_predpol_aggregate_areas(c("a", "b"), c(1, 2), c(1, 0),
    population = c(1, 2, 3)
  ))
})

test_that("morie_predpol_calibration_audit reports Spearman and rank gaps", {
  res <- morie_predpol_calibration_audit(
    areas = c("d1", "d2", "d3", "d4", "d5", "d6"),
    mean_risk = c(90, 80, 70, 30, 20, 10),
    outcome_rate = c(10, 20, 30, 70, 80, 90),
    group = c("X", "X", "X", "Y", "Y", "Y")
  )
  expect_true(is.list(res))
  expect_true(all(c(
    "value", "spearman", "spearman_pvalue",
    "group_rank_gap", "worst_group", "rank_gap",
    "warnings", "interpretation"
  ) %in% names(res)))
  expect_true(is.finite(res$spearman))
  expect_lte(res$spearman, 0)
  expect_true(res$worst_group %in% c("X", "Y"))
})

test_that("morie_predpol_calibration_audit drops non-finite areas", {
  res <- morie_predpol_calibration_audit(
    areas = c("d1", "d2", "d3", "d4"),
    mean_risk = c(90, 80, NA, 30),
    outcome_rate = c(10, 20, 30, 70),
    group = c("X", "X", "Y", "Y")
  )
  expect_true(length(res$warnings) >= 1L)
})

test_that("morie_predpol_calibration_audit errors on bad input", {
  expect_error(morie_predpol_calibration_audit(c("d1"), c(1), c(1), c("X")))
  expect_error(morie_predpol_calibration_audit(
    c("d1", "d2"), c(1, 2),
    c(1, 2), c("X")
  ))
})

test_that("morie_predpol_score_disparity returns ANOVA-backed summary", {
  res <- morie_predpol_score_disparity(
    score = c(9, 10, 11, 19, 20, 21),
    group = c("A", "A", "A", "B", "B", "B")
  )
  expect_true(is.list(res))
  expect_true(all(c(
    "value", "spread", "group_means", "gaps", "anova_f",
    "anova_pvalue", "significant", "reference",
    "per_group", "warnings", "interpretation"
  ) %in%
    names(res)))
  expect_equal(res$value, 10)
  expect_type(res$significant, "logical")
  expect_true(res$reference %in% c("A", "B"))
})

test_that("morie_predpol_score_disparity honours explicit reference", {
  res <- morie_predpol_score_disparity(
    score = c(9, 10, 11, 19, 20, 21),
    group = c("A", "A", "A", "B", "B", "B"),
    reference = "B"
  )
  expect_equal(res$reference, "B")
})

test_that("morie_predpol_score_disparity errors on bad input", {
  expect_error(morie_predpol_score_disparity(c(1, 2, 3), c("A", "A")))
  expect_error(morie_predpol_score_disparity(
    c(1, 2, 3),
    c("A", "A", "A")
  ))
  expect_error(morie_predpol_score_disparity(c(1, 2, 3, 4),
    c("A", "A", "B", "B"),
    reference = "Z"
  ))
})

test_that("morie_predpol_temporal_audit audits cells across periods", {
  period <- c(rep("p1", 10), rep("p2", 10))
  city <- rep("A", 20)
  pred <- rep(c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0), 2)
  grp <- rep(c(rep("X", 5), rep("Y", 5)), 2)
  res <- morie_predpol_temporal_audit(period, city, pred, grp,
    privileged = "X"
  )
  expect_true(is.list(res))
  expect_true(all(c(
    "value", "worst_dir_range", "cross_city_dir_spread",
    "per_city", "cells", "privileged", "warnings",
    "interpretation"
  ) %in% names(res)))
  expect_true(is.list(res$per_city$A))
  expect_equal(res$per_city$A$dir_range, 0)
  expect_equal(res$privileged, "X")
})

test_that("morie_predpol_temporal_audit infers privileged group with warning", {
  period <- c(rep("p1", 10), rep("p2", 10))
  city <- rep("A", 20)
  pred <- rep(c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0), 2)
  grp <- rep(c(rep("X", 5), rep("Y", 5)), 2)
  res <- morie_predpol_temporal_audit(period, city, pred, grp)
  expect_true(length(res$warnings) >= 1L)
  expect_true(res$privileged %in% c("X", "Y"))
})

test_that("morie_predpol_temporal_audit errors on misaligned/empty input", {
  expect_error(morie_predpol_temporal_audit(
    c("p1", "p2"), c("A"),
    c(1, 0), c("X", "Y")
  ))
  expect_error(morie_predpol_temporal_audit(
    character(0), character(0),
    numeric(0), character(0)
  ))
})

test_that("morie_fwpas_forward_pass_dense computes a matrix forward pass", {
  set.seed(50)
  x <- matrix(stats::rnorm(6 * 4), 6, 4)
  w <- matrix(stats::rnorm(3 * 4), 3, 4)
  b <- stats::rnorm(3)
  res <- morie_fwpas_forward_pass_dense(x, w, b, activation = "sigmoid")
  expect_true(is.list(res))
  expect_named(res, c("z", "a", "estimate", "activation", "method"))
  expect_equal(dim(res$a), c(6L, 3L))
  expect_true(all(res$a >= 0 & res$a <= 1))
  expect_identical(res$a, res$estimate)
})

test_that("morie_fwpas_forward_pass_dense supports all activations", {
  set.seed(51)
  x <- matrix(stats::rnorm(5 * 4), 5, 4)
  w <- matrix(stats::rnorm(3 * 4), 3, 4)
  b <- stats::rnorm(3)
  for (act in c(
    "identity", "linear", "none", "tanh", "relu",
    "softmax"
  )) {
    res <- morie_fwpas_forward_pass_dense(x, w, b, activation = act)
    expect_equal(dim(res$a), c(5L, 3L))
    expect_true(all(is.finite(res$a)))
  }
  sm <- morie_fwpas_forward_pass_dense(x, w, b, activation = "softmax")
  expect_true(all(abs(rowSums(sm$a) - 1) < 1e-8))
  rl <- morie_fwpas_forward_pass_dense(x, w, b, activation = "relu")
  expect_true(all(rl$a >= 0))
})

test_that("morie_fwpas_forward_pass_dense errors on unknown activation", {
  x <- matrix(stats::rnorm(8), 2, 4)
  w <- matrix(stats::rnorm(12), 3, 4)
  b <- stats::rnorm(3)
  expect_error(morie_fwpas_forward_pass_dense(x, w, b, activation = "bogus"))
})

test_that("morie_forward_pass_dense alias matches morie_fwpas_forward_pass_dense", {
  set.seed(52)
  x <- matrix(stats::rnorm(4 * 3), 4, 3)
  w <- matrix(stats::rnorm(2 * 3), 2, 3)
  b <- stats::rnorm(2)
  expect_equal(
    morie_forward_pass_dense(x, w, b),
    morie_fwpas_forward_pass_dense(x, w, b)
  )
})

test_that("fzbrd returns a bias-reduced KDFE estimate", {
  set.seed(60)
  x <- stats::rnorm(400)
  res <- fzbrd(x, t = 0)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "F_h", "F_ch", "se", "h", "c", "t",
    "n", "method"
  ))
  expect_true(is.finite(res$estimate))
  expect_gt(res$se, 0)
  expect_equal(res$n, 400L)
  expect_equal(res$c, 2)
})

test_that("fzbrd uses default t/h and custom c", {
  set.seed(61)
  x <- stats::rnorm(120)
  res <- fzbrd(x, c = 3)
  expect_equal(res$c, 3)
  expect_true(is.finite(res$h))
  expect_true(is.finite(res$t))
})

test_that("fzbrd handles too-few obs and invalid c", {
  res <- fzbrd(c(1))
  expect_true(is.na(res$estimate))
  expect_equal(res$n, 1L)
  expect_error(fzbrd(stats::rnorm(20), c = 1))
})

test_that("morie_fauzi_bias_reduced_kdfe alias matches fzbrd", {
  set.seed(62)
  x <- stats::rnorm(80)
  expect_equal(morie_fauzi_bias_reduced_kdfe(x, t = 0), fzbrd(x, t = 0))
})

test_that("fzcvm computes a smoothed Cramer-von Mises statistic", {
  set.seed(70)
  x <- stats::rnorm(300)
  res <- fzcvm(x, cdf = "norm", args = list(0, 1))
  expect_true(is.list(res))
  expect_named(res, c("statistic", "p_value", "h", "n", "method"))
  expect_gte(res$statistic, 0)
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
  expect_equal(res$n, 300L)
})

test_that("fzcvm accepts a function CDF and default args", {
  set.seed(71)
  x <- stats::runif(150)
  res_fun <- fzcvm(x, cdf = function(t) punif(t))
  expect_gte(res_fun$statistic, 0)

  res_def <- fzcvm(stats::rnorm(100))
  expect_gte(res_def$statistic, 0)
})

test_that("fzcvm handles too-few obs and bad cdf", {
  res <- fzcvm(c(1, 2, 3))
  expect_true(is.na(res$statistic))
  expect_equal(res$n, 3L)
  expect_error(fzcvm(stats::rnorm(20), cdf = "weibull"))
})

test_that("morie_fauzi_cvm_smoothed alias matches fzcvm", {
  set.seed(72)
  x <- stats::rnorm(60)
  expect_equal(
    morie_fauzi_cvm_smoothed(x, cdf = "norm", args = list(0, 1)),
    fzcvm(x, cdf = "norm", args = list(0, 1))
  )
})

test_that("fzedg returns Edgeworth correction components", {
  res <- fzedg(1:50, z = 1.96, p = 0.5)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "normal_approx", "edgeworth_correction",
    "cornish_fisher_correction", "skew", "p1z", "z",
    "p", "n", "method"
  ))
  expect_true(is.finite(res$estimate))
  expect_lt(abs(res$skew), 1e-10)
  expect_equal(res$z, 1.96)
  expect_equal(res$n, 50L)
})

test_that("fzedg handles a skewed quantile probability", {
  res <- fzedg(1:80, z = 1.64, p = 0.9)
  expect_true(is.finite(res$skew))
  expect_true(res$skew != 0)
  expect_true(is.finite(res$edgeworth_correction))
})

test_that("fzedg handles too-few obs", {
  res <- fzedg(c(1, 2, 3))
  expect_true(is.na(res$estimate))
  expect_equal(res$n, 3L)
})

test_that("morie_fauzi_edgeworth_quantile alias matches fzedg", {
  expect_equal(morie_fauzi_edgeworth_quantile(1:30), fzedg(1:30))
})

test_that("fzhdc computes a Hoeffding decomposition", {
  set.seed(80)
  x <- stats::rnorm(150)
  res <- fzhdc(x)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "sigma1_sq", "sigma2_sq", "se", "n",
    "n_pairs", "method"
  ))
  expect_true(is.finite(res$estimate))
  expect_gte(res$sigma1_sq, 0)
  expect_gte(res$se, 0)
  expect_equal(res$n, 150L)
  expect_gt(res$n_pairs, 0L)
})

test_that("fzhdc subsamples pairs when above max_pairs", {
  set.seed(81)
  x <- stats::rnorm(200)
  res <- fzhdc(x, max_pairs = 300L, seed = 1L)
  expect_lte(res$n_pairs, 300L)
  expect_true(is.finite(res$estimate))
})

test_that("fzhdc accepts a custom kernel and handles too-few obs", {
  set.seed(82)
  x <- stats::rnorm(60)
  res <- fzhdc(x, kernel = function(a, b) abs(a - b))
  expect_true(is.finite(res$estimate))

  few <- fzhdc(c(1, 2, 3))
  expect_true(is.na(few$estimate))
  expect_equal(few$n, 3L)
})

test_that("morie_fauzi_h_decomposition alias matches fzhdc", {
  set.seed(83)
  x <- stats::rnorm(40)
  expect_equal(morie_fauzi_h_decomposition(x), fzhdc(x))
})

test_that("fzhok computes an order-4 kernel density estimate", {
  set.seed(90)
  x <- stats::rnorm(2000)
  res <- fzhok(x, t = 0)
  expect_true(is.list(res))
  expect_named(res, c(
    "estimate", "h", "t", "order", "mu_r", "R_K", "n",
    "method"
  ))
  expect_true(is.finite(res$estimate))
  expect_equal(res$order, 4L)
  expect_equal(res$mu_r, -3)
  expect_equal(res$n, 2000L)
})

test_that("fzhok uses default t/h and rejects non-4 orders", {
  set.seed(91)
  x <- stats::rnorm(100)
  res <- fzhok(x)
  expect_true(is.finite(res$h))
  expect_true(is.finite(res$t))
  expect_error(fzhok(x, order = 2L))
})

test_that("fzhok handles too-few obs", {
  res <- fzhok(c(1))
  expect_true(is.na(res$estimate))
  expect_equal(res$n, 1L)
})

test_that("morie_fauzi_higher_order_kernel alias matches fzhok", {
  set.seed(92)
  x <- stats::rnorm(80)
  expect_equal(morie_fauzi_higher_order_kernel(x, t = 0), fzhok(x, t = 0))
})
