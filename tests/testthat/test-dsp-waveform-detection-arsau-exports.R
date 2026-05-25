# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3BB: tests for exported callables across dsp_waveform.R (~20
# math-only feature extractors), dsp_detection.R (~5 detectors), and
# arsau_datasets.R helpers (~3 schema / registry helpers).

# ================================================================ dsp_waveform.R

test_that("morie_dsp_rms equals sqrt(mean(x^2))", {
  x <- c(3, 4)
  expect_equal(morie_dsp_rms(x), sqrt(mean(x^2)))
})

test_that("morie_dsp_form_factor returns ~1.11 for a pure sine", {
  x <- sin(seq(0, 2 * pi, length.out = 1000L))
  expect_equal(morie_dsp_form_factor(x), 1.11, tolerance = 0.02)
})

test_that("morie_dsp_form_factor returns 0 on zero vector", {
  expect_equal(morie_dsp_form_factor(rep(0, 10L)), 0)
})

test_that("morie_dsp_crest_factor returns ~sqrt(2) for a pure sine", {
  x <- sin(seq(0, 2 * pi, length.out = 1000L))
  expect_equal(morie_dsp_crest_factor(x), sqrt(2), tolerance = 0.02)
})

test_that("morie_dsp_crest_factor returns 0 on zero vector", {
  expect_equal(morie_dsp_crest_factor(rep(0, 10L)), 0)
})

test_that("morie_dsp_shape_factor returns 0 on zero vector", {
  expect_equal(morie_dsp_shape_factor(rep(0, 10L)), 0)
})

test_that("morie_dsp_shape_factor positive on a sine", {
  x <- sin(seq(0, 2 * pi, length.out = 200L))
  expect_true(morie_dsp_shape_factor(x) > 0)
})

test_that("morie_dsp_waveform_length is sum(|diff(x)|)", {
  x <- c(1, 3, 2, 5)
  expect_equal(morie_dsp_waveform_length(x), sum(abs(diff(x))))
})

test_that("morie_dsp_waveform_length_norm is per-sample length", {
  x <- c(1, 3, 2, 5)
  expect_equal(morie_dsp_waveform_length_norm(x),
               sum(abs(diff(x))) / length(x))
})

test_that("morie_dsp_turns_count returns 0 on length-<3 input", {
  expect_equal(morie_dsp_turns_count(c(1, 2)), 0L)
})

test_that("morie_dsp_turns_count counts true sign-flips above threshold", {
  # diff = c(1, -1, 1, -1) -> 3 sign changes, all magnitude>0.
  x <- c(0, 1, 0, 1, 0)
  expect_equal(morie_dsp_turns_count(x, threshold = 0), 3L)
})

test_that("morie_dsp_slope_sign_changes returns integer count", {
  x <- c(0, 1, 0, 1, 0)
  expect_type(morie_dsp_slope_sign_changes(x, threshold = 0), "integer")
})

test_that("morie_dsp_willison_amplitude returns integer count", {
  set.seed(1L)
  x <- stats::rnorm(50L)
  out <- morie_dsp_willison_amplitude(x)
  expect_type(out, "integer")
  expect_true(out >= 0L)
})

test_that("morie_dsp_myopulse_rate returns rate in [0, 1]", {
  set.seed(2L)
  x <- stats::rnorm(50L)
  out <- morie_dsp_myopulse_rate(x)
  expect_true(out >= 0 && out <= 1)
})

test_that("morie_dsp_hjorth_activity equals variance", {
  set.seed(3L)
  x <- stats::rnorm(100L)
  expect_equal(morie_dsp_hjorth_activity(x), stats::var(x),
               tolerance = 1e-10)
})

test_that("morie_dsp_hjorth_mobility returns a positive scalar", {
  set.seed(4L)
  x <- stats::rnorm(100L)
  expect_true(morie_dsp_hjorth_mobility(x) > 0)
})

test_that("morie_dsp_hjorth_complexity returns a positive scalar", {
  set.seed(5L)
  x <- stats::rnorm(100L)
  expect_true(morie_dsp_hjorth_complexity(x) > 0)
})

test_that("morie_dsp_hjorth returns a 3-vec of (activity, mobility, complexity)", {
  set.seed(6L)
  x <- stats::rnorm(100L)
  out <- morie_dsp_hjorth(x)
  expect_length(out, 3L)
})

test_that("morie_dsp_integrated_emg equals sum(|x|)", {
  x <- c(-1, 2, -3, 4)
  expect_equal(morie_dsp_integrated_emg(x), 10)
})

test_that("morie_dsp_mean_abs equals mean(|x|)", {
  x <- c(-1, 2, -3, 4)
  expect_equal(morie_dsp_mean_abs(x), 2.5)
})

test_that("morie_dsp_arc_length is sum(sqrt(1 + diff^2))", {
  x <- c(0, 3, 0)
  # diffs c(3, -3); arc = sqrt(10) + sqrt(10).
  expect_equal(morie_dsp_arc_length(x), 2 * sqrt(10),
               tolerance = 1e-10)
})

test_that("morie_dsp_centroidal_time returns positive scalar on positive signal", {
  x <- abs(sin(seq(0, 2 * pi, length.out = 200L)))
  out <- morie_dsp_centroidal_time(x, fs = 1)
  expect_true(is.numeric(out) && out > 0)
})

test_that("morie_dsp_amplitude_histogram returns counts/centers/edges list", {
  set.seed(7L)
  x <- stats::rnorm(500L)
  out <- morie_dsp_amplitude_histogram(x, n_bins = 25L)
  expect_type(out, "list")
  expect_true(all(c("counts", "centers", "probabilities", "edges")
                  %in% names(out)))
  expect_length(out$counts, 25L)
  expect_length(out$centers, 25L)
  expect_length(out$edges, 26L)
})

test_that("morie_dsp_entropy_histogram returns non-negative scalar", {
  set.seed(8L)
  x <- stats::rnorm(500L)
  out <- morie_dsp_entropy_histogram(x, n_bins = 25L)
  expect_true(is.numeric(out) && out >= 0)
})

# ================================================================ dsp_detection.R

test_that("morie_dsp_threshold_detect returns integer peak indices", {
  x <- c(0, 0, 5, 0, 0, 7, 0, 0)
  out <- morie_dsp_threshold_detect(x, threshold = 4,
                                      min_distance = 1L)
  expect_type(out, "integer")
  expect_true(all(out %in% which(x >= 4)))
})

test_that("morie_dsp_zero_crossing returns count or per-frame counts", {
  x <- c(1, -1, 1, -1, 1)
  out <- morie_dsp_zero_crossing(x)
  expect_true(is.numeric(out) && length(out) >= 1L)
})

test_that("morie_dsp_shannon_energy returns same-length numeric vector", {
  set.seed(9L)
  x <- stats::rnorm(100L)
  out <- morie_dsp_shannon_energy(x)
  expect_type(out, "double")
  expect_length(out, length(x))
})

test_that("morie_dsp_teager_energy returns numeric vector", {
  set.seed(10L)
  x <- stats::rnorm(50L)
  out <- morie_dsp_teager_energy(x)
  expect_true(is.numeric(out))
  expect_true(length(out) >= length(x) - 2L)
})

test_that("morie_dsp_hr_from_rr converts RR intervals to BPM", {
  # RR of 1.0s -> 60 bpm.
  out <- morie_dsp_hr_from_rr(c(1.0, 1.0, 1.0))
  expect_true(all(abs(out - 60) < 1e-6))
})

# ================================================================ arsau_datasets.R

test_that("morie_arsau_registry_df returns a data.frame on default lang", {
  out <- morie_arsau_registry_df()
  expect_s3_class(out, "data.frame")
  for (k in c("year_or_range", "kind", "csv_filename",
              "expected_rows", "is_valid", "description"))
    expect_true(k %in% names(out))
})

test_that("morie_arsau_registry_df respects French language flag", {
  out_fr <- morie_arsau_registry_df("fr")
  expect_s3_class(out_fr, "data.frame")
})

test_that("morie_arsau_sidecar_schema returns tidy [name,type,notes]", {
  sidecar <- list(fields = list(
    list(id = "incident_no", type = "text",
         info = list(notes = "Police-assigned ID.")),
    list(id = "year",        type = "int",
         info = list(notes = "Calendar year."))))
  out <- morie_arsau_sidecar_schema(sidecar)
  expect_s3_class(out, "data.frame")
  expect_equal(out$name, c("incident_no", "year"))
  expect_true(all(c("type", "notes") %in% names(out)))
})

test_that("morie_arsau_sidecar_schema errors on non-list input", {
  expect_error(morie_arsau_sidecar_schema(42),
               regexp = "must be a list")
})

test_that("morie_arsau_sidecar_schema returns 0-row frame on no fields", {
  out <- morie_arsau_sidecar_schema(list(fields = list()))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})
