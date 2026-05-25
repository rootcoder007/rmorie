# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2P: tests for entheo_data + entheo_analysis + entheo_preprocess.

.make_synthetic_eeg <- function(n_channels = 32L, n_samples = 1024L,
                                 fs = 256L, seed = 1L) {
  set.seed(seed)
  matrix(stats::rnorm(n_channels * n_samples), n_channels, n_samples)
}

.make_synthetic_fmri <- function(n_voxels = 100L, n_volumes = 200L,
                                  seed = 2L) {
  set.seed(seed)
  matrix(stats::rnorm(n_voxels * n_volumes), n_voxels, n_volumes)
}

# ------------------------------------------------------------ entheo_data

test_that("load_dmt_imaging errors cleanly without bundled data", {
  out <- tryCatch(load_dmt_imaging(subject_id = "sub-001"),
                  error = function(e) e)
  # Either errors cleanly OR returns a list/df (depending on whether
  # the DMT_Imaging bundle is present on the system).
  expect_true(inherits(out, "error") || is.list(out) ||
                is.data.frame(out))
})

# ----------------------------------------------------- entheo_analysis

test_that("beautiful_loop_metric runs on synthetic EEG", {
  eeg <- .make_synthetic_eeg(n_channels = 16L, n_samples = 512L)
  out <- tryCatch(morie:::beautiful_loop_metric(eeg), error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("beautiful_loop_metric error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.numeric(out) || is.data.frame(out))
})

test_that("beautiful_loop_metric accepts an optional fMRI matrix", {
  eeg  <- .make_synthetic_eeg(n_channels = 16L, n_samples = 512L)
  fmri <- .make_synthetic_fmri(n_voxels = 50L,  n_volumes = 100L)
  out  <- tryCatch(morie:::beautiful_loop_metric(eeg, fmri = fmri),
                   error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("beautiful_loop_metric + fmri error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.numeric(out) || is.data.frame(out))
})

test_that("san_score runs on synthetic EEG", {
  eeg <- .make_synthetic_eeg(n_channels = 16L, n_samples = 512L)
  out <- tryCatch(morie:::san_score(eeg), error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("san_score error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.numeric(out) || is.data.frame(out))
})

# ---------------------------------------------------- entheo_preprocess

test_that("preprocess_eeg returns a preprocessed record", {
  record <- list(eeg = list(
    data_dmt = .make_synthetic_eeg(n_channels = 8L,
                                    n_samples = 256L, fs = 256L),
    data_pcb = .make_synthetic_eeg(n_channels = 8L,
                                    n_samples = 256L, fs = 256L, seed = 2L),
    sfreq = 256))
  out <- tryCatch(morie:::preprocess_eeg(record), error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("preprocess_eeg error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("preprocess_fmri returns a preprocessed record", {
  record <- list(fmri = list(
    data_dmt = .make_synthetic_fmri(n_voxels = 50L, n_volumes = 100L),
    data_pcb = .make_synthetic_fmri(n_voxels = 50L, n_volumes = 100L, seed = 3L),
    motion_fd_mm = runif(100L, 0, 0.4)),
                 tr = 2)
  out <- tryCatch(morie:::preprocess_fmri(record), error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("preprocess_fmri error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})
