# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 6 -- R/entheo_data.R: the DMT-imaging loader, its
# synthetic-fixture fallback, the subject lister, and the real .mat
# reader (R.matlab mocked).

test_that("load_dmt_imaging falls back to synthetic records with no root", {
  res <- load_dmt_imaging(subject_id = 7, root = tempfile("no-dmt-"))
  expect_true(res$synthetic)
  expect_true(is.na(res$root))
  expect_equal(res$subject_ids, "07")
  expect_length(res$records, 1L)
  expect_true(res$records[[1]]$.synthetic)
  expect_true(any(grepl("synthetic", res$warnings)))
})

test_that("load_dmt_imaging with NULL subject id uses the default panel", {
  res <- load_dmt_imaging(root = tempfile("no-dmt-"))
  expect_true(length(res$records) >= 10L)
  expect_true(all(vapply(
    res$records, function(r) r$.synthetic,
    logical(1)
  )))
})

test_that(".entheo_synthetic_record builds eeg / fmri / behavioural blocks", {
  rec <- morie:::.entheo_synthetic_record("09")
  expect_equal(rec$subject_id, "09")
  expect_equal(dim(rec$eeg$data_dmt), c(32L, 480L))
  expect_equal(rec$fmri$n_parcels, 100L)
  expect_length(rec$behavioural$intensity_dmt, 12L)
  expect_true(rec$.synthetic)
})

test_that(".entheo_list_subjects reads ids from a fMRI directory", {
  expect_equal(
    morie:::.entheo_list_subjects(tempfile("none-")),
    character(0)
  )
  root <- tempfile("dmt-root-")
  dir.create(file.path(root, "fMRI"), recursive = TRUE)
  file.create(file.path(
    root, "fMRI",
    c("LongS05DMT.mat", "LongS05PCB.mat", "LongS12DMT.mat")
  ))
  expect_equal(morie:::.entheo_list_subjects(root), c("05", "12"))
})

test_that(".entheo_load_real returns NULL without R.matlab", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "R.matlab")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_null(morie:::.entheo_load_real("05", tempfile("r-")))
})

test_that(".entheo_load_real returns NULL when the .mat files are absent", {
  root <- tempfile("dmt-empty-")
  dir.create(file.path(root, "fMRI"), recursive = TRUE)
  expect_null(morie:::.entheo_load_real("05", root))
})

test_that("load_dmt_imaging reads a real subject via a mocked R.matlab", {
  root <- tempfile("dmt-real-")
  dir.create(file.path(root, "fMRI"), recursive = TRUE)
  file.create(file.path(
    root, "fMRI",
    c("LongS05DMT.mat", "LongS05PCB.mat")
  ))
  testthat::local_mocked_bindings(
    readMat = function(con, ...) {
      list(big = matrix(
        stats::rnorm(200),
        10, 20
      ))
    },
    .package = "R.matlab"
  )
  res <- load_dmt_imaging(subject_id = 5, root = root)
  expect_false(is.na(res$root))
  expect_false(res$records[[1]]$.synthetic)
  expect_equal(res$records[[1]]$fmri$n_parcels, 10L)
})
