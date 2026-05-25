# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2O: tests for dataset_catalog.R.

test_that("morie_dataset_catalog returns a non-empty list / data.frame", {
  out <- morie_dataset_catalog()
  expect_true(is.list(out) || is.data.frame(out))
  if (is.data.frame(out)) {
    expect_gt(nrow(out), 0L)
    expect_true(all(c("key", "source") %in% names(out)))
  } else {
    expect_gt(length(out), 0L)
    expect_true("key" %in% names(out[[1]]))
  }
})

test_that("morie_dataset_catalog entries reference real survey families", {
  out <- morie_dataset_catalog()
  surveys <- if (is.data.frame(out)) out$survey
             else vapply(out, function(e) as.character(e$survey),
                         character(1))
  expect_true("cpads" %in% surveys)
})
