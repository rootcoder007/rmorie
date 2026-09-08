# SPDX-License-Identifier: AGPL-3.0-or-later
# The offline default is what every example, test and fresh install uses.
# Before rmoriedata bundled the vic_* slugs it returned a 0-row frame, so
# these assert the fallback actually produces data -- an anchor that fails
# if the bundling regresses, not one that passes on an empty result.

test_that("morie_datasets_vic_table offline returns the bundled sample", {
  skip_if_not_installed("rmoriedata", minimum_version = "0.3.0")
  skip_if(is.null(.morie_vic_bundled("criminal_incidents")),
          "rmoriedata build does not carry the vic_* slugs")

  d <- morie_datasets_vic_table("criminal_incidents",
                                cache_dir = tempfile())
  expect_s3_class(d, "data.frame")
  expect_gt(nrow(d), 0L)
  expect_true(all(c("Year", "Offence Division") %in% names(d)))
  # Real counts, not placeholders.
  expect_true(is.numeric(d[["Incidents Recorded"]]))
  expect_gt(sum(d[["Incidents Recorded"]], na.rm = TRUE), 0)
})

test_that("the fallback covers more than one key", {
  skip_if_not_installed("rmoriedata", minimum_version = "0.3.0")
  skip_if(is.null(.morie_vic_bundled("lga_victim_reports")),
          "rmoriedata build does not carry the vic_* slugs")

  d <- morie_datasets_vic_table("lga_victim_reports", cache_dir = tempfile())
  expect_gt(nrow(d), 0L)
  expect_true("Local Government Area" %in% names(d))
})

test_that("an unbundled key still degrades to an empty frame", {
  # Not every catalog key is bundled; those must keep the old behaviour
  # rather than error.
  fake <- .morie_vic_bundled("definitely_not_a_bundled_key_9999")
  expect_null(fake)
})
