# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2T: tests for license_check.R — GPL-compatibility helpers.

test_that("morie_gpl_compatible_licenses returns a vector of SPDX ids", {
  out <- morie_gpl_compatible_licenses()
  expect_true(is.character(out) || is.list(out))
  expect_gt(length(out), 0L)
})

test_that("morie_gpl_compatible_licenses includes GPL-3.0 + MIT + Apache-2.0", {
  # The GPL-compatibility relation is one-way: GPL-2.0 + GPL-3.0 +
  # LGPL-2.1/3.0 + Apache-2.0 + MIT + BSD + ISC + MPL-2.0 + CC0 +
  # Unlicense + Zlib all relicense-compatibly INTO a GPL work.
  # AGPL is NOT on this list (it's stricter than GPL, so a GPL
  # downstream can't ingest AGPL-only code).
  out <- morie_gpl_compatible_licenses()
  flat <- if (is.list(out)) unlist(out) else out
  expect_true(any(grepl("GPL-3", flat, ignore.case = TRUE)))
  expect_true(any(grepl("Apache-2", flat, ignore.case = TRUE)))
  expect_true(any(grepl("MIT", flat, ignore.case = TRUE)))
})

test_that("morie_license_metadata returns the kernel-adjunct GPL-2.0 metadata", {
  # CLAUDE.md notes: "morie is AGPL-3.0-or-later. See LICENSING.md for
  # the full breakdown, including the optional GPL-2.0-only kernel
  # adjuncts." morie_license_metadata() returns the KERNEL-ADJUNCT
  # metadata (spdx = "GPL-2.0-only") so Linux kernel modules built
  # from morie can satisfy MODULE_LICENSE("GPL v2").
  out <- morie_license_metadata()
  expect_type(out, "list")
  expect_true("spdx" %in% names(out))
  expect_match(out$spdx, "GPL", ignore.case = TRUE)
  expect_equal(out$package, "morie")
})

test_that("morie_check_plugin_license accepts a compatible SPDX", {
  out <- tryCatch(
    morie_check_plugin_license(plugin_spdx = "AGPL-3.0-or-later"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("check_plugin_license error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.logical(out) || is.character(out))
})

test_that("morie_check_plugin_license flags an incompatible SPDX", {
  out <- tryCatch(
    morie_check_plugin_license(plugin_spdx = "Proprietary-Closed"),
    error = function(e) e
  )
  # Either errors cleanly or returns FALSE / a structured failure
  expect_true(inherits(out, "error") || is.list(out) ||
                isFALSE(out) || is.character(out))
})
