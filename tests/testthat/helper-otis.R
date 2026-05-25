# SPDX-License-Identifier: AGPL-3.0-or-later
#
# OTIS synthetic helpers used by the testthat suite. The 29 per-id
# panel generators (.morie_otis_<id>_panel) and the
# morie_synth_otis() / morie_synth_otis_all() dispatchers now live in
# R/synth_otis.R and ship as part of the package, so the same
# generators are available to users and to the test suite.
#
# This file keeps two test-only aliases for back-compat with the older
# helper-otis API:
#   make_synthetic_otis(id, n, seed)
#   make_synthetic_otis_datasets_complete(n, seed)
# Both delegate to the package fns.

make_synthetic_otis <- function(id, n = 200L, seed = 1L) {
  morie::morie_synth_otis(id, n = n, seed = seed)
}

make_synthetic_otis_datasets_complete <- function(n = 80L, seed = 2L) {
  morie::morie_synth_otis_all(n = n, seed = seed)
}
