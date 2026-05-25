# CRAN-conservative test cache override.
#
# Per CRAN repository policy, packages must not write to the user's
# home directory or any persistent location during R CMD check. morie's
# production cache resolves via `tools::R_user_dir("morie", "cache")`
# (correct for end users), but during tests we redirect every cache
# write to a per-session tempdir so check leaves nothing behind on the
# CRAN test machines.
#
# MORIE_CACHE_DIR env var is the single override read by
# morie_cache_dir() (see R/database.R). Setting it here puts every
# morie cache, db, and download under a fresh tempfile path that R
# auto-cleans at session exit.
Sys.setenv(MORIE_CACHE_DIR = tempfile("morie-test-cache-"))

# NOT_CRAN handling.
#
# Bare `testthat::test_dir()` (e.g. our covr scripts) does NOT set
# NOT_CRAN automatically; without it, `skip_on_cran()` would skip the
# network-tied tests we want to exercise locally. `devtools::test()`
# sets NOT_CRAN=true itself, so that path is already covered.
#
# Under R CMD check, however, the network/slow tests gated by
# `skip_on_cran()` MUST skip -- they make live API calls to CKAN /
# StatCan / Socrata endpoints that take 30+ s to time out per request
# on Windows runners and hang the whole `checking tests` step.
#
# R CMD check sets `R_TESTS` for every testthat invocation under it,
# so we use that as the discriminator: only set NOT_CRAN=true when
# `R_TESTS` is empty (i.e. not running under R CMD check).
if (!nzchar(Sys.getenv("R_TESTS"))) {
  Sys.setenv(NOT_CRAN = "true")
}
