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
# R_TESTS is NOT a usable discriminator: R sources it during startup and
# then UNSETS it, so by the time this file runs it is always empty and
# this branch always fired -- setting NOT_CRAN=true even under R CMD
# check, which made every skip_on_cran() in this suite a no-op
# everywhere, including on CRAN and r-universe. That is why the slow and
# network-tied tests this comment describes have never actually skipped.
#
# _R_CHECK_PACKAGE_NAME_ does survive, and is what R/siu.R already uses
# for the same decision.
if (!nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
  Sys.setenv(NOT_CRAN = "true")
}

# Deterministic ollama probing across the whole suite: force the cached
# result FALSE so no provider-detection test ever hits the network.
# (The OllamaFreeAPI provider was removed 2026-07 -- the community registry
# was chronically down, 1/59 hosts live, and its parallel probe hung R CMD
# check examples on CI runners.)
options(morie.llm.ollama_cached = FALSE)

# Heavy-test gate.
#
# skip_on_cran() proved unreliable here: NOT_CRAN ends up "true" in this
# suite through more than one route, so the calls became no-ops and a
# measured run produced zero "On CRAN" skips. _R_CHECK_PACKAGE_NAME_ is
# the discriminator this package already relies on -- R CMD check sets
# it, nothing else does, and R/siu.R uses it for exactly this purpose.
#
# Skip when a reference check is running and NOT_CRAN has not been set
# deliberately. Our own workflows set NOT_CRAN=true, so the heavy files
# still run there; r-universe and CRAN do not, so they skip.
skip_heavy <- function() {
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
        !identical(Sys.getenv("NOT_CRAN"), "true")) {
    testthat::skip("heavy: outside our own CI's time budget")
  }
  invisible(TRUE)
}
