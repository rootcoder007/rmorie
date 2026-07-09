# cran-comments.md — rmorie 1.0.3

## Submission

First CRAN submission of rmorie (Multi-Domain Open Research and
Inferential Estimation): causal inference, sampling, psychometrics,
point-process modeling, and criminological accountability analysis,
hosting the MRM (Multilevel Reconciliation Methodology) framework for
Canadian carceral, police, and oversight data.

rmorie Imports `rmoriebricklayer` and Suggests `rmoriedata`, both
submitted to CRAN ahead of this package. `Additional_repositories`
covers `cmdstanr` (stan-dev r-universe); `qvalue` is on Bioconductor.

## History with CRAN

An ancestor package (`morie` 0.9.4) was archived in 2026 after
Prof. Uwe Ligges flagged writes to `~/.cache` — a CRAN Policy
violation. rmorie was rebuilt around that lesson:

* `morie_cache_dir()` resolves to `tools::R_user_dir("morie", "cache")`
  only on explicit user opt-in; every default writes under `tempdir()`.
* The test suite forces `MORIE_CACHE_DIR` to a session `tempfile()`
  so `R CMD check` leaves nothing behind.
* All network-tied tests are gated behind `skip_on_cran()`; examples
  and vignettes run fully offline on bundled fixtures.

## Test environments

* Local: Debian (aarch64), R 4.5.x — R CMD check --as-cran
* GitHub Actions: ubuntu-latest (release + devel), ubuntu-22.04
  (oldrel-1), windows-latest, macos-latest
* win-builder (R-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* "New submission".

## Notes for the reviewers

* The package intentionally exports a large `morie_`-prefixed API
  (documented in full at
  <https://rootcoder007.github.io/rmorie/>); the prefix avoids any
  namespace collision with existing CRAN packages.
* `configure`/`cleanup` probe libcurl and libsodium and generate/remove
  `src/Makevars`; the package installs (with reduced crypto features)
  when libsodium is absent.
