## Submission

This is morie 0.9.5.

morie is a multi-domain toolkit for observational inference and
intervention analysis, hosting the MRM (Multilevel Reconciliation
Methodology) framework for Canadian carceral, police, and oversight
data as its primary application.

morie 0.9.4 was archived on CRAN after Prof. Uwe Ligges flagged
that the package created `~/.cache/morie` -- a violation of CRAN
Policy. 0.9.5 fixes that, plus everything else flagged in the
parallel rOpenSci package review (issue #770). The package has been
held back from re-submission until the policy fix was verified end-to-end.

## CRAN-Policy fix in 0.9.5 (the cause of the 0.9.4 archival)

* `morie_cache_dir()` no longer returns `~/.cache/morie`. It now
  returns `tools::R_user_dir("morie", which = "cache")` (R-Project
  sanctioned, allowed under the CRAN Policy for R >= 4.0). Users can
  override the location via the `MORIE_CACHE_DIR` environment
  variable. `DESCRIPTION` already declares `Depends: R (>= 4.3.0)`.
* All persistent caching is now strictly opt-in. Every morie function
  that can write to disk (`morie_fetch_siu`, `morie_fetch_tps`, and
  the SIU audit helpers) now defaults `cache_dir` to a session-scoped
  subdirectory of `tempdir()`. R cleans that subdirectory up
  automatically when the session ends, so the package by default
  never persists anything outside `tempdir()`.
* Users who want cross-session caching opt in explicitly by passing
  `cache_dir = morie_cache_dir(<subdir>)` to the function. The
  `morie_cache_dir()` Rd documents this contract in full.
* New exported function `morie_cache_clear(subdir = NULL,
  confirm = interactive())` lets users actively manage the persistent
  cache (CRAN Policy explicitly requires "active management" for
  caches stored via `tools::R_user_dir()`).
* No `\donttest{}` example writes outside `tempdir()`. The five
  examples that hit external services (SIU website, Gemini, Ollama)
  are wrapped in `\dontrun{}` -- they are genuine external-service
  examples, not boilerplate.
* Tests previously exercising the `XDG_CACHE_HOME` override path were
  updated to exercise the new `MORIE_CACHE_DIR` override and
  `tools::R_user_dir()` default.

## Other changes in 0.9.5 (cleared in parallel with the CRAN fix)

* Full rOpenSci #770 cleanup: `CONTRIBUTING.md`, all 16 functions
  previously missing `@return` documented, full roxygen2 conversion
  (`RoxygenNote: 7.3.3`), all 15 functions previously missing
  `@examples` covered, coverage raised from 21% to 95.3% (verified by
  `covr::package_coverage()`), 352 unprefixed exported functions
  renamed to a `morie_*` prefix to clear inter-CRAN name collisions.
* Toronto Police Service (TPS) open-data ingestion fixes carried over
  from the original 0.9.5 plan: catalog date ranges for Homicides /
  Shootings (now `2004-present`), `morie_fetch_tps()` ArcGIS
  pagination follows `exceededTransferLimit` flag, daily-resolution
  Hawkes fits build occurrence date from local-time
  `OCC_YEAR`/`OCC_MONTH`/`OCC_DAY` rather than the UTC `OCC_DATE`.
* `T -> T_horizon` rename in the Hawkes C++ likelihood so the
  auto-generated `R/RcppExports.R` no longer trips `lintr`'s
  `T-as-TRUE-shadow` rule.
* `setwd()` in `morie_run_workflow_step()` replaced with
  `withr::local_dir()`.
* New SIU subsystem: a hand-rolled C++ parser for the Ontario Special
  Investigations Unit director's-report corpus, English + French
  template families from 2005 to the present, a polite token-bucket
  HTTP fetcher (4 RPS default, exponential backoff on 429/5xx), a
  language-aware DRID manifest (4,743 drids; en=2,531, fr=2,212), a
  canonical-override system that lets the parser learn from
  hand-verified corrections (ships with 47), and audit / sanity-check
  / AI-extraction / translation helpers (ollama default, optional
  Gemini / Claude / Vertex).

See `NEWS.md` for the full changelog.

## Test environments

* local macOS 26 (Darwin 25.4.0), R 4.6.0 -- `R CMD check --as-cran`:
  0 ERROR, 0 WARNING, 1 NOTE (the standard "new submission" note).
* GitHub Actions, 6-cell matrix (all green on
  `release/v0.9.5-audit` HEAD):
  * macos-latest (release)
  * windows-2025 (release)
  * ubuntu-latest (release)
  * ubuntu-latest (release + postgres-15)
  * ubuntu-latest (oldrel-1)
  * ubuntu-latest (devel)
* rOpenSci `pkgcheck`: 0 errors, 0 warnings (the one prior warning,
  "inconsolata.sty not found" on the pkgcheck job's internal
  rcmdcheck, is fixed by installing tinytex + inconsolata in the
  workflow).
* win-builder R-devel / R-release / R-oldrelease: tarball submitted
  (3 jobs, results emailed to the maintainer).
* r-universe (Linux + macOS + Windows binaries): the `hadesllm`
  r-universe builds morie continuously; `R CMD check` reports OK on
  the linux-devel x86_64 and arm64 runners.

## R CMD check results

`R CMD check --as-cran` on the 0.9.5 source tarball (local macOS 26,
R 4.6.0):

```
Status: 0 ERROR, 0 WARNING, 1 NOTE.
```

The single NOTE is the standard CRAN-incoming feasibility check:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Vansh Singh Ruhela <hadesllm@proton.me>'
New submission
Package was archived on CRAN
CRAN repository db overrides:
  X-CRAN-Comment: Archived on YYYY-MM-DD as written to user HOME.
```

This is expected -- 0.9.5 is the resubmission with the
`~/.cache/morie` HOME-write violation fixed. See the "CRAN-Policy
fix in 0.9.5" section above for the full account.

## Compiled code

morie 0.9.1 introduced a shared C++ computational backend
(`libmorie`) -- a header-only numeric core compiled into the package
through Rcpp (`LinkingTo: Rcpp`). It is standard, portable C++ with no
system dependencies beyond a C++ compiler, and it builds on the
r-universe Linux, macOS, and Windows runners. The same core also
backs the Python companion package, so the two language sides share
one numerical implementation.

## Reverse dependencies

There are no reverse dependencies on CRAN.

## Dependencies

There are no hard runtime dependencies beyond the base + recommended
set plus `Rcpp`. Every other package -- spatial, time-series, machine
learning, psychometric, and signal-processing tooling -- is declared
in `Suggests` and gated with `requireNamespace(..., quietly = TRUE)`
in the function bodies, so the package loads and checks without them.

## Vignettes

Vignettes are pre-built and shipped in `inst/doc/`; they rebuild
cleanly under `R CMD build`. Network-touching SIU vignette chunks are
`eval = FALSE` so `R CMD check` never hits the SIU server during
build.

## DESCRIPTION authorship fields

`DESCRIPTION` carries both a modern `Authors@R:` field and an explicit
`Author:` field. The explicit `Author:` field is retained for
compatibility with the stricter author parsing introduced in R 4.6.0;
both fields agree on the maintainer and author identity.

## License

The package is licensed `AGPL-3` (`AGPL-3.0-or-later`) on both
language sides -- Python on PyPI and R here. The Linux-kernel adjuncts
shipped in the GitHub repository's `kernel-module/` and `daemon/`
directories remain `GPL-2.0-only` (a Linux kernel ABI requirement) and
are not part of the CRAN source tarball.

The deprecated `moirais` alias package is not part of this
submission; users install and upgrade through the canonical `morie`
package.

## AI-assistance disclosure

morie was developed with substantial assistance from Anthropic's
Claude family (used via Claude Code, Anthropic's official CLI agent)
and from Google DeepMind's Gemini 2.5 (Pro and Flash) on the Vertex AI
platform. Both providers extended research-credit programmes. Full
citations ship in the package's `CITATION.cff` and in the companion
papers' `.bib` files (bibkeys: `Anthropic2024ClaudeFamily`,
`Anthropic2024ClaudeCode`, `Bai2022ConstitutionalAI`,
`GoogleDeepMind2024Gemini`, `GoogleCloud2024VertexAI`). The author
retains full responsibility for the code, the methodology, the
empirical findings, and the published text; AI assistance accelerated
implementation, not authorship.
