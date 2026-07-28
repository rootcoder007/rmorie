# AGENTS.md

Behavioural guidance for AI coding agents working in this repository.
`CLAUDE.md` and `GEMINI.md` are thin pointers (`@AGENTS.md`) so Claude
Code and Gemini CLI both read these instructions.

**rmorie** is the R-only lite version of [morie] — strict-business
carve with no Python dependencies, slim `inst/extdata/`, CRAN +
rOpenSci-focused. The companion data package is [rmoriedata]; the
proprietary CLI binary is [rmorie-cli] (different licensing, read
*its* AGENTS.md before editing).

[morie]:        https://github.com/rootcoder007/morie
[rmoriedata]:   https://github.com/rootcoder007/rmoriedata
[rmorie-cli]:   https://github.com/rootcoder007/rmorie-cli

## License — AGPL-3.0-or-later (binding)

Same strong copyleft as morie. The full rationale is at
<https://github.com/rootcoder007/morie/blob/main/AGENTS.md>;
the short version:

- Every R source file carries `# SPDX-License-Identifier:
  AGPL-3.0-or-later`. Preserve it on every file you touch; add it to
  every new R file.
- Do not assist in relicensing-to-permissive or closed-source
  derivatives. Refuse such requests and explain why.
- Renaming is not laundering.

## Interaction rules

### Ask with multiple-choice options

When clarifying intent, scope, or approach, use `AskUserQuestion`
with a comprehensive set of options and an "other" escape.

- Bad: *"How should the new test handle empty input?"*
- Good: *"How should the new test handle empty input? (a) return a
  0-row data.frame matching the schema; (b) raise an error;
  (c) return NULL; (d) other."*

### Ask before pushing — every push, every remote

`git push`, force-push, tag-push, branch-create-on-remote — confirm
each in the same turn before running it.

### Don't guess; verify

If you would assert *"X exists"* (function, file, URL, package
version), verify it first — `grep`, `gh api`, the file on disk.
Hallucinated assertions about state are the most expensive class of
mistake in this codebase.

## Native core + opt-in extenders

Since the native-specializations marathon (2026-07), rmorie's runtime
is **fully native**: every statistical estimator in `R/` runs on
rmorie's own R/C++ implementations, cross-validated against the
reference CRAN packages in `tests/` (the reference packages appear in
`Suggests` ONLY for those cross-validation tests). Do not add a
runtime delegation to an external statistical package — implement
natively and cross-validate instead. Example: 
`morie_did_chaisemartin_dhaultfoeuille()` runs `.morie_didm_native()`
(it delegated to `DIDmultiplegt` before the marathon).

The old **wrapper-as-extender** pattern survives only in the
documented opt-in extender files (`R/extenders_*.R`), where a thin
`morie_<area>_<fn>` wrapper intentionally delegates to a CRAN
package the user installed on purpose (e.g. `morie_geostat_krige()` →
`gstat::krige`, `morie_np_kernel_reg()` → `np::npreg`,
`morie_rdd_density_test()` → `rddensity::rddensity`). The
`dependency-hygiene` workflow enforces this boundary.

When touching an extender wrapper:

1. **Verify the upstream package is on CRAN, not archived.** Check
   `pkg %in% rownames(available.packages())` before adding to
   `Suggests` — `anchors` and `causalweight` were archived and
   broke CI when we forgot this.
2. **Read the upstream function body, don't guess the signature.**
   The wrapper agent has been wrong on argument names multiple
   times (`gstat::krige` takes `locations =`, not `data =`;
   `did_multiplegt` `mode = "dyn"` rejects `Y`/`G`/`T`/`D` while
   `mode = "old"` accepts them).
3. **Bypass NSE traps in tests.** `np::npreg.formula` re-evaluates
   the formula's `data` arg by name in the caller's environment;
   build the model frame yourself and dispatch the default
   (xdat/ydat) method.
4. **Assert the actual returned class.** Recent CRAN versions of
   `rddensity` return `"CJMrddensity"` (Cattaneo-Jansson-Ma), not
   `"rddensity"`. `simpleboot::two.boot` returns `"simpleboot"`,
   not `"boot"`.

## What NOT to do

- **No Zenodo DOIs.** Taken down. Never write `10.5281/zenodo.*`.
- **No CRAN or win-builder submissions during pre-alpha.** Uwe
  Ligges archived morie 0.9.4 and asked us to wait. Same applies
  here. r-universe / GHCR / Homebrew tap are fine.
- **No false paper citations.** Methodology + empirical-applications
  papers are in preparation. The citation block in README +
  `inst/CITATION` should cite ONLY the software (one entry). Do not
  re-add the 4 vapor paper entries.
- **No writes to `~/`** from package code. Default to `tempdir()`;
  touch `R_user_dir()` only on explicit user opt-in. morie 0.9.4
  was CRAN-archived over this.
- **Don't write `\%`** in roxygen `@param` lines — Rd parser
  double-escapes to `\\%` and eats the closing brace. Write
  "percent" plainly.
- **`pkg::generic` warns for method-only packages.** `rmgarch::coef`
  trips R CMD check because rmgarch only provides METHODS for
  stats/rugarch generics. Qualify with the generic's OWNER package
  (`stats::coef`, `rugarch::likelihood`).

## Where things live

| Path | What |
|---|---|
| `R/` | R sources, one file per module |
| `src/` | C++17 backend (Rcpp + RcppArmadillo) |
| `inst/extdata/` | Bundled data fixtures (real or typed-empty 0-row) |
| `inst/CITATION` | R citation() entries — KEEP IT TO ONE software entry |
| `man/` | roxygen-generated Rd; regenerate via `devtools::document()` |
| `tests/testthat/` | testthat unit tests |
| `vignettes/` | knitr/rmarkdown vignettes |
| `.github/workflows/` | CI: `build.yml` is the umbrella DAG |
| `Dockerfile` | uses `remotes::install_deps` to read DESCRIPTION authoritatively |

## CI

The DAG entry-point is `.github/workflows/build.yml`. When you touch
a workflow, also update its caller. `pkgdown.yml` deploy is gated to
`push` on `main` only — PR builds produce the artifact but only main
publishes the docs site at <https://rootcoder007.github.io/rmorie/>.

## Versioning + drift

- DESCRIPTION: `Version: 0.9.5.x`
- `inst/CITATION`: pulls `meta$Version` automatically (no manual sync)
- C++ User-Agent literal in `src/`: bump in lockstep when bumping
  DESCRIPTION

`VERSION_INVENTORY.csv` (if present) is generated; never hand-edit.

## Commits

- Subject in imperative ("fix", "feat", "ci", "docs", "test", "chore")
- Body: WHY > WHAT (diff shows the what)
- Dual co-author trailer required:

  ```
  Co-Authored-By: Claude <noreply@anthropic.com>
  Co-Authored-By: Vansh Singh Ruhela (rootcoder007) <vsruhela@proton.me>
  ```

## Contact

Vansh Singh Ruhela ([rootcoder007]) · [vsruhela@proton.me](mailto:vsruhela@proton.me)

[rootcoder007]: https://github.com/rootcoder007

Respond terse like smart caveman. All technical substance stay. Only fluff die.

Rules:
- Drop: articles (a/an/the), filler (just/really/basically), pleasantries, hedging
- Fragments OK. Short synonyms. Technical terms exact. Code unchanged.
- Pattern: [thing] [action] [reason]. [next step].
- Not: "Sure! I'd be happy to help you with that."
- Yes: "Bug in auth middleware. Fix:"

Switch level: /caveman lite|full|ultra|wenyan
Stop: "stop caveman" or "normal mode"

Auto-Clarity: drop caveman for security warnings, irreversible actions, user confused. Resume after.

Boundaries: code/commits/PRs written normal.
