# Contributing to rmorie

Thanks for considering a contribution. rmorie is AGPL-3.0-or-later.

## Quick start

```bash
git clone https://github.com/rootcoder007/rmorie.git
cd rmorie
R -e 'devtools::install_deps()'
R -e 'devtools::test()'        # 13,000+ tests
R -e 'devtools::check()'       # R CMD check
```

## House rules

1. **No Python deps**. rmorie is pure-R (with C++ via Rcpp). No `reticulate`,
   no `system2("python", ...)`, no Python-only Suggests.
2. **HTTPS only**. Any network call uses `https://` URLs; `http://` is a
   security review fail.
3. **No `eval(parse())` of remote strings**, no `system()` with
   network-derived arguments.
4. **CRAN-compliant**. No writes outside `tempdir()` without user opt-in;
   no `~/.cache` defaults.
5. **rOpenSci style**. `goodpractice::gp()` clean before PR.

## PR checklist

- `devtools::check()` passes with 0 errors, 0 warnings.
- New code has tests in `tests/testthat/`.
- New exported functions have roxygen docs with `@param`, `@return`,
  `@examples`.
- `lintr::lint_package()` is clean.
- A NEWS.md entry under `# rmorie 0.x.y.z (in development)`.

## Reporting bugs

Public bugs: https://github.com/rootcoder007/rmorie/issues
Security: see SECURITY.md


## Self-sufficiency policy (enforced by CI)

rmorie is a self-contained statistical package. Pull requests that
add a runtime dependency (Depends/Imports/LinkingTo) on MatchIt,
survey, DoubleML, grf, dagitty, rdrobust, did, fixest, DRDID,
bacondecomp, DIDmultiplegt, TwoWayFEWeights, HonestDiD, coresynth,
AER, ivreg, gmm, plm, psych, mirt, gstat, spdep, signal, wavelets,
hawkes, emhawkes, bsts, xml2, jsonlite, arrow, httr, httr2, digest,
or openssl will be rejected — the `dependency-hygiene` CI job blocks
them structurally, and also blocks `pkg::` calls to those packages
anywhere in production `R/` (the only exceptions are the documented
requireNamespace-guarded accelerator shims for jsonlite/xml2/arrow).

If you need a method currently living in one of those packages, the
right path is to add it NATIVELY: its own `R/` engine file, unit
tests, a cross-validation file under `tests/cross/` against the
reference implementation, and a benchmark under `inst/benchmarks/`.
See `BRANCH_PLAN.md` for the established per-module pattern, and
include a short "what makes ours special" paragraph in the PR body —
if that paragraph cannot be written, the module is not ready.

Every PR must add a line to `NEWS.md`.
