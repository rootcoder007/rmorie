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
