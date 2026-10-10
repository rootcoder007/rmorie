You are one of 16 parallel workers raising test coverage of the R package **rmorie** (repo rootcoder007/rmorie, AGPL-3.0-or-later) toward ~100%. You own SHARD __N__ only. Read AGENTS.md first and follow it, except: you are authorised to push your own branch (below) without asking.

## Setup
1. The repo is checked out at branch `update` (commit 562c8876 or later). Create and switch to branch `cov/shard-__N__`.
2. R is installed (Ubuntu noble). Install the package's dependencies from prebuilt binaries into a user library:
   ```
   export R_LIBS_USER=$HOME/rlib; mkdir -p $R_LIBS_USER
   Rscript -e 'options(repos = c(P3M = "https://packagemanager.posit.co/cran/__linux__/noble/latest", rmorie = "https://rootcoder007.r-universe.dev")); install.packages(c("remotes","testthat","covr","lintr","withr")); remotes::install_deps(".", dependencies = TRUE, upgrade = "never")'
   MAKEFLAGS=-j$(nproc) R CMD INSTALL --with-keep.source .
   ```
   Some Suggests may fail to install; that is fine (their tests skip).

## Your files
These files (from Codecov's report on commit 1c47d7fb, which counts 100,307 missed lines across the package) are yours:

__FILES__

Codecov's missed lines per file are public: `curl -s "https://api.codecov.io/api/v2/github/rootcoder007/repos/rmorie/file_report/<path>?sha=1c47d7fb360a92bec2ed3c672db91da9b066d242"` returns `line_coverage` as `[line, status]` pairs, status 1 = missed. Files changed since that commit (notably R/gam_native.R, R/lmm_native.R, R/rq_native.R, R/nls_native.R, R/effects_native.R, R/causal_impact_native.R, R/llm*.R, R/cli_entry.R and any file `git diff --stat 1c47d7fb HEAD` lists) have shifted line numbers: for those, find the uncovered lines yourself with covr (below).

## The work
For each file, write tests that execute its uncovered lines, in NEW files named `tests/testthat/test-cov-<file basename without extension>.R` (one per source file; never edit existing test files, never touch another shard's files).

Measuring: `covr::file_coverage(source_files = "R/x.R", test_files = "tests/testthat/test-cov-x.R", parent_env = asNamespace("rmorie"))` instruments only that file, fast. In the new tests call the file's functions **unqualified** (`f(...)`, not `rmorie::f(...)`) so the instrumented copies run; internal (dot) functions are reachable unqualified because testthat runs tests inside the package namespace. Aim for every line of every file covered. `covr::zero_coverage()` lists what remains.

C++ files (`src/*.cpp`): cover them by calling the R functions that reach them (R/RcppExports.R names the `.Call` wrappers); you cannot measure them with file_coverage, so target the branches by reading the code.

Rules for the tests (CRAN will run them):
- Real assertions on real behaviour (values, classes, dimensions, error messages via `expect_error(..., "pattern")`, warnings via `expect_warning`). A test that only calls a function without checking anything is not acceptable. Use small inputs and `set.seed()`.
- No network: mock with `testthat::local_mocked_bindings()` (mock the parser's expected shape, per AGENTS.md). No writes outside `tempdir()` (`withr::local_tempdir()`); never touch `~`.
- Optional packages: `skip_if_not_installed("pkg")` only around the lines that need them.
- Keep each new test file fast (well under ~30 s total; the whole suite already takes about an hour on CI). Gate anything slower with `skip_on_cran()`.
- Every new file starts with `# SPDX-License-Identifier: AGPL-3.0-or-later`.
- No `skip()` used to dodge a failing assertion, and no deleting or weakening existing tests.
- `# nocov start` / `# nocov end` only for code that genuinely cannot run in a test (an interactive-only prompt, a branch for another OS, a defensive branch provably unreachable). Keep these rare and list every one in your final report with the reason.
- If a test exposes a real bug in R/ or src/, fix it minimally in that same file (only files in your list), add the regression test, and report it. Do not refactor, rename or reformat source.
- Never use the word "battery" (use "suite", "set" or "panel").
- Lint your new tests: `Rscript -e 'lintr::lint("tests/testthat/test-cov-x.R")'` must be clean (the repo's .lintr applies).
- Run each new test file against the installed package: `Rscript -e 'testthat::test_file("tests/testthat/test-cov-x.R", package = "rmorie", load_package = "installed")'` with zero failures. If you fixed source, reinstall first.

## Committing and pushing
- Commit in batches (e.g. every ~10 files) as the repository owner, with no trailers at all: `git -c user.name="Vansh Singh Ruhela" -c user.email="278967282+rootcoder007@users.noreply.github.com" commit -m "test: cover <area>"`. No Co-Authored-By, no Claude-Session line, no session URL anywhere (commit messages, code, comments).
- Push only to `cov/shard-__N__` (`git push -u origin cov/shard-__N__`), after each batch, so work survives. Never push to `update` or `main`, never force-push anything but your own branch, open no pull request.

## When done
Reply with a short report: files completed, files left (and why), approximate lines newly covered, every `nocov` with its reason, every source bug fixed (file:line, what), and the final commit SHA on `cov/shard-__N__`.
