# R-MORIE <img src="man/figures/logo.png" align="right" height="139" alt="rmorie hex logo" />

<!-- badges: start -->
[![Status](https://img.shields.io/badge/status-active-success.svg)](https://www.repostatus.org/#active) [![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![r-universe](https://rootcoder007.r-universe.dev/badges/rmorie)](https://rootcoder007.r-universe.dev/rmorie) [![CI](https://github.com/rootcoder007/rmorie/actions/workflows/build.yml/badge.svg)](https://github.com/rootcoder007/rmorie/actions/workflows/build.yml) [![Coverage](https://codecov.io/gh/rootcoder007/rmorie/graph/badge.svg)](https://app.codecov.io/gh/rootcoder007/rmorie) [![AGPL-3.0](https://img.shields.io/badge/AGPL--3.0-blue.svg)](https://spdx.org/licenses/AGPL-3.0-or-later.html) [![Website](https://img.shields.io/badge/website-rmorie.com-1d1d1f.svg)](https://rmorie.com) [![Hosted LLM](https://img.shields.io/badge/hosted%20LLM-llm.rmorie.com-0066cc.svg)](https://llm.rmorie.com)
<!-- badges: end -->

R-MORIE (**Multi-domain Open Research and Inferential Estimation**) is an
R package for causal inference, sampling, psychometrics, point-process
modeling, and criminological accountability analysis, with no Python
dependencies.

## Statement of need

Applied observational research on Canadian carceral, policing, and
oversight data usually means stitching together a dozen single-purpose
packages — one for difference-in-differences, one for propensity-score
matching, one for spatial scan statistics, one for self-exciting point
processes — each with its own data contract, and none aware of the
survey-weighting, provenance, and privacy constraints these data carry.
rmorie is for criminologists, quantitative social scientists, and
accountability researchers who need those estimators in one consistent,
provenance-preserving toolkit, with the MRM (Multilevel Reconciliation
Methodology) framework as its motivating application. Every public
function is prefixed `morie_*` so it composes safely alongside the
specialist packages it wraps, and results carry the labeling that keeps
synthetic development runs from being mistaken for inferential findings.
Primary methodological references for each estimator family are listed
in the package-level help (`?rmorie`).

## Documentation

- **Reference manual** (all `morie_*` functions, one searchable doc):
  <https://rootcoder007.r-universe.dev/rmorie/doc/manual.html>
- **Package website** (browsable, with articles):
  <https://rootcoder007.github.io/rmorie/>
- **r-universe project page**: <https://rootcoder007.r-universe.dev/rmorie>
- **Website**: <https://rmorie.com> — the MORIE family (rmorie, morie, rmoriebricklayer, rmoriedata) in one place.
- **Hosted LLM tier**: <https://llm.rmorie.com> — the fallback model behind `morie_llm_ask()` when there is no local Ollama. Sign in with `morie_llm_login()` (GitHub) or `morie_llm_login(email = "you@example.com")`, or from the shell after `install_cli()`: `rmorie login`. The tier serves ollama.com cloud models and additional AI models (kimi-k2.6:cf, kimi-k2.7-code:cf, deepseek-v4-pro:cf, deepseek-v4-flash:cf, glm-5.2:cf, glm-5.3:cf, glm-5.3-flash:cf, gpt-oss-120b:cf, gpt-oss-20b:cf, llama-4-scout:cf, qwen3.8-27b:cf, nemotron-3-120b:cf and gemma-4-26b:cf); when a cloud model is rate limited or down the gateway answers from one of the additional models, so `morie_llm_models()` is the list to trust.

> With over 13,000 exported functions, the full reference is large — use the
> manual or the package site above rather than scrolling the function
> index. This README covers install + the most common workflows only.

## Why rmorie?

rmorie is a **self-contained** research toolkit: every statistical
algorithm in the package — matching (nearest-neighbour, Mahalanobis,
exact, CEM, optimal, genetic, cardinality), design-based IPW and
doubly-robust estimation, double machine learning, causal forests and
meta-learners, causal DAGs with identification and refutation, the
full quasi-experimental family (DiD incl. Callaway-Sant'Anna, event
studies, synthetic control and synthetic DiD, RDD, interrupted time
series, IV), item-response theory, geostatistics, digital signal
processing, Hawkes processes, cryptographic hashing with
post-quantum primitives, and the data-access parsers — is implemented
natively in this package's R and C++. rmorie does not call MatchIt,
survey, DoubleML, grf, dagitty, did, fixest, rdrobust, ivreg, psych,
gstat, spdep, signal, wavelets, digest, or openssl at runtime. This
means:

- **No upstream breakage.** A major release of any of those packages
  cannot change rmorie's results. A paper run in 2026 reproduces
  identically in 2030.
- **Validated, not just reimplemented.** Every native engine is
  cross-validated in `tests/cross/` against its reference package —
  to machine precision where the estimand is deterministic (see the
  table below) — and benchmarked in `inst/benchmarks/`.
- **Composable workflows.** Matchers, ATE/CATE/DiD estimators, and
  the DAG pipeline share one class system with common `print()`,
  `summary()`, and reporting methods; the phase-17 composition test
  runs DAG -> identification -> matching -> DML -> refutation ->
  publication table end to end in one suite.
- **Category-integrity guards.** `morie_safe_recode()`,
  `morie_safe_factor()`, `morie_audit_categories()`, and
  `morie_crosstab_verify()` make the silent category-mapping errors
  that have corrupted published disparity analyses structurally
  impossible in an rmorie workflow.

The remaining `Suggests` entries exist ONLY for the cross-validation
tests under `tests/cross/` and as optional accelerators for the
parsers (jsonlite/xml2/arrow fast paths with pure-R fallbacks); no
production statistics path requires any of them.

### Cross-validation at a glance

| Family | Replaces | Validation |
|---|---|---|
| Matching (7 methods) | MatchIt, optmatch, Matching, designmatch | pair-identical or provably better optimum |
| IPW / design-based GLM | survey | svyglm coefficients + SEs to 1e-6 |
| DML (PLR + IRM) | DoubleML/mlr3 | CI-overlap agreement; 40-60x faster |
| Causal forest / meta-learners | grf | CATE agreement; 1.5-2.9x faster |
| DAG identify/estimate/refute | dagitty, DoWhy | adjustment sets == dagitty on every graph tested |
| DiD family (TWFE/event/CS/DR/Bacon/DID-M/feTR) | fixest, did, DRDID, bacondecomp, DIDmultiplegt, TwoWayFEWeights | coefficients, SEs, and influence functions to 1e-8-1e-10 |
| Synth control + SDID | Synth/coresynth | recovers simulated truths; placebo inference built in |
| RDD (IK bw, sharp/fuzzy/kink, McCrary) | rdrobust, rdd | point estimates == rdrobust at fixed h to 1e-8 |
| IV (2SLS/LIML/GMM) + ITS | AER, ivreg, gmm | 2SLS == ivreg + sandwich HC1 to 1e-8 |
| IRT (2PL/GRM/EAP) + psychometrics | psych, mirt | KMO == psych to 1e-8; 2PL vs mirt within 0.1 |
| Geostatistics (variogram/kriging) | gstat, spdep | kriging == gstat to 1e-6; Moran variance == spdep |
| DSP (Butterworth/FIR/Welch/DWT) | signal, wavelets | butter coefficients == signal to 1e-8; DWT perfect reconstruction |
| Hawkes MLE | hawkes | exponential-kernel loglik == hawkes |
| SHA-256/HMAC/PBKDF2 + PQC | digest, openssl | NIST FIPS + RFC vectors bit-for-bit; ML-KEM/ML-DSA/SLH-DSA/HQC via liboqs |
| Parsers (JSON/XML/HTML/Parquet) | jsonlite, xml2, arrow | jsonlite-parity outputs; accelerators optional |
| Weighting family (ps/entropy/CBPS/OW/stabilized/SuperLearner) | WeightIt, CBPS | glm weights == WeightIt to 1e-8; CBPS moments < 1e-6 |
| Modern staggered DiD (Sun-Abraham/Borusyak/did2s) | did2s, didimputation | point estimates within 0.02 of did2s |
| Unified front-ends (morie_did/morie_iv_2sls/morie_rdd) | did, AER, rdrobust | CS overall == did::aggte to 0.1; 2SLS == ivreg to 1e-6 |
| Crim methods (ETAS/multivariate Hawkes/Knox/RTM) | (papers) | recovers simulated truths; Knox permutation calibrated |

## What rmorie is NOT

rmorie is not a wrapper. At runtime it does not call:

- **MatchIt / optmatch / Matching / designmatch** (matching) — replaced by `morie_matching_*`
- **WeightIt / CBPS** (propensity weighting) — replaced by `morie_weight_*`
- **survey** (design-based estimation) — replaced by the native svyglm engine behind `morie_ipw_*` / `morie_ebac_*`
- **DoubleML / mlr3** (double machine learning) — replaced by the native PLR/IRM/PLIV cross-fit engines
- **grf / EconML-style learners** (heterogeneous effects) — replaced by the native causal forest and T/S/X/DR meta-learners
- **dagitty / DoWhy** (DAGs, identification, refutation) — replaced by `morie_dag_*`
- **did / fixest / did2s / didimputation** (modern DiD) — replaced by `morie_did_*` with auto-dispatch to Callaway-Sant'Anna, Sun-Abraham, Borusyak, and Gardner two-stage
- **rdrobust** (RDD) — replaced by `morie_rdd` (IK bandwidth + McCrary + placebo cutoffs bundled)
- **Synth** (synthetic control) — replaced by `morie_synth_control` with built-in placebo inference
- **AER / ivreg** (IV) — replaced by `morie_iv_2sls` with the Staiger-Stock refusal gate
- **psych / mirt** (psychometrics) — replaced by `morie_psymet_*` and `morie_irt_*`
- **gstat / spdep** (geostatistics) — replaced by the native variogram/kriging/GWR stack
- **signal / wavelets** (DSP) — replaced by `rgfir`/`rgiir`/`rgwav` and `morie_dsp_*`
- **hawkes** (point processes) — replaced by the native C++ Hawkes kernel family + `morie_crim_etas` / `morie_crim_hawkes_multivariate`
- **digest / openssl** (hashing/KDF) — replaced by the native C++ SHA-2/HMAC/PBKDF2 + liboqs PQC
- **jsonlite / xml2 / arrow as requirements** (parsing) — replaced by `morie_fetch_*` pure-R parsers (those packages remain optional fast paths only)

Those packages appear in `Suggests` solely so `tests/cross/` can
prove, on every CI run, that the native engines match them.

## What's in v1.4.0

The Lean-backed research programme grows by four problems, each with its R
function and tests: pooling evaluations (DerSimonian-Laird), separation in a
logistic fit, Imbens-Manski intervals for partially identified sentencing
effects, the Cheeger bound on a hot-spot boundary, Duncan-Davis bounds, monotone
treatment selection, the extinction probability of a near-repeat chain, judge-leniency
designs, the Oaxaca-Blinder decomposition, Little's law on a docket and the
incapacitation identity. The Health Infobase tables and the OTIS research
environments are served from data.rmorie.com. The Python package carries every
research function at parity. Seven continuations close the Lean ledger (264
theorems, 0 sorry), the 36 findings from the 1.3.9 stress test are fixed with
tests, the 116 example topics that emitted warnings run clean and the launcher
works on R 4.6.

## What's in v1.3.9

Hotfix: module runs fall back to rmoriedata's synthetic CPADS PUMF, the
first-paper template ships, sources are ASCII-only.

## What's in v1.3.8

Every verb of the Python command line now has an R twin (`rmorie selftest`,
`pipeline`, `emissions`, `verify-pollution`, `crypto`, `ingest`, ...). The
pollution-health module and a compute-emissions tracker with a C++ background
sampler and signed bricklayer capsules are native R.

## What's in v1.3.7

The command line gains `list-modules`, `run-module`, `list-datasets`, `pull`,
`cheatsheet` and `provider` (attach your own OpenAI-compatible model endpoint,
shared with the Python package).

## What's in v1.3.6

Version lockstep with the Python package's 1.3.6 hotfix; no change to the R
package.

## What's in v1.3.5

CRAN follow-ups (example widths and costs, `research/` out of the tarball)
and a test harness that runs in one process under covr and on Windows, with
a time budget on R-universe's slow Intel mac runner. No change to any
function.

## What's in v1.3.4

- **5,000+ exported functions, every one tested** — 5,189 `morie_*` entry
  points (13,655 exports in all), each with a test that recomputes its
  value, and the same names in the Python package `morie`, checked against
  each other in CI.
- **Native causal-inference engines** — matching, double machine learning,
  causal forests, meta-learners, design-based GLM and the `morie_dag*`
  toolkit, cross-validated against MatchIt, DoubleML, grf and dagitty.
- **Hosted LLM tier** — `morie_llm_ask()` uses a local Ollama first and
  falls back to <https://llm.rmorie.com>; sign in with
  `morie_llm_login()` (GitHub or an emailed code). Gemini, an
  OpenAI-compatible endpoint and OpenAI keys are honoured after that; a
  keyword fallback needs no network.
- **Command line inside the package** — `install_cli()` puts `rmorie` on
  your PATH: `rmorie login` (GitHub), `rmorie login --email you@example.com`
  (no GitHub account: a code is emailed to you), `rmorie doctor`, `rmorie ask`,
  `rmorie analyze`.
  On a server, over SSH or with no browser, `rmorie login --no-browser`
  prints a link and a code: open it on any phone or laptop.
- **Criminology research program** — the open-problems ledger under
  `research/`, implemented as package functions with Lean 4 proofs of the
  identification results.
- **SIU subsystem** — the Ontario Special Investigations Unit
  director's-report corpus (English + French, 2005-present). See *SIU
  pipeline* below.
- **Built-in datasets** through `rmoriedata` (CRAN), fetched with
  provenance by `rmoriebricklayer` (CRAN).
- **CPADS contract helpers**, IPW / eBAC workflows, outputs-manifest
  tooling, synthetic data generators, a C/C++ backend for the Hawkes
  likelihoods and the SIU parser, the causal-taphonomy suite, and the
  A2AJ / CanLII Canadian legal-data ingest.
- **`agent()`** — call the rmorie CLI agent from R (with
  `agent_available()` to probe for the binary).

## Scientific guardrail

- Synthetic data is for development, testing, demos, and CI only.
- Final inferential or policy-facing results must be produced from
  approved real data with full provenance.
- Synthetic runs must be explicitly labeled as synthetic in outputs
  and reporting text.

## Install

From a clone of this repository:

```r
install.packages(".", repos = NULL, type = "source")   # from the repository root
# or, without cloning:
# remotes::install_github("rootcoder007/rmorie")
```

From r-universe (development snapshot):

```r
install.packages(
  "rmorie",
  repos = c(rootcoder007 = "https://rootcoder007.r-universe.dev",
            CRAN         = "https://cloud.r-project.org")
)

# want every optional package too, in one shot? add dependencies = TRUE
# (large: compiles many specialist packages — see "Optional packages" below)
install.packages(
  "rmorie", dependencies = TRUE,
  repos = c(rootcoder007 = "https://rootcoder007.r-universe.dev",
            CRAN         = "https://cloud.r-project.org")
)
```

The assistant (`morie_llm_ask()`) tries a local Ollama, then the hosted
MORIE tier at <https://llm.rmorie.com> once you have signed in with
`morie_llm_login()`, then any Gemini or OpenAI-compatible key in the
environment, and finally a keyword fallback that needs no network.

### Optional packages (the R equivalent of `pip install pkg[extra]`)

The base install is complete for every statistical workflow — the
native engines need nothing beyond the hard dependencies. `Suggests`
entries are (a) reference packages used only by the cross-validation
tests in `tests/cross/`, and (b) optional accelerators (e.g.
jsonlite/xml2/arrow fast paths for the parsers, ML backends). Every
optional-path function tells you what to install when it's missing,
and the test suite skips (never fails) without them. To provision the
extras up front:

```r
# install every optional package rmorie can use (one-time, ~15 min)
morie_install_extras(which = "all", ask = FALSE)

# or just what's missing, interactively
morie_install_extras()

# or a specific family, e.g. machine learning
morie_install_extras(which = c("randomForest", "glmnet", "xgboost",
                               "ranger", "caret", "pROC"))
```

Common families: ML (`randomForest`, `glmnet`, `xgboost`/`gbm`,
`ranger`, `caret`, `pROC`, `Rtsne`, `e1071`, `dbscan`), DSP
(`signal`, `pracma`, `wavelets`), causal (`DoubleML`, `mlr3`,
`mlr3learners`, `ivreg`, `fixest`), storage (`RSQLite`, `duckdb`).

## Outputs-manifest example

```r
library(rmorie)

# a project that keeps data/manifest/outputs_manifest.csv (morie_build_outputs_manifest() writes one)
manifest <- morie_read_outputs_manifest(project_root = "/path/to/project")
audit    <- morie_audit_public_outputs(project_root = "/path/to/project",
                                       manifest     = manifest)
morie_summarize_output_audit(audit)
```

## Synthetic data example

```r
library(rmorie)

synthetic_path <- morie_write_synthetic_data(
  path      = "data/private/synthetic_study_data.csv",
  n         = 8000,
  seed      = 2026,
  overwrite = TRUE
)
```

## Cross-project adaptation

```r
library(rmorie)

name_map <- morie_default_synthetic_name_map("generic")
name_map["cannabis_use"] <- "exposure_any"
name_map["bac"]          <- "outcome_continuous"

dat <- morie_generate_synthetic_data(
  n        = 5000,
  seed     = 1,
  name_map = name_map
)
```

## SIU pipeline

rmorie ships the **first open-source parser and data-mining subsystem
for the Ontario Special Investigations Unit (SIU) director's-report
corpus** — created by Vansh Singh Ruhela as part of the MORIE / R-MORIE
ecosystem and the MRM (Multilevel Reconciliation Methodology)
framework. To our knowledge no prior public, open-source SIU
director's-report parser or automated SIU data-mining engine existed
in Canada; the SIU publishes the reports, but there was no
programmatic pipeline to fetch, parse, and analyse them at corpus
scale until this one.

A first-class subsystem for the Ontario Special Investigations Unit
director's-report corpus. The fetcher handles both English and French
templates from 2005 onward across all three of the site's historical
layout generations (pre-2017 Attorney-General releases, the 2017-2019
transitional format, and the post-2019 SIU Act template); the parser
extracts the report fields (police service, incident/notification/
decision dates, investigator and witness/subject-official counts,
affected-person demographics, injuries, legislation, charges verdict,
and director's decision) and is hand-rolled for correctness under
SIU's heterogeneous markup.

Since 1.1.4 the subsystem is layered on the compiled SIU core in
'rmoriebricklayer' and the **verified corpus** shipped by
'rmoriedata': a 65-column table of 5,157 reports whose 2,182 English
entries were read and cross-audited by a multi-agent review panel
(every subject-official count verified; the mechanical resolver
scores zero wrong against it). `morie_siu_reports()` returns that
corpus verbatim and only ever fetches/parses reports newer than it;
`morie_siu_resolve_so(report_text)` resolves the subject-official count
from a report's text, the verified corpus first, then the compiled rule engine; `morie_siu_panel()` runs the
same Mixture-of-Agents reading panel on new reports through any
Ollama-compatible endpoint you point it at (your models, your host —
no hardcoded default).

The subsystem powers downstream MRM analyses — Hawkes self-exciting
point processes, causal estimators, fairness audits, and the physics-
of-crime modules — on Canadian police-oversight data.

Key entry points: `morie_siu_reports()`, `morie_siu_resolve_so()`,
`morie_siu_panel()`, `morie_siu_parse_report()`, plus the legacy
fetch path `morie_fetch_siu()`, `morie_siu_index_url()`,
`morie_siu_refresh_manifest()`, `morie_siu_audit_case()`,
`morie_siu_sanity_check()`, `morie_siu_all_analyses()`.

### Fetch and parse the full corpus

```r
library(rmorie)

# Prefer the panel-verified corpus (5,157 reports x 65 columns,
# 2,182 English entries human+multi-agent reviewed) — no re-fetching:
df <- morie_siu_reports(update = FALSE)

# The legacy live fetch remains available:
# df <- morie_fetch_siu(lang = "en", cache_html = TRUE, rate_limit = 4)
nrow(df)
```

### Audit a single case

```r
# Inspect parser row + raw HTML + cleaned text side-by-side.
morie_siu_audit_case("17-OVI-201")

# Per-field "does the HTML actually support this value?" check.
morie_siu_anomaly_check("17-OVI-201")

# Diff parser output against an external table.
# field_map maps the external table's columns to parser fields; external_case_col names its case column
morie_siu_compare(
  case_number       = "17-OVI-201",
  external          = my_other_table,          # e.g. data.frame(case_number = "17-OVI-201", n_officers = 2L)
  field_map         = list(n_officers = "number_of_officers_involved"),
  external_case_col = "case_number"
)
```

### AI extraction (free local model by default)

```r
# Default: local Ollama with gemma3:4b. No API key required.
morie_siu_llm_extract("17-OVI-201")

# Failover chain: local first, then the hosted MORIE tier (after `rmorie login`, GitHub or --email), then Gemini.
morie_siu_llm_extract("17-OVI-201", model = c("ollama", "hosted", "gemini"))

# French to English translation of the cached reports (field by field, via the local model).
morie_siu_translate(target_lang = "en", case_numbers = "26-OCI-168")
```

Supported providers: `ollama` (default), `hosted` (llm.rmorie.com), `gemini`, `claude`, `vertex`,
`openai`, `openai_compatible`. Environment knobs: `OLLAMA_HOST` (defaults to `http://localhost:11434`),
`OLLAMA_MODEL` (defaults to `gemma3:4b`), `OLLAMA_KEEP_ALIVE` (`30m`), `GEMINI_API_KEY`.

### Format-validity sweep

```r
df   <- morie_siu_reports()        # the reviewed corpus, from rmoriedata (CRAN)
sane <- morie_siu_sanity_check(df)  # one row per report, with its format issues (if any)
table(sane$issues_count > 0)        # regex / ISO date / yes-no / chrome leak; older reports leave many fields blank
head(sane$issues[sane$issues_count > 0], 3)
```

### Aggregate accuracy

```r
# How accurate is each column across a sample of cases?
df <- morie_siu_reports()
morie_siu_audit_columns(case_numbers = sample(df$case_number[nzchar(df$case_number)], 50))
```

### Canonical override system

The parser learns. Ship-time corrections live in
`inst/extdata/siu_canonical_overrides.csv.gz` (47 hand-verified
corrections covering 10 spot-checked cases). Users can add their own:

```r
morie_siu_record_correction(
  case_number    = "20-OFD-082",
  field          = "number_of_officers_involved",
  verified_value = "3 SO"
)
```

Overrides are applied automatically at the end of `morie_fetch_siu()`,
per cell, by case number.

### Inspect the manifest

```r
manifest <- morie_siu_index()
table(manifest$`_language`)  # en=2532, fr=2217, other=0
```

## Continuous integration

The R CMD check matrix covers six cells, all green on `main`:

| Platform        | R version             |
| --------------- | --------------------- |
| macos-latest    | release               |
| windows-2025    | release               |
| ubuntu-latest   | release               |
| ubuntu-latest   | release + postgres-15 |
| ubuntu-latest   | oldrel-1              |
| ubuntu-latest   | devel                 |

Plus: `pkgcheck`, `covr` + Codecov upload, `lintr`, `goodpractice`, and
CodeQL.

## Citation

If you use rmorie in your research, please cite the software:

> Ruhela, V. S. (2026). *rmorie: Multi-domain Open Research and Inferential Estimation in R.* https://github.com/rootcoder007/rmorie

BibTeX (or run `citation("rmorie")` after installation for the entry
stamped with the exact installed version, sourced from `inst/CITATION`):

```bibtex
@Manual{ruhela_rmorie_2026,
  title   = {rmorie: Multi-domain Open Research and Inferential Estimation in R},
  author  = {Ruhela, Vansh Singh},
  year    = {2026},
  url     = {https://github.com/rootcoder007/rmorie}
}
```

See [`CITATION.cff`](https://github.com/rootcoder007/rmorie/blob/main/CITATION.cff)
for the machine-readable metadata GitHub's "Cite this repository" button uses.

## License

R-MORIE is licensed under **AGPL-3.0-or-later**. See `LICENSE` for the
full text and `LICENSING.md` for the per-component breakdown.

## Bayesian priors

rmorie's Bayesian regression (`morie_bayes_lm`) places zero-mean Normal
priors on the regression coefficients; the `prior_sd` argument is the
prior standard deviation (the scale of plausible coefficient values).
Larger `prior_sd` is weakly informative; smaller values pull estimates
toward zero (regularisation). Example:

```r
d <- data.frame(x = rnorm(100)); d$y <- 1 + 2 * d$x + rnorm(100)
# weakly-informative prior (sd = 10) vs a tight regularising prior (sd = 0.5)
fit_weak  <- morie_bayes_lm(y ~ x, d, prior_sd = 10)
fit_tight <- morie_bayes_lm(y ~ x, d, prior_sd = 0.5)
```

See the **bayesian-priors** vignette for applied guidance.
