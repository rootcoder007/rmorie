# morie 森 — The Fates' Forest

<!-- badges: start -->
[![R-CMD-check](https://github.com/hadesllm/morie/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/hadesllm/morie/actions/workflows/r-cmd-check.yml)
[![codecov](https://codecov.io/gh/hadesllm/morie/branch/main/graph/badge.svg)](https://app.codecov.io/gh/hadesllm/morie)
[![License: AGPL v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![rOpenSci review](https://img.shields.io/badge/rOpenSci-under_review_%23770-orange)](https://github.com/ropensci/software-review/issues/770)
<!-- badges: end -->

`morie` is a dual-language (R + Python) scientific computing package for
causal inference, sampling, psychometrics, point-process modeling, and
criminological accountability analysis. The name is a compound: the Greek
*Moirai* (/ˈmɔɪraɪ/, "MOY-rye", the three Fates) paired with the
Japanese **森** (/moɾi/, "MOH-ree", "forest" — three trees stacked into
one ideogram). It expands to **Multi-domain Open Research and
Inferential Estimation**.

## What's in v0.9.5

- **559 exported `morie_*` R functions** — every public callable is now
  prefixed to avoid name collisions with other CRAN packages
  (`morie_chi_square_test`, `morie_kmeans_clustering`,
  `morie_decision_tree_split`, etc.). The companion `morie.fn` Python
  library mirrors these for cross-language parity.
- **SIU subsystem** — a full pipeline for the Ontario Special
  Investigations Unit director's-report corpus (English + French,
  2005-present). See *SIU pipeline* below.
- **Free-first AI helpers** — local Ollama by default
  (`gemma3:4b`, `translategemma:latest`), with optional Gemini, Claude,
  or Vertex AI fallback. No paid API key is required for the default
  workflow.
- **Polite-by-default HTTP fetcher** — token-bucket throttling at 4
  req/s, exponential backoff on 429/5xx, on-disk page cache.
- **Built-in datasets** — 41 datasets accessible through the shared
  SQLite store (`morie_datasets.db`), plus the SIU manifest (4,743
  drids, 2,218 unique cases, language-classified).
- **CPADS contract helpers** and IPW / eBAC workflow functions.
- **Outputs-manifest tooling** — read, validate, audit, and build
  `outputs_manifest.csv` tables for reproducible research projects.
- **Synthetic data generators** for development and CI.
- **C/C++ computational backend** — Hawkes self-exciting point process
  likelihood (Markovian + non-Markovian), HTML-to-text state machine,
  SIU parser. See `src/`.

## Scientific guardrail

- Synthetic data is for development, testing, demos, and CI only.
- Final inferential or policy-facing results must be produced from
  approved real data with full provenance.
- Synthetic runs must be explicitly labeled as synthetic in outputs
  and reporting text.

## Install

From local source:

```r
install.packages("r-package/morie", repos = NULL, type = "source")
```

From r-universe (development snapshot):

```r
install.packages(
  "morie",
  repos = c(hadesllm = "https://hadesllm.r-universe.dev",
            CRAN     = "https://cloud.r-project.org")
)
```

The assistant bridge supports a local fallback through the Python
package when no live OpenAI / Anthropic credentials are configured.

## Outputs-manifest example

```r
library(morie)

manifest <- morie_read_outputs_manifest(project_root = "/path/to/project")
audit    <- morie_audit_public_outputs(project_root = "/path/to/project",
                                       manifest     = manifest)
morie_summarize_output_audit(audit)
```

## Synthetic data example

```r
library(morie)

synthetic_path <- morie_write_synthetic_data(
  path      = "data/private/synthetic_study_data.csv",
  n         = 8000,
  seed      = 2026,
  overwrite = TRUE
)
```

## Cross-project adaptation

```r
library(morie)

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

A first-class subsystem for the Ontario Special Investigations Unit
director's-report corpus. The fetcher handles both English and French
templates from 2005 onward; the parser is hand-rolled C++ for
correctness under SIU's heterogeneous markup.

### Fetch and parse the full corpus

```r
library(morie)

# Use the shipped language-aware DRID manifest; English-only,
# cache pages so re-runs are fast.
df <- morie_fetch_siu(
  lang       = "en",         # skip French drids automatically
  cache_html = TRUE,         # persist every fetched page locally
  rate_limit = 4             # requests per second (polite default)
)

# 2,218 unique cases x 64 columns; 100% format-clean on the
# shipping corpus per morie_siu_sanity_check().
nrow(df)
```

### Audit a single case

```r
# Inspect parser row + raw HTML + cleaned text side-by-side.
morie_siu_audit_case("16-OFI-019")

# Per-field "does the HTML actually support this value?" check.
morie_siu_anomaly_check("16-OFI-019")

# Diff parser output against an external table.
morie_siu_compare(
  case_number = "16-OFI-019",
  external    = my_other_table,
  field_map   = c(officer_count = "n_officers")
)
```

### AI extraction (free local model by default)

```r
# Default: local Ollama with gemma3:4b. No API key required.
morie_siu_llm_extract("16-OFI-019")

# Failover chain: try local first, fall back to Gemini only on error.
morie_siu_llm_extract("16-OFI-019", model = c("ollama", "gemini"))

# French to English translation via translategemma.
morie_siu_translate(text = "L'enquete a ete close...", target_lang = "en")
```

Supported providers: `ollama` (default), `gemini`, `claude`, `vertex`.
Environment knobs: `OLLAMA_HOST` (defaults to `http://localhost:11434`),
`OLLAMA_MODEL` (defaults to `gemma3:4b`), `OLLAMA_KEEP_ALIVE` (`30m`).

### Format-validity sweep

```r
sane <- morie_siu_sanity_check(df)
sum(!sane$ok)  # rows with format issues (regex / ISO date / Yes-No / chrome leak)
```

### Aggregate accuracy

```r
# How accurate is each column across a sample of cases?
morie_siu_audit_columns(case_numbers = sample(df$case_number, 50))
```

### Canonical override system

The parser learns. Ship-time corrections live in
`inst/extdata/siu_canonical_overrides.csv.gz` (47 hand-verified
corrections covering 10 spot-checked cases). Users can add their own:

```r
morie_siu_record_correction(
  case_number = "20-OFD-082",
  field       = "officer_count",
  value       = 3L
)
```

Overrides are applied automatically at the end of `morie_fetch_siu()`,
per cell, by case number.

### Inspect the manifest

```r
manifest <- morie_siu_index()
table(manifest$`_language`)  # en=2531, fr=2212, unknown=0
```

## Continuous integration

The R CMD check matrix covers six cells, all green on the
`release/v0.9.5-audit` head:

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

Run `citation("morie")` after installation. Please cite **both** the
software and the relevant companion papers.

```bibtex
@Manual{ruhela_morie_R_2026,
  title   = {morie: Multi-domain Open Research and Inferential Estimation in R},
  author  = {Ruhela, Vansh Singh},
  year    = {2026},
  note    = {R package version 0.9.5.4},
  doi     = {10.5281/zenodo.20111233},
  url     = {https://github.com/hadesllm/morie}
}

@Misc{ruhela_morie_python_2026,
  title     = {morie: Multi-domain Open Research and Inferential Estimation in Python},
  author    = {Ruhela, Vansh Singh},
  year      = {2026},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.20096350},
  url       = {https://doi.org/10.5281/zenodo.20096350}
}

@Misc{ruhela_mrm_framework_2026,
  title     = {MRM: Multilevel Reconciliation Methodology --- A multi-source statistical foundation for Canadian carceral, police, and oversight data},
  author    = {Ruhela, Vansh Singh},
  year      = {2026},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.20096075},
  url       = {https://doi.org/10.5281/zenodo.20096075}
}

@Misc{ruhela_hawkes_2026,
  title     = {Criminological Hawkes Process via MORIE: Markovian and Non-Markovian Self-Exciting Point Processes for Toronto Crime},
  author    = {Ruhela, Vansh Singh},
  year      = {2026},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.20102198},
  url       = {https://doi.org/10.5281/zenodo.20102198}
}

@Misc{ruhela_empirical_2026,
  title     = {Solitary Confinement, Self-Excitation, and Institutional Churn: Empirical Applications of MRM to Canadian Carceral and Police Data},
  author    = {Ruhela, Vansh Singh},
  year      = {2026},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.20175689},
  url       = {https://doi.org/10.5281/zenodo.20175689}
}
```

## License

morie is licensed under **AGPL-3.0-or-later**. See `LICENSE` for the
full text and `LICENSING.md` for the per-component breakdown.

## rOpenSci review

morie is under review at rOpenSci:
[ropensci/software-review#770](https://github.com/ropensci/software-review/issues/770).
