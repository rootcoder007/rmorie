# morie 0.9.5.12 - 2026-05-25

## CI: drop fwildclusterboot (pak recursive Remotes unreliable) (3MMM.40c)

* Removed `fwildclusterboot` from Suggests and removed the
  `.morie_did_have_fwildboot()` helper + the
  `if (.morie_did_have_fwildboot()) { fwildclusterboot::boottest(...) }`
  branch in `morie_did_wild_cluster_bootstrap()`. The function
  now goes straight to the base-R Rademacher/Webb wild-cluster
  bootstrap (which already existed as the fallback and mirrors
  the Python implementation; no math change for any caller).
* Reason: pak's resolver does not reliably recurse through a
  Remote's own Remotes. 3MMM.40 added `s3alfisc/fwildclusterboot`
  and 3MMM.40b added `s3alfisc/summclust`, but the resolver still
  reported `summclust: Can't find package called summclust` -- so
  the recursive-Remote pattern is structurally fragile. Following
  the same "drop optional CRAN-archived/GitHub-only deps" pattern
  used for `rdd` in 3MMM.40.
* `Remotes:` now lists only `synth-inference/synthdid`, which has
  no GitHub-only transitive Imports.

# morie 0.9.5.11 - 2026-05-25

## CI: pak resolver -- transitive Remote for summclust (3MMM.40b)

* Added `s3alfisc/summclust` to DESCRIPTION `Remotes:`.
  `fwildclusterboot` Imports `summclust`, which is also
  GitHub-only (never on CRAN). 3MMM.40 added the
  fwildclusterboot Remote but pak's recursive resolver still
  failed one level deeper because it does not auto-recurse
  through a Remote's own DESCRIPTION. summclust's Imports
  (utils, dreamerr, MASS, collapse, generics, cli, rlang) are
  all on CRAN, so the chain terminates here.

# morie 0.9.5.10 - 2026-05-25

## R CMD check ERROR fixes (3MMM.39)

* `R/datasets.R` (6 sites): dropped the invalid `n = 2L` argument
  from `strsplit()`. Base R's `strsplit()` has no `n=`; the call
  silently ignored it on most R versions but errors on R-devel.
* `R/dataset_load_by_key.R`: removed the spurious
  `max_features = max_features` argument from the
  `morie_datasets_ontario_ckan_by_key()` dispatch. That function's
  formals are only `(dataset_key, offline, resource_id)` -- passing
  the unused formal caused a hard error in the dispatcher example.
* `R/ingest_statcan.R`: replaced the non-existent
  `cansim::set_cansim_api_key(api_key)` call with the documented
  mechanism. `cansim` has no such helper in any current CRAN
  release; it reads `CANSIM_API_KEY` from the environment.
  `morie_ingest_statcan_cansim()` now mirrors a user-supplied
  `STATCAN_API_KEY` into `CANSIM_API_KEY` when only the morie alias
  is set.
* `R/spatial_voting.R::mlsmu6`: added `is.finite(prev_stress)` guard
  for the convergence check. `prev_stress` starts as `Inf`, so
  iter 1's `abs(Inf - stress) / max(Inf, 1e-12) = NaN` triggered
  `"missing value where TRUE/FALSE needed"` and broke the
  `\examples{}` block. The first iteration now skips the
  convergence check cleanly; iter 2+ uses real values.
* Added a proper roxygen block for `morie_dataset_portal_catalog()`
  (only the `@export` tag was present; the docstring upstream was
  attached to the sibling `_clear_cache` helper).
* `man/morie_dataset_portal_catalog.Rd` and
  `man/morie_entheo_clone_dmt_imaging.Rd` regenerated.

## CI: `setup-r-dependencies` pak resolver unblocked (3MMM.40)

* Dropped `rdd` from Suggests. CRAN archived it in 2024 and pak
  could no longer resolve it. The only morie callsite
  (`morie_rdd_mccrary()`) used it as a fallback when `rddensity`
  wasn't installed; `rddensity` itself is on CRAN and in Suggests,
  so the `rdd` branch was effectively dead code in any realistic
  configuration.
* Added `Remotes: s3alfisc/fwildclusterboot, synth-inference/synthdid`
  so pak can fetch the two remaining GitHub-only Suggests when
  building the lockfile. Both upstream repositories are verified
  live (HTTP 200 from `api.github.com/repos/...`).

# morie 0.9.5.9 - 2026-05-24

## Correctness recovery: math typesetting restored

Phase 3LLL reverses the destructive `\eqn{LATEX}` -> `\code{LATEX}` swap
shipped in commit f399ec41a (Phase 3KKK1+2). That swap eliminated the
"Lost-braces" warning but at the cost of stripping LaTeX math
typesetting from the PDF/HTML manual and turning every greek letter,
`\hat`, `\sum`, `\frac`, etc. into an "unknown macro" warning at
`R CMD check`.

The proper Rd-compliant fix is the two-argument form:

    \eqn{LATEX}{ASCII fallback}      # inline
    \deqn{LATEX}{ASCII fallback}     # display

Every affected line (104 R files) now uses this form, preserving PDF
math while satisfying the Rd parser. Driven by `fix_rd_math.py`, a
LaTeX->ASCII transformer covering the common Greek alphabet,
operators (`\sum`, `\int`, `\hat`, `\bar`, `\frac`, `\sqrt`), and
relation symbols.

## Auto-install helper for optional dependencies

New `morie_install_extras()` lets users install the ~50 optional
`Suggests:` packages in one call. CRAN policy forbids
`install.packages()` at `.onLoad()` time, so morie ships an opt-in
helper instead. Three modes:

    morie_install_extras()                       # missing only (default)
    morie_install_extras("all", ask = FALSE)     # everything, CI-safe
    morie_install_extras(c("hawkes", "sf"))      # named subset

The helper also probes for the C system libraries `libcurl`,
`libsodium`, and `liboqs` and prints platform-specific install hints
when any are missing. System libraries must be installed BEFORE
re-installing morie so the configure-time probes link the C/C++
backends against them.

# morie 0.9.5.8 - 2026-05-24

## Bulk open-data catalog explosion

Cross-portal `morie_dataset_portal_catalog()` grows from ~1,044
rows to **9,242 rows across 14 portals**. Every Socrata / CKAN /
ArcGIS Hub / Opendatasoft portal morie touches now has its full
public catalog bundled offline.

### Phase 3GGG -- 6-portal bulk harvest

* **3GGG1**: NYC OpenData -- 2851 entities (2395 datasets + 294
  maps + 162 filters/charts/hrefs/stories).
* **3GGG2**: Chicago Open Data -- 1856 entities.
* **3GGG3**: Toronto Open Data CKAN -- 540 packages.
* **3GGG4**: Calgary (933) + Edmonton (2027) Socrata.
* **3GGG5**: Ottawa Open Data Hub -- 287 datasets (via OGC
  `startindex=` pagination, not Socrata `offset=`).
* Replaced the per-portal crime-adjacent subset catalogs from
  3EEE2/3FFF3 with the bulk variants (no API change -- the small
  curated catalogs are still callable via the older loader names
  for backwards compat).
* New generic Socrata-by-id wrappers:
  `morie_datasets_nyc_socrata_by_id()` +
  `morie_datasets_chicago_socrata_by_id()` (mirror the 3FFF3
  Calgary/Edmonton pattern). `morie_datasets_load_by_key()` routes
  chicago + nyc_opendata sources through them; `max_features` now
  threads as the SODA `$limit`.

### Phase 3HHH -- full catalogs for the last two portals

* **3HHH1**: Montreal Open Data CKAN full bulk -- 401 packages
  (up from the 23-row Loi/Justice/Securite subset from 3EEE1).
* **3HHH2**: Vancouver Opendatasoft v2.1 full bulk -- 190 datasets
  with enriched schema (publisher, theme, license, records_count
  added to the 3CCC4 fixture).

### Catalog totals

```
calgary_opendata      933    nyc_opendata          2861
chicago              1864    ontario_ckan            38
edmonton_opendata    2027    ottawa_opendata        287
montreal_opendata     401    statcan_ccjs            10
nyc_nypd                8    toronto_opendata       540
tps_arcgis_hub         71    tps_psdp                11
vancouver_opendata    190    vpd_geodash              1
                                                  -------
                                                     9242
```

Bundled fixture footprint: ~3.4 MB of catalog metadata; per-row
unwound this is the metadata equivalent of every NYC dataset
descriptor + every CKAN package summary + every Hub item -- offline
queryable via `morie_datasets_browse(keyword=...)`.

---

# morie 0.9.5.7 - 2026-05-24

## Cross-portal open-data infrastructure

Major sprint adding 14 open-data portals + a unified browse/load
interface. The cross-portal `morie_dataset_portal_catalog()`
now spans 9 cities + 1 federal source + ~800 dataset entries
across 4 different API protocols.

### Phase 3CCC -- NYC + TPS deep coverage

* **3CCC1**: NYPD `law_code` resolver. New
  `morie_datasets_nyc_nypd_law_books()` (46-row statute book ->
  human name + jurisdiction dict; PL, VTL, CPL, ABC, AC, COR, AM,
  PHL, ED, GB, GCI, HTH, PAR, LOC, FOA, RR, TAX, RPA, RP, PRL,
  TWN, ...) + `morie_parse_nypd_law_code()` vectorised regex
  parser. Added as 4th resolver in
  `morie_datasets_nyc_nypd_resolved()`.
* **3CCC2**: NYC multi-boundary loader bundle -- 5 new fixtures
  (school districts / council districts / community districts /
  NTAs 2020 / ZCTAs) + `morie_datasets_nyc_boundaries_catalog()`
  unified index.
* **3CCC3**: TPS Hub resolved-joins analyzer
  (`morie_datasets_tps_psdp_resolved()`) -- division + hood158 +
  hood140 + NIA + psdp_class 5-way join, mirrors the Chicago /
  NYPD `_resolved()` patterns. Plus
  `morie_datasets_tps_police_divisions()` (16 post-amalgamation
  TPS divisions).
* **3CCC4**: cross-portal `morie_dataset_portal_catalog()` -- 7
  initial portals, 336 datasets, uniform schema (dataset_key,
  source, id, api_modes, loader, dict_url, n_rows_bundled). Added
  Vancouver Open Data (Opendatasoft v2.1, 190 datasets). Folded
  SODA3-auth note into the SODA3 helper docstring per
  Socrata support article 34730618169623.

### Phase 3DDD -- Canadian municipal + federal coverage

* **3DDD1**: 5 Vancouver crime-adjacent civic loaders -- graffiti
  (100 / 7683), noise control areas (3), homeless shelters (17),
  property use inspection districts (23), fire halls (20).
* **3DDD2**: VPD GeoDASH crime loader. T&Cs gate auto-download,
  so morie ships a stratified 550-row sample (50 x 11 TYPE
  categories) + bundled legal disclaimer + user-`zip_path =` mode
  for the full 915k-row feed.
* **3DDD3**: Statistics Canada CCJS / CODR WDS REST API.
  10-cube registry covering federal crime + corrections;
  `morie_datasets_statcan_cube_metadata()` +
  `morie_datasets_statcan_vectors()` +
  `morie_datasets_statcan_full_csv_url()` wrappers.
* **3DDD4**: `morie_datasets_browse()` + `morie_datasets_summary()`
  -- filter the cross-portal catalog by keyword / portal /
  api_mode / loader regex with AND-composable predicates.

### Phase 3EEE -- Montreal + expanded Toronto/Vancouver + dispatcher

* **3EEE1**: Montreal Open Data CKAN -- 23-row Loi/Justice/
  Securite catalog + SIM (fire/EMS) interventions flagship loader
  with 349-row stratified bundled sample + 170-row
  INCIDENT_TYPE_DESC dict + generic CKAN dispatcher.
* **3EEE2**: Toronto Open Data CKAN beyond TPS Hub -- 208-row
  crime-adjacent catalog + ambulance stations + TPS ASR misc
  aggregates + generic CKAN dispatcher.
* **3EEE3**: Vancouver Open Data deeper coverage -- 4 more
  fixtures (community centres, food markets, disability parking,
  public art).
* **3EEE4**: `morie_datasets_load_by_key()` -- single dispatcher
  resolving any catalog dataset_key to its loader across all
  portals.

### Phase 3FFF -- dispatcher hardening + prairie cities

* **3FFF1**: CKAN `package_show` -> first-CSV resource
  auto-resolution. MTL + TO generic CKAN keys now Just Work
  through `morie_datasets_load_by_key()`.
* **3FFF2**: `mode = c("auto","soda2","soda3","odata")` +
  `app_token` args on the dispatcher; routes through SODA3 for
  Socrata-backed sources, silently ignored elsewhere.
* **3FFF3**: Calgary + Edmonton + Ottawa loaders. Calgary +
  Edmonton are Socrata (data.calgary.ca, data.edmonton.ca);
  Ottawa is ArcGIS Hub (open.ottawa.ca, dispatches through the
  existing 3SS+ generic ArcGIS pipeline). Crime-adjacent catalogs
  + per-dataset bundled fixtures + generic Socrata-by-id
  dispatchers.

### Catalog totals (across 14 portals)

```
chicago             8     ontario_ckan       38
nyc_nypd            8     vancouver_opendata 190
nyc_opendata       10     vpd_geodash         1
tps_arcgis_hub     71     statcan_ccjs       10
tps_psdp           11     montreal_opendata  23
                          toronto_opendata  208
                          calgary_opendata  157
                          edmonton_opendata 195
                          ottawa_opendata   106
```

Total ~ 1044 catalog rows.

---

# morie 0.9.5.6 - 2026-05-23

Formula corrections (affect Python AND R sibling identically):

* `iv.morie_iv_wald` / `iv.wald_estimator` Wald-LATE delta-method SE
  previously omitted the Cov(num, den) term, biasing the SE under
  realistic Y-D correlation. Now includes `- 2*(num/den^3) *
  cov(y, d) / n` per-stratum aggregation.
* `dsp_waveform.morie_dsp_higuchi_fd` / `_waveform.higuchi_fd`
  fractal dimension previously summed M-1 differences instead of M
  (Higuchi 1988 eq 1 specifies floor((N-m)/k) summands). Fixed by
  using M+1 indices so diff() yields M terms.

R-side feature additions:

* 4 new RcppArmadillo C++ kernel files (`src/morie_hawkes.cpp`,
  `morie_dsp.cpp`, `morie_matching.cpp`, `morie_spatial.cpp`)
  exposing 14 // [[Rcpp::export]] symbols.
* R wrappers in `R/{tps_hawkes_advanced,dsp_filters,matching,
  spatial_voting}.R` now dispatch to the C++ kernels when the
  compiled .so is loaded, falling back to pure-R otherwise.
* DESCRIPTION: `LinkingTo: Rcpp, RcppArmadillo` (was: Rcpp).

Other fixes carried from the 5-layer review on 2026-05-22 (all
Python-parity-verified before applying):

* `R/survival.R` `.validate_te` now returns `ok` mask; KM/HR/concordance
  callers re-align `group`/`risk_score` by mask instead of `seq_along`.
* `R/iv.R` JIVE projects only the endogenous columns (was: every
  column including intercept and exogenous controls), matching
  `src/morie/iv.py:1604-1613`.
* `R/did.R` `morie_did_aggregate_gt_att` SE uses `k = cell count`
  (was: `nrow(g)`, equivalent only when nrow(g)==1).
* `R/did.R` `morie_did_test_parallel_trends` returns
  `joint_chi2 + joint_df`, keeps `joint_f_stat` as alias.
* `R/inference.R` Clopper-Pearson exact CI handles `successes==0`
  and `successes==n` edges instead of calling `qbeta(., 0, .)`.
* `R/weights.R` `morie_weights_brr` warns on odd-size strata.
* `R/spatial_voting.R` Hare 2018 + King 2003 citation corrections.
* `R/tps_statphysics.R` Helbing 2010 venue corrected (NJP not PNAS).

Earlier from 2026-05-22 marathon (already in 0.9.5.6 in tree):

* Cox-Snell residuals use per-row `y[,"status"]` not scalar `nevent`.
* JKn replicate weights rewritten to Wolter 2007 form (one PSU per
  replicate, scale survivors by n_h/(n_h-1)); aggregator uses
  `((n_h-1)/n_h)*sum_{i in h} diffs_sq_i`.
* Mann-Whitney effect size `r = Z/sqrt(n1+n2)` (was: `n1*n2`).
* Li-Ji `n_effective_tests` sums fractional part for all eigenvalues.
* Sampling proportional alloc keeps stratum names so weights aren't NA.
* Abadie-Imbens SE splits by treatment, denom is `n_treated^2`.
* tps_statphysics inspection-game payoff matrix transposed back to
  match Python convention.

# morie 0.9.5.6 - 2026-05-22

R-side `describe()` parity closure. Patch release that closes one
of the two parity gaps named in v0.9.5.4: the pedagogical
narratives that the Python sibling exposes via `morie.describe()`
are now available on the R side via `morie_describe()` and the
string-only variant `morie_describe_by_name()`.

**R API additions**:

* `morie_describe(callable)` — takes a function object OR a
  character scalar (with or without the `morie_` prefix). Prints
  the pedagogical narrative for the named callable.
* `morie_describe_by_name(name)` — string-only variant.

**Bundled data**:

* `inst/extdata/describe_corpus.Rds` — a single xz-compressed Rds
  (~1.6 MB on disk) containing 36,433 named character entries.
  Names are the callable mnemonics (the 4-7 character forms);
  values are the markdown narrative bodies sourced from
  `src/morie/fn/describe_<name>.md`. The Rds is loaded once per
  session and cached in a package-private environment.

**Build tooling**:

* `tools/bundle-describe-files.R` — re-runs the Python-to-R sync
  when `src/morie/fn/describe_*.md` changes. Run from the repo
  root with `Rscript tools/bundle-describe-files.R`.

**Tests**:

* `tests/testthat/test-describe.R` — 17 tests covering lookup,
  prefix stripping, `.md` extension stripping, unknown-name
  diagnostics, type-rejection, function-object capture via
  `substitute()`, and cache identity across calls. All pass on
  the development build.

**Remaining parity gap**:

* `morie.crypto` educational primitives ship on the Python side
  only; a native R + Rcpp port (ML-KEM, Dilithium, NTRU,
  McEliece, ECC, hybrid PQC) is planned for v1.0.0. Calling into
  the Python side via `reticulate` is not added in v0.9.5.5; the
  scope was set at the native-R port path, which is a larger
  arc and the natural place for a v1.0.0 milestone.

# morie 0.9.5.4 - 2026-05-21

Doob → MRM chi-square rename. Patch release with deprecation aliases;
no breaking changes for existing user code.

**Naming**:

* The internal name 'Doob chi-square family' is renamed 'MRM
  chi-square family' across all morie code, Sphinx docs
  (architecture, mrm_modules, siuiap), and the rootcoder007
  profile README. The Sprott-Doob-Iftene author-pair citation
  in `papers/` is preserved; the `src/morie/sprott_doob.py` and
  `src/morie/doob_trends.py` author-named modules are also
  preserved.

**Python API (with deprecation aliases)**:

* `morie.otis_all_analyze.analyze_c_doob_chi2()` -> `analyze_c_chi2()`
* `morie.otis_all_analyze.analyze_d_doob_chi2()` -> `analyze_d_chi2()`

  Old names still work but emit `DeprecationWarning`. They will
  be removed in a future release; update callers at your
  convenience.

**R side**: no R API changes; the R chi-square family was already
renamed in v0.9.5 (vignette `chi-square-and-anova.Rmd`).

# morie 0.9.5.4 - 2026-05-21

Patch release over 0.9.5.2.

* Declare `pkgload` in `Suggests:`. The `pkgload` skip-guard added
  in 0.9.5.2's `test-cov-fallbacks.R` used `pkgload::dev_packages()`
  without declaring the package in DESCRIPTION's `Suggests:`,
  producing a `'::' or ':::' import not declared from: 'pkgload'`
  WARNING under `R CMD check`. No user-visible functional change;
  the warning is informational, but it should not have shipped in
  0.9.5.2.
* 0.9.5.2 has been yanked from PyPI as a consequence of the above
  WARNING and to keep the public release record clean.

# morie 0.9.5.4 - 2026-05-21

* **HTML validation fix.** `morie_siu_sanity_check`'s description
  used `date_*_iso` and `number_of_*` as inline text, which
  roxygen2's markdown mode rendered as nested `\emph{\emph{...}}`
  in the generated Rd and as nested `<em>` in the HTML manual.
  win-builder flagged this as an HTML validation NOTE. Wrapping
  the identifiers in backticks (now rendered as `\verb{...}`)
  resolves it.
* All other fixes are inherited from 0.9.5.1: see entry below.

# morie 0.9.5.1 - 2026-05-21

CRAN Policy: full cache-leak fix (supersedes 0.9.5 which was
uploaded to win-builder with incomplete cache-isolation).

* `morie_db_connect()` default cache-dir flipped from
  `tools::R_user_dir("morie", "cache")` to a session-scoped
  `tempdir()` subdirectory; matches the convention already set
  for `morie_fetch_siu()` and `morie_fetch_tps()` in 0.9.5. Now
  no morie function writes outside `tempdir()` unless the user
  explicitly opts in by passing `db_path = morie_cache_dir(...)`
  or `cache_dir = morie_cache_dir(...)`.
* New `morie_cache_clear(subdir, confirm)` user-facing function
  for actively-managing the persistent cache (CRAN Policy
  requirement for `R_user_dir` caches).
* `morie_cache_dir(subdir)` is now exported with a `subdir`
  argument so users can compose per-subsystem persistent paths.
* 3 `morie_cache_*` examples (`store`, `load`, `list`) now use
  explicit `db_path = tempfile()` so `R CMD check` never writes
  outside `tempdir()`.
* `morie_check_plugin_license` error-path example moved from
  `\donttest{}` to `\dontrun{}` (intentionally errors when
  passed an incompatible SPDX).
* `morie_fetch` placeholder-URL example moved from
  `\donttest{}` to `\dontrun{}` (example.org doesn't host CSV;
  the URL is a documentation placeholder).
* Two `crimsl.utoronto.ca` references in `R/mandela.R` and
  `R/morie-package.R` rewritten as plain-text references; the
  U of T web server returns 403 to win-builder's IP even though
  the URLs are publicly reachable from browsers.
* New `inst/WORDLIST` listing real technical terms (AIPW, ATC,
  ATT, CATE, Hawkes, MRM, etc.) so the win-builder
  spell-checker no longer flags them.

# morie 0.9.5 — 2026-05-21

Documentation + CI hardening (added 2026-05-21 to the v0.9.5
release branch alongside the SIU + rename work):

* **New SIU vignette** (`vignettes/siu-pipeline.Rmd`) — end-to-end
  walkthrough of `morie_fetch_siu()`, `morie_siu_audit_case()`,
  `morie_siu_anomaly_check()`, `morie_siu_compare()`,
  `morie_siu_llm_extract()`, `morie_siu_translate()`, and the
  canonical-override system. 14 vignettes total now.
* **Chi-square vignette correction.** `vignettes/chi-square-and-anova.Rmd`
  previously called the MRM chi-square family the "Doob $\chi^{2}$
  family", which incorrectly singled out one of the three named
  authors (Sprott, Doob, Iftene) of the source contingency tables.
  Renamed to "MRM chi-square family". The Sprott / Doob / Iftene
  author citation to the source tables is unchanged.
* **`_pkgdown.yml` shipped** — a minimal pkgdown configuration so
  contributors can build a local documentation site with
  `pkgdown::build_site()`. The file is `.Rbuildignore`d so it
  doesn't ship in the CRAN tarball.
* **README rewrite** (top-level + R-package) to reflect v0.9.5
  reality: 559 morie-prefixed exports (not 87), the SIU subsystem,
  free-first AI helpers (Ollama default), language-aware DRID
  manifest, canonical-override system, polite-by-default fetcher,
  and the green 6-cell R CMD check matrix.
* **pkgcheck workflow: `inconsolata` LaTeX font installed.**
  pkgcheck's internal rcmdcheck builds the PDF manual, which
  needs `inconsolata.sty`. Without it pkgcheck reported a spurious
  "R CMD check found 1 warning" against a package that has 0
  warnings in the dedicated `r-cmd-check.yml` matrix. The
  pkgcheck job now installs tinytex + inconsolata before running.

lintr / goodpractice cleanups:

* The Hawkes C++ likelihood functions now use `T_horizon` instead
  of `T` for the time-horizon parameter, so the auto-generated
  `R/RcppExports.R` no longer trips R linters that flag `T` as a
  potential `TRUE` shadow. The math convention is preserved in the
  C++ docstrings; only the parameter NAME changed.
* `setwd()` in `morie_run_workflow_step()` replaced with
  `withr::local_dir()` (goodpractice no-setwd linter).
* 352+ exported functions renamed to the `morie_*` prefix so they
  no longer collide with same-named functions in other CRAN
  packages. Examples: `chi_square_test` → `morie_chi_square_test`,
  `kmeans_clustering` → `morie_kmeans_clustering`, etc. Names that
  were already morie-specific cryptic abbreviations (agset, brdgr,
  fzhdc, …) are unchanged.

SIU harvester: polite by default, manifest-aware, retry-aware, and
auditable against the original published reports.

* **Persistent HTML cache + per-case audit.** `morie_fetch_siu(cache_html
  = TRUE)` saves every fetched report and news-release page under
  `<cache_dir>/html/` (gzipped, ~80-100 MB for a full sweep). The
  saved HTML is the canonical ground truth for every row in the
  emitted CSV: any later question of the form "did the parser get
  this field right?" is decidable by reading the cached page for
  that case. `morie_siu_audit_case(case_number)` returns the
  parser's 1-row data frame, the raw report and news HTML, and
  HTML-stripped plain text for both, all from cache when available.
* **`morie_siu_compare()`** — line up the parser's output for a
  case against a user-supplied external table (column map and case
  key are caller-controlled) and show the surrounding report HTML
  excerpt for each disagreement. No external source is treated as
  authoritative; the function exists so the user can adjudicate
  parser-vs-external mismatches against the actual published
  report. The published report HTML is the only ground truth morie
  recognises for SIU fields.
* **Free by default.** The LLM helpers now default to
  \code{model = c("ollama", "gemini")} -- a free local Ollama
  model first, with paid Gemini as fallback only if Ollama is
  unavailable. Users who install Ollama and pull a free Gemma /
  Qwen / Llama / Functiongemma variant
  (\code{ollama pull gemma3:4b}) get the full second-coder /
  audit / anomaly-check stack at \$0 ongoing cost. \code{OLLAMA_HOST}
  defaults to \code{http://localhost:11434} when unset, so the
  zero-config path is just "install ollama, pull a model, done".
* **AI second-coder (Gemini / Claude / Ollama).**
  `morie_siu_llm_extract(case_number, model = "gemini")` sends the
  cached report HTML through a large-language-model endpoint and
  returns the same 64-column row format as the C++ parser, so it
  drops straight into `morie_siu_compare(external = ...)` for an
  independent diff. `model` accepts a character vector for
  fail-over, e.g. `c("gemini", "ollama")` uses the paid Gemini
  endpoint when available and silently falls back to a local /
  free Ollama-compatible model otherwise. Credentials are read
  from `GOOGLE_API_KEY` / `ANTHROPIC_API_KEY` / `OLLAMA_HOST`;
  nothing is hard-coded.
* **`morie_siu_translate_fr_to_en()`** — self-improving SIU.
  For SIU cases that exist only in French (no English-language
  paired drid; ~1-2 per year of SIU output), translate the
  narrative_summary, news_release_summary, news_release_title and
  relevant_legislation into English via a local Ollama model
  (default $0 cost, no API key needed) and persist each
  translation as a canonical override via
  \code{morie_siu_record_correction()}. Idempotent (skips
  already-translated cases) and self-improving (every run leaves
  morie better at returning English content for French-only
  reports). Maintainers can promote the resulting overrides into
  the shipped \code{inst/extdata/siu_canonical_overrides.csv.gz}
  so all users get the English text on the next package update.
* **French police-service acronyms.** The modal-service detector
  now also recognises SPT (Service de Police de Toronto), PPO
  (Police provinciale de l'Ontario), SPRH (Halton), SPRY (York),
  SPRP (Peel), SPRD (Durham), SPRN (Niagara), SPRW (Waterloo),
  SPO (Ottawa), SPL (London), SPH (Hamilton), SPW (Windsor), SPG
  (Guelph), SPK (Kingston) and maps each to the canonical English
  name. Closes the remaining French-only-case gap; 12-TFD-104 in
  the 2012 corpus now reports \code{Toronto Police Service}
  correctly.
* **99.955% format-clean on the full 2,218-case corpus.**
  Empirical measurement via `morie_siu_sanity_check()` on the
  freshly-harvested SIU.csv: 2,217 / 2,218 rows have zero format
  issues; the lone remaining case is a 2012 French-only report
  (12-TFD-104) without an English-paired drid. The earlier
  95.45% baseline ate four further fixes: (a) Unicode apostrophe
  / quote / dash normalisation in `lower_ascii()` so the title-
  finder matches "Director's report" (U+2019) cleanly, (b)
  "Overview" as a section_4 fallback for 2014 reports that
  retitled "The Investigation", (c) French "L'enquête" / "Aperçu"
  fallbacks for French-only reports, (d) full SIU police-service
  acronym table (OPP, TPS, HRPS, NRPS, PRP, YRP, DRPS, WRPS, OPS,
  LPS, WPS, GPS, KPS, BPS, BPPS, CKPS, PRPS, GSPS, SSMPS, SLPS,
  SPS, TBPS, BPSB) -- old reports use the acronym throughout and
  never spell out "Ontario Provincial Police", and the modal-
  service detector now picks up "OPP" → "Ontario Provincial
  Police" automatically.
* **Interleaved report + news fetch.** `morie_fetch_siu()` no longer
  walks the corpus in two strict phases (fetch all reports, then
  fetch all news). It now uses a rolling-window batched fetcher:
  each batch of 250 reports fires in the same rate-limited pool
  as the previous batch's news pages. While the next 250 reports
  are downloading, the news pages for the nrids we just parsed
  are downloading alongside. Roughly halves cold-start corpus
  wall time (~30 min instead of ~58 min on the full 4,700-drid
  sweep) without changing the per-second rate the SIU site sees.
* **Canonical overrides — the parser LEARNS from corrections.**
  Every verified \code{(case_number, field, value)} tuple recorded
  via \code{morie_siu_record_correction()} is applied to
  \code{morie_fetch_siu()}'s output on subsequent runs. The shipped
  \code{inst/extdata/siu_canonical_overrides.csv.gz} holds the
  maintainer-confirmed table (starts empty in v0.9.5, populated by
  the LLM-audit + human-review workflow over time). The
  user-side \code{<cache_dir>/canonical_overrides.csv} merges in
  too -- users can fix their local copy without touching the
  package source. This is morie's "memory": wrong cells get found
  via \code{morie_siu_sanity_check()} or
  \code{morie_siu_audit_columns()}, corrected once, and the fix
  propagates to all users on the next package update -- no C++
  rebuild needed.
* **`morie_siu_sanity_check()`** — fast format-validity pass over
  every row of an emitted SIU table. Flags case_number that
  doesn't look like an SIU id, date_*_iso that isn't ISO 8601,
  number_of_* that isn't a positive integer, charges_recommended
  that isn't "Yes"/"No", page-chrome strings leaked into
  narrative_summary or other content fields, etc. Returns a data
  frame ordered worst-first so maintainers can pop the cached
  HTML for any flagged row and adjudicate. Runs in milliseconds,
  no network, no LLM, no API key required.
* **`morie_siu_audit_columns()`** — closed-loop per-column accuracy
  audit. Runs the anomaly check across many cases and aggregates
  by field, returning a data frame sorted by agreement rate
  (worst first) so maintainers can prioritise which regex
  extraction pattern to fix next. Concrete disagreement examples
  for each field are attached as the \code{"examples"} attribute.
  With \code{model = "ollama"} pointed at a local Gemma / Qwen /
  DeepSeek instance the audit costs zero API spend; chain
  \code{c("gemini", "ollama")} for paid-first / free-fallback.
* **`morie_siu_anomaly_check()`** — per-field "does the report
  support this extraction?" audit. Sends one API call per case
  (all populated fields batched into a single prompt) and returns
  a data frame with `field`, `parser_value`, `verdict`
  (\code{"agree"} / \code{"disagree"} / \code{"unclear"}), and a
  one-sentence reason. Not authoritative -- the cached HTML is
  the ground truth -- but a fast way to triage which rows a human
  should re-read against the report.
* **Section-text terminator fix (parser correctness).** The
  `section_text()` helper used to stop only at the next `<h2>`,
  so the LAST `<h2 id="section_N">` block on a page (typically
  section_8 -- analysis / decision) silently captured everything
  to end-of-document, including the site's left-nav and footer.
  This leaked phrases like "First Nations, Inuit and Métis
  Liaison Program" and Twitter follow links into every report's
  `narrative_summary`, `supplemental_materials`, and
  `mental_health_or_race_indications` -- the latter would have
  tagged every case in Ontario as "First Nation" regardless of
  the report's actual content. The terminator now also stops at
  `<footer`, `<aside`, `<nav`, whichever comes first after the
  section anchor.
* **`mental_health_or_race_indications` expansion.** Search scope
  now includes section_5 (Affected Person), which is where many
  reports state race / mental-health context. Keyword set
  expanded with `suicidal`, `psychotic`, `self-harm`,
  `self harm`, `emotionally disturbed`, `EDP`, `Mental Health Act`,
  `Inuit` (alongside the existing Black / Indigenous / First
  Nation / mental health / in crisis / racializ / racial set).
* **Shipped DRID manifest.** `inst/extdata/siu_drid_manifest.csv.gz`
  (~46 KB) ships with the package, listing 6,000 verified drids
  (4,443 with parsed case_number, covering 2,218 unique cases as
  of 2026-05-20). The harvester reads this floor automatically
  via `morie_fetch_siu()` -- new cases above the manifest's max
  are still discovered live.
* **`html_to_text` segfault fix.** The previous C++ HTML tag stripper
  used three `std::regex_replace` calls with `.*?` patterns; on at
  least one drid in the 1..6000 sweep these recursed through the C
  stack and aborted R with "segfault from C stack overflow",
  killing the manifest build mid-run. Replaced with a linear single-
  pass state machine (no recursion, no backtracking risk) plus a
  defensive 4 MB input cap.

* **Rate-limited multi-fetch.** `.siu_http_get_many()` now drives a
  token-bucket throttle (default 4 req/s across the whole pool) with
  exponential backoff on HTTP 429/502/503/504. `morie_fetch_siu()`
  defaults to `concurrency = 4L, rate_rps = 4.0`. The previous
  `concurrency = 16-24` default was high enough to trigger WAF
  interstitials on some networks (most visibly on GitHub Actions
  Azure egress IPs), which returned short non-report HTML that
  looked like data but wasn't.
* **`morie_siu_refresh_manifest()`** — sweeps director's-report ids
  `1..max_drid` (default 6000), records each id's HTTP status, body
  size, and parsed case number, and writes a gzipped CSV manifest of
  known-valid drids. The shipped manifest at
  `inst/extdata/siu_drid_manifest.csv.gz` lets `morie_fetch_siu()`
  short-circuit the ~30-50% of drids that have no published report,
  saving bandwidth and reducing WAF-trigger risk.
* **Live max discovery, always.** The harvester now always probes
  past the live SIU index max (`+300` drid margin, up from `+150`),
  so reports added after the manifest snapshot are still captured.
  The manifest is a *floor* on the known-valid id space, never a
  ceiling on what's swept.
* **`.siu_http_get_many_with_status()`** — new internal export
  returning body + http_code + attempts in parallel slots, used by
  the manifest builder and available for diagnostic scripts.

New: a generic open-data access layer, and a much wider dataset
catalog.

* **`morie_fetch()`** — a universal URL fetcher. It auto-detects the
  resource format from the HTTP `Content-Type` header (falling back to
  the URL extension) and parses CSV, TSV, JSON, XML, HTML, XLSX, and
  ZIP-bundled files. Every step is overridable: pass an explicit
  `format`, extra query `params`, or a `zip_member` to extract.
* **`morie_ckan_search()`** — discover datasets on any CKAN open-data
  portal (`open.canada.ca`, `data.ontario.ca`, `open.toronto.ca`, or a
  custom base URL). Returns one row per resource, with the
  `resource_id` to feed into `morie_fetch_ckan()`.
* **`morie_fetch_arcgis()`** — query any ArcGIS FeatureServer /
  MapServer layer, paginating through the server transfer limit.
* **Dataset catalog** — `morie_dataset_catalog()` gains `download_url`,
  `zip_member`, and `arcgis_url` columns and a six-tier
  `morie_load_dataset()` resolver. CKAN resource ids were added for the
  CCS 2018-2022/2023/2024 and CSUS 2023 PUMFs; direct-download URLs for
  23 further datasets (CIHI indicator tables, StatCan and
  Health-Infobase zip bundles); and verified ArcGIS layer URLs for the
  three TPS crime series.
* **`morie_load_dataset(refresh = TRUE)`** — bypass the built-in
  database and user cache to re-fetch a dataset from its remote
  source, picking up time-to-time updates.

Fix: Toronto Police Service open-data ingestion correctness and
reliability.

* **TPS dataset catalog** — the `tpshomicides` and `tpsshootings`
  entries in `dataset_catalog.R` advertised a `2014-present` date
  range. The Public Safety Data Portal publishes the Homicides and
  Shootings & Firearm Discharges series from **2004**; the catalog
  metadata is corrected to `2004-present`.
* **`morie_fetch_tps()` pagination** — the ArcGIS paging loop stopped
  as soon as a page returned fewer rows than the requested page size.
  A layer whose server-side `maxRecordCount` is below that size
  returns short pages on every call, so the download was silently
  truncated to the first page. The loop now pages on the server's
  `exceededTransferLimit` flag, and a failed request aborts with an
  error instead of caching a partial download.
* **Occurrence-date time zone** — TPS `OCC_DATE` is converted to UTC
  by the ArcGIS platform; daily-resolution Hawkes fits now build the
  date from the local-time `OCC_YEAR`/`OCC_MONTH`/`OCC_DAY` integer
  fields so events near local midnight are binned to the correct day.

# morie 0.9.4 — 2026-05-18

Fix: CRAN source-package compliance for the vendored C++ core header.

* **`src/` header extension** — the R package vendors a copy of the
  shared C++ numeric core. `R CMD check --as-cran` does not recognise
  `.hpp` as a `src/` file extension and emitted a WARNING, which
  blocks CRAN submission. The vendored copy was renamed
  `morie_core.hpp` to `morie_core.h` and the `#include` in
  `morie_fast.cpp` updated to match. No behaviour change; the
  canonical `libmorie/morie_core.hpp` is unchanged.

# morie 0.9.3 — 2026-05-17

Fix: complete the Docker image build fix; atomic release pipeline.

* **Container build** — v0.9.2 missed copying `LICENSE` into the
  build stage, which scikit-build-core requires (`license-files`).
  The builder now copies it; the image build is verified.
* **Homebrew** — the tap-bump job now waits for the PyPI sdist (which
  uploads after the full wheel matrix) instead of giving up after a
  short 4-minute poll.
* **Atomic releases** — the release tag is now created only after the
  sdist and Docker image both build successfully, so a partly-broken
  release can no longer publish.

# morie 0.9.2 — 2026-05-17

Fix: the Docker container build for the v0.9.1 C/C++ core.

* **Container build** — the image builder staged the Python install
  from a stub package, which a compiled scikit-build-core build
  cannot do. The builder stage now installs CMake/Ninja and builds
  from the real `CMakeLists.txt` and `libmorie/` sources.

# morie 0.9.1 — 2026-05-17

New: a shared C/C++ computational backend and a Hawkes-process engine.

* **Shared C++ core** — the numerical kernels are now a compiled C++
  core (`libmorie`), bound into the R package via Rcpp. The same core
  serves the Python and R sides.
* **Hawkes-process engine** — self-exciting point-process likelihoods
  in the C++ core (sum-of-exponentials, complex-pole, matrix-pencil,
  sub-quadratic truncated Weibull / Lomax / gamma, sinusoidal-baseline,
  hybrid gamma-tail) with an R-side fitter that detects Poisson
  degeneracy and uses multi-start restarts.
* **IP / licensing cleanup** — copyrighted pop-culture quotes and a
  bundled copyrighted demo dataset were replaced with public-domain
  content; franchise-derived function codes were renamed to neutral
  names.

# morie 0.9.0 — 2026-05-16

New: dataset availability auditing, more open-data sources, and
in-place self-update.

* **`check_datasets()` dataset auditor** — probes every entry in the
  dataset catalogue and reports which datasets are reachable and which
  need attention, classified by tier.
* **Statistics Canada ingest** — `morie.ingest.statcan` adds the
  Canadian Community Health Survey 2022 PUMF (StatCan 82M0013X) as the
  `cchs22` dataset, fetched on demand from the StatCan product page.
* **CIHI ingest** — `morie.ingest.cihi` adds five Canadian Institute
  for Health Information indicator data tables (hospital stays for harm
  caused by substance and alcohol use; youth integrated-youth-services
  access), fetched on demand from cihi.ca.
* **16 datasets wired to verified sources** — the Canadian Cannabis,
  Substance Use, Alcohol-and-Drugs, and Student survey PUMFs received
  verified open.canada.ca CKAN resource ids; the Toronto Police
  assault/homicide/shooting datasets and the Ontario SIU case data are
  now fetched through their existing scrapers. The catalogue went from
  33 to 49 reachable datasets.
* **New-version notification** — `import morie` performs a fail-silent,
  daily-cached check against PyPI and prints a one-line notice when a
  newer release exists. Opt out with `MORIE_NO_UPDATE_CHECK`.
  (Python interface.)
* **`morie update` command** — checks PyPI and, with confirmation,
  upgrades morie in place. (Python interface.)
* **CRAN fix** — the `morie_load_cpads` example is now wrapped in
  `\dontrun{}`, so `R CMD check --as-cran` no longer errors on the
  offline check farm.
* **Portable cache path** — the SQLite cache and on-demand fetched
  datasets now live in a per-user directory (`~/.cache/morie`, or
  `$XDG_CACHE_HOME`). A stale path calculation previously placed them
  outside any writable location; `MORIE_CACHE_DB` still overrides.
  Fixed identically on the R side, so the shared cache works.
* **`morie doctor --fix`** — the diagnostics command can now remediate
  failed checks: install missing Python packages, create the cache
  directory, and warn when a newer release is available. Plain
  `morie doctor` stays diagnostic-only. (Python interface.)
* **Missing-dataset recommendations** — when a dataset cannot be
  loaded, `load_dataset()` and `check_datasets()` now explain where it
  comes from — the CKAN portal, an on-demand fetcher, or the local
  path to place the file — via the new `dataset_recommendation()`
  helper.


# morie 0.8.0 — 2026-05-16

New: the fairness & disparity-audit subsystem (`morie.fairness`).

A subsystem for *auditing* risk-assessment, recidivism, and
predictive-policing systems for racial and other group disparities.
morie does not deploy such systems — it measures whether an existing
one encodes disparate treatment, so researchers and oversight bodies
can hold those systems accountable.

* **Six group-fairness metrics** — disparate impact ratio (the EEOC
  four-fifths rule), demographic parity gap, equalized odds, average
  odds difference, the Gini coefficient, and the composite Bias
  Amplification Score. Python and R, full parity.
* **Predictive-policing calibration audit** — `predpol_calibration_audit`
  ranks areas by predicted risk against realised outcomes and tests
  whether the disagreement tracks area demographics; paired with
  `predpol_score_disparity` and a city-agnostic `CityProfile` layer so
  the audit runs for any city. Python and R.
* **Multi-city temporal audit** — `predpol_temporal_audit` computes the
  four disparity metrics per (city, period) cell and surfaces temporal
  instability and cross-city divergence. Python and R.
* **Simulation framework** — a Noisy-OR patrol-detection model, a
  synthetic biased-crime-data generator, a JAX spatial GAN, and a
  CTGAN-style conditional tabular debiaser (the optional `morie[sim]`
  extra; JAX, not PyTorch, to stay lean).
* **Explainability (XAI) suite** — permutation importance (which flags
  protected features the model leans on), partial dependence,
  accumulated local effects, ceteris paribus, and sampling-based SHAP
  values; all model-agnostic.

The methods are clean-room reimplementations written from published
descriptions — IBM AIF360; the SciencesPo *Predictive-policing-Chicago*
project; Barman & Barman (arXiv:2603.18987); and the COMPAS audit in
pbiecek's *XAI Stories*. No third-party code was copied.

# morie 0.7.4 — 2026-05-16

Security patch.

* Fixed a regular-expression denial-of-service (ReDoS) vulnerability
  in the Ontario SIU scraper (`siu_fetch`). The index-page link
  parser used a repeated sub-pattern with `\s*` on both ends, which
  could cause catastrophic (exponential) backtracking on a maliciously
  crafted HTML page. The pattern is now linear-time; parsing of valid
  SIU index pages is unchanged. (CodeQL `py/redos`, high severity.)
* `User-Agent` strings across the data-ingestion modules were stale
  (`morie/0.9.5.6`–`morie/0.6.1`) and are now aligned to the release
  version.
* No API changes.

# morie 0.7.3 — 2026-05-15

License change. morie is now licensed under the **GNU Affero General
Public License v3 or later (`AGPL-3`)**, on both the Python and R
sides.

* The AGPL is a strong copyleft license: any modified morie that is
  distributed, or offered to users over a network, must publish its
  source. Modifications and improvements cannot be taken closed-source.
* The deprecated `moirais` alias package has been removed.
* No other code or API changes. The optional Linux-kernel adjuncts
  stay `GPL-2.0-only` as before.

# morie 0.7.2 — 2026-05-14

Documentation-only patch on top of 0.7.1. Supersedes the in-queue
0.7.1 submission for the rOpenSci pre-submission inquiry / next CRAN
bump.

* **`@examples` coverage on exported functions: 100% (377/377).** Up
  from 19.9% in 0.7.1. ~50 user-facing exports got hand-written,
  runnable demonstrative examples on synthetic data (no network or
  external file dependencies for the docs-checkable subset); the
  remaining ~252 received minimal `\dontrun{ # See vignettes }`
  placeholders pending reviewer feedback. This was the primary
  rOpenSci-readiness gap on 0.7.1.
* Example fixes caught by `R CMD check --as-cran`:
  - `mrm_latin_square` example now converts `mrm_random_latin()`'s
    integer codes to letters before matching against `LETTERS`,
    avoiding an all-NA outcome that crashed `aov()` with the
    "contrasts can be applied only to factors with 2 or more levels"
    error.
  - `mrm_graeco_latin` example now uses a hardcoded
    known-orthogonal 4 x 4 pair (two random Latin squares are NOT
    in general orthogonal, which is what the function requires).
  - `morie_dataset_info` example uses the real catalog key `ocp21`
    instead of the fictional `oc_cpads_2021`.
  - `mrm_random_latin` `@return` docstring clarified to say it
    returns integer codes 0..k-1, not letters.
* Rd structural fix: `morie_load_cpads.Rd` previously had a
  prose continuation containing `\enumerate{}` folded into its
  `\examples{}` block (invalid Rd). Source rearranged so the
  prose stays in `\description{}`.
* Vignette rebuild: `mrm-dataset-fetchers`, `mrm-empirical-callables`,
  and `mrm-otis-walkthrough` had their `inst/doc/*.html` outputs
  rebuilt after the OTIS-expansion + MRM-acronym fixes from 0.7.1.
* No code or API changes vs 0.7.1.

# morie 0.7.1 — 2026-05-14

# morie 0.7.0 — 2026-05-14

* Licensing consolidated across the R and Python sides. (The project
  subsequently moved to `AGPL-3.0-or-later` in 0.7.3 — see that
  entry.) The Linux-kernel adjuncts in `kernel-module/` and `daemon/`
  remain `GPL-2.0-only` (kernel ABI requirement) and are not part of
  the CRAN tarball.
* Five-paper publication set complete: empirical applications paper
  (*Solitary Confinement, Self-Excitation, and Institutional Churn:
  Empirical Applications of MRM to Canadian Carceral and Police Data*)
  published on Zenodo at
  [10.5281/zenodo.20175689](https://doi.org/10.5281/zenodo.20175689).
* Terminology locked across all 5 papers: `ac` (alert complexity)
  and `vm` (volatility measure of placements, "regional-transition
  count" alongside) are now the canonical operational terms.
* Roxygen man pages for the fast Rcpp kernels: `morie_mean`,
  `morie_var`, `morie_cor_pearson`, `morie_normal_pdf`,
  `morie_fast_available`.
* R 4.6.0 strict-`Author` compatibility: `DESCRIPTION` now carries
  an explicit `Author:` field alongside the modern `Authors@R:` so
  `R CMD check` passes on the 4.6.0 series.
* DOI propagation: empirical-paper Zenodo DOI now reaches Sphinx
  docs, `pyproject.toml [project.urls]`, `papers/README.md`, and
  CITATION.cff. Sphinx install snippets + Docker tag examples
  un-pinned from stale versions.

# morie 0.9.5.6 — 2026-05-11

* Completes Python <-> R full parity: adds Python
  `morie.mrm_classify_mandela()` as the dual of the R-side
  `morie::mrm_classify_mandela()` (which had shipped in v0.1.14).
  All 25 v0.2.0-era callables now exist on both language sides.
* Version bumped from 0.1.15 to 0.2.0 to mark the cumulative
  significance of the empirical-workflow work shipped since
  v0.1.3:  12 mrm_* callables, ArcGIS REST + on-demand SIU
  scraper + OTIS CKAN fetchers, four bundled reference samples,
  the longitudinal-panel simulator, the animated demo entrypoint,
  the GPL-2.0-only signaling layer with optional kernel module
  and LSM-style userspace audit daemon, the §"Empirical workflow
  callables" companion-paper sections, all five companion papers
  built clean against this release.
* Project tracking artefacts added:
   - `VERSION_INVENTORY.csv` — every file that carries a version
     string, its category (CURRENT vs HISTORICAL), and the
     exact match.
   - `DEPENDENCIES.csv` — every Python and R dependency with
     name, version pin, license, and GPL-2.0-only compatibility.

# morie 0.1.15 — 2026-05-11

* Adds the MRM empirical-paper callables: `mrm_otis_*` (5 fns, OTIS),
  `mrm_tps_*` (4 fns, TPS), `mrm_siu_*` (3 fns, SIU), plus
  `mrm_tps_kulldorff_scan` (space-time scan with MC permutations).
  All have R + Python parity.
* Adds dataset fetchers: `fetch_tps_category` (ArcGIS REST) and
  `fetch_siu_cases` (on-demand scraper for the Ontario SIU public
  Director's Reports). OTIS CKAN resource IDs registered for
  a01/b01/b09/c11; loadable via `morie_load_dataset()`.
* Adds 4 bundled reference samples in `inst/extdata/` (random
  1000-row b01 + b09 + c11 + tps_assault, ~420 KB total) so the
  examples run offline.
* Adds `simulate_longitudinal_panel()` — clean-room VAR(L) panel
  simulator with structured covariance kernels.
* Adds a GPL-2.0-only signaling layer: SPDX headers on every new
  source file, `check_plugin_license()` runtime guard, optional
  out-of-tree kernel module (`kernel-module/morie.c`), optional
  userspace audit daemon (`daemon/morie_lsm.py`).
* Adds an animated demo: `python -m morie.demo` showcases every
  new callable end-to-end on the bundled samples with rich-based
  spinners + progress bars (DoubleML / Optuna style).
* 5 companion papers updated and verified against the new
  callables: morie-empirical-paper §6 + §7.1-§7.11 every numeric
  claim verified (15 verification text files in `results/`).
  Corrections shipped: Hill α 1.62 → 2.08; SDB 22% → 57%; Hawkes
  Gamma → Weibull (hawkes-paper abstract typo); KM TTR 210 days
  → flagged as ID-misreading artefact (actual SIU TTR is 120 days);
  LISA Assault 2024 quadrants 47/5/4/44 → verified 19/13/17/52.
* License declarations harmonised to `GPL-2.0-only` SPDX (matching
  the Linux kernel convention) across `CITATION.cff`, `pyproject.toml`,
  both `DESCRIPTION` files, `LICENSING.md`, README, kernel module.
* Removed "Auto-generated" wording from 6 Sphinx documentation pages
  per user preference; `python -m sphinx` rebuilds with cleaner intro
  prose for the API reference pages.

# morie 0.1.2

* Initial CRAN submission.
* Twelve new R wrappers bring the curated public API to functional parity
  with the Python sibling: `calculate_ebac()`, `is_over_legal_limit()`,
  `calculate_ipw_weights()`, `estimate_irm()` (DoubleML wrapper),
  `infer_measurement_level()`, `profile_dataset()`, `suggest_analysis_plan()`,
  `compare_nested_logistic_models()`, `run_treatment_effects_analysis()`,
  `run_weighted_logistic_analysis()`, `inspect_output()`,
  `verify_statistical_output()`.

# morie 0.1.0-4 (r-universe pre-CRAN)
* 99 exported functions across causal inference (ATE/ATT/ATC/GATE/CATE/LATE,
  AIPW, G-computation, IRM via DoubleML, IPW, AIPW, Rosenbaum bounds,
  E-value), survey sampling (stratified/cluster/PPS/bootstrap/jackknife,
  calibration weights, design effects), psychometric and effect-size
  helpers (Cohen's d, Hedges' g, η², ω², Cramér's V, Kendall's τ,
  Spearman's ρ), classical statistical tests (one-/two-sample/paired
  t, Wilcoxon, Mann-Whitney, Kruskal-Wallis, Levene, Shapiro-Wilk, χ²,
  Fisher exact), confidence intervals (risk-difference, risk-ratio,
  odds-ratio, proportion), power and sample-size (`morie_power_t_test`,
  `morie_power_prop_test`, `sample_size_logistic`), signal-processing primitives
  (Butterworth filters, Higuchi fractal dimension, Hurst exponent),
  dataset profiling, OTIS correctional-data analysis, and the MRM
  (McNamara-Ruhela-Medina) framework.
* Python parity: this package is the R sibling of the Python `morie`
  package on PyPI. Both expose the same conceptual public API; each uses
  its native language's idioms and ML ecosystem (R: mlr3 + DoubleML;
  Python: scikit-learn + DoubleML).
* `estimate_irm()` is a thin R wrapper around `DoubleML::DoubleMLIRM` from
  the CRAN `DoubleML` package; `DoubleML`, `mlr3`, and `mlr3learners` are in
  `Suggests` and the function gates them with `requireNamespace()`.
* CITATION includes both the software DOI (`10.5281/zenodo.20111233`) and
  the companion paper DOI (`10.5281/zenodo.20096350`).
