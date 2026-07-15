# feat/native-specializations — branch plan

Goal: every specialization morie exposes gets a native implementation
with zero Suggests at runtime. Modules land on `main` only by
cherry-pick, only when their full protocol is green: BRANCH_PLAN entry,
structural tests, `tests/cross/` vs the reference (Suggests allowed
*only* there), `tests/property/`, `inst/benchmarks/` (≤2x reference at
100k rows), and `R CMD check --as-cran` green.

Public API is sacred: existing function names, argument names, defaults,
and the `morie_match_result` / result-object shapes do not change.

## Module registry

| # | module | replaces | status |
|---|--------|----------|--------|
| 1 | morie_matching_nearest_neighbor (native) | MatchIt::matchit(method="nearest") | DONE (49/49 tests; 1.4x of MatchIt at 100k — within the 2x bar) |
| 2 | morie_matching_mahalanobis (native) | MatchIt (mahalanobis distance) | planned |
| 3 | morie_matching_cem (native) | MatchIt (cem) | planned |
| 4 | morie_matching_exact (native) | MatchIt (exact) | planned |
| 5 | morie_matching_optimal (new) | MatchIt/optmatch | planned |
| 6 | morie_matching_genetic (native) | Matching::GenMatch | planned |
| 7 | morie_matching_cardinality (native) | designmatch | planned |
| 8 | morie_ipw_ate / att / trimmed (native) | survey::svyglm | planned |
| 9 | morie_doubly_robust (native) | survey | planned |
| 10 | morie_dml (native cross-fit) | DoubleML/mlr3 | planned |
| 11 | morie_causal_forest | grf | planned |
| 12 | meta-learners T/S/X/DR | — (new) | planned |
| 13 | morie_dag / identify / estimate / refute | — (new) | planned |
| 14 | morie_did (+ staggered CS2021) / event study | did/DIDmultiplegt | planned |
| 15 | morie_synth_control | Synth | planned |
| 16 | morie_rdd (IK bandwidth) | rdrobust/rdd | planned |
| 17 | morie_its / morie_iv_2sls | AER/ivreg | planned |
| 18 | psych: 2PL EM, GRM, alpha, omega, EAP | psych/lavaan | planned |
| 19 | spatial: variogram MLE, kriging, GWR | gstat/spdep | planned |
| 20 | signal: PSD/coherence, Butterworth+filtfilt, MODWT | signal/wavelets | planned |
| 21 | Hawkes uni/multivariate MLE | hawkes/emhawkes | planned |
| 22 | crypto: sha256/512, hmac, pbkdf2, csprng | digest/openssl | planned |
| 23 | parsers: XML(SAX)/HTML/JSON/parquet-min | xml2/jsonlite/arrow | planned |
| 24 | MRM: load/reconcile/estimate/report | — (flagship) | planned |

---

## Module 1 — native nearest-neighbour propensity matching

**Replaces.** `morie_matching_nearest_neighbor()` — currently a thin
wrapper over `MatchIt::matchit(method = "nearest", distance = "glm")`.
Signature and the `morie_match_result` return shape are preserved
exactly (`matched_data`, `n_treated`, `n_matched_control`,
`match_pairs(treated_idx, control_idx, distance)`, `method`,
`details`).

**Reference algorithm.** Greedy nearest-neighbour matching on the
logit propensity score: Rosenbaum & Rubin (1985), "Constructing a
Control Group Using Multivariate Matched Sampling Methods That
Incorporate the Propensity Score", *The American Statistician* 39(1).
Caliper matching per Cochran & Rubin (1973): caliper expressed in SD
units of the logit propensity. Propensity model: logistic regression
(base `stats::glm`), identical to MatchIt's `distance = "glm"`.

**Reference implementation + tolerance.** `MatchIt::matchit(method =
"nearest", distance = "glm")`. Cross-test asserts (a) identical
matched-set *size*, (b) post-matching ATE (difference in means on the
matched data) within 1e-3 on n=500 and 1e-2 on n=50,000, (c) identical
pairs on a tie-free DGP with `m.order = "largest"` ordering.

**Internal structures.** No new S3 class in module 1 (shape parity
first); the sorted-logit index (`order()` + `findInterval()` +
outward expansion over an availability mask) is the matching engine.

**Tests.** `tests/testthat/test-matching-native.R` (structural +
known-DGP balance), `tests/cross/test-morie_vs_matchit.R`,
`tests/property/test-properties-matching.R`.

**Benchmark.** `inst/benchmarks/bench-morie_vs_matchit.R`. Measured
(L14, R 4.6.1, 2026-07-15): n=1k 1.7x faster; n=10k 0.4x; n=100k 0.7x
(i.e. 1.4x slower) — within the branch's <=2x release bar. The C++
kernel itself is O(n log n); the remaining gap is the base-glm
propensity fit and result assembly. Revisit if profiling shows a win;
the honest headline is parity with MatchIt, plus version-stability.

**What makes ours special.** Greedy PS matching only ever needs
*one-dimensional* nearest-neighbour lookup on the logit score, so the
right data structure is a sorted vector with binary search and outward
expansion — O(n log n) total, no distance matrix, no KD-tree needed,
memory O(n). MatchIt is a general framework and pays generality costs;
morie's matcher is specialized, deterministic across versions (the
result can never change under us because we own every line), and
returns influence-ready matched data that chains directly into the
Phase-3 native estimators.
