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
| 2 | morie_matching_mahalanobis (native) | MatchIt (mahalanobis distance) | DONE (60/60 suite; whitened-kd C++ kernel, exact strata, caliper) |
| 3 | morie_matching_cem (native) | MatchIt (cem) | DONE (structural+cross+property green; 1.3x of MatchIt at 100k) |
| 4 | morie_matching_exact (native) | MatchIt (exact) | DONE (parity-to-faster vs MatchIt at all sizes) |
| 5 | morie_matching_optimal_pair (native) | MatchIt/optmatch | DONE (exact optimum; 7-14x faster; completes 100k where optmatch OOMs) |
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

## Module 2 — native Mahalanobis matching

**Replaces.** `morie_matching_mahalanobis()` (was
`MatchIt::matchit(method = "nearest", distance = "mahalanobis")`).
Signature and `morie_match_result` shape unchanged, including `exact`
strata support.

**Reference algorithm.** Rubin (1980), "Bias Reduction Using
Mahalanobis-Metric Matching", *Biometrics* 36(2). Covariance estimated
on the control pool; caliper in Mahalanobis-distance units.

**Implementation.** Cholesky whitening (Mahalanobis distance in the
original space == Euclidean distance in whitened space) + a streaming
greedy k-d scan kernel in C++ (`src/morie_matching_mahal.cpp`):
O(nt·nc·k) compute with early-exit, O(n) memory — no distance matrix,
which is the reference paths' memory wall. Exact strata are matched
independently per stratum. Near-singular pools get a proportional
ridge on the covariance diagonal.

**Validation.** Structural (shape, no-reuse, balance SMD < 0.1 on weak
confounding, strata never cross, caliper bound); cross vs MatchIt
(identical matched-set sizes and ATE agreement on a generous-pool DGP
where greedy order is irrelevant); property invariants shared with
module 1. 60/60 on L14.

**What makes ours special.** Whitening turns every pairwise
Mahalanobis evaluation into a plain squared-Euclidean loop — one
Cholesky for the whole problem instead of a solve per pair — and the
streaming kernel never materializes the O(n²) distance matrix that
makes the reference implementation infeasible at scale. Exact-strata
support runs the kernel per stratum, so mixed exact+distance designs
are first-class rather than an afterthought.

## Module 3/4 — native CEM + exact matching

**Replaces.** `morie_matching_cem()` (was `MatchIt::matchit(method =
"cem")`, which shells to the \pkg{cem} package) and
`morie_matching_exact()` (was `method = "exact"`). Signatures and the
`morie_match_result` shape unchanged; matched data now carries
`weights` + `subclass` columns (the CEM/exact estimand is
weighting-based — `match_pairs` is empty by design, as with MatchIt).

**Reference algorithm.** Iacus, King & Porro (2012), "Causal Inference
without Balance Checking: Coarsened Exact Matching", *Political
Analysis* 20(1). Exact matching is the degenerate no-coarsening case.
Control weights: (m_t_s / m_c_s) * (M_c / M_t) per stratum; multivariate
L1 imbalance reported in `details$l1_before`.

**Implementation.** Pure base R: stratum keys via `paste(sep = "\r")`,
two `table()` passes, one subset. Coarsening = quantile cutpoints per
covariate (`n_bins` integer or per-variable list; Sturges' rule on NA);
low-cardinality numerics (binary/ordinal codes) pass through as
discrete — quantile breaks on a 0/1 column collapse to a single bin and
silently uncontrol the variable (caught by the balance test).

**Validation.** Structural (two-arm strata only, CEM weight convention,
exact weighted balance to 1e-8, SMD < 0.1 on the weak-confounding DGP,
per-variable bins, coarser-bins-retain-more); cross vs MatchIt (exact:
identical retained units + weighted ATT to 1e-6; CEM: ATT within 5e-2
across binning conventions, both near the true effect); property
invariants over 4 seeds. 64+10+76 green on L14.

**Benchmark** (L14, R 4.6.1, 2026-07-15): exact 0.36s vs MatchIt 0.37s
at 100k (faster at 1k/10k); CEM 0.52s vs 0.40s at 100k (1.3x — within
the <=2x bar).

**What makes ours special.** The whole CEM estimator is two hash-table
passes — no cem-package dependency chain (MatchIt's cem path drags in
\pkg{cem} + lattice/randomForest transitively), deterministic across
versions, and the L1 diagnostic comes for free from the same tables.
Low-cardinality guard makes binary covariates safe by construction.

## Module 5 — native optimal pair matching

**Replaces.** `morie_matching_optimal_pair()` (was
`MatchIt::matchit(method = "optimal")` -> \pkg{optmatch}). Signature +
result shape unchanged; both `distance = "propensity"` and
`"mahalanobis"` modes native.

**Reference algorithm.** Rosenbaum (1989, JASA 84(408)): optimal
matching as a minimum-cost assignment; Hansen & Klopfer (2006) for the
reference implementation. Propensity mode exploits the non-crossing
property of 1-D optimal matching: sorted treated match to a monotone
subsequence of sorted controls, so the assignment collapses to a
dynamic program over the two sorted score vectors — O(nt*nc) time,
uint8 backtrack, no distance matrix. Mahalanobis mode: exact shortest
augmenting-path assignment (Jonker-Volgenant duals) on Cholesky-
whitened covariates, float cost matrix, guarded at nt*nc <= 5e7.

**Validation.** Structural (shape, every treated matched, no reuse,
optimal <= greedy total, balance SMD < 0.1); cross vs MatchIt/optmatch:
identical matched-set sizes, both recover the simulated true effect,
and — fed optmatch's own probability-scale distances — our DP total is
**never worse and strictly better** than optmatch's (0.4884 vs 0.4968
on the test DGP; optmatch solves on a rounded integer-cost grid, ours
is exact). Property invariants over 3 seeds. 77+20+94 green on L14, 0
skips (cem + optmatch installed).

**Benchmark** (L14, R 4.6.1, 2026-07-15): 1k 0.045s vs 0.62s (13.8x
faster); 10k 1.66s vs 12.3s (7.4x); 100k ours 182s, optmatch
OOM-killed (~12 GB dense distance matrix). Well inside the bar.

**What makes ours special.** The 1-D DP needs no distance matrix at
all — optimal matching's memory wall disappears, so exact optimal
matching becomes feasible at 100k rows where the reference cannot run.
Exactness beats the reference's rounded-grid optimum, deterministically.
