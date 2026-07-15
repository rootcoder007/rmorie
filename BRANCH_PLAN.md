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
| 6 | morie_matching_genetic (native) | Matching::GenMatch | DONE (same GA budget: 0.7-1.75x of GenMatch, within 2x bar) |
| 7 | morie_matching_cardinality (native) | designmatch | DONE (already native: caliper sweep over module-1 engine; balance-guarantee tests added) |
| 8 | IPW family (ipw.R + investigation.R survey-free) | survey::svyglm | DONE (svyglm reproduced to 1e-6; 2-5x faster) |
| 9 | morie_matching_doubly_robust / morie_estimate_aipw | survey | DONE (verified already native: base stats lm/glm + bootstrap/IF SEs; no survey at runtime) |
| 10 | morie_estimate_double_ml / morie_estimate_irm (native) | DoubleML/mlr3/ranger | DONE (native-only; 40-60x faster; CI-overlap agreement with DoubleML) |
| 11 | morie_estimate_dr_forest (native R-learner forest) | grf | DONE (OpenMP kernel: 1.5-2.9x FASTER than grf incl. 100k) |
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

## Module 6 — native genetic matching

**Replaces.** `morie_matching_genetic()` (was `Matching::GenMatch` +
`Matching::Match`, dragging in rgenoud). Signature + result shape
unchanged; `details$best_weights` still carries the selected diagonal
weights (now named by covariate).

**Reference algorithm.** Diamond & Sekhon (2013, REStat 95(3)):
genetic search over the diagonal Mahalanobis weight matrix maximizing
worst-case covariate balance. Fitness = min across covariates of the
paired-t p-value on matched differences (GenMatch's default loss
family). Real-coded GA on log10-weights in [-2, 2]: elitism (top 20
percent), blend crossover, Gaussian mutation; the equal-weight
candidate is seeded into generation 0, so the search can never do
worse than plain Mahalanobis (module 2). Deterministic given `seed`.

**Implementation.** Pure R GA loop; every fitness evaluation is one
run of the module-2 streaming kd C++ kernel on weight-scaled whitened
covariates, so no Matching/rgenoud and no distance matrices. Final
match honours `n_neighbors`.

**Validation.** Structural (shape, named positive weights, seed
determinism, GA-beats-plain-Mahalanobis with slack, n_neighbors
bound); cross vs GenMatch at the same GA budget (identical matched-set
sizes; our worst-covariate SMD within 0.05 absolute of GenMatch's);
property invariants across seeds. 88+22+104 green on L14, 0 skips.

**Benchmark** (L14, R 4.6.1, 2026-07-15, pop 20 x 8 generations):
n=500 0.96s vs 0.73s (1.3x); n=2k 11.5s vs 14.5s (0.8x — faster);
n=10k 279s vs 159s (1.75x) — within the <=2x bar. GA cost is
fitness-evaluation-bound on both sides; ours trades rgenoud's C
optimizer for a dependency-free loop.

**What makes ours special.** Zero heavy deps (GenMatch pulls rgenoud +
its own snow/parallel stack), deterministic across package versions,
and the fitness kernel is shared verbatim with module 2 — one
audited matching engine under every weighted design.

## Module 7 — cardinality matching (native caliper sweep)

**Status.** Already dependency-free: the implementation sweeps calipers
(none, 0.5 ... 0.05 SD) over the module-1 native nearest-neighbour
engine and keeps the largest matched sample whose max |SMD| passes
`balance_threshold` — designmatch appears only as a doc cross-link.
Module 7 therefore adds the missing validation, not a new engine.

**Reference algorithm.** Zubizarreta (2012, JASA 107(500)) poses
cardinality matching as a MIP: maximize matched sample size subject to
balance constraints. Our sweep is a monotone heuristic for the same
objective (larger caliper -> larger sample, looser balance); it
reports honestly via `details$warning` when no caliper passes.
Upgrade path if a reviewer wants certified optimality: swap the sweep
for a bisection + assignment formulation on the module-5 kernels.

**Validation.** Structural (threshold met when feasible, explicit
warning when not, looser threshold admits >= sample); property
invariants across seeds (balance guarantee holds whenever no warning).
94+22+112 green on L14, 0 skips.

**Benchmark.** Cost = a handful of module-1 runs (each 2.2s at 100k);
designmatch needs a MIP solver (gurobi/glpk) and is not runnable as a
reference on this branch — recorded as not-benchmarked by design.

## Module 8 — native design-based weighted GLM (IPW family)

**Replaces.** `survey::svydesign(ids = ~1)` + `survey::svyglm` inside
`morie_run_ebac_selection_ipw_analysis()`; `R/ipw.R` is now
survey-free (the propensity-IPW path was already base-R). Public
outputs unchanged (same OR/linear/comparison tables).

**Reference algorithm.** Binder (1983, Int. Stat. Rev. 51):
design-based variance for GLM estimates by Taylor linearization —
sandwich over centred weighted score contributions with the n/(n-1)
factor, bread = inverse expected information of the weighted IRLS
fit. Lumley (2004, JSS) is the reference implementation.

**Implementation.** `.morie_svyglm_native()` in `R/ipw_native.R`:
coefficients from weighted `stats::glm`; variance assembled from the
model matrix + IRLS quantities. ~40 lines, no new dependencies.

**Validation.** Cross vs survey (`tests/cross/test-morie_vs_survey.R`):
coefficients to 1e-8 and standard errors to **1e-6** for both
quasibinomial and gaussian designs; the full eBAC pipeline row matches
a direct svyglm rerun on the same frame. Structural tests prove the
pipeline runs with no survey package involved. 101+22+7+112 green.

**Benchmark** (L14, 2026-07-15): 1k 0.022s vs 0.049s; 10k 0.108s vs
0.622s (5.8x faster); 100k 0.58s vs 2.92s (5x faster).

**What makes ours special.** svyglm pays the full svydesign machinery
for the ids=~1 case morie actually uses; the linearization collapses
to one crossprod + one Cholesky, which is why the SEs agree to 1e-6
at 5x the speed with zero dependencies.

## Module 9 — doubly robust / AIPW (verification)

`morie_matching_doubly_robust()` (regression-adjusted matched ATT with
bootstrap SEs) and `morie_estimate_aipw()` (influence-function SEs)
were audited and are already pure base-stats implementations — no
survey at runtime. Runtime survey usage on the branch is now confined
to `R/survey.R` (public wrappers whose documented return value IS a
survey object — API-sacred, kept) and the `survey::calibrate` deferral
in `R/weights.R` (which already has a base-R raking fallback).

## Module 10 — native double machine learning (PLR + IRM)

**Replaces.** The DoubleML/mlr3/ranger delegation inside
`morie_estimate_double_ml()` and `morie_estimate_irm()`; both are now
native-only. Also fixed a latent bug: `morie_estimate_irm` was defined
TWICE (R/causal.R and R/irm.R) with collate order silently picking the
DoubleML one — single definition now lives in R/irm.R.

**Reference algorithm.** Chernozhukov et al. (2018): Neyman-orthogonal
scores + K-fold cross-fitting. PLR: residualise Y and D via cross-fit
GCV-tuned ridge (SVD path, lambda over 10^[-3,3]); theta from the
orthogonal score with IF-based SE; `n_rep` aggregated by DoubleML's
median rule (se^2 = median(se_r^2 + (theta_r - theta_med)^2)). IRM:
cross-fit logistic propensity clipped to [0.01, 0.99] + per-arm ridge
outcome regressions, AIPW score. Both-arms validation errors early.

**Validation.** Structural (theta recovery within 3 SE, exact
determinism given seed, n_rep stability, method labels); cross vs
DoubleML+ranger (tests/cross/test-morie_vs_doubleml.R): PLR and IRM
estimates agree within the joint 95 percent margin and both cover the
simulated truth. Regression suites test-batch11 / causal-supplement /
causal-internals updated + green. 108+7+22+9+112 on L14, 0 skips.

**Benchmark** (L14, 2026-07-15, 5-fold): 1k 0.13s vs 5.4s (40x);
10k 0.52s vs 12.5s (24x); 100k 2.65s vs 161s (61x faster).

**What makes ours special.** DoubleML drags in mlr3 + ranger + future
(whose worker segfaults forced skip_on_ci guards in our own suite —
now deleted); the native path is deterministic, dependency-free, and
the GCV-SVD ridge makes every nuisance fit a single decomposition.

## Module 11 — native causal forest (R-learner)

**Replaces.** The grf delegation inside `morie_estimate_dr_forest()`;
all four `target_sample` modes native.

**Reference algorithm.** Nie & Wager (2021, Biometrika 108(2)): the
R-learner — cross-fit m(x), e(x) nuisances, then minimize the
orthogonalized loss; tau(x) fit as a weighted regression of the
pseudo-outcome (Y-m)/(W-e) with weights (W-e)^2. Athey, Tibshirani &
Wager (2019) defines the causal-forest estimand; grf is the reference
implementation. ATE via the AIPW score with forest-based mu1/mu0.

**Implementation.** New C++ kernel `src/morie_rlearner_forest.cpp`: a
weighted subsampled regression forest (greedy CART, mtry = ceil(sqrt(p)),
weighted-mean leaves, half-sample spread as a tree-noise gauge),
OpenMP-parallel over trees with per-tree RNGs + ordered reduction, so
results are seed-exact serial or parallel. Nuisances reuse the
module-10 cross-fit engines.

**Validation.** Structural (constant-effect recovery, tau(x) tracks
true heterogeneity cor > 0.5, all target_sample modes finite,
determinism); cross vs grf (ATE within joint 95 percent CI; tau(x)
correlation > 0.6 against grf predictions); property invariants.
117+3(grf cross)+112 green on L14.

**Benchmark** (L14, 2026-07-15, 500 trees vs grf defaults): 1k 0.33s vs
0.48s; 10k 4.5s vs 11.3s (2.5x faster); 100k **84.8s vs 245.6s (2.9x
faster)**. Pre-OpenMP the kernel was 3.9x slower — the tree loop
parallelization is what closes it, with determinism preserved.

**What makes ours special.** grf is a large compiled dependency with
its own ABI churn; the native forest is ~200 lines of audited C++
sharing the DML nuisance stack, seed-exact across thread counts, and
faster at every benchmarked size.
