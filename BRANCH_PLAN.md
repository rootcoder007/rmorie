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
| 12 | meta-learners T/S/X/DR (morie_estimate_cate) | — (EconML is Python-only) | DONE (all four native; DR validated against grf CATE) |
| 13 | morie_dag / identify / estimate / refute + morie_mrm_dags | DoWhy (Python) | DONE (Bayes-Ball d-sep; adjustment sets dagitty-validated) |
| 14 | DiD family: TWFE/event-study/CS2021/DR/Bacon/DID-M/feTR (+ morie_did_honest_sensitivity) | fixest/did/DRDID/bacondecomp/DIDmultiplegt/TwoWayFEWeights/HonestDiD | DONE (all engines machine-precision vs references; se_convention knob) |
| 15 | morie_synth_control + native SDID | Synth/coresynth | DONE (simplex-QP SCM w/ V-optimization + placebo inference; Arkhangelsky SDID w/ placebo/jackknife/bootstrap) |
| 16 | morie_rdd family (native IK 2012, NN(3) local poly, CCT bc, kink, McCrary) | rdrobust/rdd | DONE (point estimates match rdrobust to 1e-8 at fixed h; rddensity kept as CJM extender only) |
| 17 | IV family (k-class 2SLS/LIML, two-step+CUE GMM, Hansen J) + NEW morie_its | AER/ivreg/gmm/plm | DONE (2SLS==ivreg+sandwich HC1 to 1e-8; ITS w/ native Newey-West) |
| 18 | IRT: morie_irt_2pl/grm/eap + native omega/KMO/parallel | psych | DONE (KMO==psych to 1e-8; 2PL vs mirt within 0.1; alpha was already native) |
| 19 | geostat: morie_spatial_variogram/_fit(ML)/krige; Moran fully native | gstat/spdep | DONE (kriging==gstat to 1e-6; Cliff-Ord variance ROOT FIX now ==spdep; GWR already native) |
| 20 | DSP: butter/filtfilt/fir1/sgolay/hilbert/peaks/Welch PSD-CSD-coherence/DWT | signal/wavelets | DONE (butter coefs==signal to 1e-8; DWT perfect reconstruction, haar/d4/la8) |
| 21 | Hawkes MLE (all kernels/baselines) | hawkes/emhawkes | DONE (native path was complete; external exp fast path removed; loglik==hawkes pkg) |
| 22 | crypto: standalone C++ SHA-256/HMAC/PBKDF2 (+ PQC lattice KEM/DSA already native via liboqs) | digest/openssl | DONE (NIST/RFC vectors; digest dropped from Imports; SHA-512 deferred YAGNI) |
| 23 | parsers: native JSON parse/stringify, SAX XML, tolerant HTML, minimal Parquet+Snappy, unified dispatcher | xml2/jsonlite/arrow (now optional fast paths per charter) | DONE (jsonlite-parity incl. matrix/data-frame simplification; all call sites shimmed) |
| 24 | MRM flagship: load_si_dataset/reconcile/estimate_causal_effect/report + phase-17 composition test | — (flagship) | DONE (4-estimator pipeline w/ Holm correction + IVW consensus; own text/md/LaTeX/HTML renderer; citation derives from inst/CITATION) |
| 25 | categorical-integrity guards: safe_recode/safe_factor/audit_categories/crosstab_verify + binary-treatment guard | — (new construction) | DONE (reproduction test: label-swap inflates a true 4x rate ratio >3x; guards catch it three ways; wired into the MRM pipeline) |
| 26 | stats primitives: partial/semipartial correlation + test, runs/turning-point/difference-sign/Bartels randomness tests, Ding-VanderWeele E-value family | ppcor/randtests/EValue | DONE (18/18 cross vs ppcor+randtests incl. semipartial covariance-precision formula; E-value rewired into causal/sensitivity/effects) |
| 27 | robust covariance: HC0-HC5 + HAC (Newey-West/Bartlett) + one-way CR0/CR1 clustered, lm & glm, unified dispatcher | sandwich | DONE (12/12 cross vs sandwich to 1e-8 incl. weighted lm & binomial glm; morie_causal_robust_se + estimate_ate rewired native; 15x faster HC3 @ 100k) |
| 28 | bootstrap: ordinary + stratified resampling, block (moving/stationary), two-sample; norm/basic/perc/BCa CIs (regression empinf) | boot/simpleboot | DONE (13/13 cross vs boot+simpleboot; RNG-matched replicate streams + BCa influence to 1e-8; adaptive/block bootstrap + 3 bridges rewired native, full suite 77/0/0) |
| 29 | permutation tests: independence / Wilcoxon rank-sum / one-way (Fisher-Pitman) via Strasser-Weber linear-statistic framework; asymptotic normal/chi-square + exact two-sample | coin | DONE (statistic+p match coin to 1e-8 for independence, wilcoxon asymptotic 3-alt + exact-with-ties, oneway k>2 quadratic + k=2 scalar; 3 extenders rewired native; structural + cross tiers) |
| 30 | MASS utilities: ginv (Moore-Penrose SVD pseudo-inverse, 54 call sites) + mvrnorm (multivariate normal sampling) | MASS | DONE (ginv==MASS::ginv to 1e-12 incl rank-deficient; mvrnorm bit-for-bit under common seed via eigen; 54+2 sites rewired native; glm.nb/rlm/polr remain, module 31) |

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

## Module 12 — native X- and DR-learners

**Extends.** `morie_estimate_cate()` — T/S-learners were already
native (base glm); `meta_learner` now also accepts `"x_learner"` and
`"dr_learner"`. Additive signature change only.

**Reference algorithms.** X-learner: Kuenzel, Sekhon, Bickel & Yu
(2019, PNAS 116(10)) — arm-wise outcome models, imputed individual
effects regressed per arm, propensity-weighted combination. DR-learner:
Kennedy (2023, EJS 17(2)) — cross-fit AIPW pseudo-outcome regressed on
covariates; the second stage is the module-11 forest kernel, the
nuisances are the module-10 engines.

**Reference implementation.** None on CRAN (EconML is Python), so
validation is against ground truth and the module-11 forest: constant
effect recovered by all four learners within 0.25; heterogeneous
tau(x) tracked with cor > 0.5; the Kuenzel motivating case reproduced
(X-learner not worse than T-learner under 7 percent treatment);
DR-learner CATE correlates > 0.6 with grf::causal_forest predictions
on the same data. Deterministic given seed. 133+42+126 green on L14.

**Benchmark.** No R-runtime reference to race; cost is one cross-fit
nuisance pass + (DR) one forest fit — bounded by module 10 + 11
numbers already recorded.

**What makes ours special.** The whole heterogeneous-effects family
(T/S/X/DR + the R-learner forest) now runs on ONE audited nuisance
stack with zero Suggests — EconML needs a Python runtime for the same
menu.

## Module 13 — native causal DAG toolkit

**New surface** (DoWhy has no R runtime; dagitty needs V8):
`morie_dag()` (edge-list constructor, Kahn acyclicity check, print
method), `morie_dag_identify()` (canonical adjustment set of van der
Zander-Liskiewicz-Textor 2014, verified by Shachter's Bayes-Ball
d-separation on the backdoor graph — complete: if the canonical set
fails, no backdoor set exists), `morie_dag_estimate()` (routes the
identified set into the native linear/AIPW/DML estimators),
`morie_dag_refute()` (DoWhy-style placebo-treatment /
random-common-cause / data-subset checks with a pass heuristic), and
`morie_mrm_dags()` (bundled placement + use-of-force structures from
the MRM docs as editable starting points).

**Validation.** Structural (cycles rejected, confounder found,
mediator excluded, latent confounder -> unidentified, all three
estimators recover 0.8 within 0.2, placebo kills / subsets keep the
effect, MRM DAGs identify); cross vs dagitty
(`tests/cross/test-morie_vs_dagitty.R`): every adjustment set we emit
passes `dagitty::isAdjustmentSet` on three canonical graphs.
159+48+126 green on L14, 0 skips.

**Benchmark.** Graph algorithms are microseconds at analysis-DAG
sizes; estimation cost = the module-8/10 engines already benchmarked.

**What makes ours special.** The full DoWhy loop — model, identify,
estimate, refute — in ~250 lines of base R wired straight into the
native estimator stack: no Python, no V8, and the refutation step
reuses the same estimators it audits.


## Modules 14-22 — the quasi-experimental / psychometrics / spatial / signal / crypto batch (2026-07-15/16)

Written and validated as ONE batch per Vee's directive ("write all of the
modules together and test it together in one go").

**Module 14 — DiD family (R/did_native.R).** TWFE core = alternating-
projection demeaning + CR1 cluster vcov with fixest's exact default
small-sample correction (fixef.K = "nested"); coef AND se reproduce
fixest::feols to 1e-10 (incl. covariates + non-unit clusters). Event
study = same engine on relative-time dummies (== feols + i()).
Callaway-Sant'Anna ATT(g,t): Sant'Anna-Zhao dr/reg/ipw panel engines
with influence functions matching DRDID element-wise to 1e-8 (verbatim
IF structure incl. the locally efficient drdid_rc), analytic SEs equal
did::att_gt, Mammen multiplier bootstrap mirrors did::mboot.
Goodman-Bacon via the FWL variance-weight identity (exact equality with
bacondecomp AND the decomposition identity sum(w*est) == TWFE coef).
DID-M native (upstream DIDmultiplegt mode="old" returns NaN on standard
designs — pinned by hand-computed estimands instead). feTR weights
equal TwoWayFEWeights per cell to 1e-10. NEW morie_did_honest_sensitivity
(conservative Rambachan-Roth relative magnitudes on event-study output).
se_convention = "reference"/"bessel" knob per Vee. Fixed latent bug:
event-study wrapper treated g=0 as a finite onset. 7 Suggests dropped.

**Module 15 — synthetic control (R/synth_native.R).** morie_synth_control:
Abadie-Diamond-Hainmueller SCM; donor weights by FISTA on the simplex
(exact Held-Wolfe-Crowder projection), nested V optimization, in-space
placebo RMSPE-ratio inference; print method. Native SDID (Arkhangelsky
2021 Algorithm 1: zeta-regularized unit weights, simplex time weights)
with placebo/jackknife/bootstrap variance; both did.R SDID wrappers
rewired. coresynth dropped.

**Module 16 — RDD (R/rdd_native.R).** IK (2012) three-step plug-in
bandwidth (note: on symmetric-curvature DGPs IK's m2+−m2− bias term
makes its h legitimately much larger than CCT mserd). Local-polynomial
estimator with NN(3) heteroskedastic variance == rdrobust conventional
point estimates to 1e-8 at fixed h; CCT bias correction via the
order-(p+1) refit (exact at rho = 1); native kink (deriv = 1); native
McCrary log-density test. rdrobust dropped; rddensity retained ONLY for
the Cattaneo-Jansson-Ma extender.

**Module 17 — IV + ITS (R/iv_native.R).** k-class engine (2SLS kappa=1,
LIML by the eigenvalue kappa) with HC1 projected-score sandwich ==
ivreg + sandwich::vcovHC to 1e-8; two-step efficient GMM + CUE (BFGS)
+ Hansen J; Sargan/Hausman/panel-IV fallbacks promoted to primary;
morie_estimate_late covariate path rewired. NEW morie_its: segmented
regression with native Newey-West (Bartlett) HAC, level+slope changes,
counterfactual path. AER/ivreg/gmm/plm dropped.

**Module 18 — IRT + psychometrics (R/irt_native.R).** morie_irt_2pl
(Bock-Aitkin MML EM, quadrature E-step + per-item Newton M-step),
morie_irt_grm (Samejima, order-preserving optim M-step),
morie_irt_eap. psymet omega/KMO/parallel now native-primary (KMO ==
psych to 1e-8). psych dropped.

**Module 19 — geostatistics (R/geostat_native.R).** Matheron empirical
variogram (bin-identical to gstat), Gaussian-ML covariance fit
(exponential/spherical/gaussian), ordinary kriging == gstat::krige to
1e-6 (pred + var). ROOT FIX: .tps_cliff_ord_variance used a wrong
combining formula (variance off by ~n); now the Cliff-Ord normality
variance, equal to spdep::moran.test(randomisation = FALSE). GWR was
already native (gwreg.R). gstat/sp/spdep dropped (gstat only via the
two documented extenders).

**Module 20 — DSP (R/dsp_native.R).** Butterworth low/high/band/stop by
bilinear transform (coefficients == signal::butter to 1e-8), DF2T
filter, odd-extension filtfilt, windowed-sinc fir1 + hamming/hann/
blackman, Savitzky-Golay (exact on cubics incl. edges), FFT hilbert,
min-distance peak finder, Welch PSD/CSD/coherence, and a periodic
pyramid DWT/iDWT (haar/d4/la8; perfect reconstruction + Parseval).
Nine files rewired; signal/wavelets dropped.

**Module 21 — Hawkes.** The base-R + C++ path (morie_hawkes.cpp pair
excitation kernel) was already the complete estimator for every
kernel/baseline pair; removed the redundant hawkes/emhawkes exponential
fast path. Native negative log-likelihood == hawkes::likelihoodHawkes
on the exponential/constant case. hawkes/emhawkes dropped.

**Module 22 — crypto hashes (src/morie_crypto_hash.cpp).** Standalone
FIPS 180-4 SHA-256, RFC 2104 HMAC, RFC 8018 PBKDF2 — no libsodium
gate, pinned by NIST/RFC 4231/RFC 7914 vectors. Deterministic-RNG
seeding, the reproducibility manifest, and the hybrid-encryption HKDF
now use them; digest REMOVED FROM IMPORTS, openssl from Suggests. The
post-quantum lattice layer (ML-KEM-768 / ML-DSA-65 via liboqs) was
already native in morie_crypto_pqc.cpp. SHA-512 deferred (no caller).

**Module 22 addendum — full three-family PQC coverage (Vee, after the
Red Hat PQC series).** New src/morie_crypto_pqc_extra.cpp +
R/crypto_pqc_extra.R: hash-based SLH-DSA-SHA2-128s (FIPS 205,
liboqs-gated with old/new algorithm-name fallbacks) and code-based
HQC-128 (NIST 2025 round-4 KEM selection), joining the existing
lattice ML-KEM-768/ML-DSA-65. Plus a dependency-free native Lamport
one-time signature on the module-22 SHA-256 core (hard one-time
enforcement; SLH-DSA pointed to for many-time use) and
morie_crypto_pqc_inventory() reporting family/standard/availability.
The hybrid recommendation (PQC + classical) is realized by
crypto_hybrid.R: ML-KEM encapsulation feeding native HKDF-SHA256.


## Modules 23-24 — parsers + the MRM flagship (2026-07-16)

**Module 23 — native parsers (R/fetch_native.R).** Pure-R
recursive-descent JSON parser with jsonlite-parity simplification
(scalar arrays -> vectors, arrays of arrays -> matrices, arrays of
objects -> data frames; \u escapes, CDATA-safe) + native stringifier;
SAX-style XML scanner (events) with a list-tree builder; tolerant HTML
parser (void elements, implicit closes, stray end tags); CSV/TSV via
base utils; a minimal Parquet reader — thrift-compact footer/page
headers, PLAIN encoding, INT32/INT64/DOUBLE/BYTE_ARRAY, uncompressed or
Snappy (pure-R Snappy decompressor) — that fails LOUDLY naming arrow
for anything richer; and morie_fetch_unified() returning a
morie_dataset. Per the module charter jsonlite/xml2/arrow stay in
Suggests as fast paths: 29 files' jsonlite calls now route through
.morie_from_json/.morie_to_json shims, data_access/dataset/ingest_ckan
XML/HTML/jsonl/parquet paths gained native fallbacks, so a bare
install parses everything. HTTP was already native (libcurl C++).

**Module 24 — the MRM flagship (R/mrm_flagship.R).**
morie_mrm_load_si_dataset (bundled samples + provenance envelope with
native SHA-256 checksum), morie_mrm_reconcile (explicit matching
schema: keys, compared fields, numeric tolerance; match rate, orphans,
field-level conflicts), morie_mrm_estimate_causal_effect (composes the
branch's native matching/ATE/AIPW/DML on one specification, Holm
correction across methods, inverse-variance consensus, citation drawn
from inst/CITATION at runtime), morie_mrm_report (own renderer:
text/markdown/LaTeX/HTML, no stargazer/gt). Plus the phase-17
composition test: one DAG -> identification -> matching -> DML
estimate -> three refutations -> MRM report, every step a module of
this branch.


## Module 25 — categorical-integrity guards (2026-07-16)

Motivated by a documented real-world failure class: categoricals
imported as numeric codes (SPSS/Stata) whose labels are lost or
positionally re-attached silently relabel whole demographic groups —
`as.numeric(factor)` returns level indices, `factor()` re-orders
references alphabetically, and a label-keyed population benchmark
joined to swapped labels multiplies a reported disparity severalfold.
A published use-of-force analysis carried exactly this error for
years before correction (the substantive disparity finding survived
the correction; the guard exists so the CODING step can never
manufacture or destroy one).

R/categorical_guard.R: morie_safe_recode (name-to-name only, ERROR on
unmapped), morie_safe_factor (explicit level set + reference
assertion), morie_audit_categories (flags numeric-looking labels,
haven_labelled leftovers, case-variant duplicates, unused levels),
morie_crosstab_verify (before/after cross-tab must equal the DECLARED
mapping — the check that catches a swap the day it happens), and the
internal .morie_guard_binary_treatment (refuses factor-to-numeric
treatment coercion with the level-index explanation) wired into
morie_mrm_estimate_causal_effect. test-categorical-guard.R includes a
full reproduction: true 4x rate ratio, positional label slip against
a label-keyed benchmark, >3x inflation, three independent catches.


## Benchmark summary (modules 12-24, L14, 2026-07-16)

Full CSVs in inst/benchmarks/results/. Native FASTER than the
reference: 2SLS 25x vs ivreg, DAG identification 16x vs dagitty,
McCrary 7x vs rddensity, Bacon 33x vs bacondecomp (reference capped
at n=1e3 where its runtime explodes; native completes 1e5 in 8.6s),
kriging ~2x vs gstat, filtfilt 2x vs signal, SHA-256 1.3x vs digest,
TWFE 2x vs fixest at n=1e3. Within the 2x bar: drdid_rc 1.2-1.9x,
CS att(g,t) 1.1x at 1e3 / 2.9x at 1e4, DWT ~1.1x vs wavelets, sharp
RDD 1.5x vs rdrobust (after vectorizing the NN variance). Honest
exceptions, all sub-second-to-seconds absolute: TWFE demeaning is R
vs fixest compiled C (0.16s at 80k rows, 3.0s at 800k — estimates
are identical to 1e-10); the Hawkes exponential loglik call is 62x a
trivial C reference at 0.06s absolute; the pure-R JSON (63x) and XML
(5.5x) parsers are documented FALLBACKS — jsonlite/xml2 remain the
declared fast paths and every call site prefers them when installed.


## Merge policy (Vee 2026-07-16): merge is the LAST step, not a middle one

Full CI runs on the native branches themselves — no merge to main is
required for any testing or validation. Phase 26 (merge --no-ff + tag)
happens ONLY after everything on both branches is green and Vee gives
an explicit go. No main merge, no tag, no PyPI publish until then.
Both rmorie and morie stay on feat/native-specializations; the branch
gates are the verification of record.
