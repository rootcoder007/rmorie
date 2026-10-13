# The hardest open problems in criminology: a working ledger

Rootcoder007, started 2026-09-21. Fresh start: nothing here is
taken from earlier MRP/VSR work; those results are a later reproducibility
target inside rmorie, not the seed. The Lean sources live in `research/lean/`
of this repository; the R functions in `R/`, the Python port in morie.

Selection rule: a problem is on this list if the field itself calls it
unsolved AND a correct, machine-checked method would change what the field
can conclude, not just how fast it computes. Each entry states the estimand,
the identifying assumptions, what must be proved, and what would count as
"cracked".

Sources are listed at the end; every claim below traces to one of them or
to standard results.

## P1. The dark figure: what is the true crime count?

Official counts record a selected, non-random subset of offences; surveys
add misreporting in both directions (under-reporting from stigma and
non-recall, over-reporting from misunderstanding). Parametric corrections
assume one direction and a known mechanism. The 2024 JQC paper shows the
honest answer is a *partial identification* problem: with weak, credible
assumptions the dark figure is an interval, not a point.

- Estimand: true incidence N* (per area, per offence) and the reporting
  rate r = N_recorded / N*.
- Assumptions to formalise: (a) survey misreport rates bounded by known
  constants (both tails); (b) recorded counts are a monotone function of
  true counts; (c) optional: two-source independence (capture-recapture).
- To prove in Lean: the sharp identified set of N* under (a) and (b)
  (Manski-style bounds), that adding (c) tightens to a point and how each
  assumption violation moves the bound; that the estimator of the bounds is
  consistent.
- Cracked means: a function that returns the sharp interval plus a
  breakdown analysis ("how much misreporting would it take to move the
  conclusion"), with the interval's sharpness proved, applied to a real
  survey/recorded pair (CSEW small-area style, or Canadian GSS + UCR).
- rmorie now: nothing on partial identification of counts. Hawkes and
  sensitivity modules exist but do not address this.
- Progress 2026-09-21: Lean `Research.P1` (research/lean/P1DarkFigure.lean):
  Petersen identity N = θ n₁n₂/m for a two-source model with dependence
  factor θ, Lincoln–Petersen as the θ = 1 case, the interval
  [n₁n₂/(κm), κn₁n₂/m] for θ ∈ [1/κ, κ] with both ends attained, and the
  survey+record interval max(r, (v_obs−ᾱ)/(1−ᾱ)) ≤ v ≤ v_obs/(1−β̄) built on
  the P5 noise theorems, plus the dark-figure version; 8 theorems, 0
  sorry. R: `morie_dark_figure_two_source` (with Chapman), `morie_dark_figure_bounds`.
  Next: breakdown analysis (how large must β̄ be to move a conclusion), the
  small-area application on a real survey/record pair, and the
  three-source log-linear case with proved identification under one
  interaction.

- Progress 2026-09-21 (three lists): `Research.P1` (P1ThreeList.lean):
  `petersen_ge_floor`, `chapman_ge_floor` (both estimators respect the
  logical floor n1+n2-m; the Wald interval does not), and
  `three_list_saturated_fits` / `missing_cell_unconstrained` (the
  saturated log-linear model reproduces any positive eight-cell table, so
  the count of units on no list is free: N is identified only by the
  analyst's choice of the three-way interaction, conventionally zero).
  R: `morie_dark_figure_three_list()`; the test matches the closed form
  against a Poisson GLM fit and sweeps the floor on 200 random tables.

- Progress 2026-09-21 (hierarchy rule): `Research.P1.offence_count_bounds`,
  `offence_count_eq`, `category_not_identified` (P1Hierarchy.lean):
  incident counting under a hierarchy rule versus offence counting differ
  by exactly the mean extra offences per incident, bounded by the maximum
  co-offence multiplicity (library scan A13). R:
  `morie_dark_figure_hierarchy()`; empirical target: FBI CDE SRS-to-NIBRS
  transition years by agency.
- Progress 2026-10-02 (Le Cam, library scan C10): `Research.P1LeCam` (P1LeCam.lean). On a
  finite space, sum min(p, q) = 1 - TV (`sum_min`, `tv_nonneg`, `tv_le_one`) and for every
  estimator E_p|T - theta_p| + E_q|T - theta_q| >= |theta_p - theta_q| (1 - TV)
  (`two_point`), so the worse risk is at least |theta_p - theta_q| (1 - TV)/2 (`minimax`):
  two recording mechanisms close in total variation but with true rates delta apart leave
  every estimator a worst-case error of delta (1 - TV)/2. R: `morie_two_point_bound()`;
  Python `two_point_bound()`.
- Progress 2026-10-12 (breakdown, three sources): the breakdown value β̄* = 1 − v_obs/t of the
  under-reporting box was proved on 2026-10-02 (P1Breakdown.lean: `conclusion_holds_below_breakdown`,
  `conclusion_fails_above_breakdown`; `morie_fairness_compare_groups` reports it) and is recorded
  here. The three-source case under one interaction constraint: `Research.P1.Interaction`
  (P1Interaction.lean) writes the 2×2×2 expected cells from the log-linear parameters and proves
  that under u₁₂₃ = 0 the never-listed cell is m₁₀₀m₀₁₀m₀₀₁m₁₁₁/(m₁₁₀m₁₀₁m₀₁₁)
  (`missing_cell_identified`, `total_identified`), that with interaction t the same ratio equals
  m₀₀₀eᵗ (`missing_cell_with_interaction`) so the data never choose t
  (`interaction_not_identified`). R: `morie_dark_figure_no_interaction()` (the test matches the
  Poisson log-linear fit with every two-way term); Python `dark_figure_no_interaction()`.
  Open: the small-area application on a real survey/record pair.
- Progress 2026-10-12 (the survey/record pair, with the NCVS): the NCVS 1992-2022 concatenated
  file (ICPSR 38604, https://doi.org/10.3886/ICPSR38604.v1) is held and served
  (`ncvs_38604/*` at data.rmorie.com). `research/applications/ncvs_victimisation.R` takes the
  victimisations the survey counts (WGTVICCY with the BJS series rule) against the ones the
  victim says the police learned of (V4399), by year and class, and applies
  `morie_dark_figure_bounds()` with stated misreport bounds (alpha_max 0.05, beta_max 0.25):
  violent victimisations per person 0.081 in 1993 and 0.024 in 2022, of which the police learned
  of 0.034 and 0.010 (reported shares 0.42 and 0.41), identified sets [0.034, 0.107] and
  [0.010, 0.031]; property per household 0.354 and 0.102, sets [0.320, 0.472] and [0.055, 0.136].
  The breakdown value of the decline (`Research.P1Breakdown`): under-reporting in 2022 would have
  to reach beta* = 0.31 (violent) or 0.68 (property) to lift the 2022 upper bound to the 1993
  lower bound, above the stated 0.25, so the decline survives; for personal theft beta* = 0.19 and
  it does not. Results: research/applications/results/ncvs_victimisation.md.
- Progress 2026-10-12 (the comparable subset, library scan A14): `Research.P1.Comparable`
  (P1Comparable.lean). With the survey covering offence set A and the record covering A and B,
  the rate on A is identified from the survey's V_A (`rate_on_A`); the total rate is at most
  R/(V_A + R_B) (`total_rate_le`) and has no positive lower bound without an assumption on B
  (`total_rate_unbounded_below`); under r_B >= lambda r_A the total count lies in
  [V_A + R_B, V_A + R_B/(lambda r_A)] (`total_interval`) and the dark figure in
  [V_A - R_A, V_A - R_A + R_B(1/(lambda r_A) - 1)] (`dark_interval`), an interval of width
  R_B (1 - lambda r_A)/(lambda r_A) (`dark_figure_width`). R: `morie_dark_figure_comparable()`;
  Python `dark_figure_comparable()`. On the NCVS the identified r_A for 2022 is 0.415 (violent),
  0.318 (property), 0.230 (personal theft); every recorded offence outside the survey's reach
  widens the dark-figure interval by 3.8, 5.3 and 7.7 at lambda = 1/2. The FBI count R_B is not
  held, so the factor is reported, not the interval.
- Progress 2026-10-12 (nonresponse, library scan B10): `Research.P1.Nonresponse`
  (P1Nonresponse.lean). With reporting rate r and observed share p_obs among reporters, the
  proportion lies in [r p_obs, r p_obs + 1 − r], both ends attained (`region_sharp`), of width
  exactly 1 − r (`region_width`); at Mexico's 10.6 percent reporting rate the region is 0.894
  wide. R: `morie_nonresponse_region()`; Python `nonresponse_region()`.
- Progress 2026-10-12 (thinning, scan C9): `Research.P1.Thinning` (P1Thinning.lean). Recording
  each event independently with probability γ turns a Poisson(λ) generating function into the
  Poisson(λγ) one (`thinned_pgf`), the recorded and unrecorded parts factorise (`joint_pgf_factorises`),
  and (λ, γ) is known only through λγ (`not_identified`); a Poisson recorded count does not make the
  true count Poisson (`shifted_not_equidispersed`). R: `morie_thinning_identification()`; Python
  `thinning_identification()`.

## P2. Selection in police-recorded data: the record is the treatment

Every recorded outcome (arrest, charge, use of force) is conditional on a
police decision to be present and to record. Estimands like "racial
disparity in stops" are only defined relative to who could have been
stopped; mobility data changes the denominator. The 2024 arXiv work on new
estimands with mobility data and the earlier "benchmarking" debate show the
field has no agreed estimand, let alone estimator.

- Estimand: risk ratio of an outcome per *exposure* to police, where
  exposure is a latent count.
- Assumptions: exposure proportional to an observed mobility measure up to
  a bounded factor; outcome independent of race given exposure and observed
  covariates (testable in part with the "veil of darkness" design).
- To prove: identification of the disparity ratio under the bounded-factor
  assumption as an interval; that the veil-of-darkness contrast identifies
  the sign under a stated visibility model; consistency of the estimators.
- Cracked means: a disparity estimator that reports the identified interval
  for a given exposure-proxy error bound, with the theorem that no narrower
  interval is possible without more assumptions.
- rmorie now: nothing.
- Progress 2026-09-21: Lean `Research.P2` (research/lean/P2Selection.lean):
  rate interval [y/(γm), γy/m] under exposure within a factor γ of the
  proxy, both ends attained; disparity ratio within γ² of the proxy
  ratio; sign identified when the proxy ratio exceeds γ²; 5 theorems, 0
  sorry. R: `morie_disparity_exposure_bounds`. Next: the veil-of-darkness
  contrast as a formal sign test, and a Toronto stops application with a
  stated mobility proxy.

- Progress 2026-09-21 (benchmarks): `Research.P2` (P2Benchmark.lean):
  `offset_shift` and `disparity_ratio_shift` (a log-link rate model with
  the exposure as offset moves a group coefficient by exactly -log kappa
  when the exposure is scaled by kappa, so disparity ratios are identified
  only up to the ratio of exposure errors: the mechanism behind the 2022
  TPS use-of-force correction), `benchmark_product` (force-per-resident
  disparity = contact-per-resident disparity x force-per-contact
  disparity) and `benchmark_not_additive` (witness 3 x 2 = 6, not 5,
  against the TPS methodological report's "additive"). R:
  `morie_disparity_benchmark()`; test refits a Poisson GLM with a scaled
  offset and recovers -log kappa.

- Progress 2026-09-21 (relative risk): `Research.P2.or_eq_rr_mul`,
  `rr_between`, `or_overstates` (P2RelativeRisk.lean): from arrest-only or
  case-control data the relative risk is squeezed between 1 and the
  identified odds ratio and the rare-outcome substitution always
  overstates it in log magnitude (Manski sec. 6.2; library scan B3). R:
  `morie_relative_risk_from_or()`, point-identifying the risks when a base
  rate is supplied.

- Progress 2026-09-21 (interracial rates): `Research.P2.rate_per_offender_group`,
  `null_slope_positive`, `dyad_ratio`, `pair_exposure_rate_constant`
  (P2Interracial.lean): under random mixing the per-offender-group dyad
  rate is k p_victim, so "percent Black predicts interracial offending"
  and "Black-on-White exceeds White-on-Black per capita" are exposure
  arithmetic; the pair-exposure denominator is the null that a test of
  targeting needs (library scan A20). R: `morie_interracial_rates()`.

- Progress 2026-09-21 (collider): `Research.P2.population_or_one`,
  `collider_or_eq_background`, `collider_or_lt_one` (P2Collider.lean):
  under an "or" arrest mechanism two independent risk factors have odds
  ratio exactly pi (the background arrest prevalence) among arrestees:
  a manufactured negative association (Hernan & Robins Fine Point 8.2;
  library scan B16). R: `morie_collider_arrest()`.
- Progress 2026-10-12 (veil of darkness): `Research.P2.Veil` (P2Veil.lean). In the
  inter-twilight window the same people drive in light and dark, so with latent exposures E_g,
  dark propensities r_g and visible-race factors (1 + δ_g) the ratio of daylight to darkness odds
  against the reference is exactly (1 + δ_g)/(1 + δ_ref) (`odds_ratio_identifies_bias`): the
  relative bias is point-identified, its sign is the sign of the daylight-minus-darkness share
  difference (`share_sign`), a differential shift of who drives by light condition is
  observationally a bias (`exposure_shift_confounds`), and a common scale of both factors is
  invisible (`bias_not_absolute`). R: `morie_veil_of_darkness()`; Python `veil_of_darkness()`.
  Open: the Toronto stops application with a stated mobility proxy.
- Noted 2026-10-12: the public TPS Regulated Interactions table is counts by year and category
  only (no time of day, no group), so the veil-of-darkness application cannot be run from it.
- Progress 2026-10-12 (misclassification, scan B14): `Research.P2.Misclassification`
  (P2Misclassification.lean). Nondifferential misclassification of a binary exposure gives
  RD* = (se + sp − 1) RD π(1 − π)/(d₁d₀) with d₁, d₀ the measured group shares
  (`binary_attenuation`), so the sign is kept whenever se + sp ≥ 1 (`binary_sign`); a
  three-level exposure with a non-monotone map has measured contrast 0 (`ordinal_witness`) or
  the opposite sign (`ordinal_reversal`), so the attenuation theorem is a binary theorem. R:
  `morie_misclassified_rd()`, `morie_misclassified_contrast()`; Python `misclassified_rd()`,
  `misclassified_contrast()`.
- Progress 2026-10-12 (outcome test, scan A24): `Research.P2.OutcomeTest` (P2OutcomeTest.lean).
  In a threshold model two groups with different signal laws and thresholds 0.5 and 0.2 have the
  same hit rate and different stop rates (`infra_marginality`); the same threshold with different
  laws gives different hit rates (`equal_thresholds_unequal_hits`); only under one signal law with
  a strictly increasing conditional mean, E[q | q > t] = (1 + t)/2 for the uniform case
  (`uniform_hit_rate`), do equal hit rates certify equal thresholds
  (`uniform_equal_hits_equal_thresholds`). R: `morie_outcome_test()`; Python `outcome_test()`.

## P3. Causal effects of policing under interference and displacement

"No interference" fails by construction: hot-spot policing displaces or
diffuses crime to neighbours. Point identification under general
interference needs restrictive exposure mappings; the 2026 arXiv review
frames it as an interpretability/estimability trade-off. PanelMatch-style
estimators handle repeated on/off treatment but still assume away
spillover.

- Estimand: direct effect at treated places, spillover effect at places
  within distance d, and the total (net) effect on the whole area.
- Assumptions: an exposure mapping that depends on treatment within radius
  d only (partial interference by neighbourhood), ignorability of the
  exposure given covariates, positivity of every exposure level.
- To prove: identification of direct and spillover effects under the
  radius-d exposure mapping via a generalized adjustment formula; a
  Horvitz-Thompson estimator's unbiasedness; that mis-specifying d by one
  ring biases the direct effect by a computable amount (a bound to report,
  not hide); the net-effect decomposition identity.
- Cracked means: `morie_spillover_effect()` on a street-segment network
  with an exposure mapping the user states, returning direct, spillover
  and net effects with the mis-specification bound, plus a C++ kernel for
  the network exposure counts.
- rmorie now: `mrm_primitives_spatial_spillover.R` (primitives only), the
  DiD family, Kulldorff scan. No exposure-mapping estimator.
- Progress 2026-09-21: Lean `Research.P3` (research/lean/P3Interference.lean):
  finite-population exposure adjustment (E[Y(ℓ)] identified by stratified
  observed means under positivity + exposure ignorability), the
  direct/spillover decomposition, the pooled-mean mixture identity and the
  pooling-bias formula share(1)·(mean(1) − mean(0)); 5 theorems, 0 sorry.
  R: `morie_spillover_exposure` (C++ neighbour-count kernel, three-level
  exposure on a stated ring) and `morie_spillover_effects` (adjusted means,
  spillover, direct, total, pooling bias, positivity check). Same day:
  `Research.P3.Design.ht_unbiased` / `ht_contrast_unbiased` (Horvitz–Thompson
  over a finite randomised design, no ignorability needed) with
  `morie_spillover_ht` and `morie_spillover_exposure_probs` (exact
  enumeration or Monte Carlo of the design); the test checks exact design
  unbiasedness over all 56 assignments of a path graph. Next: ring
  mis-specification bound as a function of the d+1 ring's exposure share;
  variance of the HT estimator; TPS street-segment application.

- Progress 2026-09-21 (SYG): `Research.P3.SYG` (P3SYG.lean): on a
  fixed-size symmetric design the Sen-Yates-Grundy form equals the
  Horvitz-Thompson variance (`syg_eq_ht`) and vanishes for outcomes
  proportional to the inclusion probabilities (`syg_zero_of_const`).
  Exposure levels of a randomised deployment are not fixed-size in
  general, so `morie_spillover_ht_variance(form = "both")` reports the
  condition; the path-graph test shows the two forms differing when it
  fails (library scan B7).
- Progress 2026-10-02 (Cheeger, library scan C14): `Research.P3` in P3Cheeger.lean:
  the two-valued test vector of a hot-spot set S is degree-orthogonal to constants
  (`testVec_orth`), with D-norm 1/vol S + 1/vol S^c (`testVec_dnorm`) and Dirichlet
  form cut(S)(1/vol S + 1/vol S^c)^2 (`testVec_dirichlet`), so its Rayleigh
  quotient is cut(S)(1/vol S + 1/vol S^c) (`rayleigh_testVec`) <= 2 h(S)
  (`rayleigh_le_two_conductance`); with lambda_2 defined variationally,
  lambda_2 <= 2 h(S) for every non-trivial S (`cheeger_easy`). The hard direction
  lambda_2 >= h^2/2 is not proved. R: `morie_cheeger_bound()` (conductance, test
  Rayleigh quotient, lambda_2 of the normalised Laplacian, exhaustive h_G on
  small graphs); the test rebuilds every identity on random weighted graphs.
- Recorded 2026-10-12: the Horvitz–Thompson variance and its unbiased estimator were proved on
  2026-10-02 (P3Variance.lean: `ht_variance`, `ht_variance_estimator_unbiased`, positivity of the
  joint exposure probabilities as the design condition; `morie_spillover_ht_variance()`). Still
  open: the Cheeger hard direction λ₂ ≥ h²/2, the ring mis-specification bound, the TPS
  street-segment application.
- Application 2026-10-12 (research/applications/p03_toronto_neighbourhood_network.R): the public
  files carry Toronto's 158 neighbourhoods, not street segments, so the design computation is run
  on the queen-contiguity graph (475 edges) and labelled as such. The cut around the 33
  Neighbourhood Improvement Areas has conductance 0.516 (98 of the NIAs' 190 edge-ends leave the
  set): a scatter, not a cluster, so a treatment confined to the NIAs puts most of its contiguity
  spillover on untreated neighbours. λ₂ = 0.0157 and the proved chain λ₂ ≤ E(f)/D(f) = 0.645 ≤ 2h(S)
  = 1.03 holds. Under a completely randomised allocation of 33 treated neighbourhoods the rarest
  exposure class (untreated with no treated neighbour) has probability 0.056 at the degree-11
  neighbourhood 098, which is what sets the Horvitz-Thompson variance; outcomes are not used
  because the NIAs were not randomised. Still open: the Cheeger hard direction.
- Applied 2026-10-13, street segments (research/applications/p03_street_segments.R): the City of
  Toronto Centreline (local, collector and arterial roads, joined at shared intersections) inside
  TPS Division 14's 2024 break-and-enter extent, largest connected piece 3,264 segments and 5,650
  adjacencies; 506 of 513 incidents within 150 m of a segment. The 120 repeat segments (two or
  more incidents, 3.7%) hold 64% of the incidents and have conductance 0.972: 97% of a hot
  segment's neighbours are not hot, so displacement or diffusion from a hot-spot treatment lands
  on untreated streets. λ₂ = 0.00042 (long chains of local streets) and λ₂ ≤ E(f)/D(f) = 1.011 ≤
  2h(S) holds. Under a random allocation of 120 treated segments every segment reaches every
  exposure class (smallest: untreated with a treated neighbour, 0.0245).
- Progress 2026-10-12 (ring mis-specification): `Research.P3.Ring` (P3Ring.lean). If the
  outcome also moves by γ_i when unit i's (d+1)-ring is treated, the d-ring estimand misses
  (1/N) Σ_i γ_i 1[outer ring of i exposed] at every assignment (`gap_eq`), zero when every γ_i = 0
  (`gap_zero`), in expectation (1/N) Σ_i γ_i π_i with π_i the outer-ring exposure share
  (`expected_gap_eq`), and at most G · π̄ when |γ_i| ≤ G (`expected_gap_le`). The design computes
  π_i (the exposure probabilities with the larger ring), so the bias of a d-ring analysis is a
  number before any outcome is seen. R: `morie_spillover_ring_bound()`; Python
  `spillover_ring_bound()`. Still open: the Cheeger hard direction and the street-segment data.
- Progress 2026-10-12 (Basu's elephant, scan B8; design effect, scans A10 and B9):
  `Research.P3.Basu` (P3Basu.lean): the Horvitz–Thompson total under independent inclusion has
  mean Σt (`ht_mean`) and variance Σ t²(1 − π)/π (`ht_variance`); one unit with inclusion
  probability t_e²/(2(B + t_e²)) alone exceeds any bound B (`elephant_unbounded`), while the
  other units' contribution is bounded without reference to π_e (`ratio_bounded`).
  `Research.P13.Design` (P13Design.lean): the exchangeable-covariance quadratic form is
  (1 − ρ)Σx² + ρΣ_c(Σ_j x_cj)² (`quad_form`); a cluster-level regressor inflates the naive
  variance by 1 + (M − 1)ρ (`cluster_level`), a within-cluster-balanced one deflates it by 1 − ρ
  (`within_balanced`), and non-negativity at a one-cluster indicator (`quad_indicator`) forces
  ρ ≥ −1/(M − 1) (`icc_lower_bound`). R: `morie_ht_elephant()`,
  `morie_design_effect_direction()`; Python `ht_elephant()`, `design_effect_direction()`.
- Progress 2026-10-12 (blocking, scan A8; adjustment, scan A9): `Research.P3.Blocking`
  (P3Blocking.lean): with k = δ√(n/2)/σ and critical values c_p > c_u, the paired design has more
  approximate power iff ρ > ρ* = 1 − (1 + (c_p − c_u)/k)^{−2} (`paired_better_iff`), and ρ* > 0
  (`crossover_pos`). `Research.P3.Adjustment` (P3Adjustment.lean): under any balanced finite design
  the unadjusted difference in means is unbiased (`unadjusted_unbiased`); in the superpopulation
  asymptotics Lin's interaction estimator is never worse than either (`lin_le_unadjusted`,
  `lin_le_adjusted`), the pooled-slope adjustment never hurts at p = 1/2
  (`adjusted_le_unadjusted_balanced`), and with b₀ = 0 it hurts iff p > 2/3 (`adjusted_worse`).
  R: `morie_blocking_crossover()`, `morie_adjustment_variances()`; Python `blocking_crossover()`,
  `adjustment_variances()`.
- Progress 2026-10-12 (spatial omission, scan A25; exposure and permutation, scan B21; random walk,
  scan C15): `Research.P3.Spatial` (P3Spatial.lean): in the two-unit lag model the lag regressor has
  covariance ρ/(1 − ρ²) ≠ 0 with the error (`lag_regressor_correlated`), least squares stays
  unbiased under correlated errors (`ols_unbiased_correlated_errors`) but GLS is more precise
  (`gls_more_precise`), and m uncorrected local tests reject somewhere with probability
  1 − (1 − α)^m → 1 (`fwer_tendsto_one`). `Research.P3.Exposure` (P3Exposure.lean): the
  all-neighbours-treated probability p^d → 0 and the Horvitz–Thompson weight → ∞
  (`exposure_prob_tendsto_zero`, `ht_weight_unbounded`); the permutation p-value over any finite
  design is exact, #{p ≤ α} ≤ α|A| (`permutation_valid`), which under interference needs an
  imputable statistic. `Research.P3.RandomWalk` (P3RandomWalk.lean): the simple random walk on an
  undirected graph has stationary law d_v/(2|E|) (`stationary`, `stationary_sum_one`), and the
  out-degree law fails on a directed network (`directed_fails`). R: `morie_spatial_omission()`,
  `morie_exposure_positivity()`, `morie_permutation_valid()`, `morie_random_walk_exposure()`;
  Python `spatial_omission()`, `exposure_positivity()`, `permutation_valid()`,
  `random_walk_exposure()`.

## P4. Predictive policing feedback: when does the loop run away?

Ensign et al. prove with a Polya-urn model that a system trained on its
own discovered crime sends police back to the same places regardless of
the true rate, and that public reports mitigate but cannot remove it. The
open problem is the general statement: for a given allocation rule and
discovery model, is the long-run allocation a function of the true rates
or of the initial condition?

- Estimand: the limit of the allocation process, and the bias of the
  observed rate relative to the true rate under a stated policy.
- Assumptions: a discovery model (crimes found ∝ presence × true rate),
  an update rule (allocation ∝ observed rate), an exogenous reporting
  channel with known share.
- To prove: (i) the urn theorem: with reinforcement from discovered crimes
  only, the allocation converges to a random limit independent of true
  rates (runaway); (ii) with the corrected update (discount by presence)
  the allocation converges to the true-rate proportion; (iii) the
  mitigation bound with a reporting share ρ. These are martingale /
  stochastic-approximation results and are provable in Lean with Mathlib's
  probability library, at least in the two-region case first.
- Cracked means: a simulator with the proved limit as its oracle, and a
  diagnostic that, given a department's historical allocation and reports,
  estimates how far its observed rates are from the corrected ones.
- rmorie now: nothing.
- Progress 2026-09-21: Lean `Research.P4` (research/lean/P4Feedback.lean,
  P4Limit.lean): mean-field drift identity, strict monotonicity, runaway
  limit x_n → 1 for λA > λB (harmonic divergence argument), corrected
  update closed form and limit λA/(λA+λB); 7 theorems, 0 sorry, standard
  axioms only. R: `morie_feedback_loop_meanfield/_sim/_limit` with a C++
  urn kernel. Finding: the runaway is logarithmically slow (0.010 → 0.015
  in 2·10^6 steps for a 5% rate gap from a 1/99 start), so the practically
  decisive object is the rate, not the limit. Next: a proved rate bound
  and the stochastic (martingale) version; then the ρ > 0 mitigation bound.
- Progress 2026-09-21 (later): `Research.P4.rho_cap` (P4Mitigation.lean):
  with a reporting share ρ ∈ [0, 1] the naive share never exceeds
  max(x₀, λA/(λA + ρλB)); `cap_zero` and `cap_one` pin the ends (1 and the
  true-rate proportion). Reports mitigate but do not remove the loop, and
  the size of the residual is now an explicit number. R:
  `morie_feedback_loop_limit(rho = ...)$cap`; test checks the cap on the
  full trajectory. The stochastic urn (martingale) version follows below.
- Progress 2026-09-21 (stochastic): `Research.P4.Urn` (P4Urn.lean), on the
  finite path space: `urn_step_martingale` (the share is a martingale for
  any reinforcement), `polya_uniform` (equal rates, one count each: the
  number of A-discoveries after n draws is uniform on 0..n) and
  `polya_no_concentration` (P(1/4 ≤ share ≤ 3/4) ≥ 1/4 for all n ≥ 2). So
  "equal rates, no runaway" holds for the mean field and fails for the
  process: the share converges to a random limit (uniform on (0, 1)) fixed
  by early luck. R: `morie_feedback_loop_urn_law()`. The Azuma drift envelope
  and the a.s. convergence statement landed on 2026-10-12 (below). Still open: the
  unequal-rate urn (a Rubin-type reinforcement theorem; the library has no
  source for it, see LIBRARY-SCAN-2026-09-21.md C1–C3).
- Recorded 2026-10-12: the rate bound x_N − x₀ ≤ ((λA − λB)/4) Σ_{k<N} 1/(c₀ + kλmin) was proved
  on 2026-10-02 (P4Rate.lean: `naiveShare_rate_bound`, `naiveTotal_ge`,
  `not_summable_shifted_harmonic`).
- Progress 2026-10-12 (drift envelope): `Research.P4.Envelope` (P4Envelope.lean). For the
  two-region urn at any starting counts (r₀, b₀) and reinforcement a, on the exact finite path
  space: the share S_n is a martingale (`step_mean`, `mean_const`: E S_n = r₀/(r₀+b₀) for all n),
  one step adds exactly s(1−s)(a/(T+a))² ≤ (a/(T+a))²/4 to the second moment (`step_var`,
  `second_moment_step`), the increments telescope (`step_sum_le`), so Var S_n ≤ a/(4(r₀+b₀))
  for every n (`var_le`) and P(|S_n − S_0| ≥ t) ≤ a/(4(r₀+b₀)t²) (`chebyshev`, `polya_envelope`).
  The envelope is uniform in n: with r₀ + b₀ = 200 the share cannot leave a band of half-width
  0.1 about its start with probability above 1/80, ever. So the "runaway" of the naive loop is,
  for the stochastic urn, a drift of bounded size fixed by the starting mass, not a flight to
  the boundary. R: `morie_feedback_loop_envelope()` (the bound beside the exact law by the urn
  recursion). Still open: the unequal-rate urn limit (a Rubin-type reinforcement theorem).
- Progress 2026-10-12 (almost-sure convergence): `Research.P4.Martingale` (P4Martingale.lean). On
  any probability space with a filtration, a [0, 1]-valued martingale (what P4Envelope's
  `step_mean` makes the urn share) is uniformly integrable (`uniformIntegrable_of_unit`),
  converges almost surely (`share_converges`, Doob's theorem through Mathlib's
  `Submartingale.ae_tendsto_limitProcess`) and in L¹ (`share_converges_L1`), and is at every time
  the conditional expectation of its limit (`share_eq_condExp_limit`), so the limit is a random
  variable with the starting share as its mean (`limit_mean`): the long-run allocation is random,
  not the mean-field fixed point.
- Progress 2026-10-12 (hot-spot threshold, scan item C8): `Research.P4.Hotspot`
  (P4Hotspot.lean). The Short-Brantingham uniform system has exactly one homogeneous steady
  state, B̄ = εDγ/ω and ρ̄ = γ/Ā with Ā = A₀ + B̄ (`steady_iff`). Its mode-k linearisation
  has determinant P k² − Q k + R with P = ηD²/z, Q = D(3εDρ̄ − ω − ηĀ/z) and R = ωĀ > 0
  (`det_eq`, `P_eq`, `Q_eq`, `R_eq`) and a negative trace at every k ≥ 0 (`tr_neg`, from
  εDρ̄ ≤ ω), so a mode grows iff the determinant is negative (`unstable_iff`). Threshold:
  3εDρ̄ ≤ ω + ηĀ/z gives a stable uniform state at every wavelength
  (`no_hotspots_below_threshold`; with ε = 0, `no_hotspots_without_reinforcement`, which
  refutes "hot spots for all parameters"); Q > 0 with Q² > 4PR gives a growing critical mode
  k* = Q/(2P) (`hotspots_above_threshold`). R: `morie_hotspot_threshold()` (steady state,
  P, Q, R, the two sides of the threshold, the critical wavenumber and its growth rate, a
  dispersion table). Still open: the amplitude equation that decides whether the bifurcation
  is subcritical (A16's second half), and the stochastic items above.
- Progress 2026-10-12 (Friedman urn, scan C2): `Research.P4.Friedman` (P4Friedman.lean).
  Under cross-colour reinforcement (B − R)(B + R − 1) is a martingale (`friedman_step`) and
  E[D_{n+1}] = E[D_n](1 − 1/T_n) (`difference_step`, `meanDiff_succ`), so
  E[D_n] = D₀(T₀ − 1)/(T₀ + n − 1) → 0 (`difference_mean_tendsto`) and the mean share tends to
  1/2 (`mean_share_tendsto_half`): same-colour reinforcement (P4Urn, P4Martingale) has a random
  limit, cross-colour reinforcement a deterministic one. R: `morie_friedman_urn()`; Python
  `friedman_urn()`.
- Progress 2026-10-12 (absorption, scan C4): `Research.P4.Ruin` (P4Ruin.lean). With ρ = q/p,
  pρ^{k+2} + qρ^k = ρ^{k+1} (`deMoivre_step`, the de Moivre martingale), and
  p_k = (ρ^k − ρ^N)/(1 − ρ^N) solves the harmonic equation with p₀ = 1, p_N = 0
  (`ruin_harmonic`, `ruin_boundary`) together with the optional-stopping identity
  ρ^k = p_k + (1 − p_k)ρ^N (`optional_stopping_identity`); at p = 1/2 the formula is 0/0 and
  1 − k/N solves the same problem (`ruin_symmetric`), which is why optional stopping needs its
  boundedness condition and not only the martingale property. R: `morie_ruin_probability()`;
  Python `ruin_probability()`.
- Progress 2026-10-12 (attractiveness contraction, scan C7): `Research.P4.Contraction`
  (P4Contraction.lean). With no events the lattice update B' = (1 − ω)[(1 − η)B + ηSB]
  contracts the sup norm by 1 − ω (`sup_contraction`, `decay`), doubly stochastic spreading
  conserves the total so ΣB' = (1 − ω)ΣB (`spreading_conserves`, `update_total`), and the
  uniform feedback mode grows iff ερ > ω/(1 − ω) (`feedback_gain`), the site form of
  P4Hotspot's threshold. R: `morie_attractiveness_contraction()`; Python
  `attractiveness_contraction()`.
- Progress 2026-10-12 (Bellman contraction, scan C21): `Research.P4.Bellman` (P4Bellman.lean).
  The Bellman optimality operator on a finite MDP satisfies ‖Tv − Tw‖_∞ ≤ γ‖v − w‖_∞
  (`bellman_lipschitz`), is a contraction (`bellman_contracting`) with a unique fixed point
  (`optimal_value_exists_unique`); in the three-state example the greedy action (1 now, 1/2 after)
  loses to the patient one (0 now, 1 after) iff γ > 2/3 (`myopic_suboptimal`). R:
  `morie_bellman_value()`, `morie_myopic_example()`; Python `bellman_value()`, `myopic_example()`.

## P5. Recidivism prediction under label bias: what can be certified?

Rearrest is the proxy for reoffending; over-policing makes the proxy's
error rate group-dependent. The impossibility results (Chouldechova;
Kleinberg, Mullainathan, Raghavan) show calibration and equal error rates
cannot hold together when base rates differ. But with *label bias* the base
rates themselves are unobserved, so even calibration is not verifiable on
the data. What is identifiable about a tool's fairness when labels are
biased is open.

- Estimand: group-wise false positive/negative rates with respect to the
  *true* outcome.
- Assumptions: group-specific label noise rates bounded in known
  intervals; noise independent of features given group and true outcome.
- To prove: (i) the impossibility theorem itself, formally (a short Lean
  proof, useful as a certified statement); (ii) partial identification of
  true-outcome error rates under bounded label noise, with sharp bounds;
  (iii) which fairness comparisons remain decidable under those bounds.
- Cracked means: `morie_fairness_bounds()` returning identified intervals
  for each metric given noise bounds, plus a proof that the intervals are
  sharp, applied to COMPAS-style data.
- rmorie now: no fairness module.
- Progress 2026-09-21: Lean `Research.P5` (research/lean/P5Fairness.lean):
  Chouldechova's identity on a confusion table, the impossibility theorem
  as its corollary, the recorded-rate model p_obs = p(1-β) + (1-p)α, the
  sharp interval for the true base rate under noise boxes [0,ᾱ]×[0,β̄]
  with both ends attained, and exact inversion for known noise; 6
  theorems, 0 sorry. R: `morie_fairness_rates/_implied_fpr/
  _base_rate_bounds/_true_rate`. (iii) done the same day: `Research.P5.compare_decided`
  (disjoint intervals order the true rates for every admissible noise) and
  `compare_undecided` (overlapping intervals admit either order), with
  `morie_fairness_compare_groups` reporting the breakdown β̄. Next: bounds
  on group-wise FPR/FNR (not only base rates) under the same noise boxes.

- Progress 2026-09-21 (rescaling): `Research.P5.rescale_*`,
  `reduced_coefficient`, `ratio_is_rescaling` (P5Rescale.lean): dropping an
  orthogonal covariate with latent variance v rescales every identified
  logit coefficient by sqrt((pi^2/3)/(pi^2/3+v)) with no confounding, so
  odds ratios of a risk score are not comparable across covariate sets
  without a variance normalisation (Karlson-Holm-Breen; library scan A5).
  R: `morie_logit_rescale()`; the test recovers the factor exactly from two
  fitted probits on 200,000 simulated cases (normal + normal is normal) and
  to within a few percent for logits, where the mixture is not logistic and
  the factor is KHB's approximation.

- Progress 2026-09-21 (ranking): `Research.P5.rank_reversal_exists`,
  `rank_stable_of_gap`, `identified_scores_le` (P5Ranking.lean): a ranking
  by noisy scores has resolution exactly twice the noise half-width, so
  the Baldus proportionality-review ordering (intervals covering most of
  [0,1]) identifies no pair (library scan A3). R:
  `morie_ranking_resolution()`.

- Progress 2026-09-21 (hazard ratios): `Research.P5.survivor_hazard_mono`,
  `hr2_gt_one_of_depletion`, `hr2_witness` (P5Hazard.lean): with two risk
  types and no period-2 effect, the arm that depletes the high-risk type
  less in period 1 shows a period-2 hazard ratio above one (Hernan &
  Robins Fine Point 17.2; library scan B15). R: `morie_hazard_selection()`.
- Progress 2026-10-02 (separation, library scan A1): `Research.P5` in P5Separation.lean:
  with labels as signs the logistic log-likelihood is a sum of strictly
  increasing, negative terms (`ll1_strictMono`, `ll1_neg`); a completely
  separating direction d makes loglik(b + t d) > loglik(b) for every b and t > 0
  (`loglik_lt_shift`), quasi-complete separation likewise
  (`loglik_lt_shift_quasi`), so no maximum-likelihood estimate exists
  (`no_mle`, `no_mle_quasi`); the likelihood is bounded by 0 (`loglik_neg`) and
  tends to 0 along d (`loglik_tendsto_zero`). The Baldus proportionality-review
  logit that "would not converge" (Weisburd & Britt ch. 1) is this geometry, not
  a specification problem. Albert-Anderson's converse (overlap => a finite MLE)
  is not proved. R: `morie_logit_separation()` (exact linear programme through
  lpSolve, glm heuristic otherwise; reports the direction, the margins and the
  log-likelihood climbing along the direction).
- Progress 2026-10-12 (Albert–Anderson converse): `Research.P5` in P5Overlap.lean. Overlap
  (every direction d ≠ 0 gives some observation a strictly negative margin) implies a finite
  maximum-likelihood estimate (`mle_exists`). The smallest margin is continuous
  (`minMargin_continuous`), negative on the unit sphere, hence at most −δ there for some δ > 0
  by compactness (`sphere_margin_neg`); the log-likelihood is at most ll1 of the smallest margin
  (`loglik_le_ll1_min`), so at most ll1(−δ‖b‖) ≤ −δ‖b‖ (`loglik_le_of_norm`, `ll1_le_neg`) and
  falls below any level far from the origin (`loglik_small_far`); on the remaining closed ball
  the continuous log-likelihood (`loglik_continuous`) attains its maximum. With P5Separation
  the characterisation is complete: a maximiser exists iff the data are not (quasi-)completely
  separated. The failure of overlap splits into separation proper and a rank-deficient design
  (a direction with every margin zero), which the R check reports separately.
  R: `morie_logit_separation()$mle_exists`.
- Progress 2026-10-12 (attenuation, scan A4): `Research.P5.Attenuation` (P5Attenuation.lean).
  Classical error on one regressor attenuates its slope by s₁₁/(s₁₁ + s_u²)
  (`single_attenuation`); with a second, error-free regressor the population coefficients are
  b₁ = β₁(s₁₁s₂₂ − s₁₂²)/D*, b₂ = β₂ + β₁s_u²s₁₂/D* (`normal_equations`,
  `two_regressor_coeffs`), so the error-free coefficient is biased unless β₁, s_u² or s₁₂ is
  zero (`error_free_biased_iff`) and the bias has the sign of β₁s₁₂ (`bias_sign`): "any
  measurement error attenuates" is a one-regressor fact. R: `morie_attenuation_two_regressor()`;
  Python `attenuation_two_regressor()`.
- Progress 2026-10-12 (Γ bounds, scan A7; design sensitivity, scan B17): `Research.P5.Gamma`
  (P5GammaBound.lean): the worst-case mean nΓ/(1 + Γ) is non-decreasing in Γ (`worst_mean_mono`),
  Γ* = T/(n − T) is where it reaches T (`worst_mean_at_gamma_star`, `gamma_star_kills`,
  `below_gamma_star`) and depends on the table alone (`gamma_star_table_only`).
  `Research.P5.DesignSensitivity` (P5DesignSensitivity.lean): along T_n/n → μ, c_n/n → μ_Γ the test
  rejects for all large n iff μ > μ_Γ (`eventually_reject`, `eventually_accept`), and the design
  sensitivity is the unique root of μ_Γ = μ (`design_sensitivity_exists_unique`). R:
  `morie_gamma_bound()`, `morie_design_sensitivity()`; Python `gamma_bound()`,
  `design_sensitivity()`.
- Progress 2026-10-12 (data processing and calibration, scan C20; budget rule, scan C22):
  `Research.P5.DataProcessing` (P5DataProcessing.lean): every predictor from a post-processed score
  is a predictor from the record, so the best squared-error risk from the record is at most that
  from the score (`bayes_risk_data_processing`); with symmetric flip rates, calibration to the
  record in both groups forces true rates (r − f_g)/(1 − 2f_g) that differ
  (`calibration_incompatible`), and calibration to the truth forces recorded rates that differ
  (`calibration_to_W_incompatible`). `Research.P5.BudgetRule` (P5BudgetRule.lean): a rule that
  treats above a level, not below, and spends the budget maximises the total effect over all
  fractional rules (`threshold_optimal`); at a tied cell no deterministic rule attains it
  (`stochastic_needed`). R: `morie_calibration_flip()`, `morie_bayes_risk_dpi()`,
  `morie_budget_rule()`; Python `calibration_flip()`, `bayes_risk_dpi()`, `budget_rule()`.

## P6. The age-crime curve: invariant law or mixture artefact?

Hirschi and Gottfredson's invariance claim is contradicted by cross-national
peaks past 30, US arrest data with a 2024 peak age of 32, and cohort
trajectory studies with a small persistent group. The open methodological
question is whether an aggregate curve can ever discriminate "everyone
follows the same curve" from "a mixture of a few types", and what data
would.

- Estimand: the mixture structure of individual offending trajectories
  (number of components, their shapes) behind an aggregate curve.
- Assumptions: trajectories are Poisson counts with rates from a finite
  mixture of smooth age functions.
- To prove: non-identifiability of the mixture from the aggregate curve
  alone (a clean counterexample theorem), identifiability given panel data
  under a stated separation condition, and the aggregate-curve invariance
  statement as a testable hypothesis with a proved test statistic
  distribution.
- Cracked means: a test of invariance that says exactly what data can and
  cannot decide it, with the counterexample as part of the documentation.
- rmorie now: group-based trajectory tooling is absent.
- Progress 2026-09-21: Lean `Research.P6` (research/lean/P6AgeCrime.lean):
  every mixture's aggregate curve is a one-type curve (non-identification
  from the aggregate), invariance of types is sufficient for aggregate
  invariance and explicitly not necessary (two-type witness); 4 theorems,
  0 sorry. R: `morie_age_crime_aggregate` returning the equivalent
  one-type curve. Next: panel identifiability under a separation
  condition (finite mixture of Poisson trajectories) and the invariance
  test with a proved null.
- Progress 2026-10-12 (panel): `Research.P6.Panel` (P6Panel.lean). With two ages observed on
  the same individuals and counts conditionally independent given type, the cross-age
  covariance is the covariance of the type rates, Σ_k w_k (λ_k(a) − m_a)(λ_k(b) − m_b)
  (`cross_moment_eq`), zero for one type (`one_type_cov_zero`: the null of the invariance test,
  exactly), w₁w₂(λ₁(a) − λ₂(a))(λ₁(b) − λ₂(b)) for two (`two_type_cov`); the P6AgeCrime witness
  has the one-type aggregate and covariance −1 (`panel_separates_witness`), which the aggregate
  cannot see (`aggregate_blind`). The separation condition is that the types differ at both ages.
  R: `morie_age_crime_type_cov()` and `morie_age_crime_panel_cov()` (sample covariance with an
  exact one-type permutation null); Python `age_crime_type_cov()`, `age_crime_panel_cov()`.
  Open: the full identifiability of K types from T periods (Teicher-type), a cohort application.
- Noted 2026-10-12: no cohort file is held; OTIS gives age categories of people in segregation,
  not offending by age, so the cohort application stays open.

## P7. The law of crime concentration: how much is chance?

Weisburd's law says a narrow band of street segments carries most crime;
the JQC editors list four open questions, including how much concentration
random placement would produce and how much the "law" depends on
crime-free places. Gini-style measures are known to be inflated when
counts are sparse relative to units.

- Estimand: concentration in excess of the null (crimes placed
  independently with unit-level rates), and its stability over time.
- Assumptions: a null of Poisson placement with a known offset (e.g., unit
  length or population); a persistence model for ranks.
- To prove: the expected Gini/Lorenz concentration under the Poisson null
  as a function of the total count and number of units (closed form or
  tight bounds); that the corrected statistic is zero in expectation
  under the null; the rank-persistence statistic's null distribution.
- Cracked means: `morie_concentration_excess()` with the proved null
  correction, so a "law" can be tested rather than eyeballed.
- rmorie now: no concentration module.
- Progress 2026-09-21: Lean `Research.P7` (research/lean/P7Concentration.lean):
  the exact identity G(all) = z + (1−z)·G(positive places) for the Gini of
  any count vector, and P(Poisson(μ)=0) = e^{−μ}; 2 theorems, 0 sorry. R:
  `morie_concentration_gini`, `morie_concentration_decompose` (zero share,
  positive-place Gini, Poisson-null zero share e^{−μ}, excess zero share).
  This answers JQC question (4) directly: how much a "law" rests on
  crime-free places is a mechanical function of the mean count. Next:
  the expected Gini under the Poisson null (or a proved bound), the
  rank-persistence null, and a Toronto street-segment application.

- Progress 2026-09-21 (mixture): `Research.P7.Mixture` (P7Mixture.lean): for
  any finite Poisson mixture the zero share is at least exp(-mean)
  (`mixture_zero_ge_exp_neg_mean`, Jensen), the variance is at least the
  mean with excess equal to the variance of the intensity
  (`variance_eq`, `mixture_var_ge_mean`), and equality holds iff the
  intensity is constant on the support (`mixture_var_eq_mean_iff`). So the
  Poisson null zero share of `poisson_zero_prob` is a lower bound for every
  heterogeneous population with the same mean, and a dispersion index
  above one is implied by any heterogeneity: neither excess zeros nor a
  Gini above the null separates "criminology of place" from unequal
  exposure. R: `morie_concentration_dispersion()`.

- Progress 2026-09-21 (distinct places): `Research.P7` (P7Distinct.lean):
  `log_le_S`, `S_le_log`, `expectedDistinct_bounds` — under Polya
  allocation with concentration M the expected number of distinct places
  after n events lies between M log((M+n)/M) and 1 + M log((M+n-1)/M):
  logarithmic growth, so polynomial growth of distinct addresses rejects a
  concentration-only mechanism (library scan C13). R:
  `morie_concentration_distinct_growth()` with a log-log slope diagnostic.
- Progress 2026-10-12 (null bound): `Research.P7.Bound` (P7Bound.lean). For any count vector
  the Gini index is at most √(v/2)/x̄, the coefficient of variation over √2 (`gini_le_cv`, by
  Cauchy–Schwarz over the n² pairs and `pair_sq_identity` ΣΣ(x_i − x_j)² = 2nΣ(x_i − x̄)²), so
  under a Poisson null (variance = mean) the Gini is at most about 1/√(2x̄): the amount of
  concentration chance can show is a closed-form function of the mean count. R:
  `morie_concentration_null_bound()` (the bound plus the simulated Poisson-null distribution and a
  p-value); Python `concentration_null_bound()`. Open: the exact expected null Gini, the
  rank-persistence null, the Toronto application.
- Application 2026-10-12 (research/applications/p07_concentration_toronto.R): TPS neighbourhood
  counts, 158 neighbourhoods, 2014-2025. In 2025 every offence with more than a handful of
  events has a Gini far above the equal-rate Poisson null (assault 0.39 against a null mean of
  0.045; break and enter 0.34 against 0.09; bike theft 0.63 against 0.15; p = 0 in 2,000 draws).
  Homicide (45 events, mean 1.2 per neighbourhood) has Gini 0.14 below its null mean of 0.48,
  and shootings (262 events) Gini 0.38 against 0.34 with p = 0.056: at those means the null
  bound 1/√(2x̄) is 0.64 and 0.45, so the "concentration" of rare offences is the arithmetic of
  rare counts, as P7's bound says. Break-and-enter concentration rose from 0.31 (2014) to 0.40
  (2021) and fell back to 0.34. The null is equal-rate; populations run from 10,000 to 44,636, so
  a population-proportional null (and a bound for it) is the next step.
- Progress 2026-10-12 (rank persistence): `Research.P7.Persistence` (P7Persistence.lean). Under
  the null that this year's top-k set is a uniformly random k-subset of the N places with no
  memory of last year's, the k-sets meeting the old top-k in exactly j places number
  C(k, j) C(N−k, k−j) (`card_overlapSets`, by the bijection B ↦ (B ∩ A, B \ A)), so
  P(overlap = j) = C(k, j) C(N−k, k−j) / C(N, k) (`overlap_prob`) and the law sums to one
  (`overlap_prob_sum`); its mean is k²/N. R: `morie_rank_persistence_null()`; Python
  `rank_persistence_null()`. Applied to Toronto break and enter (top 16 of 158, consecutive years
  2014-2025): the observed overlaps are far above the null mean of 1.6, with upper-tail
  probabilities below 1e-6 in every pair of years, so the persistence of hot neighbourhoods is
  the part of concentration that chance does not produce. Still open: the exact expected Gini of
  the finite Poisson sample (the ratio's bias) and the population-proportional null.
- Progress 2026-10-12 (victims, with the NCVS): victimisations per person-year (violent and
  personal theft) and per household-year (property), zeros from the person and household files,
  through `morie_concentration_decompose()` (research/applications/ncvs_victimisation.R). 2022:
  185,616 persons, mean 0.016, zero share 0.991 against a Poisson 0.984, Gini among the
  victimised 0.36, dispersion index 4.4; 152,794 households, mean 0.051, zero share 0.962 against
  0.950, Gini among the victimised 0.23, dispersion 2.2; 1994 the same shape at twice the mean.
  The excess zeros and the dispersion are what the Poisson null cannot produce; the Gini among
  the victimised is the part of concentration that is not the zeros.
- Progress 2026-10-12 (Cox overdispersion, scan C12; Vuong boundary, scan B24): `Research.P7.Cox`
  (P7Cox.lean). For a finite mixture of Poissons Var N = E N + Var Λ (`total_variance`), so
  Var N ≥ E N (`variance_ge_mean`) with equality iff the intensity is constant on the support
  (`equality_iff_constant`): a Gini above the Poisson null is implied by any random intensity,
  which is why `morie_concentration_decompose()` separates mixing from contagion. At π = 0 the
  zero-inflated pmf is the Poisson pmf (`zip_zero_eq_poisson`), every pointwise log-likelihood
  ratio is 0 (`llr_zero`) and Vuong's statistic is 0/0, not N(0, 1). R:
  `morie_cox_overdispersion()`, `morie_zip_vuong_boundary()`; Python `cox_overdispersion()`,
  `zip_vuong_boundary()`.
- Progress 2026-10-12 (zero inflation, scan A6; Moran trend, scan B19; Chernoff, scan C11; tail
  shape, scan C18): `Research.P7.ZeroInflation` (P7ZeroInflation.lean): the ZIP zero share lies
  strictly in (e^{−μ}, 1) (`zip_zero_gt_poisson`, `zip_zero_lt_one`), the NB zero share runs
  continuously from 1 to e^{−μ} (`nb_zero_tendsto_zero`, `nb_zero_tendsto_atTop`), so an NB with the
  same mean and zero share exists (`nb_matches_zip`). `Research.P7.MoranTrend` (P7MoranTrend.lean):
  a noiseless linear trend on the four-node path gives I = 1/3, not −1/3 (`trend_witness`), and for
  residuals of a symmetric idempotent M the summed numerator and denominator are c·tr(WM) and
  c·tr(M) (`residual_moran_ratio`). `Research.P7.Chernoff` (P7Chernoff.lean):
  P(S ≥ (1 + ε)λ) ≤ exp(−λ[(1 + ε)log(1 + ε) − ε]) from Markov on the generating function
  (`poisson_chernoff`), rate positive (`rate_pos`). `Research.P7.TailShape` (P7TailShape.lean): the
  tail ratio S(2k)/S(k) is q^k → 0 for a geometric tail, 2^{−α} for a power tail, and the NB pmf
  ratio is at most (2k + r)^{r−1}q^k → 0 (`nb_pmf_ratio_le`, `nb_pmf_ratio_tendsto_zero`): the
  Gamma-mixed Poisson is light-tailed however heterogeneous the places. R:
  `morie_nb_matches_zip()`, `morie_moran_trend()`, `morie_poisson_chernoff()`,
  `morie_tail_ratio()`; Python `nb_matches_zip()`, `moran_trend()`, `poisson_chernoff()`,
  `tail_ratio()`.

## P8. Deterrence: certainty, severity and celerity cannot be varied alone

Every punishment has all three dimensions; studies manipulate one and
attribute the effect to it. Nagin's reviews and the 2023 England and Wales
police-force analysis note the three have never been separated in one
model. Identification is the unsolved part.

- Estimand: partial effects of certainty p, severity s and celerity c on
  offending, holding the others fixed.
- Assumptions: a structural response model with a stated functional form
  (e.g., expected-cost with discounting), variation in (p, s, c) that is
  not collinear, instruments for at least one dimension.
- To prove: conditions under which the three partial effects are jointly
  identified (a rank condition on the design), and that with only two
  varying, the third is not identified (a formal non-identification
  result), so that empirical claims can be checked against the design.
- Cracked means: a design-diagnostic that takes a dataset's (p, s, c)
  variation and reports which partial effects are identified, with the
  theorem behind the verdict.
- rmorie now: nothing.
- Progress 2026-09-21: Lean `Research.P8` (research/lean/P8Deterrence.lean):
  a sanction dimension held fixed in the design has an unidentified
  partial effect (explicit second coefficient vector with identical fits),
  identification ⇔ injectivity of the coefficient-to-fit map, and a
  corner design that identifies all three; 3 theorems, 0 sorry. R:
  `morie_deterrence_design_check` (rank of (1,p,s,c) and per-dimension
  identification). Next: the nonlinear expected-cost response with
  discounting, and the instrument condition for one dimension.
- Progress 2026-10-12 (expected cost, instrument): `Research.P8.Instrument` (P8Instrument.lean).
  log(p s e^{−ρc}) = log p + log s − ρc (`logcost_linear`), so the expected-cost response with a
  common elasticity is linear in the log design and the celerity-to-certainty coefficient ratio
  is −ρ (`ratio_is_discount`); identification is P8Deterrence's rank condition on the transformed
  design. With y = α + βp + u and an instrument z, cov(z, u) = 0 and cov(z, p) ≠ 0 give
  β = cov(z, y)/cov(z, p) on the finite population (`wald_identifies`), and without relevance
  every slope yields the same instrument covariance (`wald_undefined`). R:
  `morie_deterrence_logcost()`, `morie_wald_iv()` (the test matches two-stage least squares);
  Python `deterrence_logcost()`, `wald_iv()`.
- Progress 2026-10-12 (instrumental inequality, scan B12; inspection game, scans A18 and C24):
  `Research.P8.IVInequality` (P8IVInequality.lean): for finite Z, X, Y with a latent confounder
  independent of Z, Σ_y max_z P(y, x | z) ≤ 1 for every x (`instrumental_inequality`), so a
  table that violates it refutes the instrument. `Research.P8.Inspection` (P8Inspection.lean):
  the rock–paper–scissors replicator field has xᵀAx = 0 (`payoff_zero_sum`), keeps the simplex
  (`simplex_invariant`), has the centroid as its only interior rest point
  (`interior_fixed_point`, `only_interior_rest`) and conserves V = x₁x₂x₃ (`conserved`): the
  interior rest point is a centre, orbits cycle, and more inspection does not monotonically
  reduce crime. R: `morie_instrumental_inequality()`, `morie_inspection_game()`; Python
  `instrumental_inequality()`, `inspection_game()`.
- Progress 2026-10-12 (budget optimum, scan A19; natural bounds, scan B11): `Research.P8.Budget`
  (P8Budget.lean): g(θ) + d(C − θ) is concave for concave g, d (`concave_sum`), attains a maximum
  (`exists_max`), interior whenever some interior θ beats both corners (`interior_optimum`), and
  a bounded deterrence response loses to a rehabilitation gain above its range
  (`rehab_beats_bound`). `Research.P8.NaturalBounds` (P8NaturalBounds.lean): the IV natural
  bounds contain the effect (`ate_mem`), are attained (`bounds_attained`), have width
  P(A = 0 | Z = 1) + P(A = 1 | Z = 0) (`width`), zero under perfect compliance and one at fifty
  percent noncompliance (`perfect_compliance_point`, `half_noncompliance_width`); the Balke–Pearl
  linear-programming bounds are not formalised. R: `morie_budget_optimum()`,
  `morie_iv_natural_bounds()`; Python `budget_optimum()`, `iv_natural_bounds()`.

## Order of attack

P4 first (the mathematics is self-contained, the theorem is concrete, and
Mathlib has the probability tools), then P1 and P5 (both partial
identification, sharing one Lean library of bounds), then P3 (largest
implementation, C++ network kernels), then P7, P6, P2, P8.

## Sources

- Partial identification of the dark figure with survey data under
  misreporting: https://link.springer.com/article/10.1007/s10940-024-09593-4
- Estimating dark figures of crime (Bellert, Günster, AEA 2026):
  https://www.aeaweb.org/conference/2026/program/paper/FntaGdGr
- Dark figure small-area estimation (BJC): https://academic.oup.com/bjc/article/61/2/364/5924614
- Causal inference and racial bias in policing, new estimands with mobility
  data: https://arxiv.org/html/2409.08059v1
- Causal inference under interference, 2026 review: https://arxiv.org/pdf/2607.20156
- PanelMatch for policing with irregular treatment (JQC 2026):
  https://link.springer.com/article/10.1007/s10940-026-09678-2
- Is crime displacement inevitable (Fortaleza): https://arxiv.org/html/2503.13571v1
- Runaway feedback loops in predictive policing (Ensign et al.):
  https://proceedings.mlr.press/v81/ensign18a.html
- Fair prediction with disparate impact (Chouldechova): https://arxiv.org/pdf/1610.07524
- Fairness evaluation with biased noisy labels: https://arxiv.org/pdf/2003.13808
- The measure and mismeasure of fairness: https://arxiv.org/pdf/1808.00023
- International and historical variation in the age-crime curve (Annual
  Review of Criminology): https://www.annualreviews.org/content/journals/10.1146/annurev-criminol-111523-122451
- The age-crime curve is dead, long live the age-crime curve (2024 FBI data):
  https://www.tandfonline.com/doi/full/10.1080/0735648X.2025.2571411
- Stockholm birth cohort trajectories: https://www.sciencedirect.com/science/article/pii/S0047235224000047
- Law of crime concentration, editors' introduction (JQC):
  https://link.springer.com/article/10.1007/s10940-017-9342-0
- Law of crime concentration for most crime, multi-city (2026):
  https://link.springer.com/article/10.1007/s12103-026-09917-z
- Permanent and transitory crime risk in hot spot analysis: https://arxiv.org/pdf/2512.07467
- Deterrence in the twenty-first century (Nagin): https://www.journals.uchicago.edu/doi/abs/10.1086/670398
- Classical deterrence theory revisited, England and Wales:
  https://journals.sagepub.com/doi/10.1177/14773708211072415
- Challenges and prospects for evidence-informed policy (Annual Review of
  Criminology): https://poodle-banjo-jhsp.squarespace.com/s/blomberg-et-al-2024-challenges-and-prospects-for-evidence-informed-policy-in-criminology.pdf


## Library scan (2026-09-21)

Three readers went through the criminology, statistics and mathematics shelves of the WD library and the full-text corpus index; 74 candidate problems with page citations, and a ten-step priority queue, are in research/LIBRARY-SCAN-2026-09-21.md. First in the queue: the stochastic urn for P4 (martingale share, Friedman contrast, Azuma drift envelope), then the Jensen sharpening of the P7 Poisson null and Cox overdispersion, then the Petersen/Chapman floor and k-list non-identification for P1.


- Progress 2026-09-21 (comparative statics): `Research.P8.certainty_monotone`,
  `severity_monotone`, `aggregate_monotone` (P8Comparative.lean): the
  direction of deterrence follows from decreasing differences alone (any
  benefit shape, any menu, sanction strictly increasing); convexity buys
  uniqueness and smoothness, not the sign (library scan C19 corrected).
  R: `morie_deterrence_response()`.

- Progress 2026-09-21 (necessity): `Research.P8.necessity_bounds`,
  `necessity_of_monotone`, `necessity_of_disjoint` (P8Necessity.lean):
  the share for whom the sanction was necessary lies in
  [max(0, a-b), min(a, 1-b)] with both ends attained and equals a-b under
  monotonicity, giving Pearl's PN bounds and the excess-risk-ratio
  identity (library scan B13). R: `morie_probability_of_necessity()`.

Scan items closed by problem (2026-10-12; status table in LIBRARY-SCAN-2026-09-21.md): P1: B5, B6,
B10, A14, C9; P2: A21, A22, A23, B14, A24; P3: B7, C16, B8, A10, B9, A8, A9, A25, B21, C15; P4: C1,
C2, C3, C4, C7, C8, C21; P5: A1, A2, A4, A5, A7, B17, C20, C22; P7: B23, C12, B24, A6, B19, C11, C18;
P8: B12, A18, C24, A19, B11; P9: A12, A15; P10: C5, C6, A17, B20, C17, C23; P11: B1, B2, B4, B18, B25.
Every scan item now has a formalised core; the parts left open inside an item are named in its
bullet (Balke–Pearl sharp bounds in B11, Lorden's constant in C23, the Gaussian AR(1) extremal
index in C17, Wager's variance-bias formula in B21).

## P9. Crime recording as a linear map: what counting rules can hide

Recorded counts are a linear image r = M c of true counts, with M_ij the
share of true category-j offences recorded as category i (Eterno, Verma &
Silverman 2012, ch. 14-15: NYPD downgrading; UK "cuffing"). What is
invariant under re-classification, what cuffing destroys, and why an
aggregate clearance rate is not a performance measure.

- Progress 2026-09-21: `Research.P9` (P9Recording.lean):
  `total_invariant_of_colStochastic` (pure re-classification leaves the
  total unchanged), `total_le_of_colSubstochastic` (cuffing lowers it by
  exactly the dropped mass), `reclassification_moves_ratio` (a share q of
  category j moved into i keeps the total and moves the ratio r_i/r_j from
  c_i/c_j to c_i/c_j + q/(1-q)(1 + c_i/c_j)), `detection_rate_rises`
  (moving a share q of a class with detection rate d into a disposal
  detected with probability one raises the aggregate rate by q n (1-d)/N).
  R: `morie_recording_map()`, `morie_detection_rate_shift()`.
- Progress 2026-10-11 (two regimes): `Research.P9.regimes_not_identified`,
  `second_source_separates` (P9Recording.lean): any non-negative recorded series is
  produced both by the identity map on true counts equal to the series (pure
  re-classification) and by the map (1-eps) I on true counts r/(1-eps) (cuffing a share
  eps), whose true total is larger, so one series cannot say which regime produced it;
  with a second source fixing the true total, re-classification reproduces it exactly and
  cuffing a positive share of a category with positive count records strictly less. R:
  `morie_recording_regimes()`; Python `recording_regimes()`.
- Open: application to TPS MCI 2014-2024 robbery/theft and assault ratios with CSUS
  victimisation as the second source (library scan A12, A13, A15).


## P10. Near-repeat contagion: what a branching ratio commits you to

- Progress 2026-10-12 (the moments, with the NCVS): the person-year and household-year counts of
  research/applications/ncvs_victimisation.R have dispersion indices 3.7 to 4.7 (persons) and
  2.2 to 2.5 (households) with zero shares above the Poisson's: the two moments `Research.P10.Moments`
  shows contagion (a branching process) and heterogeneity (a mixed Poisson) fit alike, so the
  repeat victimisation in the NCVS is not, from these moments, evidence of contagion.

Self-exciting (Hawkes) models of near-repeat victimisation (Mohler et al.;
D'Orsogna & Perc review) report a branching ratio n. The offspring of one
background event form a Galton-Watson tree, so n fixes the expected
cluster size, the stationary rate and the share of "contagious" events.

- Progress 2026-09-21: `Research.P10` (P10Contagion.lean):
  `generation_mean`, `cluster_size_of_lt_one` (1/(1-n)),
  `stationary_rate` (mu/(1-n)), `cluster_size_diverges_of_ge_one`,
  `endogeneity_share` (= n). R: `morie_contagion_branching()`; the test
  simulates Poisson-offspring trees and recovers 1/(1-n).
- Open: (extinction done 2026-10-02, below)
  (needs convexity of the PGF; GS Theorem 5.4.5), the Bartlett
  non-identification of contagion vs heterogeneity from the K-function
  (library scan B20), and an application to TPS break-and-enter with
  constant vs KDE background (A17).
- Progress 2026-10-02 (extinction): `Research.P10` in P10Extinction.lean: the generating
  function of a finite offspring law is monotone on [0,1] with f(1) = 1 (`pgf_mono`,
  `pgf_one`); the iterates from 0 are monotone, bounded by every fixed point, and converge
  to a fixed point that lies below every fixed point in [0,1] (`iter_mono`,
  `iter_le_fixed`, `iter_tendsto`, `extinction_fixed`, `extinction_le_fixed`): the
  extinction probability. 1 - f(s) <= m(1-s) gives certain extinction when the mean
  offspring m < 1 (`one_sub_pgf_le`, `subcritical_extinction_one`); 1 - f(s) >= (1-s) f'(s)
  and continuity of f' give a fixed point below one when m > 1
  (`one_sub_pgf_ge`, `supercritical_extinction_lt_one`). R: `morie_contagion_extinction()`;
  Python `contagion_extinction()`.
- Progress 2026-10-12 (Bartlett, second moments): `Research.P10.Moments` (P10Moments.lean).
  Two count laws on {0,1,2,3}, p = (1/2, 0, 1/2, 0) and q = (1/3, 1/2, 0, 1/6), have the same
  mean and variance (`witness_same_mean`, `witness_same_second_moment`), dispersion index one in
  both (`witness_dispersion_one`), and differ at zero (`witness_distinct`); hence no function of
  the first two moments — a dispersion index, a K-function's cell summary — separates them
  (`second_moments_do_not_identify`). This is the finite core of Bartlett's result that contagion
  and heterogeneity share second moments. R: `morie_contagion_moment_witness()`; Python
  `contagion_moment_witness()`. Open: the K-function statement on a spatial process, the TPS
  break-and-enter application.
- Application 2026-10-12 (research/applications/p10_near_repeat_bne.R): TPS break and enter,
  76,466 occurrences in 158 neighbourhoods over 574 weeks (2014-2024). Every neighbourhood's
  weekly series is over-dispersed (median var/mean 1.33, a gamma mixture of size 2.3 matches the
  same two moments), which by `second_moments_do_not_identify` a self-exciting process and a
  heterogeneous Poisson reproduce alike. Hawkes fits of the five busiest neighbourhoods' full
  series give branching ratios 0.24-0.79 with decay half-lives of 1.3 to 18 days: the temporal
  order, not the moments, is what identifies them, and a background that moves with the
  seasons would be read as self-excitation. Open: the constant-versus-KDE background comparison
  (A17) and the K-function statement.
- Progress 2026-10-12 (extremal index, scan C17; run length, scan C23):
  `Research.P10.ExtremalIndex` (P10ExtremalIndex.lean): in the block-cluster process with n blocks
  of m days the no-exceedance probability tends to e^{−τ/m} while the independent comparison tends
  to e^{−τ} (`cluster_tendsto`, `iid_tendsto`), and e^{−τ/m} = (e^{−τ})^{1/m} (`extremal_index`):
  θ = 1/m, the reciprocal cluster size. `Research.P10.Cusum` (P10Cusum.lean): for restarted copies
  with P(τ < ∞) ≤ α, {N ≤ n} ⊆ ⋃_{k ≤ n}{τ_k < ∞} (`stop_subset_union`), P(N ≤ n) ≤ (n + 1)α
  (`stop_prob_le`) and E N ≥ Σ_{n<J}(1 − nα) ≥ J/2 for Jα ≤ 1 (`partial_sum_ge_half`,
  `mean_run_length_ge`); Lorden's 1/α constant and CUSUM optimality are not formalised. R:
  `morie_extremal_index_clusters()`, `morie_run_length_bound()`; Python
  `extremal_index_clusters()`, `run_length_bound()`.


## P11. Sentencing effects as intervals: what observational data can say before assumptions

Manski's worst-case selection bounds (Identification for Prediction and
Decision, sec. 7.2; Manski & Nagin 1998 on Utah juvenile sentencing).

- Progress 2026-09-21: `Research.P11` (P11Bounds.lean), finite weighted
  population: `outcome_bounds` with `lower_attained` / `upper_attained`
  (P(y=1,z=t) <= P[y(t)=1] <= P(y=1,z=t) + P(z!=t), sharp), `ate_width_one`
  and `ate_contains_zero` (the contrast interval has width exactly one and
  contains zero: data alone cannot sign a sentencing effect). R:
  `morie_sentence_effect_bounds()`; the test attains both ends by filling
  the unobserved arm.
- Progress 2026-09-21 (contamination): `clean_bounds`, attained ends,
  `clean_width` (p/(1-p)) and `clean_informative` (P11Contaminated.lean;
  library scan B2). R: `morie_contaminated_bounds()`.
- Progress 2026-09-21 (MTR): `mtr_lower`, `mtr_upper`, `mtr_upper_attained`
  (P11Monotone.lean): monotone treatment response gives the contrast the
  sign and the sharp interval [0, P(y=1,z=b) + P(y=0,z=a)] (library scan
  B4). R: `morie_sentence_effect_mtr()`.
- Open: application to OTIS/CPADS
  custody-vs-community sentences and reconviction.
- Noted 2026-10-12: OTIS is segregation placements, not sentences, and CPADS is a postsecondary
  substance-use survey; the application needs a sentencing file with reconviction and stays open.
- Progress 2026-10-02 (Imbens-Manski, library scan B25): `Research.P11` in P11Coverage.lean:
  on a finite probability space P[H subset of C] <= P[theta in C] whenever theta
  in H (`region_coverage_le`); for a strictly increasing CDF the coverage
  F(c + Delta) - F(-c) is strictly increasing in c (`coverage_strictMono_c`), the
  cutoff solving it is non-increasing in Delta (`im_cutoff_antitone`), lies
  below the two-sided quantile with F(-c) <= alpha (`im_cutoff_between`), and the
  two-sided quantile over-covers any region of positive width
  (`two_sided_overcovers`). Existence and uniqueness of the cutoff: for a continuous strictly
  increasing F with the limits of a distribution function the coverage is continuous in c with
  limits -1 and 1, so the intermediate value theorem gives a root for every alpha in (0, 1)
  (`im_cutoff_exists`, added 2026-10-11) and strict monotonicity makes it the only one
  (`im_cutoff_unique`); the root finder computes what the theorems guarantee. R:
  `morie_bounds_confidence()`; the test checks the equation, the ordering of the
  three cutoffs, monotonicity in Delta and the over-coverage on random bounds.
- Progress 2026-10-02 (MTS, Manski & Pepper 2000): `Research.P11.Pop` in P11Selection.lean:
  under monotone treatment selection E[y(b)] <= E[y | z=b] (`mts_mean_b_le`) and
  E[y(a)] >= E[y | z=a] (`mts_mean_a_ge`), so the naive difference of observed means
  overstates the effect (`mts_ate_le_naive`); with MTR as well the contrast lies in
  [0, naive difference] (`mtr_mts_bounds`). R: `morie_sentence_effect_mts()`; Python
  `sentence_effect_mts()`.
- Progress 2026-10-12 (amplification, scan B18): `Research.P11.Amplification`
  (P11Amplification.lean). Γ = (ΛΔ + 1)/(Λ + Δ) is symmetric, at least 1, at most min(Λ, Δ)
  and non-decreasing in each argument (`symmetric`, `ge_one`, `le_min`, `mono_left`), and every
  Γ ≥ 1 is the curve Δ = (ΛΓ − 1)/(Λ − Γ), Λ > Γ (`level_curve`): a sensitivity statement at Γ
  is the same statement at every (Λ, Δ) on that curve. R: `morie_sensitivity_amplification()`;
  Python `sensitivity_amplification()`.

## P12. Ecological inference: when group-level correlations say anything about people

Neighbourhood crime rates regressed on neighbourhood composition are the
workhorse of place-based criminology; Robinson (1950) showed the
group-level correlation can differ in size and sign from the individual one.

- Progress 2026-09-21: `Research.P12` (P12Ecological.lean): `within_orth`
  (the within-group residual is orthogonal to every group-level function),
  exact `cov_decomp` / `var_decomp`, `var_nonneg`, and `ecological_ge`
  (zero within-group covariance gives corr(x,y)^2 <= corr(group means)^2:
  the ecological correlation overstates). R:
  `morie_ecological_decompose()`; the test builds an exactly-orthogonal
  within part and reproduces Robinson's four-person sign reversal
  (individual +0.6, ecological -1).
- Open: an application with
  PSDP/CPADS individual records against TPS neighbourhood aggregates.
- Noted 2026-10-12: the only neighbourhood trait held is the NIA flag (0/1 per neighbourhood), for
  which the Duncan-Davis interval is a point; census trait shares by neighbourhood are needed.
- Progress 2026-10-02 (Duncan-Davis, library scan B22): `Research.P12` in P12Bounds.lean:
  on a finite weighted population the joint mass satisfies p + q - 1 <= pq <= min(p, q)
  (`pq_ge`, `pq_le_p`, `pq_le_q`), so P(y | x) lies in [max(0,(p+q-1)/p), min(1, q/p)]
  (`dd_bounds`) with both ends attained by admissible joint tables (`Cells.ends_attained`);
  the complement rate is pinned by q = p r + (1-p) r' (`dd_complement`); the aggregate rate
  is the m_g p_g-weighted mean, so its bounds are the weighted means of the neighbourhood
  bounds (`dd_aggregate_bounds`). R: `morie_ecological_bounds()`; Python
  `ecological_bounds()`.

## P13. Pooling evaluations: when random effects say more than the sites did

Hot-spots policing and other place-based evaluations are pooled across sites or
studies (Weisburd & Britt ch. 11). The DerSimonian-Laird random-effects model
estimates the between-site variance by a truncation, and a reader is told that
"with homogeneity the random-effects result collapses to the fixed-effect one".

- Estimand: the pooled effect and the between-site variance tau^2; what the
  truncation does to both in a finite sample.
- Progress 2026-10-02 (library scan A11): `Research.P13` (P13Meta.lean). On a
  finite probability space E[max(X,0)] = E[X] + E[max(-X,0)] >= E[X]
  (`truncation_bias`, `pos_part_ge`), and E[max(X,0)] > 0 as soon as one
  outcome of positive probability has X > 0 (`pos_part_pos`): under homogeneity
  E[Q] = k - 1 gives E[X] = 0 for X = (Q - (k-1))/c while Q > k - 1 keeps
  positive probability, so E[tau2_DL] > 0 (`dl_biased_under_homogeneity`).
  tau2_DL >= 0 and = 0 iff Q <= k - 1 (`tauDL_nonneg`, `tauDL_eq_zero_iff`).
  For every tau^2 >= 0 the random-effects variance 1/sum 1/(v_i + tau^2) is at
  least the fixed-effect variance 1/sum 1/v_i, with equality iff tau^2 = 0
  (`re_var_ge`, `re_var_eq_iff`): the two methods coincide in a finite sample
  only when the estimate happens to be truncated. R:
  `morie_meta_random_effects()` (FE and DL-RE with the truncation flag and the
  variance ratio) and `morie_meta_dl_bias()` (the size of the bias under
  homogeneity by simulation on the shared Philox stream).
- Progress 2026-10-02 (HKSJ): `Research.P13HKSJ` (P13HKSJ.lean). The HKSJ variance
  q/sum w exceeds the Wald variance exactly when q >= 1 (`hksj_wider_iff`), q = 0 only
  when every site equals the pooled value (`Q_eq_zero_iff`), and with equal weights the
  HKSJ variance is the one-sample t variance s^2/k (`hksj_equal_weights`). R:
  `morie_meta_hksj()` (DerSimonian-Laird or REML by golden-section search); Python
  `meta_hksj()`.
- Open: the chapter's hot-spots effects as the worked example.

## P14. Judge-leniency designs: what the instrument identifies

Pretrial detention, incarceration and sentence length are studied with the
leniency of a randomly assigned judge as an instrument (Kling 2006; Dobbie,
Goldin & Yang 2018; Aizer & Doyle 2015). The headline number is a Wald ratio;
what it estimates depends on an assumption about judges that the data cannot
check.

- Estimand: the mean effect of detention among the defendants whose detention
  the judge assignment changed (compliers), not the population effect.
- Progress 2026-10-02: `Research.P14` (P14Instrument.lean). On a finite weighted
  population with potential treatments d(z) and outcomes y(d): the intention-to-treat
  contrast equals the compliers' effect mass minus the defiers' (`itt_decomposition`),
  the first stage equals P(c) - P(d) (`first_stage_decomposition`); with no defiers
  and P(c) > 0 the Wald ratio is the compliers' mean effect (`late_identification`);
  with defiers it is (P(c) tau_c - P(d) tau_d)/(P(c) - P(d)) (`wald_with_defiers`), and
  a two-person witness has every effect positive and Wald = -5 (`defiers_can_flip`).
  R: `morie_judge_iv_population()` (exact decomposition on a specified population),
  `morie_judge_iv()` (observed data: Wald, type shares under monotonicity, defier
  sensitivity); Python `judge_iv_population()`, `judge_iv()`.
- Progress 2026-10-02 (many judges): `Research.P14Slope` (P14Slope.lean). With judges
  nested (k detains everyone j detains) the detention rate is monotone
  (`propensity_mono`), the outcome difference is the effect over the marginal compliers
  (`outcome_diff`), and |Y_k - Y_j| <= (hi - lo)(P_k - P_j) (`slope_bound`); a steeper
  pair is not nested (`violation_refutes_monotonicity`), the Frandsen-Lefgren-Leslie test.
  R: `morie_judge_slope_test()`; Python `judge_slope_test()`.
- Progress 2026-10-13 (subgroups): `Research.P14Slope.subgroup_propensity_mono`,
  `subgroup_reversal_refutes`, `crossing_refutes_both` (P14Slope.lean): nesting orders the
  judges' detention rates inside every subgroup, so a pair whose overall order flips inside one
  is nested in neither direction. R: `morie_judge_monotonicity()` (every pair in every subgroup,
  one-sided pooled two-proportion test, Holm-adjusted, and the per-group slope of the subgroup rate
  on the overall rate); Python `judge_monotonicity()` (parity to 5e-15).
- Applied 2026-10-13: Cook County (Illinois) felony sentencing, 26th Street courthouse 2011-2023,
  53 judges, 14 offence categories, 86,568 original sentences (research/applications/
  p14_cook_judges.md). 1,745 of 6,564 within-category comparisons reverse the overall order and
  36 are significant after Holm adjustment, involving 28 of the 53 judges; the Armed Robbery slope
  is negative. Under calendar (not random) assignment each significant reversal is either a
  failure of monotonicity or a difference in case mix inside the category; both break a
  single-scale judge design. The file has no post-sentence outcome, so the Wald ratio itself
  stays open; OTIS carries no judge identifiers.

## P15. Sentencing disparity: what the "unexplained" part of a decomposition is

Gaps in sentences between groups are routinely split into a part "explained by
legal factors" and an "unexplained" remainder read as disparity or discrimination
(Oaxaca 1973; Blinder 1973; the sentencing literature since Mustard 2001).

- Estimand: the gap in mean outcomes and its exact algebraic split, under a
  stated reference group and a stated coding of the covariates.
- Progress 2026-10-02: `Research.P15` (P15Decomposition.lean). With group means
  and least-squares coefficients (each fit through its means), the gap splits
  exactly as explained-at-B's-prices plus unexplained-at-A's-means (`twofold_B`),
  the mirror (`twofold_A`), and endowments + coefficients + interaction
  (`threefold`); the two explained parts differ by exactly the interaction
  (`reference_dependence`, `explained_eq_iff`); recentring a covariate keeps the
  unexplained total but moves c (beta_A - beta_B)_j between the intercept's and
  that covariate's attribution lines (`attribution_shift`). R:
  `morie_disparity_decomposition()` (both references, the interaction, the
  per-variable shift); Python `disparity_decomposition()`.
- Progress 2026-10-02 (DFL): `Research.P15Reweight` (P15Reweight.lean). Reweighting
  group 0 by psi(x) = m_1(x)/m_0(x) reproduces group 1's covariate distribution exactly
  under common support (`reweighting_matches`, `reweighted_mass`); with group-0 outcomes a
  function of x the reweighted mean is group 1's composition at group 0's structure
  (`counterfactual_outcome`), and the gap splits into structure plus composition
  (`decomposition`). R: `morie_dfl_reweight()`; Python `dfl_reweight()`.
- Open: the distributional (recentered-influence-function) versions; an application to
  OTIS sentence lengths by offence score.
- Progress 2026-10-12 (RIF): `Research.P15.RIF` (P15RIF.lean). On a finite weighted population
  the RIF of the mean averages to the mean (`rif_mean_avg`); the RIF of the τ-quantile at a point q
  with density f averages to q + (τ − F(q))/f (`rif_quantile_avg`), equal to q exactly when
  F(q) = τ (`rif_quantile_avg_eq_iff`); the gap of two groups' RIF means is the quantile gap plus
  the two recentering terms (`rif_gap`), so the P15 split applied to RIF means is a split of the
  quantile gap. R: `morie_rif_quantile()`, `morie_rif_gap()`; Python `rif_quantile()`, `rif_gap()`.
  Open: the OTIS sentence-length application.
- Application 2026-10-12 (research/applications/p15_otis_placement_lengths.R): OTIS segregation
  placements, 82,001 rows (fiscal 2023-2025), consecutive days per placement, women against men,
  age category × region (15 cells) as the covariates. Women's placements are 1.42 days longer on
  average; reweighting men to women's cells moves men's mean by −0.14 days, so the composition
  term is negative and the structure term is 1.56 days (the DFL identity holds to 1e-16). The RIF
  split says where: the medians coincide (2 days), the 75th-percentile gap is 1 day and the
  90th-percentile gap 3 days, with recentering terms of −0.47 and −0.63. The gap is a tail, not
  a shift. "Structure" is what two covariates leave; placement reason, alerts and institution
  are in the file and are the next covariates. OTIS carries no sentence lengths, so the
  sentence-length version named here is not possible from the public file.

## P16. Court backlog: what a disposition-time statistic already says

Courts report mean time to disposition and filings per year; backlog is argued
about as if it were a third number.

- Estimand: the time-average number of pending cases over a window.
- Progress 2026-10-02: `Research.P16` (P16Backlog.lean). On a finite docket inside
  [0, T] the integral of the pending count is the sum of the case durations
  (`occupancy_integral`, a Lebesgue integral of indicator sums), so the time-average
  load equals the filing rate times the mean disposition time (`little`); a reported
  mean disposition time and filing rate determine the backlog (`little_backlog`),
  and a backlog target fixes the mean disposition time that achieves it
  (`little_target`). No probability model. R: `morie_court_backlog()` (reports the
  censored cases, whose exclusion truncates the mean: a P1-type dark figure);
  Python `court_backlog()`.
- Progress 2026-10-02 (censoring): `Research.P16Censoring` (P16Censoring.lean). The true
  cohort mean is at least (sum t + sum a)/(n + m) (`true_mean_ge`); the lower bound minus the
  disposed mean is m/(n+m)(abar - tbar) (`lower_bound_sub`, `bias_lower`), so the published
  mean understates the truth whenever the pending cases are older on average
  (`disposed_understates`); no upper bound exists without a cap (`no_upper_bound`). R:
  `morie_backlog_censoring()`; Python `backlog_censoring()`.
- Progress 2026-10-13 (published statistics): `Research.P16Censoring.band_floor_le_mean`,
  `band_floor_shares` (P16Censoring.lean): the share-weighted band floors bound the disposed mean
  from below, in the form courts publish. R: `morie_backlog_published()` (Little's law read
  backwards from published filings and pending counts, against the published mean, with the band
  bound); Python `backlog_published()`.
- Applied 2026-10-13: Ontario Court of Justice criminal statistics, October 2018 to March 2026,
  61 courts (research/applications/p16_ocj_backlog.md). In all 7 full years the pending docket
  implies cases stay 1.21 to 1.97 times as long as the published mean (2021: 278 days against
  141), highest in the pandemic years; the published mean always exceeds its band floor bound.
  Over the last 12 months the ratio runs from 0.98 (Ottawa, Cobourg, Cornwall) to 3.3 (Welland),
  and every offence group is above 1 (1.13 to 1.29).

## P17. Incapacitation: what a sentence year buys

The incapacitation estimate behind every "lock them up" argument (Avi-Itzhak &
Shinnar 1973; Blumstein, Cohen & Nagin 1978) is a steady-state identity.

- Estimand: the long-run crime rate of an offender who offends at rate lambda while
  free and serves S after each conviction (probability q per crime), and its gradient.
- Progress 2026-10-02: `Research.P17` (P17Incapacitation.lean): lambda f/(f + S) at the
  mean free time f = 1/(lambda q) equals lambda/(1 + lambda q S) (`steady_state_rate`), and
  the realised rate on any N cycles with proportional crime counts is the same
  expression at the sample mean free time (`cycle_rate`); the prevented share
  lambda q S/(1 + lambda q S) is 1 - rate/lambda and strictly below one
  (`prevented_share_eq`, `prevented_share_lt_one`); the rate is antitone in S and in q
  (`rate_antitone_in_S`, `rate_antitone_in_q`); one more year prevents
  lambda^2 q/((1+lambda q S)(1+lambda q(S+1))) > 0 crimes a year (`marginal_prevention_eq`,
  `marginal_prevention_pos`); a uniform sentence removes the larger share from the
  higher-rate group (`high_rate_more_prevented`). R: `morie_incapacitation()`; Python
  `incapacitation()`.
- Progress 2026-10-02 (replacement, desistance): `Research.P17Replacement`
  (P17Replacement.lean). Replacement scales the prevented share by 1 - r, monotonically to
  zero (`replaced_le`, `replaced_antitone`, `replaced_full`); under a non-increasing rate
  path the crimes prevented by S periods from career age t0 lie between S lam(t0 + S) and
  S lam(t0) (`prevented_ge_const`, `prevented_le_const`), fall when the sentence is served
  later (`later_sentence_prevents_less`), and the entry-rate constant-lambda estimate is
  an upper bound under both corrections (`prevented_net_le`). R:
  `morie_incapacitation_career()`; Python `incapacitation_career()`.
- Applied 2026-10-13: published reconviction, in convictions (the conviction rate lambda q is what a
  reconviction hazard measures, and the prevented share depends on lambda and q only through it)
  (research/applications/p17_reconviction_incapacitation.md). Juristat 85-002-X 2024001: the
  quarterly hazard of a first new conviction falls from 0.30 to 0.01 after custody and 0.14 to 0.01
  after community supervision over four years; a year of custody imposed at release prevents
  0.754 first reconvictions per person released from custody (constant entry-rate bound 1.43),
  0.132 if served two years later. Ontario 2014/15 releases by risk level: a six-month term
  removes 0.7% of a very-low-risk releasee's convictions and 20.6% of a very-high-risk one's.
  The published hazard is the cohort's, so desistance and selection are not separated, and
  offending per conviction (q) stays unidentified; custody lengths by person remain unpublished.

## P18. Selective labels: evaluating a release rule from the released alone

Bail and pretrial-detention algorithms are judged against judges, but a
failure (a missed court date, a new arrest) is observed only for defendants
some judge released (Lakkaraju et al. 2017; Kleinberg et al. 2018).

- Estimand: the failure rate of the set a proposed rule would release.
- Progress 2026-10-02: `Research.P18` (P18Selective.lean). The reported rate is
  the rate among the released (`observed_rate_is_conditional`); a rule's set
  inside a more lenient judge's release set is scored exactly from that judge's
  outcomes, the contraction argument (`nested_rate_identified`); otherwise the
  rule's rate lies in [fails(M & R)/w(M), (fails(M & R) + w(M \ R))/w(M)]
  (`unobserved_bounds`), an interval whose width is the unobserved share
  (`unobserved_width`) with both ends attained (`unobserved_ends_attained`).
  R: `morie_selective_labels()`; Python `selective_labels()`.
- Progress 2026-10-11 (many judges): `Research.P18.unobserved_mass_antitone`,
  `interval_width_antitone`, `chain_contraction`, `chain_width_antitone`, `chain_width_zero_iff`
  (P18Selective.lean): with judges' release sets nested by leniency a rule's unobserved mass,
  hence its identified interval's width, is non-increasing along the chain; a rule whose set
  the chain covers lies inside the most lenient judge's set and is scored exactly from that
  judge's outcomes; the width is zero exactly on containment (positive weights). R:
  `morie_selective_labels_judges()` (one row per judge, nestedness check, sharpest interval);
  Python `selective_labels_judges()`.
- Open: an application to Ontario bail outcomes once a released-cohort file is public.

## P19. Regression to the mean at selected hot spots

Crime falls at the places chosen for being high even with no intervention
(Galton 1886; Campbell & Stanley 1963); the hot-spots trials randomise within
the selected set for exactly this reason (Sherman & Weisburd 1995).

- Estimand: the change in the second period on the places selected on the first.
- Progress 2026-10-02: `Research.P19` (P19Regression.lean). On a finite weighted
  population whose two periods are exchangeable (a weight-preserving bijection swaps
  them), the selected mass and the cross term reindex (`exchange_mass`,
  `exchange_cross`), x1 (1{x1>c} - 1{x2>c}) >= c (1{x1>c} - 1{x2>c}) pointwise
  (`indicator_bound`), hence sum_{x1>c} w (x2 - x1) <= 0 (`selected_change_nonpos`)
  and the mirror for low-selected places (`low_selected_change_nonneg`). R:
  `morie_regression_to_mean()` reports the observed, mirror and symmetrised change
  (the statistic on the data plus its swapped copy, which the theorem makes
  non-positive); Python `regression_to_mean()`.
- Progress 2026-10-02 (shrinkage): `Research.P19Shrinkage` (P19Shrinkage.lean). Under
  the finite noise law (sum w e = 0, sum w theta e = 0) the loss of the shrinkage
  estimator (1 - B) y + B ybar is (1 - B)^2 S_e + B^2 S_theta (`loss_eq`), minimised at
  B* = S_e/(S_e + S_theta) (`loss_min`, `bstar_mem`) where it never exceeds the raw loss
  (`loss_bstar_eq`, `loss_bstar_le_raw`); the predicted fall of a place is B (y - ybar)
  (`predicted_fall`), the size the sign theorem left open. R: `morie_hotspot_shrinkage()`,
  `morie_shrinkage_loss()`; Python `hotspot_shrinkage()`, `shrinkage_loss()`.
- Open: the sampling error of B* itself (S_theta is estimated from the same counts).
- Progress 2026-10-12 (error of B*): `Research.P19Error` (P19Error.lean). Under the noise law the
  total mass of the counts is S_e + S_θ (`Sy_eq`), so B* = S_e/S_y (`bstar_from_total`); the
  plug-in B̂ = S_e/Ŝ_y is off by S_e(S_y − Ŝ_y)/(S_yŜ_y) (`bhat_error`), at most
  B*|Ŝ_y − S_y|/Ŝ_y (`bhat_error_bound`), the predicted fall inherits the error times the place's
  excess (`fall_error`), and B̂ falls as Ŝ_y rises (`bhat_antitone`). R:
  `morie_shrinkage_factor_error()` (jackknife spread of Ŝ_y, the implied interval for B̂, the exact
  error when the total is known); Python `shrinkage_factor_error()`.
