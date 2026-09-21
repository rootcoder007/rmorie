# The hardest open problems in criminology: a working ledger

Rootcoder007, started 2026-09-21. Fresh start: nothing here is
taken from earlier MRP/VSR work; those results are a later reproducibility
target inside rmorie, not the seed. Workspace: l14 `~/work/rmorie-research`
(branch `research`), Lean project to be created as `~/work/researchproofs`.

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
  full trajectory. Still open: the stochastic urn (martingale) version.
- Progress 2026-09-21 (stochastic): `Research.P4.Urn` (P4Urn.lean), on the
  finite path space: `urn_step_martingale` (the share is a martingale for
  any reinforcement), `polya_uniform` (equal rates, one count each: the
  number of A-discoveries after n draws is uniform on 0..n) and
  `polya_no_concentration` (P(1/4 ≤ share ≤ 3/4) ≥ 1/4 for all n ≥ 2). So
  "equal rates, no runaway" holds for the mean field and fails for the
  process: the share converges to a random limit (uniform on (0, 1)) fixed
  by early luck. R: `morie_feedback_loop_urn_law()`. Still open: the
  unequal-rate urn (a Rubin-type reinforcement theorem; the library has no
  source for it, see LIBRARY-SCAN-2026-09-21.md C1–C3), the Azuma drift
  envelope, and the a.s. convergence statement itself (needs Mathlib's
  martingale convergence on a genuine filtered space).

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
- Open: the two regimes (re-classification vs cuffing) are not
  distinguishable from a single recorded series (a formal
  non-identification statement with a second source as the fix);
  application to TPS MCI 2014-2024 robbery/theft and assault ratios with
  CSUS victimisation as the second source (library scan A12, A13, A15).


## P10. Near-repeat contagion: what a branching ratio commits you to

Self-exciting (Hawkes) models of near-repeat victimisation (Mohler et al.;
D'Orsogna & Perc review) report a branching ratio n. The offspring of one
background event form a Galton-Watson tree, so n fixes the expected
cluster size, the stationary rate and the share of "contagious" events.

- Progress 2026-09-21: `Research.P10` (P10Contagion.lean):
  `generation_mean`, `cluster_size_of_lt_one` (1/(1-n)),
  `stationary_rate` (mu/(1-n)), `cluster_size_diverges_of_ge_one`,
  `endogeneity_share` (= n). R: `morie_contagion_branching()`; the test
  simulates Poisson-offspring trees and recovers 1/(1-n).
- Open: extinction probability as the smallest fixed point of the PGF
  (needs convexity of the PGF; GS Theorem 5.4.5), the Bartlett
  non-identification of contagion vs heterogeneity from the K-function
  (library scan B20), and an application to TPS break-and-enter with
  constant vs KDE background (A17).


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
- Open: monotone treatment response and monotone treatment selection
  bounds (Manski sec. 9.3, library scan B4), Imbens-Manski confidence sets (B25); application to OTIS/CPADS
  custody-vs-community sentences and reconviction.


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
- Open: Duncan-Davis bounds for the individual proportion from group
  marginals (Manski sec. 5.1; library scan B22), and an application with
  PSDP/CPADS individual records against TPS neighbourhood aggregates.
