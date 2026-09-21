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
  spillover, direct, total, pooling bias, positivity check). Next: the
  Horvitz–Thompson version with known assignment probabilities and its
  unbiasedness proof; ring mis-specification bound as a function of the
  d+1 ring's exposure share; TPS street-segment application.

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
  _base_rate_bounds/_true_rate`. Next: (iii) which group comparisons stay
  decidable when the two intervals overlap, and bounds on group-wise FPR/FNR
  (not only base rates) under the same noise boxes.

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
