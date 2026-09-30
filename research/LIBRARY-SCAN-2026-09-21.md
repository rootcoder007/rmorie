# Library scan, 2026-09-21: candidate problems from the criminology, statistics and mathematics shelves

Three readers went through the WD library (`/run/media/rootcoder/WD_BLACK/library/pdf`, 655 PDFs),
the full-text corpus index (`~/work/ledger/wave3/corpus_index`, 730 PDFs) and `~/work/corpus/papers`
on l14, looking for claims that can be stated as a theorem, a counterexample target, or an
empirical test on data we hold (Toronto Police Service open data, Ontario OTIS, StatCan
CSUS/CPADS/PSDP, NYC complaint CSV, FBI CDE). Page numbers are the printed pages of each source
unless marked PDF. Every item is a candidate, not a result: nothing below is proved until it has a
file under `research/lean/` and a row in `research/README.md`.

Each item: **title** (source) — problem — PROVE/DISPROVE — EMPIRICAL — EXTENDS.

## A. Criminology shelf (Weisburd & Britt; Eterno, Verma & Silverman; Kikuchi; D'Orsogna & Perc; Stolzenberg et al.; TPS reports; corpus papers)

A1. **Separation, not "misspecification", is why the Baldus death-penalty logit failed** (Weisburd & Britt, *Advanced Statistics in Criminology and Criminal Justice*, ch. 1 pp. 3-7) — the book reads non-convergence of the NJ proportionality-review logit as "serious problems in the specification"; with 39 death sentences and dozens of aggravators it is complete or quasi-complete separation — PROVE: the logistic MLE exists and is unique iff no b ≠ 0 satisfies (2y_i − 1) x_i·b ≥ 0 for all i (Albert–Anderson); counterexample: a correctly specified model with one perfectly predictive dummy has no MLE — EMPIRICAL: refit a prison-in/out logit on OTIS or CSUS with rare-aggravator dummies; Firth fit vs ML divergence — EXTENDS: new, touches P5.

A2. **LDA coefficients are not logistic coefficients** (same, p. 5) — Baldus substituted discriminant analysis when the logit would not converge, claiming equivalence — PROVE: Fisher LDA log-odds equal a logistic model iff class-conditional X are Gaussian with equal covariance; counterexample: binary covariates — EMPIRICAL: simulate binary-aggravator sentencing data; compare LDA-derived vs Firth-logit odds ratios — EXTENDS: A1.

A3. **Prediction intervals spanning [0,1] make a ranking uninformative** (same, pp. 5-6, Fig. 1.1) — PROVE: ranking by point estimates with Var(p̂_i) ≥ v has expected Kendall τ with the true ranking ≤ f(v, n), and f → 0 as v → 1/4 — EMPIRICAL: bootstrap rank stability of a recidivism score on OTIS/CSUS — EXTENDS: P5.

A4. **"Any measurement error in the independent variables attenuates the coefficient toward 0"** (W&B ch. 2 p. 32) — true only for classical error in a single regressor — PROVE: plim β̂ = β σ²_x/(σ²_x + σ²_u) in the one-regressor case; counterexample: two correlated regressors with error in one, explicit 2×2 formula gives bias of either sign on the other — EMPIRICAL: TPS neighbourhood crime on census disadvantage plus density with added noise — EXTENDS: P2.

A5. **Logistic coefficients are not comparable across nested models** (W&B ch. 4 pp. 158-161) — PROVE: in a latent-index logit, adding an independent covariate of variance τ² rescales existing coefficients by (π²/3)/(π²/3 + τ²)^{1/2} (Karlson–Holm–Breen); odds-ratio comparisons across covariate sets are not identified without variance normalisation — EMPIRICAL: CSUS custody logits with incremental covariates; raw β vs KHB vs average marginal effects — EXTENDS: P2, P5.

A6. **Negative binomial "does not effectively model zero-inflated data"** (W&B ch. 6 p. 255) — PROVE: for any ZIP(π, λ) there is an NB(μ, θ) matching P(0) and the mean, so the zero share alone cannot identify inflation; state when ZIP and NB are distinguishable — EMPIRICAL: TPS MCI by neighbourhood-month and NYC CSV; Poisson/NB/ZIP/ZINB, rootograms, boundary-corrected LR — EXTENDS: P7.

A7. **Rosenbaum Γ-bounds are not a test of ignorability** (W&B ch. 10 pp. 431-433) — PROVE: the worst-case Mantel–Haenszel p-value is monotone in Γ, some finite Γ* kills any finite-sample effect, and Γ* is a function of the observed table only (invariant to the true hidden bias) — EMPIRICAL: matched OTIS re-incarceration; add a synthetic hidden confounder and show Γ* unchanged — EXTENDS: P5, P8.

A8. **Blocking does not always increase power** (W&B ch. 9 pp. 389-390, 400-401) — PROVE: the exact crossover in within-pair correlation below which the unblocked t design has more power for n pairs — EMPIRICAL: blocked vs naive analysis of paired TPS hot spots — EXTENDS: P3.

A9. **Adding baseline covariates "will not affect the treatment estimate, in theory"** (same pp. 400-401) — PROVE: under the Neyman model the OLS-adjusted difference is consistent but not unbiased and can be less precise than the unadjusted one under unequal allocation and differing slopes (Freedman 2008); Lin's interaction estimator restores it — EMPIRICAL: permutation study on TPS zones with unequal allocation — EXTENDS: P3.

A10. **Design-effect inflation is direction-specific** (W&B ch. 7 pp. 285-287) — PROVE: for a cluster-level regressor Var(β̂_OLS) is understated by 1 + (m − 1)ρ; for a within-cluster-balanced regressor it is overstated — EMPIRICAL: CSUS sentences nested in judge; naive vs cluster-robust vs mixed — EXTENDS: P3.

A11. **Random-effects meta-analysis does not collapse to fixed-effect under homogeneity in finite samples** (W&B ch. 11 pp. 467-470) — PROVE: E[τ̂²_DL] > 0 when τ² = 0; RE variance ≥ FE variance with equality iff τ̂² = 0 — EMPIRICAL: meta-analyse the chapter's hot-spots effects with FE/DL/REML/HKSJ — EXTENDS: new.

A12. **Pure downgrading is invisible in totals but visible in category ratios** (Eterno, Verma & Silverman, *How Countries Count Crime*, ch. 15 pp. 221-222; Patrick ch. 14 pp. 180-183, 206-208) — PROVE: with recorded r = M c, M column-stochastic on the notifiable set, Σr = Σc and the split is identified only if M is known; cuffing to non-notifiable makes M substochastic and the total falls; reclassification-only vs cuffing regimes are not distinguishable from a single series — EMPIRICAL: TPS MCI 2014-2024 and NYC complaints, robbery/larceny and aggravated/simple assault ratios around accountability changes, with CSUS/NCVS as second source — EXTENDS: P1, P2.

A13. **Hierarchy-rule undercount is bounded by the co-offence rate** (Eterno ch. 15 p. 221) — PROVE: N_UCR ≤ N_offences ≤ k N_UCR with k the maximum offences per incident — EMPIRICAL: FBI CDE NIBRS vs SRS 2016-2023 by agency; TPS MCI multi-offence rows — EXTENDS: P1.

A14. **Recording rate is identified only on the survey-comparable subset** (Patrick ch. 14 pp. 180-181) — PROVE: with the survey covering offence set A, the rate on A is identified and the total dark figure is bounded as a function of the share of A and the comparable-subset rate — EMPIRICAL: StatCan GSS victimisation vs CSUS for the comparable offences, Ontario — EXTENDS: P1.

A15. **"Downgrade and detect via caution" mechanically raises the detection rate** (Patrick ch. 14 pp. 206-208) — PROVE: moving a fraction q of class i (detection rate d_i) into a class detected with probability 1 raises aggregate detection by q n_i (1 − d_i)/N with Σr fixed — EMPIRICAL: TPS clearance by offence and disposal type; cleared-otherwise share vs clearance-rate jumps — EXTENDS: P2, P4.

A16. **Suppression displaces supercritical hot spots but eradicates subcritical ones** (D'Orsogna & Perc, *Statistical physics of crime*, pp. 4-5, after Short et al. 2010) — PROVE: the uniform state of the Short–Brantingham reaction-diffusion system loses stability at a Turing threshold; the bifurcation is subcritical iff a computable cubic amplitude coefficient is positive — EMPIRICAL: fit repeat-victimisation parameters to TPS break-and-enter near-repeat statistics; classify neighbourhoods by regime — EXTENDS: P3, P4.

A17. **A Hawkes forecast is stationary only when the branching ratio is below 1** (D'Orsogna & Perc p. 6; Mohler et al.) — PROVE: λ(t) = μ + Σ g(t − t_i) is stationary with finite mean iff n = ∫g < 1, mean rate μ/(1 − n); n̂ is biased upward under unmodelled background heterogeneity — EMPIRICAL: TPS break-and-enter and NYC CSV with constant vs KDE background — EXTENDS: P4, P7.

A18. **The inspection game has a cyclic phase where more inspection does not monotonically reduce crime** (D'Orsogna & Perc pp. 7-8, Eq. 6) — PROVE: in the replicator limit of the three-strategy game the interior fixed point is a centre for a parameter set of positive measure — EMPIRICAL: lagged negative autocorrelation between TPS charges and subsequent offence counts by zone — EXTENDS: P8.

A19. **Stick versus carrot under a fixed budget has an interior optimum** (D'Orsogna & Perc p. 11; Berenji et al.) — PROVE: under hτ + θ = C the recidivism ratio is quasi-concave in θ with an interior maximiser when the rehabilitation response is concave and the deterrence response bounded — EMPIRICAL: OTIS sentence length and program participation vs re-admission — EXTENDS: P8, P5.

A20. **Interracial crime rates with race-of-offender denominators load mechanically on percent Black** (Stolzenberg, Eitle & D'Alessio 2006 pp. 308-312) — PROVE: under random mixing, expected A-on-B offences are proportional to p_A p_B N, so the "per A population" rate equals k p_B and regressing on percent B recovers a positive coefficient with no behavioural content — EMPIRICAL: NYC complaints and FBI NIBRS with pair-exposure offsets — EXTENDS: P2, new.

A21. **Race-specific arrest rates proxy offending only under equal arrest probability** (Stolzenberg p. 306; D'Alessio & Stolzenberg 2003) — PROVE: arrest_g = offend_g π_g; the arrest ratio identifies the offending ratio iff π_B = π_W; with π known within a factor c the ratio lies in [obs/c, obs·c]; the Hindelang convergence test is valid only for contact offences — EMPIRICAL: TPS race-based data vs victim-reported offender descriptions; NYC complaints vs arrests — EXTENDS: P2 (this is the missing test for its premise).

A22. **Multiple-benchmark disproportionality is multiplicative, not additive** (TPS *Understanding Use of Force in 2020: Methodological Report*, pp. 20-25) — PROVE: the resident-benchmark ratio equals R_contact × R_force|contact; the enforcement-action benchmark is valid only if contact is independent of latent force propensity; state the selection bound — EMPIRICAL: TPS use-of-force and enforcement-action data 2020-2023 — EXTENDS: P2.

A23. **Offset errors shift a rate-model disparity by exactly −log κ** (Jung 2022 TPS review, pp. 2-3, 30-31: a 30-58× disparity became 4-5×) — PROVE: in a log-link rate model with offset log E_g, a multiplicative error κ in E_g shifts the group coefficient by −log κ; disparity ratios are identified only up to the ratio of exposure errors — EMPIRICAL: reproduce the review's models on TPS use-of-force zone counts — EXTENDS: P2.

A24. **Equal hit rates with unequal stop rates is consistent with bias or with none** (TPS *From Impact to Action*, hit-rate paragraph after Bowling & Phillips) — PROVE: the outcome test needs a single-crossing signal condition; construct the infra-marginality counterexample and the threshold model in which equal hit rates certify equal thresholds — EMPIRICAL: NYC stop-and-frisk hit rates by race and precinct via a threshold model — EXTENDS: P2, P5.

A25. **Spatial-lag omission is bias, spatial-error omission is inefficiency; GWR "significant variation" is a multiple-testing artefact** (Kikuchi, *Neighborhood Structures and Crime*, pp. 3-4, 83-95) — PROVE: OLS is unbiased but inefficient under a spatial error and inconsistent under a spatial lag; the GWR local test's family-wise error tends to 1 without an effective-parameter correction — EMPIRICAL: TPS neighbourhood MCI on census covariates, SEM vs SAR vs OLS, GWR with and without correction — EXTENDS: P3, P7.

## B. Statistics shelf (Manski; Lohr; Hernán & Robins; Pearl; Zubizarreta et al.; Schabenberger & Gotway; Wager; Friendly & Meyer; Freedman, Pisani & Purves; Brus)

B1. **Worst-case selection bounds on a treatment effect have width 1 and contain 0** (Manski, *Identification for Prediction and Decision*, §7.2 pp. 134-136) — PROVE: for binary y, H{P[y(t) = 1]} = [P(y = 1, z = t), P(y = 1, z = t) + P(z ≠ t)] and the ATE region has width exactly 1 and contains 0 for every treatment share — EMPIRICAL: OTIS/CPADS custody vs community sentence and 2-year reconviction; report the region — EXTENDS: P8, P5.

B2. **Contaminated-sample bounds have width p/(1 − p) and are informative only when p < min(P(B), 1 − P(B))** (Manski §5.2 pp. 98-101, eq. 5.10; Complement 5A p. 107) — PROVE: sharpness via truncated extremal distributions; for discrete y the quantile region is [D(L_p), D(U_p)] ∩ Y with interior points infeasible — EMPIRICAL: TPS MCI with a known unfounded share p; NYC offence-code changes — EXTENDS: P2.

B3. **Relative risk lies between 1 and the odds ratio under case-control sampling** (Manski §6.2 pp. 114-117, eqs. 6.11-6.14) — PROVE: RR_p is monotone in the base rate p with derivative sign sign(1 − OR); derive the attributable-risk bound — EMPIRICAL: FBI CDE arrests vs CSUS victimisation base rates — EXTENDS: P5, P2.

B4. **Monotone treatment response: lower bound 0, sharp data-dependent upper bound** (Manski §9.3 pp. 189-191, eqs. 9.10-9.15) — PROVE: sharpness via the per-person chain; binary case — EMPIRICAL: OTIS ordered sentence length vs reconviction — EXTENDS: P8.

B5. **Petersen and Chapman respect the logical floor N ≥ n₁ + n₂ − m; the Wald interval does not** (Lohr, *Sampling: Design and Analysis*, §13.1 pp. 495-498) — PROVE: n₁n₂/m − (n₁ + n₂ − m) = (n₁ − m)(n₂ − m)/m ≥ 0 and the Chapman analogue with m + 1; characterise when the Wald lower limit falls below the floor — EMPIRICAL: TPS reported list vs CSUS victim list — EXTENDS: P1.

B6. **k-list capture-recapture never identifies the highest-order interaction** (Lohr §13.2 pp. 502-506) — PROVE: the map from interaction terms to the 2^k − 1 observed cells is surjective, so the missing cell ranges over (0, ∞) and N̂ over [distinct count, ∞); only pairwise dependence is testable for k ≥ 3 — EMPIRICAL: three-list estimation on TPS, CSUS and a third list — EXTENDS: P1 (generalises the two-source breakdown to k sources).

B7. **Sen–Yates–Grundy variance is zero iff t_i ∝ π_i; unbiased variance estimation needs π_ik > 0 for all pairs** (Lohr §6.4 pp. 241-242, Thm 6.2/6.4; Brus, *Spatial Sampling with R*, ch. 5) — PROVE: V(t̂_HT) = ½ΣΣ(π_iπ_k − π_ik)(t_i/π_i − t_k/π_k)²; counterexample: systematic sampling has no design-unbiased variance estimator — EMPIRICAL: systematic vs SRS spatial samples of TPS grid counts — EXTENDS: P3 (the joint-probability condition now proved in `Research.P3.Design.ht_variance_estimator_unbiased`; the SYG form and the zero-variance characterisation are open).

B8. **Basu's elephant: unbiased Horvitz–Thompson with exploding variance** (Lohr §6.7 pp. 263-264) — PROVE: for the design with one near-certain unit, Var(t̂_HT) = Σ t_i²(1 − π_i)/π_i is unboundedly worse than the ratio-type alternative — EMPIRICAL: reweighting TPS hot-spot samples with extreme inclusion probabilities — EXTENDS: P3.

B9. **Design effect 1 + (M − 1) ICC with ICC ≥ −1/(M − 1)** (Lohr §5.2 eqs. 5.8-5.10) — PROVE: the ICC identity and the variance ratio — EMPIRICAL: ICC of TPS offences within neighbourhoods; effective sample size for neighbourhood rates — EXTENDS: P7, P3.

B10. **Nonresponse bias and a 93 percent dark figure** (Lohr §8.1 p. 332; Eterno et al., Azaola & Roberson ch. p. 62: Mexico 10.6 percent reported) — PROVE: with reporting rate r the identification region of a proportion has width exactly 1 − r (Manski ch. 2 eq. 2.4) — EMPIRICAL: CSUS reporting rate by offence times TPS counts: per-offence bound widths — EXTENDS: P1.

B11. **Instrumental-variable natural bounds and Balke's collapse** (Hernán & Robins, *What If*, Technical Point 16.2; Pearl, *Causality* 2e §8.2.4 pp. 268-269) — PROVE: natural bounds have width P(A = 1 | Z = 0) + P(A = 0 | Z = 1); construct the case where sharp bounds are a point at 50 percent noncompliance; natural bounds are sharp iff no contrarians — EMPIRICAL: assigned patrol or mandatory-arrest policy as Z, actual arrest as A, re-offence as Y — EXTENDS: P8.

B12. **Instrumental inequality** (Pearl 2e §8.4 pp. 274-275, eqs. 8.22-8.23) — PROVE: max_x Σ_y max_z P(y, x | z) ≤ 1 for finite Z, X, Y; counterexample target: no testable constraint for continuous X (Bonet) — EMPIRICAL: test court/officer assignment instruments in OTIS — EXTENDS: P8.

B13. **Probability of necessity: sharp bounds, identification under monotonicity, and a testable monotonicity condition** (Pearl 2e Thms 9.2.10-9.2.15 pp. 289-295) — PROVE: the bounds under exogeneity, PN = excess risk ratio under monotonicity, the confounded correction, and the necessary test P(y_{x'}) ≤ P(y) ≤ P(y_x) — EMPIRICAL: fraction of desistance attributable to sanction in OTIS with an experimental benchmark — EXTENDS: P8, P5.

B14. **Nondifferential misclassification attenuates only for binary exposure; ordinal exposure can reverse sign** (Hernán & Robins Fine Point 9.1, Technical Point 9.1) — PROVE: RD* = (se + sp − 1) RD for binary; counterexample with a three-level exposure and non-monotone E[A* | A] — EMPIRICAL: TPS severity codes vs CSUS self-reports; label noise in reconviction — EXTENDS: P5, P2.

B15. **Hazard ratios carry built-in selection: HR_{k+1} < 1 with zero individual effect** (Hernán & Robins Fine Point 17.2, Fig. 17.3) — PROVE: the two-type construction; a Cox-averaged HR can equal 1 with non-identical survival curves — EMPIRICAL: OTIS time-to-reconviction Cox HR vs 24-month risk difference — EXTENDS: P5 (new estimand: survival).

B16. **Collider-stratification sign depends on mechanism** (Hernán & Robins Fine Point 8.2 pp. 123-124) — PROVE: closed-form OR_{AE | Y = 1} under the "or" sufficient-cause model and its limit as background prevalence tends to 1; the "and" mechanism gives positive association — EMPIRICAL: association between independent risk factors among TPS arrestees — EXTENDS: P2.

B17. **Design sensitivity: the power of a sensitivity analysis tends to 1 or 0 depending only on μ versus μ_Γ** (Zubizarreta et al. Handbook, Fogarty ch. 25 §25.4.1 pp. 560-561, eq. 25.15; W&B ch. 10 pp. 431-432) — PROVE: the limit and the definition of Γ̃ as the root of μ = μ_Γ — EMPIRICAL: matched OTIS programme evaluation, Γ̃ for Wilcoxon vs M-statistics — EXTENDS: P8, P5.

B18. **Amplification: one Γ is a curve of (Λ, Δ) pairs** (Handbook ch. 2 §2.3.4 p. 28, eq. 2.12) — PROVE: Γ = (ΛΔ + 1)/(Λ + Δ) is an exact reparametrisation with identical worst-case p-values — EMPIRICAL: as B17 — EXTENDS: P8.

B19. **Moran's I detects spurious autocorrelation under a deterministic trend** (Schabenberger & Gotway, *Statistical Methods for Spatial Data Analysis*, §1.3.2.2 pp. 22-23, eq. 1.16) — PROVE: E[I] ≠ −1/(n − 1) under mean nonstationarity; for OLS residuals E[I_res] = (n/w..) tr(MW)/(n − p) — EMPIRICAL: TPS neighbourhood counts with a downtown-distance trend, raw vs residual Moran — EXTENDS: P7, new.

B20. **Cluster process and heterogeneous Cox process are indistinguishable from one realisation** (Schabenberger & Gotway §3.7 pp. 122-128, eqs. 3.16-3.17; Bartlett) — PROVE: the Neyman–Scott K-function does not depend on the offspring mean and equals that of a matching Cox process, so near-repeat contagion vs risk heterogeneity is not identified from K — EMPIRICAL: TPS burglary K-function; Hawkes vs Cox fits — EXTENDS: P4, P6-type non-identifiability.

B21. **Permutation tests under interference are valid only with focal-unit imputable statistics; IPW exposure estimator unbiased iff positivity; variance bias formula** (Wager, *Causal Inference* notes, ch. 11 pp. 142-147, Thms 11.1-11.2; ch. 12 Thm 12.1 p. 153, Thm 12.4 p. 160) — PROVE: the three statements — EMPIRICAL: TPS neighbourhood interventions with adjacency exposure — EXTENDS: P3.

B22. **Ecological correlations overstate individual correlations, but not always** (Freedman, Pisani & Purves, *Statistics* 4e, ch. 9 §4 pp. 148-150; Manski §5.1-5.2 pp. 94-99, Duncan–Davis bounds) — PROVE: with uncorrelated within-group (x, y), |corr(group means)| ≥ |corr(individuals)|; counterexample with opposite-sign within-group correlation (Robinson 1950) — EMPIRICAL: PSDP/CPADS individual records vs TPS neighbourhood aggregates — EXTENDS: P2, P7, new.

B23. **The Poisson zero share e^{−μ} is a lower bound for every Poisson mixture with the same mean** (Friendly & Meyer, *Discrete Data Analysis with R*, §11.4.1 pp. 452-453, eq. 11.9) — PROVE: P(Y = 0) = E[e^{−Λ}] ≥ e^{−E Λ} by Jensen, equality iff Λ degenerate; ZIP moments — EMPIRICAL: NYC/TPS grid-cell zero shares vs e^{−μ̂}; dispersion test — EXTENDS: P7 (sharpens `poisson_zero_prob` to an inequality).

B24. **Vuong's test is misapplied to ZIP vs Poisson** (Friendly & Meyer §11.2 pp. 434-435, fn. 6) — PROVE: at π = 0 the models overlap, the pointwise log-likelihood ratios have zero variance under H₀ and the statistic is not N(0, 1) — EMPIRICAL: bootstrap the statistic on Poisson-simulated TPS-scale counts — EXTENDS: P7, P6.

B25. **Confidence sets for identification regions** (Manski §2.7 pp. 52-54; Imbens & Manski 2004) — PROVE: Prob[H(θ) ⊂ C] ≤ Prob[θ ∈ C]; the construction with δ_N → 0 and uniform coverage — EMPIRICAL: bootstrap the bounds in B1, B2, B10 on OTIS/CSUS — EXTENDS: P1, P5 (inference layer for every bound problem).

## C. Mathematics shelf (Grimmett & Stirzaker; D'Orsogna & Perc; Chung; Coles; Ghosal & van der Vaart; Wager; Puterman; MacKay; Cooter & Ulen; Lai; Luedtke & van der Laan)

Coverage note: the library has no Robbins–Monro, no Turing-instability text, no Rubin/Hill–Lane–Sudderth nonlinear-urn source; Hawkes appears only by name. Grimmett & Stirzaker (GS, 4th ed., PDF page = book page + 12) carries most of the probability.

C1. **The generalised Pólya urn share is a bounded martingale** (GS Problem 12.9.14 p. 574, Problem 12.9.13(a)) — PROVE: with adapted non-negative reinforcement A_n, the share Y_n is a martingale, so Y_n → Y_∞ a.s. and in mean (GS 12.3.1 p. 544); DISPROVE the mean-field claim Y_∞ ∈ {0, 1}: for A_n ≡ 1 the limit is Beta(r, b) — EMPIRICAL: TPS division shares over time; stabilisation vs runaway — EXTENDS: P4 (the stochastic version; first target).

C2. **Friedman urn: opposite-colour reinforcement forces the share to 1/2** (GS Exercise 12.3.6 p. 549) — PROVE: (B_n − R_n)(B_n + R_n − 1) is a martingale, does not converge, while the share tends to 1/2; pair with C1 as "same-colour reinforcement gives a random limit, cross-reinforcement a deterministic one" — EMPIRICAL: hot-spot vs rotating deployment — EXTENDS: P4.

C3. **Finite-n concentration of the urn share (Hoeffding–Azuma)** (GS Theorem 12.2.3 p. 538) — PROVE: with increments bounded by A/(r + b + n − 1), ΣK_i² < ∞ and the share never drifts more than O(1) with high probability: the stochastic analogue of the harmonic rate bound — EMPIRICAL: bootstrap drift envelope of TPS division shares — EXTENDS: P4.

C4. **Absorption probability of a finite-capacity allocation** (GS Example 12.5.8, Theorems 12.5.1 and 12.5.9 pp. 554-555) — PROVE: the de Moivre martingale gives p_k = ((q/p)^k − (q/p)^N)/(1 − (q/p)^N); optional stopping fails without its condition (c) — EXTENDS: P4.

C5. **Galton–Watson extinction equals Hawkes cluster finiteness** (GS Theorem 5.4.5, Lemma 5.4.2 pp. 193-195) — PROVE: extinction probability is the smallest root of s = G(s), equal to 1 iff μ ≤ 1 given σ² > 0; mean cluster size 1/(1 − n) for n < 1; counterexample μ = 1, σ² = 0 — EMPIRICAL: temporal Hawkes on TPS break-and-enter and shootings; n̂ and 1/(1 − n̂) vs declustered data — EXTENDS: new, feeds P7.

C6. **Quasi-stationary cluster size exists iff non-critical** (GS Corollary 6.7.7, Theorem 6.7.8 p. 275) — PROVE: Σ π_j = 1 iff μ ≠ 1; at μ = 1 the Yaglom limit Z_n/(nσ²) | Z_n > 0 → Exp(2) — EMPIRICAL: cluster-size tail of TPS shooting sequences — EXTENDS: C5.

C7. **The discrete attractiveness field is a (1 − ω)-contraction absent crime** (D'Orsogna & Perc Eq. 2 p. 3) — PROVE: with E ≡ 0 the update contracts in sup-norm and ℓ¹ and the spreading conserves ΣB; with feedback the linearised gain exceeds 1 for small A₀ — EMPIRICAL: fit ω, η to TPS repeat/near-repeat burglary intervals — EXTENDS: P4.

C8. **Hot-spot linear-stability threshold for the continuum model** (D'Orsogna & Perc Eqs. 4-5 p. 4; Short et al. 2010) — PROVE: unique homogeneous steady state (B̄ = εDγ/ω, ρ̄ = γ/Ā) and the dispersion relation σ(k) with an explicit instability inequality; DISPROVE "hot spots for all parameters" — EMPIRICAL: place estimated (ω, η) for Toronto burglary in the diagram — EXTENDS: new.

C9. **Independent thinning of a Poisson process is Poisson; the reporting rate is not identifiable** (GS Colouring Theorem 6.13.14, Conditional Property 6.13.11 pp. 323-324) — PROVE: the recorded process is Poisson with intensity γ(x)λ(x), independent of the unrecorded one, so (λ, γ) is not identified; counterexample to "Poisson recorded implies Poisson true": GS Problem 6.15.22(c) — EMPIRICAL: TPS vs GSS reporting rates by offence; Ripley K invariant only under constant thinning — EXTENDS: P2, P1.

C10. **Stein–Chen bound for the Poisson null with dependent counts** (GS Theorem 4.12.12, Example 4.12.14 p. 147) — PROVE: d_TV(S, Poi(λ)) ≤ 2(1 ∧ λ⁻¹)(λ − Var S) under a monotone coupling; instantiate for m crimes over n places — EMPIRICAL: TPS street-segment zero-count fraction vs the bound — EXTENDS: P7.

C11. **Chernoff/Cramér tail for the top-k share under the Poisson null** (GS Theorem 5.11.4 p. 226; Problem 5.11.5 p. 231) — PROVE: P(S > (1 + ε)μ) ≤ exp(−μ[(1 + ε) log(1 + ε) − ε]) and the large-deviation rate; counterexample: Cauchy summands — EMPIRICAL: observed top-5 percent share of TPS/NYC/CDE place counts against the bound — EXTENDS: P7.

C12. **Any intensity heterogeneity forces overdispersion** (GS Problem 6.15.22(b); Friendly & Meyer p. 83, p. 87) — PROVE: for a Cox process Var N(t) ≥ E N(t) with equality iff the intensity is a.s. constant; corollary: Gini above the Poisson null is implied by any random intensity, so the P7 decomposition must separate the mixing term — EMPIRICAL: dispersion index of TPS segment counts; NB vs Poisson — EXTENDS: P7.

C13. **Number of distinct places under Pólya allocation grows like M log n** (Ghosal & van der Vaart, *Fundamentals of Nonparametric Bayesian Inference*, Proposition 4.8 p. 66; ch. 14 for Pitman–Yor) — PROVE: E K_n ~ M log n ~ Var K_n, K_n/log n → M a.s., d_TV(K_n, Poi(E K_n)) = O(1/log n); polynomial growth under Pitman–Yor — EMPIRICAL: distinct TPS repeat addresses vs n; log vs power growth; species-sampling estimate of unseen victims — EXTENDS: P7, P1.

C14. **Cheeger inequality bounds spillover by hot-spot boundary** (Chung, *Four proofs of the Cheeger inequality*, Theorem 1 p. 6; GS Theorem 6.14.12 p. 334) — PROVE: 2h_G ≥ λ_G ≥ h_G²/2, the easy direction via the Rayleigh quotient; tightness only up to the square (path and cycle graphs) — EMPIRICAL: conductance of TPS hot-spot sets on the street graph vs measured displacement — EXTENDS: P3.

C15. **Random-walk exposure on a graph is proportional to degree** (GS Exercise 6.4.6 p. 264; 6.5.9 p. 268) — PROVE: π_v = d_v/(2η) on a connected undirected graph, reversible; fails on directed networks — EMPIRICAL: TPS counts vs OSM segment degree — EXTENDS: P3.

C16. **Exposure-mapping IPW is unbiased iff positivity; exact zeros under cluster designs** (Wager Thm 12.1 p. 153, Def. 11.1 p. 145, Thm 11.1 p. 147) — PROVE: unbiasedness under e_i(h), e_i(h') > 0; failure when "all neighbours treated" has probability p^{d_i} → 0 — EMPIRICAL: Neighbourhood Improvement Area designation with adjacency exposure — EXTENDS: P3 (already proved in `Research.P3.Design.ht_unbiased`; the zero-probability failure mode is the open half).

C17. **Extremal index equals the reciprocal mean cluster size for near-repeat exceedances** (Coles, *An Introduction to Statistical Modeling of Extreme Values*, Theorem 5.2 p. 96, eq. 5.4 p. 97, Def. 5.1 p. 93) — PROVE: θ = (a + 1)⁻¹ for the max-AR example; under D(u_n) maxima converge to G^θ; counterexample: lag-1 correlation near 1 with θ = 1 — EMPIRICAL: runs estimator of θ on daily TPS shootings vs burglary; compare with the Hawkes cluster size — EXTENDS: new.

C18. **GPD tail shape discriminates a power law from a mixed-Poisson place distribution** (Coles Theorem 4.1 p. 75, Theorem 3.1.1 p. 47) — PROVE: excesses of a Gamma-mixed Poisson have GPD limit with ξ = 0 while a Zipf(α) count has ξ = 1/α — EMPIRICAL: GPD fit to TPS/NYC segment counts, LR test of ξ = 0 — EXTENDS: P7.

C19. **Becker–Cooter deterrence comparative statics need convexity** (Cooter & Ulen, *Law and Economics*, ch. 12 pp. 465-468) — PROVE: with y concave and pf convex the optimal offending level is non-increasing in a certainty shift; DISPROVE monotonicity when pf is non-convex — EMPIRICAL: StatCan clearance rates and OTIS/CPADS sentence length vs offence counts — EXTENDS: P8.

C20. **Data-processing inequality caps what a risk score can know about true reoffending** (MacKay, *Information Theory, Inference, and Learning Algorithms*, Exercise 8.9 p. 141; Gibbs' inequality p. 34) — PROVE: I(W; R) ≤ I(W; D) for any post-processing; two groups with different D | W flip rates make calibration with respect to D and with respect to W incompatible — EMPIRICAL: CPADS/OTIS reconviction vs rearrest by group — EXTENDS: P5.

C21. **Bellman contraction and non-optimality of myopic hot-spot allocation** (Puterman, *Markov Decision Processes*, Theorem 6.2.3 p. 150, Prop. 6.2.4, Theorem 6.2.5 p. 151, Theorem 4.7.4 p. 106) — PROVE: the operator is a λ-contraction with the optimal value as fixed point; two-region counterexample in which greedy "allocate to highest predicted rate" is suboptimal because allocation suppresses observation — EMPIRICAL: simulate on TPS division rates — EXTENDS: P4, P8.

C22. **Resource-constrained optimal treatment rule is a CATE threshold, randomised at ties** (Luedtke & van der Laan 2016, Theorems 1-2) — PROVE: the quantile rule; a stochastic rule is needed when the CATE has an atom at the threshold — EMPIRICAL: CPADS/OTIS program allocation under caseload caps — EXTENDS: P5.

C23. **Lorden lower bound and CUSUM for hot-spot emergence** (Lai 1995 JRSS-B pp. 620-621, Lemma 1 p. 626, Lemma 2) — PROVE: Lemma 1 (a stopping rule restarted at every k satisfies E N ≥ 1/α when P(τ < ∞) ≤ α); state Lorden's worst-case delay bound and CUSUM attainment — EMPIRICAL: weekly TPS shooting counts by neighbourhood — EXTENDS: new.

C24. **Cyclic dominance in the criminal–citizen–inspector game has no stable interior equilibrium** (D'Orsogna & Perc Sec. IV p. 8, Fig. 8) — PROVE: a rock–paper–scissors payoff has a conserved quantity under replicator dynamics, so the interior fixed point is a centre — EMPIRICAL: FBI CDE clearance vs crime cycles by agency — EXTENDS: new, same object as A18.

## Priority queue (what gets a Lean file next)

1. C1 + C2 + C3 — the stochastic P4 (martingale share, Friedman contrast, Azuma drift envelope). Closes the "urn is not yet proved" gap in `R/feedback_loop.R`.
2. B23 + C12 — sharpen P7's Poisson null to an inequality (Jensen) and prove Cox overdispersion; both are short and change what `morie_concentration_decompose` reports.
3. B5 + B6 — Petersen/Chapman floor identities and the k-list non-identification; extends P1 to three sources.
4. A23 + A22 + A21 — the exposure-offset identity, the multiplicative benchmark identity and the arrest-probability premise; all three are the P2 machinery the TPS reports actually need.
5. A12 + A15 — reclassification algebra (column-stochastic recording); new P9 "crime counting as a linear map".
6. C5 + C6 + A17 — Hawkes branching criticality (Galton–Watson) as new P10 "near-repeat contagion".
7. B7 (SYG form) + C16 (zero-probability failure) — finish the P3 design layer.
8. B1 + B2 + B4 + B25 — Manski bounds as a P11 "sentencing effects as intervals" with the inference layer.
9. A1 + A2 + A5 — logit separation, LDA, KHB rescaling; a P12 on "what a coefficient means".
10. C8 + A16, B20 — reaction-diffusion threshold and the Bartlett non-identification; the spatial P13.

Empirical work is gated on data access already recorded in memory (TPS open data, OTIS extract in AZM_FINAL, StatCan PUMF guides in `~/work/corpus/userguides`).
