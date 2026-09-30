# Vsrproofs

Lean 4 proofs of the identification theory used in

> *Alert Complexity and Placement Volatility in Ontario Restrictive
> Confinement Data* (OTIS A01-RCDD, 2023-2025)

the MRP (major research paper) whose estimators the `rmorie` MRM
functions implement. The paper itself is not here; only its mathematics.

`Vsrproofs.lean` holds 38 theorems and lemmas in namespace `VSR`, over a
finite population with known weights:

- the adjustment formula (`adjustment`) and inverse-probability weighting (`ipw`);
- double robustness of the augmented estimator (`aipw_propensity_correct`,
  `aipw_outcome_correct`);
- exact-matching covariate balance (`matching_balance`, `matching_card`);
- the log-link incidence-rate-ratio identity (`irr_log_link`) and
  negative-binomial overdispersion (`nb_var_gt_poisson`);
- the Lakner stock/flow identity and a sign-divergence witness instantiated
  at the paper's published counts (`lakner_identity`,
  `lakner_sign_divergence`);
- the confounding-factor bound (`confounding_factor`, `confounding_factor_otis`);
- Manski's worst-case bound (`manski_upper`);
- counterexamples showing what fails when positivity or ignorability does
  not hold (`counterexample_*`).

What is not proved, and cannot be: that positivity and conditional mean
ignorability hold of the OTIS data. Lean checks the implication, never the
antecedent.

## Building

Lean `v4.34.0` with Mathlib (pinned in `lakefile.toml`):

```sh
lake build
lake env lean Audit.lean    # prints the axioms of every theorem
```

The criminology open-problems proofs live in the parent project `..`.
