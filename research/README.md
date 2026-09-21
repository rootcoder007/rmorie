# research/ — the hardest problems in criminology, with proofs

This directory holds the research programme started 2026-09-21 on a
separate clone of rmorie. Nothing here is merged into rmorie main until
it has passed the same gates as the rest of the package and has been
signed off.

- `LEDGER.md`: the eight problems, each with estimand, identifying
  assumptions, what must be proved, what "cracked" means, sources, and a
  progress log.
- `lean/`: the Lean 4 sources (Mathlib, project `~/work/researchproofs`
  on l14). `Audit.lean` prints the axioms of every theorem; all depend on
  `propext`, `Classical.choice`, `Quot.sound` only, and no file contains
  `sorry`.

## Theorem index: which function rests on which proof

| Problem | Lean theorem | R function(s) in `R/` |
|---|---|---|
| P1 dark figure | `Research.P1.TwoSource.petersen_identity`, `lincoln_petersen`, `petersen_bounds`, `petersen_lower_attained`, `petersen_upper_attained` | `morie_dark_figure_two_source()` |
| P1 dark figure | `Research.P1.true_rate_bounds`, `dark_figure_bounds` | `morie_dark_figure_bounds()` |
| P1 dark figure | `Research.P1.conclusion_holds_below_breakdown`, `conclusion_fails_above_breakdown` | `morie_dark_figure_breakdown()` |
| P1 dark figure | `Research.P1.petersen_ge_floor`, `chapman_ge_floor`, `three_list_saturated_fits`, `missing_cell_unconstrained` | `morie_dark_figure_three_list()` |
| P2 selection | `Research.P2.rate_bounds`, `rate_lower_attained`, `rate_upper_attained`, `disparity_bounds`, `disparity_sign_identified` | `morie_disparity_exposure_bounds()` |
| P2 selection | `Research.P2.offset_shift`, `disparity_ratio_shift`, `benchmark_product`, `benchmark_not_additive` | `morie_disparity_benchmark()` |
| P3 interference | `Research.P3.Model.exposure_adjustment`, `direct_spillover_decomposition`, `pooled_mean_mixture`, `misspecified_exposure_bias` | `morie_spillover_exposure()`, `morie_spillover_effects()` |
| P3 interference | `Research.P3.Design.ht_second_moment`, `ht_variance`, `ht_variance_estimator_unbiased` | `morie_spillover_ht(joint = ...)`, `morie_spillover_ht_variance()`, `morie_spillover_exposure_probs(joint = TRUE)` |
| P3 interference | `Research.P3.SYG.row_sum_zero`, `syg_eq_ht`, `syg_zero_of_const` | `morie_spillover_ht_variance(form = "both")` |
| P4 feedback loops | `Research.P4.naive_step_drift`, `naive_share_increasing`, `naiveShare_strictMono`, `naiveShare_tendsto_one`, `not_summable_shifted_harmonic` | `morie_feedback_loop_meanfield()`, `morie_feedback_loop_limit()`, `morie_feedback_loop_sim()` |
| P4 feedback loops | `Research.P4.corrected_share_closed_form`, `corrected_share_tendsto` | same, `update = "corrected"` |
| P4 feedback loops | `Research.P4.naiveTotal_ge`, `naiveShare_gain_le`, `naiveShare_rate_bound` | `morie_feedback_loop_bound()` |
| P4 feedback loops | `Research.P4.inc_share_le_cap`, `rhoStep_share_le`, `rho_cap`, `cap_zero`, `cap_one` | `morie_feedback_loop_limit(rho = ...)$cap`, `morie_feedback_loop_meanfield(rho = ...)` |
| P4 feedback loops | `Research.P4.Urn.urn_step_martingale`, `polya_uniform`, `polya_no_concentration` | `morie_feedback_loop_urn_law()`, `morie_feedback_loop_limit()` (equal rates) |
| P5 fairness | `Research.P5.Table.chouldechova`, `impossibility` | `morie_fairness_rates()`, `morie_fairness_implied_fpr()` |
| P5 fairness | `Research.P5.true_base_rate_bounds`, `lower_bound_attained`, `upper_bound_attained`, `trueRate_observedRate` | `morie_fairness_base_rate_bounds()`, `morie_fairness_true_rate()` |
| P5 fairness | `Research.P5.compare_decided`, `compare_undecided` | `morie_fairness_compare_groups()` |
| P6 age-crime | `Research.P6.aggregate_not_identifying`, `invariance_sufficient`, `invariance_not_necessary` | `morie_age_crime_aggregate()` |
| P7 concentration | `Research.P7.gini_zero_decomposition`, `poisson_zero_prob` | `morie_concentration_gini()`, `morie_concentration_decompose()` |
| P7 concentration | `Research.P7.Mixture.mixture_zero_ge_exp_neg_mean`, `variance_eq`, `mixture_var_ge_mean`, `mixture_var_eq_mean_iff` | `morie_concentration_dispersion()`, `morie_concentration_decompose()` (`null_zero_share` is a lower bound) |
| P7 concentration | `Research.P7.log_le_S`, `S_le_log`, `expectedDistinct_bounds` | `morie_concentration_distinct_growth()` |
| P8 deterrence | `Research.P8.constant_dimension_not_identified`, `identified_iff_injective`, `corner_design_identified` | `morie_deterrence_design_check()` |
| P9 recording map | `Research.P9.total_invariant_of_colStochastic`, `total_le_of_colSubstochastic`, `reclassification_moves_ratio`, `detection_rate_rises` | `morie_recording_map()`, `morie_detection_rate_shift()` |
| P10 contagion | `Research.P10.generation_mean`, `cluster_size_of_lt_one`, `stationary_rate`, `cluster_size_diverges_of_ge_one`, `endogeneity_share` | `morie_contagion_branching()` |
| P11 sentencing bounds | `Research.P11.Pop.outcome_bounds`, `lower_attained`, `upper_attained`, `ate_width_one`, `ate_contains_zero` | `morie_sentence_effect_bounds()` |
| P12 ecological | `Research.P12.within_orth`, `cov_decomp`, `var_decomp`, `var_nonneg`, `ecological_ge` | `morie_ecological_decompose()` |
| P11 sentencing bounds | `Research.P11.clean_bounds`, `clean_lower_attained`, `clean_upper_attained`, `clean_width`, `clean_informative` | `morie_contaminated_bounds()` |
| P11 sentencing bounds | `Research.P11.Pop.mtr_lower`, `mtr_upper`, `mtr_upper_attained` | `morie_sentence_effect_mtr()` |

C++ kernels: `src/morie_feedback_loop.cpp` (two-region urn),
`src/morie_spillover.cpp` (treated-neighbour counts on a place network).

## The honesty rule

Lean certifies the implication, never the antecedent. Every function's
help page says which theorem covers which step and where the empirical
assumption (independence of two lists, the noise boxes, the exposure
ring, the discovery model) enters. A proof here never shows that an
assumption holds in Toronto, Ontario, or anywhere else.

## Reproducing the checks

```
cd ~/work/researchproofs && lake build && lake env lean Audit.lean
cd ~/work/rmorie-research && Rscript -e 'testthat::test_dir("tests/testthat", filter = "feedback-loop|fairness-bounds|dark-figure|spillover|concentration|research-p268")'
```
