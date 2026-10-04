# The tables each module writes (generated from morie's MODULE_SPECS; the same file names in both packages).
# verify/inspect --module select by this list.
.morie_module_outputs <- list(
  "data-wrangling" = c(
    "data_na_summary.csv",
    "data_wrangling_log.csv"
  ),
  "descriptive-statistics" = c(
    "binomial_summaries.csv",
    "binomial_summaries_survey_weighted.csv",
    "probability_estimates.csv"
  ),
  "distribution-tests" = c(
    "distribution_tests.csv",
    "alcohol_correlation_matrix.csv",
    "clt_convergence.csv"
  ),
  "frequentist-inference" = c(
    "frequentist_heavy_drinking_prevalence_ci.csv",
    "frequentist_effect_sizes.csv",
    "frequentist_hypothesis_tests.csv"
  ),
  "bayesian-inference" = c(
    "bayesian_posterior_summaries.csv",
    "bayesian_bayes_factors.csv",
    "bayesian_vs_frequentist_ci.csv"
  ),
  "power-design" = c(
    "power_summary.csv",
    "power_two_proportion_gender.csv",
    "power_one_proportion_grid.csv",
    "power_ebac_endpoint_anchors.csv",
    "power_gpower_reference_two_group.csv",
    "power_interaction_assumptions.csv",
    "power_interaction_feasibility_flags.csv",
    "power_interaction_group_allocations.csv",
    "power_interaction_imbalance_penalty.csv",
    "power_interaction_pairwise_details.csv",
    "power_interaction_sample_size_targets.csv",
    "randomization_block_blueprints.csv",
    "randomization_schedule_example_heavy_drinking_30d.csv",
    "randomization_schedule_example_ebac_legal.csv",
    "randomization_schedule_example_ebac_tot.csv"
  ),
  "logistic-models" = c(
    "logistic_odds_ratios.csv",
    "logistic_interaction_odds_ratios.csv",
    "logistic_interaction_tests.csv",
    "logistic_smote_status.csv",
    "logistic_smote_odds_ratios.csv"
  ),
  "model-comparison" = c(
    "model_comparison_summary.csv",
    "model_comparison_full_coefs.csv",
    "model_comparison_interaction.csv",
    "model_comparison_wald_tests.csv"
  ),
  "regression-models" = c(
    "regression_coefficients.csv",
    "regression_model_comparison.csv"
  ),
  "propensity-scores" = c(
    "ipw_results.csv",
    "ipw_diagnostics.csv"
  ),
  "causal-estimators" = c(
    "causal_estimator_comparison.csv"
  ),
  "treatment-effects" = c(
    "treatment_effects_summary.csv",
    "cate_subgroup_estimates.csv"
  ),
  "dag-specification" = c(
    "official_doc_alignment_checklist.csv"
  ),
  "meta-synthesis" = c(
    "10_methods_results_paper.md",
    "11_interpretation.md"
  ),
  "ebac-core" = c(
    "ebac_data_quality_checks.csv",
    "ebac_distribution_unweighted.csv",
    "ebac_model_samples.csv",
    "ebac_weighted_summaries.csv",
    "ebac_missingness_weighted.csv",
    "ebac_missingness_or.csv",
    "ebac_missingness_or_eligible_drinkers.csv",
    "ebac_logistic_or_primary.csv",
    "ebac_linear_coefficients_primary.csv",
    "ebac_logistic_or_sensitivity_with_heavy.csv",
    "ebac_linear_coefficients_sensitivity_with_heavy.csv"
  ),
  "ebac-selection-adjustment-ipw" = c(
    "ebac_ipw_weight_diagnostics.csv",
    "ebac_ipw_logistic_or.csv",
    "ebac_ipw_linear_coefficients.csv",
    "ebac_ipw_cannabis_comparison.csv",
    "ebac_ipw_observation_model_or.csv",
    "ebac_ipw_covariate_balance.csv",
    "ebac_final_ipw_diagnostics.csv",
    "ebac_final_ipw_or.csv",
    "ebac_final_ipw_linear.csv",
    "ebac_final_ipw_comparison.csv"
  ),
  "ebac-integrations" = c(
    "ebac_final_domain_samples.csv",
    "ebac_final_formula_input_audit.csv",
    "ebac_final_formula_validation.csv",
    "ebac_final_interaction_tests.csv",
    "ebac_final_weighted_descriptives.csv",
    "ebac_final_weighted_linear.csv",
    "ebac_final_weighted_or.csv",
    "ebac_final_smote_compare.csv",
    "ebac_final_smote_or.csv",
    "ebac_final_smote_status.csv",
    "ebac_final_causal_effects.csv",
    "ebac_final_cate.csv",
    "ebac_final_consistency_checks.csv",
    "ebac_final_crosswalk_previous.csv",
    "ebac_final_dml_results.csv",
    "ebac_final_dml_status.csv",
    "ebac_final_key_summary.csv",
    "ebac_final_user_guide_variable_map.csv",
    "ebac_final_variable_audit.csv"
  ),
  "ebac-gender-smote-sensitivity" = c(
    "ebac_gender_interaction_svy_or.csv",
    "ebac_gender_interaction_tests.csv",
    "ebac_gender_marginal_probs.csv",
    "ebac_smote_status.csv",
    "ebac_smote_or.csv",
    "ebac_smote_compare.csv"
  ),
  "figures" = c(
    "figures/balance_plot.pdf",
    "figures/bayesian_prior_posterior.pdf",
    "figures/bayesian_prior_posterior.png",
    "figures/bayesian_vs_frequentist_ci.pdf",
    "figures/bayesian_vs_frequentist_ci.png",
    "figures/binge_by_demographics.pdf",
    "figures/binge_by_demographics.png",
    "figures/binge_by_mental_health.pdf",
    "figures/binge_by_mental_health.png",
    "figures/cate_forest_plot.pdf",
    "figures/cate_forest_plot.png",
    "figures/dag_heavy_drinking.pdf",
    "figures/qq_plots.pdf"
  ),
  "tables" = c(
    "table1.html"
  ),
  "final-report" = c(
    "ebac_final_output_coverage.csv",
    "ebac_final_output_shapes.csv",
    "ebac_final_script_run_status.csv",
    "ebac_final_audit_checks.csv",
    "ebac_final_user_guide_excerpt.txt"
  ),
  "otis-analysis" = c(
    "otis_descriptives.csv",
    "otis_alert_combos.csv",
    "otis_dml_results.csv",
    "otis_trends.csv"
  ),
  "mapq-psychometrics" = c(
    "mapq_reliability.csv",
    "mapq_factor_loadings.csv",
    "mapq_dml_results.csv"
  )
)

# One line per module (morie's ModuleSpec.description).
.morie_module_descriptions <- function() c(
  "data-wrangling" =
    "Canonicalize and validate the real CPADS PUMF input.",
  "descriptive-statistics" =
    "Survey-weighted prevalence and probability summaries.",
  "distribution-tests" =
    "Distributional diagnostics, correlations, and CLT checks.",
  "frequentist-inference" =
    "Frequentist prevalence, effect-size, and hypothesis-test outputs.",
  "bayesian-inference" =
    "Beta-binomial Bayesian summaries for key CPADS endpoints.",
  "power-design" =
    "Survey-weighted power planning summaries from real CPADS data.",
  "logistic-models" =
    "Survey-weighted logistic models for heavy drinking.",
  "model-comparison" =
    "Nested model comparison for heavy drinking models.",
  "regression-models" =
    "Weighted regression models for eBAC outcomes.",
  "propensity-scores" =
    "Propensity/IPW workflow for cannabis and heavy drinking.",
  "causal-estimators" =
    "Causal-estimator comparison across IPW, outcome-regression, and AIPW.",
  "treatment-effects" =
    "ATE/ATT/ATC and subgroup treatment-effect summaries.",
  "dag-specification" =
    "DAG and official-document alignment checklist outputs.",
  "meta-synthesis" =
    "Narrative synthesis outputs for study integration and interpretation.",
  "ebac-core" =
    "Core eBAC weighted, missingness, and model outputs.",
  "ebac-selection-adjustment-ipw" =
    "Selection-adjusted eBAC IPW workflow.",
  "ebac-integrations" =
    "Integrated eBAC final-summary outputs.",
  "ebac-gender-smote-sensitivity" =
    "eBAC interaction and SMOTE-sensitivity status outputs.",
  "figures" =
    "Figure exports for the documented analysis workflow.",
  "tables" =
    "HTML table exports for the documented analysis workflow.",
  "final-report" =
    "Final report and output-audit summaries.",
  "otis-analysis" =
    "OTIS restrictive-confinement analysis (descriptives, alert combos, cross-fitted DML, trends) on the bundled b01 sample.",
  "mapq-psychometrics" =
    "MAPQ psychometric validation (reliability, factor loadings) + DML (gender -> KS) on a synthetic MAPQII panel."
)
