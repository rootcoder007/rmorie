# SPDX-License-Identifier: AGPL-3.0-or-later
# Generated from morie's explain registry (src/morie/explain.py, _explain_tables.py): the same
# text in both packages, with `morie VERB` written `rmorie VERB`. Regenerate; do not hand-edit.

#' Internal helper: the long explanations of the most-read module tables
#' @noRd
.morie_explanations <- function() {
  list(
    "power_summary.csv" = paste0(
      "Question this file answers: \"What sample did the power analysis run on,\nand what anchors did it use?\"\n\nIt's a long-format table with two columns \u2014 `me",
      "tric` and `value` \u2014 one\nrow per quantity.  Rows include:\n  - analysis_n                            Records in the analysis sample\n  - heavy_drinking_p",
      "revalence_weighted    Weighted outcome prevalence\n  - gender_levels_used                    Gender strata in the design\n  - (plus eBAC endpoint anchor",
      " rows when those measures are present)\n\nThe actual power/sample-size grids live in the companion files:\n`power_two_proportion_gender.csv` (two-proport",
      "ion grid) and\n`power_one_proportion_grid.csv` (one-proportion grid)."
    ),
    "power_two_proportion_gender.csv" = paste0(
      "Question this file answers: \"How many participants per gender group\ndo I need to detect an effect of size X?\"\n\nRead each row: it compares two groups (",
      "group1 vs group2) as observed in the\ndata; p1 and p2 are their weighted prevalences of the outcome and h is the\ngap between them as an effect size.\n\nC",
      "olumns (both routes write all of them; one row per group pair and outcome):\n  - group1, group2       The two groups compared (e.g. men vs women)\n  - p",
      "1, p2               The weighted outcome prevalence observed in each group\n  - h                    Cohen's h for the two proportions\n  - n1, n2      ",
      "         The observed group sizes in the data (not sample sizes needed)\n  - n_eq                 Per-group n needed for 80% power at this h, equal all",
      "ocation,\n                         simple random sampling: 2((z_a + z_b) / h)^2\n  - power_srs            Power the observed n1, n2 give under simple ra",
      "ndom sampling\n  - n_eq_eff             n_eq inflated by Kish's design effect of the two groups' weights\n  - power_deff           Power the observed si",
      "zes give once the design effect is applied\n  - analysis_mode        \"observational\": the groups are as surveyed, not assigned\n  - power_scope         ",
      " The outcome the comparison is about (e.g. heavy_drinking_30d)\n\nTypical usage: read n_eq_eff (the design-adjusted per-group n needed) and double\nit fo",
      "r the total sample; power_srs / power_deff say what the survey as it stands\ncan detect."
    ),
    "power_one_proportion_grid.csv" = paste0(
      "Same idea as power_two_proportion_gender.csv, but for a single-proportion\ndesign (one group, no comparison): \"What's the smallest deviation from\na nul",
      "l proportion p0 I can detect with n participants at alpha=0.05?\"\n\nPick a row by n; read effect_size for the smallest detectable difference."
    ),
    "power_ebac_endpoint_anchors.csv" = paste0(
      "Power-analysis grid specific to alcohol-impairment endpoints (eBAC =\nestimated blood alcohol concentration).  Each row is one anchor point:\n\"if your e",
      "ndpoint is ebac_tot > 0.08 vs <= 0.08, what's the sample size?\""
    ),
    "power_gpower_reference_two_group.csv" = paste0(
      "Cross-reference: matches morie's two-group power numbers to G*Power\n(the gold-standard reference tool).  If you're submitting to a journal\nthat demand",
      "s G*Power, this is the table to cite."
    ),
    "power_interaction_assumptions.csv" = paste0(
      "Assumptions used by the interaction-effect (e.g. gender \u00d7 age) power\ncalculation.  Read this if you want to know what the interaction model\nassumed be",
      "fore trusting power_interaction_pairwise_details.csv."
    ),
    "power_interaction_feasibility_flags.csv" = "Flags for whether each proposed interaction cell is feasible at the\ntarget sample size.  TRUE = enough data expected, FALSE = under-powered.",
    "power_interaction_group_allocations.csv" = "How the total sample is split across interaction cells (e.g. men 18-24,\nmen 25-44, women 18-24, women 25-44).  Tells you the per-cell n.",
    "power_interaction_imbalance_penalty.csv" = paste0(
      "The penalty to power introduced by unequal cell sizes.  If allocations\nin power_interaction_group_allocations are skewed, this quantifies how\nmuch pow",
      "er you lose vs. a balanced design."
    ),
    "power_interaction_pairwise_details.csv" = "The detail backing power_interaction_assumptions.  Pairwise effect sizes\nfor each combination of interaction levels.",
    "power_interaction_sample_size_targets.csv" = "The sample-size *targets* (per cell) to hit your desired power for each\ninteraction comparison.  Compare to your actual allocations file.",
    "randomization_block_blueprints.csv" = paste0(
      "Pre-baked randomization-scheme blueprints (block sizes, stratification\nfactors).  Pick one and the *_example CSVs show what the resulting\nallocation l",
      "ooks like."
    ),
    "randomization_schedule_example_heavy_drinking_30d.csv" = paste0(
      "A *worked-example* randomization schedule using heavy-drinking-30-day as\nthe stratifying outcome.  Shows the participant id \u2192 arm assignment\ntable; us",
      "eful for replicating in your own survey software."
    ),
    "randomization_schedule_example_ebac_legal.csv" = "Worked-example randomization schedule stratified by ebac_legal (the\nlegal-limit blood-alcohol-concentration endpoint).",
    "randomization_schedule_example_ebac_tot.csv" = "Worked-example randomization schedule stratified by ebac_tot (the\ntotal-impairment blood-alcohol-concentration endpoint).",
    "data_na_summary.csv" = paste0(
      "Per-column missingness summary.  Each row is one input column; columns\ninclude n_missing, pct_missing.  Read this BEFORE running any inference\nmodule ",
      "\u2014 fields with high missingness need imputation or exclusion."
    ),
    "data_wrangling_log.csv" = "Step-by-step log of what the data-wrangling module did to your input\n(renames, coercions, dropped rows).  Useful for the methods section.",
    "binomial_summaries.csv" = paste0(
      "Unweighted binomial summaries (e.g. heavy_drinking_30d prevalence): plain\nsample proportions with Wilson intervals, no survey weights applied. Compare",
      "\nagainst binomial_summaries_survey_weighted to see how much the design\nweights shift the estimates."
    ),
    "binomial_summaries_survey_weighted.csv" = "Survey-weighted binomial summaries WITH the CPADS weighting variable\napplied.  These are the prevalence estimates you'd report in a paper.",
    "probability_estimates.csv" = "Joint and conditional probability estimates across the survey design.\nRead column by column; row labels indicate the conditioning event.",
    "frequentist_heavy_drinking_prevalence_ci.csv" = paste0(
      "Frequentist (Wilson / Clopper-Pearson) confidence intervals for the\nprevalence of heavy drinking.  Each row is one subgroup; columns are\nestimate, ci_",
      "lower, ci_upper."
    ),
    "frequentist_effect_sizes.csv" = paste0(
      "Cohen's-d / odds-ratio / risk-difference effect sizes for the primary\ncontrasts of the analysis.  Read alongside p-values from\nfrequentist_hypothesis_",
      "tests.csv."
    ),
    "frequentist_hypothesis_tests.csv" = paste0(
      "Per-contrast p-values and test statistics.  CAUTION: these are\nNOT corrected for multiple comparisons by default \u2014 apply\nBonferroni / Benjamini-Hochbe",
      "rg yourself if your design demands it."
    )
  )
}

#' Internal helper: one purpose line for every module output table
#' @noRd
.morie_explain_purpose <- function() {
  list(
    "distribution_tests.csv" = "Distributional checks of the key measures (normality tests and summary shape) before any model assumes a form.",
    "alcohol_correlation_matrix.csv" = "Pairwise correlations between the alcohol measures.",
    "clt_convergence.csv" = "How fast sample means settle as the sample grows: the observed spread of means against the CLT standard error.",
    "bayesian_posterior_summaries.csv" = "Beta-binomial posterior for each endpoint's prevalence: prior, posterior parameters, mean and credible interval.",
    "bayesian_bayes_factors.csv" = "Bayes factors (BF10) comparing the hypotheses for each endpoint; above 1 favours H1.",
    "bayesian_vs_frequentist_ci.csv" = "Each endpoint's Bayesian credible interval beside its frequentist confidence interval.",
    "logistic_odds_ratios.csv" = "Survey-weighted logistic model of heavy drinking: one odds ratio per term (categories against their reference level).",
    "logistic_interaction_odds_ratios.csv" = "The same model with a cannabis x gender interaction: odds ratios including the interaction terms.",
    "logistic_interaction_tests.csv" = "Joint Wald test of the cannabis x gender interaction terms.",
    "logistic_smote_status.csv" = "Whether the SMOTE sensitivity check ran (it is skipped on an already balanced outcome) and the class counts before/after.",
    "logistic_smote_odds_ratios.csv" = "Odds ratios refitted on SMOTE-rebalanced data, to compare with the survey-weighted ones (empty when SMOTE was skipped).",
    "model_comparison_summary.csv" = "Nested heavy-drinking models side by side: fit statistics (deviance, AIC, pseudo R2) as terms are added.",
    "model_comparison_full_coefs.csv" = "Every coefficient of every nested model, for tracing how estimates move as covariates enter.",
    "model_comparison_interaction.csv" = "Coefficients of the model with the cannabis x gender interaction.",
    "model_comparison_wald_tests.csv" = "Wald tests for the blocks of terms each nested model adds.",
    "regression_coefficients.csv" = "Survey-weighted regression of the eBAC outcomes: coefficients, standard errors and intervals.",
    "regression_model_comparison.csv" = "Fit of the competing eBAC regression specifications.",
    "ipw_results.csv" = "Inverse-probability-weighted effect of cannabis use on heavy drinking, with the unweighted contrast for comparison.",
    "ipw_diagnostics.csv" = "Weight diagnostics for the IPW fit: weight range and effective sample size; extreme weights flag poor overlap.",
    "causal_estimator_comparison.csv" = "The same effect estimated by IPW, outcome regression and AIPW (doubly robust) side by side.",
    "treatment_effects_summary.csv" = "Average effect of cannabis use on heavy drinking: ATE, ATT and ATC with intervals.",
    "cate_subgroup_estimates.csv" = "Conditional (subgroup) treatment effects: the effect within each level of a grouping variable.",
    "official_doc_alignment_checklist.csv" = "Checklist mapping each design requirement from the official documentation to where the analysis meets it.",
    "ebac_data_quality_checks.csv" = "Data-quality checks on the eBAC inputs (valid ranges, missing values) with pass/fail.",
    "ebac_distribution_unweighted.csv" = "Unweighted distribution of eBAC: quantiles, mean and spread.",
    "ebac_model_samples.csv" = "Size of each analysis sample the eBAC models use, after their exclusions.",
    "ebac_weighted_summaries.csv" = "Survey-weighted eBAC summaries by group (means and prevalence of the legal-limit flag).",
    "ebac_missingness_weighted.csv" = "Weighted share of respondents with a missing eBAC, by group.",
    "ebac_missingness_or.csv" = "Logistic model of eBAC missingness: which characteristics predict a missing eBAC (odds ratios).",
    "ebac_missingness_or_eligible_drinkers.csv" = "The missingness model restricted to eligible drinkers.",
    "ebac_logistic_or_primary.csv" = "Primary logistic model of the eBAC legal-limit flag: odds ratios.",
    "ebac_linear_coefficients_primary.csv" = "Primary linear model of continuous eBAC: coefficients.",
    "ebac_logistic_or_sensitivity_with_heavy.csv" = "Sensitivity version of the logistic eBAC model adding heavy drinking as a covariate.",
    "ebac_linear_coefficients_sensitivity_with_heavy.csv" = "Sensitivity version of the linear eBAC model adding heavy drinking as a covariate.",
    "ebac_ipw_weight_diagnostics.csv" = "Diagnostics of the selection weights for having an observed eBAC (range, effective sample size).",
    "ebac_ipw_logistic_or.csv" = "Selection-weighted logistic eBAC model: odds ratios corrected for who reports eBAC.",
    "ebac_ipw_linear_coefficients.csv" = "Selection-weighted linear eBAC model: coefficients.",
    "ebac_ipw_cannabis_comparison.csv" = "The cannabis coefficient with and without the selection weights.",
    "ebac_ipw_observation_model_or.csv" = "The observation (selection) model: odds of having an observed eBAC.",
    "ebac_ipw_covariate_balance.csv" = "Covariate balance before and after weighting: standardized mean differences (|SMD| < 0.1 is the usual target).",
    "ebac_final_ipw_diagnostics.csv" = "Final-report copy of the selection-weight diagnostics.",
    "ebac_final_ipw_or.csv" = "Final-report copy of the selection-weighted odds ratios.",
    "ebac_final_ipw_linear.csv" = "Final-report copy of the selection-weighted linear coefficients.",
    "ebac_final_ipw_comparison.csv" = "Final-report comparison of weighted and unweighted eBAC estimates.",
    "ebac_final_domain_samples.csv" = "Sample size of each analysis domain in the integrated eBAC report.",
    "ebac_final_formula_input_audit.csv" = "Audit of the inputs to the eBAC formula (drinks, weight, hours, sex): presence and valid ranges.",
    "ebac_final_formula_validation.csv" = "eBAC recomputed by the Widmark formula against the stored eBAC, row checks summarised.",
    "ebac_final_interaction_tests.csv" = "Interaction tests of the integrated eBAC models.",
    "ebac_final_weighted_descriptives.csv" = "Weighted eBAC descriptives for the integrated report.",
    "ebac_final_weighted_linear.csv" = "Weighted linear eBAC model for the integrated report.",
    "ebac_final_weighted_or.csv" = "Weighted logistic eBAC model (odds ratios) for the integrated report.",
    "ebac_final_smote_compare.csv" = "Integrated report: odds ratios with and without SMOTE rebalancing.",
    "ebac_final_smote_or.csv" = "Integrated report: SMOTE-refitted odds ratios.",
    "ebac_final_smote_status.csv" = "Integrated report: whether SMOTE ran and the class counts.",
    "ebac_final_causal_effects.csv" = "Integrated report: causal effect estimates of cannabis on eBAC outcomes.",
    "ebac_final_cate.csv" = "Integrated report: subgroup (conditional) effects on eBAC.",
    "ebac_final_consistency_checks.csv" = "Consistency checks between the integrated tables (the same quantity agrees across them).",
    "ebac_final_crosswalk_previous.csv" = "Crosswalk of these results against the previous run's.",
    "ebac_final_dml_results.csv" = "Cross-fitted double machine learning estimate of the cannabis effect on eBAC.",
    "ebac_final_dml_status.csv" = "Whether the DML stage ran and with which learners.",
    "ebac_final_key_summary.csv" = "One-page summary of the headline eBAC numbers.",
    "ebac_final_user_guide_variable_map.csv" = "Map from each analysis variable to its description in the survey user guide.",
    "ebac_final_variable_audit.csv" = "Audit of every analysis variable: present in the wrangled data and coded as documented.",
    "ebac_gender_interaction_svy_or.csv" = "Survey-weighted eBAC model with a gender interaction: odds ratios.",
    "ebac_gender_interaction_tests.csv" = "Tests of the gender interaction terms.",
    "ebac_gender_marginal_probs.csv" = "Predicted probability of exceeding the legal limit by gender (marginal probabilities).",
    "ebac_smote_status.csv" = "Whether the eBAC SMOTE sensitivity ran and the class counts before/after.",
    "ebac_smote_or.csv" = "eBAC odds ratios refitted on SMOTE-rebalanced data.",
    "ebac_smote_compare.csv" = "eBAC odds ratios with and without SMOTE, side by side.",
    "ebac_final_output_coverage.csv" = "Which expected output files exist after the run.",
    "ebac_final_output_shapes.csv" = "Row and column counts of each output file.",
    "ebac_final_script_run_status.csv" = "Run status of each analysis step (completed, warnings).",
    "ebac_final_audit_checks.csv" = "Final audit checks with pass/fail.",
    "otis_descriptives.csv" = "Descriptive counts from the OTIS restrictive-confinement data (placements, individuals, days).",
    "otis_alert_combos.csv" = "How often each combination of mental-health/suicide alerts occurs among placements.",
    "otis_dml_results.csv" = "Cross-fitted double machine learning estimate of an alert's effect on confinement duration.",
    "otis_trends.csv" = "Counts by fiscal year: the trend over time.",
    "mapq_reliability.csv" = "Reliability of each MAPQ subscale and the total: Cronbach's alpha and McDonald's omega.",
    "mapq_factor_loadings.csv" = "Factor loadings of the 20 MAPQ items on the four factors, with each item's assigned subscale.",
    "mapq_dml_results.csv" = "Cross-fitted DML estimate of the gender effect on the Knowledge Scale score."
  )
}

#' Internal helper: what the columns the module tables share mean
#' @noRd
.morie_explain_glossary <- function() {
  list(
    "term" = "the model term (a category appears as its level against the reference)",
    "estimate" = "the point estimate",
    "coef" = "the coefficient",
    "log_odds" = "the coefficient on the log-odds scale",
    "se" = "standard error of the estimate",
    "std.error" = "standard error of the estimate",
    "SE" = "standard error of the estimate",
    "statistic" = "the test statistic",
    "t_value" = "the t statistic (estimate / SE)",
    "F_stat" = "the F statistic",
    "F_statistic" = "the F statistic",
    "df_num" = "numerator degrees of freedom",
    "df_den" = "denominator degrees of freedom",
    "df_denom" = "denominator degrees of freedom",
    "df_residual" = "residual degrees of freedom",
    "p_value" = "p-value (not corrected for multiple comparisons)",
    "p.value" = "p-value (not corrected for multiple comparisons)",
    "significant" = "* when p < 0.05",
    "OR" = "odds ratio = exp(coefficient); above 1 raises the odds",
    "or" = "odds ratio = exp(coefficient); above 1 raises the odds",
    "OR_lower95" = "lower bound of the 95% confidence interval of the odds ratio",
    "OR_upper95" = "upper bound of the 95% confidence interval of the odds ratio",
    "or_lower95" = "lower bound of the 95% confidence interval of the odds ratio",
    "or_upper95" = "upper bound of the 95% confidence interval of the odds ratio",
    "OR_lower" = "lower bound of the odds ratio's interval",
    "OR_upper" = "upper bound of the odds ratio's interval",
    "OR_original" = "odds ratio on the original data",
    "OR_smote" = "odds ratio on the SMOTE-rebalanced data",
    "p_original" = "p-value on the original data",
    "p_smote" = "p-value on the SMOTE-rebalanced data",
    "ci_lower" = "lower bound of the 95% interval",
    "ci_upper" = "upper bound of the 95% interval",
    "ci_lower95" = "lower bound of the 95% interval",
    "ci_upper95" = "upper bound of the 95% interval",
    "conf.low" = "lower bound of the 95% interval",
    "conf.high" = "upper bound of the 95% interval",
    "ci_width" = "width of the interval",
    "ate" = "average treatment effect",
    "cate" = "conditional (subgroup) average treatment effect",
    "estimand" = "which effect the row estimates (ATE, ATT, ATC)",
    "n" = "number of records",
    "n_total" = "number of records in total",
    "n_treated" = "number of treated records",
    "n_control" = "number of control records",
    "n_nonmissing" = "records with a value",
    "sample_size" = "records in the sample",
    "weight" = "survey weight",
    "mean" = "mean",
    "median" = "median",
    "sd" = "standard deviation",
    "min" = "minimum",
    "max" = "maximum",
    "p25" = "25th percentile",
    "p75" = "75th percentile",
    "p90" = "90th percentile",
    "p95" = "95th percentile",
    "prevalence" = "share of records with the outcome",
    "method" = "how the row was computed",
    "model" = "which model the row comes from",
    "outcome" = "the outcome variable",
    "treatment" = "the treatment variable",
    "predictor" = "the predictor variable",
    "variable" = "the variable",
    "covariate" = "the covariate",
    "group" = "the group",
    "subgroup_var" = "the variable that defines the subgroups",
    "subgroup_level" = "the subgroup's level",
    "smd_raw" = "standardized mean difference before weighting",
    "smd_ipw" = "standardized mean difference after weighting (|SMD| < 0.1 is the usual balance target)",
    "deviance" = "model deviance (lower fits better on the same data)",
    "AIC_approx" = "approximate AIC (lower is better)",
    "pseudo_R2" = "McFadden pseudo R-squared",
    "bf10" = "Bayes factor for H1 over H0 (> 1 favours H1)",
    "alpha_prior" = "Beta prior alpha",
    "beta_prior" = "Beta prior beta",
    "alpha_post" = "Beta posterior alpha",
    "beta_post" = "Beta posterior beta",
    "post_mean" = "posterior mean",
    "post_sd" = "posterior standard deviation",
    "alpha_raw" = "Cronbach's alpha from the raw items",
    "alpha_std" = "Cronbach's alpha from the standardized items",
    "alpha_ci_low" = "lower bound of alpha's interval",
    "alpha_ci_high" = "upper bound of alpha's interval",
    "omega_total" = "McDonald's omega total",
    "omega_hier" = "omega hierarchical (the general factor's share, Schmid-Leiman)",
    "splithalf_sb" = "split-half reliability, Spearman-Brown corrected",
    "n_items" = "items in the scale",
    "status" = "run status",
    "check" = "the check",
    "check_name" = "the check",
    "pass" = "TRUE when the check passed",
    "note" = "a note on the row",
    "metric" = "the quantity named on the row",
    "value" = "its value",
    "power" = "statistical power (probability of detecting the effect)",
    "marginal_prob" = "predicted probability averaged over the sample"
  )
}

#' Internal helper: purpose of a module table plus the columns its file really has; NULL if unknown
#' @noRd
.morie_explain_table <- function(name, path = NULL) {
  purpose <- .morie_explain_purpose()[[name]]
  if (is.null(purpose)) return(NULL)
  lines <- purpose
  if (!is.null(path) && file.exists(path) && !dir.exists(path)) {
    header <- tryCatch(names(utils::read.csv(path, nrows = 1L, check.names = FALSE)), error = function(e) character())
    if (length(header)) {
      g <- .morie_explain_glossary()
      w <- max(nchar(header))
      lines <- c(lines, "", "Columns:", vapply(header, function(col) {
        sprintf("  %-*s  %s", w, col, g[[col]] %||% "(module-specific; see the module documentation)")
      }, ""))
    }
  } else {
    lines <- c(lines, "", "Run `rmorie explain` on the file itself to see its columns explained.")
  }
  paste(lines, collapse = "\n")
}
