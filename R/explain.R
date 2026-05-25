# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie - Multi-domain Open Research and Inferential Estimation
# Copyright (C) 2026 Vansh Singh Ruhela and morie contributors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.

# ---------------------------------------------------------------------------
# morie::explain - human-readable descriptions of module-output CSVs
# ---------------------------------------------------------------------------
# R port of src/morie/explain.py.  Backs `morie::explain_file()` and
# `morie::cheatsheet()` (R analogues of the `morie explain` and
# `morie cheatsheet` CLI subcommands).  Explanations target a user
# who just ran a morie module and is staring at 10-15 CSVs not
# knowing where to start.
#
# Adding a new module's output files means appending an entry to
# `.morie_explanations()`.

.morie_explanations <- function() {
  list(
    # --- power-design outputs ----------------------------------------
    "power_summary.csv" = paste0(
"Question this file answers: \"What were the design assumptions for my\
",
"power analysis, and what's the recommended sample size at a glance?\"\
",
"\
",
"It's a one-row summary.  Read across; columns include:\
",
"  - effect_size          The minimum effect you said you want to detect\
",
"  - alpha                False-positive rate (typically 0.05)\
",
"  - power                True-positive rate (typically 0.80)\
",
"  - n_recommended        Per-group sample size to hit (power, effect) at alpha\
",
"\
",
"If you want detail beyond the summary row, see the companion files\
",
"listed in `power_two_proportion_gender.csv` (two-proportion grid) and\
",
"`power_one_proportion_grid.csv` (one-proportion grid)."),

    "power_two_proportion_gender.csv" = paste0(
"Question this file answers: \"How many participants per gender group\
",
"do I need to detect an effect of size X?\"\
",
"\
",
"Read each row: pick the effect_size you want to detect; the row tells\
",
"you the per-group sample size needed.\
",
"\
",
"Columns:\
",
"  - effect_size          The difference in proportions you want to detect\
",
"                         (e.g. 0.05 = a 5 percentage-point gap between\
",
"                          men and women)\
",
"  - n_per_group          Number of participants per group required\
",
"  - power                Achieved power at this n (should match your design)\
",
"  - assumed_baseline     The baseline proportion the calculation uses\
",
"\
",
"Typical usage: scroll to the row matching your hypothesised effect, read\
",
"n_per_group, double it for total sample."),

    "power_one_proportion_grid.csv" = paste0(
"Same idea as power_two_proportion_gender.csv, but for a single-proportion\
",
"design (one group, no comparison): \"What's the smallest deviation from\
",
"a null proportion p0 I can detect with n participants at alpha=0.05?\"\
",
"\
",
"Pick a row by n; read effect_size for the smallest detectable difference."),

    "power_ebac_endpoint_anchors.csv" = paste0(
"Power-analysis grid specific to alcohol-impairment endpoints (eBAC =\
",
"estimated blood alcohol concentration).  Each row is one anchor point:\
",
"\"if your endpoint is ebac_tot > 0.08 vs <= 0.08, what's the sample size?\""),

    "power_gpower_reference_two_group.csv" = paste0(
"Cross-reference: matches morie's two-group power numbers to G*Power\
",
"(the gold-standard reference tool).  If you're submitting to a journal\
",
"that demands G*Power, this is the table to cite."),

    "power_interaction_assumptions.csv" = paste0(
"Assumptions used by the interaction-effect (e.g. gender x age) power\
",
"calculation.  Read this if you want to know what the interaction model\
",
"assumed before trusting power_interaction_pairwise_details.csv."),

    "power_interaction_feasibility_flags.csv" = paste0(
"Flags for whether each proposed interaction cell is feasible at the\
",
"target sample size.  TRUE = enough data expected, FALSE = under-powered."),

    "power_interaction_group_allocations.csv" = paste0(
"How the total sample is split across interaction cells (e.g. men 18-24,\
",
"men 25-44, women 18-24, women 25-44).  Tells you the per-cell n."),

    "power_interaction_imbalance_penalty.csv" = paste0(
"The penalty to power introduced by unequal cell sizes.  If allocations\
",
"in power_interaction_group_allocations are skewed, this quantifies how\
",
"much power you lose vs. a balanced design."),

    "power_interaction_pairwise_details.csv" = paste0(
"The detail backing power_interaction_assumptions.  Pairwise effect sizes\
",
"for each combination of interaction levels."),

    "power_interaction_sample_size_targets.csv" = paste0(
"The sample-size *targets* (per cell) to hit your desired power for each\
",
"interaction comparison.  Compare to your actual allocations file."),

    "randomization_block_blueprints.csv" = paste0(
"Pre-baked randomization-scheme blueprints (block sizes, stratification\
",
"factors).  Pick one and the *_example CSVs show what the resulting\
",
"allocation looks like."),

    "randomization_schedule_example_heavy_drinking_30d.csv" = paste0(
"A *worked-example* randomization schedule using heavy-drinking-30-day as\
",
"the stratifying outcome.  Shows the participant id -> arm assignment\
",
"table; useful for replicating in your own survey software."),

    "randomization_schedule_example_ebac_legal.csv" = paste0(
"Worked-example randomization schedule stratified by ebac_legal (the\
",
"legal-limit blood-alcohol-concentration endpoint)."),

    "randomization_schedule_example_ebac_tot.csv" = paste0(
"Worked-example randomization schedule stratified by ebac_tot (the\
",
"total-impairment blood-alcohol-concentration endpoint)."),

    # --- data-wrangling outputs --------------------------------------
    "data_na_summary.csv" = paste0(
"Per-column missingness summary.  Each row is one input column; columns\
",
"include n_missing, pct_missing.  Read this BEFORE running any inference\
",
"module -- fields with high missingness need imputation or exclusion."),

    "data_wrangling_log.csv" = paste0(
"Step-by-step log of what the data-wrangling module did to your input\
",
"(renames, coercions, dropped rows).  Useful for the methods section."),

    # --- descriptive-statistics outputs ------------------------------
    "binomial_summaries.csv" = paste0(
"Survey-weighted binomial summaries (e.g. heavy_drinking_30d prevalence)\
",
"WITHOUT survey weights.  Compare against binomial_summaries_survey_weighted\
",
"to see how much the weights shift the estimates."),

    "binomial_summaries_survey_weighted.csv" = paste0(
"Survey-weighted binomial summaries WITH the CPADS weighting variable\
",
"applied.  These are the prevalence estimates you'd report in a paper."),

    "probability_estimates.csv" = paste0(
"Joint and conditional probability estimates across the survey design.\
",
"Read column by column; row labels indicate the conditioning event."),

    # --- frequentist-inference outputs -------------------------------
    "frequentist_heavy_drinking_prevalence_ci.csv" = paste0(
"Frequentist (Wilson / Clopper-Pearson) confidence intervals for the\
",
"prevalence of heavy drinking.  Each row is one subgroup; columns are\
",
"estimate, ci_lower, ci_upper."),

    "frequentist_effect_sizes.csv" = paste0(
"Cohen's-d / odds-ratio / risk-difference effect sizes for the primary\
",
"contrasts of the analysis.  Read alongside p-values from\
",
"frequentist_hypothesis_tests.csv."),

    "frequentist_hypothesis_tests.csv" = paste0(
"Per-contrast p-values and test statistics.  CAUTION: these are\
",
"NOT corrected for multiple comparisons by default -- apply\
",
"Bonferroni / Benjamini-Hochberg yourself if your design demands it.")
  )
}

#' Human-readable description of a morie output CSV
#'
#' Looks up a one-paragraph + short-table explanation by filename
#' (any leading directory components are stripped).  Falls back to
#' matching on the filename stem if the extension differs.
#'
#' @param filename The CSV filename, with or without a path.
#'
#' @return A character scalar containing the explanation.  If no
#'   registered entry matches, returns a fallback listing the known
#'   files.
#'
#' @examples
#' cat(explain_file("power_summary.csv"))
#'
#' @export
explain_file <- function(filename) {
  explanations <- .morie_explanations()
  name <- basename(filename)
  if (name %in% names(explanations)) return(explanations[[name]])

  base <- tools::file_path_sans_ext(name)
  for (candidate in names(explanations)) {
    if (tools::file_path_sans_ext(candidate) == base)
      return(explanations[[candidate]])
  }

  paste0(
    sprintf("No registered explanation for '%s'.\
\
", name),
    "Known files:\
",
    paste(sprintf("  - %s", sort(names(explanations))), collapse = "\
"),
    "\
\
If you think this file should be explained, file an issue at ",
    "https://github.com/hadesllm/morie/issues."
  )
}

#' Print the morie cheat sheet
#'
#' Mirrors the \code{morie cheatsheet} CLI subcommand: a one-screen
#' reference of install / learn / run / pull / ingest / help commands.
#'
#' @return Invisibly returns a character scalar of the cheatsheet.
#'   Called for its side effect of printing to the console.
#'
#' @examples
#' cheatsheet()
#'
#' @export
cheatsheet <- function() {
  body <- paste(
    "morie cheat sheet",
    "=================",
    "",
    "Install",
    "  curl -fsSL https://hadesllm.github.io/morie/install.sh | bash",
    "  brew tap hadesllm/morie && brew install morie",
    "  pip install morie",
    "  install.packages('morie', repos = 'https://hadesllm.r-universe.dev')",
    "  docker run --rm ghcr.io/hadesllm/morie:latest morie --help",
    "",
    "Learn",
    "  morie tutorial                  Interactive walkthrough",
    "  morie cheatsheet                This card",
    "  morie list-modules              List all 23 analysis modules",
    "  morie list-datasets             List built-in datasets",
    "  morie explain power_summary.csv What does this output mean?",
    "",
    "Run",
    "  morie run-module power-design --output-dir out/",
    "  morie run-module descriptive-statistics --output-dir out/",
    "  morie run-module frequentist-inference --output-dir out/",
    "  morie run-modules all --output-dir out/",
    "",
    "Pull",
    "  morie pull tps-major --year 2024 --out tps-2024.csv",
    "  morie pull tps-shootings --year 2024",
    "  morie pull tps-homicide --year 2024",
    "  morie pull tps-layers                                   # registry",
    "  morie pull cpads --out cpads.csv                        # synth or real",
    "  morie pull otis-a01-toy --out otis.csv                  # toy",
    "  morie pull siu-toy --out siu.csv                        # toy SIU report",
    "",
    "Ingest",
    "  morie ingest tps --layer major-crime --year 2024 --out tps.csv",
    "  morie ingest ckan --portal https://open.canada.ca/data --search alcohol",
    "  morie ingest siu --report-id 22-OFD-001 --out report/",
    "",
    "Help",
    "  morie ask \"I have a treatment-control design; what module fits?\"",
    "  morie doctor                    Check what's installed and working",
    "  morie --help                    Top-level help",
    "",
    "Refs",
    "  Docs:     https://hadesllm.github.io/morie/",
    "  Issues:   https://github.com/hadesllm/morie/issues",
    "  PyPI:     https://pypi.org/project/morie/",
    "  R:        https://hadesllm.r-universe.dev/morie",
    sep = "\
"
  )
  cat(body, "\
", sep = "")
  invisible(body)
}

#' Names of all morie output CSVs with registered explanations
#'
#' @return Character vector of filenames.
#' @export
explain_known_files <- function() sort(names(.morie_explanations()))
