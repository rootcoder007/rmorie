# SPDX-License-Identifier: AGPL-3.0-or-later
#
# OTIS + MAPQ pipeline module runners.
#
# Unlike the CPADS study modules, these two consume their own inputs:
#
#   * otis-analysis runs on the bundled OTIS b01 restrictive-confinement
#     sample (real data.ontario.ca slice, OGL-Ontario; see
#     inst/extdata/otis_b01_sample.csv) via the otis.R analyzers.
#   * mapq-psychometrics runs on a deterministic synthetic MAPQII panel
#     (same role as the .morie_otis_*_panel generators in synth_otis.R);
#     the real TKARONTOMAPQ survey is a private VSR dataset and is not
#     redistributable.

# The b01 sample ships with data.ontario.ca column headers; the otis.R
# analyzers speak the lower-snake schema of the canonical loader.
#' The b01 sample ships with data.ontario.ca column headers; the otis.R
#'
#' analyzers speak the lower-snake schema of the canonical loader.
#'
#' @param df Passed to \code{names}.
#' @return The value of \code{df}, as built in the body.
#' @export
#' @examples
#' df <- data.frame(x = c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), y = c(2.9, 5.1, 6.8,
#' 9.4, 11.2, 13.1, 15.0, 17.6), g = c('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b'),
#' stringsAsFactors = FALSE)
#' res <- .otis_b01_canonical(df = df)
#' res
.otis_b01_canonical <- function(df) {
  map <- c(
    EndFiscalYear = "end_fiscal_year",
    UniqueIndividual_ID = "unique_individual_id",
    Gender = "gender",
    Region_AtTimeOfPlacement = "region_at_time_of_placement",
    Region_MostRecentPlacement = "region_most_recent_placement",
    Age_Category = "age_category",
    MentalHealth_Alert = "mental_health_alert",
    SuicideRisk_Alert = "suicide_risk_alert",
    SuicideWatch_Alert = "suicide_watch_alert",
    NumberConsecutiveDays_Segregation = "number_consecutive_days_segregation",
    Number_Of_Placements = "number_of_placements"
  )
  hit <- names(df) %in% names(map)
  names(df)[hit] <- unname(map[names(df)[hit]])
  df
}

#' .run_otis_analysis_module_internal
#'
#' A step of the study_otis_mapq implementation. Called by \code{morie_run_morie_module}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A list with \code{otis_descriptives}, \code{otis_alert_combos},
#' \code{otis_dml_results}, \code{otis_trends}.
#' @export
#' @examples
#' res <- .run_otis_analysis_module_internal()
#' res
.run_otis_analysis_module_internal <- function() {
  df <- .otis_b01_canonical(morie_sample("otis_b01"))

  desc <- morie_otis_otdesc(df)$payload
  descriptives <- data.frame(
    metric = c(
      "n_individuals_unique", "n_records",
      "placements_min", "placements_q1", "placements_median",
      "placements_mean", "placements_q3", "placements_max"
    ),
    value = c(
      desc$n_total, desc$n_records,
      desc$placement_dist$min, desc$placement_dist$q1,
      desc$placement_dist$median, desc$placement_dist$mean,
      desc$placement_dist$q3, desc$placement_dist$max
    ),
    stringsAsFactors = FALSE
  )

  combos <- morie_otis_astcmb(df)$payload$summary
  trends <- morie_otis_rctrnd(df)$payload$trends

  dml_df <- df
  dml_df$mh_alert <- .otis_binarise(df$mental_health_alert)
  dml <- morie_otis_otdml(
    dml_df,
    outcome = "number_consecutive_days_segregation",
    treatment = "mh_alert",
    covariates = c(
      "gender", "age_category",
      "region_at_time_of_placement", "region_most_recent_placement"
    )
  )$payload
  dml_results <- data.frame(
    estimand = c("ATE", "ATT"),
    estimate = c(dml$ate, dml$att),
    se = c(dml$ate_se, dml$att_se),
    p_value = c(dml$ate_pval, dml$att_pval),
    n = dml$n,
    method = dml$method,
    treatment = "mental_health_alert",
    outcome = "number_consecutive_days_segregation",
    stringsAsFactors = FALSE
  )

  list(
    otis_descriptives = descriptives,
    otis_alert_combos = combos,
    otis_dml_results = dml_results,
    otis_trends = trends
  )
}

# MAPQII structure: 20 Likert items, 4 subscales (see the Python
# mirror in src/morie/fn/_mapq_const.py).
#' MAPQII structure: 20 Likert items, 4 subscales (see the Python
#'
#' mirror in src/morie/fn/_mapq_const.py).
#'
#' @return A list with \code{EE}, \code{EA}, \code{UA}, \code{ER}.
#' @export
#' @examples
#' res <- .mapq_subscales()
#' res
.mapq_subscales <- function() {
  list(
    EE = paste0("EE", 1:5),  # Experiential Engagement
    EA = paste0("EA", 1:5),  # Epistemic Attitudes
    UA = paste0("UA", 1:5),  # Utilitarian Attitudes
    ER = paste0("ER", 1:5)   # Ethical Reservations
  )
}

# Deterministic synthetic MAPQII panel with a planted 4-factor
# structure, plus gender/age demographics and a Knowledge Scale (KS)
# driven by epistemic attitudes and a modest gender gap -- so the DML
# stage has a real signal to recover.
#' Deterministic synthetic MAPQII panel with a planted 4-factor
#'
#' structure, plus gender/age demographics and a Knowledge Scale (KS)
#' driven by epistemic attitudes and a modest gender gap -- so the DML
#' stage has a real signal to recover.
#'
#' @param n Passed to \code{sample}. Defaults to \code{400L}.
#' @param seed Passed to \code{set.seed}. Defaults to \code{2026L}.
#' @return The value of \code{panel}, as built in the body.
#' @export
#' @examples
#' res <- .morie_mapq_synth_panel()
#' res
.morie_mapq_synth_panel <- function(n = 400L, seed = 2026L) {
  .rmorie_local_seed(seed)
  subscales <- .mapq_subscales()
  panel <- data.frame(
    gender_male = stats::rbinom(n, 1L, 0.5),
    age = sample(18:65, n, replace = TRUE)
  )
  for (f in names(subscales)) {
    latent <- stats::rnorm(n)
    for (item in subscales[[f]]) {
      raw <- 3 + 0.9 * latent + stats::rnorm(n, sd = 0.8)
      panel[[item]] <- pmin(5L, pmax(1L, as.integer(round(raw))))
    }
  }
  for (f in names(subscales)) {
    panel[[paste0(tolower(f), "_score")]] <- rowSums(panel[, subscales[[f]]])
  }
  panel$ks_score <- as.numeric(
    10 + 0.4 * panel$ea_score + 1.5 * panel$gender_male +
      0.02 * panel$age + stats::rnorm(n)
  )
  panel
}

#' .run_mapq_psychometrics_module_internal
#'
#' A step of the study_otis_mapq implementation. Called by \code{morie_run_morie_module}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A list with \code{mapq_reliability}, \code{mapq_factor_loadings},
#' \code{mapq_dml_results}.
#' @export
#' @examples
#' res <- .run_mapq_psychometrics_module_internal()
#' res
.run_mapq_psychometrics_module_internal <- function() {
  panel <- .morie_mapq_synth_panel()
  subscales <- .mapq_subscales()
  scales <- c(subscales, list(Total = unlist(subscales, use.names = FALSE)))

  rel_rows <- lapply(names(scales), function(nm) {
    items <- panel[, scales[[nm]], drop = FALSE]
    a <- morie_psymet_alpha(items)
    # psych::omega chatters about single-factor omega_h; we report
    # omega_total per scale, so silence the advisory output.
    utils::capture.output(
      o <- suppressWarnings(suppressMessages(morie_psymet_omega(items)))
    )
    data.frame(
      scale = nm,
      n_items = ncol(items),
      n = a$n,
      alpha_raw = a$raw,
      alpha_std = a$std,
      alpha_ci_low = a$ci_lo,
      alpha_ci_high = a$ci_hi,
      omega_total = o$total,
      omega_hier = o$hier,
      splithalf_sb = morie_psymet_splithalf(items),
      stringsAsFactors = FALSE
    )
  })
  reliability <- do.call(rbind, rel_rows)

  items <- as.matrix(panel[, unlist(subscales, use.names = FALSE)])
  fa <- tryCatch(
    stats::factanal(items, factors = 4L, rotation = "varimax"),
    error = function(e) NULL
  )
  if (!is.null(fa)) {
    L <- unclass(fa$loadings)
    method <- "ml_factanal_varimax"
  } else {
    # Principal-axis fallback mirroring morie_psymet_omega.
    R <- stats::cor(items)
    eig <- eigen(R, symmetric = TRUE)
    L <- eig$vectors[, 1:4, drop = FALSE] %*% diag(sqrt(pmax(eig$values[1:4], 0)))
    rownames(L) <- colnames(items)
    method <- "pca_fallback"
  }
  loadings <- data.frame(
    item = rownames(L),
    assigned_subscale = rep(names(subscales), each = 5L),
    factor1 = as.numeric(L[, 1]),
    factor2 = as.numeric(L[, 2]),
    factor3 = as.numeric(L[, 3]),
    factor4 = as.numeric(L[, 4]),
    method = method,
    stringsAsFactors = FALSE
  )

  dml <- morie_otis_otdml(
    panel,
    outcome = "ks_score",
    treatment = "gender_male",
    covariates = c("age", "ee_score", "ea_score", "ua_score", "er_score")
  )$payload
  dml_results <- data.frame(
    estimand = c("ATE", "ATT"),
    estimate = c(dml$ate, dml$att),
    se = c(dml$ate_se, dml$att_se),
    p_value = c(dml$ate_pval, dml$att_pval),
    n = dml$n,
    method = dml$method,
    treatment = "gender_male",
    outcome = "ks_score",
    stringsAsFactors = FALSE
  )

  list(
    mapq_reliability = reliability,
    mapq_factor_loadings = loadings,
    mapq_dml_results = dml_results
  )
}
