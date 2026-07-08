# SPDX-License-Identifier: AGPL-3.0-or-later
#' Forensic toxicology: matrix-aware quantitation and antemortem inference
#'
#' A thin domain layer for postmortem forensic toxicology, aimed at the hard
#' case where peripheral blood is compromised (hemodilution after water
#' submersion, or decompositional change) and the analyst must reason from
#' alternative matrices (liver, vitreous humour, brain, gastric contents).
#' Like \code{\link{taphonomy}}, it adds no new statistics: it supplies typed,
#' zero-row schemas and dispatches quantitation to base-R least squares and the
#' antemortem-vs-artefact question to MORIE's existing likelihood-ratio
#' machinery (\code{\link{morie_taphonomy_likelihood_ratio}}).
#'
#' \strong{This module ships no data.} Calibrator responses, case measurements,
#' and matrix concentrations must be supplied by the caller from real analytical
#' runs. \code{\link{morie_tox_matrix_schema}} returns a typed zero-row frame;
#' it never fabricates rows.
#'
#' \strong{What it can and cannot do.} It fits an internal-standard calibration
#' curve and inverse-predicts an unknown concentration with LOD/LOQ, flags
#' postmortem redistribution from a central:peripheral ratio, and quantifies
#' support for antemortem ingestion versus postmortem microbial production as a
#' likelihood ratio. It reports bounded statistical support, never proof or a
#' posterior probability, and it does not establish cause of death.
#'
#' @name tox
#' @references
#' Dinis-Oliveira RJ, et al. (2010). Collection of biological samples in
#'   forensic toxicology. \emph{Toxicology Mechanisms and Methods} 20(7),
#'   363--414. \doi{10.3109/15376516.2010.497976}
#'
#' Pounder DJ, Jones GR (1990). Post-mortem drug redistribution -- a
#'   toxicological nightmare. \emph{Forensic Science International} 45(3),
#'   253--263. \doi{10.1016/0379-0738(90)90182-X}
#'
#' Kugelberg FC, Jones AW (2007). Interpreting results of ethanol analysis in
#'   postmortem specimens. \emph{Forensic Science International} 165(1), 10--29.
#'   \doi{10.1016/j.forsciint.2006.05.004}
NULL

# Null-coalescing helper (package idiom: defined per-file, see taphonomy.R).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Recognised specimen matrices, ordered by resistance to dilution/putrefaction.
# Used only to validate the `matrix` field; carries no fabricated priors.
.tox_matrices <- c(
  "peripheral_blood", "central_blood", "liver", "vitreous_humour",
  "brain", "gastric", "urine", "bile", "muscle", "kidney"
)

#' Typed zero-row schema for a forensic-toxicology sample table
#'
#' Describes the columns a caller must supply for matrix-aware quantitation.
#' Returns a zero-row data frame; it never fabricates measurements.
#'
#' @return A zero-row \code{data.frame} with a \code{"role"} attribute mapping
#'   each column to \code{"identifier"}, \code{"matrix"}, \code{"measurement"},
#'   or \code{"quality"}.
#' @examples
#' morie_tox_matrix_schema()
#' @export
morie_tox_matrix_schema <- function() {
  spec <- c(
    case_id        = "character", # opaque case/sample identifier
    analyte        = "character", # substance measured (parent compound)
    matrix         = "character", # one of .tox_matrices
    conc           = "numeric",   # measured concentration (mg/L or mg/kg)
    conc_units     = "character", # e.g. "mg/L", "mg/kg"
    lod            = "numeric",   # assay limit of detection (same units)
    loq            = "numeric",   # assay limit of quantitation
    decomp_stage   = "integer",   # ordinal decomposition stage (0 = fresh)
    submersion_days = "numeric",  # days submerged (0 if not a water case)
    censored       = "integer"    # 1 = below LOD (left-censored)
  )
  roles <- c(
    "identifier", "identifier", "matrix", "measurement", "measurement",
    "quality", "quality", "matrix", "matrix", "quality"
  )
  cols <- Map(function(t) {
    switch(t, integer = integer(0), numeric = numeric(0), character(0))
  }, spec)
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  attr(df, "role") <- stats::setNames(roles, names(spec))
  df
}

#' Fit an internal-standard calibration curve and inverse-predict an unknown
#'
#' Weighted linear least squares of instrument response on known calibrator
#' concentration, the standard basis for GC-MS / LC-MS/MS quantitation. LOD and
#' LOQ follow the residual-standard-error convention (LOD = 3.3 s/b,
#' LOQ = 10 s/b; ICH Q2). With \code{response_unknown}, inverse-predicts the
#' unknown concentration and flags it against LOD/LOQ.
#'
#' @param conc Numeric vector of known calibrator concentrations (> 0).
#' @param response Numeric vector of instrument responses (same length).
#' @param weights Optional weights; \code{"1/x^2"} (default, the usual variance
#'   model for wide tox calibration ranges), \code{"1/x"}, or \code{"none"}, or
#'   a numeric vector.
#' @param response_unknown Optional scalar response for a case sample to
#'   inverse-predict.
#' @return A list with the fitted \code{slope}, \code{intercept},
#'   \code{r_squared}, \code{lod}, \code{loq}, and (if \code{response_unknown}
#'   is given) \code{conc_hat} plus a \code{flag} of \code{"below_lod"},
#'   \code{"below_loq"}, or \code{"quantifiable"}.
#' @examples
#' cal <- morie_tox_calibration(
#'   conc = c(0.05, 0.1, 0.5, 1, 5, 10),
#'   response = c(48, 102, 495, 1010, 5050, 9980),
#'   response_unknown = 250
#' )
#' cal$conc_hat
#' @export
morie_tox_calibration <- function(conc, response, weights = "1/x^2",
                                  response_unknown = NULL) {
  conc <- as.numeric(conc)
  response <- as.numeric(response)
  if (length(conc) != length(response)) {
    stop("`conc` and `response` must be the same length", call. = FALSE)
  }
  if (length(conc) < 3L) {
    stop("need at least 3 calibrators to fit a curve", call. = FALSE)
  }
  if (any(!is.finite(conc)) || any(!is.finite(response))) {
    stop("`conc`/`response` have non-finite values", call. = FALSE)
  }
  if (any(conc <= 0)) stop("calibrator `conc` must be > 0", call. = FALSE)

  w <- if (is.numeric(weights)) {
    if (length(weights) != length(conc)) {
      stop("numeric `weights` must match `conc` length", call. = FALSE)
    }
    weights
  } else {
    switch(weights,
      "1/x^2" = 1 / conc^2,
      "1/x"   = 1 / conc,
      "none"  = rep(1, length(conc)),
      stop("`weights` must be '1/x^2', '1/x', 'none', or numeric", call. = FALSE)
    )
  }

  fit <- stats::lm(response ~ conc, weights = w)
  co <- stats::coef(fit)
  intercept <- unname(co[[1]])
  slope <- unname(co[[2]])
  if (!is.finite(slope) || slope == 0) {
    stop("degenerate calibration (zero/undefined slope)", call. = FALSE)
  }
  s_resid <- summary(fit)$sigma
  lod <- 3.3 * s_resid / abs(slope)
  loq <- 10 * s_resid / abs(slope)

  out <- list(
    slope = slope,
    intercept = intercept,
    r_squared = summary(fit)$r.squared,
    lod = lod,
    loq = loq,
    weights = if (is.numeric(weights)) "numeric" else weights
  )
  if (!is.null(response_unknown)) {
    ru <- as.numeric(response_unknown)
    if (length(ru) != 1L || !is.finite(ru)) {
      stop("`response_unknown` must be a finite scalar", call. = FALSE)
    }
    conc_hat <- (ru - intercept) / slope
    out$conc_hat <- conc_hat
    out$flag <- if (conc_hat < lod) "below_lod" else if (conc_hat < loq) {
      "below_loq"
    } else {
      "quantifiable"
    }
  }
  out
}

#' Flag postmortem redistribution from a central:peripheral concentration ratio
#'
#' Postmortem redistribution (PMR) inflates central-blood (e.g. cardiac) drug
#' concentrations relative to peripheral (femoral) blood after death, so a raw
#' central value can overstate the antemortem level. The central:peripheral
#' (C/P) ratio is the standard screen: C/P near 1 is stable; larger ratios
#' indicate redistribution and unreliable central quantitation.
#'
#' @param central Central (cardiac) blood concentration (> 0).
#' @param peripheral Peripheral (femoral) blood concentration (> 0).
#' @return A list with the \code{cp_ratio} and an ordinal \code{redistribution}
#'   flag (\code{"minimal"}, \code{"modest"}, or \code{"significant"}) plus an
#'   \code{interpretation} string. Thresholds follow the common C/P > 1 (modest)
#'   and C/P > 2--3 (significant) forensic convention.
#' @examples
#' morie_tox_pmr_ratio(central = 2.4, peripheral = 0.8)
#' @export
morie_tox_pmr_ratio <- function(central, peripheral) {
  central <- as.numeric(central)
  peripheral <- as.numeric(peripheral)
  if (length(central) != 1L || length(peripheral) != 1L) {
    stop("`central` and `peripheral` must be scalars", call. = FALSE)
  }
  if (!is.finite(central) || !is.finite(peripheral)) {
    stop("`central`/`peripheral` must be finite", call. = FALSE)
  }
  if (peripheral <= 0 || central <= 0) {
    stop("concentrations must be > 0", call. = FALSE)
  }
  cp <- central / peripheral
  flag <- if (cp <= 1) "minimal" else if (cp <= 2) "modest" else "significant"
  list(
    cp_ratio = cp,
    redistribution = flag,
    interpretation = sprintf(
      paste0("C/P ratio = %.2f (%s redistribution). %s"),
      cp, flag,
      switch(flag,
        minimal = "Central and peripheral agree; central quantitation is a reasonable antemortem proxy.",
        modest = "Some redistribution; prefer the peripheral (femoral) value for interpretation.",
        significant = "Marked redistribution; the central value likely overstates the antemortem concentration -- interpret from peripheral blood or an alternative matrix."))
  )
}

#' Likelihood ratio for antemortem ingestion versus postmortem artefact
#'
#' Quantifies support for an analyte being present antemortem versus arising
#' from a postmortem process (e.g. microbial ethanol production during
#' decomposition or submersion). The caller supplies an observed marker value
#' -- typically a specific secondary metabolite that only a living, functioning
#' liver produces (ethyl glucuronide / ethyl sulfate for ethanol), or a
#' congener/matrix ratio -- and Gaussian models for the two hypotheses. This is
#' a thin domain wrapper over \code{\link{morie_taphonomy_preservation_lr}}.
#'
#' @param marker Observed marker value(s) (e.g. EtG concentration, or a
#'   parent:artefact ratio).
#' @param antemortem \code{list(mean =, sd =)} for the marker under antemortem
#'   ingestion (H1).
#' @param postmortem \code{list(mean =, sd =)} for the marker under a
#'   postmortem-artefact process (H2).
#' @return The list returned by \code{\link{morie_taphonomy_likelihood_ratio}}
#'   (\code{lr}, \code{log10_lr}, \code{verbal}, \code{interpretation}, and the
#'   two log-likelihoods), with \code{interpretation} reframed for the
#'   antemortem question.
#' @examples
#' # EtG well above the postmortem-artefact background supports antemortem intake
#' morie_tox_antemortem_lr(
#'   marker = 1.8,
#'   antemortem = list(mean = 2.0, sd = 0.5),
#'   postmortem = list(mean = 0.1, sd = 0.3)
#' )$verbal
#' @export
morie_tox_antemortem_lr <- function(marker, antemortem, postmortem) {
  out <- morie_taphonomy_preservation_lr(
    evidence = marker,
    natural = antemortem,
    alternative = postmortem
  )
  out$interpretation <- sprintf(
    paste0("LR = %.4g (log10 = %.3f): the evidence is %s for antemortem ",
           "presence. The observed marker is %s more probable under H1 ",
           "(antemortem ingestion) than under H2 (postmortem artefact). This ",
           "is bounded support, not proof and not a posterior probability."),
    out$lr, out$log10_lr, out$verbal,
    if (is.finite(out$lr) && out$lr >= 1) sprintf("%.4g times", out$lr)
    else if (is.finite(out$lr)) sprintf("%.4g times less", 1 / out$lr)
    else "infinitely")
  out
}

# Documented resistance-to-degradation ordering of specimen matrices under
# dilution/putrefaction. A transparent heuristic (Dinis-Oliveira et al. 2010),
# NOT an empirical prior: higher = more robust when peripheral blood fails.
# Blood matrices carry a `dilutes` flag (susceptible to submersion hemodilution).
.tox_matrix_reliability <- list(
  vitreous_humour  = list(base = 0.95, dilutes = FALSE),
  brain            = list(base = 0.85, dilutes = FALSE),
  liver            = list(base = 0.80, dilutes = FALSE),
  bile             = list(base = 0.70, dilutes = FALSE),
  muscle           = list(base = 0.65, dilutes = FALSE),
  kidney           = list(base = 0.65, dilutes = FALSE),
  gastric          = list(base = 0.55, dilutes = FALSE),
  urine            = list(base = 0.60, dilutes = TRUE),
  peripheral_blood = list(base = 0.55, dilutes = TRUE),
  central_blood    = list(base = 0.35, dilutes = TRUE)
)

#' Rank specimen matrices by reliability under submersion and decomposition
#'
#' When peripheral blood is compromised (water submersion hemodilution or
#' decompositional change), the analyst must choose an alternative matrix. This
#' encodes the documented resistance-to-degradation ordering (vitreous humour
#' and brain are anatomically protected; the liver concentrates metabolites;
#' blood-based matrices dilute) as a transparent heuristic score, penalised by
#' days submerged and decomposition stage. It is a documented ordering, not an
#' empirical prior, and never fabricates case data.
#'
#' @param matrix Optional character vector of matrices to score (default: all
#'   recognised). Each must be one of \code{morie_tox_matrix_schema()}'s matrix
#'   values.
#' @param submersion_days Days submerged (>= 0); penalises diluting matrices.
#' @param decomp_stage Ordinal decomposition stage (0 = fresh); penalises all.
#' @return A \code{data.frame} of \code{matrix}, \code{reliability} (0--1), and
#'   \code{rank} (1 = most reliable), ordered best first.
#' @examples
#' morie_tox_matrix_reliability(submersion_days = 14, decomp_stage = 3)
#' @export
morie_tox_matrix_reliability <- function(matrix = NULL, submersion_days = 0,
                                         decomp_stage = 0L) {
  submersion_days <- as.numeric(submersion_days)
  decomp_stage <- as.numeric(decomp_stage)
  if (length(submersion_days) != 1L || !is.finite(submersion_days) ||
      submersion_days < 0) {
    stop("`submersion_days` must be a finite scalar >= 0", call. = FALSE)
  }
  if (length(decomp_stage) != 1L || !is.finite(decomp_stage) ||
      decomp_stage < 0) {
    stop("`decomp_stage` must be a finite scalar >= 0", call. = FALSE)
  }
  mats <- matrix %||% names(.tox_matrix_reliability)
  unknown <- setdiff(mats, names(.tox_matrix_reliability))
  if (length(unknown)) {
    stop("unknown matrix: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  # Monotone decay: submersion hurts diluting matrices most; decomp hurts all.
  dil_pen <- 1 - min(submersion_days / 30, 0.8)   # up to -80% at 30+ days
  dec_pen <- 1 - min(decomp_stage / 10, 0.6)       # up to -60% at stage 10+
  rel <- vapply(mats, function(m) {
    spec <- .tox_matrix_reliability[[m]]
    r <- spec$base * dec_pen
    if (spec$dilutes) r <- r * dil_pen
    r
  }, numeric(1))
  ord <- order(rel, decreasing = TRUE)
  data.frame(
    matrix = mats[ord],
    reliability = round(rel[ord], 3),
    rank = seq_along(mats),
    stringsAsFactors = FALSE
  )
}

#' Impute left-censored (below-LOD) toxicology values
#'
#' Forensic assays left-censor: results below the limit of detection are
#' reported only as "< LOD". Naive deletion or zero-filling biases summaries.
#' This applies a documented simple-substitution rule to the censored entries;
#' for regression on censored data prefer
#' \code{\link{morie_horowitz_censored_regression}}.
#'
#' @param values Numeric vector of measured concentrations; censored entries are
#'   \code{NA} (or values below \code{lod}).
#' @param lod Limit of detection (scalar > 0). Entries \code{NA} or \code{< lod}
#'   are treated as left-censored.
#' @param method \code{"half"} (LOD/2, the common default), \code{"sqrt2"}
#'   (LOD/sqrt(2), for moderate censoring), or \code{"lod"} (substitute LOD).
#' @return A list with the \code{imputed} vector, a logical \code{censored} mask,
#'   and the \code{fraction_censored}.
#' @examples
#' morie_tox_left_censor_impute(c(0.4, NA, 0.9, 0.02), lod = 0.05)$imputed
#' @export
morie_tox_left_censor_impute <- function(values, lod, method = "half") {
  values <- as.numeric(values)
  lod <- as.numeric(lod)
  if (length(lod) != 1L || !is.finite(lod) || lod <= 0) {
    stop("`lod` must be a finite scalar > 0", call. = FALSE)
  }
  sub <- switch(method,
    half  = lod / 2,
    sqrt2 = lod / sqrt(2),
    lod   = lod,
    stop("`method` must be 'half', 'sqrt2', or 'lod'", call. = FALSE)
  )
  censored <- is.na(values) | values < lod
  imputed <- values
  imputed[censored] <- sub
  list(
    imputed = imputed,
    censored = censored,
    fraction_censored = mean(censored)
  )
}

#' Adjudicate ethanol as antemortem ingestion versus postmortem production
#'
#' Decomposition and submersion let microbes ferment glucose into ethanol,
#' mimicking antemortem drinking. The standard discriminators (Kugelberg &
#' Jones 2007): higher alcohols (n-propanol, n-butanol, isobutanol) are
#' fermentation by-products, so their presence points to postmortem production;
#' and a specific liver metabolite (ethyl glucuronide, EtG) is produced only by
#' living metabolism, so its presence points to antemortem intake. This returns
#' a flag from those signals; for a quantified likelihood ratio, feed EtG to
#' \code{\link{morie_tox_antemortem_lr}}.
#'
#' @param ethanol Measured ethanol concentration (>= 0).
#' @param n_propanol,n_butanol Higher-alcohol concentrations (>= 0); non-zero
#'   indicates microbial fermentation. Default 0.
#' @param etg Ethyl glucuronide concentration (>= 0); non-zero indicates
#'   antemortem intake. Default \code{NA} (not measured).
#' @param congener_threshold Higher-alcohol level above which fermentation is
#'   flagged. Default 0.
#' @return A list with the \code{verdict} (\code{"antemortem"},
#'   \code{"postmortem_production"}, or \code{"indeterminate"}) and an
#'   \code{interpretation} string.
#' @examples
#' morie_tox_ethanol_congeners(ethanol = 1.2, n_propanol = 0.08)$verdict
#' morie_tox_ethanol_congeners(ethanol = 1.2, etg = 3.5)$verdict
#' @export
morie_tox_ethanol_congeners <- function(ethanol, n_propanol = 0, n_butanol = 0,
                                        etg = NA_real_, congener_threshold = 0) {
  ethanol <- as.numeric(ethanol)
  if (length(ethanol) != 1L || !is.finite(ethanol) || ethanol < 0) {
    stop("`ethanol` must be a finite scalar >= 0", call. = FALSE)
  }
  higher <- max(as.numeric(n_propanol), as.numeric(n_butanol), na.rm = TRUE)
  ferment <- is.finite(higher) && higher > congener_threshold
  antemortem <- is.finite(as.numeric(etg)) && as.numeric(etg) > 0

  verdict <- if (antemortem) {
    "antemortem"
  } else if (ferment) {
    "postmortem_production"
  } else {
    "indeterminate"
  }
  interpretation <- switch(verdict,
    antemortem = sprintf(
      "EtG present (%.3g); ethyl glucuronide requires living metabolism, supporting antemortem ingestion despite decomposition.",
      as.numeric(etg)),
    postmortem_production = sprintf(
      "Higher alcohols present (%.3g > %.3g); n-propanol/n-butanol are fermentation by-products, so the ethanol (%.3g) is consistent with postmortem microbial production.",
      higher, congener_threshold, ethanol),
    indeterminate = sprintf(
      "No fermentation congeners and no EtG measured; ethanol (%.3g) cannot be adjudicated antemortem vs postmortem from these signals alone -- add EtG/EtS or a vitreous:blood comparison.",
      ethanol))
  list(verdict = verdict, interpretation = interpretation,
       higher_alcohol = higher, etg = as.numeric(etg))
}
