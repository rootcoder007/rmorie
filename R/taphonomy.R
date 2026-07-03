# SPDX-License-Identifier: AGPL-3.0-or-later
#' Taphonomic preservation as a causal-inference problem
#'
#' A thin domain layer that recasts the question "is this body's preservation
#' natural or anomalous?" as a treatment-effect estimate over MORIE's existing
#' causal estimators. It adds no new statistics: it documents the taphonomy
#' variable set and dispatches to \code{\link{morie_estimate_irm}} /
#' \code{\link{morie_estimate_cate}}, then attaches an E-value
#' (\code{\link{morie_e_value}}) that quantifies how strong an \emph{unmeasured}
#' cause would have to be to explain away the natural preservation effect --
#' i.e. it turns a claim of "incorruptibility" into a bounded statistical one.
#'
#' \strong{This module ships no data.} Forensic-taphonomy comparanda (documented
#' lime/desiccation burials with preservation outcomes) and any non-invasive
#' readings of a specific case (CT/micro-CT density, pXRF elemental signatures,
#' hyperspectral surface composition) must be supplied by the caller from real
#' sources. \code{\link{morie_taphonomy_schema}} returns a typed, zero-row frame
#' describing the expected columns; it never fabricates rows.
#'
#' \strong{What it can and cannot do.} With comparanda it estimates the average
#' preservation effect attributable to burial \emph{processing} (e.g. quicklime
#' desiccation) and reports the E-value residual. It cannot falsify a miracle:
#' a small natural-preservation probability yields a \emph{delta}, not a
#' disproof (Chernozhukov et al. 2018 give the effect; VanderWeele & Ding 2017
#' bound the unmeasured-confounding needed to nullify it).
#'
#' @name taphonomy
#' @references
#' Chernozhukov V, et al. (2018). Double/debiased machine learning.
#'   \emph{The Econometrics Journal} 21(1), C1--C68. \doi{10.1111/ectj.12097}
#'
#' VanderWeele TJ, Ding P (2017). Sensitivity analysis in observational
#'   research: introducing the E-value. \emph{Annals of Internal Medicine}
#'   167(4), 268--274. \doi{10.7326/M16-2607}
NULL

# Null-coalescing helper (package idiom: defined per-file, see did.R / iv.R).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Canonical taphonomy variable set. Grouped by causal role. Kept internal so
# the schema and the estimator agree on one source of truth.
.taphonomy_vars <- function() {
  list(
    # Anthropogenic "processing" — candidate treatment(s).
    treatment = c(
      lime_treatment = "integer"   # 1 = interred with quicklime, 0 = not
    ),
    # Environmental + handling covariates (confounders / effect modifiers).
    covariates = c(
      temp_c            = "numeric",  # mean interment temperature (deg C)
      humidity_pct      = "numeric",  # mean relative humidity (%)
      arid              = "integer",  # 1 = cool/arid microclimate
      casket_sealed     = "integer",  # 1 = sealed casket / low air exchange
      reinterment_count = "integer",  # times exhumed / moved
      exposure_days     = "numeric",  # days of pre-burial exposure
      decades_elapsed   = "numeric"   # time since death (decades)
    ),
    # Non-invasive measurement channels (outcome proxies / auxiliary covariates).
    measurements = c(
      ct_density_hu       = "numeric",  # CT/micro-CT mean tissue density (HU)
      ct_void_fraction    = "numeric",  # internal void fraction (0-1)
      pxrf_ca_ppm         = "numeric",  # pXRF residual surface calcium (ppm)
      hyperspectral_resin = "integer"   # 1 = applied resin/wax detected
    ),
    # Preservation outcome.
    outcome = c(
      preservation_score = "numeric"  # 0 (fully decayed) .. 1 (intact)
    )
  )
}

#' Taphonomy data schema (zero-row template)
#'
#' Returns an empty, typed \code{data.frame} documenting the columns
#' \code{\link{morie_taphonomy_preservation_delta}} expects. No rows are
#' fabricated -- the caller fills it with real comparanda.
#'
#' @return A zero-row \code{data.frame}; each column has the correct type and a
#'   \code{"role"} attribute (\code{"treatment"}, \code{"covariate"},
#'   \code{"measurement"}, or \code{"outcome"}).
#' @examples
#' str(morie_taphonomy_schema())
#' @export
morie_taphonomy_schema <- function() {
  v <- .taphonomy_vars()
  spec <- c(v$treatment, v$covariates, v$measurements, v$outcome)
  roles <- c(
    rep("treatment",   length(v$treatment)),
    rep("covariate",   length(v$covariates)),
    rep("measurement", length(v$measurements)),
    rep("outcome",     length(v$outcome))
  )
  cols <- Map(function(type) {
    switch(type, integer = integer(0), numeric = numeric(0), character(0))
  }, spec)
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  attr(df, "role") <- stats::setNames(roles, names(spec))
  df
}

# Convert a standardized mean difference d = ATE / sd(outcome) to an approximate
# risk ratio for the E-value, per VanderWeele & Ding (2017): RR ~= exp(0.91 * d).
# E-values are defined on ratio scales; this is the standard continuous-outcome
# bridge. Returns a value >= 1 (E-value machinery expects RR on the ">1" side).
.taphonomy_smd_to_rr <- function(d) {
  rr <- exp(0.91 * d)
  if (rr < 1) rr <- 1 / rr
  rr
}

#' Estimate the natural preservation "delta" and its E-value
#'
#' Recasts a taphonomy question as a treatment-effect estimate: how much of the
#' observed preservation is attributable to burial \emph{processing} (the
#' \code{treatment}, e.g. quicklime desiccation), holding environment and
#' handling fixed, and how strong an unmeasured cause (a "miracle") would need to
#' be to explain the rest. A thin dispatch over
#' \code{\link{morie_estimate_irm}} (DoubleML IRM, cross-fit ATE + SE; default)
#' or \code{\link{morie_estimate_cate}} (meta-learner, per-unit heterogeneous
#' effects) plus \code{\link{morie_e_value}}. Adds no new statistics.
#'
#' @param data A \code{data.frame} of real comparanda (see
#'   \code{\link{morie_taphonomy_schema}}). Must be non-empty.
#' @param treatment Binary treatment column (default \code{"lime_treatment"}).
#' @param outcome Continuous preservation outcome (default
#'   \code{"preservation_score"}).
#' @param covariates Character vector of confounder/measurement columns. Defaults
#'   to every schema covariate + measurement present in \code{data}.
#' @param estimator \code{"irm"} (DoubleML cross-fit ATE with an
#'   orthogonal SE; default) or \code{"cate"} (meta-learner per-unit effects,
#'   summarised to the mean delta, with the full per-unit vector in
#'   \code{cate_per_unit} and its dispersion in \code{cate_sd}).
#' @param se_method Inference for the \code{"cate"} path only:
#'   \code{"none"} (default) reports the point estimate + \code{cate_sd}
#'   dispersion with no SE/CI/p-value; \code{"bootstrap"} resamples rows and
#'   refits the CATE procedure \code{n_boot} times to give a \emph{valid} SE,
#'   percentile CI, and p-value for the mean effect. (\code{sd(tau)/sqrt(n)} is
#'   deliberately not offered: the per-unit effects are correlated fitted
#'   predictions, so it understates the true SE -- use \code{cate_sd} for
#'   heterogeneity or bootstrap for inference.) Ignored for \code{"irm"}, which
#'   carries its own orthogonal SE.
#' @param n_boot Bootstrap resamples when \code{se_method = "bootstrap"}
#'   (default 199).
#' @param boot_seed RNG seed for the bootstrap (default 42).
#' @param ... Passed to the chosen estimator (e.g. \code{n_folds} for IRM,
#'   \code{meta_learner} for CATE).
#' @return A named \code{list} (RichResult-style; all estimates are
#'   \code{double}): \code{value} (the preservation delta = ATE / mean CATE),
#'   \code{se}, \code{p_value} (Wald; \code{NA} on the CATE path -- no valid SE),
#'   \code{ci_lower}, \code{ci_upper}, \code{n} (integer count),
#'   \code{e_value}, \code{e_value_ci}, \code{estimator}, \code{cate_per_unit}
#'   and \code{cate_sd} (NULL unless \code{estimator = "cate"}), \code{warnings},
#'   and a plain-language \code{interpretation}.
#' @seealso \code{\link{morie_estimate_irm}}, \code{\link{morie_estimate_cate}},
#'   \code{\link{morie_e_value}}, \code{\link{morie_causal_impact}}
#' @examples
#' \donttest{
#' if (requireNamespace("DoubleML", quietly = TRUE)) {
#'   df <- morie_taphonomy_schema()        # fill with REAL comparanda first
#'   # morie_taphonomy_preservation_delta(df)
#' }
#' }
#' @export
morie_taphonomy_preservation_delta <- function(data,
                                               treatment = "lime_treatment",
                                               outcome = "preservation_score",
                                               covariates = NULL,
                                               estimator = c("irm", "cate"),
                                               se_method = c("none", "bootstrap"),
                                               n_boot = 199L,
                                               boot_seed = 42L,
                                               ...) {
  estimator <- match.arg(estimator)
  se_method <- match.arg(se_method)
  if (!is.data.frame(data)) stop("`data` must be a data.frame", call. = FALSE)
  if (nrow(data) == 0L) {
    stop("`data` is empty. Fill morie_taphonomy_schema() with real comparanda; ",
         "this module never fabricates burial rows.", call. = FALSE)
  }
  for (col in c(treatment, outcome)) {
    if (!col %in% names(data)) {
      stop(sprintf("column '%s' not found in `data`", col), call. = FALSE)
    }
  }
  if (is.null(covariates)) {
    v <- .taphonomy_vars()
    wanted <- names(c(v$covariates, v$measurements))
    covariates <- intersect(wanted, names(data))
  }
  covariates <- setdiff(covariates, c(treatment, outcome))
  if (length(covariates) == 0L) {
    stop("no covariates available; supply environment/handling/measurement ",
         "columns (see morie_taphonomy_schema())", call. = FALSE)
  }

  warnings <- character(0)
  tt <- data[[treatment]]
  if (length(unique(stats::na.omit(tt))) < 2L) {
    warnings <- c(warnings, "treatment has no contrast (all treated or all ",
                  "control) -- the effect is not identified.")
  }

  # Normalise each estimator's output to a common summary (ate/se/ci/n/method).
  # IRM returns a list (cross-fit ATE + orthogonal SE); CATE returns a bare
  # per-unit numeric vector, summarised here to its mean + dispersion-based SE.
  cate_per_unit <- NULL
  cate_sd <- NULL
  if (estimator == "irm") {
    est <- morie_estimate_irm(data, treatment, outcome, covariates, ...)
    ate <- as.numeric(est$ate)
    se  <- as.numeric(est$se %||% NA_real_)
    ci_lower <- as.numeric(est$ci_lower %||% NA_real_)
    ci_upper <- as.numeric(est$ci_upper %||% NA_real_)
    n <- as.integer(est$n %||% nrow(data))
    method <- est$method %||% "IRM (DoubleML)"
  } else {
    tau <- as.numeric(morie_estimate_cate(data, treatment, outcome, covariates, ...))
    tau <- tau[is.finite(tau)]
    if (length(tau) == 0L) stop("CATE estimation returned no finite effects",
                                call. = FALSE)
    cate_per_unit <- tau
    n <- length(tau)
    ate <- mean(tau)
    cate_sd <- if (n > 1L) stats::sd(tau) else NA_real_  # effect heterogeneity
    method <- "CATE (meta-learner, mean of per-unit effects)"
    if (se_method == "bootstrap") {
      # Valid inference: resample rows and refit the whole CATE procedure, so
      # the SE reflects sampling + estimation uncertainty of the mean effect
      # (not the correlated per-unit dispersion).
      set.seed(boot_seed)
      nb <- as.integer(n_boot)
      boot <- vapply(seq_len(nb), function(b) {
        idx <- sample.int(nrow(data), replace = TRUE)
        tb <- as.numeric(morie_estimate_cate(
          data[idx, , drop = FALSE], treatment, outcome, covariates, ...))
        mean(tb[is.finite(tb)])
      }, numeric(1))
      boot <- boot[is.finite(boot)]
      se <- stats::sd(boot)
      qs <- stats::quantile(boot, c(0.025, 0.975), names = FALSE)
      ci_lower <- qs[1]
      ci_upper <- qs[2]
      method <- sprintf("CATE (meta-learner, mean; %d-boot SE/CI)", length(boot))
    } else {
      # Point summary only. sd(tau)/sqrt(n) is NOT reported as an SE: the
      # per-unit effects are correlated fitted predictions, so it would
      # understate the true uncertainty. Use se_method="bootstrap" for a valid
      # SE, or cate_sd for heterogeneity.
      se <- NA_real_
      ci_lower <- NA_real_
      ci_upper <- NA_real_
      warnings <- c(warnings, sprintf(
        paste0("CATE point summary: no SE reported (per-unit effects are ",
               "correlated fitted predictions; cate_sd=%.3f is heterogeneity, ",
               "range [%.3f, %.3f]). Pass se_method='bootstrap' for a valid SE ",
               "+ CI + p-value, or use estimator='irm'."),
        cate_sd, min(tau), max(tau)))
    }
  }

  # Wald p-value from a valid SE only. IRM's SE is a cross-fit orthogonal SE
  # (test is exact-normal in the limit); the CATE path has se = NA (its per-unit
  # spread is heterogeneity, not sampling error) so no p-value is claimed.
  p_value <- if (is.finite(se) && se > 0 && is.finite(ate)) {
    2 * stats::pnorm(-abs(ate / se))
  } else {
    NA_real_
  }

  # E-value on the standardized effect (continuous-outcome bridge).
  out_sd <- stats::sd(data[[outcome]], na.rm = TRUE)
  ev <- list(morie_e_value = NA_real_, e_value_ci = NA_real_)
  if (is.finite(out_sd) && out_sd > 0 && is.finite(ate)) {
    d  <- ate / out_sd
    rr <- .taphonomy_smd_to_rr(d)
    rr_lo <- if (is.finite(se) && se > 0) {
      .taphonomy_smd_to_rr((abs(ate) - 1.959964 * se) / out_sd)
    } else NULL
    if (!is.null(rr_lo) && rr_lo < 1) rr_lo <- 1  # CI crosses null
    ev <- morie_e_value(rr, rr_lower = rr_lo)
  } else {
    warnings <- c(warnings, "outcome has zero/undefined SD; E-value skipped.")
  }

  interp <- sprintf(
    paste0(
      "Preservation delta (ATE of %s on %s) = %.3f [%.3f, %.3f], n=%d, via %s. ",
      "An unmeasured cause would need an E-value of %s (association with both ",
      "treatment and outcome, on the risk-ratio scale) to fully explain it ",
      "away. This bounds -- it does not falsify -- any 'incorruptibility' claim: ",
      "a natural mechanism (e.g. quicklime desiccation) of this strength is ",
      "sufficient, but sufficiency is not proof of exclusivity."
    ),
    treatment, outcome, ate, ci_lower, ci_upper, n, method,
    if (is.finite(ev$morie_e_value)) sprintf("%.2f", ev$morie_e_value) else "NA"
  )

  list(
    value          = ate,
    se             = se,
    p_value        = p_value,
    ci_lower       = ci_lower,
    ci_upper       = ci_upper,
    n              = n,
    e_value        = ev$morie_e_value,
    e_value_ci     = ev$e_value_ci,
    estimator      = method,
    cate_per_unit  = cate_per_unit,
    cate_sd        = cate_sd,
    warnings       = warnings,
    interpretation = interp
  )
}
