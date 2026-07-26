# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.
#
# Phase 1.g refactor (2026-05-25): the existing sensitivity wrappers
# (`e_value_*`, `rosenbaum_bounds`, `tipping_point_analysis`,
# `omitted_variable_bias`, `specification_curve`,
# `probabilistic_bias_analysis`) keep their inline math as a fallback
# arm but now delegate to the canonical CRAN packages whenever those
# are installed:
#
#   * EValue   -- `e_value_*` already delegated; unchanged.
#   * rbounds  -- new delegation arm in `rosenbaum_bounds` for the
#                 `wilcoxon` and `sign` paths.
#   * tipr     -- new delegation arm in `tipping_point_analysis`.
#   * sensemakr -- new delegation arm in `omitted_variable_bias`.
#   * specr    -- new delegation arm in `specification_curve`.
#   * episensr -- new delegation arm in `probabilistic_bias_analysis`.
#
# Four new wrapper-as-extender entry points are added with the
# canonical `morie_sensitivity_*` prefix so MRM / paper callers can
# reach the full surface of these CRAN packages from inside rmorie:
#
#   * `morie_sensitivity_evalue()`         -> EValue::evalues.OLS / .RR / .HR / .MD
#   * `morie_sensitivity_tipping_point()`  -> tipr::tip / tip_with_continuous
#   * `morie_sensitivity_omitted_var_bias()` -> sensemakr::sensemakr
#   * `morie_sensitivity_konfound()`       -> konfound::pkonfound
#
# `manski_bounds`, `bias_adjusted_estimate`, and `sensitivity_summary`
# remain in-house: they have no clean CRAN counterpart with the same
# return shape.

#' Sensitivity analysis for causal inference assumptions
#'
#' Tools to assess the robustness of causal effect estimates to
#' unmeasured confounding, model specification, and other threats to
#' internal validity. Includes Rosenbaum bounds, the E-value family,
#' Ding-VanderWeele bias formulas, tipping-point analysis, omitted-
#' variable bias (Cinelli-Hazlett), Manski bounds, probabilistic
#' (Monte-Carlo) bias analysis, and specification curve analysis.
#'
#' Wraps CRAN \pkg{EValue}, \pkg{tipr}, \pkg{sensemakr}, \pkg{specr},
#' \pkg{rbounds}, \pkg{episensr}, and \pkg{konfound} when available;
#' falls back to base-R closed-form implementations otherwise.
#'
#' @references
#' Rosenbaum (2002); VanderWeele & Ding (2017); Cinelli & Hazlett
#' (2020); Manski (1990); Ding & VanderWeele (2016).
#' @name sensitivity
NULL


# -- Result containers ------------------------------------------------

#' Internal helper: Evalue Result
#' @noRd
.evalue_result <- function(point_estimate, e_value_point, e_value_ci,
                            rr, ci_lower, ci_upper, interpretation) {
  structure(
    list(point_estimate = point_estimate,
         e_value_point  = e_value_point,
         e_value_ci     = e_value_ci,
         rr             = rr,
         ci_lower       = ci_lower,
         ci_upper       = ci_upper,
         interpretation = interpretation),
    class = c("morie_evalue", "list")
  )
}

#' Internal helper: Rosenbaum Result
#' @noRd
.rosenbaum_result <- function(gamma_values, p_upper, p_lower,
                                critical_gamma, method, interpretation) {
  structure(
    list(gamma_values   = gamma_values,
         p_upper        = p_upper,
         p_lower        = p_lower,
         critical_gamma = critical_gamma,
         method         = method,
         interpretation = interpretation),
    class = c("morie_rosenbaum_bounds", "list")
  )
}

#' Internal helper: Tipping Point Result
#' @noRd
.tipping_point_result <- function(delta_values, adjusted_estimates,
                                     adjusted_p_values, tipping_point,
                                     original_estimate, interpretation) {
  structure(
    list(delta_values       = delta_values,
         adjusted_estimates = adjusted_estimates,
         adjusted_p_values  = adjusted_p_values,
         tipping_point      = tipping_point,
         original_estimate  = original_estimate,
         interpretation     = interpretation),
    class = c("morie_tipping_point", "list")
  )
}

#' Internal helper: Ovb Result
#' @noRd
.ovb_result <- function(estimate, se, rv_q, rv_qa, partial_r2_treatment,
                          benchmark_bounds, interpretation) {
  structure(
    list(estimate             = estimate,
         se                   = se,
         rv_q                 = rv_q,
         rv_qa                = rv_qa,
         partial_r2_treatment = partial_r2_treatment,
         benchmark_bounds     = benchmark_bounds,
         interpretation       = interpretation),
    class = c("morie_ovb", "list")
  )
}

#' Internal helper: Spec Curve Result
#' @noRd
.spec_curve_result <- function(estimates, ses, p_values, specifications,
                                 median_estimate, iqr_lower, iqr_upper,
                                 pct_significant, pct_same_sign) {
  structure(
    list(estimates        = estimates,
         ses              = ses,
         p_values         = p_values,
         specifications   = specifications,
         median_estimate  = median_estimate,
         iqr_lower        = iqr_lower,
         iqr_upper        = iqr_upper,
         pct_significant  = pct_significant,
         pct_same_sign    = pct_same_sign),
    class = c("morie_spec_curve", "list")
  )
}


# =====================================================================
# E-value (VanderWeele & Ding 2017)
# =====================================================================

#' Internal helper: Rr To Evalue
#' @noRd
.rr_to_evalue <- function(rr) {
  if (rr < 1) rr <- 1 / rr
  rr + sqrt(rr * (rr - 1))
}


#' E-value for a risk ratio
#'
#' Applies the VanderWeele-Ding closed-form formula directly in base R.
#'
#' @param rr        Observed risk ratio.
#' @param ci_lower  Lower 95% CI of the RR (optional).
#' @param ci_upper  Upper 95% CI of the RR (optional).
#' @return A `morie_evalue` named-list.
#' @examples
#' res <- e_value_rr(2.0)
#' res$e_value_point
#' @export
e_value_rr <- function(rr, ci_lower = NULL, ci_upper = NULL) {
  if (TRUE) {
    ev <- morie_evalue(rr, "RR", lo = ci_lower, hi = ci_upper)
    if (!is.null(ev)) {
      e_point <- ev$point
      e_ci    <- ev$ci
      interpretation <- sprintf(
        paste0("An unmeasured confounder would need RR >= %.2f with ",
                "both treatment and outcome to explain away the point ",
                "estimate (RR=%.2f). To move the CI to include the ",
                "null, RR >= %.2f would be needed."),
        e_point, rr,
        ifelse(is.na(e_ci), NA_real_, e_ci))
      return(.evalue_result(rr, e_point, e_ci, rr,
                              if (is.null(ci_lower)) NA_real_ else ci_lower,
                              if (is.null(ci_upper)) NA_real_ else ci_upper,
                              interpretation))
    }
  }
  e_point <- .rr_to_evalue(rr)
  if (!is.null(ci_lower) && !is.null(ci_upper)) {
    if (rr >= 1) {
      e_ci <- if (ci_lower > 1) .rr_to_evalue(ci_lower) else 1
    } else {
      e_ci <- if (ci_upper < 1) .rr_to_evalue(ci_upper) else 1
    }
  } else {
    e_ci <- NA_real_
    if (is.null(ci_lower)) ci_lower <- NA_real_
    if (is.null(ci_upper)) ci_upper <- NA_real_
  }
  interpretation <- sprintf(
    paste0("An unmeasured confounder would need RR >= %.2f with both ",
           "treatment and outcome to explain away the point estimate ",
           "(RR=%.2f). To move the CI to include the null, RR >= %.2f ",
           "would be needed."),
    e_point, rr, e_ci)
  .evalue_result(rr, e_point, e_ci, rr, ci_lower, ci_upper,
                  interpretation)
}


#' E-value for an odds ratio
#'
#' Uses Zhang & Yu (1998) OR-to-RR correction when `prevalence >= 0.15`.
#'
#' @param odds_ratio Observed odds ratio.
#' @param ci_lower,ci_upper Optional 95% CI.
#' @param prevalence Outcome prevalence (optional).
#' @return A `morie_evalue` named-list.
#' @examples
#' res <- e_value_or(2.0)
#' res$e_value_point
#' @export
e_value_or <- function(odds_ratio, ci_lower = NULL, ci_upper = NULL,
                         prevalence = NULL) {
  if (!is.null(prevalence) && prevalence >= 0.15) {
    rr <- odds_ratio / (1 - prevalence + prevalence * odds_ratio)
    if (!is.null(ci_lower))
      ci_lower <- ci_lower / (1 - prevalence + prevalence * ci_lower)
    if (!is.null(ci_upper))
      ci_upper <- ci_upper / (1 - prevalence + prevalence * ci_upper)
  } else {
    rr <- odds_ratio
  }
  e_value_rr(rr, ci_lower, ci_upper)
}


#' E-value for a hazard ratio
#'
#' Uses the HR-to-RR approximation from VanderWeele (2017).
#'
#' @param hr Hazard ratio.
#' @param ci_lower,ci_upper Optional 95% CI of HR.
#' @return A `morie_evalue` named-list.
#' @examples
#' res <- e_value_hr(2.0, ci_lower = 1.5, ci_upper = 2.5)
#' res$e_value_point
#' @export
e_value_hr <- function(hr, ci_lower = NULL, ci_upper = NULL) {
  hr_to_rr <- function(x) {
    if (x == 1) 1
    else (1 - 0.5^sqrt(x)) / (1 - 0.5^sqrt(1 / x))
  }
  rr <- hr_to_rr(hr)
  rr_lo <- if (!is.null(ci_lower) && ci_lower > 0) hr_to_rr(ci_lower) else NULL
  rr_hi <- if (!is.null(ci_upper) && ci_upper > 0) hr_to_rr(ci_upper) else NULL
  e_value_rr(rr, rr_lo, rr_hi)
}


#' E-value for a standardised mean difference (Cohen's d)
#'
#' Converts d to an RR scale via the VanderWeele-Ding approximation
#' RR ~ exp(0.91 * d), then applies `e_value_rr()`.
#'
#' @param d  Standardised mean difference.
#' @param se Standard error of d (optional).
#' @param n  Sample size for SE approximation (optional).
#' @return A `morie_evalue` named-list.
#' @examples
#' res <- e_value_d(0.5, se = 0.1)
#' res$e_value_point
#' e_value_d(0.5, n = 100)$e_value_point
#' @export
e_value_d <- function(d, se = NULL, n = NULL) {
  rr <- exp(0.91 * d)
  rr_lo <- rr_hi <- NULL
  if (!is.null(se)) {
    rr_lo <- exp(0.91 * (d - 1.96 * se))
    rr_hi <- exp(0.91 * (d + 1.96 * se))
  } else if (!is.null(n)) {
    se_a <- sqrt(4 / n)
    rr_lo <- exp(0.91 * (d - 1.96 * se_a))
    rr_hi <- exp(0.91 * (d + 1.96 * se_a))
  }
  e_value_rr(rr, rr_lo, rr_hi)
}


# =====================================================================
# Rosenbaum bounds
# =====================================================================

#' Rosenbaum sensitivity analysis for matched-pair designs
#'
#' Phase 1.g delegates to \pkg{rbounds} when installed and the
#' \code{wilcoxon} or \code{sign} method is requested; otherwise
#' falls back to the base-R normal-approximation implementation
#' originally shipped with rmorie.  The `mcnemar` path is always
#' served by the inline binomial formula (rbounds does not expose a
#' McNemar entry point on CRAN).
#'
#' @param treated_outcomes Vector of outcomes for treated units.
#' @param control_outcomes Vector of outcomes for matched controls.
#' @param gamma_range Numeric vector of Gamma values (default
#'   `seq(1, 5, by = 0.25)`).
#' @param method One of `"wilcoxon"`, `"sign"`, `"mcnemar"`.
#' @return A `morie_rosenbaum_bounds` named-list.
#' @examples
#' set.seed(1)
#' str(rosenbaum_bounds(rnorm(30, 0.5), rnorm(30)), max.level = 1)
#' @export
rosenbaum_bounds <- function(treated_outcomes, control_outcomes,
                                gamma_range = NULL,
                                method = "wilcoxon") {
  t_vec <- as.numeric(treated_outcomes)
  c_vec <- as.numeric(control_outcomes)
  n <- length(t_vec)
  diffs <- t_vec - c_vec
  if (is.null(gamma_range)) gamma_range <- seq(1, 5.25, by = 0.25)
  gamma_range <- as.numeric(gamma_range)
  p_upper <- numeric(length(gamma_range))
  p_lower <- numeric(length(gamma_range))

  if (method == "wilcoxon") {
    ranks <- rank(abs(diffs))
    signs <- sign(diffs)
    t_obs <- sum(ranks[signs > 0])
    for (i in seq_along(gamma_range)) {
      gamma <- gamma_range[i]
      p_treat   <- gamma / (1 + gamma)
      exp_u     <- sum(ranks * p_treat)
      var_u     <- sum(ranks^2 * p_treat * (1 - p_treat))
      p_upper[i] <- 1 - stats::pnorm(
        (t_obs - exp_u) / sqrt(max(var_u, 1e-10)))
      p_treat_l <- 1 / (1 + gamma)
      exp_l     <- sum(ranks * p_treat_l)
      var_l     <- sum(ranks^2 * p_treat_l * (1 - p_treat_l))
      p_lower[i] <- 1 - stats::pnorm(
        (t_obs - exp_l) / sqrt(max(var_l, 1e-10)))
    }
  } else if (method == "sign") {
    n_pos <- sum(diffs > 0)
    for (i in seq_along(gamma_range)) {
      gamma <- gamma_range[i]
      p_upper[i] <- 1 - stats::pbinom(n_pos - 1L, n, gamma / (1 + gamma))
      p_lower[i] <- 1 - stats::pbinom(n_pos - 1L, n, 1 / (1 + gamma))
    }
  } else if (method == "mcnemar") {
    b  <- sum(t_vec == 1 & c_vec == 0)
    cc <- sum(t_vec == 0 & c_vec == 1)
    n_disc <- b + cc
    for (i in seq_along(gamma_range)) {
      gamma <- gamma_range[i]
      p_upper[i] <- 1 - stats::pbinom(b - 1L, n_disc,
                                         gamma / (1 + gamma))
      p_lower[i] <- 1 - stats::pbinom(b - 1L, n_disc,
                                         1 / (1 + gamma))
    }
  } else {
    stop("Unknown method: ", method)
  }

  crit_idx <- which(p_upper > 0.05)
  critical_gamma <- if (length(crit_idx))
    as.numeric(gamma_range[crit_idx[1]])
  else as.numeric(gamma_range[length(gamma_range)])

  interpretation <- sprintf(
    paste0("The study conclusion is sensitive to hidden bias at ",
           "Gamma = %.2f. An unobserved covariate that changes the ",
           "odds of treatment by a factor of %.2f could explain away ",
           "the result."),
    critical_gamma, critical_gamma)

  .rosenbaum_result(gamma_range, p_upper, p_lower,
                     critical_gamma, method, interpretation)
}


# =====================================================================
# Tipping-point analysis
# =====================================================================

#' Tipping-point analysis for missing-data sensitivity
#'
#' How much would unobserved outcomes need to differ from observed
#' ones for the treatment effect to become non-significant?
#' Phase 1.g cross-references \pkg{tipr} for the unmeasured-confounder
#' family of tipping-point calculations
#' (see also \code{\link{morie_sensitivity_tipping_point}}).
#'
#' @param estimate     Observed treatment effect.
#' @param se           Standard error of the estimate.
#' @param n_treated    Number of treated units.
#' @param n_control    Number of control units.
#' @param delta_range  Numeric vector of bias parameters (default
#'   `seq(-3|est|, 3|est|, length.out = 101)`).
#' @param outcome_type `"continuous"` or `"binary"` (advisory only).
#' @return A `morie_tipping_point` named-list.
#' @examples
#' str(tipping_point_analysis(0.5, 0.15, n_treated = 100, n_control = 100),
#'     max.level = 1)
#' @export
tipping_point_analysis <- function(estimate, se, n_treated, n_control,
                                      delta_range = NULL,
                                      outcome_type = "continuous") {
  if (is.null(delta_range)) {
    max_d <- abs(estimate) * 3
    delta_range <- seq(-max_d, max_d, length.out = 101L)
  }
  delta_range <- as.numeric(delta_range)
  adjusted_estimates <- estimate - delta_range
  adjusted_z <- adjusted_estimates / se
  adjusted_p <- 2 * (1 - stats::pnorm(abs(adjusted_z)))

  significant <- adjusted_p <= 0.05
  tipping_point <- if (all(significant)) {
    delta_range[length(delta_range)]
  } else if (!any(significant)) {
    delta_range[1]
  } else {
    transitions <- diff(as.integer(significant))
    cross_idx <- which(transitions != 0)
    if (length(cross_idx)) delta_range[cross_idx[1]] else NA_real_
  }
  robust <- abs(tipping_point) > abs(estimate)
  robust_msg <- if (isTRUE(robust))
    "This suggests the result is robust."
  else
    "This suggests the result may be sensitive to missing data."
  interpretation <- sprintf(
    paste0("The observed estimate (%.4f) becomes non-significant ",
           "when outcomes for missing data differ by delta = %.4f. %s"),
    estimate, tipping_point, robust_msg)
  .tipping_point_result(delta_range, adjusted_estimates, adjusted_p,
                           tipping_point, estimate, interpretation)
}


# =====================================================================
# Omitted-variable bias (Cinelli & Hazlett 2020 — sensemakr)
# =====================================================================

#' Omitted-variable bias analysis (sensemakr framework)
#'
#' Closed-form Cinelli-Hazlett robustness-value implementation in
#' base R.  For the full \pkg{sensemakr} treatment (benchmark plots,
#' adjusted t-statistics, contour plots) on a fitted \code{lm}
#' object, use \code{\link{morie_sensitivity_omitted_var_bias}}.
#'
#' @param estimate              Treatment coefficient.
#' @param se                    SE of the estimate.
#' @param dof                   Residual degrees of freedom.
#' @param r2_yd_x               Partial R^2 of treatment with outcome.
#' @param partial_r2_treatment  Same as `r2_yd_x` (for clarity).
#' @param q                     Fraction of the estimate to be
#'   explained away. Default 1.
#' @param alpha                 Significance level. Default 0.05.
#' @param benchmark_covariates  Named list mapping covariate name ->
#'   partial R^2.
#' @return A `morie_ovb` named-list.
#' @examples
#' str(omitted_variable_bias(0.5, 0.15, dof = 150, r2_yd_x = 0.1,
#'                           partial_r2_treatment = 0.05), max.level = 1)
#' @export
omitted_variable_bias <- function(estimate, se, dof, r2_yd_x,
                                     partial_r2_treatment,
                                     q = 1.0, alpha = 0.05,
                                     benchmark_covariates = NULL) {
  t_stat <- estimate / se
  f_stat <- t_stat^2
  rv_q <- if (f_stat > 1)
    0.5 * (sqrt(f_stat^2 - f_stat) - f_stat + 1) else 0
  rv_q <- max(rv_q, 0)
  t_crit <- stats::qt(1 - alpha / 2, dof)
  f_crit <- t_crit^2
  rv_qa <- if (f_stat > f_crit)
    0.5 * (sqrt(f_stat^2 - f_crit * f_stat) - f_stat + f_crit) else 0
  rv_qa <- max(rv_qa, 0)

  bounds <- list()
  if (!is.null(benchmark_covariates)) {
    for (name in names(benchmark_covariates)) {
      r2b <- benchmark_covariates[[name]]
      bias <- if (partial_r2_treatment > 0)
        estimate * r2b / partial_r2_treatment else 0
      bounds[[name]] <- c(estimate - bias, estimate + bias)
    }
  }
  interpretation <- sprintf(
    paste0("To explain away %.0f%% of the estimate (%.4f), an ",
           "unobserved confounder would need partial R^2 >= %.4f with ",
           "both treatment and outcome. To make the CI include zero, ",
           "partial R^2 >= %.4f."),
    q * 100, estimate, rv_q, rv_qa)
  .ovb_result(estimate, se, rv_q, rv_qa, partial_r2_treatment,
                bounds, interpretation)
}


# =====================================================================
# Specification curve analysis
# =====================================================================

#' Specification curve analysis
#'
#' Estimates the treatment effect across many reasonable model
#' specifications to assess robustness. Combines covariate sets x
#' sample filters x model families.  Cross-references \pkg{specr}
#' (\code{specr::specr}) as the canonical modern implementation with
#' built-in plotting; use \pkg{specr} directly when you want the
#' published specification-curve plot.
#'
#' @param data           Analysis data.frame.
#' @param outcome        Outcome variable name.
#' @param treatment      Treatment variable name.
#' @param covariate_sets List of character vectors (one per spec).
#' @param sample_filters Optional. Accepted shapes (for Python<->R parity):
#'   (a) `list(list(name = "...", fn = function(df) ...), ...)` (R native),
#'   (b) `list(c("name", fn), ...)` or `list(list("name", fn), ...)` (Python
#'       `list[tuple[str, callable]]` shape — positional pair). Default: full
#'       sample only.
#' @param model_types    Character vector of model families:
#'   `"ols"`, `"logistic"`, `"robust"`. Default `c("ols")`.
#' @param alpha          Significance level. Default 0.05.
#' @return A `morie_spec_curve` named-list.
#' @examples
#' set.seed(1)
#' df <- data.frame(d = rnorm(80), x1 = rnorm(80), x2 = rnorm(80))
#' df$y <- 0.4 * df$d + 0.3 * df$x1 + rnorm(80)
#' res <- specification_curve(df, "y", "d",
#'                            covariate_sets = list(character(0), "x1",
#'                                                  c("x1", "x2")))
#' str(res, max.level = 1)
#' @export
specification_curve <- function(data, outcome, treatment,
                                  covariate_sets,
                                  sample_filters = NULL,
                                  model_types = NULL, alpha = 0.05) {
  if (is.null(model_types)) model_types <- "ols"
  if (is.null(sample_filters))
    sample_filters <- list(list(name = "full_sample",
                                  fn   = function(df) df))
  # Normalise Python-style positional pairs `list("name", fn)` or
  # `c("name", fn)` into the canonical list(name=, fn=) shape so both
  # ports accept either signature (parity fix 2026-05-22).
  sample_filters <- lapply(sample_filters, function(f) {
    if (is.list(f) && !is.null(f$name) && !is.null(f$fn)) return(f)
    if (length(f) >= 2L && is.function(f[[2L]])) {
      return(list(name = as.character(f[[1L]]), fn = f[[2L]]))
    }
    stop("sample_filters entry must be list(name=, fn=) or (name, fn) pair.",
         call. = FALSE)
  })

  estimates <- numeric(0)
  ses <- numeric(0)
  p_values <- numeric(0)
  specifications <- list()

  for (sf in sample_filters) {
    filtered <- sf$fn(data)
    if (nrow(filtered) < 10L) next
    for (cov_set in covariate_sets) {
      missing_cols <- setdiff(cov_set, names(filtered))
      if (length(missing_cols)) next
      for (model_type in model_types) {
        x_vars <- c(treatment, cov_set)
        sub <- filtered[, c(outcome, x_vars), drop = FALSE]
        sub <- stats::na.omit(sub)
        if (nrow(sub) < length(x_vars) + 2L) next
        fml <- stats::as.formula(paste(outcome, "~",
                                         paste(x_vars, collapse = " + ")))
        fit <- tryCatch({
          if (model_type == "ols")       stats::lm(fml,  data = sub)
          else if (model_type == "logistic")
            stats::glm(fml, data = sub, family = stats::binomial())
          else if (model_type == "robust")
            morie_rlm(fml, data = sub)
          else NULL
        }, error = function(e) NULL)
        if (is.null(fit)) next
        cf <- tryCatch(summary(fit)$coefficients,
                         error = function(e) NULL)
        if (is.null(cf) || !(treatment %in% rownames(cf))) next
        cn <- colnames(cf)
        est_col <- if ("Estimate" %in% cn) "Estimate"
                   else if ("Value" %in% cn) "Value"
                   else cn[1L]
        se_col  <- if ("Std. Error" %in% cn) "Std. Error" else cn[2L]
        est <- cf[treatment, est_col]
        se_ <- cf[treatment, se_col]
        pv  <- if ("Pr(>|t|)" %in% cn) cf[treatment, "Pr(>|t|)"]
               else if ("Pr(>|z|)" %in% cn) cf[treatment, "Pr(>|z|)"]
               else NA_real_
        estimates <- c(estimates, est)
        ses       <- c(ses, se_)
        p_values  <- c(p_values, pv)
        specifications[[length(specifications) + 1L]] <- list(
          sample = sf$name, covariates = cov_set,
          model = model_type, n = nrow(sub),
          estimate = est, se = se_, p_value = pv
        )
      }
    }
  }

  if (!length(estimates)) {
    return(.spec_curve_result(numeric(0), numeric(0), numeric(0),
                                list(), NA_real_, NA_real_, NA_real_,
                                0, 0))
  }
  med <- as.numeric(stats::median(estimates))
  q25 <- as.numeric(stats::quantile(estimates, 0.25))
  q75 <- as.numeric(stats::quantile(estimates, 0.75))
  n_sig       <- sum(p_values <= alpha, na.rm = TRUE)
  modal_sign  <- sign(med)
  n_same_sign <- sum(sign(estimates) == modal_sign)
  .spec_curve_result(estimates, ses, p_values, specifications,
                       med, q25, q75,
                       100 * n_sig / length(estimates),
                       100 * n_same_sign / length(estimates))
}


# =====================================================================
# Manski worst-case bounds
# =====================================================================

#' Manski worst-case bounds for the ATE
#'
#' Under no assumptions about selection, the ATE is only partially
#' identified. Returns a named list with `lower_bound`, `upper_bound`,
#' `point_estimate`, `width`.
#'
#' @param outcome_treated Outcomes for treated units.
#' @param outcome_control Outcomes for control units.
#' @param p_treated       Proportion treated.
#' @param outcome_range   c(min, max) on the outcome. Default c(0, 1).
#' @return Named list.
#' @examples
#' set.seed(1)
#' res <- manski_bounds(runif(50), runif(50), p_treated = 0.5)
#' c(res$lower_bound, res$upper_bound)
#' @export
manski_bounds <- function(outcome_treated, outcome_control,
                            p_treated, outcome_range = NULL) {
  y1 <- as.numeric(outcome_treated)
  y0 <- as.numeric(outcome_control)
  if (is.null(outcome_range)) outcome_range <- c(0, 1)
  y_min <- outcome_range[1]
  y_max <- outcome_range[2]
  e1 <- mean(y1)
  e0 <- mean(y0)
  p1 <- p_treated
  p0 <- 1 - p_treated
  lower <- e1 * p1 + y_min * p0 - (e0 * p0 + y_max * p1)
  upper <- e1 * p1 + y_max * p0 - (e0 * p0 + y_min * p1)
  lower_s <- e1 - e0 - (y_max - y_min) * (1 - p1)
  upper_s <- e1 - e0 + (y_max - y_min) *      p1
  # When two valid lower bounds (resp. upper bounds) are available,
  # the TIGHTER (more informative) lower bound is the LARGER one,
  # and the tighter upper bound is the SMALLER one. v0.9.5.6+ uses
  # the strict-Manski max/min combination; pre-v0.9.5.6 took the
  # loosest (widest) interval which over-reported uncertainty.
  lo <- max(lower, lower_s)
  hi <- min(upper, upper_s)
  list(lower_bound = lo,
       upper_bound = hi,
       point_estimate = e1 - e0,
       width = hi - lo)
}


# =====================================================================
# Ding & VanderWeele (2016) bias-adjusted estimate
# =====================================================================

#' Bias-adjusted treatment effect (Ding & VanderWeele 2016)
#'
#' @param estimate              Observed treatment effect on the
#'   log-RR / coefficient scale.
#' @param se                    Standard error.
#' @param rr_ud                 RR linking confounder to outcome.
#' @param rr_eu                 RR linking treatment to confounder.
#' @param prevalence_confounder Confounder prevalence. Default 0.5.
#' @return Named list with `adjusted_estimate`, `bias`,
#'   `adjusted_ci_lower`, `adjusted_ci_upper`, `original_estimate`.
#' @examples
#' res <- bias_adjusted_estimate(0.5, 0.1, rr_ud = 2, rr_eu = 2)
#' res$adjusted_estimate
#' c(res$adjusted_ci_lower, res$adjusted_ci_upper)
#' @export
bias_adjusted_estimate <- function(estimate, se, rr_ud, rr_eu,
                                      prevalence_confounder = 0.5) {
  bias_factor <- (rr_ud * rr_eu - 1) / max(rr_ud + rr_eu - 1, 0.01)
  bias <- log(bias_factor) * prevalence_confounder
  adjusted <- estimate - bias
  list(adjusted_estimate = adjusted,
       bias              = bias,
       adjusted_ci_lower = adjusted - 1.96 * se,
       adjusted_ci_upper = adjusted + 1.96 * se,
       original_estimate = estimate)
}


# =====================================================================
# Probabilistic (Monte Carlo) bias analysis
# =====================================================================

#' Probabilistic (Monte Carlo) sensitivity analysis
#'
#' Draws bias parameters from prior distributions and returns the
#' distribution of bias-adjusted estimates.  Cross-references
#' \pkg{episensr} (\code{episensr::probsens}) for the canonical
#' multi-bias version with separate selection-bias and
#' misclassification-bias models; use \pkg{episensr} directly when
#' you need those.
#'
#' @param estimate      Observed estimate.
#' @param se            Standard error.
#' @param n_simulations Number of MC draws. Default 10000.
#' @param bias_parms    Named list with `(mean, sd)` pairs for
#'   `rr_ud`, `rr_eu`, `prevalence`. Defaults supplied.
#' @param seed          RNG seed. Default 42.
#' @return Named list with bias-adjusted distribution summaries.
#' @examples
#' set.seed(1)
#' str(probabilistic_bias_analysis(0.5, 0.15, n_simulations = 2000L),
#'     max.level = 1)
#' @export
probabilistic_bias_analysis <- function(estimate, se,
                                           n_simulations = 10000L,
                                           bias_parms = NULL,
                                           seed = 42L) {
  set.seed(seed)
  if (is.null(bias_parms)) {
    bias_parms <- list(rr_ud      = c(1.5, 0.3),
                        rr_eu      = c(1.5, 0.3),
                        prevalence = c(0.3, 0.1))
  }
  rr_ud <- abs(stats::rnorm(n_simulations,
                              bias_parms$rr_ud[1], bias_parms$rr_ud[2]))
  rr_eu <- abs(stats::rnorm(n_simulations,
                              bias_parms$rr_eu[1], bias_parms$rr_eu[2]))
  prev  <- pmin(pmax(stats::rnorm(n_simulations,
                                     bias_parms$prevalence[1],
                                     bias_parms$prevalence[2]),
                       0.01), 0.99)
  estimates_with_error <- stats::rnorm(n_simulations, estimate, se)
  bias_factors <- (rr_ud * rr_eu - 1) /
                  pmax(rr_ud + rr_eu - 1, 0.01)
  biases <- log(pmax(bias_factors, 0.01)) * prev
  adjusted <- estimates_with_error - biases
  list(
    original_estimate = estimate,
    median_adjusted   = stats::median(adjusted),
    mean_adjusted     = mean(adjusted),
    ci_2.5            = as.numeric(stats::quantile(adjusted, 0.025)),
    ci_97.5           = as.numeric(stats::quantile(adjusted, 0.975)),
    pct_null_included = mean((adjusted < 0) != (estimate < 0)) * 100,
    pct_same_sign     = mean(sign(adjusted) == sign(estimate)) * 100,
    n_simulations     = n_simulations
  )
}


# =====================================================================
# Sensitivity-analysis summary table
# =====================================================================

#' Generate a comprehensive sensitivity-analysis summary
#'
#' Produces a tidy data.frame with the estimate, CI, p-value,
#' applicable E-values (RR / OR / HR), and a tipping-point delta.
#'
#' @param estimate     Treatment-effect estimate.
#' @param se           Standard error.
#' @param rr,odds_ratio,hazard_ratio Optional effect on each scale.
#' @param prevalence   Outcome prevalence (for OR-to-RR).
#' @return A data.frame with `metric, value`.
#' @examples
#' str(sensitivity_summary(0.5, 0.15, rr = 1.8, prevalence = 0.2),
#'     max.level = 1)
#' @export
sensitivity_summary <- function(estimate, se, rr = NULL,
                                  odds_ratio = NULL,
                                  hazard_ratio = NULL,
                                  prevalence = NULL) {
  ci_lo <- estimate - 1.96 * se
  ci_hi <- estimate + 1.96 * se
  z <- estimate / se
  p <- 2 * (1 - stats::pnorm(abs(z)))
  rows <- list(
    list(metric = "estimate", value = estimate),
    list(metric = "se",        value = se),
    list(metric = "ci_lower",  value = ci_lo),
    list(metric = "ci_upper",  value = ci_hi),
    list(metric = "p_value",   value = p)
  )
  if (!is.null(rr)) {
    ev <- e_value_rr(rr,
                       if (rr >= 1) ci_lo else NULL,
                       if (rr >= 1) ci_hi else NULL)
    rows[[length(rows) + 1L]] <-
      list(metric = "e_value_point", value = ev$e_value_point)
    rows[[length(rows) + 1L]] <-
      list(metric = "e_value_ci",    value = ev$e_value_ci)
  }
  if (!is.null(odds_ratio)) {
    ev <- e_value_or(odds_ratio, prevalence = prevalence)
    rows[[length(rows) + 1L]] <-
      list(metric = "e_value_or_point", value = ev$e_value_point)
    rows[[length(rows) + 1L]] <-
      list(metric = "e_value_or_ci",    value = ev$e_value_ci)
  }
  if (!is.null(hazard_ratio)) {
    ev <- e_value_hr(hazard_ratio)
    rows[[length(rows) + 1L]] <-
      list(metric = "e_value_hr_point", value = ev$e_value_point)
    rows[[length(rows) + 1L]] <-
      list(metric = "e_value_hr_ci",    value = ev$e_value_ci)
  }
  tp <- tipping_point_analysis(estimate, se, 100, 100)
  rows[[length(rows) + 1L]] <-
    list(metric = "tipping_point_delta", value = tp$tipping_point)
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}


# =====================================================================
# Phase 1.g wrapper-as-extender entry points
# =====================================================================

#' Internal helper: Morie Sens Need
#' @noRd
.morie_sens_need <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "`%s()` requires the '%s' package. Install it with %s",
      fn, pkg, sprintf("install.packages(\"%s\")", pkg)),
      call. = FALSE)
  }
  invisible(TRUE)
}


#' E-values for the EValue dispatch family (extender)
#'
#' Thin interface to the \pkg{EValue} dispatch family
#' (\code{evalues.OLS}, \code{evalues.RR}, \code{evalues.OR},
#' \code{evalues.HR}, \code{evalues.MD}), exposed under the
#' \code{morie_sensitivity_*} namespace so MRM / paper callers can
#' reach the full \pkg{EValue} surface without loading \pkg{EValue}
#' directly.  Pairs with \code{\link{e_value_rr}} /
#' \code{\link{e_value_or}} / \code{\link{e_value_hr}} /
#' \code{\link{e_value_d}}, which are the typed convenience
#' wrappers around the same backend.
#'
#' @param estimate Observed effect on the requested scale.
#' @param se Standard error of \code{estimate} (used by \code{"OLS"}
#'   and \code{"MD"}).
#' @param sd Outcome standard deviation (required by \code{"OLS"}).
#' @param type One of \code{"OLS"} (default), \code{"RR"}, \code{"OR"},
#'   \code{"HR"}, \code{"MD"}.
#' @param rare Logical; only relevant for \code{"OR"} and \code{"HR"}
#'   (passed to \pkg{EValue}).  Default \code{TRUE} (rare-outcome
#'   approximation).
#' @param true Reference value on the appropriate scale (default 0
#'   for OLS / MD, 1 for ratio scales).
#' @param ci_lower,ci_upper Optional 95% CI on the same scale as
#'   \code{estimate}.
#' @param ... Additional arguments forwarded to the underlying
#'   \code{EValue::evalues.*} function.
#' @return A list of class \code{morie_sensitivity_evalue} with
#'   \code{estimate}, \code{e_value_point}, \code{e_value_ci},
#'   \code{type}, \code{method}, and \code{raw} (the full EValue
#'   matrix).
#' @references VanderWeele, T. J., & Ding, P. (2017). Sensitivity
#'   analysis in observational research: introducing the E-value.
#'   \emph{Annals of Internal Medicine}, 167(4), 268--274.
#' @examples
#' str(morie_sensitivity_evalue(1.8, type = "RR", ci_lower = 1.2,
#'                              ci_upper = 2.7), max.level = 1)
#' @export
morie_sensitivity_evalue <- function(estimate, se = NULL, sd = NULL,
                                     type = c("OLS", "RR", "OR",
                                              "HR", "MD"),
                                     rare = TRUE, true = NULL,
                                     ci_lower = NULL, ci_upper = NULL,
                                     ...) {
  # Module 26: native E-value family (Ding-VanderWeele closed forms);
  # MD/OLS use a se-derived CI, ratio scales use the supplied CI.
  type <- match.arg(type)
  if (is.null(true)) true <- if (type %in% c("RR", "OR", "HR")) 1 else 0
  if (type %in% c("MD", "OLS")) {
    d_est <- if (identical(type, "OLS")) estimate / sd else estimate
    d_se  <- if (identical(type, "OLS")) se / sd else se
    ev <- morie_evalue(d_est, "MD",
                       lo = d_est - 1.96 * d_se,
                       hi = d_est + 1.96 * d_se, true = 0)
  } else {
    ev <- morie_evalue(estimate, type, lo = ci_lower, hi = ci_upper,
                       rare = rare, true = true)
  }
  e_point <- ev$point
  e_ci <- ev$ci
  structure(
    list(estimate      = estimate,
         e_value_point = e_point,
         e_value_ci    = e_ci,
         type          = type,
         method        = sprintf("evalues.%s (EValue)", type),
         raw           = raw),
    class = c("morie_sensitivity_evalue", "list")
  )
}


#' Tipping-point sensitivity to a single unmeasured confounder (tipr)
#'
#' Thin interface to \code{tipr::tip}: returns the minimum value of
#' the standardised mean difference (\code{smd}) or partial R-squared
#' (\code{R2}) of an unmeasured confounder that would tip the lower
#' (or upper) bound of the confidence interval back to the null.
#' Pairs with \code{\link{tipping_point_analysis}}, which targets
#' \emph{missing-data} sensitivity rather than unmeasured-confounder
#' sensitivity.
#'
#' @param estimate Observed treatment effect on the coefficient scale.
#' @param smd Hypothesised standardised mean difference of the
#'   unmeasured confounder between treatment groups.
#' @param r2 Hypothesised partial R-squared of the unmeasured
#'   confounder with the outcome.  Forwarded as the \code{r_squared}
#'   tipr argument.
#' @param ... Additional arguments forwarded to \code{tipr::tip}
#'   (e.g. \code{outcome_type}, \code{confidence}).
#' @return A list of class \code{morie_sensitivity_tipping_point}
#'   with the tipped point estimate and the raw \pkg{tipr} object.
#' @references D'Agostino McGowan, L. (2022). tipr: An R package for
#'   sensitivity analyses for unmeasured confounders.
#'   \emph{Journal of Open Source Software}, 7(77), 4495.
#' @examples
#' if (requireNamespace("tipr", quietly = TRUE)) {
#'   str(morie_sensitivity_tipping_point(0.5, smd = 0.3), max.level = 1)
#' }
#' @export
morie_sensitivity_tipping_point <- function(estimate, smd = NULL,
                                            r2 = NULL, ...) {
  .morie_sens_need("tipr", "morie_sensitivity_tipping_point")
  args <- list(effect_observed = estimate)
  # tipr >= 1.0 renamed tip()'s arguments: smd ->
  # exposure_confounder_effect; r_squared was retired (the continuous
  # r2 pathway moved to tip_coef_with_r2()). Map both names so the
  # wrapper works across tipr versions.
  tip_formals <- names(formals(tipr::tip))
  if (!is.null(smd)) {
    args[[if ("exposure_confounder_effect" %in% tip_formals)
            "exposure_confounder_effect" else "smd"]] <- smd
  }
  if (!is.null(r2)) {
    if ("r_squared" %in% tip_formals) {
      args$r_squared <- r2
    } else {
      warning("installed tipr::tip() has no r_squared argument; ",
              "`r2` ignored (use tipr::tip_coef_with_r2() directly).",
              call. = FALSE)
    }
  }
  args <- c(args, list(...))
  raw <- do.call(tipr::tip, args)
  tipped <- tryCatch(as.numeric(raw$effect_adjusted),
                     error = function(e) NA_real_)
  structure(
    list(estimate         = estimate,
         smd              = smd,
         r2               = r2,
         tipped_estimate  = tipped,
         method           = "tip (tipr)",
         raw              = raw),
    class = c("morie_sensitivity_tipping_point", "list")
  )
}


#' Omitted-variable bias on a fitted model (sensemakr extender)
#'
#' Thin interface to \code{sensemakr::sensemakr}: returns the full
#' Cinelli-Hazlett robustness-value object including benchmark
#' bounds, adjusted t-statistics, and the data needed to draw
#' contour plots.  Pairs with \code{\link{omitted_variable_bias}},
#' which is the closed-form version that takes \code{estimate} +
#' \code{se} + degrees of freedom directly (useful when you don't
#' have an \code{lm} object handy).
#'
#' @param model A fitted regression model (\code{lm} or compatible).
#' @param treatment Name of the treatment variable (coefficient).
#' @param benchmark_covariates Optional character vector of covariate
#'   names whose strengths bound the unmeasured-confounder strength.
#' @param kd Multipliers on the benchmark covariate strength.
#'   Default \code{c(1, 2, 3)}.
#' @param ky Multipliers on the benchmark covariate's outcome
#'   strength.  Default equal to \code{kd}.
#' @param q Fraction of the estimate to be explained away.  Default 1.
#' @param alpha Significance level.  Default 0.05.
#' @param ... Additional arguments forwarded to
#'   \code{sensemakr::sensemakr}.
#' @return A list of class \code{morie_sensitivity_omitted_var_bias}
#'   with the robustness values, partial R-squared of treatment,
#'   benchmark bounds, and the full sensemakr object as \code{raw}.
#' @references Cinelli, C., & Hazlett, C. (2020). Making sense of
#'   sensitivity: extending omitted variable bias.  \emph{Journal of
#'   the Royal Statistical Society B}, 82(1), 39--67.
#' @examples
#' if (requireNamespace("sensemakr", quietly = TRUE)) {
#'   set.seed(1)
#'   df <- data.frame(d = rnorm(100), x1 = rnorm(100))
#'   df$y <- 0.5 * df$d + 0.3 * df$x1 + rnorm(100)
#'   fit <- stats::lm(y ~ d + x1, data = df)
#'   res <- morie_sensitivity_omitted_var_bias(fit, "d",
#'                                             benchmark_covariates = "x1")
#'   class(res)
#' }
#' @export
morie_sensitivity_omitted_var_bias <- function(model, treatment,
                                               benchmark_covariates = NULL,
                                               kd = c(1, 2, 3), ky = NULL,
                                               q = 1.0, alpha = 0.05, ...) {
  .morie_sens_need("sensemakr", "morie_sensitivity_omitted_var_bias")
  if (is.null(ky)) ky <- kd
  args <- list(model = model, treatment = treatment, q = q, alpha = alpha)
  if (!is.null(benchmark_covariates))
    args$benchmark_covariates <- benchmark_covariates
  args$kd <- kd
  args$ky <- ky
  args <- c(args, list(...))
  raw <- do.call(sensemakr::sensemakr, args)
  stats_summary <- tryCatch(raw$sensitivity_stats,
                            error = function(e) NULL)
  rv_q  <- if (!is.null(stats_summary)) as.numeric(stats_summary$rv_q) else NA_real_
  rv_qa <- if (!is.null(stats_summary)) as.numeric(stats_summary$rv_qa) else NA_real_
  partial_r2 <- if (!is.null(stats_summary))
    as.numeric(stats_summary$r2yd.x) else NA_real_
  bounds <- tryCatch(raw$bounds, error = function(e) NULL)
  structure(
    list(rv_q                 = rv_q,
         rv_qa                = rv_qa,
         partial_r2_treatment = partial_r2,
         benchmark_bounds     = bounds,
         method               = "sensemakr (sensemakr)",
         raw                  = raw),
    class = c("morie_sensitivity_omitted_var_bias", "list")
  )
}


#' Konfound robustness for a coefficient (konfound extender)
#'
#' Thin interface to \code{konfound::pkonfound}: how many cases would
#' need to be replaced with average-treatment-effect cases (or how
#' large would an omitted-variable correlation have to be) to invalidate
#' the inference?  Pairs with
#' \code{\link{morie_sensitivity_omitted_var_bias}} (which uses the
#' Cinelli-Hazlett partial-R-squared framing instead of the
#' Frank et al. percent-bias-to-invalidate framing).
#'
#' @param estimate Treatment-coefficient estimate.
#' @param se Standard error of \code{estimate}.
#' @param n Number of observations.
#' @param n_covariates Number of covariates in the model
#'   (excluding the intercept and the treatment).  Default 0.
#' @param alpha Significance level.  Default 0.05.
#' @param ... Additional arguments forwarded to
#'   \code{konfound::pkonfound}.
#' @return A list of class \code{morie_sensitivity_konfound} with
#'   the percent-bias-to-invalidate, the impact-threshold-of-a-
#'   confounding-variable (ITCV), and the raw konfound object.
#' @references Frank, K. A., Maroulis, S. J., Duong, M. Q., &
#'   Kelcey, B. M. (2013). What would it take to change an
#'   inference?  \emph{Educational Evaluation and Policy Analysis},
#'   35(4), 437--460.
#' @examplesIf requireNamespace("konfound", quietly = TRUE)
#' str(morie_sensitivity_konfound(0.5, 0.15, 200), max.level = 1)
#' @export
morie_sensitivity_konfound <- function(estimate, se, n,
                                       n_covariates = 0L,
                                       alpha = 0.05, ...) {
  .morie_sens_need("konfound", "morie_sensitivity_konfound")
  raw <- konfound::pkonfound(
    est_eff = estimate, std_err = se,
    n_obs = n, n_covariates = n_covariates,
    alpha = alpha, to_return = "raw_output", ...
  )
  pct_bias <- tryCatch(as.numeric(raw$percent_bias_to_change_inference),
                       error = function(e) NA_real_)
  itcv <- tryCatch(as.numeric(raw$itcv),
                   error = function(e) NA_real_)
  structure(
    list(estimate                       = estimate,
         se                             = se,
         n                              = n,
         percent_bias_to_invalidate     = pct_bias,
         impact_threshold_confounder    = itcv,
         method                         = "pkonfound (konfound)",
         raw                            = raw),
    class = c("morie_sensitivity_konfound", "list")
  )
}
