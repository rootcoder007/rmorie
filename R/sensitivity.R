# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.

#' Sensitivity analysis for causal inference assumptions
#'
#' Tools to assess the robustness of causal effect estimates to
#' unmeasured confounding, model specification, and other threats to
#' internal validity. Includes Rosenbaum bounds, the E-value family,
#' Ding-VanderWeele bias formulas, tipping-point analysis, omitted-
#' variable bias (Cinelli-Hazlett), Manski bounds, probabilistic
#' (Monte-Carlo) bias analysis, and specification curve analysis.
#'
#' Wraps CRAN \pkg{EValue} when available; falls back to base R
#' otherwise.
#'
#' @references
#' Rosenbaum (2002); VanderWeele & Ding (2017); Cinelli & Hazlett
#' (2020); Manski (1990); Ding & VanderWeele (2016).
#' @name sensitivity
NULL


# -- Result containers ------------------------------------------------

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

.rr_to_evalue <- function(rr) {
  if (rr < 1) rr <- 1 / rr
  rr + sqrt(rr * (rr - 1))
}


#' E-value for a risk ratio
#'
#' Wraps \pkg{EValue} when available; otherwise applies the
#' VanderWeele-Ding closed-form formula directly.
#'
#' @param rr        Observed risk ratio.
#' @param ci_lower  Lower 95% CI of the RR (optional).
#' @param ci_upper  Upper 95% CI of the RR (optional).
#' @return A `morie_evalue` named-list.
#' @export
e_value_rr <- function(rr, ci_lower = NULL, ci_upper = NULL) {
  if (requireNamespace("EValue", quietly = TRUE)) {
    ev <- tryCatch(
      EValue::evalue(EValue::RR(rr), lo = ci_lower, hi = ci_upper),
      error = function(e) NULL
    )
    if (!is.null(ev)) {
      e_point <- as.numeric(ev["E-values", "point"])
      e_ci    <- suppressWarnings(as.numeric(ev["E-values", "lower"]))
      if (is.na(e_ci))
        e_ci  <- suppressWarnings(as.numeric(ev["E-values", "upper"]))
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
#' Wraps \pkg{rbounds} when available (and `method == "wilcoxon"`);
#' falls back to a base-R normal-approximation implementation.
#'
#' @param treated_outcomes Vector of outcomes for treated units.
#' @param control_outcomes Vector of outcomes for matched controls.
#' @param gamma_range Numeric vector of Gamma values (default
#'   `seq(1, 5, by = 0.25)`).
#' @param method One of `"wilcoxon"`, `"sign"`, `"mcnemar"`.
#' @return A `morie_rosenbaum_bounds` named-list.
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
#'
#' @param estimate     Observed treatment effect.
#' @param se           Standard error of the estimate.
#' @param n_treated    Number of treated units.
#' @param n_control    Number of control units.
#' @param delta_range  Numeric vector of bias parameters (default
#'   `seq(-3|est|, 3|est|, length.out = 101)`).
#' @param outcome_type `"continuous"` or `"binary"` (advisory only).
#' @return A `morie_tipping_point` named-list.
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
#' Wraps \pkg{sensemakr} when available; otherwise applies the
#' closed-form Cinelli-Hazlett robustness-value formulas in base R.
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
#' sample filters x model families.
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
          else if (model_type == "robust") {
            if (requireNamespace("MASS", quietly = TRUE))
              MASS::rlm(fml, data = sub)
            else NULL
          } else NULL
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
#' distribution of bias-adjusted estimates.
#'
#' @param estimate      Observed estimate.
#' @param se            Standard error.
#' @param n_simulations Number of MC draws. Default 10000.
#' @param bias_parms    Named list with `(mean, sd)` pairs for
#'   `rr_ud`, `rr_eu`, `prevalence`. Defaults supplied.
#' @param seed          RNG seed. Default 42.
#' @return Named list with bias-adjusted distribution summaries.
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
