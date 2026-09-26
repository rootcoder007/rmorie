# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.
#
# Every method here is native base R, verified against the canonical
# package: e_value_* and morie_sensitivity_evalue (EValue),
# rosenbaum_bounds (rbounds::psens; the McNemar path is the exact
# binomial tail P(X >= b), where rbounds::binarysens is off by one),
# omitted_variable_bias and morie_sensitivity_omitted_var_bias
# (sensemakr), morie_sensitivity_tipping_point (tipr::tip) and
# morie_sensitivity_konfound (konfound::pkonfound).

#' Sensitivity analysis for causal inference assumptions
#'
#' Tools to assess the robustness of causal effect estimates to
#' unmeasured confounding, model specification, and other threats to
#' internal validity. Includes Rosenbaum bounds, the E-value family,
#' Ding-VanderWeele bias formulas, tipping-point analysis, omitted-
#' variable bias (Cinelli-Hazlett), Manski bounds, probabilistic
#' (Monte-Carlo) bias analysis, and specification curve analysis.
#'
#' Native implementations of the methods of \pkg{EValue},
#' \pkg{tipr}, \pkg{sensemakr}, \pkg{rbounds} and \pkg{konfound},
#' verified against those packages.
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
#' For a rare outcome (\code{prevalence} below 0.15 or not given) the OR
#' approximates the RR; for a common outcome the OR is converted with
#' \eqn{RR \approx \sqrt{OR}} (VanderWeele & Ding 2017), as
#' \code{EValue::evalues.OR(rare = FALSE)}.
#'
#' @param odds_ratio Observed odds ratio.
#' @param ci_lower,ci_upper Optional 95% CI.
#' @param prevalence Outcome prevalence (optional); 0.15 or more selects
#'   the common-outcome conversion.
#' @return A `morie_evalue` named-list.
#' @examples
#' res <- e_value_or(2.0)
#' res$e_value_point
#' @export
e_value_or <- function(odds_ratio, ci_lower = NULL, ci_upper = NULL,
                         prevalence = NULL) {
  if (!is.null(prevalence) && prevalence >= 0.15) {
    rr <- sqrt(odds_ratio)
    if (!is.null(ci_lower)) ci_lower <- sqrt(ci_lower)
    if (!is.null(ci_upper)) ci_upper <- sqrt(ci_upper)
  } else {
    rr <- odds_ratio
  }
  e_value_rr(rr, ci_lower, ci_upper)
}


#' E-value for a hazard ratio
#'
#' For a common outcome the HR is converted with VanderWeele's (2017)
#' approximation \eqn{RR = (1 - 0.5^{\sqrt{HR}}) / (1 - 0.5^{\sqrt{1/HR}})};
#' for a rare outcome the HR approximates the RR, as
#' \code{EValue::evalues.HR}.
#'
#' @param hr Hazard ratio.
#' @param ci_lower,ci_upper Optional 95% CI of HR.
#' @param rare Logical; outcome rare (below 15%) at the end of
#'   follow-up.  Default \code{FALSE}.
#' @return A `morie_evalue` named-list.
#' @examples
#' res <- e_value_hr(2.0, ci_lower = 1.5, ci_upper = 2.5)
#' res$e_value_point
#' @export
e_value_hr <- function(hr, ci_lower = NULL, ci_upper = NULL, rare = FALSE) {
  hr_to_rr <- function(x) {
    if (is.null(x) || x <= 0) return(NULL)
    if (rare || x == 1) x
    else (1 - 0.5^sqrt(x)) / (1 - 0.5^sqrt(1 / x))
  }
  e_value_rr(hr_to_rr(hr), hr_to_rr(ci_lower), hr_to_rr(ci_upper))
}


#' E-value for a standardised mean difference (Cohen's d)
#'
#' Converts d to an RR scale via the VanderWeele-Ding approximation
#' \eqn{RR \approx \exp(0.91 d)}, CI \eqn{\exp(0.91 d \pm 1.78 s)}, as
#' \code{EValue::evalues.MD}, then applies `e_value_rr()`.
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
  if (is.null(se) && !is.null(n)) se <- sqrt(4 / n)
  rr_lo <- rr_hi <- NULL
  if (!is.null(se)) {
    rr_lo <- exp(0.91 * d - 1.78 * se)
    rr_hi <- exp(0.91 * d + 1.78 * se)
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
    # zero differences carry no sign information (Wilcoxon's rule)
    diffs <- diffs[diffs != 0]
    ranks <- rank(abs(diffs))
    signs <- sign(diffs)
    t_obs <- sum(ranks[signs > 0])
    for (i in seq_along(gamma_range)) {
      gamma <- gamma_range[i]
      p_treat   <- gamma / (1 + gamma)
      exp_u     <- sum(ranks * p_treat)
      var_u     <- sum(ranks^2 * p_treat * (1 - p_treat))
      p_upper[i] <- stats::pnorm(
        (t_obs - exp_u) / sqrt(max(var_u, 1e-10)), lower.tail = FALSE)
      p_treat_l <- 1 / (1 + gamma)
      exp_l     <- sum(ranks * p_treat_l)
      var_l     <- sum(ranks^2 * p_treat_l * (1 - p_treat_l))
      p_lower[i] <- stats::pnorm(
        (t_obs - exp_l) / sqrt(max(var_l, 1e-10)), lower.tail = FALSE)
    }
  } else if (method == "sign") {
    # zero differences carry no sign; n counts the non-zero pairs
    n_pos <- sum(diffs > 0)
    n <- sum(diffs != 0)
    for (i in seq_along(gamma_range)) {
      gamma <- gamma_range[i]
      p_upper[i] <- stats::pbinom(n_pos - 1L, n, gamma / (1 + gamma), lower.tail = FALSE)
      p_lower[i] <- stats::pbinom(n_pos - 1L, n, 1 / (1 + gamma), lower.tail = FALSE)
    }
  } else if (method == "mcnemar") {
    b  <- sum(t_vec == 1 & c_vec == 0)
    cc <- sum(t_vec == 0 & c_vec == 1)
    n_disc <- b + cc
    for (i in seq_along(gamma_range)) {
      gamma <- gamma_range[i]
      p_upper[i] <- stats::pbinom(b - 1L, n_disc,
                                     gamma / (1 + gamma), lower.tail = FALSE)
      p_lower[i] <- stats::pbinom(b - 1L, n_disc,
                                     1 / (1 + gamma), lower.tail = FALSE)
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
#' ones for the treatment effect to become non-significant?  The
#' tipping point is the smallest shift \eqn{\delta} of the estimate that
#' makes the two-sided 5\% test non-significant,
#' \eqn{\delta^* = \hat\tau - \mathrm{sign}(\hat\tau) z_{0.975} se} (0 when
#' the estimate is already non-significant); the grid reports adjusted
#' estimates and p-values over \code{delta_range}.  For the
#' unmeasured-confounder tipping point see
#' \code{\link{morie_sensitivity_tipping_point}}.
#'
#' @param estimate     Observed treatment effect.
#' @param se           Standard error of the estimate.
#' @param n_treated    Number of treated units.
#' @param n_control    Number of control units.
#' @param delta_range  Numeric vector of bias parameters (default
#'   `seq(-3|est|, 3|est|, length.out = 101)`).
#' @param outcome_type `"continuous"` or `"binary"`; for a binary outcome
#'   the default delta grid is capped at the unit interval.
#' @return A `morie_tipping_point` named-list.
#' @examples
#' str(tipping_point_analysis(0.5, 0.15, n_treated = 100, n_control = 100),
#'     max.level = 1)
#' @export
tipping_point_analysis <- function(estimate, se, n_treated, n_control,
                                      delta_range = NULL,
                                      outcome_type = "continuous") {
  outcome_type <- match.arg(outcome_type, c("continuous", "binary"))
  if (is.null(delta_range)) {
    max_d <- abs(estimate) * 3
    # a risk difference cannot be shifted beyond the unit interval
    if (outcome_type == "binary") max_d <- min(max_d, 1)
    delta_range <- seq(-max_d, max_d, length.out = 101L)
  }
  delta_range <- as.numeric(delta_range)
  adjusted_estimates <- estimate - delta_range
  adjusted_p <- 2 * stats::pnorm(abs(adjusted_estimates / se),
                                 lower.tail = FALSE)
  z <- stats::qnorm(0.975)
  tipping_point <- if (abs(estimate) > z * se) {
    estimate - sign(estimate) * z * se
  } else {
    0
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
# Omitted-variable bias (Cinelli & Hazlett 2020 -- sensemakr)
# =====================================================================

# Robustness value (sensemakr::robustness_value): alpha = 1 gives RV_q.
.morie_ovb_rv <- function(t_stat, dof, q = 1, alpha = 0.05) {
  fq <- q * abs(t_stat / sqrt(dof))
  f_crit <- if (alpha < 1) {
    abs(stats::qt(alpha / 2, df = dof - 1)) / sqrt(dof - 1)
  } else {
    0
  }
  fqa <- fq - f_crit
  if (fqa <= 0) return(0)
  # extreme robustness value when fq > 1 / f_crit
  if (f_crit > 0 && fq > 1 / f_crit) return((fq^2 - f_crit^2) / (1 + fq^2))
  2 / (1 + sqrt(1 + 4 / fqa^2))
}

# Benchmark bound and adjusted estimate (sensemakr::ovb_partial_r2_bound,
# adjusted_estimate / adjusted_se / adjusted_ci) for a confounder kd / ky
# times as strong as a covariate with partial R2 r2dxj (with treatment)
# and r2yxj (with outcome).
.morie_ovb_bound <- function(estimate, se, dof, r2dxj, r2yxj, kd = 1,
                             ky = kd, alpha = 0.05) {
  r2dz <- kd * r2dxj / (1 - r2dxj)
  if (any(r2dz >= 1)) {
    stop("Implied bound on r2dz.x >= 1; use a lower kd.", call. = FALSE)
  }
  r2zxj <- kd * r2dxj^2 / ((1 - kd * r2dxj) * (1 - r2dxj))
  if (any(r2zxj >= 1)) stop("Impossible kd value; use a lower kd.", call. = FALSE)
  r2yz <- pmin(((sqrt(ky) + sqrt(r2zxj)) / sqrt(1 - r2zxj))^2 *
                 (r2yxj / (1 - r2yxj)), 1)
  bias <- sqrt(r2yz * r2dz / (1 - r2dz)) * se * sqrt(dof)
  adj <- sign(estimate) * (abs(estimate) - bias)
  adj_se <- sqrt((1 - r2yz) / (1 - r2dz)) * se * sqrt(dof / (dof - 1))
  tc <- stats::qt(1 - alpha / 2, dof)
  data.frame(
    r2dz.x = r2dz, r2yz.dx = r2yz, adjusted_estimate = adj,
    adjusted_se = adj_se, adjusted_t = adj / adj_se,
    adjusted_lower_CI = adj - tc * adj_se,
    adjusted_upper_CI = adj + tc * adj_se
  )
}

#' Omitted-variable bias analysis (sensemakr framework)
#'
#' Closed-form Cinelli-Hazlett (2020) sensitivity from the estimate,
#' its standard error and the residual degrees of freedom.  Robustness
#' values as \code{sensemakr::robustness_value}: with
#' \eqn{f_q = q |t| / \sqrt{dof}},
#' \eqn{RV_q = (\sqrt{f_q^4 + 4 f_q^2} - f_q^2) / 2}, and
#' \eqn{RV_{q,\alpha}} the same in \eqn{f_q - f^*},
#' \eqn{f^* = |t^*_{\alpha, dof-1}| / \sqrt{dof - 1}} (the extreme
#' robustness value when \eqn{f_q > 1/f^*}).  Benchmark bounds follow
#' \code{sensemakr::ovb_bounds}.  For a fitted \code{lm} see
#' \code{\link{morie_sensitivity_omitted_var_bias}}.
#'
#' @param estimate              Treatment coefficient.
#' @param se                    SE of the estimate.
#' @param dof                   Residual degrees of freedom.
#' @param r2_yd_x               Partial R^2 of treatment with outcome.
#' @param partial_r2_treatment  Same as `r2_yd_x` (for clarity).
#' @param q                     Fraction of the estimate to be
#'   explained away. Default 1.
#' @param alpha                 Significance level. Default 0.05.
#' @param benchmark_covariates  Named list mapping covariate name to
#'   \code{c(r2_dxj_x, r2_yxj_dx)}: its partial R^2 with the treatment
#'   (given the other covariates) and with the outcome (given treatment
#'   and the other covariates).  A single number is used for both.
#' @param kd,ky Strength multipliers of the confounder relative to a
#'   benchmark (\code{ky} defaults to \code{kd}).
#' @return A `morie_ovb` named-list; \code{benchmark_bounds} maps each
#'   covariate to a one-row data frame with \code{r2dz.x},
#'   \code{r2yz.dx}, \code{adjusted_estimate}, \code{adjusted_se},
#'   \code{adjusted_t}, \code{adjusted_lower_CI}, \code{adjusted_upper_CI}.
#' @references Cinelli, C., & Hazlett, C. (2020). Making sense of
#'   sensitivity: extending omitted variable bias.  \emph{Journal of
#'   the Royal Statistical Society B}, 82(1), 39--67.
#' @examples
#' str(omitted_variable_bias(0.5, 0.15, dof = 150, r2_yd_x = 0.1,
#'                           partial_r2_treatment = 0.05), max.level = 1)
#' @export
omitted_variable_bias <- function(estimate, se, dof, r2_yd_x,
                                     partial_r2_treatment,
                                     q = 1.0, alpha = 0.05,
                                     benchmark_covariates = NULL,
                                     kd = 1, ky = kd) {
  t_stat <- estimate / se
  rv_q <- .morie_ovb_rv(t_stat, dof, q, 1)
  rv_qa <- .morie_ovb_rv(t_stat, dof, q, alpha)
  bounds <- list()
  for (name in names(benchmark_covariates)) {
    r2 <- benchmark_covariates[[name]]
    if (length(r2) == 1L) r2 <- c(r2, r2)
    bounds[[name]] <- .morie_ovb_bound(estimate, se, dof, r2[1], r2[2],
                                       kd, ky, alpha)
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
#'       `list[tuple[str, callable]]` shape -- positional pair). Default: full
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
#' identified (Manski 1990): with the outcome in \eqn{[a, b]},
#' \eqn{E[Y_1] \in [p \bar y_1 + a(1-p), p \bar y_1 + b(1-p)]} and
#' \eqn{E[Y_0] \in [(1-p) \bar y_0 + a p, (1-p) \bar y_0 + b p]}, so the
#' bounds always have width \eqn{b - a}.  Returns a named list with
#' `lower_bound`, `upper_bound`, `point_estimate`, `width`.
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
  lo <- e1 * p1 + y_min * p0 - (e0 * p0 + y_max * p1)
  hi <- e1 * p1 + y_max * p0 - (e0 * p0 + y_min * p1)
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
#' Without \code{prevalence_confounder} the adjustment is the Ding &
#' VanderWeele (2016) bounding factor
#' \eqn{B = RR_{UD} RR_{EU} / (RR_{UD} + RR_{EU} - 1)}, the largest bias
#' any confounder with those strengths can produce, applied toward the
#' null.  With the prevalence \eqn{p_0} of a binary confounder among the
#' unexposed, \code{rr_eu} is its prevalence ratio
#' (\eqn{p_1 = RR_{EU} p_0}) and the bias is Schlesselman's (1978) exact
#' factor \eqn{(1 + (RR_{UD}-1) p_1) / (1 + (RR_{UD}-1) p_0)}.
#'
#' @param estimate              Observed treatment effect on the
#'   log-RR scale.
#' @param se                    Standard error.
#' @param rr_ud                 RR linking confounder to outcome.
#' @param rr_eu                 RR linking treatment to confounder.
#' @param prevalence_confounder Optional confounder prevalence among
#'   the unexposed.
#' @return Named list with `adjusted_estimate`, `bias` (log scale),
#'   `adjusted_ci_lower`, `adjusted_ci_upper`, `original_estimate`.
#' @examples
#' res <- bias_adjusted_estimate(0.5, 0.1, rr_ud = 2, rr_eu = 2)
#' res$adjusted_estimate
#' c(res$adjusted_ci_lower, res$adjusted_ci_upper)
#' @export
bias_adjusted_estimate <- function(estimate, se, rr_ud, rr_eu,
                                      prevalence_confounder = NULL) {
  if (is.null(prevalence_confounder)) {
    bias <- log(rr_ud * rr_eu / (rr_ud + rr_eu - 1)) *
      (if (estimate >= 0) 1 else -1)
  } else {
    p0 <- prevalence_confounder
    p1 <- rr_eu * p0
    if (p1 < 0 || p1 > 1) {
      stop("rr_eu * prevalence_confounder must be a probability.",
           call. = FALSE)
    }
    bias <- log((1 + (rr_ud - 1) * p1) / (1 + (rr_ud - 1) * p0))
  }
  adjusted <- estimate - bias
  z <- stats::qnorm(0.975)
  list(adjusted_estimate = adjusted,
       bias              = bias,
       adjusted_ci_lower = adjusted - z * se,
       adjusted_ci_upper = adjusted + z * se,
       original_estimate = estimate)
}


# =====================================================================
# Probabilistic (Monte Carlo) bias analysis
# =====================================================================

#' Probabilistic (Monte Carlo) sensitivity analysis
#'
#' Draws bias parameters from prior distributions and the estimate from
#' its sampling distribution, and removes Schlesselman's
#' binary-confounder bias \eqn{\log[(1 + (RR_{UD}-1)p_1) / (1 + (RR_{UD}-1)p_0)]}
#' with \eqn{p_0} the drawn prevalence among the unexposed and
#' \eqn{p_1 = \min(RR_{EU} p_0, 1)}.  Cross-references
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
  .rmorie_local_seed(seed)
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
  # Schlesselman's binary-confounder bias with p0 the prevalence among
  # the unexposed and p1 = min(rr_eu p0, 1) (Lash, Fox & Fink 2009, ch. 8)
  p1 <- pmin(rr_eu * prev, 1)
  biases <- log((1 + (rr_ud - 1) * p1) / (1 + (rr_ud - 1) * prev))
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
  p <- 2 * (stats::pnorm(abs(z), lower.tail = FALSE))
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


#' E-values for the EValue dispatch family (extender)
#'
#' Native E-values for the \pkg{EValue} dispatch family
#' (\code{evalues.OLS}, \code{evalues.RR}, \code{evalues.OR},
#' \code{evalues.HR}, \code{evalues.MD}); OLS and MD use the
#' \eqn{\exp(0.91 d \pm 1.78 se)} interval.  Pairs with
#' \code{\link{e_value_rr}} / \code{\link{e_value_or}} /
#' \code{\link{e_value_hr}} / \code{\link{e_value_d}}.
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
#' @param ... Unused.
#' @return A list of class \code{morie_sensitivity_evalue} with
#'   \code{estimate}, \code{e_value_point}, \code{e_value_ci},
#'   \code{type}, \code{method}, and \code{raw} (the point and CI
#'   E-values).
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
    # EValue's CI exp(0.91 d -+ 1.78 se) on the RR scale
    ev <- morie_evalue(d_est, "MD",
                       lo = d_est - 1.78 / 0.91 * d_se,
                       hi = d_est + 1.78 / 0.91 * d_se, true = 0)
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
         method        = sprintf("evalues.%s (native)", type),
         raw           = ev),
    class = c("morie_sensitivity_evalue", "list")
  )
}


#' Tipping point for a single unmeasured confounder (native tipr)
#'
#' Native \code{tipr::tip}: an unmeasured confounder whose standardised
#' mean difference between exposure groups is \code{smd} tips an
#' observed ratio \eqn{b} (use the CI bound nearest the null) to 1 when
#' its effect on the outcome is \eqn{b^{1/smd}} (Lin, Psaty & Kronmal
#' 1998).  Pairs with \code{\link{tipping_point_analysis}}, which
#' targets \emph{missing-data} sensitivity.  Mirrors
#' \code{tipping_point_smd} in the Python arm.
#'
#' @param estimate Observed ratio (e.g. risk ratio).
#' @param smd Standardised mean difference of the unmeasured confounder
#'   between exposure groups.
#' @param r2 Unsupported (the partial-R2 pathway is
#'   \code{\link{omitted_variable_bias}}); must be \code{NULL}.
#' @param ... Unused.
#' @return A list of class \code{morie_sensitivity_tipping_point} with
#'   \code{tipped_estimate} (1) and \code{confounder_outcome_effect}.
#' @references D'Agostino McGowan, L. (2022). tipr: An R package for
#'   sensitivity analyses for unmeasured confounders.
#'   \emph{Journal of Open Source Software}, 7(77), 4495.
#' @examples
#' str(morie_sensitivity_tipping_point(1.8, smd = 0.5), max.level = 1)
#' @export
morie_sensitivity_tipping_point <- function(estimate, smd = NULL,
                                            r2 = NULL, ...) {
  if (!is.null(r2)) {
    stop("the partial-R2 pathway is omitted_variable_bias(); give `smd`.",
         call. = FALSE)
  }
  if (is.null(smd)) stop("`smd` is required.", call. = FALSE)
  cy <- estimate^(1 / smd)
  structure(
    list(estimate                  = estimate,
         smd                       = smd,
         r2                        = r2,
         tipped_estimate           = 1,
         confounder_outcome_effect = cy,
         method                    = "tip (native)",
         raw                       = list(effect_observed = estimate,
                                          exposure_confounder_effect = smd,
                                          confounder_outcome_effect = cy)),
    class = c("morie_sensitivity_tipping_point", "list")
  )
}


#' Omitted-variable bias on a fitted model (native sensemakr)
#'
#' Native \code{sensemakr::sensemakr} for a fitted linear model:
#' robustness values, the partial R-squared of the treatment, and
#' benchmark bounds for confounders \code{kd} / \code{ky} times as
#' strong as each benchmark covariate (its partial R-squared with the
#' outcome from \code{model}, with the treatment from the regression of
#' the treatment on the other regressors).  Pairs with
#' \code{\link{omitted_variable_bias}}, the closed form from
#' \code{estimate}, \code{se} and degrees of freedom.
#'
#' @param model A fitted \code{lm}.
#' @param treatment Name of the treatment coefficient.
#' @param benchmark_covariates Optional character vector of coefficient
#'   names whose strengths bound the unmeasured-confounder strength.
#' @param kd Multipliers on the benchmark covariate strength.
#'   Default \code{c(1, 2, 3)}.
#' @param ky Multipliers on the benchmark covariate's outcome
#'   strength.  Default equal to \code{kd}.
#' @param q Fraction of the estimate to be explained away.  Default 1.
#' @param alpha Significance level.  Default 0.05.
#' @param ... Unused.
#' @return A list of class \code{morie_sensitivity_omitted_var_bias}
#'   with \code{rv_q}, \code{rv_qa}, \code{partial_r2_treatment} and
#'   \code{benchmark_bounds} (the columns of sensemakr's bounds table).
#' @references Cinelli, C., & Hazlett, C. (2020). Making sense of
#'   sensitivity: extending omitted variable bias.  \emph{Journal of
#'   the Royal Statistical Society B}, 82(1), 39--67.
#' @examples
#' set.seed(1)
#' df <- data.frame(d = rnorm(100), x1 = rnorm(100))
#' df$y <- 0.5 * df$d + 0.3 * df$x1 + rnorm(100)
#' fit <- stats::lm(y ~ d + x1, data = df)
#' res <- morie_sensitivity_omitted_var_bias(fit, "d",
#'                                           benchmark_covariates = "x1",
#'                                           kd = 1)
#' res$rv_q
#' @export
morie_sensitivity_omitted_var_bias <- function(model, treatment,
                                               benchmark_covariates = NULL,
                                               kd = c(1, 2, 3), ky = NULL,
                                               q = 1.0, alpha = 0.05, ...) {
  if (is.null(ky)) ky <- kd
  X <- stats::model.matrix(model)
  y <- stats::model.response(stats::model.frame(model))
  dof <- nrow(X) - ncol(X)
  # coefficient t statistics of a least-squares fit
  tstats <- function(M, v) {
    fit <- stats::lm.fit(M, v)
    df_ <- nrow(M) - ncol(M)
    s2 <- sum(fit$residuals^2) / df_
    XtXi <- chol2inv(chol(crossprod(M)))
    list(t = fit$coefficients / sqrt(s2 * diag(XtXi)), df = df_)
  }
  ty <- tstats(X, y)
  est <- stats::coef(model)[[treatment]]
  t_d <- ty$t[[treatment]]
  se <- est / t_d
  bounds <- NULL
  if (length(benchmark_covariates)) {
    Xo <- X[, colnames(X) != treatment, drop = FALSE]
    td <- tstats(Xo, X[, treatment])
    rows <- lapply(benchmark_covariates, function(b) {
      r2yxj <- ty$t[[b]]^2 / (ty$t[[b]]^2 + ty$df)
      r2dxj <- td$t[[b]]^2 / (td$t[[b]]^2 + td$df)
      out <- .morie_ovb_bound(est, se, dof, r2dxj, r2yxj, kd, ky, alpha)
      cbind(bound_label = paste0(kd, "x ", b), out[, 1:2],
            treatment = treatment, out[, -(1:2)],
            stringsAsFactors = FALSE)
    })
    bounds <- do.call(rbind, rows)
    rownames(bounds) <- NULL
  }
  structure(
    list(rv_q                 = .morie_ovb_rv(t_d, dof, q, 1),
         rv_qa                = .morie_ovb_rv(t_d, dof, q, alpha),
         partial_r2_treatment = t_d^2 / (t_d^2 + dof),
         benchmark_bounds     = bounds,
         method               = "sensemakr (native)",
         raw                  = list(estimate = est, se = se, dof = dof)),
    class = c("morie_sensitivity_omitted_var_bias", "list")
  )
}


#' Konfound robustness for a coefficient (native konfound)
#'
#' Native \code{konfound::pkonfound} (two tails, null 0): the threshold
#' is \eqn{t^* se} with \eqn{t^*} on \eqn{n - n_{cov} - 2} df; the
#' percent bias to invalidate is \eqn{100 (1 - t^* se / \hat\beta)} (to
#' sustain when not significant) and RIR the corresponding number of
#' cases to replace; the impact threshold of a confounding variable is
#' \eqn{(r - r^*) / (1 \pm |r^*|)} with \eqn{r = t / \sqrt{t^2 + df}}.
#' Pairs with \code{\link{morie_sensitivity_omitted_var_bias}}.  Mirrors
#' \code{konfound} in the Python arm.
#'
#' @param estimate Treatment-coefficient estimate.
#' @param se Standard error of \code{estimate}.
#' @param n Number of observations.
#' @param n_covariates Number of covariates in the model
#'   (excluding the intercept and the treatment).  Default 0.
#' @param alpha Significance level.  Default 0.05.
#' @param ... Unused.
#' @return A list of class \code{morie_sensitivity_konfound} with
#'   \code{percent_bias_to_invalidate}, \code{rir},
#'   \code{impact_threshold_confounder} and \code{beta_threshold}.
#' @references Frank, K. A., Maroulis, S. J., Duong, M. Q., &
#'   Kelcey, B. M. (2013). What would it take to change an
#'   inference?  \emph{Educational Evaluation and Policy Analysis},
#'   35(4), 437--460.
#' @examples
#' str(morie_sensitivity_konfound(0.5, 0.15, 200), max.level = 1)
#' @export
morie_sensitivity_konfound <- function(estimate, se, n,
                                       n_covariates = 0L,
                                       alpha = 0.05, ...) {
  df <- n - n_covariates - 2
  t_crit <- stats::qt(1 - alpha / 2, df) * (if (estimate < 0) -1 else 1)
  thr <- t_crit * se
  # percent bias to invalidate, or to sustain when not significant
  pct <- if (abs(estimate) > abs(thr)) {
    100 * (1 - thr / estimate)
  } else {
    100 * (1 - estimate / thr)
  }
  act_t <- estimate / se
  act_r <- act_t / sqrt(act_t^2 + df)
  crit_r <- t_crit / sqrt(t_crit^2 + df)
  mp <- if (estimate > -abs(thr) && estimate < abs(thr)) 1 else -1
  sgn <- sign(estimate - thr)
  itcv <- sgn * abs(act_r - crit_r) / (1 + mp * abs(crit_r))
  structure(
    list(estimate                    = estimate,
         se                          = se,
         n                           = n,
         percent_bias_to_invalidate  = pct,
         rir                         = round(n * pct / 100),
         impact_threshold_confounder = itcv,
         beta_threshold              = thr,
         method                      = "pkonfound (native)"),
    class = c("morie_sensitivity_konfound", "list")
  )
}
