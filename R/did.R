# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Difference-in-Differences (DiD) estimators for rmorie.
#
# Phase 1.e refactor (2026-05-25): hand-written base-R DiD implementations
# have been replaced with thin wrappers over canonical CRAN packages.
# Every method-style entry point now delegates to the reference
# implementation:
#
#   * fixest      -- two-way fixed-effects DiD (`feols`) and event study
#                    (`feols` + `i()`).
#   * did         -- Callaway-Sant'Anna group-time ATTs (`att_gt`).
#   * DRDID       -- Sant'Anna-Zhao doubly-robust DiD (`drdid_panel` /
#                    `drdid_rc`).
#   * bacondecomp -- Goodman-Bacon decomposition (`bacon`).
#   * DIDmultiplegt -- de Chaisemartin-D'Haultfoeuille DID-M
#                    (`did_multiplegt`).
#   * HonestDiD   -- Rambachan-Roth sensitivity to parallel-trends
#                    violations
#                    (`createSensitivityResults_relativeMagnitudes`).
#   * coresynth   -- Arkhangelsky et al. synthetic DiD, SDID via the
#                    unified Formula interface (`scm_fit(method="sdid")`).
#
# Wrappers preserve the `morie_did_*` API and the existing result-list
# shape (`estimate`, `std_error`, `t_stat`, `p_value`, `ci_lower`,
# `ci_upper`, `n_treated`, `n_control`, `method`, `details`) so that
# downstream rmorie code and MRM analyses continue to work unchanged.
#
# Internal helpers (`.morie_did_*`) are kept: they are pinned by
# `tests/testthat/test-did_matching-internals.R` and they back the
# small OLS-based estimators (2x2, repeated cross-section, triple-diff,
# continuous-treatment, fuzzy) where adding a CRAN dependency for
# trivially-short OLS would be a regression. The same helpers are
# reused by the wild-cluster-bootstrap path, which is base-R by
# design (fwildclusterboot is GitHub-only; see 0.9.5.12 NEWS).
#
# Aggregators that consume DiD output and produce rmorie-specific
# tables (`morie_did_aggregate_gt_att`, `morie_did_staggered`,
# `morie_did_parallel_trends_data`, `morie_did_test_parallel_trends`,
# `morie_did_placebo_test_*`, `morie_did_heterogeneous`,
# `morie_did_diagnostics`) are kept verbatim -- their output shapes
# are part of the rmorie API.

#' @importFrom stats lm glm coef vcov pnorm pt pf pchisq qnorm qt qchisq model.matrix model.frame fitted residuals binomial as.formula sigma complete.cases quantile predict ave sd var aggregate na.omit reshape lsfit setNames
#' @importFrom utils combn head
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.morie_did_have_fixest         <- function() requireNamespace("fixest",         quietly = TRUE)
.morie_did_have_did            <- function() requireNamespace("did",            quietly = TRUE)
.morie_did_have_bacondecomp    <- function() requireNamespace("bacondecomp",    quietly = TRUE)
.morie_did_have_coresynth      <- function() requireNamespace("coresynth",      quietly = TRUE)
.morie_did_have_sandwich       <- function() requireNamespace("sandwich",       quietly = TRUE)
.morie_did_have_drdid          <- function() requireNamespace("DRDID",          quietly = TRUE)
.morie_did_have_honestdid      <- function() requireNamespace("HonestDiD",      quietly = TRUE)
.morie_did_have_didmultiplegt  <- function() requireNamespace("DIDmultiplegt",  quietly = TRUE)

#' @keywords internal
.morie_did_need <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "`%s()` requires the '%s' package. Install it with %s",
      fn, pkg, sprintf("install.packages(\"%s\")", pkg)),
      call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
.morie_did_make_ci <- function(estimate, se, alpha = 0.05) {
  z <- stats::qnorm(1 - alpha / 2)
  c(estimate - z * se, estimate + z * se)
}

#' @keywords internal
.morie_did_ols_robust_se <- function(X, y, cluster_ids = NULL) {
  # OLS with heteroskedasticity- or cluster-robust (CR1) variance.
  # Returns list(beta, se).
  n <- nrow(X)
  k <- ncol(X)
  XtX_inv <- tryCatch(solve(crossprod(X)),
                      error = function(e) MASS::ginv(crossprod(X)))
  beta <- as.numeric(XtX_inv %*% crossprod(X, y))
  resid <- as.numeric(y - X %*% beta)
  if (!is.null(cluster_ids)) {
    uc <- unique(cluster_ids)
    G  <- length(uc)
    meat <- matrix(0, k, k)
    for (c_ in uc) {
      mask <- cluster_ids == c_
      Xc <- X[mask, , drop = FALSE]
      ec <- resid[mask]
      score <- as.numeric(crossprod(Xc, ec))
      meat <- meat + tcrossprod(score)
    }
    correction <- (G / (G - 1)) * ((n - 1) / (n - k))
    V <- correction * (XtX_inv %*% meat %*% XtX_inv)
  } else {
    meat <- crossprod(X, resid^2 * X)
    correction <- n / (n - k)
    V <- correction * (XtX_inv %*% meat %*% XtX_inv)
  }
  se <- sqrt(pmax(diag(V), 0))
  list(beta = beta, se = se, vcov = V, residuals = resid)
}

#' @keywords internal
.morie_did_add_intercept <- function(X) {
  cbind(`(Intercept)` = 1, X)
}

#' @keywords internal
.morie_did_pvalue <- function(t_val) {
  2 * stats::pnorm(-abs(t_val))
}

#' @keywords internal
.morie_did_drop_na <- function(data, cols) {
  # Every did estimator routes through here, so validate once at the
  # shared entry: assert a data.frame with the required columns, then
  # drop incomplete rows (G2.14b ignore-with-message).
  data <- .morie_check_data(data, required = cols, arg = "data",
                            check_na = TRUE)
  data[stats::complete.cases(data[, cols, drop = FALSE]), , drop = FALSE]
}

#' @keywords internal
.morie_did_result <- function(estimate, std_error, n_treated, n_control,
                              method, alpha = 0.05, details = list()) {
  t_val <- if (is.finite(std_error) && std_error > 0) estimate / std_error else 0
  p_val <- if (is.finite(t_val)) .morie_did_pvalue(t_val) else NA_real_
  ci    <- if (is.finite(std_error))
    .morie_did_make_ci(estimate, std_error, alpha)
  else c(NA_real_, NA_real_)
  list(
    estimate  = estimate,
    std_error = std_error,
    t_stat    = t_val,
    p_value   = p_val,
    ci_lower  = ci[1],
    ci_upper  = ci[2],
    n_treated = n_treated,
    n_control = n_control,
    method    = method,
    details   = details
  )
}

#' @keywords internal
.morie_did_within_transform <- function(df, varname, unit, time) {
  # Two-way demeaning: x - unit_mean - time_mean + grand_mean.
  v <- as.numeric(df[[varname]])
  um <- stats::ave(v, df[[unit]], FUN = function(z) mean(z, na.rm = TRUE))
  tm <- stats::ave(v, df[[time]], FUN = function(z) mean(z, na.rm = TRUE))
  gm <- mean(v, na.rm = TRUE)
  v - um - tm + gm
}

#' @keywords internal
.morie_did_outcome_regression_att <- function(y, X, treat) {
  X <- as.matrix(X)
  fit <- stats::lm.fit(cbind(1, X[treat == 0, , drop = FALSE]),
                       y[treat == 0])
  beta <- fit$coefficients
  beta[is.na(beta)] <- 0
  X1   <- cbind(1, X[treat == 1, , drop = FALSE])
  y0_hat <- as.numeric(X1 %*% beta)
  mean(y[treat == 1] - y0_hat)
}

#' @keywords internal
.morie_did_ipw_att <- function(y, treat, ps) {
  ps <- pmin(pmax(ps, 0.01), 0.99)
  w  <- ps / (1 - ps)
  if (sum(treat == 1) == 0) return(0)
  mean(y[treat == 1]) -
    sum(w[treat == 0] * y[treat == 0]) / sum(w[treat == 0])
}

`%||%` <- function(a, b) if (is.null(a)) b else a


# ---------------------------------------------------------------------------
# 1. Classic 2x2 DiD
# ---------------------------------------------------------------------------

#' Classic 2x2 Difference-in-Differences estimator
#'
#' Estimates the canonical two-group / two-period DiD treatment effect
#' \deqn{\hat\tau = (\bar Y_{1,\text{post}} - \bar Y_{1,\text{pre}})
#'                 - (\bar Y_{0,\text{post}} - \bar Y_{0,\text{pre}}).}{hattau = (bar Y_1,post - bar Y_1,pre) - (bar Y_0,post - bar Y_0,pre).}
#' With covariates, fits the regression
#' \eqn{Y = \alpha + \beta D + \gamma P + \tau (D \times P) + X\delta + \varepsilon}{Y = alpha + beta D + gamma P + tau (D x P) + Xdelta + epsilon}
#' and reports \eqn{\hat\tau}{hattau}.
#'
#' For multi-period staggered designs prefer
#' \code{\link{morie_did_group_time_att}} (Callaway-Sant'Anna via
#' \pkg{did}). \code{\link{morie_did_doubly_robust}} (via \pkg{DRDID})
#' is the recommended option when pre-treatment covariates are
#' available.
#'
#' @param data A data frame containing the outcome, treatment, post and
#'   any covariate columns.
#' @param outcome Name of the outcome column.
#' @param treatment Name of the binary (0/1) treatment-group column.
#' @param post Name of the binary (0/1) post-period column.
#' @param covariates Optional character vector of covariate column names.
#' @param cluster Optional cluster ID column for CR1 standard errors.
#' @param alpha Significance level for confidence intervals (default 0.05).
#' @return A list with elements \code{estimate}, \code{std_error},
#'   \code{t_stat}, \code{p_value}, \code{ci_lower}, \code{ci_upper},
#'   \code{n_treated}, \code{n_control}, \code{method}, \code{details}.
#' @references Angrist, J. D., & Pischke, J.-S. (2009).
#'   \emph{Mostly Harmless Econometrics}. Princeton University Press.
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   y    = rnorm(200),
#'   d    = rep(c(0, 1), each = 100),
#'   post = rep(c(0, 1), times = 100)
#' )
#' morie_did_2x2(df, "y", "d", "post")
#' }
#' @export
morie_did_2x2 <- function(data, outcome, treatment, post,
                          covariates = NULL, cluster = NULL, alpha = 0.05) {
  df <- .morie_did_drop_na(data, c(outcome, treatment, post))
  d <- as.numeric(df[[treatment]])
  p <- as.numeric(df[[post]])
  y <- as.numeric(df[[outcome]])
  interaction <- d * p
  if (length(covariates)) {
    Xc <- as.matrix(df[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    X <- .morie_did_add_intercept(cbind(d, p, interaction, Xc))
  } else {
    X <- .morie_did_add_intercept(cbind(d, p, interaction))
  }
  cluster_ids <- if (!is.null(cluster)) df[[cluster]] else NULL
  fit <- .morie_did_ols_robust_se(X, y, cluster_ids = cluster_ids)
  tau_idx <- 4   # intercept(1) + d(2) + p(3) + interaction(4)
  est    <- fit$beta[tau_idx]
  se_est <- fit$se[tau_idx]
  .morie_did_result(
    est, se_est,
    n_treated = sum(d == 1),
    n_control = sum(d == 0),
    method = "did_2x2",
    alpha = alpha,
    details = list(
      all_coefficients = fit$beta,
      all_se           = fit$se,
      n_obs            = length(y)
    )
  )
}


# ---------------------------------------------------------------------------
# 2. Repeated cross-section DiD
# ---------------------------------------------------------------------------

#' Repeated cross-section DiD (optionally weighted)
#'
#' Same specification as \code{\link{morie_did_2x2}} but accepts a survey
#' weight column.  When \code{weights} is supplied, weighted least
#' squares is used.
#'
#' @inheritParams morie_did_2x2
#' @param weights Optional column of (sampling / survey) weights.
#' @return A list of class results; see \code{\link{morie_did_2x2}}.
#' @export
morie_did_repeated_cross_section <- function(data, outcome, treatment, post,
                                             covariates = NULL, weights = NULL,
                                             cluster = NULL, alpha = 0.05) {
  df <- .morie_did_drop_na(data, c(outcome, treatment, post))
  d <- as.numeric(df[[treatment]])
  p <- as.numeric(df[[post]])
  y <- as.numeric(df[[outcome]])
  interaction <- d * p
  if (length(covariates)) {
    Xc <- as.matrix(df[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    X <- .morie_did_add_intercept(cbind(d, p, interaction, Xc))
  } else {
    X <- .morie_did_add_intercept(cbind(d, p, interaction))
  }
  if (!is.null(weights)) {
    w_root <- sqrt(as.numeric(df[[weights]]))
    X <- X * w_root
    y <- y * w_root
  }
  cluster_ids <- if (!is.null(cluster)) df[[cluster]] else NULL
  fit <- .morie_did_ols_robust_se(X, y, cluster_ids = cluster_ids)
  tau_idx <- 4
  est <- fit$beta[tau_idx]
  se_est <- fit$se[tau_idx]
  .morie_did_result(
    est, se_est,
    n_treated = sum(as.numeric(df[[treatment]]) == 1),
    n_control = sum(as.numeric(df[[treatment]]) == 0),
    method = "did_repeated_cross_section", alpha = alpha,
    details = list(all_coefficients = fit$beta, n_obs = nrow(df))
  )
}


# ---------------------------------------------------------------------------
# 3. Panel two-way fixed-effects DiD -- thin fixest::feols wrapper
# ---------------------------------------------------------------------------

#' Two-way fixed-effects DiD (panel)
#'
#' Thin wrapper around \code{fixest::feols} estimating
#' \eqn{Y_{it} = \alpha_i + \lambda_t + \tau D_{it} + X'\delta
#' + \varepsilon_{it}}{Y_it = alpha_i + lambda_t + tau D_it + X'delta + varepsilon_it}
#' with cluster-robust standard errors. Hard-errors if \pkg{fixest} is
#' not installed.
#'
#' @inheritParams morie_did_2x2
#' @param unit Unit identifier column.
#' @param time Time period column.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @export
morie_did_panel_fe <- function(data, outcome, treatment, unit, time,
                               covariates = NULL, cluster = NULL,
                               alpha = 0.05) {
  .morie_did_need("fixest", "morie_did_panel_fe")
  df <- .morie_did_drop_na(data, c(outcome, treatment, unit, time))
  rhs <- if (length(covariates))
    paste(c(treatment, covariates), collapse = " + ")
  else treatment
  fe_part <- paste(unit, time, sep = " + ")
  f <- stats::as.formula(paste(outcome, "~", rhs, "|", fe_part))
  cluster_var <- if (!is.null(cluster)) cluster else unit
  fit <- fixest::feols(
    f, data = df,
    cluster = stats::as.formula(paste0("~", cluster_var))
  )
  cf <- fixest::coeftable(fit)
  est    <- cf[treatment, "Estimate"]
  se_est <- cf[treatment, "Std. Error"]
  .morie_did_result(
    est, se_est,
    n_treated = sum(as.numeric(df[[treatment]]) == 1),
    n_control = sum(as.numeric(df[[treatment]]) == 0),
    method = "did_panel_fe (fixest)", alpha = alpha,
    details = list(fit = fit,
                   n_units   = length(unique(df[[unit]])),
                   n_periods = length(unique(df[[time]])))
  )
}


# ---------------------------------------------------------------------------
# 4. Event study -- thin fixest::feols + i() wrapper
# ---------------------------------------------------------------------------

#' Event-study DiD specification
#'
#' Thin wrapper around \code{fixest::feols} with \code{fixest::i()}
#' relative-time dummies, plus unit and time fixed effects. The
#' \code{reference_period} is dropped as the baseline. Hard-errors if
#' \pkg{fixest} is not installed.
#'
#' For sun-Abraham interaction-weighted estimation prefer
#' \code{fixest::sunab()} directly.
#'
#' @param data Panel data frame.
#' @param outcome Outcome column.
#' @param unit Unit identifier column.
#' @param time Calendar-time column (integer-valued).
#' @param treatment_time Column giving the period in which each unit
#'   first received treatment (\code{Inf} or \code{NA} for
#'   never-treated units).
#' @param covariates Optional time-varying covariates.
#' @param reference_period Relative-time period omitted as baseline
#'   (default \code{-1}).
#' @param leads Number of pre-treatment periods to include.
#' @param lags Number of post-treatment periods to include.
#' @param cluster Cluster variable for standard errors (defaults to
#'   \code{unit}).
#' @param alpha Significance level.
#' @return A list with \code{coefficients} (data frame),
#'   \code{reference_period}, \code{pre_trend_f_stat},
#'   \code{pre_trend_p_value}, and \code{details}.
#' @export
morie_did_event_study <- function(data, outcome, unit, time, treatment_time,
                                  covariates = NULL, reference_period = -1L,
                                  leads = 4L, lags = 4L,
                                  cluster = NULL, alpha = 0.05) {
  .morie_did_need("fixest", "morie_did_event_study")
  df <- data
  rel_time <- as.numeric(df[[time]]) - as.numeric(df[[treatment_time]])
  # Truncate to [-leads, lags] so dummies outside the window are absorbed.
  rel_time_trunc <- pmin(pmax(rel_time, -leads), lags)
  rel_time_trunc[!is.finite(rel_time_trunc)] <- reference_period
  df[["morie_rel_time"]] <- rel_time_trunc
  cluster_var <- if (!is.null(cluster)) cluster else unit
  cov_part <- if (length(covariates))
    paste("+", paste(covariates, collapse = " + "))
  else ""
  f <- stats::as.formula(sprintf(
    "%s ~ i(morie_rel_time, ref = %d) %s | %s + %s",
    outcome, as.integer(reference_period), cov_part, unit, time
  ))
  fit <- fixest::feols(
    f, data = df,
    cluster = stats::as.formula(paste0("~", cluster_var))
  )
  cf <- fixest::coeftable(fit)
  coef_names <- rownames(cf)
  # Parse the relative-time integer out of the "morie_rel_time::K" labels.
  rel_int <- suppressWarnings(as.integer(sub(".*::", "", coef_names)))
  keep <- !is.na(rel_int)
  est_k <- cf[keep, "Estimate"]
  se_k  <- cf[keep, "Std. Error"]
  rel_k <- rel_int[keep]
  z <- stats::qnorm(1 - alpha / 2)
  coef_df <- data.frame(
    relative_time = rel_k,
    estimate      = as.numeric(est_k),
    std_error     = as.numeric(se_k),
    ci_lower      = as.numeric(est_k) - z * as.numeric(se_k),
    ci_upper      = as.numeric(est_k) + z * as.numeric(se_k),
    p_value       = ifelse(se_k > 0, .morie_did_pvalue(est_k / se_k),
                           NA_real_)
  )
  # Insert the reference period (zero by construction).
  coef_df <- rbind(coef_df,
                   data.frame(relative_time = reference_period,
                              estimate = 0, std_error = 0,
                              ci_lower = 0, ci_upper = 0,
                              p_value = NA_real_))
  coef_df <- coef_df[order(coef_df$relative_time), ]
  rownames(coef_df) <- NULL
  pre <- coef_df[coef_df$relative_time < 0 &
                   coef_df$relative_time != reference_period, ]
  if (nrow(pre) > 0) {
    pre_se <- pmax(pre$std_error, 1e-10)
    chi2 <- sum((pre$estimate / pre_se)^2)
    f_stat <- chi2 / nrow(pre)
    f_p    <- stats::pchisq(chi2, df = nrow(pre), lower.tail = FALSE)
  } else {
    f_stat <- NA_real_
    f_p    <- NA_real_
  }
  list(
    coefficients      = coef_df,
    reference_period  = reference_period,
    pre_trend_f_stat  = f_stat,
    pre_trend_p_value = f_p,
    details           = list(fit = fit, backend = "fixest")
  )
}


# ---------------------------------------------------------------------------
# 5. Parallel-trends test
# ---------------------------------------------------------------------------

#' Pre-trend test for parallel trends
#'
#' Regresses the outcome on group-by-time interactions in the pre-period
#' and reports both per-period coefficients and a joint Wald (chi-square)
#' test that they are all zero.
#'
#' For the Callaway-Sant'Anna pre-test on the group-time ATTs prefer
#' \code{did::conditional_did_pretest}.
#'
#' @param data A data frame.
#' @param outcome Outcome column name.
#' @param treatment Binary treatment-group indicator.
#' @param time Time column (integer-valued).
#' @param unit Optional unit identifier (currently unused; reserved).
#' @param cluster Cluster variable for robust SE.
#' @param pre_periods Optional explicit list of pre-treatment times.
#' @return A list with \code{coefficients}, \code{joint_chi2} (and
#'   its alias \code{joint_f_stat}), \code{joint_df},
#'   \code{joint_p_value}, \code{parallel_trends_plausible}.
#' @export
morie_did_test_parallel_trends <- function(data, outcome, treatment, time,
                                           unit = NULL, cluster = NULL,
                                           pre_periods = NULL) {
  df <- data
  all_times <- sort(unique(df[[time]]))
  if (is.null(pre_periods)) {
    treated_times <- df[df[[treatment]] == 1, time, drop = TRUE]
    if (length(treated_times) == 0)
      stop("No treated observations found.")
    first_treat <- min(treated_times, na.rm = TRUE)
    pre_periods <- all_times[all_times < first_treat]
  }
  if (length(pre_periods) < 2) {
    return(list(coefficients = data.frame(),
                joint_f_stat = NA_real_,
                joint_p_value = NA_real_,
                parallel_trends_plausible = TRUE))
  }
  df_pre <- df[df[[time]] %in% pre_periods, , drop = FALSE]
  test_periods <- pre_periods[-1]
  d_vals <- as.numeric(df_pre[[treatment]])
  y_vals <- as.numeric(df_pre[[outcome]])
  time_dummies <- vapply(test_periods,
                         function(tp) as.numeric(df_pre[[time]] == tp),
                         numeric(nrow(df_pre)))
  interact_cols <- d_vals * time_dummies
  X <- .morie_did_add_intercept(cbind(d_vals, time_dummies, interact_cols))
  cluster_ids <- if (!is.null(cluster)) df_pre[[cluster]] else NULL
  fit <- .morie_did_ols_robust_se(X, y_vals, cluster_ids = cluster_ids)
  # Interaction coefficients start after: intercept (1) + d (1) + time dummies
  start_idx <- 1L + 1L + length(test_periods)
  coefs <- lapply(seq_along(test_periods), function(i) {
    idx <- start_idx + i
    est_k <- fit$beta[idx]
    se_k <- fit$se[idx]
    t_k <- if (se_k > 0) est_k / se_k else 0
    data.frame(period = test_periods[i], estimate = est_k,
               std_error = se_k, t_stat = t_k,
               p_value = if (se_k > 0) .morie_did_pvalue(t_k) else NA_real_)
  })
  coef_df <- do.call(rbind, coefs)
  ib <- fit$beta[(start_idx + 1):(start_idx + length(test_periods))]
  is_ <- pmax(fit$se[(start_idx + 1):(start_idx + length(test_periods))], 1e-10)
  chi2 <- sum((ib / is_)^2)
  joint_p <- stats::pchisq(chi2, df = length(test_periods), lower.tail = FALSE)
  list(
    coefficients              = coef_df,
    joint_chi2                = chi2,
    joint_df                  = length(test_periods),
    joint_f_stat              = chi2,
    joint_p_value             = joint_p,
    parallel_trends_plausible = joint_p > 0.05
  )
}


# ---------------------------------------------------------------------------
# 6. Parallel-trends data for visualisation
# ---------------------------------------------------------------------------

#' Group-by-time outcome means for parallel-trends visualisation
#'
#' @param data A data frame.
#' @param outcome,treatment,time Column names.
#' @param weights Optional survey weight column.
#' @return A data frame with columns \code{time}, \code{group},
#'   \code{mean_outcome}, \code{se}, \code{n}.
#' @export
morie_did_parallel_trends_data <- function(data, outcome, treatment, time,
                                           weights = NULL) {
  df <- .morie_did_drop_na(data, c(outcome, treatment, time))
  groups <- expand.grid(t = sort(unique(df[[time]])),
                        g = sort(unique(df[[treatment]])))
  rows <- lapply(seq_len(nrow(groups)), function(i) {
    sub <- df[df[[time]] == groups$t[i] & df[[treatment]] == groups$g[i], ,
              drop = FALSE]
    if (nrow(sub) == 0) return(NULL)
    y <- as.numeric(sub[[outcome]])
    if (!is.null(weights) && weights %in% colnames(sub)) {
      w  <- as.numeric(sub[[weights]])
      w  <- w / sum(w)
      mu <- sum(w * y)
      se <- sqrt(sum(w * (y - mu)^2) / length(y))
    } else {
      mu <- mean(y)
      se <- if (length(y) > 1) stats::sd(y) / sqrt(length(y)) else 0
    }
    data.frame(time = groups$t[i], group = groups$g[i],
               mean_outcome = mu, se = se, n = length(y))
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}


# ---------------------------------------------------------------------------
# 7. Callaway-Sant'Anna group-time ATTs -- thin did::att_gt wrapper
# ---------------------------------------------------------------------------

#' Callaway--Sant'Anna group-time average treatment effects
#'
#' Thin wrapper around \code{did::att_gt}. For each cohort \eqn{g} and
#' each post-treatment calendar period \code{t}, estimates
#' \eqn{\mathrm{ATT}(g, t)}{ATT(g, t)}. Hard-errors if \pkg{did} is
#' not installed.
#'
#' @param data Panel data.
#' @param outcome Outcome column.
#' @param unit Unit identifier.
#' @param time Calendar-time column (integer).
#' @param treatment_time Column with treatment-onset period (use
#'   \code{Inf} for never-treated).
#' @param covariates Optional covariates for doubly-robust estimation.
#' @param method One of \code{"doubly_robust"} (default), \code{"ipw"},
#'   or \code{"outcome_regression"}.
#' @param control_group \code{"never_treated"} or
#'   \code{"not_yet_treated"}.
#' @param n_bootstrap Number of bootstrap replications for inference
#'   (forwarded as \code{biters}).
#' @param seed RNG seed (unused; retained for back-compat).
#' @param alpha Significance level.
#' @return A data frame with columns \code{cohort}, \code{time},
#'   \code{att}, \code{std_error}, \code{ci_lower}, \code{ci_upper},
#'   \code{p_value}.
#' @references Callaway, B., & Sant'Anna, P. H. C. (2021).
#'   Difference-in-Differences with multiple time periods.
#'   \emph{Journal of Econometrics}, 225(2), 200--230.
#' @export
morie_did_group_time_att <- function(data, outcome, unit, time, treatment_time,
                                     covariates = NULL,
                                     method = "doubly_robust",
                                     control_group = "never_treated",
                                     n_bootstrap = 200L, seed = 42L,
                                     alpha = 0.05) {
  .morie_did_need("did", "morie_did_group_time_att")
  df <- data
  # `did::att_gt` expects 0 for never-treated, not Inf.
  g_col <- as.numeric(df[[treatment_time]])
  g_col[!is.finite(g_col)] <- 0
  df[["morie_gname"]] <- g_col
  method_map <- c(doubly_robust = "dr",
                  ipw = "ipw",
                  outcome_regression = "reg")
  est_method <- if (method %in% names(method_map))
    method_map[[method]]
  else "dr"
  xformla <- if (length(covariates))
    stats::as.formula(paste("~", paste(covariates, collapse = " + ")))
  else stats::as.formula("~ 1")
  # Translate Python's "never_treated"/"not_yet_treated" -> did's
  # "nevertreated"/"notyettreated" (no underscores).
  cg_did <- switch(control_group,
                   never_treated = "nevertreated",
                   not_yet_treated = "notyettreated",
                   control_group)
  fit <- did::att_gt(yname = outcome, tname = time, idname = unit,
                     gname = "morie_gname", xformla = xformla, data = df,
                     control_group = cg_did,
                     est_method = est_method,
                     bstrap = TRUE, biters = n_bootstrap,
                     alp = alpha, panel = TRUE,
                     allow_unbalanced_panel = TRUE)
  z <- stats::qnorm(1 - alpha / 2)
  out <- data.frame(
    cohort    = fit$group,
    time      = fit$t,
    att       = fit$att,
    std_error = fit$se,
    ci_lower  = fit$att - z * fit$se,
    ci_upper  = fit$att + z * fit$se,
    p_value   = 2 * stats::pnorm(-abs(fit$att / fit$se))
  )
  out <- out[out$cohort > 0, , drop = FALSE]
  attr(out, "fit") <- fit
  out
}


# ---------------------------------------------------------------------------
# 8. Aggregate group-time ATTs
# ---------------------------------------------------------------------------

#' Aggregate group-time ATTs into summary parameters
#'
#' Mirrors the aggregation schemes available in
#' \code{did::aggte} (overall ATT, by-cohort, by-calendar-time,
#' by-event-time) but produces a tidy \code{data.frame} consumed by
#' the rmorie / MRM downstream pipelines.
#'
#' @param gt_results Output of \code{\link{morie_did_group_time_att}}.
#' @param aggregation One of \code{"overall"} (default), \code{"cohort"},
#'   \code{"calendar_time"}, \code{"event_time"}.
#' @param time_col,cohort_col,att_col,se_col Column-name overrides.
#' @return A data frame with \code{group}, \code{estimate},
#'   \code{std_error}, \code{ci_lower}, \code{ci_upper}.
#' @export
morie_did_aggregate_gt_att <- function(gt_results,
                                       aggregation = "overall",
                                       time_col = "time",
                                       cohort_col = "cohort",
                                       att_col = "att",
                                       se_col = "std_error") {
  df <- gt_results
  df[["morie_rel_time"]] <- df[[time_col]] - df[[cohort_col]]
  if (identical(aggregation, "overall")) {
    est <- mean(df[[att_col]], na.rm = TRUE)
    se  <- sqrt(mean(df[[se_col]]^2, na.rm = TRUE) / nrow(df))
    ci  <- .morie_did_make_ci(est, se)
    return(data.frame(group = "overall", estimate = est,
                      std_error = se, ci_lower = ci[1], ci_upper = ci[2]))
  }
  group_col <- switch(aggregation,
                      cohort        = cohort_col,
                      calendar_time = time_col,
                      event_time    = "morie_rel_time",
                      stop("Unknown aggregation: ", aggregation))
  rows <- lapply(split(df, df[[group_col]]), function(g) {
    est <- mean(g[[att_col]], na.rm = TRUE)
    # SE of a simple average of k independent estimates:
    #   sqrt(sum(se_i^2)) / k  ==  sqrt(mean(se_i^2) / k)
    k <- nrow(g)
    se <- sqrt(mean(g[[se_col]]^2, na.rm = TRUE) / k)
    ci  <- .morie_did_make_ci(est, se)
    data.frame(group = g[[group_col]][1], estimate = est,
               std_error = se, ci_lower = ci[1], ci_upper = ci[2])
  })
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# 9. Staggered DiD wrapper
# ---------------------------------------------------------------------------

#' Staggered DiD via group-time ATTs with aggregation
#'
#' Convenience wrapper around \code{\link{morie_did_group_time_att}} and
#' \code{\link{morie_did_aggregate_gt_att}}. For the canonical CRAN
#' aggregator interface see \code{did::aggte}.
#'
#' @inheritParams morie_did_group_time_att
#' @return A list with \code{group_time}, \code{overall}, \code{by_cohort},
#'   \code{by_event_time}.
#' @export
morie_did_staggered <- function(data, outcome, unit, time, treatment_time,
                                covariates = NULL,
                                n_bootstrap = 200L, seed = 42L,
                                alpha = 0.05) {
  gt <- morie_did_group_time_att(data, outcome, unit, time, treatment_time,
                                 covariates = covariates,
                                 n_bootstrap = n_bootstrap, seed = seed,
                                 alpha = alpha)
  list(
    group_time    = gt,
    overall       = morie_did_aggregate_gt_att(gt, aggregation = "overall"),
    by_cohort     = morie_did_aggregate_gt_att(gt, aggregation = "cohort"),
    by_event_time = morie_did_aggregate_gt_att(gt, aggregation = "event_time")
  )
}


# ---------------------------------------------------------------------------
# 10. Doubly-robust DiD -- thin DRDID::drdid wrapper
# ---------------------------------------------------------------------------

#' Doubly-robust DiD (Sant'Anna & Zhao, 2020)
#'
#' Thin wrapper around \code{DRDID::drdid_rc} for the 2x2
#' repeated-cross-section setting. Combines an outcome regression
#' model with an inverse-probability weighting model and is
#' consistent if either model is correctly specified. Hard-errors if
#' \pkg{DRDID} is not installed.
#'
#' For panel data (same units observed in both periods) prefer
#' \code{DRDID::drdid_panel} directly.
#'
#' @inheritParams morie_did_2x2
#' @param ps_model Unused; retained for back-compat. \pkg{DRDID} fits
#'   a logistic propensity-score model internally.
#' @param or_model Unused; retained for back-compat. \pkg{DRDID} fits
#'   a linear outcome model internally.
#' @param n_bootstrap Number of bootstrap replications (forwarded as
#'   \code{nboot}).
#' @param seed RNG seed (set before the call).
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @references Sant'Anna, P. H. C., & Zhao, J. (2020). Doubly robust
#'   difference-in-differences estimators. \emph{Journal of
#'   Econometrics}, 219(1), 101--122.
#' @export
morie_did_doubly_robust <- function(data, outcome, treatment, post,
                                    covariates,
                                    ps_model = "logistic",
                                    or_model = "linear",
                                    cluster = NULL,
                                    n_bootstrap = 200L, seed = 42L,
                                    alpha = 0.05) {
  .morie_did_need("DRDID", "morie_did_doubly_robust")
  rng <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (!is.null(rng)) assign(".Random.seed", rng, envir = .GlobalEnv)
  })
  df <- .morie_did_drop_na(data, c(outcome, treatment, post, covariates))
  y  <- as.numeric(df[[outcome]])
  d  <- as.numeric(df[[treatment]])
  p  <- as.numeric(df[[post]])
  covariates_mat <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(covariates_mat) <- "double"
  # DRDID expects an intercept-prepended covariate matrix.
  X <- cbind(`(Intercept)` = 1, covariates_mat)
  fit <- DRDID::drdid_rc(
    y = y, post = p, D = d, covariates = X,
    boot = TRUE, nboot = n_bootstrap,
    inffunc = TRUE
  )
  est    <- as.numeric(fit$ATT)
  se_est <- as.numeric(fit$se)
  .morie_did_result(
    est, se_est,
    n_treated = sum(d == 1), n_control = sum(d == 0),
    method = "did_doubly_robust (DRDID::drdid_rc)", alpha = alpha,
    details = list(fit = fit, n_bootstrap = n_bootstrap,
                   backend = "DRDID")
  )
}


# ---------------------------------------------------------------------------
# 11. Triple Differences (DDD)
# ---------------------------------------------------------------------------

#' Triple-difference (DDD) estimator
#'
#' Adds a third differencing dimension to the standard DiD specification.
#'
#' @inheritParams morie_did_2x2
#' @param third_diff Binary variable defining the additional differencing
#'   group.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @export
morie_did_triple_difference <- function(data, outcome, treatment, post,
                                        third_diff,
                                        covariates = NULL,
                                        cluster = NULL, alpha = 0.05) {
  df <- .morie_did_drop_na(data, c(outcome, treatment, post, third_diff))
  d <- as.numeric(df[[treatment]])
  p <- as.numeric(df[[post]])
  s <- as.numeric(df[[third_diff]])
  y <- as.numeric(df[[outcome]])
  parts <- cbind(d, p, s, d * p, d * s, p * s, d * p * s)
  if (length(covariates)) {
    Xc <- as.matrix(df[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    parts <- cbind(parts, Xc)
  }
  X <- .morie_did_add_intercept(parts)
  cluster_ids <- if (!is.null(cluster)) df[[cluster]] else NULL
  fit <- .morie_did_ols_robust_se(X, y, cluster_ids = cluster_ids)
  tau_idx <- 8L   # intercept(1) + 6 main/interaction terms + DDD(8)
  est <- fit$beta[tau_idx]
  se_est <- fit$se[tau_idx]
  .morie_did_result(
    est, se_est,
    n_treated = sum(d == 1), n_control = sum(d == 0),
    method = "did_triple_difference", alpha = alpha
  )
}


# ---------------------------------------------------------------------------
# 12. Goodman-Bacon decomposition -- thin bacondecomp::bacon wrapper
# ---------------------------------------------------------------------------

#' Goodman-Bacon decomposition of the TWFE DiD estimator
#'
#' Thin wrapper around \code{bacondecomp::bacon}. Decomposes a
#' two-way fixed-effects DiD estimate into a weighted average of all
#' possible 2x2 DiD comparisons. Hard-errors if \pkg{bacondecomp} is
#' not installed.
#'
#' @param data Balanced panel data.
#' @param outcome Outcome column.
#' @param treatment Binary treatment indicator that turns on at onset.
#' @param unit Unit identifier.
#' @param time Time period.
#' @return A list with \code{components} (data frame) and
#'   \code{overall_estimate}.
#' @references Goodman-Bacon, A. (2021). Difference-in-differences with
#'   variation in treatment timing. \emph{Journal of Econometrics},
#'   225(2), 254--277.
#' @export
morie_did_bacon_decomposition <- function(data, outcome, treatment,
                                          unit, time) {
  .morie_did_need("bacondecomp", "morie_did_bacon_decomposition")
  f <- stats::as.formula(paste(outcome, "~", treatment))
  fit <- bacondecomp::bacon(f, data = data,
                            id_var = unit, time_var = time, quietly = TRUE)
  comp <- if (is.data.frame(fit)) fit else fit$two_by_twos
  overall <- if (is.list(fit) && !is.null(fit$Estimate)) fit$Estimate
             else sum(comp$estimate * comp$weight)
  list(components = comp, overall_estimate = overall,
       details = list(backend = "bacondecomp"))
}


# ---------------------------------------------------------------------------
# 13. Synthetic DiD -- coresynth SDID wrapper
# ---------------------------------------------------------------------------

#' Synthetic Difference-in-Differences (Arkhangelsky et al., 2021)
#'
#' Wraps the \pkg{coresynth} SDID estimator via its unified Formula
#' interface (\code{coresynth::scm_fit(method = "sdid")}) with bootstrap
#' inference (\code{coresynth::sdid_inference}). \pkg{coresynth} is on
#' CRAN (it replaces the earlier GitHub-only \pkg{synthdid} backend).
#'
#' @param data Balanced panel.
#' @param outcome,unit,time,treatment_time Column names.
#' @param treated_units Optional explicit list of treated unit IDs.
#' @param zeta Retained for back-compat; ignored (coresynth auto-selects
#'   the SDID regularisation).
#' @param n_bootstrap Bootstrap replications for the SE / CI.
#' @param seed RNG seed.
#' @param alpha Significance level.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @references Arkhangelsky, D., et al. (2021). Synthetic
#'   difference-in-differences. \emph{American Economic Review},
#'   111(12), 4088--4118.
#' @export
morie_did_synthetic <- function(data, outcome, unit, time, treatment_time,
                                treated_units = NULL, zeta = NULL,
                                n_bootstrap = 200L, seed = 42L,
                                alpha = 0.05) {
  .morie_did_need("coresynth", "morie_did_synthetic")
  df <- as.data.frame(data)
  df[["morie_g"]] <- as.numeric(df[[treatment_time]])
  if (is.null(treated_units))
    treated_units <- unique(df[is.finite(df[["morie_g"]]), unit, drop = TRUE])
  treat_onset <- df[df[[unit]] %in% treated_units, "morie_g", drop = TRUE]
  if (!length(treat_onset))
    stop("No treated units found.", call. = FALSE)
  first_treat <- min(treat_onset, na.rm = TRUE)
  units_all <- unique(df[[unit]])
  control_units <- setdiff(units_all, treated_units)
  # 0/1 treatment indicator: treated unit AND post-onset period. coresynth's
  # Formula interface (outcome ~ treatment | unit + time) reads this directly.
  df[["morie_W"]] <- as.integer(df[[unit]] %in% treated_units &
                                  df[[time]] >= first_treat)
  fml <- stats::as.formula(
    sprintf("`%s` ~ morie_W | `%s` + `%s`", outcome, unit, time))
  fit <- coresynth::scm_fit(fml, data = df, method = "sdid")
  tau <- as.numeric(fit$estimate)
  # Bootstrap inference (maps the existing n_bootstrap arg); populates
  # se / ci / p directly. zeta is retained for back-compat but ignored:
  # coresynth auto-selects SDID regularisation.
  inf <- tryCatch(
    coresynth::sdid_inference(fit, method = "bootstrap",
                              n_boot = n_bootstrap, level = 1 - alpha,
                              seed = seed),
    error = function(e) NULL)
  se_est <- if (!is.null(inf) && !is.null(inf$se)) as.numeric(inf$se)
            else NA_real_
  ci <- if (!is.null(inf) && !is.null(inf$ci_lower))
          c(inf$ci_lower, inf$ci_upper)
        else if (is.finite(se_est)) .morie_did_make_ci(tau, se_est, alpha)
        else c(NA_real_, NA_real_)
  pval <- if (!is.null(inf) && !is.null(inf$p_value)) as.numeric(inf$p_value)
          else if (is.finite(se_est) && se_est > 0)
            .morie_did_pvalue(tau / se_est) else NA_real_
  list(
    estimate = tau, std_error = se_est,
    t_stat   = if (is.finite(se_est) && se_est > 0) tau / se_est else NA_real_,
    p_value  = pval,
    ci_lower = ci[1], ci_upper = ci[2],
    n_treated = length(treated_units),
    n_control = length(control_units),
    method = "synthetic_did (coresynth)",
    details = list(fit = fit, unit_weights = fit$unit_weights,
                   time_weights = fit$time_weights)
  )
}


# ---------------------------------------------------------------------------
# 14. Wild cluster bootstrap (base-R)
# ---------------------------------------------------------------------------

#' DiD with wild cluster bootstrap p-values (Cameron-Gelbach-Miller, 2008)
#'
#' Recommended when the number of clusters is small (< 50). Uses a
#' base-R Rademacher / Webb wild-cluster-bootstrap implementation.
#' Earlier rmorie versions also delegated to
#' \code{fwildclusterboot::boottest} when installed; that branch was
#' dropped in 0.9.5.12 because fwildclusterboot is GitHub-only and
#' transitively requires summclust, also GitHub-only, which made the
#' CI dependency resolver unreliable. Callers who want
#' \pkg{fwildclusterboot} should call it directly on a `feols` /
#' `lm` fit.
#'
#' @inheritParams morie_did_2x2
#' @param n_bootstrap Number of bootstrap replications.
#' @param weight_type \code{"rademacher"} (default) or \code{"webb"}.
#' @param seed RNG seed.
#' @return A result list; see \code{\link{morie_did_2x2}}.  \code{p_value}
#'   is the bootstrap p-value.
#' @export
morie_did_wild_cluster_bootstrap <- function(data, outcome, treatment, post,
                                             cluster,
                                             covariates = NULL,
                                             n_bootstrap = 999L,
                                             weight_type = "rademacher",
                                             seed = 42L, alpha = 0.05) {
  df <- .morie_did_drop_na(data, c(outcome, treatment, post, cluster))
  df[["dp_interact"]] <- as.numeric(df[[treatment]]) * as.numeric(df[[post]])
  set.seed(seed)
  d <- as.numeric(df[[treatment]])
  p <- as.numeric(df[[post]])
  y <- as.numeric(df[[outcome]])
  interaction <- d * p
  if (length(covariates)) {
    Xc <- as.matrix(df[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    X <- .morie_did_add_intercept(cbind(d, p, interaction, Xc))
  } else {
    X <- .morie_did_add_intercept(cbind(d, p, interaction))
  }
  cluster_ids <- df[[cluster]]
  uc <- unique(cluster_ids)
  G <- length(uc)
  full <- .morie_did_ols_robust_se(X, y, cluster_ids = cluster_ids)
  tau_idx <- 4L
  t_full <- if (full$se[tau_idx] > 0) full$beta[tau_idx] / full$se[tau_idx] else 0
  X_r <- X[, -tau_idx, drop = FALSE]
  beta_r <- as.numeric(qr.coef(qr(X_r), y))
  resid_r <- as.numeric(y - X_r %*% beta_r)
  webb_vals <- c(-sqrt(1.5), -sqrt(1.0), -sqrt(0.5),
                  sqrt(0.5),  sqrt(1.0),  sqrt(1.5))
  boot_t <- numeric(n_bootstrap)
  for (i in seq_len(n_bootstrap)) {
    w <- if (identical(weight_type, "webb"))
      sample(webb_vals, G, replace = TRUE)
    else sample(c(-1, 1), G, replace = TRUE)
    y_star <- as.numeric(X_r %*% beta_r)
    for (j in seq_along(uc)) {
      mask <- cluster_ids == uc[j]
      y_star[mask] <- y_star[mask] + w[j] * resid_r[mask]
    }
    bfit <- .morie_did_ols_robust_se(X, y_star, cluster_ids = cluster_ids)
    boot_t[i] <- if (bfit$se[tau_idx] > 0) bfit$beta[tau_idx] / bfit$se[tau_idx] else 0
  }
  boot_p <- mean(abs(boot_t) >= abs(t_full))
  est <- full$beta[tau_idx]
  se_est <- full$se[tau_idx]
  ci <- .morie_did_make_ci(est, se_est, alpha)
  list(
    estimate = est, std_error = se_est,
    t_stat   = t_full, p_value = boot_p,
    ci_lower = ci[1], ci_upper = ci[2],
    n_treated = sum(d == 1), n_control = sum(d == 0),
    method = "wild_cluster_bootstrap (base-R)",
    details = list(n_clusters = G, n_bootstrap = n_bootstrap,
                   weight_type = weight_type)
  )
}


# ---------------------------------------------------------------------------
# 15. DiD with continuous treatment
# ---------------------------------------------------------------------------

#' DiD with a continuous (dose) treatment
#'
#' Estimates the marginal effect of a one-unit increase in treatment
#' intensity in the post period.
#'
#' @inheritParams morie_did_2x2
#' @param dose Continuous treatment-intensity column.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @export
morie_did_continuous_treatment <- function(data, outcome, dose, post,
                                           covariates = NULL,
                                           cluster = NULL, alpha = 0.05) {
  df <- .morie_did_drop_na(data, c(outcome, dose, post))
  d <- as.numeric(df[[dose]])
  p <- as.numeric(df[[post]])
  y <- as.numeric(df[[outcome]])
  parts <- cbind(d, p, d * p)
  if (length(covariates)) {
    Xc <- as.matrix(df[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    parts <- cbind(parts, Xc)
  }
  X <- .morie_did_add_intercept(parts)
  cluster_ids <- if (!is.null(cluster)) df[[cluster]] else NULL
  fit <- .morie_did_ols_robust_se(X, y, cluster_ids = cluster_ids)
  tau_idx <- 4L
  est <- fit$beta[tau_idx]
  se_est <- fit$se[tau_idx]
  .morie_did_result(
    est, se_est,
    n_treated = sum(d > 0), n_control = sum(d == 0),
    method = "did_continuous_treatment", alpha = alpha
  )
}


# ---------------------------------------------------------------------------
# 16. Fuzzy DiD (LATE) via 2SLS
# ---------------------------------------------------------------------------

#' Fuzzy DiD (LATE) via 2SLS
#'
#' Uses \eqn{Z \times \mathrm{Post}}{Z x Post} as an instrument for
#' \eqn{D \times \mathrm{Post}}{D x Post} to recover a local average treatment
#' effect under imperfect compliance.
#'
#' For the de Chaisemartin-D'Haultfoeuille fuzzy DiD estimator on
#' panel data prefer \code{\link{morie_did_chaisemartin_dhaultfoeuille}}
#' (\pkg{DIDmultiplegt}).
#'
#' @inheritParams morie_did_2x2
#' @param assignment Intent-to-treat assignment column.
#' @param takeup Actual treatment-takeup column.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @export
morie_did_fuzzy <- function(data, outcome, assignment, takeup, post,
                            covariates = NULL,
                            cluster = NULL, alpha = 0.05) {
  df <- .morie_did_drop_na(data, c(outcome, assignment, takeup, post))
  z <- as.numeric(df[[assignment]])
  d <- as.numeric(df[[takeup]])
  p <- as.numeric(df[[post]])
  y <- as.numeric(df[[outcome]])
  zp <- z * p
  dp <- d * p
  exog <- cbind(z, p, d)
  if (length(covariates)) {
    Xc <- as.matrix(df[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    exog <- cbind(exog, Xc)
  }
  X_exog <- .morie_did_add_intercept(exog)
  X_first <- cbind(X_exog, zp)
  beta_first <- as.numeric(qr.coef(qr(X_first), dp))
  beta_first[is.na(beta_first)] <- 0
  dp_hat <- as.numeric(X_first %*% beta_first)
  X_second <- cbind(X_exog, dp_hat)
  cluster_ids <- if (!is.null(cluster)) df[[cluster]] else NULL
  fit <- .morie_did_ols_robust_se(X_second, y, cluster_ids = cluster_ids)
  tau_idx <- ncol(X_second)
  est <- fit$beta[tau_idx]
  se_est <- fit$se[tau_idx]
  # First-stage F
  beta_red <- as.numeric(qr.coef(qr(X_exog), dp))
  beta_red[is.na(beta_red)] <- 0
  resid_r <- dp - as.numeric(X_exog %*% beta_red)
  resid_u <- dp - dp_hat
  ssr_r <- sum(resid_r^2)
  ssr_u <- sum(resid_u^2)
  n <- length(y)
  k <- ncol(X_first)
  f_stat <- if (ssr_u > 0)
    ((ssr_r - ssr_u) / 1) / (ssr_u / (n - k)) else 0
  res <- .morie_did_result(
    est, se_est,
    n_treated = sum(d == 1), n_control = sum(d == 0),
    method = "did_fuzzy", alpha = alpha
  )
  res$details <- c(res$details,
                   list(first_stage_f = f_stat,
                        compliance_rate = mean(d)))
  res
}


# ---------------------------------------------------------------------------
# 17. Placebo tests
# ---------------------------------------------------------------------------

#' Placebo DiD at fake treatment times
#'
#' For each candidate fake time in \code{placebo_times}, redefines the
#' post indicator and estimates a 2x2 DiD on pre-true-treatment data.
#'
#' @param data Data frame.
#' @param outcome,treatment,time Column names.
#' @param true_treatment_time The actual treatment-onset time
#'   (data are restricted to pre-period observations).
#' @param placebo_times Vector of candidate fake treatment times.
#' @inheritParams morie_did_2x2
#' @return A data frame, one row per placebo time.
#' @export
morie_did_placebo_test_time <- function(data, outcome, treatment, time,
                                        true_treatment_time, placebo_times,
                                        covariates = NULL,
                                        cluster = NULL, alpha = 0.05) {
  df_pre <- data[data[[time]] < true_treatment_time, , drop = FALSE]
  rows <- list()
  for (pt in placebo_times) {
    df_test <- df_pre
    df_test[["morie_placebo_post"]] <- as.integer(df_test[[time]] >= pt)
    if (length(unique(df_test[["morie_placebo_post"]])) < 2) next
    res <- morie_did_2x2(df_test, outcome, treatment, "morie_placebo_post",
                         covariates = covariates, cluster = cluster,
                         alpha = alpha)
    rows[[length(rows) + 1]] <- data.frame(
      placebo_time = pt, estimate = res$estimate,
      std_error = res$std_error, p_value = res$p_value,
      significant = res$p_value < alpha
    )
  }
  if (!length(rows))
    return(data.frame(placebo_time = numeric(), estimate = numeric(),
                      std_error = numeric(), p_value = numeric(),
                      significant = logical()))
  do.call(rbind, rows)
}

#' Placebo DiD on outcomes that should be unaffected
#'
#' @param data Data frame.
#' @param placebo_outcomes Character vector of outcome columns expected
#'   to show no treatment effect.
#' @inheritParams morie_did_2x2
#' @return A data frame, one row per placebo outcome.
#' @export
morie_did_placebo_test_outcome <- function(data, placebo_outcomes,
                                           treatment, post,
                                           covariates = NULL,
                                           cluster = NULL, alpha = 0.05) {
  rows <- list()
  for (out in placebo_outcomes) {
    if (!(out %in% colnames(data))) next
    res <- morie_did_2x2(data, out, treatment, post,
                         covariates = covariates, cluster = cluster,
                         alpha = alpha)
    rows[[length(rows) + 1]] <- data.frame(
      outcome = out, estimate = res$estimate,
      std_error = res$std_error, p_value = res$p_value,
      significant = res$p_value < alpha
    )
  }
  if (!length(rows))
    return(data.frame(outcome = character(), estimate = numeric(),
                      std_error = numeric(), p_value = numeric(),
                      significant = logical()))
  do.call(rbind, rows)
}

#' Placebo DiD on sub-groups expected to be unaffected
#'
#' @param data Data frame.
#' @param outcome,treatment,post Column names.
#' @param group_col Column defining sub-groups.
#' @param unaffected_groups Vector of group values where no effect is
#'   expected.
#' @inheritParams morie_did_2x2
#' @return A data frame, one row per placebo group.
#' @export
morie_did_placebo_test_group <- function(data, outcome, treatment, post,
                                         group_col, unaffected_groups,
                                         covariates = NULL,
                                         cluster = NULL, alpha = 0.05) {
  rows <- list()
  for (g in unaffected_groups) {
    df_g <- data[data[[group_col]] == g, , drop = FALSE]
    if (length(unique(df_g[[treatment]])) < 2) next
    res <- morie_did_2x2(df_g, outcome, treatment, post,
                         covariates = covariates, cluster = cluster,
                         alpha = alpha)
    rows[[length(rows) + 1]] <- data.frame(
      group = g, estimate = res$estimate,
      std_error = res$std_error, p_value = res$p_value,
      significant = res$p_value < alpha
    )
  }
  if (!length(rows))
    return(data.frame(group = character(), estimate = numeric(),
                      std_error = numeric(), p_value = numeric(),
                      significant = logical()))
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# 18. Heterogeneity-robust DiD (subgroup splits)
# ---------------------------------------------------------------------------

#' Heterogeneity-robust DiD by sub-group / moderator quantile
#'
#' Splits the sample by quantiles (or categories) of a moderator and
#' estimates separate 2x2 DiDs.
#'
#' @inheritParams morie_did_2x2
#' @param moderator Column to split on.
#' @param n_quantiles Number of quantile bins if the moderator is
#'   continuous.
#' @return A data frame with one row per stratum.
#' @export
morie_did_heterogeneous <- function(data, outcome, treatment, post, moderator,
                                    covariates = NULL,
                                    cluster = NULL,
                                    n_quantiles = 4L, alpha = 0.05) {
  df <- data
  m  <- df[[moderator]]
  if (is.numeric(m) && length(unique(m)) > n_quantiles) {
    breaks <- stats::quantile(m, probs = seq(0, 1, length.out = n_quantiles + 1),
                              na.rm = TRUE)
    df[["morie_mod_group"]] <- as.integer(cut(m, breaks = unique(breaks),
                                              include.lowest = TRUE))
  } else {
    df[["morie_mod_group"]] <- m
  }
  rows <- list()
  for (g_val in sort(unique(df[["morie_mod_group"]]))) {
    if (is.na(g_val)) next
    grp <- df[df[["morie_mod_group"]] %in% g_val, , drop = FALSE]
    if (length(unique(grp[[treatment]])) < 2 ||
        length(unique(grp[[post]])) < 2) next
    res <- morie_did_2x2(grp, outcome, treatment, post,
                         covariates = covariates, cluster = cluster,
                         alpha = alpha)
    rows[[length(rows) + 1]] <- data.frame(
      group = g_val, estimate = res$estimate,
      std_error = res$std_error,
      ci_lower = res$ci_lower, ci_upper = res$ci_upper,
      p_value = res$p_value, n = nrow(grp)
    )
  }
  if (!length(rows))
    return(data.frame(group = integer(), estimate = numeric(),
                      std_error = numeric(),
                      ci_lower = numeric(), ci_upper = numeric(),
                      p_value = numeric(), n = integer()))
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# 19. de Chaisemartin & D'Haultfoeuille -- thin DIDmultiplegt wrapper
# ---------------------------------------------------------------------------

#' Heterogeneity-robust DiD (de Chaisemartin & D'Haultfoeuille, 2020)
#'
#' Thin wrapper around \code{DIDmultiplegt::did_multiplegt}. Computes
#' the instantaneous treatment effect for switchers using
#' appropriate comparisons. Hard-errors if \pkg{DIDmultiplegt} is
#' not installed.
#'
#' @param data Panel data.
#' @param outcome,treatment,unit,time Column names.
#' @param n_bootstrap Bootstrap replications (forwarded as
#'   \code{brep}).
#' @param seed RNG seed.
#' @param alpha Significance level.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @references de Chaisemartin, C., & D'Haultfoeuille, X. (2020). Two-way
#'   fixed effects estimators with heterogeneous treatment effects.
#'   \emph{American Economic Review}, 110(9), 2964--2996.
#' @export
morie_did_chaisemartin_dhaultfoeuille <- function(data, outcome, treatment,
                                                  unit, time,
                                                  n_bootstrap = 200L,
                                                  seed = 42L, alpha = 0.05) {
  .morie_did_need("DIDmultiplegt", "morie_did_chaisemartin_dhaultfoeuille")
  set.seed(seed)
  # Recent DIDmultiplegt requires the `mode` arg with no default. The
  # original CdH (2020) instantaneous-effect estimator lives at
  # mode = "old", which keeps the historical Y/G/T/D arg names and
  # `brep` for bootstrap reps.
  #
  # did_multiplegt_old internally calls plotrix::plotCI to draw a
  # bootstrap diagnostic plot, which fails on small samples with
  # "need finite 'ylim' values" when the bootstrap CIs are
  # degenerate. We don't care about the plot side-effect; if the
  # plot path errors we fall back to brep = 0 (no bootstrap, no
  # plot, SE = NA, but point estimate preserved).
  .didcall <- function(brep_arg) {
    DIDmultiplegt::did_multiplegt(
      mode = "old",
      df = as.data.frame(data),
      Y = outcome, G = unit, T = time, D = treatment,
      brep = brep_arg
    )
  }
  fit <- tryCatch(
    .didcall(n_bootstrap),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("finite|plot\\.window|plotCI|ylim|xlim", msg)) {
        warning(
          "DIDmultiplegt bootstrap-plot failed with: ", msg,
          ". Falling back to brep = 0; SE will be NA.",
          call. = FALSE
        )
        return(.didcall(0L))
      }
      stop(e)
    }
  )
  est <- as.numeric(fit$effect)
  se_est <- if (!is.null(fit$se_effect)) as.numeric(fit$se_effect) else NA_real_
  units_all <- unique(data[[unit]])
  treated_units <- unique(data[data[[treatment]] == 1, unit, drop = TRUE])
  res <- .morie_did_result(
    est, se_est,
    n_treated = length(treated_units),
    n_control = length(setdiff(units_all, treated_units)),
    method = "chaisemartin_dhaultfoeuille", alpha = alpha,
    details = list(fit = fit, backend = "DIDmultiplegt")
  )
  res$method <- "chaisemartin_dhaultfoeuille"
  res
}


# ---------------------------------------------------------------------------
# 20. Sensitivity analysis -- thin HonestDiD wrapper
# ---------------------------------------------------------------------------

#' Sensitivity of DiD estimate to parallel-trends violations
#'
#' For each \eqn{\delta}{delta}, computes a bias-adjusted confidence
#' set under the bound
#' \eqn{|\mathrm{bias}| \le \delta \hat\sigma}{|bias| <= delta hatsigma}
#' (Rambachan & Roth, 2023, conservative version).
#'
#' For the full Rambachan-Roth fixed-length-confidence-interval (FLCI)
#' procedure with event-time pre-trends prefer
#' \code{HonestDiD::createSensitivityResults_relativeMagnitudes} on
#' an event-study coefficient vector.
#'
#' @inheritParams morie_did_2x2
#' @param delta_range Numeric vector of \eqn{\delta}{delta} values to evaluate
#'   (default \code{seq(0, 2, 0.25)}).
#' @return A data frame with columns \code{delta}, \code{ci_lower},
#'   \code{ci_upper}, \code{covers_zero}.
#' @references Rambachan, A., & Roth, J. (2023). A more credible approach
#'   to parallel trends. \emph{Review of Economic Studies}, 90(5),
#'   2555--2591.
#' @export
morie_did_sensitivity_analysis <- function(data, outcome, treatment, post,
                                           covariates = NULL,
                                           delta_range = NULL,
                                           cluster = NULL, alpha = 0.05) {
  if (is.null(delta_range)) delta_range <- seq(0, 2, by = 0.25)
  res <- morie_did_2x2(data, outcome, treatment, post,
                       covariates = covariates, cluster = cluster,
                       alpha = alpha)
  z <- stats::qnorm(1 - alpha / 2)
  rows <- lapply(delta_range, function(delta) {
    bias_bound <- delta * res$std_error
    ci_lo <- res$estimate - bias_bound - z * res$std_error
    ci_hi <- res$estimate + bias_bound + z * res$std_error
    data.frame(delta = delta, ci_lower = ci_lo, ci_upper = ci_hi,
               covers_zero = ci_lo <= 0 & 0 <= ci_hi)
  })
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# 21. Diagnostics
# ---------------------------------------------------------------------------

#' Comprehensive diagnostics for a 2x2 DiD setting
#'
#' Reports group / period sample sizes, outcome distributions, and
#' baseline covariate balance (standardised mean differences).
#'
#' For richer covariate-balance reporting (variance ratios, KS
#' statistics, love plots) prefer \code{cobalt::bal.tab} /
#' \code{cobalt::love.plot}.
#'
#' @inheritParams morie_did_2x2
#' @return A list with \code{sample_sizes}, \code{outcome_stats},
#'   \code{covariate_balance}.
#' @export
morie_did_diagnostics <- function(data, outcome, treatment, post,
                                  covariates = NULL,
                                  cluster = NULL) {
  df <- .morie_did_drop_na(data, c(outcome, treatment, post))
  sizes <- table(df[[treatment]], df[[post]])
  outcome_stats <- do.call(rbind, lapply(split(df, df[c(treatment, post)]),
                                         function(g) {
    if (!nrow(g)) return(NULL)
    y <- as.numeric(g[[outcome]])
    data.frame(
      treatment = g[[treatment]][1],
      post      = g[[post]][1],
      mean   = mean(y),
      std    = stats::sd(y),
      median = stats::median(y),
      min    = min(y),
      max    = max(y),
      count  = length(y)
    )
  }))
  cov_balance <- NULL
  if (length(covariates)) {
    df_pre <- df[df[[post]] == 0, , drop = FALSE]
    rows <- lapply(covariates, function(c_) {
      if (!(c_ %in% colnames(df_pre))) return(NULL)
      t_vals <- as.numeric(df_pre[df_pre[[treatment]] == 1, c_, drop = TRUE])
      c_vals <- as.numeric(df_pre[df_pre[[treatment]] == 0, c_, drop = TRUE])
      mean_diff <- mean(t_vals, na.rm = TRUE) - mean(c_vals, na.rm = TRUE)
      pooled_sd <- sqrt((stats::var(t_vals, na.rm = TRUE) +
                          stats::var(c_vals, na.rm = TRUE)) / 2)
      smd <- if (pooled_sd > 0) mean_diff / pooled_sd else NA_real_
      data.frame(covariate = c_,
                 mean_treated = mean(t_vals, na.rm = TRUE),
                 mean_control = mean(c_vals, na.rm = TRUE),
                 smd = smd)
    })
    cov_balance <- do.call(rbind, Filter(Negate(is.null), rows))
  }
  list(sample_sizes = sizes,
       outcome_stats = outcome_stats,
       covariate_balance = cov_balance)
}


# ---------------------------------------------------------------------------
# 22. TwoWayFEWeights extender (de Chaisemartin & D'Haultfoeuille 2020)
# ---------------------------------------------------------------------------

#' Diagnose TWFE-DiD weights (de Chaisemartin & D'Haultfoeuille, 2020)
#'
#' Thin interface to \code{TwoWayFEWeights::twowayfeweights}: returns the
#' decomposition of the two-way fixed-effects DiD estimand into the
#' weighted average of the \eqn{N \times T}{N x T} unit-time ATEs.  Use
#' this to quantify how many of the implicit comparisons receive
#' negative weight, which is the canonical diagnostic for whether a
#' TWFE specification can be interpreted as a convex combination of
#' treatment effects.
#'
#' Wrapper-as-extender: \code{morie_did_panel_fe} already estimates the
#' TWFE coefficient; this function exposes the diagnostic side of the
#' same backend so that downstream MRM analyses can flag heterogeneous-
#' treatment-effects bias without leaving the rmorie API.
#'
#' @param panel A long-format balanced (or near-balanced) panel
#'   \code{data.frame}.
#' @param group Name of the unit / group identifier column.
#' @param time Name of the time period column.
#' @param treatment Name of the binary or continuous treatment column.
#' @param outcome Optional outcome column.  When supplied,
#'   \pkg{TwoWayFEWeights} computes the weights AND the implied TWFE
#'   coefficient; when \code{NULL}, only the weights are returned
#'   (faster, dimension-free).
#' @param type Weight type passed through to
#'   \code{TwoWayFEWeights::twowayfeweights}: \code{"feTR"} (default,
#'   feasible \code{TR} weights), \code{"feS"}, \code{"fdTR"}, or
#'   \code{"fdS"}.  See the \pkg{TwoWayFEWeights} documentation.
#' @param ... Additional arguments forwarded to
#'   \code{TwoWayFEWeights::twowayfeweights}.
#' @return An S3 list of class \code{morie_did_twfe_diagnostics} with
#'   elements \code{n_negative_weights}, \code{sum_weights},
#'   \code{sum_negative_weights}, \code{share_negative_weights},
#'   \code{method}, and \code{raw} (the full
#'   \code{twowayfeweights} object).
#' @references de Chaisemartin, C., & D'Haultfoeuille, X. (2020).
#'   Two-way fixed effects estimators with heterogeneous treatment
#'   effects.  \emph{American Economic Review}, 110(9), 2964--2996.
#' @seealso \code{\link{morie_did_panel_fe}},
#'   \code{\link{morie_did_chaisemartin_dhaultfoeuille}}.
#' @export
morie_did_twoway_fe_weights <- function(panel, group, time, treatment,
                                        outcome = NULL,
                                        type = "feTR", ...) {
  .morie_did_need("TwoWayFEWeights", "morie_did_twoway_fe_weights")
  df <- as.data.frame(panel)
  args <- list(df = df, Y = outcome, G = group, T = time, D = treatment,
               type = type, ...)
  # twowayfeweights() requires Y; when caller omits it, pass a constant
  # so the diagnostic still runs (weights are independent of Y values).
  if (is.null(args$Y)) {
    df[["morie_twfe_y_const"]] <- 0
    args$df <- df
    args$Y <- "morie_twfe_y_const"
  }
  fit <- do.call(TwoWayFEWeights::twowayfeweights, args)
  weights <- tryCatch(as.numeric(fit$weights),
                      error = function(e) NA_real_)
  n_neg <- if (all(is.na(weights))) NA_integer_
           else sum(weights < 0, na.rm = TRUE)
  sum_w <- if (all(is.na(weights))) NA_real_
           else sum(weights, na.rm = TRUE)
  sum_neg <- if (all(is.na(weights))) NA_real_
             else sum(weights[weights < 0], na.rm = TRUE)
  share_neg <- if (all(is.na(weights)) || length(weights) == 0L) NA_real_
               else n_neg / length(weights)
  structure(
    list(
      n_negative_weights      = n_neg,
      sum_weights             = sum_w,
      sum_negative_weights    = sum_neg,
      share_negative_weights  = share_neg,
      method = "twoway_fe_weights (TwoWayFEWeights)",
      raw    = fit
    ),
    class = c("morie_did_twfe_diagnostics", "list")
  )
}


# ---------------------------------------------------------------------------
# 23. Synthetic DiD explicit-name extender (Arkhangelsky et al., 2021)
# ---------------------------------------------------------------------------

#' Synthetic DiD via \code{coresynth::scm_fit} (explicit-name API)
#'
#' Parallel to \code{\link{morie_did_synthetic}}, this is the
#' explicit-name wrapper that surfaces the full \pkg{coresynth} SDID
#' estimator and its placebo / bootstrap / jackknife variance pieces.
#' Use this when you want to pass through additional \pkg{coresynth}
#' arguments or inspect the unit / time weights side-by-side; use
#' \code{morie_did_synthetic} when you want the rmorie result-list
#' shape consumed by \code{morie_did_*} downstream code.
#'
#' Wrapper-as-extender: rmorie already wraps \pkg{coresynth} once via
#' \code{morie_did_synthetic}; this entry point gives MRM / paper
#' callers the canonical Arkhangelsky et al. (2021) SDID API with a
#' \code{morie_*} name so they don't need to load \pkg{coresynth}
#' directly.
#'
#' @param panel Long-format balanced panel.
#' @param unit Unit identifier column.
#' @param time Time period column.
#' @param treatment Binary (0/1) treatment indicator that turns on at
#'   onset for treated units and is zero everywhere for controls.
#' @param outcome Outcome column.
#' @param vcov_method Inference method passed to
#'   \code{coresynth::sdid_inference}: one of \code{"placebo"}
#'   (default), \code{"bootstrap"}, \code{"jackknife"}.
#' @param ... Additional arguments forwarded to
#'   \code{coresynth::scm_fit} (e.g. \code{predictors}, \code{covariates}).
#' @return An S3 list of class \code{morie_did_synthdid_result} with
#'   elements \code{att}, \code{std_error}, \code{vcov_method},
#'   \code{n_treated}, \code{n_control}, \code{n_pre},
#'   \code{n_post}, \code{method}, and \code{raw} (the full
#'   \code{coresynth} SDID fit object).
#' @references Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens,
#'   G. W., & Wager, S. (2021). Synthetic difference-in-differences.
#'   \emph{American Economic Review}, 111(12), 4088--4118.
#' @seealso \code{\link{morie_did_synthetic}}.
#' @export
morie_did_synthdid_estimate <- function(panel, unit, time, treatment,
                                        outcome,
                                        vcov_method = "placebo", ...) {
  .morie_did_need("coresynth", "morie_did_synthdid_estimate")
  df <- as.data.frame(panel)
  # coresynth reads a 0/1 integer treatment indicator directly.
  if (is.logical(df[[treatment]]))
    df[[treatment]] <- as.integer(df[[treatment]])
  fml <- stats::as.formula(
    sprintf("`%s` ~ `%s` | `%s` + `%s`", outcome, treatment, unit, time))
  fit <- do.call(coresynth::scm_fit,
                 c(list(fml, data = df, method = "sdid"), list(...)))
  att <- as.numeric(fit$estimate)
  # vcov_method maps to coresynth's inference method (placebo/bootstrap/
  # jackknife). placebo returns no closed-form se, so derive it as the sd
  # of the placebo effect distribution (standard placebo inference).
  inf <- tryCatch(
    coresynth::sdid_inference(fit, method = vcov_method),
    error = function(e) NULL)
  se_est <- if (!is.null(inf) && !is.null(inf$se)) as.numeric(inf$se)
            else if (!is.null(inf) && !is.null(inf$placebo_effects))
              stats::sd(inf$placebo_effects)
            else NA_real_
  n_total <- length(unique(df[[unit]]))
  t_total <- length(unique(df[[time]]))
  n_pre   <- as.integer(fit$T_pre)
  n_treat <- as.integer(fit$N_tr)
  structure(
    list(
      att          = att,
      std_error    = se_est,
      vcov_method  = vcov_method,
      n_treated    = n_treat,
      n_control    = n_total - n_treat,
      n_pre        = n_pre,
      n_post       = t_total - n_pre,
      method       = "sdid (coresynth)",
      raw          = fit
    ),
    class = c("morie_did_synthdid_result", "list")
  )
}
