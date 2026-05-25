# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Difference-in-Differences (DiD) estimators for morie.  Ports the public
# API of `src/morie/did.py` (~2593 LOC) to R.
#
# Strategy: prefer CRAN wrappers.  Two-way fixed-effects DiD dispatches to
# `fixest::feols`; Callaway--Sant'Anna group-time ATTs to `did::att_gt`;
# Goodman-Bacon decomposition to `bacondecomp::bacon`; synthetic DiD to
# `synthdid::synthdid_estimate`; wild cluster bootstrap to
# `fwildclusterboot::boottest`; cluster-robust SE to `sandwich::vcovCL`.
# When an optional CRAN package is unavailable we either fall back to a
# hand-rolled base-R implementation that mirrors the Python module or
# raise a clean stop() with an install hint, depending on the
# delegation's complexity.
#
# Public R names mirror the Python module under the `morie_did_*` prefix.

#' @importFrom stats lm glm coef vcov pnorm pt pf pchisq qnorm qt qchisq
#'   model.matrix model.frame fitted residuals binomial as.formula sigma
#'   complete.cases quantile predict ave sd var aggregate na.omit
#'   reshape lsfit setNames
#' @importFrom utils combn head
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.morie_did_have_fixest         <- function() requireNamespace("fixest",         quietly = TRUE)
.morie_did_have_did            <- function() requireNamespace("did",            quietly = TRUE)
.morie_did_have_bacondecomp    <- function() requireNamespace("bacondecomp",    quietly = TRUE)
.morie_did_have_synthdid       <- function() requireNamespace("synthdid",       quietly = TRUE)
.morie_did_have_sandwich       <- function() requireNamespace("sandwich",       quietly = TRUE)

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
  cols <- c(outcome, treatment, post, covariates, cluster)
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
# 3. Panel two-way fixed-effects DiD
# ---------------------------------------------------------------------------

#' Two-way fixed-effects DiD (panel)
#'
#' Estimates \eqn{Y_{it} = \alpha_i + \lambda_t + \tau D_{it} + X'\delta
#' + \varepsilon_{it}}{Y_it = alpha_i + lambda_t + tau D_it + X'delta + varepsilon_it}.  Prefers \code{fixest::feols} for fast within
#' estimation with cluster-robust SE; falls back to a base-R two-way
#' within-transform when \pkg{fixest} is not installed.
#'
#' @inheritParams morie_did_2x2
#' @param unit Unit identifier column.
#' @param time Time period column.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @export
morie_did_panel_fe <- function(data, outcome, treatment, unit, time,
                               covariates = NULL, cluster = NULL,
                               alpha = 0.05) {
  df <- .morie_did_drop_na(data, c(outcome, treatment, unit, time))
  if (.morie_did_have_fixest()) {
    rhs <- if (length(covariates))
      paste(c(treatment, covariates), collapse = " + ")
    else treatment
    fe_part <- paste(unit, time, sep = " + ")
    f <- stats::as.formula(paste(outcome, "~", rhs, "|", fe_part))
    cluster_var <- if (!is.null(cluster)) cluster else unit
    fit <- fixest::feols(f, data = df,
                         cluster = stats::as.formula(paste0("~", cluster_var)))
    cf <- fixest::coeftable(fit)
    est    <- cf[treatment, "Estimate"]
    se_est <- cf[treatment, "Std. Error"]
    return(.morie_did_result(
      est, se_est,
      n_treated = sum(as.numeric(df[[treatment]]) == 1),
      n_control = sum(as.numeric(df[[treatment]]) == 0),
      method = "did_panel_fe (fixest)", alpha = alpha,
      details = list(fit = fit,
                     n_units   = length(unique(df[[unit]])),
                     n_periods = length(unique(df[[time]])))
    ))
  }
  # Base-R two-way within fallback
  y_dm <- .morie_did_within_transform(df, outcome,   unit, time)
  d_dm <- .morie_did_within_transform(df, treatment, unit, time)
  cols <- list(d_dm)
  if (length(covariates)) {
    for (c_ in covariates)
      cols[[length(cols) + 1]] <- .morie_did_within_transform(df, c_, unit, time)
  }
  X <- do.call(cbind, cols)
  colnames(X) <- c(treatment, covariates)
  cluster_ids <- if (!is.null(cluster)) df[[cluster]] else df[[unit]]
  fit <- .morie_did_ols_robust_se(X, y_dm, cluster_ids = cluster_ids)
  est <- fit$beta[1]
  se_est <- fit$se[1]
  .morie_did_result(
    est, se_est,
    n_treated = sum(as.numeric(df[[treatment]]) == 1),
    n_control = sum(as.numeric(df[[treatment]]) == 0),
    method = "did_panel_fe (base-R within)", alpha = alpha,
    details = list(n_units   = length(unique(df[[unit]])),
                   n_periods = length(unique(df[[time]])))
  )
}


# ---------------------------------------------------------------------------
# 4. Event study
# ---------------------------------------------------------------------------

#' Event-study DiD specification
#'
#' Constructs relative-time dummies \eqn{1\{t - g = k\}} for
#' \eqn{k \in [-\text{leads}, \text{lags}]}{k in [-leads, lags]} (omitting
#' \code{reference_period}) and regresses the outcome on these
#' indicators with unit and time fixed effects.
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
  df <- data
  df[["_rel_time"]] <- as.numeric(df[[time]]) - as.numeric(df[[treatment_time]])
  periods <- setdiff(seq.int(-leads, lags), reference_period)
  for (k in periods) {
    nm <- paste0("_rel_", k)
    df[[nm]] <- as.numeric(df[["_rel_time"]] == k)
    df[[nm]][is.na(df[[nm]])] <- 0
  }
  y_dm <- .morie_did_within_transform(df, outcome, unit, time)
  X_cols <- paste0("_rel_", periods)
  if (length(covariates)) X_cols <- c(X_cols, covariates)
  X_dm <- vapply(X_cols, function(nm) .morie_did_within_transform(df, nm, unit, time),
                 numeric(nrow(df)))
  cluster_ids <- if (!is.null(cluster)) df[[cluster]] else df[[unit]]
  fit <- .morie_did_ols_robust_se(X_dm, y_dm, cluster_ids = cluster_ids)
  beta <- fit$beta
  se <- fit$se
  coefs <- lapply(seq_along(periods), function(i) {
    est_k <- beta[i]
    se_k <- se[i]
    ci <- if (is.finite(se_k)) .morie_did_make_ci(est_k, se_k, alpha) else c(NA, NA)
    p_k <- if (se_k > 0) .morie_did_pvalue(est_k / se_k) else NA_real_
    data.frame(relative_time = periods[i], estimate = est_k,
               std_error = se_k, ci_lower = ci[1], ci_upper = ci[2],
               p_value = p_k)
  })
  coef_df <- do.call(rbind, coefs)
  # Add the reference period (zero by construction)
  coef_df <- rbind(coef_df,
                   data.frame(relative_time = reference_period,
                              estimate = 0, std_error = 0,
                              ci_lower = 0, ci_upper = 0,
                              p_value = NA_real_))
  coef_df <- coef_df[order(coef_df$relative_time), ]
  rownames(coef_df) <- NULL
  pre_idx <- which(periods < 0)
  if (length(pre_idx) > 0) {
    pre_beta <- beta[pre_idx]
    pre_se   <- pmax(se[pre_idx], 1e-10)
    chi2 <- sum((pre_beta / pre_se)^2)
    f_stat <- chi2 / length(pre_idx)
    f_p    <- stats::pchisq(chi2, df = length(pre_idx), lower.tail = FALSE)
  } else {
    f_stat <- NA_real_
    f_p <- NA_real_
  }
  list(
    coefficients      = coef_df,
    reference_period  = reference_period,
    pre_trend_f_stat  = f_stat,
    pre_trend_p_value = f_p,
    details           = list(beta = beta, se = se, periods = periods)
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
  ref_period <- pre_periods[1]
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
  # chi2 already IS a chi-square; dividing by k gave a mean-chi-square,
  # NOT an F. p-value uses pchisq(chi2, k). Report both for callers that
  # need either label; keep joint_f_stat as alias of joint_chi2 for
  # backward compatibility.
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
# 7. Callaway-Sant'Anna group-time ATTs
# ---------------------------------------------------------------------------

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

#' Callaway--Sant'Anna group-time average treatment effects
#'
#' For each cohort \eqn{g} and each post-treatment calendar period
#' \code{t}, estimates \eqn{\mathrm{ATT}(g, t)}{ATT(g, t)}.  Prefers
#' \code{did::att_gt} (the reference implementation by Callaway &
#' Sant'Anna); falls back to a base-R bootstrap-based estimator that
#' mirrors the Python module when \pkg{did} is unavailable.
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
#' @param n_bootstrap Number of bootstrap replications for inference.
#' @param seed RNG seed.
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
  if (.morie_did_have_did()) {
    df <- data
    # `did::att_gt` expects 0 for never-treated, not Inf.
    g_col <- as.numeric(df[[treatment_time]])
    g_col[!is.finite(g_col)] <- 0
    df[["_gname"]] <- g_col
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
    # "nevertreated"/"notyettreated" (no underscores). Without this,
    # did::att_gt errors and the CRAN-delegated path is dead.
    cg_did <- switch(control_group,
                     never_treated = "nevertreated",
                     not_yet_treated = "notyettreated",
                     control_group)
    fit <- did::att_gt(yname = outcome, tname = time, idname = unit,
                       gname = "_gname", xformla = xformla, data = df,
                       control_group = cg_did,
                       est_method = est_method,
                       bstrap = TRUE, biters = n_bootstrap,
                       alp = alpha, panel = TRUE, allow_unbalanced_panel = TRUE)
    out <- data.frame(
      cohort    = fit$group,
      time      = fit$t,
      att       = fit$att,
      std_error = fit$se,
      ci_lower  = fit$att - stats::qnorm(1 - alpha / 2) * fit$se,
      ci_upper  = fit$att + stats::qnorm(1 - alpha / 2) * fit$se,
      p_value   = 2 * stats::pnorm(-abs(fit$att / fit$se))
    )
    out <- out[out$cohort > 0, , drop = FALSE]
    attr(out, "fit") <- fit
    return(out)
  }
  # Base-R fallback ------------------------------------------------------------
  rng <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (!is.null(rng)) assign(".Random.seed", rng, envir = .GlobalEnv)
  })
  df <- data
  df[["_g"]] <- as.numeric(df[[treatment_time]])
  cohorts <- sort(unique(df[is.finite(df[["_g"]]), "_g"]))
  all_times <- sort(unique(df[[time]]))
  results <- list()
  estimate_one <- function(y_d, treat_ind, X_cov) {
    if (identical(method, "ipw")) {
      ps <- if (ncol(X_cov) > 0 && stats::sd(treat_ind) > 0) {
        gfit <- tryCatch(stats::glm.fit(cbind(1, X_cov), treat_ind,
                                        family = stats::binomial("logit")),
                         error = function(e) NULL)
        if (is.null(gfit)) rep(mean(treat_ind), length(treat_ind))
        else 1 / (1 + exp(-as.numeric(cbind(1, X_cov) %*% gfit$coefficients)))
      } else rep(mean(treat_ind), length(treat_ind))
      return(.morie_did_ipw_att(y_d, as.integer(treat_ind), ps))
    }
    if (identical(method, "outcome_regression"))
      return(.morie_did_outcome_regression_att(y_d, X_cov,
                                               as.integer(treat_ind)))
    # Doubly robust (simplified, mirrors did.py)
    if (sum(treat_ind == 0) < 2) {
      if (sum(treat_ind == 1) > 0) return(mean(y_d[treat_ind == 1]))
      return(0)
    }
    fit_o <- stats::lm.fit(cbind(1, X_cov[treat_ind == 0, , drop = FALSE]),
                           y_d[treat_ind == 0])
    b <- fit_o$coefficients
    b[is.na(b)] <- 0
    mu0 <- as.numeric(cbind(1, X_cov) %*% b)
    if (sum(treat_ind == 1) == 0) return(0)
    mean(y_d[treat_ind == 1] - mu0[treat_ind == 1])
  }
  for (g in cohorts) {
    post_times <- all_times[all_times >= g]
    pre_times  <- all_times[all_times < g]
    if (length(pre_times) == 0) next
    pre_time <- max(pre_times)
    for (tt in post_times) {
      cohort_units <- unique(df[df[["_g"]] == g, unit, drop = TRUE])
      ctrl_units <- if (identical(control_group, "never_treated"))
        unique(df[!is.finite(df[["_g"]]), unit, drop = TRUE])
      else
        unique(df[df[["_g"]] > tt, unit, drop = TRUE])
      if (!length(ctrl_units) || !length(cohort_units)) next
      relevant <- c(cohort_units, ctrl_units)
      sub <- df[df[[unit]] %in% relevant & df[[time]] %in% c(pre_time, tt), ,
                drop = FALSE]
      wide_pre  <- stats::aggregate(stats::as.formula(paste(outcome, "~", unit)),
                                    data = sub[sub[[time]] == pre_time, ],
                                    FUN = mean)
      wide_post <- stats::aggregate(stats::as.formula(paste(outcome, "~", unit)),
                                    data = sub[sub[[time]] == tt, ],
                                    FUN = mean)
      m <- merge(wide_pre, wide_post, by = unit,
                 suffixes = c(".pre", ".post"))
      if (!nrow(m)) next
      delta_y <- as.numeric(m[[paste0(outcome, ".post")]] -
                              m[[paste0(outcome, ".pre")]])
      treat_indicator <- as.numeric(m[[unit]] %in% cohort_units)
      cov_data <- if (length(covariates)) {
        idx <- match(m[[unit]], df[df[[time]] == pre_time, unit, drop = TRUE])
        as.matrix(df[df[[time]] == pre_time, ][idx, covariates, drop = FALSE])
      } else matrix(1, nrow = nrow(m), ncol = 1)
      storage.mode(cov_data) <- "double"
      att_hat <- estimate_one(delta_y, treat_indicator, cov_data)
      n <- length(delta_y)
      boot <- replicate(n_bootstrap, {
        idx <- sample.int(n, n, replace = TRUE)
        tryCatch(estimate_one(delta_y[idx], treat_indicator[idx],
                              cov_data[idx, , drop = FALSE]),
                 error = function(e) NA_real_)
      })
      boot <- boot[is.finite(boot)]
      se_hat <- if (length(boot) > 1) stats::sd(boot) else NA_real_
      ci <- if (is.finite(se_hat)) .morie_did_make_ci(att_hat, se_hat, alpha)
      else c(NA_real_, NA_real_)
      p_val <- if (is.finite(se_hat) && se_hat > 0)
        .morie_did_pvalue(att_hat / se_hat) else NA_real_
      results[[length(results) + 1]] <- data.frame(
        cohort = g, time = tt, att = att_hat,
        std_error = se_hat, ci_lower = ci[1], ci_upper = ci[2],
        p_value = p_val
      )
    }
  }
  if (!length(results))
    return(data.frame(cohort = numeric(), time = numeric(), att = numeric(),
                      std_error = numeric(), ci_lower = numeric(),
                      ci_upper = numeric(), p_value = numeric()))
  do.call(rbind, results)
}


# ---------------------------------------------------------------------------
# 8. Aggregate group-time ATTs
# ---------------------------------------------------------------------------

#' Aggregate group-time ATTs into summary parameters
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
  df[["_rel_time"]] <- df[[time_col]] - df[[cohort_col]]
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
                      event_time    = "_rel_time",
                      stop("Unknown aggregation: ", aggregation))
  rows <- lapply(split(df, df[[group_col]]), function(g) {
    est <- mean(g[[att_col]], na.rm = TRUE)
    # SE of a simple average of k independent estimates:
    #   sqrt(sum(se_i^2)) / k  ==  sqrt(mean(se_i^2) / k)
    # (was: sqrt(mean(se^2)/nrow(g)) which collides with `k` only when
    # nrow(g) == 1; underestimated by sqrt(k) for k > 1.)
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
#' \code{\link{morie_did_aggregate_gt_att}}.
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
# 10. Doubly-robust DiD (Sant'Anna & Zhao 2020)
# ---------------------------------------------------------------------------

#' Doubly-robust DiD (Sant'Anna & Zhao, 2020)
#'
#' Combines an outcome regression model with an inverse-probability
#' weighting model.  Consistent if either model is correctly specified.
#'
#' @inheritParams morie_did_2x2
#' @param ps_model One of \code{"logistic"} (default) or \code{"gbm"}.
#' @param or_model One of \code{"linear"} (default) or \code{"gbm"}.
#' @param n_bootstrap Number of bootstrap replications.
#' @param seed RNG seed.
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
  if ((identical(ps_model, "gbm") || identical(or_model, "gbm")) &&
      !requireNamespace("gbm", quietly = TRUE))
    stop("ps_model='gbm' or or_model='gbm' requires the 'gbm' package: ",
         "install.packages('gbm')", call. = FALSE)
  rng <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (!is.null(rng)) assign(".Random.seed", rng, envir = .GlobalEnv)
  })
  df <- .morie_did_drop_na(data, c(outcome, treatment, post, covariates))
  d <- as.numeric(df[[treatment]])
  p <- as.numeric(df[[post]])
  y <- as.numeric(df[[outcome]])
  Xc <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(Xc) <- "double"

  dr_estimate <- function(d_v, p_v, y_v, X_v) {
    ps_vals <- if (identical(ps_model, "gbm")) {
      df_ps <- data.frame(d = d_v, X_v)
      g <- gbm::gbm(d ~ ., data = df_ps, distribution = "bernoulli",
                    n.trees = 50, interaction.depth = 3,
                    bag.fraction = 1, train.fraction = 1,
                    keep.data = FALSE, verbose = FALSE)
      gbm::predict.gbm(g, newdata = df_ps, n.trees = 50, type = "response")
    } else {
      gfit <- tryCatch(stats::glm.fit(cbind(1, X_v), d_v,
                                      family = stats::binomial("logit")),
                       error = function(e) NULL)
      if (is.null(gfit)) rep(mean(d_v), length(d_v))
      else 1 / (1 + exp(-as.numeric(cbind(1, X_v) %*% gfit$coefficients)))
    }
    ps_vals <- pmin(pmax(ps_vals, 0.01), 0.99)
    ctrl_post  <- d_v == 0 & p_v == 1
    ctrl_pre   <- d_v == 0 & p_v == 0
    treat_post <- d_v == 1 & p_v == 1
    treat_pre  <- d_v == 1 & p_v == 0
    if (sum(ctrl_post) < 2 || sum(ctrl_pre) < 2) {
      if (sum(treat_post) && sum(treat_pre))
        return((mean(y_v[treat_post]) - mean(y_v[treat_pre])) -
                 (mean(y_v[ctrl_post]) - mean(y_v[ctrl_pre])))
      return(0)
    }
    fit_or <- function(mask) {
      if (identical(or_model, "gbm")) {
        df_or <- data.frame(y = y_v[mask], X_v[mask, , drop = FALSE])
        g <- gbm::gbm(y ~ ., data = df_or, distribution = "gaussian",
                      n.trees = 50, interaction.depth = 3,
                      bag.fraction = 1, train.fraction = 1,
                      keep.data = FALSE, verbose = FALSE)
        gbm::predict.gbm(g, newdata = data.frame(y = 0, X_v),
                         n.trees = 50)
      } else {
        fit <- stats::lm.fit(cbind(1, X_v[mask, , drop = FALSE]), y_v[mask])
        b <- fit$coefficients
        b[is.na(b)] <- 0
        as.numeric(cbind(1, X_v) %*% b)
      }
    }
    mu0_post <- fit_or(ctrl_post)
    mu0_pre  <- fit_or(ctrl_pre)
    if (sum(d_v == 1) == 0) return(0)
    att_or <- mean(y_v[treat_post] - mu0_post[treat_post]) -
              mean(y_v[treat_pre]  - mu0_pre[treat_pre])
    w <- ps_vals / (1 - ps_vals)
    ipw_correction <- 0
    if (sum(d_v == 0) > 0) {
      m_post <- d_v == 0 & p_v == 1
      m_pre  <- d_v == 0 & p_v == 0
      if (sum(m_post) && sum(m_pre)) {
        w_post <- w[m_post]
        w_pre <- w[m_pre]
        rp <- y_v[m_post] - mu0_post[m_post]
        rq <- y_v[m_pre]  - mu0_pre[m_pre]
        ipw_correction <- sum(w_post * rp) / max(sum(w_post), 1e-10) -
                          sum(w_pre  * rq) / max(sum(w_pre),  1e-10)
      }
    }
    att_or - ipw_correction
  }

  est <- dr_estimate(d, p, y, Xc)
  n <- nrow(df)
  boot <- replicate(n_bootstrap, {
    idx <- sample.int(n, n, replace = TRUE)
    tryCatch(dr_estimate(d[idx], p[idx], y[idx], Xc[idx, , drop = FALSE]),
             error = function(e) NA_real_)
  })
  boot <- boot[is.finite(boot)]
  se_est <- if (length(boot) > 1) stats::sd(boot) else NA_real_
  .morie_did_result(
    est, se_est,
    n_treated = sum(d == 1), n_control = sum(d == 0),
    method = "did_doubly_robust", alpha = alpha,
    details = list(n_bootstrap = length(boot),
                   ps_model = ps_model, or_model = or_model)
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
# 12. Goodman-Bacon decomposition
# ---------------------------------------------------------------------------

#' Goodman-Bacon decomposition of the TWFE DiD estimator
#'
#' Decomposes a two-way fixed-effects DiD estimate into a weighted
#' average of all possible 2x2 DiD comparisons.  Prefers
#' \code{bacondecomp::bacon}; falls back to a base-R implementation that
#' mirrors the Python module otherwise.
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
  if (.morie_did_have_bacondecomp()) {
    f <- stats::as.formula(paste(outcome, "~", treatment))
    fit <- tryCatch(
      bacondecomp::bacon(f, data = data,
                         id_var = unit, time_var = time, quietly = TRUE),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      comp <- if (is.data.frame(fit)) fit else fit$two_by_twos
      overall <- if (is.list(fit) && !is.null(fit$Estimate)) fit$Estimate
                 else sum(comp$estimate * comp$weight)
      return(list(components = comp, overall_estimate = overall,
                  details = list(backend = "bacondecomp")))
    }
  }
  # Base-R fallback ------------------------------------------------------------
  df <- data
  periods <- sort(unique(df[[time]]))
  T_total <- length(periods)
  unit_timing <- vapply(unique(df[[unit]]), function(u) {
    treated_t <- df[df[[unit]] == u & df[[treatment]] == 1, time, drop = TRUE]
    if (!length(treated_t)) Inf else min(treated_t)
  }, numeric(1))
  names(unit_timing) <- unique(df[[unit]])
  df[["_treat_time"]] <- unit_timing[as.character(df[[unit]])]
  timing_groups <- split(names(unit_timing), unit_timing)
  group_keys <- sort(as.numeric(names(timing_groups))[
    is.finite(as.numeric(names(timing_groups)))])
  never_key <- if ("Inf" %in% names(timing_groups)) "Inf" else NULL
  simple_2x2 <- function(units_t, units_c, pre_p, post_p) {
    f <- function(units, pds)
      df[df[[unit]] %in% units & df[[time]] %in% pds, outcome, drop = TRUE]
    t_pre  <- f(units_t, pre_p)
    t_post <- f(units_t, post_p)
    c_pre  <- f(units_c, pre_p)
    c_post <- f(units_c, post_p)
    if (!length(t_pre) || !length(t_post) ||
        !length(c_pre) || !length(c_post)) return(NULL)
    (mean(t_post) - mean(t_pre)) - (mean(c_post) - mean(c_pre))
  }
  components <- list()
  if (length(group_keys) >= 2) {
    pairs <- utils::combn(group_keys, 2)
    for (idx in seq_len(ncol(pairs))) {
      g_e <- pairs[1, idx]
      g_l <- pairs[2, idx]
      pre_pds <- periods[periods < g_e]
      mid_pds <- periods[periods >= g_e & periods < g_l]
      if (length(pre_pds) && length(mid_pds)) {
        est <- simple_2x2(timing_groups[[as.character(g_e)]],
                          timing_groups[[as.character(g_l)]],
                          pre_pds, mid_pds)
        if (!is.null(est)) {
          n_e <- length(timing_groups[[as.character(g_e)]])
          n_l <- length(timing_groups[[as.character(g_l)]])
          w <- (n_e * n_l * length(mid_pds) * length(pre_pds)) / T_total^2
          components[[length(components) + 1]] <- data.frame(
            group1 = g_e, group2 = g_l, estimate = est,
            weight = w, type = "earlier_vs_later"
          )
        }
      }
      post_pds <- periods[periods >= g_l]
      if (length(mid_pds) && length(post_pds)) {
        est <- simple_2x2(timing_groups[[as.character(g_l)]],
                          timing_groups[[as.character(g_e)]],
                          mid_pds, post_pds)
        if (!is.null(est)) {
          n_e <- length(timing_groups[[as.character(g_e)]])
          n_l <- length(timing_groups[[as.character(g_l)]])
          w <- (n_e * n_l * length(post_pds) * length(mid_pds)) / T_total^2
          components[[length(components) + 1]] <- data.frame(
            group1 = g_l, group2 = g_e, estimate = est,
            weight = w, type = "later_vs_earlier"
          )
        }
      }
    }
  }
  if (!is.null(never_key)) {
    for (g in group_keys) {
      pre_pds  <- periods[periods < g]
      post_pds <- periods[periods >= g]
      if (length(pre_pds) && length(post_pds)) {
        est <- simple_2x2(timing_groups[[as.character(g)]],
                          timing_groups[[never_key]],
                          pre_pds, post_pds)
        if (!is.null(est)) {
          n_t <- length(timing_groups[[as.character(g)]])
          n_c <- length(timing_groups[[never_key]])
          w <- (n_t * n_c * length(post_pds) * length(pre_pds)) / T_total^2
          components[[length(components) + 1]] <- data.frame(
            group1 = g, group2 = "never_treated", estimate = est,
            weight = w, type = "treated_vs_never"
          )
        }
      }
    }
  }
  comp_df <- if (length(components)) do.call(rbind, components)
             else data.frame(group1 = numeric(), group2 = character(),
                             estimate = numeric(), weight = numeric(),
                             type = character())
  overall <- if (nrow(comp_df) > 0) {
    comp_df$weight <- comp_df$weight / sum(comp_df$weight)
    sum(comp_df$estimate * comp_df$weight)
  } else NA_real_
  list(components = comp_df, overall_estimate = overall,
       details = list(backend = "base-R"))
}


# ---------------------------------------------------------------------------
# 13. Synthetic DiD
# ---------------------------------------------------------------------------

#' Synthetic Difference-in-Differences (Arkhangelsky et al., 2021)
#'
#' Requires the \pkg{synthdid} package
#' (\code{remotes::install_github("synth-inference/synthdid")}); the
#' algorithm has no comparably-faithful base-R port shipped here.
#'
#' @param data Balanced panel.
#' @param outcome,unit,time,treatment_time Column names.
#' @param treated_units Optional explicit list of treated unit IDs.
#' @param zeta Optional regularisation parameter (auto-selected if NULL).
#' @param n_bootstrap Bootstrap replications for placebo SE.
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
  if (!.morie_did_have_synthdid())
    stop("morie_did_synthetic requires the 'synthdid' package: ",
         "remotes::install_github('synth-inference/synthdid')",
         call. = FALSE)
  df <- data
  df[["_g"]] <- as.numeric(df[[treatment_time]])
  if (is.null(treated_units))
    treated_units <- unique(df[is.finite(df[["_g"]]), unit, drop = TRUE])
  treat_onset <- df[df[[unit]] %in% treated_units, "_g", drop = TRUE]
  if (!length(treat_onset))
    stop("No treated units found.", call. = FALSE)
  first_treat <- min(treat_onset, na.rm = TRUE)
  # Build the synthdid panel matrix (units x times) with treated rows last.
  units_all <- unique(df[[unit]])
  control_units <- setdiff(units_all, treated_units)
  ordered_units <- c(control_units, treated_units)
  setup <- synthdid::panel.matrices(
    df[, c(unit, time, outcome,
           setNames(list(as.numeric(df[[time]] >= first_treat &
                                      df[[unit]] %in% treated_units)),
                    "_W")[[1]]) %||% c(unit, time, outcome),
       drop = FALSE]
  )
  # Fallback: build manually if panel.matrices unhappy.
  if (is.null(setup) || !is.list(setup)) {
    df[["_W"]] <- as.numeric(df[[unit]] %in% treated_units &
                               df[[time]] >= first_treat)
    setup <- synthdid::panel.matrices(
      df[, c(unit, time, outcome, "_W")],
      unit = 1, time = 2, outcome = 3, treatment = 4
    )
  }
  est <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0,
                                      zeta = zeta)
  tau <- as.numeric(est)
  # Use S3-dispatched stats::vcov(); synthdid::vcov.synthdid_estimate
  # is unexported in CRAN builds and direct namespace access fails.
  # (audit a2a39fe4)
  se_est <- tryCatch(sqrt(stats::vcov(est, method = "placebo")),
                     error = function(e) NA_real_)
  ci <- if (is.finite(se_est)) .morie_did_make_ci(tau, se_est, alpha)
        else c(NA_real_, NA_real_)
  list(
    estimate = tau, std_error = se_est,
    t_stat   = if (is.finite(se_est) && se_est > 0) tau / se_est else NA_real_,
    p_value  = if (is.finite(se_est) && se_est > 0)
                 .morie_did_pvalue(tau / se_est) else NA_real_,
    ci_lower = ci[1], ci_upper = ci[2],
    n_treated = length(treated_units),
    n_control = length(control_units),
    method = "synthetic_did (synthdid)",
    details = list(fit = est, weights = attr(est, "weights"))
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a


# ---------------------------------------------------------------------------
# 14. Wild cluster bootstrap
# ---------------------------------------------------------------------------

#' DiD with wild cluster bootstrap p-values (Cameron-Gelbach-Miller, 2008)
#'
#' Recommended when the number of clusters is small (< 50). Uses a
#' base-R Rademacher / Webb wild-cluster-bootstrap implementation
#' that mirrors the Python module. Earlier morie versions also
#' delegated to \code{fwildclusterboot::boottest} when installed; we
#' dropped that branch because fwildclusterboot is GitHub-only and
#' transitively requires summclust, also GitHub-only, which made the
#' CI dependency resolver unreliable.
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
  # Base-R Rademacher / Webb wild-cluster bootstrap ---------------------------
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
# 16. Fuzzy DiD
# ---------------------------------------------------------------------------

#' Fuzzy DiD (LATE) via 2SLS
#'
#' Uses \eqn{Z \times \mathrm{Post}}{Z x Post} as an instrument for
#' \eqn{D \times \mathrm{Post}}{D x Post} to recover a local average treatment
#' effect under imperfect compliance.
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
    df_test[["_placebo_post"]] <- as.integer(df_test[[time]] >= pt)
    if (length(unique(df_test[["_placebo_post"]])) < 2) next
    res <- morie_did_2x2(df_test, outcome, treatment, "_placebo_post",
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
    df[["_mod_group"]] <- as.integer(cut(m, breaks = unique(breaks),
                                         include.lowest = TRUE))
  } else {
    df[["_mod_group"]] <- m
  }
  rows <- list()
  for (g_val in sort(unique(df[["_mod_group"]]))) {
    if (is.na(g_val)) next
    grp <- df[df[["_mod_group"]] %in% g_val, , drop = FALSE]
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
# 19. de Chaisemartin & D'Haultfoeuille (2020)
# ---------------------------------------------------------------------------

#' Heterogeneity-robust DiD (de Chaisemartin & D'Haultfoeuille, 2020)
#'
#' Computes the instantaneous treatment effect for switchers using
#' appropriate comparisons.
#'
#' @param data Panel data.
#' @param outcome,treatment,unit,time Column names.
#' @param n_bootstrap Bootstrap replications.
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
  df <- data[order(data[[unit]], data[[time]]), , drop = FALSE]
  periods <- sort(unique(df[[time]]))
  estimates <- c()
  weights <- c()
  for (t_idx in seq.int(2, length(periods))) {
    t_cur <- periods[t_idx]
    t_prev <- periods[t_idx - 1]
    df_cur  <- df[df[[time]] == t_cur, ]
    rownames(df_cur)  <- df_cur[[unit]]
    df_prev <- df[df[[time]] == t_prev, ]
    rownames(df_prev) <- df_prev[[unit]]
    common <- intersect(df_cur[[unit]], df_prev[[unit]])
    if (!length(common)) next
    d_cur  <- as.numeric(df_cur[as.character(common), treatment])
    d_prev <- as.numeric(df_prev[as.character(common), treatment])
    y_cur  <- as.numeric(df_cur[as.character(common), outcome])
    y_prev <- as.numeric(df_prev[as.character(common), outcome])
    switchers <- d_cur == 1 & d_prev == 0
    controls  <- d_cur == 0 & d_prev == 0
    n_sw <- sum(switchers)
    n_ct <- sum(controls)
    if (!n_sw || !n_ct) next
    delta_sw <- mean(y_cur[switchers] - y_prev[switchers])
    delta_ct <- mean(y_cur[controls]  - y_prev[controls])
    estimates <- c(estimates, delta_sw - delta_ct)
    weights <- c(weights, n_sw)
  }
  if (!length(estimates))
    return(.morie_did_result(NA_real_, NA_real_, 0, 0,
                             method = "chaisemartin_dhaultfoeuille",
                             alpha = alpha))
  w <- weights / sum(weights)
  delta_hat <- sum(w * estimates)
  units_all <- unique(df[[unit]])
  rng <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (!is.null(rng)) assign(".Random.seed", rng, envir = .GlobalEnv)
  })
  boot <- replicate(n_bootstrap, {
    b_units <- sample(units_all, length(units_all), replace = TRUE)
    df_b_parts <- lapply(seq_along(b_units), function(j) {
      sub <- df[df[[unit]] == b_units[j], , drop = FALSE]
      sub[[unit]] <- paste0(sub[[unit]], "_", j)
      sub
    })
    df_b <- do.call(rbind, df_b_parts)
    r <- tryCatch(morie_did_chaisemartin_dhaultfoeuille(
      df_b, outcome, treatment, unit, time,
      n_bootstrap = 0L, seed = seed, alpha = alpha
    ), error = function(e) NULL)
    if (is.null(r)) NA_real_ else r$estimate
  })
  boot <- boot[is.finite(boot)]
  se_est <- if (n_bootstrap > 0 && length(boot) > 1) stats::sd(boot)
            else NA_real_
  .morie_did_result(
    delta_hat, se_est,
    n_treated = sum(weights),
    n_control = length(units_all) - sum(weights),
    method = "chaisemartin_dhaultfoeuille", alpha = alpha
  )
}


# ---------------------------------------------------------------------------
# 20. Sensitivity analysis (Rambachan & Roth, 2023)
# ---------------------------------------------------------------------------

#' Sensitivity of DiD estimate to parallel-trends violations
#'
#' For each \eqn{\delta}{delta}, computes a bias-adjusted confidence set under
#' the bound \eqn{|\mathrm{bias}| \le \delta \hat\sigma}{|bias| <= delta hatsigma} (Rambachan &
#' Roth, 2023, conservative version).
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
      std    = stats::sd(y),   # py-parity name (was 'sd' pre-2026-05-22)
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
