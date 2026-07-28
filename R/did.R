# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Difference-in-Differences (DiD) estimators for rmorie.
#

#' srr regression (RE) standards
#'
#' The RE standards for rmorie's regression estimators (did / causal /
#' ipw / matching / dml / sensitivity / tox calibration / survival) are
#' recorded here in one reviewable place, with pointers to the code that
#' addresses each.
#'
#' @srrstats {RE1.0} Regression is specified either by a formula (the
#'   survival and glm-based wrappers) or, for the MRM estimators, by
#'   named `outcome` / `treatment` / column arguments; the column-name
#'   interface is a documented design choice for the panel/DiD estimators.
#' @srrstats {RE1.1} The conversion of the model specification to a
#'   design matrix is documented in each estimator (e.g. did builds
#'   `cbind(d, p, interaction, covariates)` then `model.matrix`).
#' @srrstats {RE1.2} Expected predictor formats/types are documented on
#'   each `@param`; `.morie_check_data()` also enforces them.
#' @srrstats {RE1.3} Coefficient names are retained on the returned
#'   coefficient/SE vectors.
#' @srrstats {RE1.3a} Per-observation input attributes not needed for
#'   inference (row names, arbitrary column `attributes()`) are not
#'   carried onto the compact result object; this is documented here.
#' @srrstats {RE1.4} Modelling assumptions are documented (e.g. parallel
#'   trends for DiD) and testable (`morie_did_test_parallel_trends`).
#' @srrstats {RE2.0} Input transformations (factor handling, intercept
#'   addition) are documented; `.viable_terms()` reports dropped terms.
#' @srrstats {RE2.1} Missing values are handled explicitly via
#'   complete-case selection (`.morie_did_drop_na` / `stats::complete.cases`)
#'   and the finite-value check in `.morie_check_numvec` distinguishes
#'   NA/NaN from Inf.
#' @srrstats {RE2.4} Perfectly collinear / zero-variance terms are
#'   identified and dropped before fitting.
#' @srrstats {RE2.4a} `.viable_terms()` detects predictor terms with a
#'   single observed level (perfect collinearity among predictors).
#' @srrstats {RE3.0} Iterative fitters (glm, Hawkes MLE, HMC backends)
#'   surface non-convergence via their upstream warnings.
#' @srrstats {RE3.1} Those warnings can be suppressed by the caller while
#'   the returned object still records fit status.
#' @srrstats {RE3.2} Convergence thresholds default to the well-tested
#'   upstream defaults (documented per wrapper).
#' @srrstats {RE3.3} Convergence thresholds can be set explicitly through
#'   the `...` pass-through to the upstream fitter.
#' @srrstats {RE4.0} Estimators return a structured result object
#'   (class `morie_rich_result`) modelling the fit.
#' @srrstats {RE4.2} Coefficients are returned (`details$all_coefficients`).
#' @srrstats {RE4.3} Confidence intervals on the estimate are returned
#'   (`ci_lower` / `ci_upper`).
#' @srrstats {RE4.4} The model specification/method is returned (`method`).
#' @srrstats {RE4.5} The number of observations is returned
#'   (`n_treated` / `n_control` / `details$n_obs`).
#' @srrstats {RE4.6} The variance-covariance matrix is available via
#'   `morie_causal_robust_se()`.
#' @srrstats {RE4.10} Residuals are provided for the survival models
#'   (Schoenfeld / martingale / deviance / Cox-Snell in `survival.R`).
#' @srrstats {RE4.11} Goodness-of-fit and effect-size statistics are
#'   provided (`effect_sizes.R`, R-squared helpers).
#' @srrstats {RE4.12} Forward-and-inverse transforms are provided where
#'   relevant (tox calibration curve + inverse prediction).
#' @srrstats {RE4.17} Result objects implement a default `print` method
#'   (via the `morie_rich_result` class).
#' @srrstats {RE7.0} Tests use noiseless exact predictor relationships.
#' @srrstats {RE7.0a} Tests confirm rejection of perfectly noiseless
#'   (zero-variance / collinear) predictors.
#' @srrstats {RE7.1} Tests use noiseless exact predictor-response
#'   relationships and confirm exact recovery.
#' @srrstats {RE7.3} Tests confirm the documented accessor fields of the
#'   returned model object (see `test-srr-standards-RE.R`).
#' @noRd
NULL

#
# Module 14 (feat/native-specializations, 2026-07-15): the DiD family
# is native. The engines live in R/did_native.R and reproduce the
# reference implementations to machine precision (see tests/cross/):
#
#   * TWFE + event study      -- native alternating-projection demeaning
#                                + CR1 cluster vcov (replaces fixest).
#   * Callaway-Sant'Anna      -- native ATT(g,t) on the Sant'Anna-Zhao
#                                panel estimators with IF-based
#                                inference + Mammen multiplier
#                                bootstrap (replaces did).
#   * Doubly robust DiD       -- native locally efficient drdid_rc
#                                estimand incl. influence function
#                                (replaces DRDID).
#   * Goodman-Bacon           -- native decomposition via the FWL
#                                variance-weight identity (replaces
#                                bacondecomp).
#   * DID-M                   -- native de Chaisemartin-D'Haultfoeuille
#                                switcher estimator + cluster bootstrap
#                                (replaces DIDmultiplegt).
#   * feTR weights            -- native TWFE weight diagnostic
#                                (replaces TwoWayFEWeights).
#
# Module 15 made the synthetic-DiD wrappers native too (SDID engine in
# R/synth_native.R; morie_synth_control is the Abadie SCM flagship).
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

#' Internal helper: Morie Did Have Fixest
#' @noRd
.morie_did_have_fixest         <- function() requireNamespace("fixest",         quietly = TRUE)
#' Internal helper: Morie Did Have Did
#' @noRd
.morie_did_have_did            <- function() requireNamespace("did",            quietly = TRUE)
#' Internal helper: Morie Did Have Bacondecomp
#' @noRd
.morie_did_have_bacondecomp    <- function() requireNamespace("bacondecomp",    quietly = TRUE)
#' Internal helper: Morie Did Have Coresynth
#' @noRd
.morie_did_have_coresynth      <- function() requireNamespace("coresynth",      quietly = TRUE)
#' Internal helper: Morie Did Have Sandwich
#' @noRd
.morie_did_have_sandwich       <- function() requireNamespace("sandwich",       quietly = TRUE)
#' Internal helper: Morie Did Have Drdid
#' @noRd
.morie_did_have_drdid          <- function() requireNamespace("DRDID",          quietly = TRUE)
#' Internal helper: Morie Did Have Honestdid
#' @noRd
.morie_did_have_honestdid      <- function() requireNamespace("HonestDiD",      quietly = TRUE)
#' Internal helper: Morie Did Have Didmultiplegt
#' @noRd
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
                      error = function(e) .morie_ginv(crossprod(X)))
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
#' native). \code{\link{morie_did_doubly_robust}} is the recommended
#' option when pre-treatment covariates are available.
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
#' \donttest{
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
#' @examples
#' set.seed(1)
#' n <- 400
#' d <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
#' y <- 1 + 0.3 * d + 0.4 * p + 0.5 * d * p + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, d = d, post = p,
#'                  w = runif(n, 0.5, 2))
#' res <- morie_did_repeated_cross_section(df, "y", "d", "post",
#'                                         weights = "w")
#' res$estimate
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
# 3. Panel two-way fixed-effects DiD -- native TWFE engine
# ---------------------------------------------------------------------------

#' Two-way fixed-effects DiD (panel)
#'
#' Native two-way fixed-effects estimator of
#' \eqn{Y_{it} = \alpha_i + \lambda_t + \tau D_{it} + X'\delta
#' + \varepsilon_{it}}{Y_it = alpha_i + lambda_t + tau D_it + X'delta + varepsilon_it}
#' via alternating-projection demeaning (Frisch-Waugh-Lovell) with
#' CR1 cluster-robust standard errors using the same small-sample
#' correction as \code{fixest::feols}'s default (reproduced to
#' machine precision in \code{tests/cross/}).
#'
#' @inheritParams morie_did_2x2
#' @param unit Unit identifier column.
#' @param time Time period column.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @examples
#' set.seed(2)
#' df <- expand.grid(unit = 1:60, time = 1:6)
#' df$treat_time <- ifelse(df$unit <= 30, 4, Inf)
#' df$d <- as.integer(df$time >= df$treat_time)
#' df$y <- 0.5 * df$time + 1.5 * df$d + rnorm(nrow(df), sd = 0.5)
#' res <- morie_did_panel_fe(df, "y", "d", "unit", "time")
#' res$estimate
#' @export
morie_did_panel_fe <- function(data, outcome, treatment, unit, time,
                               covariates = NULL, cluster = NULL,
                               alpha = 0.05) {
  df <- .morie_did_drop_na(data, c(outcome, treatment, unit, time))
  X <- cbind(as.numeric(df[[treatment]]))
  colnames(X) <- treatment
  if (length(covariates)) {
    Xc <- as.matrix(df[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    X <- cbind(X, Xc)
  }
  cluster_var <- if (!is.null(cluster)) cluster else unit
  fit <- .morie_did_twfe_native(as.numeric(df[[outcome]]), X,
                                df[[unit]], df[[time]],
                                df[[cluster_var]])
  est    <- fit$beta[[treatment]]
  se_est <- fit$se[[treatment]]
  .morie_did_result(
    est, se_est,
    n_treated = sum(as.numeric(df[[treatment]]) == 1),
    n_control = sum(as.numeric(df[[treatment]]) == 0),
    method = "did_panel_fe (rmorie native)", alpha = alpha,
    details = list(fit = fit,
                   n_units   = fit$n_units,
                   n_periods = fit$n_periods)
  )
}


# ---------------------------------------------------------------------------
# 4. Event study -- native TWFE engine on relative-time dummies
# ---------------------------------------------------------------------------

#' Event-study DiD specification
#'
#' Native event-study estimator: relative-time dummies (with
#' \code{reference_period} dropped as the baseline) on unit and time
#' fixed effects, fitted by the same native TWFE engine as
#' \code{\link{morie_did_panel_fe}}. Reproduces
#' \code{fixest::feols} + \code{fixest::i()} to machine precision
#' (see \code{tests/cross/}).
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
#' @examples
#' set.seed(1)
#' df <- expand.grid(unit = 1:30, time = 1:6)
#' df$treat_time <- ifelse(df$unit <= 15, 4, Inf)
#' df$d <- as.integer(df$time >= df$treat_time)
#' df$y <- 0.1 * df$time + 0.7 * df$d + rnorm(nrow(df), sd = 0.4)
#' res <- morie_did_event_study(df, "y", "unit", "time", "treat_time",
#'                              leads = 2L, lags = 2L)
#' res$coefficients
#' @export
morie_did_event_study <- function(data, outcome, unit, time, treatment_time,
                                  covariates = NULL, reference_period = -1L,
                                  leads = 4L, lags = 4L,
                                  cluster = NULL, alpha = 0.05) {
  df <- data
  g_num <- as.numeric(df[[treatment_time]])
  # Never-treated units: Inf, NA, or 0 (the Callaway-Sant'Anna coding).
  g_num[g_num == 0] <- Inf
  rel_time <- as.numeric(df[[time]]) - g_num
  # Truncate to [-leads, lags] so dummies outside the window are absorbed.
  rel_time_trunc <- pmin(pmax(rel_time, -leads), lags)
  rel_time_trunc[!is.finite(rel_time_trunc)] <- reference_period
  df[["morie_rel_time"]] <- rel_time_trunc
  cluster_var <- if (!is.null(cluster)) cluster else unit
  # Relative-time dummies, reference period dropped
  rel_levels <- sort(unique(rel_time_trunc))
  rel_levels <- rel_levels[rel_levels != reference_period]
  X <- vapply(rel_levels,
              function(k) as.numeric(rel_time_trunc == k),
              numeric(nrow(df)))
  colnames(X) <- paste0("rel::", rel_levels)
  if (length(covariates)) {
    Xc <- as.matrix(df[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    X <- cbind(X, Xc)
  }
  fit <- .morie_did_twfe_native(as.numeric(df[[outcome]]), X,
                                df[[unit]], df[[time]],
                                df[[cluster_var]])
  keep <- grepl("^rel::", names(fit$beta)) & !is.na(fit$beta)
  rel_k <- as.integer(sub("^rel::", "", names(fit$beta)[keep]))
  est_k <- fit$beta[keep]
  se_k  <- fit$se[keep]
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
    details           = list(fit = fit, backend = "rmorie native")
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
#' @examples
#' set.seed(1)
#' df <- expand.grid(unit = 1:30, time = 1:6)
#' df$treat <- as.integer(df$unit <= 15)
#' df$d <- as.integer(df$treat == 1L & df$time >= 4)
#' df$y <- 0.1 * df$time + 0.7 * df$d + rnorm(nrow(df), sd = 0.4)
#' res <- morie_did_test_parallel_trends(df, "y", "treat", "time",
#'                                       pre_periods = c(1, 2, 3))
#' res$parallel_trends_plausible
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
#' @examples
#' set.seed(1)
#' df <- expand.grid(unit = 1:30, time = 1:6)
#' df$treat <- as.integer(df$unit <= 15)
#' df$d <- as.integer(df$treat == 1L & df$time >= 4)
#' df$y <- 0.1 * df$time + 0.7 * df$d + rnorm(nrow(df), sd = 0.4)
#' out <- morie_did_parallel_trends_data(df, "y", "treat", "time")
#' head(out)
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
# 7. Callaway-Sant'Anna group-time ATTs -- native engine
# ---------------------------------------------------------------------------

#' Callaway--Sant'Anna group-time average treatment effects
#'
#' Native Callaway-Sant'Anna (2021) estimator. For each cohort
#' \eqn{g} and each period \code{t}, estimates
#' \eqn{\mathrm{ATT}(g, t)}{ATT(g, t)} with the Sant'Anna-Zhao (2020)
#' panel estimators (doubly robust, IPW, or outcome regression) on the
#' two-period comparison against the last pre-treatment base period
#' ("varying" base period, matching \code{did::att_gt}'s default).
#' Inference uses the analytic influence functions with a Mammen
#' multiplier bootstrap. Point estimates and influence-function
#' standard errors reproduce \code{did::att_gt} to machine precision
#' (see \code{tests/cross/}).
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
#' @param n_bootstrap Number of multiplier-bootstrap replications for
#'   inference (0 = analytic influence-function standard errors).
#' @param seed RNG seed for the multiplier bootstrap.
#' @param se_convention For analytic (non-bootstrap) standard errors:
#'   \code{"reference"} (default) uses the \pkg{did}/\pkg{DRDID}
#'   population-sd convention so results reproduce the reference
#'   packages exactly; \code{"bessel"} keeps Bessel's correction
#'   (\code{sd(IF)/sqrt(n)}). Asymptotically equivalent.
#' @param alpha Significance level.
#' @return A data frame with columns \code{cohort}, \code{time},
#'   \code{att}, \code{std_error}, \code{ci_lower}, \code{ci_upper},
#'   \code{p_value}.
#' @references Callaway, B., & Sant'Anna, P. H. C. (2021).
#'   Difference-in-Differences with multiple time periods.
#'   \emph{Journal of Econometrics}, 225(2), 200--230.
#' @examples
#' set.seed(4)
#' df <- expand.grid(unit = 1:50, time = 1:6)
#' df$treat_time <- ifelse(df$unit <= 25, 4, Inf)
#' df$d <- as.integer(df$time >= df$treat_time)
#' df$y <- 0.1 * df$time + 0.6 * df$d + rnorm(nrow(df), sd = 0.4)
#' out <- morie_did_group_time_att(df, "y", "unit", "time", "treat_time",
#'                                 n_bootstrap = 30L, seed = 4)
#' head(out)
#' @export
morie_did_group_time_att <- function(data, outcome, unit, time, treatment_time,
                                     covariates = NULL,
                                     method = "doubly_robust",
                                     control_group = "never_treated",
                                     n_bootstrap = 200L, seed = 42L,
                                     alpha = 0.05,
                                     se_convention = "reference") {
  df <- data
  # The engine codes never-treated as 0, not Inf.
  g_col <- as.numeric(df[[treatment_time]])
  g_col[!is.finite(g_col)] <- 0
  df[["morie_gname"]] <- g_col
  method_map <- c(doubly_robust = "dr",
                  ipw = "ipw",
                  outcome_regression = "reg")
  est_method <- if (method %in% names(method_map))
    method_map[[method]]
  else "dr"
  cg <- switch(control_group,
               never_treated = "nevertreated",
               not_yet_treated = "notyettreated",
               control_group)
  fit <- .morie_attgt_native(df, outcome, unit, time, "morie_gname",
                             covariates = covariates,
                             est_method = est_method,
                             control_group = cg,
                             biters = n_bootstrap, seed = seed,
                             alpha = alpha,
                             se_convention = se_convention)
  r <- fit$results
  z <- stats::qnorm(1 - alpha / 2)
  out <- data.frame(
    cohort    = r$group,
    time      = r$t,
    att       = r$att,
    std_error = r$se,
    ci_lower  = r$att - z * r$se,
    ci_upper  = r$att + z * r$se,
    p_value   = 2 * stats::pnorm(-abs(r$att / r$se)),
    n_treated = r$n_treated,
    post      = r$t >= r$group
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
#' Mirrors the canonical aggregation schemes (overall ATT, by-cohort,
#' by-calendar-time, by-event-time) and produces a tidy
#' \code{data.frame} consumed by the rmorie / MRM downstream
#' pipelines.
#'
#' @param gt_results Output of \code{\link{morie_did_group_time_att}}.
#' @param aggregation One of \code{"overall"} (default), \code{"cohort"},
#'   \code{"calendar_time"}, \code{"event_time"}.
#' @param time_col,cohort_col,att_col,se_col Column-name overrides.
#' @return A data frame with \code{group}, \code{estimate},
#'   \code{std_error}, \code{ci_lower}, \code{ci_upper}.
#' @examples
#' gt <- data.frame(cohort = c(2, 2, 3, 3),
#'                  time   = c(2, 3, 3, 4),
#'                  att    = c(1.0, 1.5, 2.0, 2.5),
#'                  std_error = c(0.4, 0.4, 0.3, 0.3))
#' out <- morie_did_aggregate_gt_att(gt, aggregation = "overall")
#' out$estimate
#' @export
morie_did_aggregate_gt_att <- function(gt_results,
                                       aggregation = "overall",
                                       time_col = "time",
                                       cohort_col = "cohort",
                                       att_col = "att",
                                       se_col = "std_error") {
  df <- gt_results
  df[["morie_rel_time"]] <- df[[time_col]] - df[[cohort_col]]
  # Cells with t < g are PRE-treatment. They are the parallel-trends
  # check, not effects, and averaging them into a summary drags it
  # toward zero -- so every aggregation except the event study, whose
  # whole point is to show the pre-periods separately, uses post cells
  # only (Callaway & Sant'Anna 2021, section 3).
  post <- df[["morie_rel_time"]] >= 0
  # Weight by cohort size where the estimator recorded it, so the
  # summary is the sample-weighted ATT rather than an unweighted mean
  # over cells, which would let a cohort of one count as much as a
  # cohort of a thousand.
  wts <- if ("n_treated" %in% names(df)) as.numeric(df[["n_treated"]]) else
    rep(1, nrow(df))
  agg_one <- function(idx, label) {
    if (!length(idx)) {
      return(data.frame(group = label, estimate = NA_real_,
                        std_error = NA_real_, ci_lower = NA_real_,
                        ci_upper = NA_real_))
    }
    w <- wts[idx] / sum(wts[idx])
    est <- sum(w * df[[att_col]][idx])
    # SE of a weighted average of k estimates, treating them as
    # independent: sqrt(sum(w_i^2 se_i^2)).
    se <- sqrt(sum(w^2 * df[[se_col]][idx]^2))
    ci <- .morie_did_make_ci(est, se)
    data.frame(group = label, estimate = est, std_error = se,
               ci_lower = ci[1], ci_upper = ci[2])
  }
  if (identical(aggregation, "overall")) {
    return(agg_one(which(post), "overall"))
  }
  group_col <- switch(aggregation,
                      cohort        = cohort_col,
                      calendar_time = time_col,
                      event_time    = "morie_rel_time",
                      stop("Unknown aggregation: ", aggregation))
  # the event study reports every relative period, pre ones included
  use <- if (identical(aggregation, "event_time")) rep(TRUE, nrow(df)) else post
  keys <- sort(unique(df[[group_col]][use]))
  rows <- lapply(keys, function(k) {
    agg_one(which(use & df[[group_col]] == k), k)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
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
#' @examples
#' set.seed(2)
#' df <- expand.grid(unit = 1:40, time = 1:6)
#' df$treat_time <- ifelse(df$unit <= 20, 4, Inf)
#' df$d <- as.integer(df$time >= df$treat_time)
#' df$y <- 0.1 * df$time + 0.6 * df$d + rnorm(nrow(df), sd = 0.4)
#' out <- morie_did_staggered(df, "y", "unit", "time", "treat_time",
#'                            n_bootstrap = 50L, seed = 2)
#' str(out, max.level = 1)
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
# 10. Doubly-robust DiD -- native Sant'Anna-Zhao engine
# ---------------------------------------------------------------------------

#' Doubly-robust DiD (Sant'Anna & Zhao, 2020)
#'
#' Native locally efficient doubly robust DiD estimator for the 2x2
#' repeated-cross-section setting (the estimand of
#' \code{DRDID::drdid_rc}, reproduced to machine precision incl. the
#' influence function; see \code{tests/cross/}). Combines a logistic
#' propensity-score model with linear outcome regressions and is
#' consistent if either model is correctly specified.
#'
#' @inheritParams morie_did_2x2
#' @param ps_model Unused; retained for back-compat. A logistic
#'   propensity-score model is fitted internally.
#' @param or_model Unused; retained for back-compat. Linear outcome
#'   models are fitted internally.
#' @param n_bootstrap Number of multiplier-bootstrap replications for
#'   the standard error (0 = analytic influence-function SE).
#' @param seed RNG seed for the multiplier bootstrap.
#' @param se_convention For the analytic SE: \code{"reference"}
#'   (default) matches \code{DRDID::drdid_rc}'s population-sd
#'   convention exactly; \code{"bessel"} keeps Bessel's correction
#'   (\code{sd(IF)/sqrt(n)}). Asymptotically equivalent.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @references Sant'Anna, P. H. C., & Zhao, J. (2020). Doubly robust
#'   difference-in-differences estimators. \emph{Journal of
#'   Econometrics}, 219(1), 101--122.
#' @examples
#' set.seed(23)
#' n <- 300
#' treat <- rep(c(0L, 1L), each = n / 2)
#' df <- data.frame(unit = rep(1:n, 2), treat = rep(treat, 2),
#'                  post = rep(c(0L, 1L), each = n), x = rnorm(2 * n))
#' df$y <- 0.5 * df$post + 3 * df$treat * df$post + rnorm(2 * n, sd = 0.4)
#' res <- morie_did_doubly_robust(df, outcome = "y", treatment = "treat",
#'                                post = "post", covariates = "x",
#'                                n_bootstrap = 50L, seed = 1L)
#' res$estimate
#' @export
morie_did_doubly_robust <- function(data, outcome, treatment, post,
                                    covariates,
                                    ps_model = "logistic",
                                    or_model = "linear",
                                    cluster = NULL,
                                    n_bootstrap = 200L, seed = 42L,
                                    alpha = 0.05,
                                    se_convention = "reference") {
  rng <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (!is.null(rng)) assign(".Random.seed", rng, envir = .GlobalEnv)
  })
  df <- .morie_did_drop_na(data, c(outcome, treatment, post, covariates))
  y  <- as.numeric(df[[outcome]])
  d  <- as.numeric(df[[treatment]])
  p  <- as.numeric(df[[post]])
  covariates_mat <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(covariates_mat) <- "double"
  X <- cbind(`(Intercept)` = 1, covariates_mat)
  fit <- .morie_drdid_rc_native(y, p, d, X)
  est    <- fit$att
  se_est <- .morie_did_if_se(fit$IF, se_convention)
  if (n_bootstrap > 0L) {
    mb <- .morie_did_mboot(matrix(fit$IF, ncol = 1L),
                           biters = n_bootstrap, seed = seed)
    if (is.finite(mb$se) && mb$se > 0) se_est <- mb$se
  }
  .morie_did_result(
    est, se_est,
    n_treated = sum(d == 1), n_control = sum(d == 0),
    method = "did_doubly_robust (rmorie native)", alpha = alpha,
    details = list(fit = fit[c("att", "se")],
                   n_bootstrap = n_bootstrap,
                   backend = "rmorie native")
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
#' @examples
#' set.seed(1)
#' n <- 400
#' d <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
#' s <- rbinom(n, 1, 0.5)
#' y <- 0.2 * d + 0.3 * p + 0.4 * s + 0.5 * d * p * s + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, d = d, post = p, group = s)
#' res <- morie_did_triple_difference(df, "y", "d", "post", "group")
#' res$estimate
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
# 12. Goodman-Bacon decomposition -- native engine
# ---------------------------------------------------------------------------

#' Goodman-Bacon decomposition of the TWFE DiD estimator
#'
#' Native Goodman-Bacon (2021) decomposition: the two-way
#' fixed-effects DiD estimate is decomposed into a weighted average of
#' all 2x2 timing comparisons, with each pair's weight derived from
#' the Frisch-Waugh-Lovell identity (subsample size squared times the
#' variance of the demeaned treatment). Reproduces
#' \code{bacondecomp::bacon} exactly (see \code{tests/cross/}).
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
#' @examples
#' set.seed(1)
#' df <- expand.grid(unit = 1:30, time = 1:6)
#' df$treat_time <- ifelse(df$unit <= 15, 4, Inf)
#' df$d <- as.integer(df$time >= df$treat_time)
#' df$y <- 0.1 * df$time + 0.7 * df$d + rnorm(nrow(df), sd = 0.4)
#' out <- morie_did_bacon_decomposition(df, "y", "d", "unit", "time")
#' out$overall_estimate
#' @export
morie_did_bacon_decomposition <- function(data, outcome, treatment,
                                          unit, time) {
  comp <- .morie_bacon_native(data, outcome, treatment, unit, time)
  overall <- sum(comp$estimate * comp$weight)
  list(components = comp, overall_estimate = overall,
       details = list(backend = "rmorie native"))
}


# ---------------------------------------------------------------------------
# 13. Synthetic DiD -- native SDID engine
# ---------------------------------------------------------------------------

#' Synthetic Difference-in-Differences (Arkhangelsky et al., 2021)
#'
#' Native synthetic difference-in-differences estimator (the
#' Arkhangelsky et al. 2021 algorithm: ridge-regularized unit weights,
#' simplex time weights, weighted DiD) with bootstrap inference. The
#' engine lives in \code{R/synth_native.R}; see also
#' \code{\link{morie_synth_control}} for the classic Abadie synthetic
#' control with placebo inference.
#'
#' @param data Balanced panel.
#' @param outcome,unit,time,treatment_time Column names.
#' @param treated_units Optional explicit list of treated unit IDs.
#' @param zeta Retained for back-compat; ignored (the engine derives
#'   the SDID regularisation from the data).
#' @param n_bootstrap Bootstrap replications for the SE / CI.
#' @param seed RNG seed.
#' @param alpha Significance level.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @references Arkhangelsky, D., et al. (2021). Synthetic
#'   difference-in-differences. \emph{American Economic Review},
#'   111(12), 4088--4118.
#' @examples
#' set.seed(3)
#' df <- expand.grid(unit = 1:30, time = 1:8)
#' df$treat_time <- ifelse(df$unit <= 5, 6, Inf)
#' df$d <- as.integer(df$time >= df$treat_time)
#' df$y <- 0.1 * df$time + 0.5 * df$d + rnorm(nrow(df), sd = 0.3)
#' out <- morie_did_synthetic(df, "y", "unit", "time", "treat_time",
#'                            n_bootstrap = 50L, seed = 3)
#' str(out, max.level = 1)
#' @export
morie_did_synthetic <- function(data, outcome, unit, time, treatment_time,
                                treated_units = NULL, zeta = NULL,
                                n_bootstrap = 200L, seed = 42L,
                                alpha = 0.05) {
  df <- as.data.frame(data)
  df[["morie_g"]] <- as.numeric(df[[treatment_time]])
  if (is.null(treated_units))
    treated_units <- unique(df[is.finite(df[["morie_g"]]) &
                                 df[["morie_g"]] > 0, unit, drop = TRUE])
  treat_onset <- df[df[[unit]] %in% treated_units, "morie_g", drop = TRUE]
  if (!length(treat_onset))
    stop("No treated units found.", call. = FALSE)
  first_treat <- min(treat_onset, na.rm = TRUE)
  units_all <- unique(df[[unit]])
  control_units <- setdiff(units_all, treated_units)
  df[["morie_W"]] <- as.integer(df[[unit]] %in% treated_units &
                                  df[[time]] >= first_treat)
  prep <- .morie_sdid_prepare(df, outcome, unit, time, "morie_W")
  # zeta is retained for back-compat but ignored: the engine derives
  # the SDID regularisation from the data (Arkhangelsky et al. 2021).
  fit <- .morie_sdid_inference(prep$Y, prep$N_co, prep$T_pre,
                               method = "bootstrap",
                               n_boot = n_bootstrap, seed = seed)
  tau <- fit$estimate
  se_est <- fit$se
  ci <- if (is.finite(se_est)) .morie_did_make_ci(tau, se_est, alpha)
        else c(NA_real_, NA_real_)
  pval <- if (is.finite(se_est) && se_est > 0)
    .morie_did_pvalue(tau / se_est) else NA_real_
  list(
    estimate = tau, std_error = se_est,
    t_stat   = if (is.finite(se_est) && se_est > 0) tau / se_est else NA_real_,
    p_value  = pval,
    ci_lower = ci[1], ci_upper = ci[2],
    n_treated = length(treated_units),
    n_control = length(control_units),
    method = "synthetic_did (rmorie native)",
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
#' @examples
#' set.seed(1)
#' n <- 300
#' d <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
#' y <- 1 + 0.3 * d + 0.4 * p + 0.5 * d * p + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, d = d, post = p,
#'                  clust = sample.int(20, n, replace = TRUE))
#' res <- morie_did_wild_cluster_bootstrap(df, "y", "d", "post",
#'                                         cluster = "clust",
#'                                         n_bootstrap = 99L, seed = 7)
#' c(res$estimate, res$p_value)
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
#' @examples
#' set.seed(1)
#' n <- 300
#' dose <- runif(n, 0, 3)
#' p <- rbinom(n, 1, 0.5)
#' y <- 0.4 * dose * p + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, dose = dose, post = p)
#' res <- morie_did_continuous_treatment(df, "y", "dose", "post")
#' res$estimate
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
#' For the de Chaisemartin-D'Haultfoeuille estimator on panel data
#' prefer \code{\link{morie_did_chaisemartin_dhaultfoeuille}} (rmorie
#' native).
#'
#' @inheritParams morie_did_2x2
#' @param assignment Intent-to-treat assignment column.
#' @param takeup Actual treatment-takeup column.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @examples
#' set.seed(1)
#' n <- 400
#' z <- rbinom(n, 1, 0.5)
#' d <- as.integer(z & rbinom(n, 1, 0.8))
#' p <- rbinom(n, 1, 0.5)
#' y <- 0.5 * d * p + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, z = z, d = d, post = p)
#' res <- morie_did_fuzzy(df, "y", "z", "d", "post")
#' c(res$estimate, res$details$first_stage_f)
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
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y = rnorm(500), d = rbinom(500, 1, 0.5),
#'   time = sample(1:8, 500, replace = TRUE)
#' )
#' out <- morie_did_placebo_test_time(df, "y", "d", "time",
#'                                    true_treatment_time = 7,
#'                                    placebo_times = c(3, 4, 5))
#' out
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
#' @examples
#' set.seed(1)
#' n <- 400
#' d <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
#' df <- data.frame(d = d, post = p,
#'                  y_pl1 = rnorm(n), y_pl2 = rnorm(n))
#' out <- morie_did_placebo_test_outcome(df, c("y_pl1", "y_pl2"),
#'                                       "d", "post")
#' out
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
#' @examples
#' set.seed(1)
#' n <- 400
#' d <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
#' y <- 1 + 0.3 * d + 0.4 * p + 0.5 * d * p + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, d = d, post = p,
#'                  grp = sample(c("A", "B"), n, replace = TRUE))
#' out <- morie_did_placebo_test_group(df, "y", "d", "post",
#'                                     group_col = "grp",
#'                                     unaffected_groups = c("A", "B"))
#' out
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
#' @examples
#' set.seed(1)
#' n <- 600
#' d <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
#' y <- 1 + 0.3 * d + 0.4 * p + 0.5 * d * p + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, d = d, post = p, mod = rnorm(n))
#' out <- morie_did_heterogeneous(df, "y", "d", "post",
#'                                moderator = "mod", n_quantiles = 3L)
#' out
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
# 19. de Chaisemartin & D'Haultfoeuille -- native DID-M engine
# ---------------------------------------------------------------------------

#' Heterogeneity-robust DiD (de Chaisemartin & D'Haultfoeuille, 2020)
#'
#' Native DID-M estimator: the instantaneous treatment effect for
#' switchers. For each pair of consecutive periods, joiners (0 to 1)
#' are compared with groups stable at 0 and leavers (1 to 0) with
#' groups stable at 1, and the per-period DiDs are averaged with
#' switcher-count weights. Standard errors come from a cluster (group)
#' bootstrap.
#'
#' @param data Panel data.
#' @param outcome,treatment,unit,time Column names.
#' @param n_bootstrap Cluster-bootstrap replications for the standard
#'   error (0 = point estimate only, SE is \code{NA}).
#' @param seed RNG seed.
#' @param alpha Significance level.
#' @return A result list; see \code{\link{morie_did_2x2}}.
#' @references de Chaisemartin, C., & D'Haultfoeuille, X. (2020). Two-way
#'   fixed effects estimators with heterogeneous treatment effects.
#'   \emph{American Economic Review}, 110(9), 2964--2996.
#' @examples
#' set.seed(9)
#' df <- expand.grid(unit = 1:60, time = 1:6)
#' df$treat_time <- ifelse(df$unit <= 30, 4, Inf)
#' df$d <- as.integer(df$time >= df$treat_time)
#' df$y <- 0.5 * df$time + 1.5 * df$d + rnorm(nrow(df), sd = 0.5)
#' res <- morie_did_chaisemartin_dhaultfoeuille(df, "y", "d", "unit", "time",
#'                                              n_bootstrap = 50L, seed = 9)
#' res$estimate
#' @export
morie_did_chaisemartin_dhaultfoeuille <- function(data, outcome, treatment,
                                                  unit, time,
                                                  n_bootstrap = 200L,
                                                  seed = 42L, alpha = 0.05) {
  fit <- .morie_didm_native(as.data.frame(data), outcome, treatment,
                            unit, time,
                            n_bootstrap = n_bootstrap, seed = seed)
  est <- as.numeric(fit$effect)
  se_est <- as.numeric(fit$se_effect)
  units_all <- unique(data[[unit]])
  treated_units <- unique(data[data[[treatment]] == 1, unit, drop = TRUE])
  res <- .morie_did_result(
    est, se_est,
    n_treated = length(treated_units),
    n_control = length(setdiff(units_all, treated_units)),
    method = "chaisemartin_dhaultfoeuille", alpha = alpha,
    details = list(fit = fit, backend = "rmorie native")
  )
  res$method <- "chaisemartin_dhaultfoeuille"
  res
}


# ---------------------------------------------------------------------------
# 20. Sensitivity analysis (base-R bias-bound confidence sets)
# ---------------------------------------------------------------------------

#' Sensitivity of DiD estimate to parallel-trends violations
#'
#' For each \eqn{\delta}{delta}, computes a bias-adjusted confidence
#' set under the bound
#' \eqn{|\mathrm{bias}| \le \delta \hat\sigma}{|bias| <= delta hatsigma}
#' (Rambachan & Roth, 2023, conservative version).
#'
#' For a relative-magnitudes bound anchored on observed event-study
#' pre-trends (Rambachan & Roth's \eqn{\bar M}{M-bar}
#' parameterization, conservative version) see
#' \code{\link{morie_did_honest_sensitivity}}.
#'
#' @inheritParams morie_did_2x2
#' @param delta_range Numeric vector of \eqn{\delta}{delta} values to evaluate
#'   (default \code{seq(0, 2, 0.25)}).
#' @return A data frame with columns \code{delta}, \code{ci_lower},
#'   \code{ci_upper}, \code{covers_zero}.
#' @references Rambachan, A., & Roth, J. (2023). A more credible approach
#'   to parallel trends. \emph{Review of Economic Studies}, 90(5),
#'   2555--2591.
#' @examples
#' set.seed(7)
#' n <- 300
#' d <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
#' y <- 1 + 0.3 * d + 0.4 * p + 0.5 * d * p + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, d = d, post = p)
#' out <- morie_did_sensitivity_analysis(df, "y", "d", "post")
#' str(out, max.level = 1)
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


#' Honest (relative-magnitudes) sensitivity for event-study estimates
#'
#' Conservative Rambachan-Roth (2023) relative-magnitudes bounds on an
#' event-study coefficient: the post-treatment bias from a
#' parallel-trends violation is bounded by
#' \eqn{\bar M}{M-bar} times the largest observed pre-treatment
#' deviation, and the confidence interval is widened by that bound.
#' \eqn{\bar M = 0}{M-bar = 0} reproduces the conventional CI;
#' \eqn{\bar M = 1}{M-bar = 1} allows post-treatment violations as
#' large as the worst pre-trend. This is the conservative (fixed-bias)
#' version of the relative-magnitudes parameterization, anchored on
#' the estimated pre-period coefficients.
#'
#' @param event_study The result of \code{\link{morie_did_event_study}}
#'   (or any list with a \code{coefficients} data frame containing
#'   \code{relative_time}, \code{estimate}, \code{std_error}).
#' @param m_bar_range Numeric vector of \eqn{\bar M}{M-bar} values
#'   (default \code{seq(0, 2, 0.5)}).
#' @param target_time Relative time of the post-treatment coefficient
#'   to bound (default \code{0}, the onset period).
#' @param alpha Significance level.
#' @return A data frame with columns \code{m_bar}, \code{estimate},
#'   \code{ci_lower}, \code{ci_upper}, \code{covers_zero}, plus a
#'   \code{breakdown_m_bar} attribute (the smallest evaluated
#'   \eqn{\bar M}{M-bar} whose interval covers zero).
#' @references Rambachan, A., & Roth, J. (2023). A more credible
#'   approach to parallel trends. \emph{Review of Economic Studies},
#'   90(5), 2555--2591.
#' @seealso \code{\link{morie_did_sensitivity_analysis}} for the 2x2
#'   \eqn{\delta \hat\sigma}{delta sigma-hat} parameterization.
#' @examples
#' set.seed(20)
#' df <- expand.grid(unit = 1:60, time = 1:8)
#' df$treat_time <- ifelse(df$unit <= 30, 5, Inf)
#' df$d <- as.integer(df$time >= df$treat_time)
#' df$y <- 0.5 * df$time + 1.5 * df$d + rnorm(nrow(df), sd = 0.5)
#' es <- morie_did_event_study(df, "y", "unit", "time", "treat_time",
#'                             leads = 3L, lags = 3L)
#' out <- morie_did_honest_sensitivity(es, m_bar_range = c(0, 1, 5))
#' out
#' @export
morie_did_honest_sensitivity <- function(event_study,
                                         m_bar_range = seq(0, 2, 0.5),
                                         target_time = 0L,
                                         alpha = 0.05) {
  cf <- event_study$coefficients
  if (is.null(cf) || !all(c("relative_time", "estimate",
                            "std_error") %in% names(cf))) {
    stop("`event_study` must carry a coefficients data frame with ",
         "relative_time / estimate / std_error.", call. = FALSE)
  }
  row <- cf[cf$relative_time == target_time, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("No event-study coefficient at relative time ", target_time,
         ".", call. = FALSE)
  }
  pre <- cf[cf$relative_time < 0 & cf$std_error > 0, , drop = FALSE]
  max_pre <- if (nrow(pre)) max(abs(pre$estimate)) else 0
  z <- stats::qnorm(1 - alpha / 2)
  rows <- lapply(m_bar_range, function(m_bar) {
    bias <- m_bar * max_pre
    lo <- row$estimate - bias - z * row$std_error
    hi <- row$estimate + bias + z * row$std_error
    data.frame(m_bar = m_bar, estimate = row$estimate,
               ci_lower = lo, ci_upper = hi,
               covers_zero = lo <= 0 & 0 <= hi)
  })
  out <- do.call(rbind, rows)
  cz <- out$m_bar[out$covers_zero]
  attr(out, "breakdown_m_bar") <- if (length(cz)) min(cz) else NA_real_
  attr(out, "max_pre_deviation") <- max_pre
  out
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
#' @examples
#' set.seed(8)
#' n <- 300
#' d <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
#' y <- 1 + 0.3 * d + 0.4 * p + 0.5 * d * p + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, d = d, post = p)
#' out <- morie_did_diagnostics(df, "y", "d", "post")
#' str(out, max.level = 1)
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
# 22. feTR weight diagnostic (de Chaisemartin & D'Haultfoeuille 2020)
# ---------------------------------------------------------------------------

#' Diagnose TWFE-DiD weights (de Chaisemartin & D'Haultfoeuille, 2020)
#'
#' Native feTR weight diagnostic: the decomposition of the two-way
#' fixed-effects DiD estimand into the weighted average of the
#' \eqn{N \times T}{N x T} unit-time ATEs. Each treated cell's weight
#' is proportional to its residual from regressing the treatment on
#' unit and time fixed effects, normalized over treated cells.  Use
#' this to quantify how many of the implicit comparisons receive
#' negative weight, which is the canonical diagnostic for whether a
#' TWFE specification can be interpreted as a convex combination of
#' treatment effects. Reproduces
#' \code{TwoWayFEWeights::twowayfeweights(type = "feTR")} weights to
#' machine precision (see \code{tests/cross/}).
#'
#' @param panel A long-format balanced (or near-balanced) panel
#'   \code{data.frame}.
#' @param group Name of the unit / group identifier column.
#' @param time Name of the time period column.
#' @param treatment Name of the binary or continuous treatment column.
#' @param outcome Optional outcome column; the feTR weights do not
#'   depend on the outcome values, so it may be \code{NULL}.
#' @param type Weight type; only \code{"feTR"} (feasible TR weights,
#'   the default and the canonical diagnostic) is supported natively.
#' @param ... Ignored; retained for back-compat.
#' @return An S3 list of class \code{morie_did_twfe_diagnostics} with
#'   elements \code{n_negative_weights}, \code{sum_weights},
#'   \code{sum_negative_weights}, \code{share_negative_weights},
#'   \code{method}, and \code{raw} (a data frame with one row per
#'   (group, time) cell and its weight).
#' @references de Chaisemartin, C., & D'Haultfoeuille, X. (2020).
#'   Two-way fixed effects estimators with heterogeneous treatment
#'   effects.  \emph{American Economic Review}, 110(9), 2964--2996.
#' @seealso \code{\link{morie_did_panel_fe}},
#'   \code{\link{morie_did_chaisemartin_dhaultfoeuille}}.
#' @examples
#' set.seed(11)
#' df <- expand.grid(unit = 1:60, time = 1:8)
#' df$treat_time <- ifelse(df$unit <= 30, sample(c(3, 5), 1), Inf)
#' df$treat_time <- ifelse(df$unit <= 15, 3, ifelse(df$unit <= 30, 5, Inf))
#' df$d <- as.integer(df$time >= df$treat_time)
#' df$y <- 0.5 * df$time + 1.5 * df$d + rnorm(nrow(df), sd = 0.5)
#' out <- morie_did_twoway_fe_weights(df, "unit", "time", "d")
#' c(out$sum_weights, out$n_negative_weights)
#' @export
morie_did_twoway_fe_weights <- function(panel, group, time, treatment,
                                        outcome = NULL,
                                        type = "feTR", ...) {
  if (!identical(type, "feTR")) {
    stop("Only type = \"feTR\" is supported by the native ",
         "implementation.", call. = FALSE)
  }
  df <- as.data.frame(panel)
  fit <- .morie_twfe_weights_native(df, group, time, treatment)
  weights <- fit$weight[!is.na(fit$weight)]
  n_neg <- sum(weights < 0)
  sum_w <- sum(weights)
  sum_neg <- sum(weights[weights < 0])
  share_neg <- if (length(weights) == 0L) NA_real_
               else n_neg / length(weights)
  structure(
    list(
      n_negative_weights      = n_neg,
      sum_weights             = sum_w,
      sum_negative_weights    = sum_neg,
      share_negative_weights  = share_neg,
      method = "twoway_fe_weights (rmorie native)",
      raw    = fit
    ),
    class = c("morie_did_twfe_diagnostics", "list")
  )
}


# ---------------------------------------------------------------------------
# 23. Synthetic DiD explicit-name extender (Arkhangelsky et al., 2021)
# ---------------------------------------------------------------------------

#' Synthetic DiD, explicit-name API (native)
#'
#' Parallel to \code{\link{morie_did_synthetic}}: the same native
#' Arkhangelsky et al. (2021) SDID engine, surfaced with the
#' placebo / bootstrap / jackknife variance options and the raw
#' unit / time weights. Use \code{morie_did_synthetic} when you want
#' the rmorie result-list shape consumed by \code{morie_did_*}
#' downstream code.
#'
#' @param panel Long-format balanced panel.
#' @param unit Unit identifier column.
#' @param time Time period column.
#' @param treatment Binary (0/1) treatment indicator that turns on at
#'   onset for treated units and is zero everywhere for controls.
#' @param outcome Outcome column.
#' @param vcov_method Inference method: one of \code{"placebo"}
#'   (default), \code{"bootstrap"}, \code{"jackknife"}.
#' @param ... Ignored; retained for back-compat.
#' @return An S3 list of class \code{morie_did_synthdid_result} with
#'   elements \code{att}, \code{std_error}, \code{vcov_method},
#'   \code{n_treated}, \code{n_control}, \code{n_pre},
#'   \code{n_post}, \code{method}, and \code{raw} (the full
#'   native SDID fit list).
#' @references Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens,
#'   G. W., & Wager, S. (2021). Synthetic difference-in-differences.
#'   \emph{American Economic Review}, 111(12), 4088--4118.
#' @seealso \code{\link{morie_did_synthetic}}.
#' @examples
#' if (requireNamespace("coresynth", quietly = TRUE)) {
#'   set.seed(13)
#'   df <- expand.grid(unit = 1:30, time = 1:8)
#'   df$treat_time <- ifelse(df$unit <= 5, 6, Inf)
#'   df$d <- as.integer(df$time >= df$treat_time)
#'   df$y <- 0.1 * df$time + 0.5 * df$d + rnorm(nrow(df), sd = 0.3)
#'   out <- morie_did_synthdid_estimate(df, unit = "unit", time = "time",
#'                                      treatment = "d", outcome = "y")
#'   out$att
#' }
#' @export
morie_did_synthdid_estimate <- function(panel, unit, time, treatment,
                                        outcome,
                                        vcov_method = "placebo", ...) {
  df <- as.data.frame(panel)
  if (is.logical(df[[treatment]]))
    df[[treatment]] <- as.integer(df[[treatment]])
  prep <- .morie_sdid_prepare(df, outcome, unit, time, treatment)
  fit <- .morie_sdid_inference(prep$Y, prep$N_co, prep$T_pre,
                               method = vcov_method)
  att <- fit$estimate
  se_est <- fit$se
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
      method       = "sdid (rmorie native)",
      raw          = fit
    ),
    class = c("morie_did_synthdid_result", "list")
  )
}
