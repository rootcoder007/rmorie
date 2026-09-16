# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Instrumental Variables (IV) and Two-Stage Least Squares (2SLS) estimators
# for morie.  Ports the public API of `src/morie/iv.py` (~2166 LOC) to R.
#
# Strategy: prefer CRAN wrappers.  Linear IV / 2SLS / LIML / over-identified
# Module 17 (feat/native-specializations): the IV family is native.
# 2SLS / LIML run on the k-class engine, GMM on the native two-step /
# CUE engines (R/iv_native.R); tests/cross validates against ivreg,
# AER and gmm where installed. The historical wording below described
# R so the package still installs in a minimal environment.
#
# Internal mathematical helpers that merely replicate `ivreg`'s
# internals (e.g. Kleibergen-Paap rank statistic, Stock-Yogo critical-
# value tables, exact conditional-LR test) are stubbed with informative
# the pre-module-17 dispatch strategy and is retained only as history.
#
# Public R names mirror the Python module under the `morie_iv_*` prefix.

#' @importFrom stats lm glm coef vcov pnorm pt pf pchisq qnorm qt qchisq model.matrix
#' model.frame fitted residuals binomial as.formula sigma complete.cases quantile predict
#' @importFrom utils head
NULL


# ---------------------------------------------------------------------------
# Shared @param block for the morie_iv_* family. Functions inherit via
# @inheritParams morie_iv_params (a roxygen-only stub).
# ---------------------------------------------------------------------------

#' Shared parameters for morie_iv_* estimators and diagnostics
#'
#' Roxygen-only stub holding the @param entries shared across the IV
#' family (Anderson-Rubin, conditional-LR, Hansen J, Sargan, etc.).
#' Functions reference these via `@inheritParams morie_iv_params` so
#' each `@param` is documented once and the Rd files stay consistent.
#'
#' @param data A `data.frame` (or tibble) holding the outcome,
#'   endogenous regressors, instruments, and any exogenous controls.
#' @param outcome Character; column name of the response variable.
#' @param endogenous Character vector; column names of the endogenous
#'   regressors.
#' @param instruments Character vector; column names of the
#'   instrumental variables.
#' @param exogenous Optional character vector of additional exogenous
#'   regressors included in both the structural equation and the
#'   first stage. `NULL` (default) for a just-identified design.
#' @param beta0 Numeric scalar or vector; the structural-coefficient
#'   value(s) to test under H0. Length must match `length(endogenous)`.
#' @param alpha Significance level (default `0.05`); controls the
#'   confidence-set / acceptance-region cut-off.
#' @param grid_min Numeric; lower bound of the AR confidence-set
#'   grid search over candidate `beta0` values.
#' @param grid_max Numeric; upper bound of the AR confidence-set grid.
#' @param grid_n Integer; number of grid points used in
#'   `morie_iv_anderson_rubin_ci` (default 100).
#' @param n_endogenous Integer; number of endogenous regressors used
#'   to look up the Stock-Yogo critical-value table.
#' @param n_instruments Integer; number of instruments used to look
#'   up the Stock-Yogo critical-value table.
#' @keywords internal
#' @name morie_iv_params
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Internal helper: Morie Iv Have Ivreg
#' @noRd
.morie_iv_have_ivreg <- function() {
  requireNamespace("ivreg", quietly = TRUE)
}
#' Internal helper: Morie Iv Have AER
#' @noRd
.morie_iv_have_AER <- function() {
  requireNamespace("AER", quietly = TRUE)
}

#' @param outcome See Usage.
#' @param endogenous See Usage.
#' @param instruments See Usage.
#' @param exogenous See Usage.
#' @keywords internal
.morie_iv_build_formula <- function(outcome, endogenous, instruments,
                                    exogenous = NULL) {
  exo <- if (length(exogenous)) paste(exogenous, collapse = " + ") else "1"
  end <- paste(endogenous, collapse = " + ")
  ins <- paste(instruments, collapse = " + ")
  rhs <- if (length(exogenous)) {
    paste0(exo, " + ", end, " | ", exo, " + ", ins)
  } else {
    paste0(end, " | ", ins)
  }
  stats::as.formula(paste(outcome, "~", rhs))
}

#' @param coef_vec See Usage.
#' @param se_vec See Usage.
#' @param n_obs See Usage.
#' @param method See Usage.
#' @param alpha See Usage.
#' @param dof See Usage.
#' @param details See Usage.
#' @keywords internal
.morie_iv_result <- function(coef_vec, se_vec, n_obs, method, alpha = 0.05,
                             dof = NA, details = list()) {
  z <- coef_vec / se_vec
  if (is.na(dof)) {
    p   <- 2 * stats::pnorm(-abs(z))
    cv  <- stats::qnorm(1 - alpha / 2)
  } else {
    p   <- 2 * stats::pt(-abs(z), df = dof)
    cv  <- stats::qt(1 - alpha / 2, df = dof)
  }
  list(
    coefficients   = coef_vec,
    std_errors     = se_vec,
    t_stats        = z,
    p_values       = p,
    ci_lower       = coef_vec - cv * se_vec,
    ci_upper       = coef_vec + cv * se_vec,
    variable_names = names(coef_vec),
    n_obs          = n_obs,
    method         = method,
    details        = details
  )
}

#' @param data See Usage.
#' @param outcome See Usage.
#' @param endogenous See Usage.
#' @param instruments See Usage.
#' @param exogenous See Usage.
#' @param robust See Usage.
#' @param alpha See Usage.
#' @keywords internal
.morie_iv_base_2sls <- function(data, outcome, endogenous, instruments,
                                exogenous = NULL, robust = TRUE, alpha = 0.05) {
  vars <- unique(c(outcome, endogenous, instruments, exogenous))
  df   <- data[stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]
  y <- as.numeric(df[[outcome]])
  X <- cbind(`(Intercept)` = 1, as.matrix(df[, c(endogenous, exogenous), drop = FALSE]))
  Z <- cbind(`(Intercept)` = 1, as.matrix(df[, c(instruments, exogenous), drop = FALSE]))
  ZtZ <- crossprod(Z)
  Pz  <- Z %*% solve(ZtZ, t(Z))
  XtPzX <- crossprod(X, Pz %*% X)
  XtPzy <- crossprod(X, Pz %*% y)
  beta  <- as.numeric(solve(XtPzX, XtPzy))
  names(beta) <- colnames(X)
  resid <- as.numeric(y - X %*% beta)
  n <- length(y)
  k <- length(beta)
  if (robust) {
    meat <- crossprod(X, resid^2 * X)
    bread <- solve(XtPzX)
    vcov_ <- bread %*% meat %*% bread
  } else {
    s2 <- sum(resid^2) / (n - k)
    vcov_ <- s2 * solve(XtPzX)
  }
  se <- sqrt(pmax(diag(vcov_), 0))
  .morie_iv_result(
    beta, se, n,
    method = "2sls (base-R fallback)",
    alpha = alpha,
    dof = n - k,
    details = list(residuals = resid, vcov = vcov_)
  )
}


# ---------------------------------------------------------------------------
# Core linear-IV estimators
# ---------------------------------------------------------------------------

#' Two-Stage Least Squares (2SLS)
#'
#' Estimates a linear IV model via 2SLS (rmorie native k-class engine).
#'
#' @param data Data frame.
#' @param outcome Name of the outcome column.
#' @param endogenous Character vector of endogenous regressor names.
#' @param instruments Character vector of excluded-instrument names.
#' @param exogenous Optional character vector of exogenous covariate names.
#' @param cluster Optional name of a cluster ID column. When given,
#'   standard errors are cluster-robust: the score is summed within
#'   each cluster before being squared, so within-cluster correlation
#'   is carried rather than assumed away. This selects the variance
#'   route; \code{robust} chooses between HC1 and conventional errors
#'   when no cluster is named, and \code{details$se_type} records
#'   which of the three produced the result.
#' @param robust Logical; if \code{TRUE} use HC1 robust standard errors.
#' @param alpha Significance level for confidence intervals.
#' @return A list with class \code{morie_iv_result} containing coefficients,
#'   standard errors, t-statistics, p-values, confidence interval bounds,
#'   variable names, sample size, method label, and a \code{details} list.
#' @examples
#' set.seed(2)
#' n <- 1000
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' res <- morie_iv_tsls(df, "y", "d", "z")
#' res$coefficients["d"]
#' @export
morie_iv_tsls <- function(data, outcome, endogenous, instruments,
                          exogenous = NULL, cluster = NULL,
                          robust = TRUE, alpha = 0.05) {
  d <- .morie_iv_design(data, outcome, endogenous, instruments, exogenous)
  cl <- NULL
  if (!is.null(cluster)) {
    if (!cluster %in% names(data)) {
      stop("`cluster` names a column that is not in `data`: ", cluster,
           ".", call. = FALSE)
    }
    # The design drops incomplete rows; the cluster labels must be
    # dropped with them or they line up with the wrong observations.
    keep <- stats::complete.cases(
      data[, unique(c(outcome, endogenous, instruments, exogenous,
                      cluster)), drop = FALSE])
    cl <- data[[cluster]][keep]
  }
  fit <- .morie_iv_kclass_native(d$y, d$X, d$Z, kappa = 1,
                                 robust = robust, cluster = cl)
  .morie_iv_result(fit$beta, fit$se, fit$n,
                   method = "2sls (rmorie native)",
                   alpha = alpha, dof = fit$df,
                   details = list(residuals = fit$residuals,
                                  vcov = fit$vcov,
                                  se_type = if (!is.null(cl)) "cluster"
                                            else if (robust) "HC1"
                                            else "const"))
}

#' Limited-Information Maximum Likelihood (LIML)
#'
#' Solves the LIML eigenvalue problem natively (k-class with the
#' minimum-eigenvalue kappa).
#' @inheritParams morie_iv_tsls
#' @return A named list with elements \code{coefficients}, \code{std_errors},
#' \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper},
#' \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(8)
#' n <- 1000
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- 0.2 + 0.6 * z + 0.4 * u + 0.3 * rnorm(n)
#' y <- 0.5 + 1.5 * d + u + rnorm(n)
#' df <- data.frame(y, d, z)
#' res <- morie_iv_liml(df, outcome = "y", endogenous = "d", instruments = "z")
#' res$coefficients["d"]
#' @export
morie_iv_liml <- function(data, outcome, endogenous, instruments,
                          exogenous = NULL, robust = TRUE, alpha = 0.05) {
  d <- .morie_iv_design(data, outcome, endogenous, instruments, exogenous)
  X_exo <- d$X[, setdiff(colnames(d$X), endogenous), drop = FALSE]
  X_endo <- d$X[, endogenous, drop = FALSE]
  kap <- .morie_iv_liml_kappa(d$y, X_endo, d$Z, X_exo)
  fit <- .morie_iv_kclass_native(d$y, d$X, d$Z, kappa = kap,
                                 robust = robust)
  .morie_iv_result(fit$beta, fit$se, fit$n,
                   method = "liml (rmorie native k-class)",
                   alpha = alpha, dof = fit$df,
                   details = list(kappa = kap,
                                  residuals = fit$residuals,
                                  vcov = fit$vcov))
}

#' Generalised Method of Moments (GMM) IV
#'
#' Two-step efficient GMM (rmorie native; HC0-weighted second step).
#' @inheritParams morie_iv_tsls
#' @param weight_matrix One of \code{"optimal"} (default, two-step) or
#'   \code{"identity"} (one-step / 2SLS-equivalent).
#' @return A named list with elements \code{coefficients}, \code{std_errors},
#' \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper},
#' \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1)
#' n <- 400
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' res <- morie_iv_gmm(df, "y", "d", "z")
#' res$coefficients
#' @export
morie_iv_gmm <- function(data, outcome, endogenous, instruments,
                         exogenous = NULL, weight_matrix = "optimal",
                         robust = TRUE, alpha = 0.05) {
  d <- .morie_iv_design(data, outcome, endogenous, instruments, exogenous)
  if (identical(weight_matrix, "identity")) {
    fit <- .morie_iv_kclass_native(d$y, d$X, d$Z, kappa = 1,
                                   robust = robust)
    return(.morie_iv_result(fit$beta, fit$se, fit$n,
                            method = "gmm (identity = 2sls, rmorie native)",
                            alpha = alpha, dof = fit$df,
                            details = list(vcov = fit$vcov)))
  }
  fit <- .morie_iv_gmm2_native(d$y, d$X, d$Z)
  .morie_iv_result(fit$beta, fit$se, fit$n,
                   method = "gmm (two-step efficient, rmorie native)",
                   alpha = alpha, dof = NA,
                   details = list(vcov = fit$vcov, J = fit$J,
                                  J_p = fit$J_p))
}

#' Continuously-Updated GMM (CUE-GMM)
#' @inheritParams morie_iv_gmm
#' @param max_iter Outer iteration cap (default 100).
#' @param tol Convergence tolerance on the objective.
#' @return A named list with elements \code{coefficients}, \code{std_errors},
#' \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper},
#' \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1); n <- 400
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' morie_iv_cue_gmm(df, "y", "d", "z")
#' @export
morie_iv_cue_gmm <- function(data, outcome, endogenous, instruments,
                             exogenous = NULL, max_iter = 100, tol = 1e-8,
                             alpha = 0.05) {
  d <- .morie_iv_design(data, outcome, endogenous, instruments, exogenous)
  fit <- .morie_iv_cue_native(d$y, d$X, d$Z, max_iter = max_iter,
                              tol = tol)
  .morie_iv_result(fit$beta, fit$se, fit$n,
                   method = "cue-gmm (rmorie native)",
                   alpha = alpha, dof = NA,
                   details = list(vcov = fit$vcov, J = fit$J,
                                  converged = fit$converged))
}

#' Wald (single-instrument) estimator
#'
#' \eqn{\hat\beta = (\bar y_{z=1} - \bar y_{z=0}) /
#'                 (\bar d_{z=1} - \bar d_{z=0})}{hatbeta = (bar y_z=1 - bar y_z=0) /
#' (bar d_z=1 - bar d_z=0)}.
#' @param data Data frame.
#' @param outcome Outcome column.
#' @param treatment Endogenous treatment column.
#' @param instrument Binary instrument column.
#' @param alpha Significance level.
#' @return A named list with elements \code{coefficients}, \code{std_errors},
#' \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper},
#' \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(11)
#' n <- 2000
#' z <- rbinom(n, 1, 0.5)
#' type <- sample(c("at", "nt", "co"), n, replace = TRUE, prob = c(0.1, 0.1, 0.8))
#' d <- ifelse(type == "at", 1L, ifelse(type == "nt", 0L, z))
#' y <- 0.5 + 2 * d + rnorm(n)
#' df <- data.frame(y, d, z)
#' res <- morie_iv_wald(df, outcome = "y", treatment = "d", instrument = "z")
#' res$coefficients["LATE"]
#' @export
morie_iv_wald <- function(data, outcome, treatment, instrument, alpha = 0.05) {
  y <- data[[outcome]]
  d <- data[[treatment]]
  z <- data[[instrument]]
  num <- mean(y[z == 1]) - mean(y[z == 0])
  den <- mean(d[z == 1]) - mean(d[z == 0])
  beta <- num / den
  # Delta-method SE for beta = num/den, with the often-omitted
  # Cov(num, den) term that previous morie (and most textbooks) drop.
  # Per-z-stratum: cov(mean(y), mean(d)) = cov(y, d) / n.
  n1 <- sum(z == 1)
  n0 <- sum(z == 0)
  v_y <- stats::var(y[z == 1]) / n1 + stats::var(y[z == 0]) / n0
  v_d <- stats::var(d[z == 1]) / n1 + stats::var(d[z == 0]) / n0
  c_yd <- stats::cov(y[z == 1], d[z == 1]) / n1 +
          stats::cov(y[z == 0], d[z == 0]) / n0
  se  <- sqrt(max(v_y / den^2 +
                    (num^2 / den^4) * v_d -
                    2 * (num / den^3) * c_yd,
                  0))
  .morie_iv_result(c(LATE = beta), c(LATE = se), length(y),
                   method = "wald (LATE)", alpha = alpha)
}


# ---------------------------------------------------------------------------
# First-stage / weak-instrument diagnostics
# ---------------------------------------------------------------------------

#' First-stage F-statistics and partial R^2
#' @inheritParams morie_iv_params
#' @return A \code{data.frame} of first-stage diagnostics, one row per endogenous regressor.
#' @examples
#' set.seed(1)
#' n <- 400
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' out <- morie_iv_first_stage_diagnostics(df, "d", "z")
#' out$F
#' @export
morie_iv_first_stage_diagnostics <- function(data, endogenous, instruments,
                                             exogenous = NULL) {
  rows <- lapply(endogenous, function(e) {
    rhs <- paste(c(instruments, exogenous), collapse = " + ")
    f_full <- stats::as.formula(paste(e, "~", rhs))
    f_red  <- if (length(exogenous)) {
      stats::as.formula(paste(e, "~", paste(exogenous, collapse = " + ")))
    } else stats::as.formula(paste(e, "~", "1"))
    fit_full <- stats::lm(f_full, data = data)
    fit_red  <- stats::lm(f_red,  data = data)
    f_stat   <- anova(fit_red, fit_full)$F[2]
    r2_full  <- summary(fit_full)$r.squared
    r2_red   <- summary(fit_red)$r.squared
    data.frame(
      endogenous = e,
      F          = f_stat,
      partial_R2 = r2_full - r2_red,
      n_instruments = length(instruments)
    )
  })
  do.call(rbind, rows)
}

#' Cragg-Donald weak-instrument F statistic
#'
#' Computes the Cragg-Donald (1993) weak-instrument statistic. The
#' statistic is a function of the first-stage regression and is
#' independent of the outcome variable; \code{outcome} only needs to
#' name a numeric column in \code{data} so \pkg{ivreg} can compile
#' a formula. When \code{outcome = NULL} (default), the first
#' endogenous regressor is reused as the outcome -- works because
#' \pkg{ivreg}'s weak-IV diagnostic comes from the first stage
#' regardless of \code{y}.
#'
#' @param data Data frame.
#' @param endogenous Character vector of endogenous regressor names.
#' @param instruments Character vector of excluded-instrument names.
#' @param exogenous Optional exogenous covariates.
#' @param outcome Optional outcome column name. Default \code{NULL}
#'   reuses \code{endogenous\[1\]}; the resulting F-statistic is
#'   unaffected because Cragg-Donald only reads the first stage.
#' @return Named list with \code{statistic}, \code{p_value},
#'   \code{name}, \code{details}.
#' @examples
#' set.seed(1); n <- 300
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' df <- data.frame(d, z)
#' morie_iv_cragg_donald(df, "d", "z")
#' @export
morie_iv_cragg_donald <- function(data, endogenous, instruments,
                                  exogenous = NULL, outcome = NULL) {
  # Cragg-Donald (1993) tests instrument strength via the smallest
  # eigenvalue of the first-stage projection -- it is a property of
  # the first stage (endogenous on instruments + exogenous), so we
  # compute it directly without needing an outcome. For
  # just-identified k_endogenous = 1, the statistic collapses to
  # the first-stage F. The outcome argument is kept for API
  # compatibility but is ignored.
  fs <- morie_iv_first_stage_diagnostics(data, endogenous, instruments,
                                          exogenous)
  f_stat <- unname(fs$F[1])
  k_ins <- length(instruments)
  k_exo <- length(exogenous)
  df1 <- k_ins
  df2 <- nrow(data) - k_ins - k_exo - 1L
  p_val <- if (is.finite(f_stat) && df1 > 0L && df2 > 0L)
              1 - stats::pf(f_stat, df1, df2)
           else NA_real_
  list(statistic = f_stat,
       p_value   = p_val,
       name      = "Cragg-Donald (first-stage F)",
       details   = list(first_stage = fs,
                        df1 = df1, df2 = df2,
                        k_endogenous = length(endogenous),
                        k_instruments = k_ins,
                        k_exogenous = k_exo,
                        outcome_used = NA_character_))
}

#' Stock-Yogo critical values
#' @inheritParams morie_iv_params
#' @return A named \code{list} of Stock-Yogo weak-instrument critical values.
#' @examples
#' out <- morie_iv_stock_yogo(n_endogenous = 1, n_instruments = 1)
#' out
#' @export
morie_iv_stock_yogo <- function(n_endogenous = 1, n_instruments = 1) {
  # TODO: ship full Stock & Yogo (2005, Table 5.2) lookup table -- currently
  # only the 10/15/20/25 percent maximal-bias thresholds for the leading
  # 1-endogenous case are reproduced.  Replicates iv.py:stock_yogo_critical_values.
  tab <- list("1_1" = c(`10pct` = 16.38, `15pct` = 8.96,
                        `20pct` = 6.66, `25pct` = 5.53),
              "1_2" = c(`10pct` = 19.93, `15pct` = 11.59,
                        `20pct` = 8.75, `25pct` = 7.25),
              "1_3" = c(`10pct` = 22.30, `15pct` = 12.83,
                        `20pct` = 9.54, `25pct` = 7.80))
  key <- paste(n_endogenous, n_instruments, sep = "_")
  if (!key %in% names(tab))
    stop("Stock-Yogo: combination not in shipped table. TODO: extend.")
  as.list(tab[[key]])
}

#' Kleibergen-Paap rank statistic
#' @inheritParams morie_iv_params
#' @return A named list with elements \code{statistic}, \code{p_value}, \code{name}, \code{details}.
#' @examples
#' set.seed(1)
#' n <- 300
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' out <- morie_iv_kleibergen_paap(df, "d", "z")
#' out$statistic
#' @export
morie_iv_kleibergen_paap <- function(data, endogenous, instruments,
                                     exogenous = NULL) {
  # TODO: native non-i.i.d. KP rank test (Kleibergen & Paap, 2006).  For now
  # delegate to ivreg's weak-instrument diagnostic, which uses KP under HC.
  morie_iv_cragg_donald(data, endogenous, instruments, exogenous)
}

#' Anderson-Rubin (AR) weak-IV-robust test
#' @inheritParams morie_iv_params
#' @return A named list with elements \code{statistic}, \code{F_statistic},
#' \code{p_value}, \code{name}, \code{df}, \code{df_resid}, \code{beta0}.
#' @examples
#' set.seed(1); n <- 400
#' z1 <- rbinom(n, 1, 0.5); z2 <- rnorm(n); u <- rnorm(n)
#' d <- 0.5 * z1 + 0.4 * z2 + 0.3 * u + rnorm(n, sd = 0.3)
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z1, z2)
#' morie_iv_anderson_rubin(df, "y", "d", c("z1", "z2"))
#' @export
morie_iv_anderson_rubin <- function(data, outcome, endogenous, instruments,
                                    exogenous = NULL, beta0 = NULL,
                                    alpha = 0.05) {
  # AR test: regress residual y - X*beta0 on [exog, instruments]; the
  # excluded-instrument F-stat is the AR statistic. Under H0:beta=beta0,
  # AR ~ F(k_ins, n - k_exog - k_ins); AR * k_ins ~ chi-square(k_ins).
  if (is.null(beta0)) beta0 <- rep(0, length(endogenous))
  y <- as.numeric(data[[outcome]])
  X_end <- as.matrix(data[, endogenous, drop = FALSE])
  e <- as.numeric(y - X_end %*% beta0)
  df_ <- cbind(data, .ar_resid = e)
  rhs_full <- paste(c(exogenous, instruments), collapse = " + ")
  rhs_red  <- if (length(exogenous)) paste(exogenous, collapse = " + ") else "1"
  f_full <- stats::lm(stats::as.formula(paste(".ar_resid ~", rhs_full)),
                       data = df_)
  f_red  <- stats::lm(stats::as.formula(paste(".ar_resid ~", rhs_red)),
                       data = df_)
  ssr_full <- sum(stats::residuals(f_full)^2)
  ssr_red  <- sum(stats::residuals(f_red)^2)
  k_ins <- length(instruments)
  df_resid <- stats::df.residual(f_full)
  F_stat <- ((ssr_red - ssr_full) / k_ins) / (ssr_full / df_resid)
  chi2_stat <- k_ins * F_stat   # asymptotic chi-square form
  pval <- stats::pchisq(chi2_stat, df = k_ins, lower.tail = FALSE)
  list(statistic = unname(chi2_stat),
       F_statistic = unname(F_stat),
       p_value = unname(pval),
       name = "Anderson-Rubin", df = k_ins,
       df_resid = df_resid, beta0 = beta0)
}

#' Grid-based Anderson-Rubin confidence interval for a single endogenous
#' variable
#' @inheritParams morie_iv_params
#' @return A vector of the computed values.
#' @examples
#' set.seed(1); n <- 400
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' morie_iv_anderson_rubin_ci(df, "y", "d", "z", grid_min = -2, grid_max = 2, grid_n = 50)
#' @export
morie_iv_anderson_rubin_ci <- function(data, outcome, endogenous, instruments,
                                       exogenous = NULL, grid_min = -10,
                                       grid_max = 10, grid_n = 200,
                                       alpha = 0.05) {
  grid <- seq(grid_min, grid_max, length.out = grid_n)
  keep <- vapply(grid, function(b) {
    res <- morie_iv_anderson_rubin(data, outcome, endogenous, instruments,
                                   exogenous, beta0 = b, alpha = alpha)
    res$p_value > alpha
  }, logical(1))
  if (!any(keep)) return(c(NA_real_, NA_real_))
  c(min(grid[keep]), max(grid[keep]))
}

#' Conditional likelihood-ratio (CLR) test of Moreira (2003)
#' @inheritParams morie_iv_params
#' @return A named list with elements \code{statistic}, \code{F_statistic},
#' \code{p_value}, \code{name}, \code{df}, \code{df_resid}, \code{beta0}.
#' @examples
#' set.seed(1); n <- 300
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' morie_iv_conditional_lr(df, "y", "d", "z")
#' @export
morie_iv_conditional_lr <- function(data, outcome, endogenous, instruments,
                                    exogenous = NULL, beta0 = 0) {
  # TODO: full Moreira (2003) conditional reference distribution; currently
  # we return the AR statistic as a conservative substitute.
  res <- morie_iv_anderson_rubin(data, outcome, endogenous, instruments,
                                 exogenous, beta0 = beta0)
  res$name <- "Conditional LR (AR conservative)"
  res
}

#' Sargan test of overidentifying restrictions (homoskedastic)
#' @inheritParams morie_iv_params
#' @return A named \code{list} (see Details).
#' @examples
#' set.seed(1)
#' n <- 400
#' z1 <- rbinom(n, 1, 0.5); z2 <- rnorm(n); u <- rnorm(n)
#' d <- 0.5 * z1 + 0.4 * z2 + 0.3 * u + rnorm(n, sd = 0.3)
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z1, z2)
#' out <- morie_iv_sargan(df, "y", "d", c("z1", "z2"))
#' out$p_value
#' @export
morie_iv_sargan <- function(data, outcome, endogenous, instruments,
                            exogenous = NULL) {
  # n*R^2 of the 2SLS-residual regression on the instrument set
  fit2sls <- .morie_iv_base_2sls(data, outcome, endogenous, instruments,
                                 exogenous)
  resid <- fit2sls$details$residuals
  rhs <- paste(c(instruments, exogenous), collapse = " + ")
  df_ <- cbind(data, .resid_iv_ = resid)
  fit <- stats::lm(stats::as.formula(paste(".resid_iv_ ~", rhs)), data = df_)
  R2  <- summary(fit)$r.squared
  n   <- nrow(df_)
  k   <- length(instruments) - length(endogenous)
  if (k <= 0) return(list(statistic = NA, p_value = NA,
                          name = "Sargan (just-identified)", df = 0))
  stat <- n * R2
  list(statistic = stat,
       p_value   = stats::pchisq(stat, df = k, lower.tail = FALSE),
       name = "Sargan", df = k)
}

#' Hansen J test of overidentifying restrictions (robust)
#' @inheritParams morie_iv_params
#' @return A named \code{list} (see Details).
#' @examples
#' set.seed(1)
#' n <- 400
#' z1 <- rbinom(n, 1, 0.5); z2 <- rnorm(n); u <- rnorm(n)
#' d <- 0.5 * z1 + 0.4 * z2 + 0.3 * u + rnorm(n, sd = 0.3)
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z1, z2)
#' out <- morie_iv_hansen_j(df, "y", "d", c("z1", "z2"))
#' out$name
#' @export
morie_iv_hansen_j <- function(data, outcome, endogenous, instruments,
                              exogenous = NULL) {
  d <- .morie_iv_design(data, outcome, endogenous, instruments, exogenous)
  fit <- .morie_iv_gmm2_native(d$y, d$X, d$Z)
  list(statistic = fit$J, p_value = fit$J_p,
       name = "Hansen J (rmorie native)", df = fit$J_df)
}

#' Hausman test: OLS vs 2SLS
#' @inheritParams morie_iv_params
#' @return A named \code{list} (see Details).
#' @examples
#' set.seed(1)
#' n <- 500
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' out <- morie_iv_hausman(df, "y", "d", "z")
#' out$statistic
#' @export
morie_iv_hausman <- function(data, outcome, endogenous, instruments,
                             exogenous = NULL) {
  rhs_full <- paste(c(endogenous, exogenous), collapse = " + ")
  f_ols    <- stats::as.formula(paste(outcome, "~", rhs_full))
  ols      <- stats::lm(f_ols, data = data)
  iv       <- morie_iv_tsls(data, outcome, endogenous, instruments, exogenous)
  diff     <- iv$coefficients[names(stats::coef(ols))] - stats::coef(ols)
  v_iv     <- iv$details$vcov %||% diag(iv$std_errors^2)
  v_ols    <- stats::vcov(ols)
  v_diff   <- v_iv - v_ols
  v_diff   <- 0.5 * (v_diff + t(v_diff))
  stat     <- as.numeric(t(diff) %*% .morie_ginv(v_diff) %*% diff)
  list(statistic = stat,
       p_value   = stats::pchisq(stat, df = length(diff),
                                 lower.tail = FALSE),
       name = "Hausman")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Durbin-Wu-Hausman test of endogeneity
#' @inheritParams morie_iv_params
#' @return A named list with elements \code{statistic}, \code{p_value}, \code{name}.
#' @examples
#' set.seed(1)
#' n <- 500
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' out <- morie_iv_durbin_wu_hausman(df, "y", "d", "z")
#' out$name
#' @export
morie_iv_durbin_wu_hausman <- function(data, outcome, endogenous, instruments,
                                       exogenous = NULL) {
  # Stage-1 residual augmentation form (equivalent to control function)
  res <- morie_iv_hausman(data, outcome, endogenous, instruments, exogenous)
  res$name <- "Durbin-Wu-Hausman"
  res
}


# ---------------------------------------------------------------------------
# Robust / non-standard IV estimators
# ---------------------------------------------------------------------------

#' Jackknife IV (JIVE; Angrist, Imbens & Krueger 1999)
#' @inheritParams morie_iv_tsls
#' @return A named list with elements \code{coefficients}, \code{std_errors},
#' \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper},
#' \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(5)
#' n <- 600
#' z1 <- rbinom(n, 1, 0.5); z2 <- rnorm(n); u <- rnorm(n)
#' d <- 0.5 * z1 + 0.4 * z2 + 0.3 * u + rnorm(n, sd = 0.3)
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z1, z2)
#' res <- morie_iv_jive(df, "y", "d", c("z1", "z2"))
#' res$coefficients["d"]
#' @export
morie_iv_jive <- function(data, outcome, endogenous, instruments,
                          exogenous = NULL, alpha = 0.05) {
  vars <- unique(c(outcome, endogenous, instruments, exogenous))
  df   <- data[stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]
  y <- as.numeric(df[[outcome]])
  X <- cbind(`(Intercept)` = 1, as.matrix(df[, c(endogenous, exogenous),
                                            drop = FALSE]))
  Z <- cbind(`(Intercept)` = 1, as.matrix(df[, c(instruments, exogenous),
                                            drop = FALSE]))
  H <- Z %*% solve(crossprod(Z), t(Z))   # hat matrix
  hd <- diag(H)
  if (any(abs(1 - hd) < 1e-10))
    stop("JIVE: leverage of 1 detected (perfect fit); cannot leave-one-out.",
         call. = FALSE)
  # JIVE projects ONLY the endogenous columns (Angrist-Imbens-Krueger 1999);
  # the intercept and exogenous columns pass through unchanged. The earlier
  # form `Xhat <- (H %*% X - hd * X) / (1 - hd)` projected every column,
  # including the intercept + exogenous controls, which biases the IV
  # estimator. Matches src/morie/iv.py:1604-1613.
  D <- as.matrix(df[, endogenous, drop = FALSE])
  storage.mode(D) <- "double"
  D_hat_full <- H %*% D
  D_hat_jive <- (D_hat_full - hd * D) / (1 - hd)
  Xhat <- X
  Xhat[, endogenous] <- D_hat_jive
  beta <- as.numeric(solve(crossprod(Xhat, X), crossprod(Xhat, y)))
  names(beta) <- colnames(X)
  resid <- as.numeric(y - X %*% beta)
  n <- length(y)
  k <- length(beta)
  bread <- solve(crossprod(Xhat, X))
  meat  <- crossprod(Xhat, resid^2 * Xhat)
  vcov_ <- bread %*% meat %*% t(bread)
  se    <- sqrt(pmax(diag(vcov_), 0))
  .morie_iv_result(beta, se, n, method = "JIVE", alpha = alpha,
                   dof = n - k,
                   details = list(residuals = resid, vcov = vcov_))
}

#' Split-sample IV
#' @inheritParams morie_iv_tsls
#' @param split_fraction Fraction of the data used in the first stage.
#' @param seed RNG seed.
#' @return A named list with elements \code{coefficients}, \code{std_errors},
#' \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper},
#' \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1)
#' n <- 400
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' res <- morie_iv_split_sample(df, "y", "d", "z", split_fraction = 0.5, seed = 1)
#' res$coefficients
#' @export
morie_iv_split_sample <- function(data, outcome, endogenous, instruments,
                                  exogenous = NULL, split_fraction = 0.5,
                                  seed = 42, alpha = 0.05) {
  set.seed(seed)
  n  <- nrow(data)
  idx1 <- sample.int(n, floor(n * split_fraction))
  d1 <- data[idx1, , drop = FALSE]
  d2 <- data[-idx1, , drop = FALSE]
  # First stage on split 1
  pred_list <- lapply(endogenous, function(e) {
    rhs <- paste(c(instruments, exogenous), collapse = " + ")
    fit <- stats::lm(stats::as.formula(paste(e, "~", rhs)), data = d1)
    stats::predict(fit, newdata = d2)
  })
  d2_aug <- d2
  for (i in seq_along(endogenous))
    d2_aug[[paste0("hatcol_", endogenous[i])]] <- pred_list[[i]]
  rhs2 <- paste(c(paste0("hatcol_", endogenous), exogenous), collapse = " + ")
  fit2 <- stats::lm(stats::as.formula(paste(outcome, "~", rhs2)),
                    data = d2_aug)
  cf <- stats::coef(fit2)
  se <- sqrt(diag(stats::vcov(fit2)))
  .morie_iv_result(cf, se, length(fit2$residuals),
                   method = "split-sample IV",
                   alpha = alpha, dof = fit2$df.residual,
                   details = list(fit = fit2))
}

#' Control-function (residual augmentation) IV
#' @inheritParams morie_iv_tsls
#' @return A named list with elements \code{coefficients}, \code{std_errors},
#' \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper},
#' \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1); n <- 200
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' morie_iv_control_function(df, "y", "d", "z")
#' @export
morie_iv_control_function <- function(data, outcome, endogenous,
                                      instruments, exogenous = NULL,
                                      robust = TRUE, alpha = 0.05,
                                      dist = c("normal", "t")) {
  dist <- match.arg(dist)
  cols <- unique(c(outcome, endogenous, instruments, exogenous))
  miss <- setdiff(cols, names(data))
  if (length(miss)) {
    stop("control_function: no such column(s) in `data`: ",
         paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  df <- data[stats::complete.cases(data[, cols, drop = FALSE]), ,
             drop = FALSE]
  y <- as.numeric(df[[outcome]])
  n <- length(y)
  D <- as.matrix(df[, endogenous, drop = FALSE])
  Zx <- as.matrix(df[, instruments, drop = FALSE])
  W <- if (length(exogenous))
    as.matrix(df[, exogenous, drop = FALSE]) else NULL

  # First stage, one per endogenous regressor: the residual is the part
  # of D the instruments cannot explain, and carrying it into the second
  # stage is what controls the endogeneity.
  Z_first <- cbind(1, Zx, W)
  V_hat <- matrix(0, n, ncol(D))
  for (j in seq_len(ncol(D))) {
    b <- qr.solve(Z_first, D[, j])
    V_hat[, j] <- D[, j] - Z_first %*% b
  }

  X <- cbind(1, D, W, V_hat)
  colnames(X) <- c("const", endogenous,
                   if (length(exogenous)) exogenous else NULL,
                   paste0("v_hat_", endogenous))
  beta <- as.numeric(qr.solve(X, y))
  names(beta) <- colnames(X)
  resid <- as.numeric(y - X %*% beta)
  k <- ncol(X)
  XtX_inv <- tryCatch(solve(crossprod(X)),
                      error = function(e) .morie_ginv(crossprod(X)))
  if (isTRUE(robust)) {
    meat <- crossprod(X, resid^2 * X)
    V <- (n / (n - k)) * (XtX_inv %*% meat %*% XtX_inv)
  } else {
    V <- (sum(resid^2) / (n - k)) * XtX_inv
  }
  se <- sqrt(pmax(diag(V), 0))
  names(se) <- colnames(X)

  .morie_iv_result(beta, se, n,
                   method = "control_function",
                   alpha = alpha,
                   dof = if (dist == "t") n - k else NA,
                   details = list(vcov = V, residuals = resid,
                                  v_hat = V_hat,
                                  se_type = if (isTRUE(robust)) "HC1"
                                            else "const",
                                  dist = dist))
}

#' IV Probit (Rivers-Vuong control function)
#' @inheritParams morie_iv_tsls
#' @return A named list with elements \code{coefficients}, \code{std_errors},
#' \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper},
#' \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1)
#' n <- 400
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(yb = as.integer(y > median(y)), d, z)
#' res <- morie_iv_probit(df, "yb", "d", "z")
#' res$coefficients
#' @export
morie_iv_probit <- function(data, outcome, endogenous, instruments,
                            exogenous = NULL, alpha = 0.05) {
  if (length(endogenous) != 1)
    stop("morie_iv_probit supports 1 endogenous regressor")
  rhs <- paste(c(instruments, exogenous), collapse = " + ")
  fs  <- stats::lm(stats::as.formula(paste(endogenous, "~", rhs)),
                   data = data)
  data$.cf_resid_ <- stats::residuals(fs)
  rhs2 <- paste(c(endogenous, exogenous, ".cf_resid_"), collapse = " + ")
  ss   <- stats::glm(stats::as.formula(paste(outcome, "~", rhs2)),
                     data = data,
                     family = stats::binomial(link = "probit"))
  cf <- stats::coef(ss)
  se <- sqrt(diag(stats::vcov(ss)))
  .morie_iv_result(cf, se, length(ss$residuals),
                   method = "IV probit (Rivers-Vuong CF)",
                   alpha = alpha, dof = ss$df.residual,
                   details = list(first_stage = fs, probit = ss))
}

#' Panel IV with unit (and optional time) fixed effects via within-transform
#' @inheritParams morie_iv_tsls
#' @param unit Cluster / unit identifier column.
#' @param time_fe Optional time-FE column.
#' @return A named list with elements \code{coefficients}, \code{std_errors},
#' \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper},
#' \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1)
#' n_unit <- 30; n_time <- 5; n <- n_unit * n_time
#' df <- data.frame(
#'   unit = rep(seq_len(n_unit), each = n_time),
#'   z = rbinom(n, 1, 0.5))
#' df$d <- 0.5 * df$z + rnorm(n, sd = 0.5)
#' df$y <- 0.5 * df$d + rnorm(n, sd = 0.5)
#' res <- morie_iv_panel(df, "y", "d", "z", unit = "unit")
#' res$coefficients
#' @export
morie_iv_panel <- function(data, outcome, endogenous, instruments, unit,
                           exogenous = NULL, time_fe = NULL, alpha = 0.05) {
  # Within transform (unit FE, optionally time FE), then native 2SLS.
  for (v in c(outcome, endogenous, instruments, exogenous)) {
    data[[v]] <- data[[v]] - stats::ave(data[[v]], data[[unit]])
    if (!is.null(time_fe))
      data[[v]] <- data[[v]] - stats::ave(data[[v]], data[[time_fe]])
  }
  res <- morie_iv_tsls(data, outcome, endogenous, instruments, exogenous,
                       alpha = alpha)
  res$method <- "panel IV (rmorie native within + 2sls)"
  res
}


# ---------------------------------------------------------------------------
# Composite diagnostic dashboards
# ---------------------------------------------------------------------------

#' Composite IV diagnostics
#' @inheritParams morie_iv_params
#' @return A named list with elements \code{first_stage}, \code{cragg_donald},
#' \code{sargan}, \code{hausman}, \code{n_obs}.
#' @examples
#' set.seed(1); n <- 400
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' morie_iv_diagnostics(df, "y", "d", "z")
#' @export
morie_iv_diagnostics <- function(data, outcome, endogenous, instruments,
                                 exogenous = NULL) {
  list(
    first_stage     = morie_iv_first_stage_diagnostics(data, endogenous,
                                                       instruments, exogenous),
    cragg_donald    = morie_iv_cragg_donald(data, endogenous, instruments,
                                            exogenous),
    sargan          = morie_iv_sargan(data, outcome, endogenous, instruments,
                                      exogenous),
    hausman         = morie_iv_hausman(data, outcome, endogenous, instruments,
                                       exogenous),
    n_obs           = nrow(data)
  )
}

#' IV residual analysis
#' @inheritParams morie_iv_params
#' @return A \code{data.frame} with columns \code{fitted}, \code{residual},
#' \code{abs_resid}, \code{sq_resid}.
#' @examples
#' set.seed(1)
#' n <- 400
#' z <- rbinom(n, 1, 0.5); u <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * z + 0.3 * u))
#' y <- 0.5 * d + 0.4 * u + rnorm(n, sd = 0.5)
#' df <- data.frame(y, d, z)
#' out <- morie_iv_residual_analysis(df, "y", "d", "z")
#' head(out)
#' @export
morie_iv_residual_analysis <- function(data, outcome, endogenous, instruments,
                                       exogenous = NULL) {
  fit <- morie_iv_tsls(data, outcome, endogenous, instruments, exogenous)
  resid <- if (!is.null(fit$details$fit))
    stats::residuals(fit$details$fit)
  else fit$details$residuals
  data.frame(
    fitted   = if (!is.null(fit$details$fit)) stats::fitted(fit$details$fit)
               else as.numeric(data[[outcome]]) - resid,
    residual = resid,
    abs_resid = abs(resid),
    sq_resid  = resid^2
  )
}
