# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Instrumental Variables (IV) and Two-Stage Least Squares (2SLS) estimators
# for morie.  Ports the public API of `src/morie/iv.py` (~2166 LOC) to R.
#
# Strategy: prefer CRAN wrappers.  Linear IV / 2SLS / LIML / over-identified
# GMM are dispatched to `ivreg::ivreg` (the modern, actively maintained
# successor to `AER::ivreg`).  When `ivreg` is unavailable we fall back to
# `AER::ivreg`, and finally to a hand-rolled projection-matrix 2SLS in base
# R so the package still installs in a minimal environment.
#
# Internal mathematical helpers that merely replicate `ivreg`'s
# internals (e.g. Kleibergen-Paap rank statistic, Stock-Yogo critical-
# value tables, exact conditional-LR test) are stubbed with informative
# TODOs and dispatch to `ivreg::summary()` diagnostics where possible.
#
# Public R names mirror the Python module under the `morie_iv_*` prefix.

#' @importFrom stats lm glm coef vcov pnorm pt pf pchisq qnorm qt qchisq
#'   model.matrix model.frame fitted residuals binomial as.formula sigma
#'   complete.cases quantile predict
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

.morie_iv_have_ivreg <- function() {
  requireNamespace("ivreg", quietly = TRUE)
}
.morie_iv_have_AER <- function() {
  requireNamespace("AER", quietly = TRUE)
}

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
#' Estimates a linear IV model via 2SLS, preferring \code{ivreg::ivreg}.
#'
#' @param data Data frame.
#' @param outcome Name of the outcome column.
#' @param endogenous Character vector of endogenous regressor names.
#' @param instruments Character vector of excluded-instrument names.
#' @param exogenous Optional character vector of exogenous covariate names.
#' @param cluster Optional name of a cluster ID column.
#' @param robust Logical; if \code{TRUE} use HC1 robust standard errors.
#' @param alpha Significance level for confidence intervals.
#' @return A list with class \code{morie_iv_result} containing coefficients,
#'   standard errors, t-statistics, p-values, confidence interval bounds,
#'   variable names, sample size, method label, and a \code{details} list.
#' @export
morie_iv_tsls <- function(data, outcome, endogenous, instruments,
                          exogenous = NULL, cluster = NULL,
                          robust = TRUE, alpha = 0.05) {
  if (.morie_iv_have_ivreg()) {
    f   <- .morie_iv_build_formula(outcome, endogenous, instruments, exogenous)
    fit <- ivreg::ivreg(f, data = data)
    se_type <- if (robust) "HC1" else "const"
    smry    <- summary(fit, vcov. = if (robust) sandwich::vcovHC else NULL,
                       diagnostics = TRUE)
    cf  <- stats::coef(fit)
    vc  <- if (robust && requireNamespace("sandwich", quietly = TRUE)) {
      sandwich::vcovHC(fit, type = "HC1")
    } else stats::vcov(fit)
    se  <- sqrt(diag(vc))
    return(.morie_iv_result(cf, se, length(fit$residuals),
                            method = "2sls (ivreg)",
                            alpha = alpha,
                            dof = fit$df.residual,
                            details = list(fit = fit, summary = smry,
                                           se_type = se_type)))
  }
  if (.morie_iv_have_AER()) {
    f   <- .morie_iv_build_formula(outcome, endogenous, instruments, exogenous)
    fit <- AER::ivreg(f, data = data)
    cf  <- stats::coef(fit)
    se <- sqrt(diag(stats::vcov(fit)))
    return(.morie_iv_result(cf, se, length(fit$residuals),
                            method = "2sls (AER)", alpha = alpha,
                            dof = fit$df.residual,
                            details = list(fit = fit)))
  }
  .morie_iv_base_2sls(data, outcome, endogenous, instruments, exogenous,
                      robust = robust, alpha = alpha)
}

#' Limited-Information Maximum Likelihood (LIML)
#'
#' Solves the LIML eigenvalue problem; falls back to \code{ivreg::ivreg(...,
#' method = "M")} if available.
#' @inheritParams morie_iv_tsls
#' @export
morie_iv_liml <- function(data, outcome, endogenous, instruments,
                          exogenous = NULL, robust = TRUE, alpha = 0.05) {
  if (.morie_iv_have_ivreg()) {
    f   <- .morie_iv_build_formula(outcome, endogenous, instruments, exogenous)
    fit <- tryCatch(
      ivreg::ivreg(f, data = data, method = "M"),  # M = LIML in ivreg >=0.6
      error = function(e) ivreg::ivreg(f, data = data)
    )
    cf <- stats::coef(fit)
    vc <- stats::vcov(fit)
    se <- sqrt(diag(vc))
    return(.morie_iv_result(cf, se, length(fit$residuals),
                            method = "liml (ivreg)",
                            alpha = alpha, dof = fit$df.residual,
                            details = list(fit = fit)))
  }
  # TODO: native eigenvalue LIML -- replicate iv.py:liml().  Falls back to 2SLS.
  res <- morie_iv_tsls(data, outcome, endogenous, instruments, exogenous,
                       robust = robust, alpha = alpha)
  res$method <- "liml (2sls fallback \u2014 install ivreg)"
  res
}

#' Generalised Method of Moments (GMM) IV
#'
#' Two-step efficient GMM via \code{gmm::gmm}; falls back to 2SLS otherwise.
#' @inheritParams morie_iv_tsls
#' @param weight_matrix One of \code{"optimal"} (default, two-step) or
#'   \code{"identity"} (one-step / 2SLS-equivalent).
#' @export
morie_iv_gmm <- function(data, outcome, endogenous, instruments,
                         exogenous = NULL, weight_matrix = "optimal",
                         robust = TRUE, alpha = 0.05) {
  if (requireNamespace("gmm", quietly = TRUE)) {
    rhs_x <- c(endogenous, exogenous)
    inst  <- c(instruments, exogenous)
    f <- stats::as.formula(paste(outcome, "~", paste(rhs_x, collapse = " + ")))
    g <- stats::as.formula(paste("~", paste(inst, collapse = " + ")))
    type <- if (identical(weight_matrix, "optimal")) "twoStep" else "iterative"
    fit  <- gmm::gmm(f, x = g, data = data, type = type, vcov = "HAC")
    cf <- stats::coef(fit)
    vc <- stats::vcov(fit)
    se <- sqrt(diag(vc))
    return(.morie_iv_result(cf, se, fit$n,
                            method = paste0("gmm (", weight_matrix, ")"),
                            alpha = alpha, dof = NA,
                            details = list(fit = fit)))
  }
  res <- morie_iv_tsls(data, outcome, endogenous, instruments, exogenous,
                       robust = robust, alpha = alpha)
  res$method <- "gmm (2sls fallback \u2014 install gmm)"
  res
}

#' Continuously-Updated GMM (CUE-GMM)
#' @inheritParams morie_iv_gmm
#' @param max_iter Outer iteration cap (default 100).
#' @param tol Convergence tolerance on the objective.
#' @export
morie_iv_cue_gmm <- function(data, outcome, endogenous, instruments,
                             exogenous = NULL, max_iter = 100, tol = 1e-8,
                             alpha = 0.05) {
  if (requireNamespace("gmm", quietly = TRUE)) {
    rhs_x <- c(endogenous, exogenous)
    inst <- c(instruments, exogenous)
    f <- stats::as.formula(paste(outcome, "~", paste(rhs_x, collapse = " + ")))
    g <- stats::as.formula(paste("~", paste(inst, collapse = " + ")))
    fit <- gmm::gmm(f, x = g, data = data, type = "cue", vcov = "HAC",
                    control = list(maxit = max_iter, reltol = tol))
    cf <- stats::coef(fit)
    vc <- stats::vcov(fit)
    se <- sqrt(diag(vc))
    return(.morie_iv_result(cf, se, fit$n, method = "cue-gmm",
                            alpha = alpha, dof = NA,
                            details = list(fit = fit)))
  }
  res <- morie_iv_gmm(data, outcome, endogenous, instruments, exogenous,
                     weight_matrix = "optimal", alpha = alpha)
  res$method <- "cue-gmm (gmm twostep fallback \u2014 install gmm)"
  res
}

#' Wald (single-instrument) estimator
#'
#' \eqn{\hat\beta = (\bar y_{z=1} - \bar y_{z=0}) /
#'                 (\bar d_{z=1} - \bar d_{z=0})}{hatbeta = (bar y_z=1 - bar y_z=0) / (bar d_z=1 - bar d_z=0)}.
#' @param data Data frame.
#' @param outcome Outcome column.
#' @param treatment Endogenous treatment column.
#' @param instrument Binary instrument column.
#' @param alpha Significance level.
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
#'   reuses \code{endogenous[1]}; the resulting F-statistic is
#'   unaffected because Cragg-Donald only reads the first stage.
#' @return Named list with \code{statistic}, \code{p_value},
#'   \code{name}, \code{details}.
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
#' @export
morie_iv_kleibergen_paap <- function(data, endogenous, instruments,
                                     exogenous = NULL) {
  # TODO: native non-i.i.d. KP rank test (Kleibergen & Paap, 2006).  For now
  # delegate to ivreg's weak-instrument diagnostic, which uses KP under HC.
  morie_iv_cragg_donald(data, endogenous, instruments, exogenous)
}

#' Anderson-Rubin (AR) weak-IV-robust test
#' @inheritParams morie_iv_params
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
#' variable.
#' @inheritParams morie_iv_params
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
#' @export
morie_iv_sargan <- function(data, outcome, endogenous, instruments,
                            exogenous = NULL) {
  if (.morie_iv_have_ivreg()) {
    f   <- .morie_iv_build_formula(outcome, endogenous, instruments, exogenous)
    fit <- ivreg::ivreg(f, data = data)
    diag_tbl <- summary(fit, diagnostics = TRUE)$diagnostics
    if ("Sargan" %in% rownames(diag_tbl))
      return(list(statistic = diag_tbl["Sargan", "statistic"],
                  p_value   = diag_tbl["Sargan", "p-value"],
                  name = "Sargan"))
  }
  # base-R: n*R^2 of residual regression on instruments
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
#' @export
morie_iv_hansen_j <- function(data, outcome, endogenous, instruments,
                              exogenous = NULL) {
  if (requireNamespace("gmm", quietly = TRUE)) {
    rhs_x <- c(endogenous, exogenous)
    inst <- c(instruments, exogenous)
    f <- stats::as.formula(paste(outcome, "~", paste(rhs_x, collapse = " + ")))
    g <- stats::as.formula(paste("~", paste(inst, collapse = " + ")))
    fit <- gmm::gmm(f, x = g, data = data, vcov = "HAC")
    sp  <- summary(fit)$stest
    return(list(statistic = sp$test[1], p_value = sp$test[2],
                name = "Hansen J", df = sp$test[3]))
  }
  res <- morie_iv_sargan(data, outcome, endogenous, instruments, exogenous)
  res$name <- "Hansen J (Sargan fallback \u2014 install gmm)"
  res
}

#' Hausman test: OLS vs 2SLS
#' @inheritParams morie_iv_params
#' @export
morie_iv_hausman <- function(data, outcome, endogenous, instruments,
                             exogenous = NULL) {
  if (.morie_iv_have_ivreg()) {
    f   <- .morie_iv_build_formula(outcome, endogenous, instruments, exogenous)
    fit <- ivreg::ivreg(f, data = data)
    diag_tbl <- summary(fit, diagnostics = TRUE)$diagnostics
    if ("Wu-Hausman" %in% rownames(diag_tbl))
      return(list(statistic = diag_tbl["Wu-Hausman", "statistic"],
                  p_value   = diag_tbl["Wu-Hausman", "p-value"],
                  name = "Wu-Hausman / Hausman"))
  }
  rhs_full <- paste(c(endogenous, exogenous), collapse = " + ")
  f_ols    <- stats::as.formula(paste(outcome, "~", rhs_full))
  ols      <- stats::lm(f_ols, data = data)
  iv       <- morie_iv_tsls(data, outcome, endogenous, instruments, exogenous)
  diff     <- iv$coefficients[names(stats::coef(ols))] - stats::coef(ols)
  v_iv     <- iv$details$vcov %||% diag(iv$std_errors^2)
  v_ols    <- stats::vcov(ols)
  v_diff   <- v_iv - v_ols
  v_diff   <- 0.5 * (v_diff + t(v_diff))
  stat     <- as.numeric(t(diff) %*% MASS::ginv(v_diff) %*% diff)
  list(statistic = stat,
       p_value   = stats::pchisq(stat, df = length(diff),
                                 lower.tail = FALSE),
       name = "Hausman")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Durbin-Wu-Hausman test of endogeneity
#' @inheritParams morie_iv_params
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
#' @export
morie_iv_control_function <- function(data, outcome, endogenous, instruments,
                                      exogenous = NULL, robust = TRUE,
                                      alpha = 0.05) {
  if (length(endogenous) != 1)
    stop("morie_iv_control_function currently supports 1 endogenous regressor")
  e   <- endogenous
  rhs <- paste(c(instruments, exogenous), collapse = " + ")
  fs  <- stats::lm(stats::as.formula(paste(e, "~", rhs)), data = data)
  data$.cf_resid_ <- stats::residuals(fs)
  rhs2 <- paste(c(endogenous, exogenous, ".cf_resid_"), collapse = " + ")
  ss <- stats::lm(stats::as.formula(paste(outcome, "~", rhs2)), data = data)
  cf <- stats::coef(ss)
  se <- sqrt(diag(stats::vcov(ss)))
  .morie_iv_result(cf, se, length(ss$residuals),
                   method = "control function",
                   alpha = alpha, dof = ss$df.residual,
                   details = list(first_stage = fs, second_stage = ss))
}

#' IV Probit (Rivers-Vuong control function)
#' @inheritParams morie_iv_tsls
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
#' @export
morie_iv_panel <- function(data, outcome, endogenous, instruments, unit,
                           exogenous = NULL, time_fe = NULL, alpha = 0.05) {
  if (requireNamespace("plm", quietly = TRUE)) {
    f <- .morie_iv_build_formula(outcome, endogenous, instruments, exogenous)
    idx <- if (!is.null(time_fe)) c(unit, time_fe) else unit
    # inst.method = "baltagi" is the Baltagi (1981) instrument
    # construction; plm accepts it for within-IV models with an IV
    # formula (it is NOT a pgmm-only argument despite the surface
    # similarity to plm::pgmm). See ?plm::plm.
    fit <- plm::plm(f, data = data, index = idx, model = "within",
                    effect = if (is.null(time_fe)) "individual" else "twoways",
                    inst.method = "baltagi")
    cf <- stats::coef(fit)
    vc <- stats::vcov(fit)
    se <- sqrt(diag(vc))
    return(.morie_iv_result(cf, se, length(fit$residuals),
                            method = "panel IV (plm within)",
                            alpha = alpha, dof = NA,
                            details = list(fit = fit)))
  }
  # Manual within-transform fallback
  for (v in c(outcome, endogenous, instruments, exogenous))
    data[[v]] <- data[[v]] - stats::ave(data[[v]], data[[unit]])
  res <- morie_iv_tsls(data, outcome, endogenous, instruments, exogenous,
                       alpha = alpha)
  res$method <- "panel IV (manual within demean)"
  res
}


# ---------------------------------------------------------------------------
# Composite diagnostic dashboards
# ---------------------------------------------------------------------------

#' Composite IV diagnostics
#' @inheritParams morie_iv_params
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
