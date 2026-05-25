# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.

#' Treatment effect estimators (ATE, LATE, G-computation, sensitivity)
#'
#' Provides:
#' \itemize{
#'   \item \code{estimate_ate()} — IPW-weighted OLS ATE.
#'   \item \code{estimate_plr()} — Partially Linear Regression via
#'     DoubleML.jl-style cross-fitting (uses \pkg{DoubleML} if
#'     installed; otherwise base R cross-fit ridge fallback).
#'   \item \code{estimate_pliv()} — Partially Linear IV (LATE) via
#'     DoubleML or 2SLS fallback.
#'   \item \code{estimate_ate_gcomputation()} — G-computation
#'     (outcome-regression / standardisation) ATE with bootstrap SE.
#'   \item \code{sensitivity_rosenbaum()} — Rosenbaum bounds for hidden
#'     confounding (wraps \pkg{rbounds} when available, else base R).
#'   \item \code{e_value()} — VanderWeele-Ding E-value (wraps
#'     \pkg{EValue} when available, else base R).
#' }
#'
#' @references
#' Chernozhukov et al. (2018); Robins (1986); VanderWeele & Ding
#' (2017); Rosenbaum (2002).
#' @name effects
NULL


# -- IPW-weighted ATE -------------------------------------------------

#' IPW-weighted OLS ATE
#'
#' @param data        Data frame containing the analytical sample.
#' @param outcome     Name of the outcome column.
#' @param treatment   Name of the binary treatment column.
#' @param weights_col Name of the weights column (e.g. IPTW).
#' @return Named list with `ate` and `se` (HC3-robust).
#' @export
estimate_ate <- function(data, outcome, treatment, weights_col) {
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    warning("sandwich not installed; SE will be naive model SE.")
  }
  fml <- stats::as.formula(paste(outcome, "~", treatment))
  w   <- data[[weights_col]]
  fit <- stats::lm(fml, data = data, weights = w)
  cf  <- summary(fit)$coefficients
  if (requireNamespace("sandwich", quietly = TRUE) &&
      requireNamespace("lmtest", quietly = TRUE)) {
    # HC3 robust SE -- mirror statsmodels.wls(...).fit(cov_type='HC3').
    vc  <- sandwich::vcovHC(fit, type = "HC3")
    ct  <- lmtest::coeftest(fit, vcov. = vc)
    list(ate = as.numeric(ct[treatment, "Estimate"]),
         se  = as.numeric(ct[treatment, "Std. Error"]))
  } else {
    list(ate = as.numeric(cf[treatment, "Estimate"]),
         se  = as.numeric(cf[treatment, "Std. Error"]))
  }
}


# -- Partially Linear Regression (DoubleML PLR) -----------------------

#' Partially Linear Regression (PLR) ATE
#'
#' Wraps \pkg{DoubleML} when available. Without DoubleML, falls back
#' to a hand-rolled cross-fitting estimator using ridge regression
#' (\pkg{glmnet}) or, last-ditch, OLS partialling out.
#'
#' @param data        Data frame with all required columns.
#' @param treatment   Column name of the treatment variable.
#' @param outcome     Column name of the outcome variable.
#' @param covariates  Character vector of covariate column names.
#' @param n_folds     Cross-fitting folds. Default 5.
#' @param random_state RNG seed. Default 42.
#' @return Named list with `ate`, `se`, `ci_lower`, `ci_upper`,
#'   `pval`, `n_obs`, `method`.
#' @export
estimate_plr <- function(data, treatment, outcome, covariates,
                           n_folds = 5L, random_state = 42L) {
  required_cols <- c(treatment, outcome, covariates)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols))
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "))
  if (n_folds < 2L)
    stop("n_folds must be >= 2, got ", n_folds)

  df <- stats::na.omit(data[, c(treatment, outcome, covariates),
                             drop = FALSE])
  n_obs <- nrow(df)

  if (requireNamespace("DoubleML", quietly = TRUE) &&
      requireNamespace("mlr3learners", quietly = TRUE) &&
      requireNamespace("mlr3", quietly = TRUE)) {
    # DoubleML path. The R DoubleML package expects an mlr3 learner.
    dml_data <- DoubleML::DoubleMLData$new(
      data = df, y_col = outcome, d_cols = treatment,
      x_cols = covariates
    )
    ml_l <- mlr3::lrn("regr.cv_glmnet", s = "lambda.min")
    ml_m <- mlr3::lrn("regr.cv_glmnet", s = "lambda.min")
    plr  <- DoubleML::DoubleMLPLR$new(
      data = dml_data, ml_l = ml_l, ml_m = ml_m,
      n_folds = n_folds, n_rep = 1L
    )
    set.seed(random_state)
    plr$fit()
    ci <- plr$confint(level = 0.95)
    return(list(
      ate      = as.numeric(plr$coef[[1]]),
      se       = as.numeric(plr$se[[1]]),
      ci_lower = as.numeric(ci[1, 1]),
      ci_upper = as.numeric(ci[1, 2]),
      pval     = as.numeric(plr$pval[[1]]),
      n_obs    = n_obs,
      method   = "DoubleML PLR"
    ))
  }

  # Base-R cross-fitting fallback: ridge or OLS partialling-out.
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n_obs))
  d <- as.numeric(df[[treatment]])
  y <- as.numeric(df[[outcome]])
  X <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(X) <- "double"

  fit_predict <- function(X_train, z_train, X_test) {
    if (requireNamespace("glmnet", quietly = TRUE)) {
      fit <- glmnet::cv.glmnet(X_train, z_train, alpha = 0)
      as.numeric(stats::predict(fit, newx = X_test, s = "lambda.min"))
    } else {
      df_tr <- as.data.frame(X_train)
      df_tr$.z <- z_train
      fit <- stats::lm(.z ~ ., data = df_tr)
      as.numeric(stats::predict(fit, newdata = as.data.frame(X_test)))
    }
  }

  y_hat <- numeric(n_obs)
  d_hat <- numeric(n_obs)
  for (k in seq_len(n_folds)) {
    train_idx <- which(folds != k)
    test_idx  <- which(folds == k)
    y_hat[test_idx] <- fit_predict(X[train_idx, , drop = FALSE],
                                     y[train_idx],
                                     X[test_idx,  , drop = FALSE])
    d_hat[test_idx] <- fit_predict(X[train_idx, , drop = FALSE],
                                     d[train_idx],
                                     X[test_idx,  , drop = FALSE])
  }
  d_resid <- d - d_hat
  y_resid <- y - y_hat
  ate <- sum(d_resid * y_resid) / sum(d_resid * d_resid)
  # Robust SE via residual orthogonality score (Chernozhukov et al.).
  psi <- (y_resid - ate * d_resid) * d_resid
  J0  <- mean(d_resid * d_resid)
  var_ate <- mean(psi^2) / (J0^2 * n_obs)
  se <- sqrt(var_ate)
  z  <- qnorm(0.975)
  list(
    ate      = ate,
    se       = se,
    ci_lower = ate - z * se,
    ci_upper = ate + z * se,
    pval     = 2 * (1 - pnorm(abs(ate / se))),
    n_obs    = n_obs,
    method   = "cross-fit ridge (DoubleML not installed)"
  )
}


# -- Partially Linear IV (LATE) ---------------------------------------

#' Partially Linear IV (PLIV) / Local Average Treatment Effect
#'
#' Wraps \pkg{DoubleML} when available. Otherwise falls back to 2SLS:
#' first stage `D ~ Z + X`, second stage `Y ~ D_hat + X`, base R OLS.
#'
#' @param data        Data frame with all required columns.
#' @param treatment   Endogenous treatment column name.
#' @param outcome     Outcome column name.
#' @param instrument  Instrument column name.
#' @param covariates  Exogenous covariate column names.
#' @param n_folds     Cross-fitting folds (DoubleML path). Default 5.
#' @param random_state RNG seed. Default 42.
#' @return Named list with `late`, `se`, `ci_lower`, `ci_upper`,
#'   `pval`, `n_obs`, `method`.
#' @export
estimate_pliv <- function(data, treatment, outcome, instrument,
                            covariates, n_folds = 5L,
                            random_state = 42L) {
  required_cols <- c(treatment, outcome, instrument, covariates)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols))
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "))

  df <- stats::na.omit(data[, c(treatment, outcome, instrument,
                                  covariates), drop = FALSE])
  n_obs <- nrow(df)

  if (requireNamespace("DoubleML", quietly = TRUE) &&
      requireNamespace("mlr3learners", quietly = TRUE) &&
      requireNamespace("mlr3", quietly = TRUE)) {
    dml_data <- DoubleML::DoubleMLData$new(
      data = df, y_col = outcome, d_cols = treatment,
      z_cols = instrument, x_cols = covariates
    )
    ml_l <- mlr3::lrn("regr.cv_glmnet", s = "lambda.min")
    ml_m <- mlr3::lrn("regr.cv_glmnet", s = "lambda.min")
    ml_r <- mlr3::lrn("regr.cv_glmnet", s = "lambda.min")
    pliv <- DoubleML::DoubleMLPLIV$new(
      data = dml_data, ml_l = ml_l, ml_m = ml_m, ml_r = ml_r,
      n_folds = n_folds, n_rep = 1L
    )
    set.seed(random_state)
    pliv$fit()
    ci <- pliv$confint(level = 0.95)
    return(list(
      late     = as.numeric(pliv$coef[[1]]),
      se       = as.numeric(pliv$se[[1]]),
      ci_lower = as.numeric(ci[1, 1]),
      ci_upper = as.numeric(ci[1, 2]),
      pval     = as.numeric(pliv$pval[[1]]),
      n_obs    = n_obs,
      method   = "DoubleML PLIV"
    ))
  }

  # 2SLS fallback (statsmodels.OLS first-stage + second-stage).
  warning("DoubleML not available; falling back to 2SLS.",
           call. = FALSE)
  X_first  <- as.data.frame(df[, c(covariates, instrument),
                                drop = FALSE])
  first_fit <- stats::lm(stats::as.formula(
    paste0(treatment, " ~ .")), data = cbind(X_first,
                                                setNames(list(df[[treatment]]),
                                                          treatment)))
  d_hat <- stats::fitted(first_fit)
  X_sec  <- data.frame(d_hat = d_hat, df[, covariates, drop = FALSE])
  X_sec[[outcome]] <- df[[outcome]]
  sec_fit <- stats::lm(stats::as.formula(paste0(outcome, " ~ .")),
                        data = X_sec)
  cf <- summary(sec_fit)$coefficients
  ci <- stats::confint(sec_fit, level = 0.95)
  list(
    late     = as.numeric(cf["d_hat", "Estimate"]),
    se       = as.numeric(cf["d_hat", "Std. Error"]),
    ci_lower = as.numeric(ci["d_hat", 1]),
    ci_upper = as.numeric(ci["d_hat", 2]),
    pval     = as.numeric(cf["d_hat", "Pr(>|t|)"]),
    n_obs    = n_obs,
    method   = "2SLS (base R fallback)"
  )
}


# -- G-computation (outcome regression / standardisation) -----------

#' G-computation ATE with bootstrap SE
#'
#' Fits the outcome model, predicts counterfactuals under T=1 and T=0,
#' averages the difference. Bootstrap SE uses 500 resamples.
#'
#' @param data         Data frame with all required columns.
#' @param treatment    Binary treatment column (0/1).
#' @param outcome      Outcome column.
#' @param covariates   Character vector of covariates.
#' @param outcome_model `"linear"` (OLS) or `"logistic"` (logit GLM).
#' @return Named list with `ate`, `se`, `ci_lower`, `ci_upper`,
#'   `n_obs`, `outcome_model`.
#' @export
estimate_ate_gcomputation <- function(data, treatment, outcome,
                                         covariates,
                                         outcome_model = "linear") {
  valid_models <- c("linear", "logistic")
  if (!outcome_model %in% valid_models)
    stop("outcome_model must be one of: ",
         paste(valid_models, collapse = ", "))
  required_cols <- c(treatment, outcome, covariates)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols))
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "))
  df <- stats::na.omit(data[, c(treatment, outcome, covariates),
                             drop = FALSE])
  n_obs <- nrow(df)
  if (n_obs < 10L)
    stop("G-computation requires at least 10 complete observations.")

  feature_cols <- c(treatment, covariates)

  # Centre+scale to mirror sklearn's StandardScaler behaviour.
  fit_and_predict_ate <- function(boot_df) {
    means <- colMeans(boot_df[, feature_cols, drop = FALSE])
    sds   <- apply(boot_df[, feature_cols, drop = FALSE], 2, sd)
    sds[sds == 0] <- 1
    Xs <- sweep(sweep(boot_df[, feature_cols, drop = FALSE], 2,
                       means, "-"), 2, sds, "/")
    df_fit <- as.data.frame(Xs)
    df_fit[[outcome]] <- boot_df[[outcome]]
    if (outcome_model == "linear") {
      mod <- stats::lm(stats::as.formula(paste0(outcome, " ~ .")),
                        data = df_fit)
    } else {
      mod <- stats::glm(stats::as.formula(paste0(outcome, " ~ .")),
                          data = df_fit, family = stats::binomial())
    }
    X_t1 <- boot_df[, feature_cols, drop = FALSE]
    X_t1[[treatment]] <- 1
    X_t0 <- boot_df[, feature_cols, drop = FALSE]
    X_t0[[treatment]] <- 0
    X_t1_s <- as.data.frame(sweep(sweep(X_t1, 2, means, "-"),
                                    2, sds, "/"))
    X_t0_s <- as.data.frame(sweep(sweep(X_t0, 2, means, "-"),
                                    2, sds, "/"))
    if (outcome_model == "linear") {
      y1_hat <- stats::predict(mod, newdata = X_t1_s)
      y0_hat <- stats::predict(mod, newdata = X_t0_s)
    } else {
      y1_hat <- stats::predict(mod, newdata = X_t1_s, type = "response")
      y0_hat <- stats::predict(mod, newdata = X_t0_s, type = "response")
    }
    mean(y1_hat - y0_hat)
  }

  ate <- fit_and_predict_ate(df)
  set.seed(42)
  boot_ates <- rep(NA_real_, 500L)
  for (b in seq_len(500L)) {
    idx <- sample.int(n_obs, n_obs, replace = TRUE)
    boot_ates[b] <- tryCatch(
      fit_and_predict_ate(df[idx, , drop = FALSE]),
      error = function(e) NA_real_
    )
  }
  boot_ates <- boot_ates[is.finite(boot_ates)]
  if (length(boot_ates) < 50L) {
    warning("Fewer than 50 successful bootstrap iterations; ",
             "SE may be unreliable.", call. = FALSE)
  }
  se <- if (length(boot_ates) > 1L) sd(boot_ates) else NA_real_
  ci_lo <- if (length(boot_ates)) as.numeric(
    quantile(boot_ates, 0.025)) else NA_real_
  ci_hi <- if (length(boot_ates)) as.numeric(
    quantile(boot_ates, 0.975)) else NA_real_
  list(ate = ate, se = se, ci_lower = ci_lo, ci_upper = ci_hi,
       n_obs = n_obs, outcome_model = outcome_model)
}


# -- Rosenbaum bounds (data-frame interface) --------------------------

#' Rosenbaum bounds sensitivity analysis (data-frame interface)
#'
#' Wraps \pkg{rbounds} when available; otherwise computes normal-
#' approximation Wilcoxon signed-rank bounds in base R.
#'
#' @param data       Data frame with treatment + outcome columns.
#' @param treatment  Binary treatment column (0/1).
#' @param outcome    Outcome column.
#' @param covariates Covariates (used only for matching approximation,
#'                   here a simple rank-match).
#' @param gamma_range c(min, max) of Gamma. Default c(1, 3).
#' @param n_gamma    Number of Gamma values. Default 20.
#' @return Data frame with `Gamma`, `p_lower`, `p_upper`.
#' @export
sensitivity_rosenbaum <- function(data, treatment, outcome,
                                     covariates,
                                     gamma_range = c(1, 3),
                                     n_gamma = 20L) {
  required_cols <- c(treatment, outcome)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols))
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "))
  if (gamma_range[1] < 1)
    stop("Minimum Gamma must be >= 1.0, got ", gamma_range[1])
  if (gamma_range[2] <= gamma_range[1])
    stop("gamma_range[2] must be > gamma_range[1].")
  if (n_gamma < 2L)
    stop("n_gamma must be >= 2, got ", n_gamma)

  df <- stats::na.omit(data[, c(treatment, outcome), drop = FALSE])
  treated <- df[df[[treatment]] == 1, outcome]
  control <- df[df[[treatment]] == 0, outcome]
  min_n <- min(length(treated), length(control))
  if (min_n < 2L)
    stop("At least 2 treated and 2 control units required.")

  treated_sorted <- sort(treated)[seq_len(min_n)]
  control_sorted <- sort(control)[seq_len(min_n)]
  differences    <- treated_sorted - control_sorted

  n_pairs  <- length(differences)
  abs_diff <- abs(differences)
  ranks    <- rank(abs_diff)
  T_plus   <- sum(ranks[differences > 0])

  gammas <- seq(gamma_range[1], gamma_range[2], length.out = n_gamma)
  results <- vector("list", n_gamma)
  for (i in seq_along(gammas)) {
    gamma <- gammas[i]
    p_max <- gamma / (1 + gamma)
    p_min <- 1 / (1 + gamma)
    mu_u  <- n_pairs * (n_pairs + 1) / 2 * p_max
    var_u <- n_pairs * (n_pairs + 1) * (2 * n_pairs + 1) / 6 *
             p_max * (1 - p_max)
    mu_l  <- n_pairs * (n_pairs + 1) / 2 * p_min
    var_l <- n_pairs * (n_pairs + 1) * (2 * n_pairs + 1) / 6 *
             p_min * (1 - p_min)
    p_upper <- if (var_u > 0) {
      2 * stats::pnorm(abs((T_plus - mu_u) / sqrt(var_u)),
                         lower.tail = FALSE)
    } else NA_real_
    p_lower <- if (var_l > 0) {
      2 * stats::pnorm(abs((T_plus - mu_l) / sqrt(var_l)),
                         lower.tail = FALSE)
    } else NA_real_
    results[[i]] <- data.frame(Gamma = gamma,
                                p_lower = p_lower,
                                p_upper = p_upper)
  }
  do.call(rbind, results)
}


# -- E-value (continuous-ATE flavour) ---------------------------------

#' E-value for unmeasured confounding (continuous-ATE scale)
#'
#' Wraps \pkg{EValue} when available. Otherwise applies the same
#' continuous-scale z-stat -> RR approximation as the Python port.
#'
#' @param ate  Point estimate of the treatment effect.
#' @param se   Standard error of the ATE (must be > 0).
#' @param null Null value. Default 0.
#' @return Scalar E-value (>= 1).
#' @export
e_value <- function(ate, se, null = 0) {
  if (se <= 0) stop("se must be > 0, got ", se)
  z <- abs(ate - null) / se
  if (z == 0) return(1)
  # Pre-2026-05-22, this also tried EValue::evalues.OLS with a hardcoded
  # sd_y=1 (assumed standardised outcome). That diverged from Python's
  # exp(z) proxy: same input, three different paths (R-with-EValue ≠
  # R-without ≠ Python). Removed; both ports now use the closed-form
  # VanderWeele-Ding E-value for the continuous-scale RR proxy.
  # Users who want the EValue OLS path with a real sd_y should call
  # EValue::evalues.OLS directly.
  rr <- exp(z)
  if (rr <= 1) return(1)
  rr + sqrt(rr * (rr - 1))
}
