# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.

#' Treatment effect estimators and marginal-effects extenders
#'
#' Two function families live here:
#'
#' \strong{Treatment-effect estimators (legacy)}
#' \itemize{
#'   \item \code{estimate_ate()} -- IPW-weighted OLS ATE.
#'   \item \code{estimate_plr()} -- Partially Linear Regression via
#'     \pkg{DoubleML}; falls back to base R cross-fit ridge.
#'   \item \code{estimate_pliv()} -- Partially Linear IV (LATE) via
#'     \pkg{DoubleML}; falls back to 2SLS.
#'   \item \code{estimate_ate_gcomputation()} -- G-computation ATE.
#'     Thin wrapper over \code{stdReg::stdGlm()} when installed; falls
#'     back to inline bootstrap standardisation otherwise.
#'   \item \code{sensitivity_rosenbaum()} -- Rosenbaum bounds. Thin
#'     wrapper over \code{rbounds::psens()} when installed; otherwise
#'     normal-approximation Wilcoxon signed-rank bounds.
#'   \item \code{e_value()} -- VanderWeele-Ding E-value. Thin wrapper
#'     over \code{EValue::evalues.OLS()} when installed; otherwise the
#'     closed-form continuous-scale RR proxy.
#' }
#'
#' \strong{Marginal-effects extenders} (Phase 1.j additions; thin
#' wrappers over Vincent Arel-Bundock's universal API and the
#' \pkg{emmeans} / \pkg{broom} ecosystems):
#' \itemize{
#'   \item \code{morie_effects_emmeans()} -> \code{emmeans::emmeans()}.
#'   \item \code{morie_effects_predictions()} ->
#'     \code{marginaleffects::predictions()}.
#'   \item \code{morie_effects_comparisons()} ->
#'     \code{marginaleffects::comparisons()}.
#'   \item \code{morie_effects_slopes()} ->
#'     \code{marginaleffects::slopes()}.
#'   \item \code{morie_effects_tidy()} -> \code{broom::tidy()} (falls
#'     back to a \code{summary()}-based tidy frame when \pkg{broom}
#'     is unavailable).
#' }
#'
#' Each extender requires the underlying CRAN package and signals a
#' clean \code{stop()} when it is missing, leaving the upstream model
#' object untouched. They return the underlying package's native
#' object verbatim so downstream code (e.g. \pkg{ggplot2} plumbing,
#' rmorie's MRM step) keeps working with the canonical API.
#'
#' @references
#' Chernozhukov et al. (2018); Robins (1986); VanderWeele & Ding
#' (2017); Rosenbaum (2002); Arel-Bundock (2024,
#' \pkg{marginaleffects}); Lenth (2024, \pkg{emmeans});
#' Robinson, Hayes & Couch (2024, \pkg{broom}).
#' @name effects
NULL


# -- IPW-weighted ATE -------------------------------------------------

#' IPW-weighted OLS ATE
#'
#' Thin wrapper over a weighted \code{stats::lm()} plus HC3 robust SEs
#' from \pkg{sandwich} + \pkg{lmtest} when installed. Note: this is the
#' legacy shape used by older MRM pipelines; new code should prefer
#' \code{morie_estimate_ate()} (in \code{R/causal.R}) for the richer
#' \code{morie_te_result} return shape.
#'
#' @param data        Data frame containing the analytical sample.
#' @param outcome     Name of the outcome column.
#' @param treatment   Name of the binary treatment column.
#' @param weights_col Name of the weights column (e.g. IPTW).
#' @return Named list with `ate` and `se` (HC3-robust when available).
#' @export
estimate_ate <- function(data, outcome, treatment, weights_col) {
  fml <- stats::as.formula(paste(outcome, "~", treatment))
  w   <- data[[weights_col]]
  fit <- stats::lm(fml, data = data, weights = w)
  if (requireNamespace("sandwich", quietly = TRUE) &&
        requireNamespace("lmtest", quietly = TRUE)) {
    vc <- sandwich::vcovHC(fit, type = "HC3")
    ct <- lmtest::coeftest(fit, vcov. = vc)
    return(list(
      ate = as.numeric(ct[treatment, "Estimate"]),
      se  = as.numeric(ct[treatment, "Std. Error"])
    ))
  }
  warning("sandwich/lmtest not installed; SE will be naive model SE.",
          call. = FALSE)
  cf <- summary(fit)$coefficients
  list(
    ate = as.numeric(cf[treatment, "Estimate"]),
    se  = as.numeric(cf[treatment, "Std. Error"])
  )
}


# -- Partially Linear Regression (DoubleML PLR) -----------------------

#' Partially Linear Regression (PLR) ATE
#'
#' Wraps \pkg{DoubleML} (+ \pkg{mlr3} / \pkg{mlr3learners}) when
#' available. Without DoubleML, falls back to a hand-rolled
#' cross-fitting estimator using ridge regression (\pkg{glmnet}) or,
#' last-ditch, OLS partialling out.
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
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "))
  }
  if (n_folds < 2L) {
    stop("n_folds must be >= 2, got ", n_folds)
  }

  df    <- stats::na.omit(data[, c(treatment, outcome, covariates),
                               drop = FALSE])
  n_obs <- nrow(df)

  if (requireNamespace("DoubleML", quietly = TRUE) &&
        requireNamespace("mlr3learners", quietly = TRUE) &&
        requireNamespace("mlr3", quietly = TRUE)) {
    # Attempt the DoubleML path; on ANY runtime failure (e.g. an mlr3/future
    # backend launch error seen on some R-devel builds) fall through to the
    # base-R cross-fit below rather than propagating -- this is the documented
    # fallback behaviour, previously only reached when DoubleML was absent.
    # future (via mlr3/DoubleML) runs a connection-misuse check on each resolve
    # that segfaults R uncatchably on some oldrel builds (so the tryCatch below
    # cannot save it). Disable that diagnostic for this call only; restore after.
    .morie_old_fut <- options(future.connections.onMisuse = "ignore")
    on.exit(options(.morie_old_fut), add = TRUE)
    dml_res <- tryCatch({
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
      # Silence {future}'s false RNG-misuse warning: cross-fitting is seeded
      # via set.seed(random_state) above, so it is a false alarm that would
      # otherwise trip R CMD check stop_on_warning. Restored on exit.
      .op <- options(future.rng.onMisuse = "ignore")
      on.exit(options(.op), add = TRUE)
      plr$fit()
      ci <- plr$confint(level = 0.95)
      list(
        ate      = as.numeric(plr$coef[[1]]),
        se       = as.numeric(plr$se[[1]]),
        ci_lower = as.numeric(ci[1, 1]),
        ci_upper = as.numeric(ci[1, 2]),
        pval     = as.numeric(plr$pval[[1]]),
        n_obs    = n_obs,
        method   = "DoubleML PLR"
      )
    }, error = function(e) NULL)
    if (!is.null(dml_res)) return(dml_res)
  }

  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n_obs))
  d <- as.numeric(df[[treatment]])
  y <- as.numeric(df[[outcome]])
  x_mat <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(x_mat) <- "double"

  fit_predict <- function(x_train, z_train, x_test) {
    if (requireNamespace("glmnet", quietly = TRUE)) {
      fit <- glmnet::cv.glmnet(x_train, z_train, alpha = 0)
      as.numeric(stats::predict(fit, newx = x_test, s = "lambda.min"))
    } else {
      df_tr <- as.data.frame(x_train)
      df_tr$.z <- z_train
      fit <- stats::lm(.z ~ ., data = df_tr)
      as.numeric(stats::predict(fit, newdata = as.data.frame(x_test)))
    }
  }

  y_hat <- numeric(n_obs)
  d_hat <- numeric(n_obs)
  for (k in seq_len(n_folds)) {
    train_idx <- which(folds != k)
    test_idx  <- which(folds == k)
    y_hat[test_idx] <- fit_predict(x_mat[train_idx, , drop = FALSE],
                                   y[train_idx],
                                   x_mat[test_idx, , drop = FALSE])
    d_hat[test_idx] <- fit_predict(x_mat[train_idx, , drop = FALSE],
                                   d[train_idx],
                                   x_mat[test_idx, , drop = FALSE])
  }
  d_resid <- d - d_hat
  y_resid <- y - y_hat
  ate     <- sum(d_resid * y_resid) / sum(d_resid * d_resid)
  psi     <- (y_resid - ate * d_resid) * d_resid
  j0      <- mean(d_resid * d_resid)
  var_ate <- mean(psi^2) / (j0^2 * n_obs)
  se      <- sqrt(var_ate)
  z       <- stats::qnorm(0.975)
  list(
    ate      = ate,
    se       = se,
    ci_lower = ate - z * se,
    ci_upper = ate + z * se,
    pval     = 2 * (1 - stats::pnorm(abs(ate / se))),
    n_obs    = n_obs,
    method   = "cross-fit ridge (base R fallback)"
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
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "))
  }

  df    <- stats::na.omit(data[, c(treatment, outcome, instrument,
                                   covariates), drop = FALSE])
  n_obs <- nrow(df)

  if (requireNamespace("DoubleML", quietly = TRUE) &&
        requireNamespace("mlr3learners", quietly = TRUE) &&
        requireNamespace("mlr3", quietly = TRUE)) {
    # Attempt the DoubleML path; on ANY runtime failure (e.g. an mlr3/future
    # backend launch error seen on some R-devel builds) fall through to the
    # 2SLS base-R fallback below rather than propagating.
    # future (via mlr3/DoubleML) runs a connection-misuse check on each resolve
    # that segfaults R uncatchably on some oldrel builds (so the tryCatch below
    # cannot save it). Disable that diagnostic for this call only; restore after.
    .morie_old_fut <- options(future.connections.onMisuse = "ignore")
    on.exit(options(.morie_old_fut), add = TRUE)
    dml_res <- tryCatch({
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
      # Silence {future}'s false RNG-misuse warning: cross-fitting is seeded
      # via set.seed(random_state) above, so it is a false alarm that would
      # otherwise trip R CMD check stop_on_warning. Restored on exit.
      .op <- options(future.rng.onMisuse = "ignore")
      on.exit(options(.op), add = TRUE)
      pliv$fit()
      ci <- pliv$confint(level = 0.95)
      list(
        late     = as.numeric(pliv$coef[[1]]),
        se       = as.numeric(pliv$se[[1]]),
        ci_lower = as.numeric(ci[1, 1]),
        ci_upper = as.numeric(ci[1, 2]),
        pval     = as.numeric(pliv$pval[[1]]),
        n_obs    = n_obs,
        method   = "DoubleML PLIV"
      )
    }, error = function(e) NULL)
    if (!is.null(dml_res)) return(dml_res)
  }

  warning("DoubleML path failed or unavailable; falling back to 2SLS.",
          call. = FALSE)
  x_first  <- as.data.frame(df[, c(covariates, instrument),
                               drop = FALSE])
  first_fit <- stats::lm(
    stats::as.formula(paste0(treatment, " ~ .")),
    data = cbind(
      x_first,
      stats::setNames(list(df[[treatment]]), treatment)
    )
  )
  d_hat  <- stats::fitted(first_fit)
  x_sec  <- data.frame(d_hat = d_hat,
                       df[, covariates, drop = FALSE])
  x_sec[[outcome]] <- df[[outcome]]
  sec_fit <- stats::lm(stats::as.formula(paste0(outcome, " ~ .")),
                       data = x_sec)
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


# -- G-computation (outcome regression / standardisation) -------------

#' G-computation ATE with bootstrap SE
#'
#' Thin wrapper over \code{stdReg::stdGlm()} (Sjolander's
#' regression-standardisation back end) when \pkg{stdReg} is
#' installed. Without \pkg{stdReg}, falls back to an inline outcome-
#' regression + bootstrap implementation (500 resamples, seed 42)
#' that mirrors the legacy rmorie behaviour.
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
  if (!outcome_model %in% valid_models) {
    stop("outcome_model must be one of: ",
         paste(valid_models, collapse = ", "))
  }
  required_cols <- c(treatment, outcome, covariates)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "))
  }
  df    <- stats::na.omit(data[, c(treatment, outcome, covariates),
                               drop = FALSE])
  n_obs <- nrow(df)
  if (n_obs < 10L) {
    stop("G-computation requires at least 10 complete observations.")
  }

  if (requireNamespace("stdReg", quietly = TRUE)) {
    fml <- stats::as.formula(
      paste0(outcome, " ~ ",
             paste(c(treatment, covariates), collapse = " + "))
    )
    fam <- if (outcome_model == "linear") {
      stats::gaussian()
    } else {
      stats::binomial()
    }
    mod    <- stats::glm(fml, data = df, family = fam)
    fitobj <- stdReg::stdGlm(fit = mod, data = df, X = treatment,
                             x = c(0, 1))
    sm     <- summary(fitobj, contrast = "difference",
                      reference = 0, CI.level = 0.95)
    est <- sm$est.table
    return(list(
      ate           = as.numeric(est[2, "Estimate"]),
      se            = as.numeric(est[2, "Std. Error"]),
      ci_lower      = as.numeric(est[2, "lower 0.95"]),
      ci_upper      = as.numeric(est[2, "upper 0.95"]),
      n_obs         = n_obs,
      outcome_model = outcome_model,
      method        = "stdReg::stdGlm"
    ))
  }

  feature_cols <- c(treatment, covariates)
  fit_and_predict_ate <- function(boot_df) {
    means <- colMeans(boot_df[, feature_cols, drop = FALSE])
    sds   <- apply(boot_df[, feature_cols, drop = FALSE], 2, stats::sd)
    sds[sds == 0] <- 1
    xs    <- sweep(sweep(boot_df[, feature_cols, drop = FALSE], 2,
                         means, "-"), 2, sds, "/")
    df_fit <- as.data.frame(xs)
    df_fit[[outcome]] <- boot_df[[outcome]]
    if (outcome_model == "linear") {
      mod <- stats::lm(stats::as.formula(paste0(outcome, " ~ .")),
                       data = df_fit)
    } else {
      mod <- stats::glm(stats::as.formula(paste0(outcome, " ~ .")),
                        data = df_fit, family = stats::binomial())
    }
    x_t1 <- boot_df[, feature_cols, drop = FALSE]
    x_t1[[treatment]] <- 1
    x_t0 <- boot_df[, feature_cols, drop = FALSE]
    x_t0[[treatment]] <- 0
    x_t1_s <- as.data.frame(sweep(sweep(x_t1, 2, means, "-"),
                                  2, sds, "/"))
    x_t0_s <- as.data.frame(sweep(sweep(x_t0, 2, means, "-"),
                                  2, sds, "/"))
    if (outcome_model == "linear") {
      y1_hat <- stats::predict(mod, newdata = x_t1_s)
      y0_hat <- stats::predict(mod, newdata = x_t0_s)
    } else {
      y1_hat <- stats::predict(mod, newdata = x_t1_s,
                               type = "response")
      y0_hat <- stats::predict(mod, newdata = x_t0_s,
                               type = "response")
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
  se   <- if (length(boot_ates) > 1L) stats::sd(boot_ates) else NA_real_
  ci_lo <- if (length(boot_ates)) {
    as.numeric(stats::quantile(boot_ates, 0.025))
  } else {
    NA_real_
  }
  ci_hi <- if (length(boot_ates)) {
    as.numeric(stats::quantile(boot_ates, 0.975))
  } else {
    NA_real_
  }
  list(
    ate           = ate,
    se            = se,
    ci_lower      = ci_lo,
    ci_upper      = ci_hi,
    n_obs         = n_obs,
    outcome_model = outcome_model,
    method        = "inline bootstrap (stdReg not installed)"
  )
}


# -- Rosenbaum bounds (data-frame interface) --------------------------

#' Rosenbaum bounds sensitivity analysis (data-frame interface)
#'
#' Thin wrapper over \code{rbounds::psens()} when \pkg{rbounds} is
#' installed (rank-matched-pair signed-rank bounds across a Gamma
#' grid). Without \pkg{rbounds}, falls back to a base R normal-
#' approximation Wilcoxon signed-rank computation.
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
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "))
  }
  if (gamma_range[1] < 1) {
    stop("Minimum Gamma must be >= 1.0, got ", gamma_range[1])
  }
  if (gamma_range[2] <= gamma_range[1]) {
    stop("gamma_range[2] must be > gamma_range[1].")
  }
  if (n_gamma < 2L) {
    stop("n_gamma must be >= 2, got ", n_gamma)
  }

  df      <- stats::na.omit(data[, c(treatment, outcome),
                                 drop = FALSE])
  treated <- df[df[[treatment]] == 1, outcome]
  control <- df[df[[treatment]] == 0, outcome]
  min_n   <- min(length(treated), length(control))
  if (min_n < 2L) {
    stop("At least 2 treated and 2 control units required.")
  }

  treated_sorted <- sort(treated)[seq_len(min_n)]
  control_sorted <- sort(control)[seq_len(min_n)]
  differences    <- treated_sorted - control_sorted
  gammas         <- seq(gamma_range[1], gamma_range[2],
                        length.out = n_gamma)

  if (requireNamespace("rbounds", quietly = TRUE) && min_n >= 5L) {
    # psens() computes upper/lower p-bounds across a Gamma sequence.
    res <- tryCatch(
      rbounds::psens(differences, Gamma = max(gammas),
                     GammaInc = (max(gammas) - 1) /
                       max(1L, n_gamma - 1L)),
      error = function(e) NULL
    )
    if (!is.null(res) && !is.null(res$bounds)) {
      bnd <- as.data.frame(res$bounds)
      out <- data.frame(
        Gamma   = as.numeric(bnd[["Gamma"]]),
        p_lower = as.numeric(bnd[["Lower bound"]]),
        p_upper = as.numeric(bnd[["Upper bound"]])
      )
      return(out)
    }
  }

  n_pairs  <- length(differences)
  abs_diff <- abs(differences)
  ranks    <- rank(abs_diff)
  t_plus   <- sum(ranks[differences > 0])
  results  <- vector("list", n_gamma)
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
      2 * stats::pnorm(abs((t_plus - mu_u) / sqrt(var_u)),
                       lower.tail = FALSE)
    } else {
      NA_real_
    }
    p_lower <- if (var_l > 0) {
      2 * stats::pnorm(abs((t_plus - mu_l) / sqrt(var_l)),
                       lower.tail = FALSE)
    } else {
      NA_real_
    }
    results[[i]] <- data.frame(Gamma = gamma,
                               p_lower = p_lower,
                               p_upper = p_upper)
  }
  do.call(rbind, results)
}


# -- E-value (continuous-ATE flavour) ---------------------------------

#' E-value for unmeasured confounding (continuous-ATE scale)
#'
#' Thin wrapper over \code{EValue::evalues.OLS()} when \pkg{EValue} is
#' installed and an outcome SD is supplied via the `sd_y` argument
#' (recommended workflow per VanderWeele & Ding 2017). Without
#' \pkg{EValue}, or when `sd_y` is left at its default of 1, falls
#' back to the closed-form continuous-scale RR proxy used by the
#' Python port so both ports stay numerically aligned.
#'
#' @param ate  Point estimate of the treatment effect.
#' @param se   Standard error of the ATE (must be > 0).
#' @param null Null value. Default 0.
#' @param sd_y Outcome standard deviation. Default 1 (use the
#'   closed-form proxy). Pass the empirical sd to route through
#'   \code{EValue::evalues.OLS()} when installed.
#' @return Scalar E-value (>= 1).
#' @export
e_value <- function(ate, se, null = 0, sd_y = 1) {
  if (se <= 0) stop("se must be > 0, got ", se)
  z <- abs(ate - null) / se
  if (z == 0) return(1)

  if (!isTRUE(all.equal(sd_y, 1)) &&
        requireNamespace("EValue", quietly = TRUE)) {
    res <- tryCatch(
      EValue::evalues.OLS(est = ate - null, se = se, sd = sd_y),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      ev <- as.numeric(res["E-values", "point"])
      if (is.finite(ev) && ev >= 1) {
        return(ev)
      }
    }
  }

  rr <- exp(z)
  if (rr <= 1) return(1)
  rr + sqrt(rr * (rr - 1))
}


# ---------------------------------------------------------------------
# Marginal-effects extenders (Phase 1.j additions)
# ---------------------------------------------------------------------

.morie_effects_require <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("`%s` requires the `%s` package. ", fn, pkg),
      sprintf("Install it with install.packages(\"%s\").", pkg),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Estimated marginal means via \pkg{emmeans}
#'
#' Thin extender over \code{emmeans::emmeans()}. The fitted model is
#' passed through unchanged; \code{specs} follows the usual emmeans
#' formula / list interface. Use \code{emmeans::pairs()} or
#' \code{emmeans::contrast()} on the returned object for pairwise or
#' custom contrasts.
#'
#' @param model A fitted model object (`lm`, `glm`, `lmerMod`, ...).
#' @param specs Specification for the marginal means -- a formula
#'   (e.g. `~ treatment`), character vector of factor names, or a
#'   list, exactly as accepted by \code{emmeans::emmeans()}.
#' @param ... Further arguments forwarded to \code{emmeans::emmeans()}.
#' @return An \code{emmGrid} object.
#' @export
morie_effects_emmeans <- function(model, specs, ...) {
  .morie_effects_require("emmeans", "morie_effects_emmeans")
  emmeans::emmeans(object = model, specs = specs, ...)
}

#' Adjusted predictions via \pkg{marginaleffects}
#'
#' Thin extender over \code{marginaleffects::predictions()} for unit-
#' level or grid-level adjusted predictions.
#'
#' @param model   A fitted model object supported by \pkg{insight} /
#'   \pkg{marginaleffects}.
#' @param newdata Optional data frame for which to predict. Defaults
#'   to the model frame when `NULL` (the marginaleffects default).
#' @param ...     Further arguments forwarded to
#'   \code{marginaleffects::predictions()}.
#' @return A `marginaleffects` data frame.
#' @export
morie_effects_predictions <- function(model, newdata = NULL, ...) {
  .morie_effects_require("marginaleffects",
                         "morie_effects_predictions")
  if (is.null(newdata)) {
    marginaleffects::predictions(model, ...)
  } else {
    marginaleffects::predictions(model, newdata = newdata, ...)
  }
}

#' Contrasts / comparisons via \pkg{marginaleffects}
#'
#' Thin extender over \code{marginaleffects::comparisons()} for unit-
#' level treatment-effect contrasts (counterfactual differences,
#' ratios, etc.).
#'
#' @param model     A fitted model object.
#' @param variables Character vector or named list of variables to
#'   contrast (see \code{marginaleffects::comparisons()}). When
#'   `NULL`, marginaleffects' default (all model variables) is used.
#' @param ...       Further arguments forwarded to
#'   \code{marginaleffects::comparisons()}.
#' @return A `marginaleffects` data frame.
#' @export
morie_effects_comparisons <- function(model, variables = NULL, ...) {
  .morie_effects_require("marginaleffects",
                         "morie_effects_comparisons")
  if (is.null(variables)) {
    marginaleffects::comparisons(model, ...)
  } else {
    marginaleffects::comparisons(model, variables = variables, ...)
  }
}

#' Marginal slopes (partial derivatives) via \pkg{marginaleffects}
#'
#' Thin extender over \code{marginaleffects::slopes()} for continuous
#' marginal effects (Stata-style \code{margins, dydx()}).
#'
#' @param model     A fitted model object.
#' @param variables Character vector of focal variables. When `NULL`,
#'   the marginaleffects default (all continuous predictors) is used.
#' @param ...       Further arguments forwarded to
#'   \code{marginaleffects::slopes()}.
#' @return A `marginaleffects` data frame.
#' @export
morie_effects_slopes <- function(model, variables = NULL, ...) {
  .morie_effects_require("marginaleffects",
                         "morie_effects_slopes")
  if (is.null(variables)) {
    marginaleffects::slopes(model, ...)
  } else {
    marginaleffects::slopes(model, variables = variables, ...)
  }
}

#' Tidy a model with \pkg{broom} (fallback: `summary()` coefficients)
#'
#' Thin extender over \code{broom::tidy()}. When \pkg{broom} is not
#' installed, falls back to building a tidy-style data frame from
#' \code{summary(model)$coefficients}, which is sufficient for the
#' core `term / estimate / std.error / statistic / p.value` columns
#' on the model classes (\code{lm} / \code{glm}) that rmorie ships.
#'
#' @param model A fitted model object.
#' @param ...   Further arguments forwarded to \code{broom::tidy()}.
#' @return A data frame with one row per model term.
#' @export
morie_effects_tidy <- function(model, ...) {
  if (requireNamespace("broom", quietly = TRUE)) {
    return(broom::tidy(model, ...))
  }
  cf <- tryCatch(summary(model)$coefficients,
                 error = function(e) NULL)
  if (is.null(cf)) {
    stop("morie_effects_tidy(): install `broom` to tidy this model ",
         "class, or pass a model with summary()$coefficients.",
         call. = FALSE)
  }
  cf <- as.data.frame(cf)
  data.frame(
    term      = rownames(cf),
    estimate  = cf[[1]],
    std.error = if (ncol(cf) >= 2L) cf[[2]] else NA_real_,
    statistic = if (ncol(cf) >= 3L) cf[[3]] else NA_real_,
    p.value   = if (ncol(cf) >= 4L) cf[[4]] else NA_real_,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
