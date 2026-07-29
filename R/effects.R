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
#'     native cross-fitting with ridge nuisance learners.
#'   \item \code{estimate_pliv()} — Partially Linear IV (LATE) via
#'     native cross-fit partialling-out.
#'   \item \code{estimate_ate_gcomputation()} — G-computation
#'     (outcome-regression / standardisation) ATE with bootstrap SE.
#'   \item \code{sensitivity_rosenbaum()} — Rosenbaum bounds for hidden
#'     confounding (native Rosenbaum signed-rank bounds).
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
#' @examples
#' set.seed(1)
#' d <- data.frame(x = rnorm(60), tr = rbinom(60, 1, 0.5))
#' d$y <- 1 + 0.5 * d$tr + 0.3 * d$x + rnorm(60)
#' d$wt <- runif(60, 0.5, 1.5)
#' res <- estimate_ate(d, outcome = "y", treatment = "tr", weights_col = "wt")
#' res$ate
#' @export
estimate_ate <- function(data, outcome, treatment, weights_col) {
  fml <- stats::as.formula(paste(outcome, "~", treatment))
  w   <- data[[weights_col]]
  fit <- stats::lm(fml, data = data, weights = w)
  vc  <- morie_vcov_hc(fit, type = "HC3")
  list(
    ate = as.numeric(stats::coef(fit)[treatment]),
    se  = as.numeric(sqrt(diag(vc))[treatment])
  )
}


# -- Partially Linear Regression (DoubleML PLR) -----------------------

#' Partially Linear Regression (PLR) ATE
#'
#' Native cross-fit partially-linear-regression estimator
#' (Chernozhukov et al. 2018) with SVD-ridge nuisance learners;
#' cross-validated against DoubleML.
#'
#' @param data        Data frame with all required columns.
#' @param treatment   Column name of the treatment variable.
#' @param outcome     Column name of the outcome variable.
#' @param covariates  Character vector of covariate column names.
#' @param n_folds     Cross-fitting folds. Default 5.
#' @param random_state RNG seed. Default 42.
#' @return Named list with `ate`, `se`, `ci_lower`, `ci_upper`,
#'   `pval`, `n_obs`, `method`.
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' X <- matrix(rnorm(n * 2), n, 2)
#' tr <- rbinom(n, 1, plogis(X\[, 1\]))
#' y <- 2 * tr + X\[, 1\] + rnorm(n)
#' df <- data.frame(y = y, d = tr, x1 = X\[, 1\], x2 = X\[, 2\])
#' res <- suppressWarnings(estimate_plr(df, treatment = "d", outcome = "y",
#'                                      covariates = c("x1", "x2")))
#' res$ate
#' }
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

  # Native cross-fit PLR (Chernozhukov et al. 2018) with ridge nuisance
  # learners -- the same estimator the DoubleML delegation computed,
  # cross-validated against DoubleML in tests.
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n_obs))
  d <- as.numeric(df[[treatment]])
  y <- as.numeric(df[[outcome]])
  X <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(X) <- "double"

  fit_predict <- function(X_train, z_train, X_test) {
    .morie_cv_ridge_predict(X_train, z_train, X_test)
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
    method   = "Native cross-fit PLR (ridge nuisances)"
  )
}


# -- Partially Linear IV (LATE) ---------------------------------------

#' Partially Linear IV (PLIV) / Local Average Treatment Effect
#'
#' Native cross-fit partialled-out IV estimator (the DoubleMLPLIV
#' estimand, Chernozhukov et al. 2018) with SVD-ridge nuisances.
#'
#' @param data        Data frame with all required columns.
#' @param treatment   Endogenous treatment column name.
#' @param outcome     Outcome column name.
#' @param instrument  Instrument column name.
#' @param covariates  Exogenous covariate column names.
#' @param n_folds     Cross-fitting folds. Default 5.
#' @param random_state RNG seed. Default 42.
#' @return Named list with `late`, `se`, `ci_lower`, `ci_upper`,
#'   `pval`, `n_obs`, `method`.
#' @examples
#' set.seed(1)
#' n <- 80
#' x1 <- rnorm(n); x2 <- rnorm(n); z <- rbinom(n, 1, 0.5)
#' d <- as.integer(plogis(0.3 + 0.8 * z + 0.4 * x1) > runif(n))
#' y <- 1 + 0.5 * d + 0.3 * x1 + rnorm(n)
#' df <- data.frame(y, d, z, x1, x2)
#' res <- suppressWarnings(estimate_pliv(df, "d", "y", "z", c("x1", "x2")))
#' res$late
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

  # Native partialled-out IV (PLIV): cross-fit ridge nuisances for
  # Y|X, D|X, Z|X, then IV regression of the Y-residual on the
  # D-residual instrumented by the Z-residual -- the DoubleMLPLIV
  # estimand (Chernozhukov et al. 2018), cross-validated in tests.
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n_obs))
  y_v <- as.numeric(df[[outcome]])
  d_v <- as.numeric(df[[treatment]])
  z_v <- as.numeric(df[[instrument]])
  x_mat <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(x_mat) <- "double"
  ry <- rd <- rz <- numeric(n_obs)
  for (f in seq_len(n_folds)) {
    te <- folds == f
    ry[te] <- y_v[te] - .morie_cv_ridge_predict(x_mat[!te, , drop = FALSE],
                                                y_v[!te],
                                                x_mat[te, , drop = FALSE])
    rd[te] <- d_v[te] - .morie_cv_ridge_predict(x_mat[!te, , drop = FALSE],
                                                d_v[!te],
                                                x_mat[te, , drop = FALSE])
    rz[te] <- z_v[te] - .morie_cv_ridge_predict(x_mat[!te, , drop = FALSE],
                                                z_v[!te],
                                                x_mat[te, , drop = FALSE])
  }
  theta <- sum(rz * ry) / sum(rz * rd)
  psi <- rz * (ry - rd * theta)
  se <- sqrt(sum(psi^2) / (sum(rz * rd)^2))
  zstat <- theta / se
  list(
    late     = theta,
    se       = se,
    ci_lower = theta - stats::qnorm(0.975) * se,
    ci_upper = theta + stats::qnorm(0.975) * se,
    pval     = 2 * stats::pnorm(-abs(zstat)),
    n_obs    = n_obs,
    method   = "Native cross-fit PLIV (ridge nuisances)"
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
#' @examples
#' set.seed(1)
#' n <- 300
#' X <- matrix(rnorm(n * 3), n, 3)
#' tr <- rbinom(n, 1, plogis(X\[, 1\]))
#' y <- 2.5 * tr + drop(X %*% c(1, 0.5, -0.7)) + rnorm(n)
#' d <- data.frame(y = y, d = tr, x1 = X\[, 1\], x2 = X\[, 2\], x3 = X\[, 3\])
#' res <- estimate_ate_gcomputation(d, treatment = "d", outcome = "y",
#'                                  covariates = c("x1", "x2", "x3"))
#' res$ate
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
#' Computes normal-approximation Wilcoxon signed-rank bounds natively
#' in base R.
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
#' Applies the continuous-scale z-stat -> RR approximation natively,
#' matching the Python port.
#'
#' @param ate  Point estimate of the treatment effect.
#' @param se   Standard error of the ATE (must be > 0).
#' @param null Null value. Default 0.
#' @return Scalar E-value (>= 1).
#' @examples
#' e_value(ate = 0.5, se = 0.1)
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
