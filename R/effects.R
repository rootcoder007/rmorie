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
#'   \item \code{estimate_plr()} -- Partially Linear Regression, native
#'     cross-fit (cross-validated against \pkg{DoubleML}).
#'   \item \code{estimate_pliv()} -- Partially Linear IV (LATE), native
#'     (cross-validated against \pkg{DoubleML}).
#'   \item \code{estimate_ate_gcomputation()} -- G-computation ATE with
#'     stdReg's sandwich SE, native.
#'   \item \code{sensitivity_rosenbaum()} -- Rosenbaum bounds, native
#'     normal-approximation Wilcoxon signed-rank bounds.
#'   \item \code{e_value()} -- VanderWeele-Ding E-value. Thin wrapper
#'     native VanderWeele-Ding continuous-scale E-value (module 26).
#' }
#'
#' \strong{Marginal effects} (native; conventions of \pkg{emmeans} and
#' Vincent Arel-Bundock's \pkg{marginaleffects}, which are used only by
#' the cross-validation tests):
#' \itemize{
#'   \item \code{morie_effects_emmeans()} -- estimated marginal means.
#'   \item \code{morie_effects_predictions()} -- adjusted predictions.
#'   \item \code{morie_effects_comparisons()} -- counterfactual contrasts.
#'   \item \code{morie_effects_slopes()} -- partial derivatives.
#'   \item \code{morie_effects_tidy()} -- native coefficient table with
#'     \code{broom::tidy()}'s columns.
#' }
#'
#' They run natively for \code{lm} / \code{glm} fits (analytic
#' delta-method standard errors) and return plain data frames.
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
#' @return Named list with `ate` and `se` (native HC3-robust).
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
  w <- data[[weights_col]]
  fit <- stats::lm(fml, data = data, weights = w)
  vc <- morie_vcov_hc(fit, type = "HC3")
  list(
    ate = as.numeric(stats::coef(fit)[treatment]),
    se  = as.numeric(sqrt(diag(vc))[treatment])
  )
}


# -- Partially Linear Regression (DoubleML PLR) -----------------------

#' Partially Linear Regression (PLR) ATE
#'
#' Native cross-fitting estimator using ridge regression, following
#' Chernozhukov et al. (2018). Cross-validated against \pkg{DoubleML}
#' in the package's cross tests but not calling it at runtime; or,
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
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' X <- matrix(rnorm(n * 2), n, 2)
#' tr <- rbinom(n, 1, plogis(X[, 1]))
#' y <- 2 * tr + X[, 1] + rnorm(n)
#' df <- data.frame(y = y, d = tr, x1 = X[, 1], x2 = X[, 2])
#' res <- suppressWarnings(estimate_plr(df,
#'   treatment = "d", outcome = "y",
#'   covariates = c("x1", "x2")
#' ))
#' res$ate
#' }
#' @export
estimate_plr <- function(data, treatment, outcome, covariates,
                         n_folds = 5L, random_state = 42L) {
  required_cols <- c(treatment, outcome, covariates)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop(
      "Columns missing from data: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if (n_folds < 2L) {
    stop("n_folds must be >= 2, got ", n_folds)
  }

  df <- stats::na.omit(data[, c(treatment, outcome, covariates),
    drop = FALSE
  ])
  n_obs <- nrow(df)

  # Native cross-fit PLR (Chernozhukov et al. 2018) with ridge nuisance
  # learners -- the same estimator the DoubleML delegation computed,
  # cross-validated against DoubleML in tests/cross/.
  .rmorie_local_seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n_obs))
  d <- as.numeric(df[[treatment]])
  y <- as.numeric(df[[outcome]])
  x_mat <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(x_mat) <- "double"

  fit_predict <- function(x_train, z_train, x_test) {
    .morie_cv_ridge_predict(x_train, z_train, x_test)
  }

  y_hat <- numeric(n_obs)
  d_hat <- numeric(n_obs)
  for (k in seq_len(n_folds)) {
    train_idx <- which(folds != k)
    test_idx <- which(folds == k)
    y_hat[test_idx] <- fit_predict(
      x_mat[train_idx, , drop = FALSE],
      y[train_idx],
      x_mat[test_idx, , drop = FALSE]
    )
    d_hat[test_idx] <- fit_predict(
      x_mat[train_idx, , drop = FALSE],
      d[train_idx],
      x_mat[test_idx, , drop = FALSE]
    )
  }
  d_resid <- d - d_hat
  y_resid <- y - y_hat
  ate <- sum(d_resid * y_resid) / sum(d_resid * d_resid)
  psi <- (y_resid - ate * d_resid) * d_resid
  j0 <- mean(d_resid * d_resid)
  var_ate <- mean(psi^2) / (j0^2 * n_obs)
  se <- sqrt(var_ate)
  z <- stats::qnorm(0.975)
  list(
    ate      = ate,
    se       = se,
    ci_lower = ate - z * se,
    ci_upper = ate + z * se,
    pval     = 2 * (stats::pnorm(abs(ate / se), lower.tail = FALSE)),
    n_obs    = n_obs,
    method   = "cross-fit ridge (base R fallback)"
  )
}


# -- Partially Linear IV (LATE) ---------------------------------------

#' Partially Linear IV (PLIV) / Local Average Treatment Effect
#'
#' Native 2SLS in base R: first stage `D ~ Z + X`, second stage
#' `Y ~ D_hat + X`. Cross-validated against \pkg{DoubleML} in the
#' package's cross tests but not calling it at runtime.
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
#' @examples
#' set.seed(1)
#' n <- 80
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' z <- rbinom(n, 1, 0.5)
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
  if (length(missing_cols)) {
    stop(
      "Columns missing from data: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  df <- stats::na.omit(data[, c(
    treatment, outcome, instrument,
    covariates
  ), drop = FALSE])
  n_obs <- nrow(df)

  # Native partialled-out IV (PLIV): cross-fit ridge nuisances for
  # Y|X, D|X, Z|X, then IV regression of the Y-residual on the
  # D-residual instrumented by the Z-residual -- the DoubleMLPLIV
  # estimand (Chernozhukov et al. 2018), cross-validated in tests.
  .rmorie_local_seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n_obs))
  y_v <- as.numeric(df[[outcome]])
  d_v <- as.numeric(df[[treatment]])
  z_v <- as.numeric(df[[instrument]])
  x_mat <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(x_mat) <- "double"
  ry <- rd <- rz <- numeric(n_obs)
  for (f in seq_len(n_folds)) {
    te <- folds == f
    ry[te] <- y_v[te] - .morie_cv_ridge_predict(
      x_mat[!te, , drop = FALSE],
      y_v[!te],
      x_mat[te, , drop = FALSE]
    )
    rd[te] <- d_v[te] - .morie_cv_ridge_predict(
      x_mat[!te, , drop = FALSE],
      d_v[!te],
      x_mat[te, , drop = FALSE]
    )
    rz[te] <- z_v[te] - .morie_cv_ridge_predict(
      x_mat[!te, , drop = FALSE],
      z_v[!te],
      x_mat[te, , drop = FALSE]
    )
  }
  theta <- sum(rz * ry) / sum(rz * rd)
  psi <- rz * (ry - rd * theta)
  se <- sqrt(sum(psi^2) / (sum(rz * rd)^2))
  zstat <- theta / se
  return(list(
    late     = theta,
    se       = se,
    ci_lower = theta - stats::qnorm(0.975) * se,
    ci_upper = theta + stats::qnorm(0.975) * se,
    pval     = 2 * stats::pnorm(-abs(zstat)),
    n_obs    = n_obs,
    method   = "Native cross-fit PLIV (ridge nuisances)"
  ))
}


# -- G-computation (outcome regression / standardisation) -------------

#' G-computation ATE with its sandwich SE
#'
#' Regression standardisation: one GLM for the outcome, its predictions
#' with the treatment set to 1 and to 0 averaged over the sample, and the
#' M-estimation sandwich SE of \code{stdReg::stdGlm()} (Sjolander 2016),
#' computed natively (the same estimate and SE as
#' \code{\link{morie_estimate_g_computation}}, cross-validated against
#' stdReg). The 95\% interval is the estimate +/- 1.96 SE.
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
#' tr <- rbinom(n, 1, plogis(X[, 1]))
#' y <- 2.5 * tr + drop(X %*% c(1, 0.5, -0.7)) + rnorm(n)
#' d <- data.frame(y = y, d = tr, x1 = X[, 1], x2 = X[, 2], x3 = X[, 3])
#' res <- estimate_ate_gcomputation(d,
#'   treatment = "d", outcome = "y",
#'   covariates = c("x1", "x2", "x3")
#' )
#' res$ate
#' @export
estimate_ate_gcomputation <- function(data, treatment, outcome,
                                      covariates,
                                      outcome_model = "linear") {
  valid_models <- c("linear", "logistic")
  if (!outcome_model %in% valid_models) {
    stop(
      "outcome_model must be one of: ",
      paste(valid_models, collapse = ", ")
    )
  }
  required_cols <- c(treatment, outcome, covariates)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop(
      "Columns missing from data: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  df <- stats::na.omit(data[, c(treatment, outcome, covariates),
    drop = FALSE
  ])
  n_obs <- nrow(df)
  if (n_obs < 10L) {
    stop("G-computation requires at least 10 complete observations.")
  }

  fml <- stats::as.formula(paste0(outcome, " ~ ", paste(c(treatment, covariates), collapse = " + ")))
  fam <- if (outcome_model == "linear") stats::gaussian() else stats::binomial()
  mod <- stats::glm(fml, data = df, family = fam)
  g <- .morie_gformula_std(mod, df, treatment)
  z <- stats::qnorm(0.975)
  list(
    ate           = g$ate,
    se            = g$se,
    ci_lower      = g$ate - z * g$se,
    ci_upper      = g$ate + z * g$se,
    n_obs         = n_obs,
    outcome_model = outcome_model,
    method        = "g-formula, sandwich SE (stdReg's estimator, native)"
  )
}


# -- Rosenbaum bounds (data-frame interface) --------------------------

#' Rosenbaum bounds sensitivity analysis (data-frame interface)
#'
#' Native base R rank-matched-pair signed-rank bounds across a Gamma
#' grid, via the normal approximation to the Wilcoxon signed-rank
#' statistic.
#'
#' @param data       Data frame with treatment + outcome columns.
#' @param treatment  Binary treatment column (0/1).
#' @param outcome    Outcome column.
#' @param covariates Covariates (used only for matching approximation,
#'                   here a simple rank-match).
#' @param gamma_range c(min, max) of Gamma. Default c(1, 3).
#' @param n_gamma    Number of Gamma values. Default 20.
#' @return Data frame with `Gamma`, `p_lower`, `p_upper`.
#' @examples
#' if (requireNamespace("EValue", quietly = TRUE) && requireNamespace("rbounds", quietly = TRUE)) {
#'   set.seed(1)
#'   df <- data.frame(d = rbinom(60, 1, 0.5), x1 = rnorm(60))
#'   df$y <- df$d * 0.5 + df$x1 + rnorm(60)
#'   res <- try(sensitivity_rosenbaum(df, "d", "y", "x1"))
#'   if (!inherits(res, "try-error")) str(res, max.level = 1)
#' }
#' @export
sensitivity_rosenbaum <- function(data, treatment, outcome,
                                  covariates,
                                  gamma_range = c(1, 3),
                                  n_gamma = 20L) {
  required_cols <- c(treatment, outcome)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop(
      "Columns missing from data: ",
      paste(missing_cols, collapse = ", ")
    )
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

  df <- stats::na.omit(data[, c(treatment, outcome),
    drop = FALSE
  ])
  treated <- df[df[[treatment]] == 1, outcome]
  control <- df[df[[treatment]] == 0, outcome]
  min_n <- min(length(treated), length(control))
  if (min_n < 2L) {
    stop("At least 2 treated and 2 control units required.")
  }

  treated_sorted <- sort(treated)[seq_len(min_n)]
  control_sorted <- sort(control)[seq_len(min_n)]
  differences <- treated_sorted - control_sorted
  gammas <- seq(gamma_range[1], gamma_range[2],
    length.out = n_gamma
  )

  if (min_n >= 5L) {
    # Native Rosenbaum signed-rank bounds across the Gamma sequence --
    # the same table rbounds::psens() prints (cross-validated in tests).
    out <- do.call(rbind, lapply(gammas, function(g) {
      b <- .morie_psens_wilcoxon_d(differences, g)
      data.frame(
        Gamma = g,
        p_lower = as.numeric(b[["p_lower"]]),
        p_upper = as.numeric(b[["p_upper"]])
      )
    }))
    return(out)
  }

  n_pairs <- length(differences)
  abs_diff <- abs(differences)
  ranks <- rank(abs_diff)
  t_plus <- sum(ranks[differences > 0])
  results <- vector("list", n_gamma)
  for (i in seq_along(gammas)) {
    gamma <- gammas[i]
    p_max <- gamma / (1 + gamma)
    p_min <- 1 / (1 + gamma)
    mu_u <- n_pairs * (n_pairs + 1) / 2 * p_max
    var_u <- n_pairs * (n_pairs + 1) * (2 * n_pairs + 1) / 6 *
      p_max * (1 - p_max)
    mu_l <- n_pairs * (n_pairs + 1) / 2 * p_min
    var_l <- n_pairs * (n_pairs + 1) * (2 * n_pairs + 1) / 6 *
      p_min * (1 - p_min)
    p_upper <- if (var_u > 0) {
      2 * stats::pnorm(abs((t_plus - mu_u) / sqrt(var_u)),
        lower.tail = FALSE
      )
    } else {
      NA_real_
    }
    p_lower <- if (var_l > 0) {
      2 * stats::pnorm(abs((t_plus - mu_l) / sqrt(var_l)),
        lower.tail = FALSE
      )
    } else {
      NA_real_
    }
    results[[i]] <- data.frame(
      Gamma = gamma,
      p_lower = p_lower,
      p_upper = p_upper
    )
  }
  do.call(rbind, results)
}


# -- E-value (continuous-ATE flavour) ---------------------------------

#' E-value for unmeasured confounding (continuous-ATE scale)
#'
#' Native implementation of the VanderWeele & Ding (2017) OLS E-value.
#' Supply an outcome SD via `sd_y` for the recommended standardised
#' workflow; when `sd_y` is left at its default of 1, it
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
#' @examples
#' e_value(ate = 0.5, se = 0.1)
#' @export
e_value <- function(ate, se, null = 0, sd_y = 1) {
  if (se <= 0) stop("se must be > 0, got ", se)
  z <- abs(ate - null) / se
  if (z == 0) {
    return(1)
  }

  # Module 26: native VanderWeele-Ding continuous-scale E-value.
  d <- (ate - null) / sd_y
  ev <- morie_evalue(d, "MD", true = 0)$point
  if (is.finite(ev) && ev >= 1) {
    return(ev)
  }
  rr <- exp(z)
  if (rr <= 1) {
    return(1)
  }
  rr + sqrt(rr * (rr - 1))
}


# ---------------------------------------------------------------------
# Marginal effects (native engine in R/effects_native.R)
# ---------------------------------------------------------------------

#' Estimated marginal means (native)
#'
#' Native estimated marginal means (least-squares means) for \code{lm}
#' and \code{glm} fits, following the conventions of
#' \code{emmeans::emmeans()} (Lenth): a reference grid crossing every
#' level of each categorical predictor with each numeric covariate held
#' at its mean (or at the values in \code{at}); the marginal mean for a
#' cell of \code{specs} is the equal-weight average of the grid
#' predictions over the predictors not named in \code{specs}, a linear
#' combination \eqn{L b} with standard error \eqn{\sqrt{L V L'}},
#' \eqn{V = } \code{vcov(model)}. Degrees of freedom are the residual df
#' for \code{lm} and gaussian / Gamma \code{glm} fits and \code{Inf}
#' (asymptotic z) otherwise. Results are on the link scale unless
#' \code{type = "response"}, which back-transforms the estimate and the
#' interval and scales the SE by the inverse-link derivative (delta
#' method), naming the estimate \code{prob} (binomial), \code{rate}
#' (poisson) or \code{response}. Cross-validated against \pkg{emmeans}
#' in the tests; \pkg{emmeans} is not needed at run time.
#'
#' @param model A fitted \code{lm} or \code{glm} object.
#' @param specs Variables whose marginal means are wanted: a one-sided
#'   formula (\code{~ g}, \code{~ g * h}, \code{~ g | h}) or a character
#'   vector of predictor names.
#' @param ... Optional native settings: \code{type} (\code{"link"},
#'   default, or \code{"response"}), \code{level} (confidence level,
#'   default 0.95), \code{at} (named list of covariate values for the
#'   grid) and \code{by} (conditioning variables). Any other argument is
#'   an error.
#' @return A data frame shaped like \code{summary(emmeans::emmeans(...))}:
#'   the spec (and by) variables, the estimate (\code{emmean} /
#'   \code{prob} / \code{rate} / \code{response}), \code{SE}, \code{df}
#'   and \code{lower.CL}/\code{upper.CL} (\code{asymp.LCL}/
#'   \code{asymp.UCL} when \code{df = Inf}). Attributes carry the
#'   estimate name, scale and the linear-function matrix
#'   (\code{"linfct"}), so a contrast matrix C gives contrasts as the matrix product of C and
#'   \code{attr(x, "linfct")} times the coefficients.
#'   The \code{emmGrid} S4 object is no longer returned.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(60), x = rnorm(60), g = factor(rep(c("a", "b"), 30)))
#' fit <- stats::lm(y ~ x + g, data = df)
#' morie_effects_emmeans(fit, specs = "g")
#' @export
morie_effects_emmeans <- function(model, specs, ...) {
  .emm_native(model, specs, list(...))
}

#' Adjusted predictions (native)
#'
#' Native unit-level (or averaged) adjusted predictions for \code{lm}
#' and \code{glm} fits with the conventions of
#' \code{marginaleffects::predictions()} (Arel-Bundock): one row per row
#' of \code{newdata} (the model data by default), delta-method standard
#' errors from the analytic Jacobian and \code{vcov(model)}, z-based
#' tests and intervals. For a \code{glm} with no \code{type} and no
#' \code{by}, inference is done on the link scale and the estimate and
#' interval are back-transformed (marginaleffects' \code{"invlink(link)"});
#' \code{std.error} and \code{statistic} are then \code{NA}. With
#' \code{by}, unit predictions are averaged within groups on the response
#' scale. Cross-validated against \pkg{marginaleffects} in the tests.
#'
#' @param model   A fitted \code{lm} or \code{glm} object.
#' @param newdata Optional data frame for which to predict. Defaults
#'   to the model data when `NULL`.
#' @param ...     Optional native settings: \code{type}
#'   (\code{"response"} or \code{"link"}), \code{by} (\code{TRUE} or
#'   grouping column names), \code{conf_level}, \code{vcov} (\code{TRUE},
#'   \code{FALSE} or a matrix) and \code{df} (default \code{Inf}). Any
#'   other argument is an error.
#' @return A data frame with \code{rowid} (or the \code{by} columns),
#'   \code{estimate}, \code{std.error}, \code{statistic}, \code{p.value},
#'   \code{s.value}, \code{conf.low}, \code{conf.high} and, unit-level,
#'   the \code{newdata} columns.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(60), x = rnorm(60), g = factor(rep(c("a", "b"), 30)))
#' fit <- stats::lm(y ~ x + g, data = df)
#' head(morie_effects_predictions(fit))
#' @export
morie_effects_predictions <- function(model, newdata = NULL, ...) {
  .mfx_predictions(model, newdata, list(...))
}

#' Contrasts / comparisons (native)
#'
#' Native unit-level counterfactual differences for \code{lm} and
#' \code{glm} fits with the defaults of
#' \code{marginaleffects::comparisons()}: a numeric predictor is moved
#' from its observed value \code{x} to \code{x + 1} (or \code{x + step}
#' via \code{variables = list(x = step)}), a factor / character /
#' logical / 0-1 predictor is moved from its reference level to each
#' other level; the difference of predictions (response scale) gets a
#' delta-method SE from the analytic Jacobian. Terms are ordered
#' alphabetically. \code{by = TRUE} averages over units (the
#' \code{avg_comparisons()} result). Cross-validated against
#' \pkg{marginaleffects} in the tests.
#'
#' @param model     A fitted \code{lm} or \code{glm} object.
#' @param variables Character vector of focal predictors, or a named
#'   list giving a numeric step for numeric predictors. When `NULL`,
#'   all predictors.
#' @param ...       Optional native settings: \code{newdata},
#'   \code{type}, \code{by}, \code{conf_level}, \code{vcov}, \code{df}
#'   and \code{comparison} (only \code{"difference"}). Any other argument
#'   is an error.
#' @return A data frame with \code{rowid} (unit-level) or the \code{by}
#'   columns, \code{term}, \code{contrast}, \code{estimate},
#'   \code{std.error}, \code{statistic}, \code{p.value}, \code{s.value},
#'   \code{conf.low}, \code{conf.high}; unit-level rows also carry the
#'   data columns and \code{predicted_lo} / \code{predicted_hi}.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(60), x = rnorm(60), g = factor(rep(c("a", "b"), 30)))
#' fit <- stats::lm(y ~ x + g, data = df)
#' head(morie_effects_comparisons(fit, variables = "g"))
#' @export
morie_effects_comparisons <- function(model, variables = NULL, ...) {
  dots <- list(...)
  .mfx_compare(model, variables, dots$newdata, dots, slope = FALSE)
}

#' Marginal slopes (partial derivatives, native)
#'
#' Native unit-level partial derivatives \eqn{dY/dX} for \code{lm} and
#' \code{glm} fits with the defaults of
#' \code{marginaleffects::slopes()}: a centred finite difference
#' \eqn{(f(x + e/2) - f(x - e/2)) / e} with
#' \eqn{e = 10^{-4} \times} the range of \code{x} in the model data, on
#' the response scale, and a delta-method SE from the analytic Jacobian
#' of that difference. Categorical predictors give differences from the
#' reference level, as in marginaleffects. \code{by = TRUE} averages
#' (\code{avg_slopes()}). Cross-validated against \pkg{marginaleffects}
#' in the tests.
#'
#' @param model     A fitted \code{lm} or \code{glm} object.
#' @param variables Character vector of focal variables. When `NULL`,
#'   all predictors.
#' @param ...       Optional native settings: \code{newdata},
#'   \code{type}, \code{by}, \code{conf_level}, \code{vcov}, \code{df},
#'   \code{eps} and \code{slope} (only \code{"dydx"}). Any other argument
#'   is an error.
#' @return A data frame shaped like \code{morie_effects_comparisons()}
#'   with \code{contrast = "dY/dX"} for numeric predictors.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(60), x = rnorm(60), g = factor(rep(c("a", "b"), 30)))
#' fit <- stats::lm(y ~ x + g, data = df)
#' head(morie_effects_slopes(fit, variables = "x"))
#' @export
morie_effects_slopes <- function(model, variables = NULL, ...) {
  dots <- list(...)
  .mfx_compare(model, variables, dots$newdata, dots, slope = TRUE)
}

#' Tidy coefficient table for a fitted model
#'
#' Native coefficient table with the columns and row conventions of
#' \code{broom::tidy()} for \code{lm} / \code{glm} fits: one row per
#' coefficient in \code{coef(model)} order (aliased coefficients kept
#' with \code{NA} statistics), columns \code{term}, \code{estimate},
#' \code{std.error}, \code{statistic}, \code{p.value} taken from
#' \code{summary(model)$coefficients}, optional \code{conf.low} /
#' \code{conf.high} from \code{stats::confint()}, and optional
#' exponentiation of the estimate and interval. Any other model class
#' whose \code{summary()} carries a \code{coefficients} matrix is
#' tidied the same way.
#'
#' @param model A fitted model object.
#' @param conf.int Logical; add \code{conf.low} / \code{conf.high}.
#' @param conf.level Confidence level for the interval.
#' @param exponentiate Logical; exponentiate \code{estimate} and the
#'   interval bounds (e.g. odds ratios for a logit \code{glm}).
#' @param ...   Further arguments passed to \code{stats::confint()}.
#' @return A data frame with one row per model term.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(60), x = rnorm(60), g = factor(rep(c("a", "b"), 30)))
#' fit <- stats::lm(y ~ x + g, data = df)
#' morie_effects_tidy(fit)
#' @export
morie_effects_tidy <- function(model, conf.int = FALSE, conf.level = 0.95,
                               exponentiate = FALSE, ...) {
  cf <- tryCatch(summary(model)$coefficients,
    error = function(e) NULL
  )
  if (is.null(cf) || is.null(dim(cf))) {
    stop("morie_effects_tidy(): cannot tidy a model of class '",
      class(model)[1L], "'; it needs summary()$coefficients.",
      call. = FALSE
    )
  }
  getcol <- function(k) {
    if (ncol(cf) >= k) as.numeric(cf[, k]) else rep(NA_real_, nrow(cf))
  }
  out <- data.frame(
    term = rownames(cf),
    estimate = getcol(1L),
    std.error = getcol(2L),
    statistic = getcol(3L),
    p.value = getcol(4L),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  # Aliased (NA) coefficients are dropped by summary(); keep them as
  # NA rows in coef() order, as broom does.
  co <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.numeric(co) && !is.null(names(co)) && length(co) != nrow(out)) {
    m <- match(names(co), out$term)
    out <- data.frame(
      term = names(co),
      estimate = unname(co),
      std.error = out$std.error[m],
      statistic = out$statistic[m],
      p.value = out$p.value[m],
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }
  if (isTRUE(conf.int)) {
    ci <- suppressMessages(stats::confint(model, level = conf.level, ...))
    if (is.null(dim(ci))) {
      ci <- matrix(ci, nrow = 1L,
                   dimnames = list(names(stats::coef(model))[1L], NULL))
    }
    m <- match(out$term, rownames(ci))
    out$conf.low <- as.numeric(ci[m, 1L])
    out$conf.high <- as.numeric(ci[m, 2L])
  }
  if (isTRUE(exponentiate)) {
    out$estimate <- exp(out$estimate)
    if (!is.null(out$conf.low)) {
      out$conf.low <- exp(out$conf.low)
      out$conf.high <- exp(out$conf.high)
    }
  }
  out
}
