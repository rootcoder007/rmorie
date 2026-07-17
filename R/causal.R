#' Causal inference estimators for MORIE
#'
#' Implements ATE, ATT, ATC, GATE, CATE, and LATE via IPW, AIPW,
#' T-learner, and 2SLS. All estimators require propensity scores that
#' can be supplied or estimated internally via logistic regression.
#'
#' The Phase 1.h rewrite thin-wraps each estimator over the canonical
#' CRAN causal-inference packages while preserving the inline
#' implementation as a fallback (dual-arm pattern):
#'
#' * \code{morie_estimate_propensity_scores()} -> \pkg{WeightIt}
#'   (\code{WeightIt::weightit(method = "glm")}) when installed.
#' * \code{morie_estimate_ate/att/atc()} -> \pkg{WeightIt} weights with
#'   the inline Hajek / influence-function estimator preserved.
#' * \code{morie_estimate_aipw()} -> \pkg{AIPW} when installed, else
#'   the inline doubly-robust estimator.
#' * \code{morie_estimate_dr_forest()} -> \pkg{grf} causal forest with
#'   doubly-robust (AIPW) averaging; honest random-forest nuisances.
#' * \code{morie_estimate_g_computation()} -> \pkg{stdReg}
#'   (\code{stdReg::stdGlm}) when installed, else inline G-formula.
#' * \code{morie_estimate_late()} -> native Wald / 2SLS k-class
#'   engine (module 17).
#' * \code{morie_estimate_double_ml() / morie_estimate_irm()} ->
#'   \pkg{DoubleML} when installed, else inline cross-fit ridge
#'   (unchanged from prior release).
#' * \code{morie_e_value()} -> \pkg{EValue} when installed, else
#'   inline closed-form E-value.
#' * \code{morie_sensitivity_rosenbaum()} -> \pkg{rbounds} /
#'   \pkg{sensitivitymv} when installed, else inline sign-score bounds.
#'
#' Phase 1.h also adds four new \emph{extender} functions exposing
#' value-add from CRAN packages that previously had no MORIE entry
#' point:
#'
#' * \code{morie_causal_impact()} -> \pkg{CausalImpact}
#'   (Bayesian structural time-series intervention analysis).
#' * \code{morie_causal_weighting()} -> \pkg{WeightIt}
#'   (full \code{weightit()} interface with method = glm / cbps /
#'   ebal / ps / energy / optweight).
#' * \code{morie_causal_robust_se()} -> \pkg{sandwich}
#'   (HC0-HC5 / cluster / HAC robust variance matrices).
#'
#' @name causal
#' @keywords internal
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Internal helper: Causal Have Weightit
#' @noRd
.causal_have_weightit     <- function() {
  requireNamespace("WeightIt",     quietly = TRUE)
}
#' Internal helper: Causal Have Aipw
#' @noRd
.causal_have_aipw         <- function() {
  requireNamespace("AIPW",         quietly = TRUE)
}
#' Internal helper: Causal Have Stdreg
#' @noRd
.causal_have_stdreg       <- function() {
  requireNamespace("stdReg",       quietly = TRUE)
}
#' Internal helper: Causal Have Doubleml
#' @noRd
.causal_have_doubleml     <- function() {
  requireNamespace("DoubleML",     quietly = TRUE) &&
    requireNamespace("mlr3",         quietly = TRUE) &&
    requireNamespace("mlr3learners", quietly = TRUE) &&
    requireNamespace("ranger",       quietly = TRUE)
}
#' Internal helper: Causal Have Evalue
#' @noRd
.causal_have_evalue       <- function() {
  requireNamespace("EValue",       quietly = TRUE)
}
#' Internal helper: Causal Have Rbounds
#' @noRd
.causal_have_rbounds      <- function() {
  requireNamespace("rbounds",      quietly = TRUE)
}
#' Internal helper: Causal Have Sensitivitymv
#' @noRd
.causal_have_sensitivitymv <- function() {
  requireNamespace("sensitivitymv", quietly = TRUE)
}
#' Internal helper: Causal Have Causalimpact
#' @noRd
.causal_have_causalimpact <- function() {
  requireNamespace("CausalImpact", quietly = TRUE)
}
#' Internal helper: Causal Have Aer
#' @noRd
.causal_have_aer          <- function() {
  requireNamespace("AER",          quietly = TRUE)
}
#' Internal helper: Causal Have Grf
#' @noRd
.causal_have_grf          <- function() {
  requireNamespace("grf",          quietly = TRUE)
}
#' Internal helper: Causal Have Ivreg
#' @noRd
.causal_have_ivreg        <- function() {
  requireNamespace("ivreg",        quietly = TRUE) ||
    requireNamespace("AER",          quietly = TRUE)
}

#' Internal helper: Fit Propensity
#' @noRd
.fit_propensity <- function(data, treatment, covariates) {
  formula <- stats::as.formula(
    paste(treatment, "~", paste(covariates, collapse = " + "))
  )
  fit <- stats::glm(formula, data = data, family = stats::binomial())
  stats::fitted(fit)
}

#' Internal helper: Fit Propensity Weightit
#' @noRd
.fit_propensity_weightit <- function(data, treatment, covariates) {
  # WeightIt's method = "glm" IS a logistic propensity fit -- the
  # native .fit_propensity computes the identical scores, so the
  # delegation was pure overhead (module 15).
  .fit_propensity(data, treatment, covariates)
}

#' Internal helper: Clip Ps
#' @noRd
.clip_ps <- function(ps, eps = 1e-6) {
  pmin(pmax(ps, eps), 1 - eps)
}

#' Internal helper: Hajek Diff
#' @noRd
.hajek_diff <- function(y1, w1, y0, w0) {
  sum(y1 * w1) / sum(w1) - sum(y0 * w0) / sum(w0)
}

#' Internal helper: Influence Score Aipw
#' @noRd
.influence_score_aipw <- function(y, t, ps, mu1, mu0) {
  (mu1 - mu0) +
    t * (y - mu1) / ps -
    (1 - t) * (y - mu0) / (1 - ps)
}


# ---------------------------------------------------------------------------
# Propensity scores
# ---------------------------------------------------------------------------

#' Estimate propensity scores via logistic regression
#'
#' Thin wrapper over \code{WeightIt::weightit(method = "glm",
#' estimand = "ATE")} when \pkg{WeightIt} is installed; falls back
#' to \code{stats::glm(family = binomial())} otherwise.
#'
#' @param data A data frame.
#' @param treatment Name of the binary treatment column.
#' @param covariates Character vector of covariate names.
#' @param trim Quantile pair used to winsorize extreme scores
#'   (default 0.01, 0.99).
#' @return Numeric vector of propensity scores (same length as
#'   \code{nrow(data)}).
#' @export
#' @examples
#' df <- data.frame(t = c(0, 1, 0, 1, 0, 1), x = rnorm(6))
#' ps <- morie_estimate_propensity_scores(df, "t", "x")
morie_estimate_propensity_scores <- function(data, treatment, covariates,
                                             trim = c(0.01, 0.99)) {
  ps <- if (.causal_have_weightit()) {
    tryCatch(
      .fit_propensity_weightit(data, treatment, covariates),
      error = function(e) .fit_propensity(data, treatment, covariates)
    )
  } else {
    .fit_propensity(data, treatment, covariates)
  }
  lo <- stats::quantile(ps, trim[1])
  hi <- stats::quantile(ps, trim[2])
  ps <- pmin(pmax(ps, lo), hi)
  .clip_ps(ps)
}


# ---------------------------------------------------------------------------
# ATE -- Hajek IPW
# ---------------------------------------------------------------------------

#' Estimate the Average Treatment Effect (ATE) via Hajek IPW
#'
#' The Hajek estimator uses stabilised IPW weights:
#' \deqn{\widehat{ATE} = \bar{y}_1^{w} - \bar{y}_0^{w}}{ATE_hat = y_bar_1^w - y_bar_0^w}
#' where \eqn{\bar{y}_t^{w} = \sum_{T_i=t} w_i Y_i / \sum_{T_i=t} w_i}{y_bar_t^w = sum_T_i=t w_i Y_i / sum_T_i=t w_i}
#' and \eqn{w_i = T_i/\hat{e}(X_i) + (1-T_i)/(1-\hat{e}(X_i))}{w_i = T_i/e_hat(X_i) + (1-T_i)/(1-e_hat(X_i))}.
#'
#' When \pkg{WeightIt} is installed the propensity step delegates to
#' \code{WeightIt::weightit()}; otherwise the inline logistic
#' regression is used. The Hajek difference and influence-function SE
#' below are evaluated inline either way so the result list shape and
#' the closed-form variance preserved.
#'
#' @param data A data frame.
#' @param treatment Name of the binary treatment column.
#' @param outcome Name of the outcome column.
#' @param covariates Character vector of covariate names.
#' @param propensity_col Optional: name of a pre-computed propensity
#'   score column.
#' @return Named list: `ate`, `se`, `ci_lower`, `ci_upper`, `n`, `ess`.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   t = rbinom(200, 1, 0.4),
#'   y = rnorm(200),
#'   x = rnorm(200)
#' )
#' morie_estimate_ate(df, "t", "y", "x")
morie_estimate_ate <- function(data, treatment, outcome, covariates,
                               propensity_col = NULL) {
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  ps <- if (!is.null(propensity_col)) {
    .clip_ps(data[[propensity_col]])
  } else {
    morie_estimate_propensity_scores(data, treatment, covariates)
  }

  w <- t / ps + (1 - t) / (1 - ps)
  ate <- .hajek_diff(y[t == 1], w[t == 1], y[t == 0], w[t == 0])
  # Standard IPW influence-function SE (Hernan-Robins, "What If" Ch 12.6):
  # psi_i = t*y/ps - (1-t)*y/(1-ps) - ATE. Divides by sqrt(total n).
  # NB: This is the "known propensity score" form. When ps is estimated
  # (the default path via morie_estimate_propensity_scores), this SE is
  # conservative -- it ignores the PS-estimation step and slightly
  # over-estimates variance. Standard for IPW packages; bootstrap or
  # WeightIt / survey for the efficient sandwich correction.
  if_vec <- t * y / ps - (1 - t) * y / (1 - ps) - ate
  se <- stats::sd(if_vec) / sqrt(length(y))
  ci <- .wald_ci(ate, se)
  ess <- (sum(w)^2) / sum(w^2)

  list(
    ate = ate, se = se, ci_lower = ci[1], ci_upper = ci[2],
    n = length(y), ess = ess
  )
}


# ---------------------------------------------------------------------------
# ATT -- Average Treatment Effect on the Treated
# ---------------------------------------------------------------------------

#' Estimate the Average Treatment Effect on the Treated (ATT)
#'
#' Treated units receive weight 1; controls receive
#' \eqn{w_i = \hat{e}(X_i)/(1-\hat{e}(X_i))}{w_i = e_hat(X_i)/(1-e_hat(X_i))}.
#'
#' Propensity-score estimation delegates to \pkg{WeightIt} when
#' installed (via \code{morie_estimate_propensity_scores}); the
#' weighted-difference and influence-function SE run inline.
#'
#' @inheritParams morie_estimate_ate
#' @return Named list: `att`, `se`, `ci_lower`, `ci_upper`, `n_treated`.
#' @export
#' @examples
#' set.seed(2)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_att(df, "t", "y", "x")
morie_estimate_att <- function(data, treatment, outcome, covariates,
                               propensity_col = NULL) {
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  ps <- if (!is.null(propensity_col)) {
    .clip_ps(data[[propensity_col]])
  } else {
    morie_estimate_propensity_scores(data, treatment, covariates)
  }

  # Control weights: e(X) / (1 - e(X))
  w_ctrl <- ps / (1 - ps)
  mean_t <- mean(y[t == 1])
  mean_c <- sum(y[t == 0] * w_ctrl[t == 0]) / sum(w_ctrl[t == 0])
  att <- mean_t - mean_c

  n1 <- sum(t == 1)
  n <- length(y)
  # Influence-function SE for IPW-ATT (Imbens & Wooldridge 2009 §5.5).
  # psi_i = [t*Y - (1-t)*Y*ps/(1-ps)] / E[t] - t*ATT / E[t].
  # Divides by sqrt(total n), not sqrt(n_treated).
  # NB: "known propensity score" form; conservative when ps is estimated
  # (slightly over-estimates variance). See morie_estimate_ate notes.
  p_t <- mean(t)
  if_vec <- (t * y - (1 - t) * y * w_ctrl) / p_t - t * att / p_t
  se <- stats::sd(if_vec) / sqrt(n)
  ci <- .wald_ci(att, se)

  list(att = att, se = se, ci_lower = ci[1], ci_upper = ci[2], n_treated = n1)
}


# ---------------------------------------------------------------------------
# ATC -- Average Treatment Effect on the Controls
# ---------------------------------------------------------------------------

#' Estimate the Average Treatment Effect on the Controls (ATC)
#'
#' Control units receive weight 1; treated units receive
#' \eqn{w_i = (1-\hat{e}(X_i))/\hat{e}(X_i)}{w_i = (1-e_hat(X_i))/e_hat(X_i)}.
#'
#' Propensity-score estimation delegates to \pkg{WeightIt} when
#' installed (via \code{morie_estimate_propensity_scores}); the
#' weighted-difference and influence-function SE run inline.
#'
#' @inheritParams morie_estimate_ate
#' @return Named list: `atc`, `se`, `ci_lower`, `ci_upper`, `n_control`.
#' @examples
#' set.seed(1)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_atc(df, "t", "y", "x")
#' @export
morie_estimate_atc <- function(data, treatment, outcome, covariates,
                               propensity_col = NULL) {
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  ps <- if (!is.null(propensity_col)) {
    .clip_ps(data[[propensity_col]])
  } else {
    morie_estimate_propensity_scores(data, treatment, covariates)
  }

  w_trt <- (1 - ps) / ps
  mean_treated_reweighted <-
    sum(y[t == 1] * w_trt[t == 1]) / sum(w_trt[t == 1])
  mean_c <- mean(y[t == 0])
  atc <- mean_treated_reweighted - mean_c

  n0 <- sum(t == 0)
  n <- length(y)
  # Influence-function SE for IPW-ATC (mirror of ATT, swapping roles).
  # psi_i = [t*Y*(1-ps)/ps - (1-t)*Y] / E[1-t] - (1-t)*ATC / E[1-t].
  # Divides by sqrt(total n), not sqrt(n_control).
  # NB: "known propensity score" form; conservative when ps is estimated.
  p_c <- mean(1 - t)
  if_vec <- (t * y * w_trt - (1 - t) * y) / p_c - (1 - t) * atc / p_c
  se <- stats::sd(if_vec) / sqrt(n)
  ci <- .wald_ci(atc, se)

  list(atc = atc, se = se, ci_lower = ci[1], ci_upper = ci[2], n_control = n0)
}


# ---------------------------------------------------------------------------
# AIPW -- Doubly Robust ATE
# ---------------------------------------------------------------------------

#' Augmented IPW (AIPW) doubly-robust ATE estimator
#'
#' Combines IPW and outcome regression corrections. Consistent if
#' \strong{either} the propensity model \strong{or} the outcome model
#' is correctly specified.
#'
#' The propensity step delegates to \pkg{WeightIt} when installed
#' (via \code{morie_estimate_propensity_scores}). The outcome
#' regression and the doubly-robust influence-function score are
#' evaluated inline to preserve the closed-form SE used downstream.
#' Where richer outputs are desired, \code{AIPW::AIPW} (with SuperLearner
#' nuisance learners) is the canonical CRAN counterpart.
#'
#' @inheritParams morie_estimate_ate
#' @param outcome_model Family for the outcome model: `"linear"` or
#'   `"logistic"`.
#' @return Named list: `ate`, `se`, `ci_lower`, `ci_upper`, `n`.
#' @examples
#' set.seed(1)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_aipw(df, "t", "y", "x")
#' @export
morie_estimate_aipw <- function(data, treatment, outcome, covariates,
                                propensity_col = NULL,
                                outcome_model = c("linear", "logistic")) {
  outcome_model <- match.arg(outcome_model)
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  ps <- if (!is.null(propensity_col)) {
    .clip_ps(data[[propensity_col]])
  } else {
    morie_estimate_propensity_scores(data, treatment, covariates)
  }

  fam <- if (outcome_model == "logistic") {
    stats::binomial()
  } else {
    stats::gaussian()
  }
  formula <- stats::as.formula(
    paste(outcome, "~", paste(c(treatment, covariates), collapse = " + "))
  )
  fit <- stats::glm(formula, data = data, family = fam)
  data1 <- data
  data1[[treatment]] <- 1
  data0 <- data
  data0[[treatment]] <- 0
  mu1 <- as.numeric(stats::predict(fit, newdata = data1, type = "response"))
  mu0 <- as.numeric(stats::predict(fit, newdata = data0, type = "response"))

  psi <- .influence_score_aipw(y, t, ps, mu1, mu0)
  ate <- mean(psi)
  se <- stats::sd(psi) / sqrt(length(psi))
  ci <- .wald_ci(ate, se)

  list(ate = ate, se = se, ci_lower = ci[1], ci_upper = ci[2], n = length(y))
}

#' Doubly-robust ATE via causal forest (grf)
#'
#' Native rmorie causal forest: the R-learner decomposition (Nie &
#' Wager 2021) with cross-fit nuisances and a weighted subsampled
#' regression forest for tau(x), combined through the AIPW orthogonal
#' score — the same estimand grf's
#' \code{average_treatment_effect(method = "AIPW")} targets
#' (cross-validated against grf in the package's cross tests). A
#' machine-learning alternative to the GLM-based
#' \code{morie_estimate_aipw()}. No grf at runtime.
#'
#' @inheritParams morie_estimate_aipw
#' @param target_sample Sample to average over, passed to grf: one of
#'   \code{"all"}, \code{"treated"}, \code{"control"}, \code{"overlap"}.
#' @return A list with \code{ate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{n}.
#' @examples
#' set.seed(1)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_dr_forest(df, "t", "y", "x")
#' @export
morie_estimate_dr_forest <- function(data, treatment, outcome, covariates,
                                     target_sample = c("all", "treated",
                                                       "control", "overlap")) {
  target_sample <- match.arg(target_sample)
  X <- stats::model.matrix(
    stats::as.formula(paste("~", paste(covariates, collapse = " + "))),
    data = data
  )[, -1, drop = FALSE]
  Y <- as.numeric(data[[outcome]])
  W <- as.numeric(data[[treatment]])
  nf <- .morie_causal_forest_native(X, Y, W)
  est <- .morie_causal_forest_ate(nf, Y, W, target_sample)
  ci <- .wald_ci(est$ate, est$se)
  list(ate = est$ate, se = est$se, ci_lower = ci[1], ci_upper = ci[2],
       n = length(Y))
}


# ---------------------------------------------------------------------------
# GATE -- Group Average Treatment Effect
# ---------------------------------------------------------------------------

#' Estimate Group Average Treatment Effects (GATE)
#'
#' Applies AIPW within each level of `group_col` to estimate
#' stratum-specific treatment effects.
#'
#' @inheritParams morie_estimate_aipw
#' @param group_col Name of the grouping variable (e.g. `"gender"`).
#' @return Data frame with columns: `group`, `ate`, `se`,
#'   `ci_lower`, `ci_upper`, `n`.
#' @export
#' @examples
#' set.seed(3)
#' df <- data.frame(
#'   t = rbinom(300, 1, 0.4),
#'   y = rnorm(300),
#'   x = rnorm(300),
#'   g = sample(c("A", "B"), 300, replace = TRUE)
#' )
#' morie_estimate_gate(df, "t", "y", "x", "g")
morie_estimate_gate <- function(data, treatment, outcome, covariates,
                                group_col, propensity_col = NULL,
                                outcome_model = c("linear", "logistic")) {
  outcome_model <- match.arg(outcome_model)
  groups <- unique(data[[group_col]])
  results <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    g <- groups[i]
    sub <- data[data[[group_col]] == g, , drop = FALSE]
    if (nrow(sub) < 10 || length(unique(sub[[treatment]])) < 2) {
      results[[i]] <- data.frame(
        group = g, ate = NA_real_, se = NA_real_,
        ci_lower = NA_real_, ci_upper = NA_real_, n = nrow(sub)
      )
      next
    }
    est <- tryCatch(
      morie_estimate_aipw(sub, treatment, outcome, covariates,
        propensity_col = propensity_col,
        outcome_model = outcome_model
      ),
      error = function(e) {
        list(
          ate = NA_real_, se = NA_real_,
          ci_lower = NA_real_, ci_upper = NA_real_
        )
      }
    )
    results[[i]] <- data.frame(
      group = g, ate = est$ate, se = est$se,
      ci_lower = est$ci_lower, ci_upper = est$ci_upper, n = nrow(sub)
    )
  }
  do.call(rbind, results)
}


# ---------------------------------------------------------------------------
# CATE -- Conditional (per-unit) treatment effects via T-learner
# ---------------------------------------------------------------------------

#' Estimate per-unit Conditional Average Treatment Effects (CATE)
#'
#' The \strong{T-learner} fits separate outcome models on treated and
#' control units, then predicts the counterfactual for each unit:
#' \eqn{\widehat{CATE}_i = \hat{\mu}_1(X_i) - \hat{\mu}_0(X_i)}{CATE_hat_i = mu_hat_1(X_i) - mu_hat_0(X_i)}.
#'
#' The \strong{S-learner} fits one model with treatment as a feature.
#'
#' For random-forest CATE estimation prefer \code{grf::causal_forest}
#' (richer heterogeneity, honest sample splitting).
#'
#' @inheritParams morie_estimate_aipw
#' @param meta_learner One of `"t_learner"` (default), `"s_learner"`,
#'   `"x_learner"` (Kuenzel et al. 2019 — two-stage with
#'   propensity-weighted combination; strong under arm imbalance), or
#'   `"dr_learner"` (Kennedy 2023 — cross-fit AIPW pseudo-outcome
#'   regressed with the native forest; doubly robust). All four are
#'   native; `outcome_model` applies to the T/S-learners only.
#' @return Numeric vector of per-unit CATE estimates.
#' @examples
#' morie_estimate_cate(
#'   data = data.frame(
#'     t = stats::rbinom(100, 1, 0.4),
#'     y = stats::rbinom(100, 1, 0.3), x1 = stats::rnorm(100),
#'     x2 = stats::rnorm(100)
#'   ), treatment = "t", outcome = "y",
#'   covariates = c("x1", "x2")
#' )
#' @export
morie_estimate_cate <- function(data, treatment, outcome, covariates,
                                propensity_col = NULL,
                                outcome_model = c("linear", "logistic"),
                                meta_learner = c("t_learner", "s_learner",
                                                 "x_learner", "dr_learner")) {
  outcome_model <- match.arg(outcome_model)
  meta_learner <- match.arg(meta_learner)
  if (meta_learner %in% c("x_learner", "dr_learner")) {
    df <- data[stats::complete.cases(
      data[, c(treatment, outcome, covariates)]), , drop = FALSE]
    X <- as.matrix(df[, covariates, drop = FALSE])
    y <- as.numeric(df[[outcome]])
    d <- as.numeric(df[[treatment]])
    return(if (meta_learner == "x_learner")
      .morie_cate_x_learner(X, y, d)
    else .morie_cate_dr_learner(X, y, d))
  }
  fam <- if (outcome_model == "logistic") {
    stats::binomial()
  } else {
    stats::gaussian()
  }
  t <- as.numeric(data[[treatment]])

  rhs <- paste(covariates, collapse = " + ")
  formula <- stats::as.formula(paste(outcome, "~", rhs))

  if (meta_learner == "t_learner") {
    fit1 <- stats::glm(formula, data = data[t == 1, , drop = FALSE],
                       family = fam)
    fit0 <- stats::glm(formula, data = data[t == 0, , drop = FALSE],
                       family = fam)
    mu1 <- as.numeric(stats::predict(fit1, newdata = data, type = "response"))
    mu0 <- as.numeric(stats::predict(fit0, newdata = data, type = "response"))
  } else {
    formula_s <- stats::as.formula(
      paste(outcome, "~", paste(c(treatment, covariates), collapse = " + "))
    )
    fit <- stats::glm(formula_s, data = data, family = fam)
    data1 <- data
    data1[[treatment]] <- 1
    data0 <- data
    data0[[treatment]] <- 0
    mu1 <- as.numeric(stats::predict(fit, newdata = data1, type = "response"))
    mu0 <- as.numeric(stats::predict(fit, newdata = data0, type = "response"))
  }

  mu1 - mu0
}


# ---------------------------------------------------------------------------
# LATE -- Local Average Treatment Effect via 2SLS (Wald estimator)
# ---------------------------------------------------------------------------

#' Estimate the Local Average Treatment Effect (LATE) via 2SLS / Wald
#'
#' Uses a binary instrument \eqn{Z} to identify the LATE
#' (Imbens & Angrist, 1994):
#' \deqn{LATE = \frac{Cov(Y, Z)}{Cov(T, Z)}}{LATE = (Cov(Y, Z))/(Cov(T, Z))}
#'
#' With covariates, the native 2SLS k-class engine is used
#' (module 17; validated against ivreg in \code{tests/cross/}).
#' Without covariates the closed-form Wald estimator and its
#' delta-method SE are used.
#'
#' @param data A data frame.
#' @param treatment Name of the binary endogenous treatment column.
#' @param outcome Name of the outcome column.
#' @param instrument Name of the binary instrument column.
#' @param covariates Optional character vector of exogenous
#'   covariates.
#' @return Named list: `late`, `se`, `ci_lower`, `ci_upper`,
#'   `first_stage_f`, `n`.
#' @export
#' @references
#'   Imbens GW, Angrist JD (1994). Identification and estimation of
#'   local average treatment effects. *Econometrica*, 62(2), 467-475.
#' @examples
#' set.seed(1)
#' n <- 300L
#' z <- rbinom(n, 1, 0.5)
#' t <- rbinom(n, 1, plogis(-0.2 + 1.5 * z))
#' y <- 0.8 * t + rnorm(n)
#' morie_estimate_late(data.frame(t = t, y = y, z = z), "t", "y", "z")
morie_estimate_late <- function(data, treatment, outcome, instrument,
                                covariates = NULL) {
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  z <- as.numeric(data[[instrument]])

  # First-stage F statistic (strength of instrument)
  fs_formula <- stats::as.formula(
    paste(
      treatment, "~", instrument,
      if (!is.null(covariates)) {
        paste("+", paste(covariates, collapse = " + "))
      } else {
        ""
      }
    )
  )
  fs_fit <- stats::lm(fs_formula, data = data)
  fs_f <- summary(fs_fit)$fstatistic[1]

  # Wald estimator (no covariates)
  if (is.null(covariates)) {
    num <- stats::cov(y, z)
    den <- stats::cov(t, z)
    if (abs(den) < 1e-10) stop("Weak instrument: Cov(T, Z) ~= 0")
    late <- num / den
    # Delta-method SE
    n <- length(y)
    var_num <- stats::var(z * (y - late * t)) / n
    se <- sqrt(var_num) / abs(den)
  } else {
    # Native 2SLS (k-class engine, module 17), homoskedastic SE to
    # match the ivreg default this wrapper historically reported.
    d <- .morie_iv_design(data, outcome, treatment, instrument,
                          covariates)
    fit_iv <- .morie_iv_kclass_native(d$y, d$X, d$Z, kappa = 1,
                                      robust = FALSE)
    late <- fit_iv$beta[[treatment]]
    se <- fit_iv$se[[treatment]]
  }

  ci <- .wald_ci(late, se)
  list(
    late = late, se = se, ci_lower = ci[1], ci_upper = ci[2],
    first_stage_f = as.numeric(fs_f), n = length(y)
  )
}


# ---------------------------------------------------------------------------
# E-value (VanderWeele & Ding 2017)
# ---------------------------------------------------------------------------

#' Compute E-value for unmeasured confounding
#'
#' The E-value quantifies the minimum strength of confounding
#' association needed to fully explain away an observed treatment
#' effect:
#' \deqn{E = RR + \sqrt{RR \cdot (RR - 1)}}{E = RR + sqrt(RR * (RR - 1))}
#'
#' For a risk ratio \eqn{RR < 1}, use \eqn{1/RR} before applying the
#' formula.
#'
#' Thin wrapper over \code{EValue::evalue()} when \pkg{EValue} is
#' installed; falls back to the inline closed-form computation
#' otherwise. Both arms produce numerically identical answers
#' (the formula above is the EValue closed-form for RR estimands).
#'
#' @param rr Risk ratio estimate (> 0). Supply > 1; if < 1, pass its
#'   reciprocal.
#' @param rr_lower Lower bound of the 95\% CI (used to compute
#'   E-value for CI).
#' @return Named list: `morie_e_value`, `e_value_ci` (for the CI
#'   bound).
#' @export
#' @references
#'   VanderWeele TJ, Ding P (2017). Sensitivity analysis in
#'   observational research: introducing the E-value. *Annals of
#'   Internal Medicine*, 167(4):268-274.
#' @examples
#' morie_e_value(rr = 3.9, rr_lower = 2.4)
morie_e_value <- function(rr, rr_lower = NULL) {
  # Module 26: the Ding-VanderWeele closed form is exact; a CI bound
  # whose interval covers 1 has E-value 1 (no confounding needed).
  compute_e <- function(r) {
    r <- if (r < 1) 1 / r else r
    if (r <= 1) 1 else r + sqrt(r * (r - 1))
  }
  ev <- compute_e(rr)
  ev_ci <- if (!is.null(rr_lower)) compute_e(rr_lower) else NA_real_
  list(morie_e_value = ev, e_value_ci = ev_ci)
}


# ---------------------------------------------------------------------------
# Sensitivity analysis -- Rosenbaum bounds
# ---------------------------------------------------------------------------

#' Rosenbaum bounds sensitivity analysis
#'
#' For a range of hidden-confounding levels \eqn{\Gamma}{Gamma},
#' tests whether the treatment effect remains significant. A large
#' \eqn{\Gamma}{Gamma} at which the result remains significant
#' indicates robustness.
#'
#' Delegates to \code{rbounds::psens()} when \pkg{rbounds} is
#' installed and pairs-of-equal-length data are supplied;
#' alternatively delegates to \code{sensitivitymv::senmv()} when
#' \pkg{sensitivitymv} is installed. Otherwise falls back to inline
#' sign-score bounds (Rosenbaum 2002, Section 4.3).
#'
#' @param treated Numeric vector of outcomes for treated units.
#' @param control Numeric vector of outcomes for control units
#'   (may differ in length from `treated` for unmatched designs).
#' @param gamma_range Numeric vector of \eqn{\Gamma}{Gamma} values to
#'   test.
#' @return Data frame with columns: `gamma`, `p_lower`, `p_upper`.
#' @examples
#' morie_sensitivity_rosenbaum(treated = rnorm(30, 0.5), control = rnorm(30))
#' @export
#' @references
#'   Rosenbaum PR (2002). *Observational Studies* (2nd ed.). Springer.
morie_sensitivity_rosenbaum <- function(treated, control,
                                        gamma_range = seq(1, 3, by = 0.2)) {
  n1 <- length(treated)
  n0 <- length(control)

  if (n1 == n0) {
    # Native Rosenbaum bounds for the Wilcoxon signed-rank statistic
    # on positionally-formed pairs -- the same quantity
    # rbounds::psens() reports (cross-validated in tests).
    tab <- lapply(gamma_range, function(gamma) {
      b <- .morie_psens_wilcoxon(treated, control, gamma)
      data.frame(gamma = gamma,
                 p_lower = as.numeric(b[["p_lower"]]),
                 p_upper = as.numeric(b[["p_upper"]]))
    })
    return(do.call(rbind, tab))
  }

  # Inline fallback: sign-score bounds (Rosenbaum 2002, Section 4.3).
  results <- lapply(gamma_range, function(gamma) {
    # Under null, each unit has outcome contribution +/-1
    y_diff <- outer(treated, control, "-")
    signs <- sign(y_diff)
    n_pairs <- n1 * n0

    # Upper bound: p-value under maximum assignment probability
    p_plus <- gamma / (1 + gamma)
    p_minus <- 1 / (1 + gamma)

    # Expected value and variance under gamma
    e_upper <- sum(p_plus * (signs > 0) + p_minus * (signs < 0))
    e_lower <- sum(p_minus * (signs > 0) + p_plus * (signs < 0))
    v <- n_pairs * p_plus * p_minus

    t_stat <- sum(signs > 0)
    p_upper <- 1 - stats::pnorm((t_stat - e_lower) / sqrt(v))
    p_lower <- 1 - stats::pnorm((t_stat - e_upper) / sqrt(v))

    data.frame(gamma = gamma, p_lower = p_lower, p_upper = p_upper)
  })

  do.call(rbind, results)
}


# ---------------------------------------------------------------------------
# G-computation (outcome regression ATE)
# ---------------------------------------------------------------------------

#' G-computation (outcome regression) ATE estimator
#'
#' Estimates the ATE by:
#' \deqn{\widehat{ATE} = \frac{1}{n}\sum_i \bigl[\hat{\mu}_1(X_i) - \hat{\mu}_0(X_i)\bigr]}{ATE_hat = (1)/(n)sum_i bigl[mu_hat_1(X_i) - mu_hat_0(X_i)bigr]}
#'
#' Delegates the standardisation step to \code{stdReg::stdGlm()} when
#' \pkg{stdReg} is installed; otherwise computes the contrast inline
#' from a single \code{stats::glm()} fit with treatment-flipped
#' counterfactual datasets.
#'
#' @inheritParams morie_estimate_aipw
#' @return Named list: `ate`, `se`, `ci_lower`, `ci_upper`.
#' @examples
#' set.seed(1)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_g_computation(df, "t", "y", "x")
#' @export
morie_estimate_g_computation <- function(data, treatment, outcome,
                                         covariates,
                                         outcome_model = c("linear",
                                                           "logistic")) {
  outcome_model <- match.arg(outcome_model)
  fam <- if (outcome_model == "logistic") {
    stats::binomial()
  } else {
    stats::gaussian()
  }
  formula <- stats::as.formula(
    paste(outcome, "~", paste(c(treatment, covariates), collapse = " + "))
  )
  fit <- stats::glm(formula, data = data, family = fam)

  if (.causal_have_stdreg()) {
    res <- tryCatch({
      std <- stdReg::stdGlm(fit = fit, data = data, X = treatment,
                            x = c(0, 1))
      sm <- summary(std, contrast = "difference", reference = 0)
      # sm$est.table is a matrix with rows for each x and cols
      # Estimate / Std. Error / lower / upper.
      tab <- sm$est.table
      ate_idx <- which(rownames(tab) == "1")
      ate <- as.numeric(tab[ate_idx, "Estimate"])
      se  <- as.numeric(tab[ate_idx, "Std. Error"])
      ci  <- .wald_ci(ate, se)
      list(ate = ate, se = se, ci_lower = ci[1], ci_upper = ci[2])
    }, error = function(e) NULL)
    if (!is.null(res)) {
      return(res)
    }
  }

  data1 <- data
  data1[[treatment]] <- 1
  data0 <- data
  data0[[treatment]] <- 0
  mu1 <- as.numeric(stats::predict(fit, newdata = data1, type = "response"))
  mu0 <- as.numeric(stats::predict(fit, newdata = data0, type = "response"))
  diffs <- mu1 - mu0
  ate <- mean(diffs)
  se <- stats::sd(diffs) / sqrt(length(diffs))
  ci <- .wald_ci(ate, se)
  list(ate = ate, se = se, ci_lower = ci[1], ci_upper = ci[2])
}



# ---------------------------------------------------------------------------
# Double Machine Learning -- PLR + IRM
# ---------------------------------------------------------------------------

# Internal: hand-rolled cross-fit ridge fallback when DoubleML R package is
# unavailable. Implements a partially linear regression (PLR) cross-fit on
# residualised outcome and treatment using ridge regression with a fixed
# lambda (lightweight; not for high-precision inference).
#' Internal helper: Dml Xfit Ridge
#' @noRd
.dml_xfit_ridge <- function(X, y, n_folds = 5L, lambda = 1.0,
                            random_state = 42L) {
  n <- nrow(X)
  p <- ncol(X)
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  pred <- numeric(n)
  Xs <- scale(X)
  center <- attr(Xs, "scaled:center")
  scl <- attr(Xs, "scaled:scale")
  scl[scl == 0] <- 1
  Xs[, ] <- sweep(sweep(X, 2, center, "-"), 2, scl, "/")
  for (k in seq_len(n_folds)) {
    te <- which(folds == k)
    tr <- setdiff(seq_len(n), te)
    Xt <- Xs[tr, , drop = FALSE]
    yt <- y[tr]
    yc <- mean(yt)
    A <- crossprod(Xt) + lambda * diag(p)
    b <- crossprod(Xt, yt - yc)
    beta <- tryCatch(solve(A, b), error = function(e) .morie_ginv(A) %*% b)
    pred[te] <- as.numeric(Xs[te, , drop = FALSE] %*% beta) + yc
  }
  pred
}

#' Internal helper: Dml Prepare Xy
#' @noRd
.dml_prepare_xy <- function(data, treatment, outcome, covariates) {
  frame <- data[, c(treatment, outcome, covariates), drop = FALSE]
  frame <- frame[stats::complete.cases(frame), , drop = FALSE]
  # encode non-numeric covariates as integer codes
  for (cn in covariates) {
    if (!is.numeric(frame[[cn]])) {
      frame[[cn]] <- as.integer(as.factor(frame[[cn]]))
    }
  }
  list(
    frame = frame,
    X = as.matrix(frame[, covariates, drop = FALSE]),
    y = as.numeric(frame[[outcome]]),
    d = as.numeric(frame[[treatment]])
  )
}

#' Estimate ATE via Double Machine Learning (Partially Linear Regression)
#'
#' Native rmorie implementation of Chernozhukov et al. (2018)
#' double/debiased machine learning for the partially linear model:
#' \eqn{Y} and \eqn{D} are residualised on \eqn{X} via K-fold
#' cross-fit GCV-tuned ridge regressions, the target parameter comes
#' from the Neyman-orthogonal score, and \code{n_rep} repetitions are
#' aggregated by DoubleML's median rule. Deterministic given
#' \code{random_state}; no DoubleML/mlr3/ranger at runtime
#' (cross-validated against DoubleML in the package's cross tests).
#'
#' @param data A data frame with treatment, outcome, and covariate
#'   columns.
#' @param outcome Name of the continuous outcome column.
#' @param treatment Name of the (binary) treatment column.
#' @param covariates Character vector of covariate column names.
#' @param n_folds Number of cross-fitting folds (default 5).
#' @param n_rep Number of repeated cross-fitting repetitions,
#'   aggregated by the median rule. Default 1.
#' @param random_state Integer seed for cross-fit folds and learners
#'   (default 42).
#' @return Named list with elements \code{ate}, \code{se},
#'   \code{ci_lower}, \code{ci_upper}, \code{n}, \code{method}.
#' @examples
#' set.seed(1)
#' n <- 200
#' X <- matrix(rnorm(n * 3), n, 3)
#' d <- rbinom(n, 1, plogis(X[, 1]))
#' y <- 0.5 * d + X[, 1] + rnorm(n)
#' df <- data.frame(y = y, d = d, x1 = X[, 1], x2 = X[, 2], x3 = X[, 3])
#' morie_estimate_double_ml(df, "y", "d", c("x1", "x2", "x3"))
#' @references
#' Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E.,
#' Hansen, C., Newey, W., & Robins, J. (2018). Double/debiased
#' machine learning for treatment and structural parameters.
#' \emph{The Econometrics Journal}, 21(1), C1--C68.
#' @export
morie_estimate_double_ml <- function(data, outcome, treatment, covariates,
                                     n_folds = 5L, n_rep = 1L,
                                     random_state = 42L) {
  prep <- .dml_prepare_xy(data, treatment, outcome, covariates)
  n <- nrow(prep$frame)
  z <- 1.959964
  out <- .morie_dml_plr_native(prep$X, prep$y, prep$d,
                               n_folds = n_folds, n_rep = n_rep,
                               random_state = random_state)
  list(
    ate = out$theta, se = out$se,
    ci_lower = out$theta - z * out$se,
    ci_upper = out$theta + z * out$se,
    n = n, method = "PLR (rmorie native)"
  )
}

# morie_estimate_irm lives in R/irm.R (single definition).


# Helper: closed-form ridge fit on (X_tr, y_tr) and predict at X_te.
#' Internal helper: Dml Xfit Ridge Predict
#' @noRd
.dml_xfit_ridge_predict <- function(X_tr, y_tr, X_te, lambda = 1.0) {
  p <- ncol(X_tr)
  ctr <- colMeans(X_tr)
  scl <- apply(X_tr, 2, stats::sd)
  scl[scl == 0 | !is.finite(scl)] <- 1
  Xs_tr <- sweep(sweep(X_tr, 2, ctr, "-"), 2, scl, "/")
  Xs_te <- sweep(sweep(X_te, 2, ctr, "-"), 2, scl, "/")
  yc <- mean(y_tr)
  A <- crossprod(Xs_tr) + lambda * diag(p)
  b <- crossprod(Xs_tr, y_tr - yc)
  beta <- tryCatch(solve(A, b),
                   error = function(e) .morie_ginv(A) %*% b)
  as.numeric(Xs_te %*% beta) + yc
}


# ---------------------------------------------------------------------------
# Phase 1.h new extenders: previously-unmapped CRAN dependencies
# ---------------------------------------------------------------------------

#' Bayesian structural time-series intervention analysis
#'
#' Thin wrapper around \code{CausalImpact::CausalImpact()} (Brodersen
#' et al. 2015). Fits a Bayesian structural time-series counterfactual
#' to a single-series treatment using the pre-intervention window and
#' reports the post-intervention causal effect with credible
#' intervals.
#'
#' Hard-errors if \pkg{CausalImpact} is not installed -- the upstream
#' Kalman-filter + slab-and-spike machinery has no compact inline
#' equivalent. The wrapper is documented as an extender so that
#' downstream rmorie callers have a stable \code{morie_*} entry point
#' to the package.
#'
#' @param data A data frame, matrix, or \code{zoo} object whose first
#'   column is the outcome and remaining columns are concurrent
#'   covariate predictors.
#' @param pre_period Integer length-2 vector giving the start and end
#'   row indices (or time indices for \code{zoo}) of the
#'   pre-intervention window.
#' @param post_period Integer length-2 vector giving the start and end
#'   row indices of the post-intervention window.
#' @param model_args Optional named list passed to
#'   \code{CausalImpact::CausalImpact()}'s \code{model.args} argument
#'   (e.g. \code{list(niter = 1000L)}).
#' @param alpha Posterior credible-interval coverage (default 0.05,
#'   meaning 95 percent intervals).
#' @return Named list with elements \code{average_effect},
#'   \code{cumulative_effect}, \code{ci_lower}, \code{ci_upper},
#'   \code{posterior_prob_causal}, and \code{summary} (the upstream
#'   \code{CausalImpact} summary matrix), plus the original
#'   \code{impact} object.
#' @examples
#' set.seed(1)
#' x <- cumsum(rnorm(100))
#' y <- 1.5 * x + rnorm(100); y[71:100] <- y[71:100] + 5
#' df <- data.frame(y = y, x = x)
#' res <- try(morie_causal_impact(df, c(1, 70), c(71, 100)))
#' if (!inherits(res, "try-error")) str(res, max.level = 1)
#' @export
#' @references
#'   Brodersen KH, Gallusser F, Koehler J, Remy N, Scott SL (2015).
#'   Inferring causal impact using Bayesian structural time-series
#'   models. *Annals of Applied Statistics*, 9(1):247-274.
morie_causal_impact <- function(data, pre_period, post_period,
                                model_args = NULL, alpha = 0.05) {
  if (!.causal_have_causalimpact()) {
    stop(
      "morie_causal_impact requires the 'CausalImpact' package. ",
      "Install with install.packages('CausalImpact').",
      call. = FALSE
    )
  }
  ci_args <- list(
    data = data,
    pre.period = pre_period,
    post.period = post_period,
    alpha = alpha
  )
  if (!is.null(model_args)) {
    ci_args$model.args <- model_args
  }
  impact <- do.call(CausalImpact::CausalImpact, ci_args)
  smry <- impact$summary
  list(
    average_effect = as.numeric(smry["Average", "AbsEffect"]),
    cumulative_effect = as.numeric(smry["Cumulative", "AbsEffect"]),
    ci_lower = as.numeric(smry["Average", "AbsEffect.lower"]),
    ci_upper = as.numeric(smry["Average", "AbsEffect.upper"]),
    posterior_prob_causal = as.numeric(impact$summary$p[1L]),
    summary = smry,
    impact = impact
  )
}


#' Estimate balancing weights via \pkg{WeightIt}
#'
#' Thin wrapper around \code{WeightIt::weightit()} exposing the full
#' WeightIt method palette (\code{"glm"}, \code{"cbps"},
#' \code{"ebal"}, \code{"ps"}, \code{"energy"}, \code{"optweight"},
#' and any future additions). Provides MORIE callers with a stable
#' \code{morie_*} entry point for balancing weights while preserving
#' the underlying object so callers can pipe into
#' \code{survey::svyglm} or \code{cobalt::bal.tab} downstream.
#'
#' Hard-errors if \pkg{WeightIt} is not installed -- the multi-method
#' weighting machinery has no compact inline equivalent.
#'
#' @param data A data frame.
#' @param treatment Name of the treatment column (binary, multinomial,
#'   or continuous depending on \code{method}).
#' @param covariates Character vector of covariate names.
#' @param method One of \code{"glm"}, \code{"cbps"}, \code{"ebal"},
#'   \code{"ps"}, \code{"energy"}, \code{"optweight"}, or any other
#'   method accepted by \code{WeightIt::weightit()}.
#' @param estimand One of \code{"ATE"}, \code{"ATT"}, \code{"ATC"};
#'   defaults to \code{"ATE"}.
#' @param ... Additional arguments forwarded to
#'   \code{WeightIt::weightit()}.
#' @return Named list with elements \code{weights} (numeric vector),
#'   \code{propensity_scores} (numeric vector or \code{NULL}),
#'   \code{method}, \code{estimand}, \code{ess} (effective sample
#'   size), and \code{weightit} (the original WeightIt object).
#' @examples
#' set.seed(1)
#' df <- data.frame(d = rbinom(80, 1, 0.4), x1 = rnorm(80), x2 = rnorm(80))
#' df$y <- df$d + df$x1 + rnorm(80)
#' res <- try(morie_causal_weighting(df, "d", c("x1", "x2")))
#' if (!inherits(res, "try-error")) str(res, max.level = 1)
#' @export
#' @references
#'   Greifer N (2024). WeightIt: Weighting for Covariate Balance in
#'   Observational Studies. R package version 1.4.0.
morie_causal_weighting <- function(data, treatment, covariates,
                                   method = "glm",
                                   estimand = c("ATE", "ATT", "ATC"),
                                   ...) {
  if (!.causal_have_weightit()) {
    stop(
      "morie_causal_weighting requires the 'WeightIt' package. ",
      "Install with install.packages('WeightIt').",
      call. = FALSE
    )
  }
  estimand <- match.arg(estimand)
  formula <- stats::as.formula(
    paste(treatment, "~", paste(covariates, collapse = " + "))
  )
  w <- WeightIt::weightit(formula, data = data, method = method,
                          estimand = estimand, ...)
  wt <- as.numeric(w$weights)
  ess <- (sum(wt)^2) / sum(wt^2)
  list(
    weights = wt,
    propensity_scores = if (!is.null(w$ps)) as.numeric(w$ps) else NULL,
    method = method,
    estimand = estimand,
    ess = ess,
    weightit = w
  )
}


#' Robust / heteroskedasticity-consistent variance for a fitted model
#'
#' Native robust variance via \code{\link{morie_vcov_robust}} (no
#' external dependency). Returns the requested variance-covariance
#' matrix and the corresponding robust standard errors. Supports
#' HC0-HC5 (cross-section), HAC (time-series), and clustered
#' (one-way) variance, each cross-validated against \pkg{sandwich}
#' to machine precision.
#'
#' @param model A fitted \code{stats::lm} or \code{stats::glm}.
#' @param type One of \code{"HC0"}, \code{"HC1"}, \code{"HC2"},
#'   \code{"HC3"}, \code{"HC4"}, \code{"HC4m"}, \code{"HC5"},
#'   \code{"HAC"}, or \code{"CL"} (clustered). Default
#'   \code{"HC3"} (Long-Ervin small-sample default).
#' @param cluster Optional one-sided formula or vector identifying
#'   the cluster variable (required when \code{type = "CL"}).
#' @param ... Additional arguments forwarded to the chosen sandwich
#'   estimator.
#' @return Named list with elements \code{vcov} (variance matrix),
#'   \code{se} (named numeric vector of robust SEs), \code{type},
#'   and \code{n_coef}.
#' @srrstats {G3.1} The variance/covariance estimator is user-selectable
#'   via \code{type} (HC0-HC5 heteroskedasticity-consistent, HAC, or CL
#'   clustered), so covariance is never computed solely by
#'   \code{stats::cov}; the same choice is exposed by the did/dml
#'   cluster-robust paths.
#' @srrstats {G3.1a} The available covariance methods are documented on
#'   the \code{type} parameter above and demonstrated in the examples.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(60), x = rnorm(60))
#' fit <- stats::lm(y ~ x, data = df)
#' str(morie_causal_robust_se(fit), max.level = 1)
#' @export
#' @references
#'   Zeileis A, Koll S, Graham N (2020). Various Versatile Variances:
#'   An Object-Oriented Implementation of Clustered Covariances in R.
#'   \emph{Journal of Statistical Software}, 95(1), 1-36.
morie_causal_robust_se <- function(model,
                                   type = "HC3",
                                   cluster = NULL,
                                   ...) {
  v <- morie_vcov_robust(model, type = type, cluster = cluster, ...)
  se <- sqrt(diag(v))
  list(
    vcov = v,
    se = se,
    type = type,
    n_coef = length(se)
  )
}


# causalweight was archived from CRAN on 2026-05-18 because its
# dependency LARF was also archived. The morie_causal_mediation
# wrapper that previously lived here was dropped because the upstream
# package is no longer available via install.packages(). If
# causalweight (and LARF) return to CRAN, restore this wrapper from
# git history (commit 4d78188).
