#' Causal inference estimators for MORIE
#'
#' Implements ATE, ATT, ATC, GATE, CATE, and LATE via IPW, AIPW,
#' T-learner, and 2SLS. All estimators require propensity scores that
#' can be supplied or estimated internally via logistic regression.
#'
#' @name causal
#' @keywords internal
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.fit_propensity <- function(data, treatment, covariates) {
  formula <- stats::as.formula(
    paste(treatment, "~", paste(covariates, collapse = " + "))
  )
  fit <- stats::glm(formula, data = data, family = stats::binomial())
  stats::fitted(fit)
}

.clip_ps <- function(ps, eps = 1e-6) {
  pmin(pmax(ps, eps), 1 - eps)
}

.hajek_diff <- function(y1, w1, y0, w0) {
  sum(y1 * w1) / sum(w1) - sum(y0 * w0) / sum(w0)
}

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
#' @param data A data frame.
#' @param treatment Name of the binary treatment column.
#' @param covariates Character vector of covariate names.
#' @param trim Quantile pair used to winsorize extreme scores (default 0.01, 0.99).
#' @return Numeric vector of propensity scores (same length as `nrow(data)`).
#' @export
#' @examples
#' df <- data.frame(t = c(0, 1, 0, 1, 0, 1), x = rnorm(6))
#' ps <- morie_estimate_propensity_scores(df, "t", "x")
morie_estimate_propensity_scores <- function(data, treatment, covariates,
                                       trim = c(0.01, 0.99)) {
  ps <- .fit_propensity(data, treatment, covariates)
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
#' @param data A data frame.
#' @param treatment Name of the binary treatment column.
#' @param outcome Name of the outcome column.
#' @param covariates Character vector of covariate names.
#' @param propensity_col Optional: name of a pre-computed propensity score column.
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
  # conservative — it ignores the PS-estimation step and slightly
  # over-estimates variance. Standard for IPW packages; bootstrap or
  # `WeightIt`/`survey` for the efficient sandwich correction.
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
  mean_treated_reweighted <- sum(y[t == 1] * w_trt[t == 1]) / sum(w_trt[t == 1])
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
#' **either** the propensity model **or** the outcome model is correctly
#' specified.
#'
#' @inheritParams morie_estimate_ate
#' @param outcome_model Family for the outcome model: `"linear"` or `"logistic"`.
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

  fam <- if (outcome_model == "logistic") stats::binomial() else stats::gaussian()
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
#' The **T-learner** fits separate outcome models on treated and control
#' units, then predicts the counterfactual for each unit:
#' \eqn{\widehat{CATE}_i = \hat{\mu}_1(X_i) - \hat{\mu}_0(X_i)}{CATE_hat_i = mu_hat_1(X_i) - mu_hat_0(X_i)}.
#'
#' The **S-learner** fits one model with treatment as a feature.
#'
#' @inheritParams morie_estimate_aipw
#' @param meta_learner `"t_learner"` (default) or `"s_learner"`.
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
                          meta_learner = c("t_learner", "s_learner")) {
  outcome_model <- match.arg(outcome_model)
  meta_learner <- match.arg(meta_learner)
  fam <- if (outcome_model == "logistic") stats::binomial() else stats::gaussian()
  t <- as.numeric(data[[treatment]])

  rhs <- paste(covariates, collapse = " + ")
  formula <- stats::as.formula(paste(outcome, "~", rhs))

  if (meta_learner == "t_learner") {
    fit1 <- stats::glm(formula, data = data[t == 1, , drop = FALSE], family = fam)
    fit0 <- stats::glm(formula, data = data[t == 0, , drop = FALSE], family = fam)
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
#' Uses a binary instrument \eqn{Z} to identify the LATE (Imbens & Angrist, 1994):
#' \deqn{LATE = \frac{Cov(Y, Z)}{Cov(T, Z)}}{LATE = (Cov(Y, Z))/(Cov(T, Z))}
#'
#' With covariates, uses two-stage OLS (Wald within residuals).
#' Requires `ivreg::ivreg()` if available; otherwise falls back to the
#' closed-form Wald estimator.
#'
#' @param data A data frame.
#' @param treatment Name of the binary endogenous treatment column.
#' @param outcome Name of the outcome column.
#' @param instrument Name of the binary instrument column.
#' @param covariates Optional character vector of exogenous covariates.
#' @return Named list: `late`, `se`, `ci_lower`, `ci_upper`,
#'   `first_stage_f`, `n`.
#' @export
#' @references
#'   Imbens GW, Angrist JD (1994). Identification and estimation of local
#'   average treatment effects. *Econometrica*, 62(2), 467-475.
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
      if (!is.null(covariates)) paste("+", paste(covariates, collapse = " + ")) else ""
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
    # 2SLS via ivreg if available
    cov_str <- paste(covariates, collapse = " + ")
    iv_formula_str <- sprintf(
      "%s ~ %s + %s | %s + %s",
      outcome, treatment, cov_str, instrument, cov_str
    )
    if (requireNamespace("ivreg", quietly = TRUE)) {
      fit_iv <- ivreg::ivreg(
        stats::as.formula(iv_formula_str),
        data = data
      )
      late <- stats::coef(fit_iv)[treatment]
      se <- sqrt(stats::vcov(fit_iv)[treatment, treatment])
    } else {
      # Fallback: manual 2SLS
      t_hat <- stats::fitted(fs_fit)
      data2 <- data
      data2[[paste0(treatment, "_hat")]] <- t_hat
      rhs2 <- paste(c(paste0(treatment, "_hat"), covariates), collapse = " + ")
      ss_fit <- stats::lm(
        stats::as.formula(paste(outcome, "~", rhs2)),
        data = data2
      )
      late <- stats::coef(ss_fit)[paste0(treatment, "_hat")]
      se <- sqrt(stats::vcov(ss_fit)[
        paste0(treatment, "_hat"),
        paste0(treatment, "_hat")
      ])
    }
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
#' The E-value quantifies the minimum strength of confounding association
#' needed to fully explain away an observed treatment effect:
#' \deqn{E = RR + \sqrt{RR \cdot (RR - 1)}}{E = RR + sqrt(RR * (RR - 1))}
#'
#' For a risk ratio \eqn{RR < 1}, use \eqn{1/RR} before applying the formula.
#'
#' @param rr Risk ratio estimate (> 0). Supply > 1; if < 1, pass its reciprocal.
#' @param rr_lower Lower bound of the 95% CI (used to compute E-value for CI).
#' @return Named list: `morie_e_value`, `e_value_ci` (for the CI bound).
#' @export
#' @references
#'   VanderWeele TJ, Ding P (2017). Sensitivity analysis in observational
#'   research: introducing the E-value. *Annals of Internal Medicine*,
#'   167(4):268-274.
#' @examples
#' morie_e_value(rr = 3.9, rr_lower = 2.4)
morie_e_value <- function(rr, rr_lower = NULL) {
  compute_e <- function(r) r + sqrt(r * (r - 1))
  ev <- compute_e(rr)
  ev_ci <- if (!is.null(rr_lower)) compute_e(rr_lower) else NA_real_
  list(morie_e_value = ev, e_value_ci = ev_ci)
}


# ---------------------------------------------------------------------------
# Sensitivity analysis -- Rosenbaum bounds
# ---------------------------------------------------------------------------

#' Rosenbaum bounds sensitivity analysis
#'
#' For a range of hidden-confounding levels \eqn{\Gamma}{Gamma}, tests whether
#' the treatment effect remains significant. A large \eqn{\Gamma}{Gamma} at
#' which the result remains significant indicates robustness.
#'
#' Uses Wilcoxon signed-rank statistic bounds for matched designs.
#' For unmatched data, computes sign-score bounds.
#'
#' @param treated Numeric vector of outcomes for treated units.
#' @param control Numeric vector of outcomes for control units
#'   (may differ in length from `treated` for unmatched designs).
#' @param gamma_range Numeric vector of \eqn{\Gamma}{Gamma} values to test.
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

  results <- lapply(gamma_range, function(gamma) {
    # Sign score bounds (Rosenbaum 2002, Section 4.3)
    # Under null, each unit has outcome contribution +/-1
    y_diff <- outer(treated, control, "-")
    signs <- sign(y_diff)
    n_pairs <- n1 * n0

    # Upper bound: p-value under maximum assignment probability
    p_plus <- gamma / (1 + gamma)
    p_minus <- 1 / (1 + gamma)

    # Expected value and variance under gamma
    E_upper <- sum(p_plus * (signs > 0) + p_minus * (signs < 0))
    E_lower <- sum(p_minus * (signs > 0) + p_plus * (signs < 0))
    V <- n_pairs * p_plus * p_minus

    T_stat <- sum(signs > 0)
    p_upper <- 1 - stats::pnorm((T_stat - E_lower) / sqrt(V))
    p_lower <- 1 - stats::pnorm((T_stat - E_upper) / sqrt(V))

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
#' @inheritParams morie_estimate_aipw
#' @return Named list: `ate`, `se`, `ci_lower`, `ci_upper`.
#' @examples
#' set.seed(1)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_g_computation(df, "t", "y", "x")
#' @export
morie_estimate_g_computation <- function(data, treatment, outcome, covariates,
                                   outcome_model = c("linear", "logistic")) {
  outcome_model <- match.arg(outcome_model)
  fam <- if (outcome_model == "logistic") stats::binomial() else stats::gaussian()
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
    beta <- tryCatch(solve(A, b), error = function(e) MASS::ginv(A) %*% b)
    pred[te] <- as.numeric(Xs[te, , drop = FALSE] %*% beta) + yc
  }
  pred
}

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
#' Implements Chernozhukov et al. (2018) double/debiased machine learning
#' for the partially linear regression model. When the
#' \pkg{DoubleML} R package is installed, delegates to
#' \code{DoubleML::DoubleMLPLR} with random-forest nuisance learners.
#' Otherwise falls back to a hand-rolled cross-fit ridge implementation:
#' residualise \eqn{Y} and \eqn{D} on \eqn{X} via K-fold ridge, then
#' regress the outcome residual on the treatment residual.
#'
#' @param data A data frame with treatment, outcome, and covariate columns.
#' @param outcome Name of the continuous outcome column.
#' @param treatment Name of the (binary) treatment column.
#' @param covariates Character vector of covariate column names.
#' @param n_folds Number of cross-fitting folds (default 5).
#' @param n_rep Number of repeated cross-fitting repetitions (DoubleML only;
#'   ignored by the ridge fallback). Default 1.
#' @param random_state Integer seed for cross-fit folds and learners (default 42).
#' @return Named list with elements \code{ate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{n}, \code{method}.
#' @examples
#' set.seed(1)
#' n <- 200
#' X <- matrix(rnorm(n * 3), n, 3)
#' d <- rbinom(n, 1, plogis(X[, 1]))
#' y <- 0.5 * d + X[, 1] + rnorm(n)
#' df <- data.frame(y = y, d = d, x1 = X[, 1], x2 = X[, 2], x3 = X[, 3])
#' morie_estimate_double_ml(df, "y", "d", c("x1", "x2", "x3"))
#' @references
#' Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C.,
#' Newey, W., & Robins, J. (2018). Double/debiased machine learning for
#' treatment and structural parameters. \emph{The Econometrics Journal},
#' 21(1), C1--C68.
#' @export
morie_estimate_double_ml <- function(data, outcome, treatment, covariates,
                                     n_folds = 5L, n_rep = 1L,
                                     random_state = 42L) {
  prep <- .dml_prepare_xy(data, treatment, outcome, covariates)
  n <- nrow(prep$frame)
  z <- 1.959964

  if (requireNamespace("DoubleML", quietly = TRUE) &&
      requireNamespace("mlr3", quietly = TRUE) &&
      requireNamespace("mlr3learners", quietly = TRUE) &&
      requireNamespace("ranger", quietly = TRUE)) {
    set.seed(random_state)
    dml_data <- DoubleML::double_ml_data_from_data_frame(
      df = prep$frame,
      y_col = outcome,
      d_cols = treatment,
      x_cols = covariates
    )
    ml_l <- mlr3::lrn("regr.ranger", num.trees = 100L, max.depth = 5L)
    ml_m <- mlr3::lrn("regr.ranger", num.trees = 100L, max.depth = 5L)
    plr <- DoubleML::DoubleMLPLR$new(dml_data, ml_l = ml_l, ml_m = ml_m,
                                     n_folds = n_folds, n_rep = n_rep)
    plr$fit()
    ate <- as.numeric(plr$coef[1])
    se <- as.numeric(plr$se[1])
    return(list(
      ate = ate, se = se,
      ci_lower = ate - z * se, ci_upper = ate + z * se,
      n = n, method = "PLR (DoubleML)"
    ))
  }

  # Hand-rolled cross-fit ridge fallback.
  ml_y <- .dml_xfit_ridge(prep$X, prep$y, n_folds = n_folds,
                          random_state = random_state)
  ml_d <- .dml_xfit_ridge(prep$X, prep$d, n_folds = n_folds,
                          random_state = random_state + 1L)
  u <- prep$y - ml_y
  v <- prep$d - ml_d
  denom <- sum(v * v)
  if (denom <= 0) {
    stop("morie_estimate_double_ml: treatment residual variance is zero")
  }
  ate <- sum(v * u) / denom
  # Neyman-orthogonal score variance estimator
  psi <- v * (u - ate * v)
  se <- sqrt(mean(psi^2) / denom^2 * n) / sqrt(n)
  se <- sqrt(sum(psi^2)) / denom
  list(
    ate = ate, se = se,
    ci_lower = ate - z * se, ci_upper = ate + z * se,
    n = n, method = "PLR (cross-fit ridge fallback)"
  )
}

#' Estimate ATE via the Interactive Regression Model (IRM)
#'
#' Implements the IRM variant of Chernozhukov et al. (2018) double machine
#' learning, which allows treatment-effect heterogeneity by fitting separate
#' outcome regressions for \eqn{T=0} and \eqn{T=1} alongside a propensity
#' model. Uses \code{DoubleML::DoubleMLIRM} when available; otherwise falls
#' back to a hand-rolled cross-fit estimator using logistic regression for
#' the propensity score and ridge regression for the conditional outcome
#' regressions.
#'
#' @param data A data frame containing treatment, outcome, and covariates.
#' @param treatment Binary treatment column name.
#' @param outcome Outcome column name.
#' @param covariates Character vector of covariate column names.
#' @param n_folds Number of cross-fitting folds (default 5).
#' @param random_state Integer seed (default 42).
#' @return Named list with \code{ate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{n}, \code{method}.
#' @examples
#' set.seed(1)
#' n <- 200
#' X <- matrix(rnorm(n * 3), n, 3)
#' d <- rbinom(n, 1, plogis(X[, 1]))
#' y <- 0.5 * d + X[, 1] + rnorm(n)
#' df <- data.frame(y = y, d = d, x1 = X[, 1], x2 = X[, 2], x3 = X[, 3])
#' morie_estimate_irm(df, treatment = "d", outcome = "y",
#'                    covariates = c("x1", "x2", "x3"))
#' @references
#' Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C.,
#' Newey, W., & Robins, J. (2018). Double/debiased machine learning for
#' treatment and structural parameters. \emph{The Econometrics Journal},
#' 21(1), C1--C68.
#' @export
morie_estimate_irm <- function(data, treatment, outcome, covariates,
                               n_folds = 5L, random_state = 42L) {
  prep <- .dml_prepare_xy(data, treatment, outcome, covariates)
  n <- nrow(prep$frame)
  z <- 1.959964

  if (requireNamespace("DoubleML", quietly = TRUE) &&
      requireNamespace("mlr3", quietly = TRUE) &&
      requireNamespace("mlr3learners", quietly = TRUE) &&
      requireNamespace("ranger", quietly = TRUE)) {
    set.seed(random_state)
    irm_frame <- prep$frame
    irm_frame[[treatment]] <- as.integer(irm_frame[[treatment]])
    dml_data <- DoubleML::double_ml_data_from_data_frame(
      df = irm_frame,
      y_col = outcome,
      d_cols = treatment,
      x_cols = covariates
    )
    ml_g <- mlr3::lrn("regr.ranger", num.trees = 100L, max.depth = 5L)
    ml_m <- mlr3::lrn("classif.ranger", num.trees = 100L, max.depth = 5L,
                      predict_type = "prob")
    irm <- DoubleML::DoubleMLIRM$new(dml_data, ml_g = ml_g, ml_m = ml_m,
                                     n_folds = n_folds)
    irm$fit()
    ate <- as.numeric(irm$coef[1])
    se <- as.numeric(irm$se[1])
    return(list(
      ate = ate, se = se,
      ci_lower = ate - z * se, ci_upper = ate + z * se,
      n = n, method = "IRM (DoubleML)"
    ))
  }

  # Hand-rolled cross-fit IRM fallback.
  d <- prep$d
  y <- prep$y
  X <- prep$X
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  mu1 <- numeric(n)
  mu0 <- numeric(n)
  ps <- numeric(n)
  for (k in seq_len(n_folds)) {
    te <- which(folds == k)
    tr <- setdiff(seq_len(n), te)
    # propensity via logistic regression on training fold
    dat_tr <- data.frame(d = d[tr], X[tr, , drop = FALSE])
    dat_te <- data.frame(X[te, , drop = FALSE])
    glm_fit <- suppressWarnings(stats::glm(d ~ ., data = dat_tr,
                                           family = stats::binomial()))
    ps[te] <- .clip_ps(stats::predict(glm_fit, newdata = dat_te,
                                      type = "response"))
    # outcome regressions on T=1 and T=0 subsamples of the training fold
    tr1 <- tr[d[tr] == 1]
    tr0 <- tr[d[tr] == 0]
    if (length(tr1) >= ncol(X) + 1L) {
      mu1[te] <- .dml_xfit_ridge_predict(X[tr1, , drop = FALSE], y[tr1],
                                         X[te, , drop = FALSE])
    } else {
      mu1[te] <- mean(y[tr1])
    }
    if (length(tr0) >= ncol(X) + 1L) {
      mu0[te] <- .dml_xfit_ridge_predict(X[tr0, , drop = FALSE], y[tr0],
                                         X[te, , drop = FALSE])
    } else {
      mu0[te] <- mean(y[tr0])
    }
  }
  # Neyman-orthogonal IRM score
  psi <- (mu1 - mu0) +
    d * (y - mu1) / ps -
    (1 - d) * (y - mu0) / (1 - ps)
  ate <- mean(psi)
  se <- stats::sd(psi) / sqrt(n)
  list(
    ate = ate, se = se,
    ci_lower = ate - z * se, ci_upper = ate + z * se,
    n = n, method = "IRM (cross-fit ridge fallback)"
  )
}

# Helper: closed-form ridge fit on (X_tr, y_tr) and predict at X_te.
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
                   error = function(e) MASS::ginv(A) %*% b)
  as.numeric(Xs_te %*% beta) + yc
}
