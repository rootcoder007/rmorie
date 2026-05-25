#' Run a weighted logistic-regression analysis
#'
#' Mirrors the Python `morie.run_weighted_logistic_analysis()`. Fits a
#' binary-outcome model using survey weights via `survey::svyglm()` if the
#' suggested `survey` package is available, otherwise falls back to base
#' `glm()` with case weights.
#'
#' @param data A `data.frame` containing outcome, predictors, and (optionally)
#'   a weights column.
#' @param outcome Column name of the binary outcome.
#' @param predictors Character vector of predictor column names.
#' @param weights_col Optional column name of analytical weights.
#'
#' @return A list with components `coefficients` (named numeric vector),
#'   `std_errors`, `p_values`, `n`, `method` ("svyglm" or "glm-weighted").
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y = rbinom(200, 1, 0.4),
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   w = runif(200, 0.5, 1.5)
#' )
#' morie_run_weighted_logistic_analysis(df,
#'   outcome = "y", predictors = c("x1", "x2"), weights_col = "w"
#' )
morie_run_weighted_logistic_analysis <- function(data, outcome, predictors,
                                           weights_col = NULL) {
  fml <- stats::as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )
  use_survey <- !is.null(weights_col) &&
    requireNamespace("survey", quietly = TRUE)

  if (use_survey) {
    design <- survey::svydesign(
      ids = ~1, data = data, weights = stats::as.formula(paste0("~", weights_col))
    )
    fit <- survey::svyglm(fml,
      design = design,
      family = stats::quasibinomial()
    )
    method <- "svyglm"
  } else {
    w <- if (!is.null(weights_col)) data[[weights_col]] else NULL
    fit <- stats::glm(fml,
      data = data, family = stats::binomial(),
      weights = w
    )
    method <- if (is.null(w)) "glm-unweighted" else "glm-weighted"
  }
  coef_summary <- stats::coef(summary(fit))
  list(
    coefficients = coef_summary[, "Estimate"],
    std_errors   = coef_summary[, "Std. Error"],
    p_values     = coef_summary[, ncol(coef_summary)],
    n            = stats::nobs(fit),
    method       = method
  )
}

#' Compare nested logistic-regression models via likelihood-ratio test
#'
#' Mirrors the Python `morie.compare_nested_logistic_models()`. Fits a
#' reduced and a full logistic model (the reduced model's predictors must be
#' a subset of the full model's), then performs an analysis-of-deviance LRT.
#'
#' @param data A `data.frame`.
#' @param outcome Column name of the binary outcome.
#' @param predictors_full Character vector: full model's predictors.
#' @param predictors_reduced Character vector: reduced model's predictors.
#'   Must be a subset of `predictors_full`.
#'
#' @return A list with `chi_sq`, `df`, `p_value`, `aic_full`, `aic_reduced`,
#'   `n`.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y = rbinom(200, 1, 0.4),
#'   x1 = rnorm(200), x2 = rnorm(200), x3 = rnorm(200)
#' )
#' morie_compare_nested_logistic_models(df,
#'   outcome = "y",
#'   predictors_full = c("x1", "x2", "x3"),
#'   predictors_reduced = c("x1")
#' )
morie_compare_nested_logistic_models <- function(data, outcome,
                                           predictors_full,
                                           predictors_reduced) {
  if (!all(predictors_reduced %in% predictors_full)) {
    stop("predictors_reduced must be a subset of predictors_full.",
      call. = FALSE
    )
  }

  fml_full <- stats::as.formula(
    paste(outcome, "~", paste(predictors_full, collapse = " + "))
  )
  fml_red <- stats::as.formula(
    paste(outcome, "~", paste(predictors_reduced, collapse = " + "))
  )

  fit_full <- stats::glm(fml_full,
    data = data,
    family = stats::binomial()
  )
  fit_red <- stats::glm(fml_red,
    data = data,
    family = stats::binomial()
  )

  chi_sq <- as.numeric(stats::deviance(fit_red) - stats::deviance(fit_full))
  df <- length(predictors_full) - length(predictors_reduced)
  p_val <- stats::pchisq(chi_sq, df = df, lower.tail = FALSE)

  list(
    chi_sq      = chi_sq,
    df          = df,
    p_value     = p_val,
    aic_full    = stats::AIC(fit_full),
    aic_reduced = stats::AIC(fit_red),
    n           = stats::nobs(fit_full)
  )
}

#' Run a treatment-effects analysis (point estimate, SE, 95% CI)
#'
#' Mirrors the Python `morie.run_treatment_effects_analysis()`. Convenience
#' wrapper around [morie_estimate_ate()] that also produces a 95% confidence
#' interval (delta-method approximation).
#'
#' @param data A `data.frame`.
#' @param treatment Column name of the binary treatment.
#' @param outcome Column name of the outcome.
#' @param covariates Character vector of covariate column names.
#'
#' @return A list with `ate`, `se`, `ci_lower`, `ci_upper`, `n`, `method`.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y = rnorm(200),
#'   t = rbinom(200, 1, 0.5),
#'   x1 = rnorm(200), x2 = rnorm(200)
#' )
#' morie_run_treatment_effects_analysis(df,
#'   treatment = "t", outcome = "y", covariates = c("x1", "x2")
#' )
morie_run_treatment_effects_analysis <- function(data, treatment, outcome,
                                           covariates) {
  ate_res <- morie_estimate_ate(data,
    treatment = treatment, outcome = outcome,
    covariates = covariates
  )
  list(
    ate      = ate_res$ate,
    se       = ate_res$se,
    ci_lower = ate_res$ci_lower,
    ci_upper = ate_res$ci_upper,
    n        = ate_res$n,
    method   = "Hajek IPW ATE (Wald CI)"
  )
}


# --- APPENDED 2026-05-22 -----------------------------------------------------
# Treatment-effects upgrades.  Replaces the previous
# morie_run_treatment_effects_analysis() so that it returns ATT, ATC, and
# CATE subgroup breakdowns in addition to ATE+SE+CI -- mirroring the Python
# investigation.run_treatment_effects_analysis() outputs.  Also exposes
# .morie_hajek_ate as a package-internal helper.
# ----------------------------------------------------------------------------

#' Hajek (normalised Horvitz-Thompson) ATE estimator
#'
#' Internal helper, mirrors Python ``investigation._hajek_ate``.  Operates
#' on raw vectors: clipped propensity scores ``ps``, binary treatment ``t``,
#' outcome ``y``.  Returns a list with ``ate`` / ``y1`` / ``y0``.
#'
#' @param ps Numeric vector of propensity scores in (0, 1).
#' @param t Numeric vector of 0/1 treatment indicators.
#' @param y Numeric vector of outcomes.
#' @return list(ate, y1, y0).
#' @keywords internal
#' @export
.morie_hajek_ate <- function(ps, t, y) {
  treated <- t == 1
  control <- t == 0
  sum_ty <- sum(y[treated] / ps[treated])
  sum_t  <- sum(1 / ps[treated])
  y1 <- if (sum_t > 0) sum_ty / sum_t else NA_real_
  sum_cy <- sum(y[control] / (1 - ps[control]))
  sum_c  <- sum(1 / (1 - ps[control]))
  y0 <- if (sum_c > 0) sum_cy / sum_c else NA_real_
  list(ate = y1 - y0, y1 = y1, y0 = y0)
}

# Internal: fit a logistic propensity model and clip to [0.01, 0.99].
.morie_fit_propensity <- function(data, treatment, covariates) {
  fml <- stats::as.formula(paste(treatment, "~",
                                  paste(covariates, collapse = " + ")))
  fit <- stats::glm(fml, data = data, family = stats::binomial())
  ps <- stats::predict(fit, type = "response")
  pmin(pmax(ps, 0.01), 0.99)
}

#' Run an extended treatment-effects analysis (ATE / ATT / ATC + CATE)
#'
#' R port of ``investigation.run_treatment_effects_analysis``.  Returns a
#' list with:
#' \itemize{
#'   \item ``treatment_effects_summary`` -- data.frame of ATE / ATT / ATC
#'     point estimates (SE / CI columns present but NA, matching Python).
#'   \item ``cate_subgroup_estimates`` -- data.frame of within-subgroup
#'     Hajek IPW CATEs with sandwich-style SEs and Wald 95\\% CIs.
#'   \item ``analysis_frame`` -- the trimmed data.frame with attached
#'     propensity score and IPW weight columns (``ps``, ``w_ate``, ``w_att``,
#'     ``w_atc``).
#'   \item Convenience scalars ``ate`` / ``att`` / ``atc`` / ``se`` /
#'     ``ci_lower`` / ``ci_upper`` / ``n`` / ``method`` preserved from the
#'     previous R surface for backward compatibility.
#' }
#'
#' @param data data.frame containing treatment, outcome, and covariates.
#' @param treatment Treatment column name.
#' @param outcome Outcome column name.
#' @param covariates Character vector of covariate column names.
#' @return Named list as described above.
#' @export
morie_run_treatment_effects_analysis <- function(data, treatment, outcome,
                                                  covariates) {
  required <- unique(c(treatment, outcome, covariates))
  if (!all(required %in% names(data))) {
    stop("data is missing required columns: ",
         paste(setdiff(required, names(data)), collapse = ", "))
  }
  frame <- data[stats::complete.cases(data[, required, drop = FALSE]), ,
                drop = FALSE]
  frame$ps <- .morie_fit_propensity(frame, treatment, covariates)
  t <- as.numeric(frame[[treatment]])
  y <- as.numeric(frame[[outcome]])
  ps <- frame$ps

  frame$w_ate <- ifelse(t == 1, 1 / ps, 1 / (1 - ps))
  frame$w_att <- ifelse(t == 1, 1, ps / (1 - ps))
  frame$w_atc <- ifelse(t == 1, (1 - ps) / ps, 1)

  ate_h <- .morie_hajek_ate(ps, t, y)
  ate   <- ate_h$ate

  treated <- t == 1
  control <- t == 0
  y1_mean <- if (any(treated)) mean(y[treated]) else NA_real_
  y0_mean <- if (any(control)) mean(y[control]) else NA_real_

  ps_c <- ps[control]
  y_c <- y[control]
  att_w <- ps_c / (1 - ps_c)
  y0_att <- if (sum(att_w) > 0) sum(y_c * att_w) / sum(att_w) else NA_real_
  att <- y1_mean - y0_att

  ps_t <- ps[treated]
  y_t <- y[treated]
  atc_w <- (1 - ps_t) / ps_t
  y1_atc <- if (sum(atc_w) > 0) sum(y_t * atc_w) / sum(atc_w) else NA_real_
  atc <- y1_atc - y0_mean

  summary_df <- data.frame(
    estimand = c("ATE", "ATT", "ATC"),
    method   = "IPW-Hajek",
    estimate = c(ate, att, atc),
    se       = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    stringsAsFactors = FALSE
  )

  # --- CATE subgroup estimates (Hajek IPW within each subgroup level) ----
  cate_rows <- list()
  for (sv in covariates) {
    levels_sv <- unique(frame[[sv]])
    for (lv in levels_sv) {
      sub <- frame[frame[[sv]] == lv, , drop = FALSE]
      st <- as.numeric(sub[[treatment]])
      sy <- as.numeric(sub[[outcome]])
      sps <- sub$ps
      n_t <- sum(st == 1)
      n_c <- sum(st == 0)
      if (n_t < 2 || n_c < 2) next
      h <- .morie_hajek_ate(sps, st, sy)
      ps_tt <- sps[st == 1]
      ps_cc <- sps[st == 0]
      y_tt  <- sy[st == 1]
      y_cc  <- sy[st == 0]
      w_t <- 1 / ps_tt
      w_c <- 1 / (1 - ps_cc)
      r_t <- y_tt - h$y1
      r_c <- y_cc - h$y0
      var_y1 <- if (sum(w_t) > 0) sum(w_t^2 * r_t^2) / (sum(w_t)^2) else 0
      var_y0 <- if (sum(w_c) > 0) sum(w_c^2 * r_c^2) / (sum(w_c)^2) else 0
      se <- sqrt(var_y1 + var_y0)
      cate_rows[[length(cate_rows) + 1L]] <- data.frame(
        subgroup_var   = sv,
        subgroup_level = as.character(lv),
        n_treated      = n_t,
        n_control      = n_c,
        cate           = h$ate,
        se             = se,
        ci_lower       = h$ate - 1.96 * se,
        ci_upper       = h$ate + 1.96 * se,
        stringsAsFactors = FALSE
      )
    }
  }
  cate_df <- if (length(cate_rows) > 0L) do.call(rbind, cate_rows) else
    data.frame(subgroup_var = character(0), subgroup_level = character(0),
               n_treated = integer(0), n_control = integer(0),
               cate = numeric(0), se = numeric(0),
               ci_lower = numeric(0), ci_upper = numeric(0),
               stringsAsFactors = FALSE)

  # Backward-compatible Wald CI on ATE via the existing morie_estimate_ate()
  # helper (sandwich SE).  Wrapped in try() so a missing dependency doesn't
  # abort the richer result.
  wald <- tryCatch(
    morie_estimate_ate(data, treatment = treatment, outcome = outcome,
                       covariates = covariates),
    error = function(e) list(se = NA_real_, ci_lower = NA_real_,
                             ci_upper = NA_real_, n = nrow(frame)))

  list(
    # New rich outputs (mirrors Python)
    treatment_effects_summary = summary_df,
    cate_subgroup_estimates   = cate_df,
    analysis_frame            = frame,
    # Back-compat scalars
    ate      = ate,
    att      = att,
    atc      = atc,
    se       = wald$se,
    ci_lower = wald$ci_lower,
    ci_upper = wald$ci_upper,
    n        = nrow(frame),
    method   = "IPW-Hajek (ATE/ATT/ATC + CATE)"
  )
}
