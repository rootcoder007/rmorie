# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie survival -- time-to-event analysis: KM, Nelson-Aalen, Cox, residuals,
# parametric / AFT models, competing risks, RMST, concordance.
#
# R port of src/morie/survival.py. Wraps the recommended `survival` package
# and the optional `cmprsk` package. Hand-rolls a Greenwood-CI KM that
# matches the Python output exactly (so downstream tests cross-check).
#
# References:
#   Therneau (2023). survival: A Package for Survival Analysis in R.
#   Fine & Gray (1999). JASA, 94(446), 496-509.
#   Kalbfleisch & Prentice (2002). The Statistical Analysis of Failure Time Data.

#' Shared parameters for morie_survival_* estimators
#'
#' Roxygen-only stub holding the @param entries shared across the
#' survival family (KM, Nelson-Aalen, Cox, RMST, AFT, parametric,
#' Fine-Gray competing risks, Turnbull, landmark, concordance).
#' Functions reference these via `@inheritParams morie_survival_params`.
#'
#' @param time Numeric vector of event/censoring times.
#' @param event Integer/logical vector; 1 = event, 0 = censored.
#' @param group Factor/character grouping variable for HR / log-rank
#'   stratified comparisons.
#' @param confidence Confidence level for interval estimates (default
#'   `0.95`).
#' @param tau RMST truncation horizon (`morie_survival_rmst`/`rmst_diff`).
#' @param dist Distribution name for parametric/AFT fits (e.g.
#'   `"weibull"`, `"lognormal"`, `"loglogistic"`).
#' @param risk_score Numeric vector of predicted risk scores aligned
#'   with `time` for concordance / discrimination metrics.
#' @param cox_result A `coxph`-style fit (as returned by
#'   `morie_survival_cox`); used by residual diagnostics
#'   (`coxsnell`, `martingale`, `deviance`).
#' @param data A `data.frame` whose columns supply `duration_col`,
#'   `event_col`, `covariate_cols`, etc.
#' @param duration_col Character; column name of the event/censoring
#'   time in `data`.
#' @param event_col Character; column name of the event-indicator
#'   variable in `data`.
#' @param covariate_cols Character vector of covariate column names.
#' @param landmark_time Numeric landmark time at which to subset
#'   the cohort before fitting (lead-time bias correction).
#' @param event_of_interest Integer event-type code for competing-
#'   risks analyses (Fine-Gray, CIF).
#' @param time1 Time vector for group 1 (`rmst_diff`).
#' @param event1 Event vector for group 1 (`rmst_diff`).
#' @param time2 Time vector for group 2 (`rmst_diff`).
#' @param event2 Event vector for group 2 (`rmst_diff`).
#' @param entry_time Left-truncation entry times.
#' @param exit_time Exit (event/censoring) times for the
#'   left-truncated KM estimator.
#' @param left Left-bracket times for interval-censored data
#'   (`morie_survival_turnbull`).
#' @param right Right-bracket times for interval-censored data.
#' @param max_iter Iteration cap for the Turnbull NPMLE EM loop.
#' @param tol Convergence tolerance for the Turnbull EM.
#' @keywords internal
#' @name morie_survival_params
NULL


.req_survival <- function() {
  morie_ensure_extras("survival")
}

.req_cmprsk <- function() {
  morie_ensure_extras("cmprsk")
}

.validate_te <- function(time, event) {
  t <- as.numeric(time)
  e <- as.numeric(event)
  if (length(t) != length(e))
    stop("time and event must have equal length.", call. = FALSE)
  ok <- is.finite(t) & is.finite(e) & t >= 0
  list(time = t[ok], event = as.integer(e[ok]), ok = ok)
}

#' Kaplan-Meier product-limit survival estimator.
#'
#' Thin wrapper around `survival::survfit()` returning a tidy list with
#' Greenwood or complementary-log-log confidence bands.
#'
#' @param time Numeric vector of observation times.
#' @param event 0/1 event indicator (1 = event observed).
#' @param confidence Confidence level (default 0.95).
#' @param ci_method "greenwood" (plain) or "log-log".
#' @return list with `times`, `survival`, `ci_lower`, `ci_upper`,
#'   `at_risk`, `events`, `censored`, `median_survival`, `method`.
#' @export
morie_survival_km <- function(time, event, confidence = 0.95,
                              ci_method = c("greenwood", "log-log")) {
  ci_method <- match.arg(ci_method)
  .req_survival()
  v <- .validate_te(time, event)
  fit <- survival::survfit(
    survival::Surv(v$time, v$event) ~ 1,
    conf.int = confidence,
    conf.type = if (ci_method == "log-log") "log-log" else "plain",
    error = "greenwood"
  )
  ev_mask <- fit$n.event > 0
  list(
    times = fit$time[ev_mask],
    survival = fit$surv[ev_mask],
    ci_lower = fit$lower[ev_mask],
    ci_upper = fit$upper[ev_mask],
    at_risk = fit$n.risk[ev_mask],
    events = fit$n.event[ev_mask],
    censored = fit$n.censor[ev_mask],
    median_survival = unname(summary(fit)$table["median"]),
    method = sprintf("Kaplan-Meier (%s CI)", ci_method)
  )
}

#' Nelson-Aalen cumulative-hazard estimator.
#'
#' @inheritParams morie_survival_km
#' @return list with `times`, `cumhaz`, `ci_lower`, `ci_upper`,
#'   `at_risk`, `events`, `censored`.
#' @export
morie_survival_nelsonaalen <- function(time, event, confidence = 0.95) {
  .req_survival()
  v <- .validate_te(time, event)
  fit <- survival::survfit(survival::Surv(v$time, v$event) ~ 1,
                           conf.int = confidence, type = "fh")
  ev <- fit$n.event > 0
  ch <- -log(fit$surv[ev])
  se <- sqrt(cumsum(fit$n.event[ev] / fit$n.risk[ev]^2))
  z <- qnorm((1 + confidence) / 2)
  list(
    times = fit$time[ev],
    cumhaz = ch,
    ci_lower = pmax(ch - z * se, 0),
    ci_upper = ch + z * se,
    at_risk = fit$n.risk[ev],
    events = fit$n.event[ev],
    censored = fit$n.censor[ev],
    method = "Nelson-Aalen"
  )
}

#' Log-rank family tests (logrank / Peto-Peto / Gehan / Tarone-Ware).
#'
#' Delegates to `survival::survdiff()` for the standard log-rank weight (rho=0)
#' and Peto-Peto (rho=1). Gehan/Tarone-Ware are not supported by `survdiff`
#' directly and currently fall back to rho=1 (Peto) as the closest analogue;
#' use `survival::survdiff(..., rho=1)` plus FH weights for exact equivalents.
#'
#' @param time,event,group Vectors.
#' @param weight One of "logrank", "peto", "gehan", "tarone".
#' @export
morie_survival_logrank <- function(time, event, group,
                                   weight = c("logrank", "peto", "gehan", "tarone")) {
  .req_survival()
  weight <- match.arg(weight)
  v <- .validate_te(time, event)
  # Align group with the SAME rows that v kept; previously this
  # took the first length(v$time) entries of group, which desyncs
  # whenever any row was filtered.
  g <- group[v$ok]
  rho <- switch(weight, logrank = 0, peto = 1, gehan = 1, tarone = 1)
  sd <- survival::survdiff(survival::Surv(v$time, v$event) ~ g, rho = rho)
  df <- length(sd$n) - 1
  list(
    method = switch(weight, logrank = "Log-rank test",
                    peto = "Peto-Peto test",
                    gehan = "Gehan-Wilcoxon test (rho=1 approx)",
                    tarone = "Tarone-Ware test (rho=1 approx)"),
    test_statistic = as.numeric(sd$chisq),
    p_value = pchisq(as.numeric(sd$chisq), df, lower.tail = FALSE),
    df = df,
    n_groups = length(sd$n),
    n_total = sum(sd$n)
  )
}

#' Cox proportional hazards model.
#'
#' Wraps `survival::coxph()` with Efron (default) or Breslow tie handling
#' and returns a tidy list including hazard ratios, CIs, p-values, and the
#' Breslow baseline cumulative hazard.
#'
#' @param data data.frame.
#' @param duration_col Name of the time column.
#' @param event_col Name of the 0/1 event column.
#' @param covariate_cols Character vector of covariate column names.
#' @param ties "efron" (default) or "breslow".
#' @param confidence Confidence level (default 0.95).
#' @param penalizer L2 penalty (passed via `ridge()` term in the formula).
#' @export
morie_survival_cox <- function(data, duration_col, event_col, covariate_cols,
                               ties = c("efron", "breslow"),
                               confidence = 0.95, penalizer = 0) {
  .req_survival()
  ties <- match.arg(ties)
  needed <- c(duration_col, event_col, covariate_cols)
  df <- data[stats::complete.cases(data[, needed]), needed, drop = FALSE]
  cov_expr <- paste(covariate_cols, collapse = " + ")
  if (penalizer > 0) {
    cov_expr <- sprintf("ridge(%s, theta=%g)",
                        paste(covariate_cols, collapse = ", "), penalizer)
  }
  fml <- stats::as.formula(sprintf("survival::Surv(%s, %s) ~ %s",
                                   duration_col, event_col, cov_expr))
  fit <- survival::coxph(fml, data = df, ties = ties)
  s <- summary(fit, conf.int = confidence)
  bh <- survival::basehaz(fit, centered = FALSE)
  list(
    coefficients = stats::coef(fit),
    standard_errors = sqrt(diag(stats::vcov(fit))),
    hazard_ratios = exp(stats::coef(fit)),
    z_scores = s$coefficients[, "z"],
    p_values = s$coefficients[, "Pr(>|z|)"],
    ci_lower = s$conf.int[, 3],
    ci_upper = s$conf.int[, 4],
    covariate_names = covariate_cols,
    concordance = unname(s$concordance[1]),
    log_likelihood = stats::logLik(fit)[[1]],
    n_events = fit$nevent,
    n_observations = fit$n,
    method = sprintf("Cox PH (%s ties)", ties),
    baseline_hazard = data.frame(time = bh$time, cumulative_hazard = bh$hazard),
    .coxph = fit
  )
}

#' Schoenfeld residuals + PH-assumption test.
#'
#' Wraps `survival::cox.zph()` (scaled Schoenfeld residuals).
#' @param cox_result Object returned by `morie_survival_cox()`.
#' @export
morie_survival_schoenfeld <- function(cox_result) {
  .req_survival()
  if (is.null(cox_result$.coxph))
    stop("cox_result must come from morie_survival_cox().", call. = FALSE)
  zph <- survival::cox.zph(cox_result$.coxph)
  list(
    residuals = stats::residuals(cox_result$.coxph, type = "schoenfeld"),
    scaled = stats::residuals(cox_result$.coxph, type = "scaledsch"),
    zph_table = as.data.frame(zph$table)
  )
}

#' Cox-Snell residuals from a fitted morie Cox model.
#' @inheritParams morie_survival_params
#' @export
morie_survival_coxsnell <- function(cox_result) {
  .req_survival()
  if (is.null(cox_result$.coxph))
    stop("cox_result must come from morie_survival_cox().", call. = FALSE)
  # CS residual = delta_i - M_i (per-row event indicator minus martingale)
  status <- cox_result$.coxph$y[, "status"]
  as.numeric(status - stats::residuals(cox_result$.coxph,
                                       type = "martingale"))
}

#' Martingale residuals.
#' @inheritParams morie_survival_params
#' @export
morie_survival_martingale <- function(cox_result) {
  .req_survival()
  if (is.null(cox_result$.coxph))
    stop("cox_result must come from morie_survival_cox().", call. = FALSE)
  stats::residuals(cox_result$.coxph, type = "martingale")
}

#' Deviance residuals.
#' @inheritParams morie_survival_params
#' @export
morie_survival_deviance <- function(cox_result) {
  .req_survival()
  if (is.null(cox_result$.coxph))
    stop("cox_result must come from morie_survival_cox().", call. = FALSE)
  stats::residuals(cox_result$.coxph, type = "deviance")
}

#' Accelerated failure time model (parametric).
#'
#' Wraps `survival::survreg()`. Supported `dist`: "weibull", "lognormal",
#' "loglogistic", "exponential", "gaussian".
#' @inheritParams morie_survival_params
#' @export
morie_survival_aft <- function(data, duration_col, event_col, covariate_cols,
                               dist = c("weibull", "lognormal", "loglogistic",
                                        "exponential", "gaussian")) {
  .req_survival()
  dist <- match.arg(dist)
  needed <- c(duration_col, event_col, covariate_cols)
  df <- data[stats::complete.cases(data[, needed]), needed, drop = FALSE]
  fml <- stats::as.formula(sprintf("survival::Surv(%s, %s) ~ %s",
                                   duration_col, event_col,
                                   paste(covariate_cols, collapse = " + ")))
  fit <- survival::survreg(fml, data = df, dist = dist)
  s <- summary(fit)
  list(
    distribution = paste0("AFT-", dist),
    coefficients = stats::coef(fit),
    scale = fit$scale,
    log_likelihood = fit$loglik[2],
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    n_observations = nrow(df),
    n_events = sum(df[[event_col]] == 1),
    .survreg = fit
  )
}

#' Simple parametric survival models (intercept-only).
#'
#' For "exponential", "weibull", "lognormal", "loglogistic", "gaussian".
#' Use `morie_survival_aft()` for covariate-adjusted parametric models.
#' @inheritParams morie_survival_params
#' @export
morie_survival_parametric <- function(time, event,
                                       dist = c("weibull", "exponential",
                                                "lognormal", "loglogistic",
                                                "gaussian")) {
  .req_survival()
  dist <- match.arg(dist)
  v <- .validate_te(time, event)
  fit <- survival::survreg(survival::Surv(v$time, v$event) ~ 1, dist = dist)
  list(
    distribution = dist,
    coefficients = stats::coef(fit),
    scale = fit$scale,
    log_likelihood = fit$loglik[2],
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    n_observations = length(v$time),
    n_events = sum(v$event == 1)
  )
}

#' Harrell's concordance index (C-statistic).
#'
#' Uses `survival::concordance()` (which handles ties + censoring correctly).
#' @inheritParams morie_survival_params
#' @export
morie_survival_concordance <- function(time, event, risk_score) {
  .req_survival()
  v <- .validate_te(time, event)
  rs <- risk_score[v$ok]
  c_obj <- survival::concordance(
    survival::Surv(v$time, v$event) ~ rs, reverse = TRUE
  )
  as.numeric(c_obj$concordance)
}

#' Restricted Mean Survival Time (RMST).
#'
#' Integrates the Kaplan-Meier estimator from 0 to `tau` using trapezoidal
#' integration on the step-function. SE follows the Klein-Moeschberger
#' formula (approximation matches the Python module).
#' @inheritParams morie_survival_params
#' @export
morie_survival_rmst <- function(time, event, tau = NULL, confidence = 0.95) {
  .req_survival()
  v <- .validate_te(time, event)
  fit <- survival::survfit(survival::Surv(v$time, v$event) ~ 1)
  if (is.null(tau)) tau <- max(fit$time)
  s <- summary(fit, rmean = tau)$table
  rmst <- as.numeric(s["rmean"])
  se <- as.numeric(s["se(rmean)"])
  z <- qnorm((1 + confidence) / 2)
  list(rmst = rmst, se = se,
       ci_lower = rmst - z * se, ci_upper = rmst + z * se, tau = tau)
}

#' Difference in RMST between two groups.
#' @inheritParams morie_survival_params
#' @export
morie_survival_rmst_diff <- function(time1, event1, time2, event2,
                                     tau = NULL, confidence = 0.95) {
  r1 <- morie_survival_rmst(time1, event1, tau = tau, confidence = confidence)
  r2 <- morie_survival_rmst(time2, event2, tau = r1$tau, confidence = confidence)
  diff <- r1$rmst - r2$rmst
  se <- sqrt(r1$se^2 + r2$se^2)
  z <- if (se > 0) diff / se else 0
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  zc <- qnorm((1 + confidence) / 2)
  list(rmst_diff = diff, se = se, z = z, p_value = p,
       ci_lower = diff - zc * se, ci_upper = diff + zc * se,
       rmst_group1 = r1$rmst, rmst_group2 = r2$rmst, tau = r1$tau)
}

#' Cumulative incidence function (Aalen-Johansen) for competing risks.
#'
#' Wraps `survival::survfit()` with multi-state `Surv()`.
#' @param event Integer event code: 0 = censored, 1 = event of interest,
#'   >=2 = competing event.
#' @inheritParams morie_survival_params
#' @export
morie_survival_cif <- function(time, event, event_of_interest = 1L,
                               confidence = 0.95) {
  .req_survival()
  t <- as.numeric(time)
  e <- factor(event, levels = c(0, sort(unique(event[event != 0]))))
  e[e == 0] <- NA
  e <- droplevels(e)
  status <- factor(ifelse(event == 0, "censor",
                          ifelse(event == event_of_interest,
                                 "event", "competing")),
                   levels = c("censor", "event", "competing"))
  # 3MMM.26 (2026-05-25): type='mstate' is deprecated in survival
  # >=3.5. Modern API: pass status as a factor whose first level is
  # the censoring code and survival auto-detects multi-state. We
  # already build `status` as a factor with "censor" first (L344-347).
  fit <- survival::survfit(survival::Surv(t, status) ~ 1,
                           conf.int = confidence)
  ev_idx <- which(colnames(fit$pstate) == "event")
  list(times = fit$time,
       cif = fit$pstate[, ev_idx],
       ci_lower = fit$lower[, ev_idx],
       ci_upper = fit$upper[, ev_idx],
       event_of_interest = event_of_interest,
       n_total = length(t),
       method = "Aalen-Johansen")
}

#' Fine-Gray subdistribution hazard model (competing risks).
#'
#' Requires the `cmprsk` package.
#' @inheritParams morie_survival_params
#' @export
morie_survival_finegray <- function(data, duration_col, event_col,
                                    covariate_cols, event_of_interest = 1L,
                                    confidence = 0.95) {
  .req_cmprsk()
  needed <- c(duration_col, event_col, covariate_cols)
  df <- data[stats::complete.cases(data[, needed]), needed, drop = FALSE]
  fit <- cmprsk::crr(
    ftime = df[[duration_col]],
    fstatus = df[[event_col]],
    cov1 = as.matrix(df[, covariate_cols, drop = FALSE]),
    failcode = event_of_interest,
    cencode = 0L
  )
  s <- summary(fit, conf.int = confidence)
  # stats::coef(fit) goes through coef.crr (defined below) -> fit$coef.
  list(
    coefficients = stats::coef(fit),
    standard_errors = sqrt(diag(fit$var)),
    hazard_ratios = exp(stats::coef(fit)),
    p_values = s$coef[, "p-value"],
    ci_lower = s$conf.int[, 3],
    ci_upper = s$conf.int[, 4],
    covariate_names = covariate_cols,
    n_events = sum(df[[event_col]] == event_of_interest),
    n_observations = nrow(df),
    method = "Fine-Gray subdistribution hazard"
  )
}

#' S3 coef method for cmprsk::crr objects.
#'
#' cmprsk::crr does not ship a `coef.crr` method, so a bare
#' `stats::coef()` on a `crr` fit falls through to `coef.default()`
#' and returns `NULL`. morie registers this method (via S3method
#' in NAMESPACE, generated by roxygen `@exportS3Method`) so the
#' standard accessor returns the fitted coefficient vector for any
#' caller -- not just `morie_survival_finegray`.
#'
#' @param object A `cmprsk::crr` fit.
#' @param ... Ignored.
#' @return Named numeric vector of regression coefficients.
#' @exportS3Method stats::coef crr
coef.crr <- function(object, ...) {
  object$coef
}

#' Hazard ratio between two groups via a simple Cox model.
#' @inheritParams morie_survival_params
#' @export
morie_survival_hr <- function(time, event, group, confidence = 0.95) {
  .req_survival()
  v <- .validate_te(time, event)
  g <- group[v$ok]
  if (length(unique(g)) != 2)
    stop("hazard_ratio requires exactly 2 groups.", call. = FALSE)
  x <- as.integer(g == sort(unique(g))[2])
  d <- data.frame(time = v$time, event = v$event, grp = x)
  res <- morie_survival_cox(d, "time", "event", "grp",
                            confidence = confidence)
  list(hr = res$hazard_ratios[[1]],
       ci_lower = res$ci_lower[[1]],
       ci_upper = res$ci_upper[[1]],
       p_value = res$p_values[[1]],
       log_hr = res$coefficients[[1]],
       se = res$standard_errors[[1]])
}

#' Landmark dataset constructor.
#' @inheritParams morie_survival_params
#' @export
morie_survival_landmark <- function(data, duration_col, event_col, landmark_time) {
  df <- data[data[[duration_col]] >= landmark_time, , drop = FALSE]
  df[[duration_col]] <- df[[duration_col]] - landmark_time
  df
}

#' Left-truncated Kaplan-Meier with delayed entry.
#' @inheritParams morie_survival_params
#' @export
morie_survival_left_truncated_km <- function(entry_time, exit_time, event,
                                             confidence = 0.95) {
  .req_survival()
  fit <- survival::survfit(
    survival::Surv(entry_time, exit_time, event) ~ 1,
    conf.int = confidence
  )
  ev <- fit$n.event > 0
  list(times = fit$time[ev], survival = fit$surv[ev],
       ci_lower = fit$lower[ev], ci_upper = fit$upper[ev],
       at_risk = fit$n.risk[ev], events = fit$n.event[ev],
       censored = fit$n.censor[ev],
       method = "Left-truncated Kaplan-Meier")
}

#' Compare parametric survival models by AIC/BIC.
#' @inheritParams morie_survival_params
#' @export
morie_survival_compare_parametric <- function(time, event) {
  dists <- c("exponential", "weibull", "lognormal", "loglogistic", "gaussian")
  out <- lapply(dists, function(d) {
    res <- tryCatch(
      morie_survival_parametric(time, event, dist = d),
      error = function(e) NULL
    )
    if (is.null(res)) return(NULL)
    data.frame(distribution = d,
               log_likelihood = res$log_likelihood,
               aic = res$aic, bic = res$bic,
               n_events = res$n_events,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  out[order(out$aic), , drop = FALSE]
}

#' Turnbull NPMLE for interval-censored data.
#'
#' Delegates to `survival::survfit()` with `Surv(left, right, type = "interval2")`.
#' Hand-rolled EM is left as a stub for environments without `survival`.
#' @inheritParams morie_survival_params
#' @export
morie_survival_turnbull <- function(left, right, max_iter = 200, tol = 1e-6) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("NotYetPorted: hand-rolled Turnbull EM not implemented; install 'survival'.",
         call. = FALSE)
  }
  L <- as.numeric(left)
  R <- as.numeric(right)
  fit <- survival::survfit(survival::Surv(L, R, type = "interval2") ~ 1)
  list(times = fit$time, survival = fit$surv, method = "Turnbull NPMLE")
}
