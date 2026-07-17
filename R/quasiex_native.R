# SPDX-License-Identifier: AGPL-3.0-or-later
#
# quasiex_native.R -- module 14 unified front-ends (Phase 9).
#
# Thin routers only: every estimator, bandwidth, diagnostic, and
# robustness check below delegates to the existing native toolboxes
# (did.R/did_native.R, rdd.R, iv.R/iv_native.R). This file adds the
# brief's three unified entry points -- auto-staggered DiD, gated
# 2SLS, and diagnostics-bundled RDD -- without re-implementing any
# statistics.

#' Difference-in-differences with automatic staggered-design handling
#'
#' Estimates the ATT by two-way fixed effects when treatment timing is
#' uniform, and switches automatically to the Callaway & Sant'Anna
#' (2021) group-time estimator when adoption is staggered -- warning
#' that a naive TWFE specification would be contaminated by forbidden
#' already-treated comparisons (Goodman-Bacon 2021).
#'
#' @srrstats {G2.1} Inputs validated for presence/type below.
#' @srrstats {RE4.4} Returns estimate, SE, CI, p-value, and (for
#'   staggered designs) the full ATT(g,t) matrix.
#'
#' @param data Data frame (long panel).
#' @param outcome,unit,time Column names.
#' @param treatment_time Column giving each unit's first treated
#'   period (NA/Inf = never treated).
#' @param covariates Optional covariate names (staggered path).
#' @param n_bootstrap Multiplier-bootstrap reps for the staggered SEs.
#' @param seed RNG seed.
#' @param alpha CI level tail. Default 0.05.
#' @return Object of class \code{"morie_did"}: estimate, std.error,
#'   conf.int, p.value, att_gt (staggered only), method, n_units,
#'   n_periods, call.
#' @references Callaway & Sant'Anna (2021) J. Econometrics 225(2);
#'   Goodman-Bacon (2021) J. Econometrics 225(2).
#' @examples
#' df <- expand.grid(id = 1:30, t = 1:6)
#' df$g <- ifelse(df$id <= 15, 4L, NA)
#' df$y <- rnorm(nrow(df)) + ifelse(!is.na(df$g) & df$t >= df$g, 2, 0)
#' morie_did(df, "y", "id", "t", "g")
#' @export
morie_did <- function(data, outcome, unit, time, treatment_time,
                      covariates = NULL, n_bootstrap = 200L,
                      seed = 42L, alpha = 0.05) {
  stopifnot(is.data.frame(data))
  need <- c(outcome, unit, time, treatment_time, covariates)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols)) {
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  g <- data[[treatment_time]]
  g[!is.finite(g)] <- NA
  cohorts <- sort(unique(g[!is.na(g)]))
  n_units <- length(unique(data[[unit]]))
  n_periods <- length(unique(data[[time]]))
  if (length(cohorts) == 0L) {
    stop("No treated units: every ", treatment_time, " is NA/Inf.",
         call. = FALSE)
  }

  if (length(cohorts) > 1L) {
    warning("Staggered adoption detected (", length(cohorts),
            " treatment cohorts): a naive TWFE specification would be ",
            "contaminated by forbidden already-treated comparisons ",
            "(Goodman-Bacon 2021). Using the Callaway-Sant'Anna ",
            "group-time estimator.", call. = FALSE)
    st <- morie_did_staggered(data, outcome, unit, time, treatment_time,
                              covariates = covariates,
                              n_bootstrap = n_bootstrap, seed = seed,
                              alpha = alpha)
    # The ATT(g,t) matrix includes pre-treatment placebo cells; the
    # overall ATT (did::aggte "simple" convention) averages POST cells
    # only -- averaging the placebos in would dilute the estimate.
    gt_post <- st$group_time[st$group_time$time >= st$group_time$cohort, ,
                             drop = FALSE]
    ov <- morie_did_aggregate_gt_att(gt_post, aggregation = "overall")
    z <- stats::qnorm(1 - alpha / 2)
    out <- list(
      estimate = ov$estimate, std.error = ov$std_error,
      conf.int = c(ov$estimate - z * ov$std_error,
                   ov$estimate + z * ov$std_error),
      p.value = 2 * stats::pnorm(-abs(ov$estimate / ov$std_error)),
      att_gt = st$group_time,
      by_cohort = st$by_cohort, by_event_time = st$by_event_time,
      method = "Callaway-Sant'Anna (2021) group-time ATT, overall (post cells)",
      n_units = n_units, n_periods = n_periods, call = match.call()
    )
  } else {
    # Uniform timing: TWFE on the treated-post indicator via the
    # native demeaning engine (did_native.R).
    d <- as.numeric(!is.na(g) & data[[time]] >= g)
    X <- cbind(treated_post = d)
    fit <- .morie_did_twfe_native(as.numeric(data[[outcome]]), X,
                                  as.factor(data[[unit]]),
                                  as.factor(data[[time]]),
                                  cluster_ids = data[[unit]])
    est <- as.numeric(fit$beta[1L])
    se <- as.numeric(fit$se[1L])
    z <- stats::qnorm(1 - alpha / 2)
    out <- list(
      estimate = est, std.error = se,
      conf.int = c(est - z * se, est + z * se),
      p.value = 2 * stats::pnorm(-abs(est / se)),
      att_gt = NULL,
      method = "TWFE DiD (uniform timing), cluster-robust on unit",
      n_units = n_units, n_periods = n_periods, call = match.call()
    )
  }
  class(out) <- "morie_did"
  out
}

#' @examples
#' \donttest{
#' df <- expand.grid(id = 1:40, t = 1:8)
#' df$g <- ifelse(df$id <= 20, 5L, NA)
#' df$y <- rnorm(nrow(df)) + ifelse(!is.na(df$g) & df$t >= df$g, 2, 0)
#' obj <- morie_did_borusyak(df, "y", "id", "t", "g", n_bootstrap = 29L)
#' \references{
#' Borusyak, Jaravel & Spiess (2024) REStud 91(6).
#' print(obj)
#' }
#' @export
print.morie_did <- function(x, ...) {
  cat("Difference-in-differences --", x$method, "\n")
  cat(sprintf("  ATT: %.4f  (SE %.4f)  95%% CI [%.4f, %.4f]  p = %.3g\n",
              x$estimate, x$std.error, x$conf.int[1], x$conf.int[2],
              x$p.value))
  cat(sprintf("  Units: %d  Periods: %d\n", x$n_units, x$n_periods))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Two-stage least squares with weak-instrument protection
# ---------------------------------------------------------------------------

#' Two-stage least squares with a weak-instrument refusal gate
#'
#' Routes to the native 2SLS engine (\code{\link{morie_iv_tsls}}),
#' first-stage diagnostics
#' (\code{\link{morie_iv_first_stage_diagnostics}}), Stock-Yogo
#' critical values (\code{\link{morie_iv_stock_yogo}}), and the
#' Anderson-Rubin confidence set
#' (\code{\link{morie_iv_anderson_rubin_ci}}). Following
#' Staiger-Stock, refuses to report the 2SLS point estimate when the
#' first-stage F is below 10 and returns the identification-robust
#' Anderson-Rubin set instead.
#'
#' @srrstats {G2.1} Inputs validated below.
#' @srrstats {RE2.4} Weak instruments detected and inference switched
#'   to an identification-robust procedure rather than reported naively.
#'
#' @param data Data frame.
#' @param outcome Outcome column.
#' @param endogenous Endogenous regressor column (single).
#' @param instruments Instrument column names.
#' @param exogenous Optional exogenous covariate names.
#' @param alpha CI tail. Default 0.05.
#' @return Object of class \code{"morie_iv"}: estimate (NA when the
#'   gate refuses), std.error, conf.int, first_stage_F, stock_yogo_10,
#'   weak_instruments, ar_confidence_set, method, n, call.
#' @references Staiger & Stock (1997); Anderson & Rubin (1949);
#'   Stock & Yogo (2005).
#' @examples
#' n <- 200
#' z <- rnorm(n); u <- rnorm(n)
#' d <- z + 0.5 * u + rnorm(n)
#' y <- 2 * d + u + rnorm(n)
#' morie_iv_2sls(data.frame(y, d, z), "y", "d", "z")
#' @export
morie_iv_2sls <- function(data, outcome, endogenous, instruments,
                          exogenous = NULL, alpha = 0.05) {
  stopifnot(is.data.frame(data), length(endogenous) == 1L)
  need <- c(outcome, endogenous, instruments, exogenous)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols)) {
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  df <- stats::na.omit(data[, need, drop = FALSE])

  fs <- morie_iv_first_stage_diagnostics(df, endogenous, instruments,
                                         exogenous = exogenous)
  first_F <- as.numeric(fs$F[1L])
  # Shipped table covers 1 endogenous x 1-3 instruments; NA beyond.
  sy_crit <- tryCatch(
    as.numeric(morie_iv_stock_yogo(1L, length(instruments))[["10pct"]]),
    error = function(e) NA_real_
  )

  # Anderson-Rubin set centred on the OLS estimate's neighbourhood.
  ols_b <- stats::coef(stats::lm(
    stats::reformulate(endogenous, response = outcome), data = df
  ))[[2L]]
  half <- 10 * max(abs(ols_b), 1)
  ar <- morie_iv_anderson_rubin_ci(df, outcome, endogenous, instruments,
                                   exogenous = exogenous,
                                   grid_min = ols_b - half,
                                   grid_max = ols_b + half,
                                   grid_n = 801L, alpha = alpha)
  # Returns a length-2 numeric range (NA,NA when the set is empty).
  ar_set <- as.numeric(ar)[1:2]

  weak <- is.finite(first_F) && first_F < 10
  if (weak) {
    warning("First-stage F = ", format(first_F, digits = 3),
            " < 10 (Staiger-Stock): refusing to report the 2SLS point ",
            "estimate; use the Anderson-Rubin confidence set.",
            call. = FALSE)
    est_part <- list(estimate = NA_real_, std.error = NA_real_,
                     conf.int = c(NA_real_, NA_real_))
  } else {
    fit <- morie_iv_tsls(df, outcome, endogenous, instruments,
                         exogenous = exogenous, alpha = alpha)
    idx <- match(endogenous, fit$variable_names)
    est_part <- list(
      estimate = as.numeric(fit$coefficients[idx]),
      std.error = as.numeric(fit$std_errors[idx]),
      conf.int = c(as.numeric(fit$ci_lower[idx]),
                   as.numeric(fit$ci_upper[idx]))
    )
  }
  out <- c(est_part, list(
    first_stage_F = first_F, stock_yogo_10 = sy_crit,
    weak_instruments = weak, ar_confidence_set = ar_set,
    method = "2SLS (native k-class, HC1) with Staiger-Stock gate",
    n = nrow(df), call = match.call()
  ))
  class(out) <- "morie_iv"
  out
}

#' @examples
#' \donttest{
#' n <- 200
#' z <- rnorm(n); u <- rnorm(n)
#' d <- z + 0.5 * u + rnorm(n)
#' y <- 2 * d + u + rnorm(n)
#' obj <- morie_iv_2sls(data.frame(y, d, z), "y", "d", "z")
#' \references{
#' Staiger & Stock (1997); Anderson & Rubin (1949);
#' Stock & Yogo (2005).
#' print(obj)
#' }
#' @export
print.morie_iv <- function(x, ...) {
  cat("Two-stage least squares --", x$method, "\n")
  if (x$weak_instruments) {
    cat(sprintf("  WEAK INSTRUMENTS (first-stage F = %.2f < 10)\n",
                x$first_stage_F))
    cat(sprintf("  Anderson-Rubin 95%% confidence set: [%.4f, %.4f]\n",
                x$ar_confidence_set[1], x$ar_confidence_set[2]))
  } else {
    cat(sprintf("  Estimate: %.4f  (SE %.4f)  95%% CI [%.4f, %.4f]\n",
                x$estimate, x$std.error, x$conf.int[1], x$conf.int[2]))
    cat(sprintf("  First-stage F = %.2f (Stock-Yogo 10%% crit = %.2f)\n",
                x$first_stage_F, x$stock_yogo_10))
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# Regression discontinuity with bundled diagnostics
# ---------------------------------------------------------------------------

#' Sharp / fuzzy regression discontinuity with bundled diagnostics
#'
#' Routes to the native RDD toolbox: bias-corrected local linear
#' estimation (\code{\link{morie_rdd_bias_corrected}} /
#' \code{\link{morie_rdd_fuzzy}}), the Imbens-Kalyanaraman bandwidth
#' (\code{\link{morie_rdd_bandwidth_ik}}), the McCrary manipulation
#' test (\code{\link{morie_rdd_mccrary}}), and placebo cutoffs
#' (\code{\link{morie_rdd_placebo_cutoff}}) -- all in one call, so the
#' diagnostics researchers routinely forget arrive with the estimate.
#'
#' @srrstats {G2.1} Inputs validated below.
#' @srrstats {RE6.2} Diagnostics (manipulation + placebo cutoffs)
#'   returned with the estimate rather than left to the user.
#'
#' @param data Data frame.
#' @param outcome Outcome column.
#' @param running Running-variable column.
#' @param cutoff Threshold. Default 0.
#' @param treatment Optional treated-indicator column: when supplied,
#'   a fuzzy RDD (local Wald ratio) is estimated.
#' @param bandwidth Optional bandwidth override (numeric).
#' @param placebo_cutoffs Optional numeric vector of placebo cutoffs;
#'   defaults to the side medians.
#' @param alpha CI tail. Default 0.05.
#' @return Object of class \code{"morie_rdd"}: estimate, std.error,
#'   conf.int, p.value, bandwidth, kind (sharp/fuzzy), manipulation
#'   (statistic + p), placebo (data.frame), n, call.
#' @references Imbens & Kalyanaraman (2012); Calonico, Cattaneo &
#'   Titiunik (2014); McCrary (2008).
#' @examples
#' set.seed(1)
#' x <- runif(500, -1, 1)
#' y <- 1 + 2 * (x >= 0) + x + rnorm(500, sd = 0.5)
#' morie_rdd(data.frame(y, x), "y", "x")
#' @export
morie_rdd <- function(data, outcome, running, cutoff = 0,
                      treatment = NULL, bandwidth = NULL,
                      placebo_cutoffs = NULL, alpha = 0.05) {
  stopifnot(is.data.frame(data))
  need <- c(outcome, running, treatment)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols)) {
    stop("Columns missing from data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  df <- stats::na.omit(data[, need, drop = FALSE])
  x <- as.numeric(df[[running]])
  y <- as.numeric(df[[outcome]])
  if (is.null(bandwidth)) {
    bandwidth <- morie_rdd_bandwidth_ik(x, y, cutoff)$bandwidth
  }

  if (is.null(treatment)) {
    kind <- "sharp"
    fit <- morie_rdd_bias_corrected(df, outcome, running, cutoff = cutoff,
                                    bandwidth = bandwidth, alpha = alpha)
  } else {
    kind <- "fuzzy"
    fit <- morie_rdd_fuzzy(df, outcome, running, treatment,
                           cutoff = cutoff, bandwidth = bandwidth,
                           alpha = alpha)
  }

  manip <- morie_rdd_mccrary(x, cutoff = cutoff)
  if (is.null(placebo_cutoffs)) {
    placebo_cutoffs <- c(
      stats::median(x[x < cutoff]),
      stats::median(x[x >= cutoff])
    )
  }
  placebo <- morie_rdd_placebo_cutoff(df, outcome, running,
                                      true_cutoff = cutoff,
                                      placebo_cutoffs = placebo_cutoffs,
                                      bandwidth = bandwidth, alpha = alpha)

  out <- list(
    estimate = fit$estimate, std.error = fit$std_error,
    conf.int = c(fit$ci_lower, fit$ci_upper),
    p.value = fit$p_value,
    bandwidth = bandwidth, kind = kind,
    manipulation = list(statistic = manip$statistic,
                        p.value = manip$p_value),
    placebo = placebo,
    n = fit$n_obs, call = match.call()
  )
  class(out) <- "morie_rdd"
  out
}

#' @examples
#' \donttest{
#' set.seed(1)
#' x <- runif(500, -1, 1)
#' y <- 1 + 2 * (x >= 0) + x + rnorm(500, sd = 0.5)
#' obj <- morie_rdd(data.frame(y, x), "y", "x")
#' \references{
#' Imbens & Kalyanaraman (2012); Calonico, Cattaneo &
#' Titiunik (2014); McCrary (2008).
#' print(obj)
#' }
#' @export
print.morie_rdd <- function(x, ...) {
  cat(sprintf("Regression discontinuity (%s), bandwidth = %.4f\n",
              x$kind, x$bandwidth))
  cat(sprintf("  Estimate: %.4f  (SE %.4f)  95%% CI [%.4f, %.4f]  p = %.3g\n",
              x$estimate, x$std.error, x$conf.int[1], x$conf.int[2],
              x$p.value))
  cat(sprintf("  Manipulation check (McCrary): stat = %.2f, p = %.3g%s\n",
              x$manipulation$statistic, x$manipulation$p.value,
              if (is.finite(x$manipulation$p.value) &&
                  x$manipulation$p.value < 0.05)
                "  ** density jump at cutoff **" else ""))
  invisible(x)
}
