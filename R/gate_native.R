# GATE: group average treatment effects by AIPW within strata.
# Sources: Robins, J. M., Rotnitzky, A. and Zhao, L. P. (1994),
# Estimation of regression coefficients when some regressors are not
# always observed, JASA 89(427), 846-866 (the augmented IPW estimator
# that is doubly robust); Chernozhukov, V., Chetverikov, D., Demirer,
# M., Duflo, E., Hansen, C., Newey, W. and Robins, J. (2018),
# Double/debiased machine learning, Econometrics Journal 21(1),
# C1-C68 (the "group average treatment effect" target, their Sec. 2,
# estimated within pre-specified strata).
#
# The estimator is deliberately stratified rather than pooled: fitting
# AIPW separately inside each group lets both the propensity and the
# outcome model differ across groups, so heterogeneity in the
# NUISANCE functions cannot be mistaken for heterogeneity in the
# effect.
#
# Native implementation mirroring Python morie.fn.gate exactly: rows
# with missing values in any required column are dropped first,
# groups are visited in sorted order, a group with no variation in
# treatment is SKIPPED with a warning, and a group whose AIPW fit
# fails is reported with NaN estimates and its own row count rather
# than being dropped silently.

#' Group average treatment effects (GATE) by stratified AIPW
#'
#' Splits the data by \code{group_col} and estimates an augmented
#' inverse-probability-weighted average treatment effect inside each
#' stratum, so that both nuisance models are allowed to differ across
#' groups.
#'
#' @param data A data frame.
#' @param treatment Name of the binary treatment column.
#' @param outcome Name of the outcome column.
#' @param covariates Character vector of covariate names.
#' @param group_col Name of the grouping column.
#' @param propensity_col Optional name of a column holding
#'   pre-computed propensity scores; \code{NULL} (default) estimates
#'   them within each group.  Both routes are available.
#' @param trim Propensity trimming bounds, or \code{NULL} for none.
#' @param trim_type \code{"value"} (default) clamps the propensity
#'   scores to the absolute bounds \code{trim}; \code{"quantile"}
#'   winsorises them at their own sample quantiles.  Both are weight
#'   truncation and keep every unit; neither is the DISCARD rule of
#'   Crump et al. (2009), which changes the estimand.  The value route is
#'   the default here precisely because sample quantiles are unstable
#'   inside small strata, which is the situation this estimator creates.
#'   The Python arm takes the same two arguments with the same defaults.
#' @param ps_model Propensity estimator: \code{"mle"} (default,
#'   unpenalised logistic maximum likelihood on the raw covariates) or
#'   \code{"ridge"} (L2-penalised logistic on standardised
#'   covariates).  Both estimators are available on both arms.
#' @param ridge_lambda Penalty strength for \code{ps_model = "ridge"}.
#' @param outcome_fit \code{"separate"} (default) fits the outcome
#'   model on each treatment arm; \code{"pooled"} fits one regression
#'   with a treatment dummy.  Both routes are available on both arms.
#' @return A data frame with one row per estimated group and columns
#'   \code{group}, \code{ate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{n}.
#' @references Robins, J. M., Rotnitzky, A. and Zhao, L. P. (1994).
#'   Estimation of regression coefficients when some regressors are
#'   not always observed. JASA, 89(427), 846-866.
#' @export
morie_gate <- function(data, treatment, outcome, covariates, group_col,
                       propensity_col = NULL, trim = c(0.01, 0.99),
                       trim_type = "value", ps_model = "mle",
                       ridge_lambda = 1, outcome_fit = "separate") {
  required_cols <- unique(c(treatment, outcome, group_col, covariates))
  frame <- data[required_cols]
  frame <- frame[stats::complete.cases(frame), , drop = FALSE]
  groups <- sort(unique(frame[[group_col]]))
  rows <- list()
  for (gv in groups) {
    gdf <- frame[frame[[group_col]] == gv, , drop = FALSE]
    if (length(unique(gdf[[treatment]])) < 2L) {
      warning(sprintf(
        "GATE: skipping group '%s' -- no variation in treatment", gv))
      next
    }
    res <- try(morie_estimate_aipw(gdf, treatment = treatment,
                                   outcome = outcome,
                                   covariates = covariates,
                                   propensity_col = propensity_col,
                                   outcome_model = "linear",
                                   trim = trim, trim_type = trim_type,
                                   ps_model = ps_model,
                                   ridge_lambda = ridge_lambda,
                                   outcome_fit = outcome_fit),
               silent = TRUE)
    if (inherits(res, "try-error")) {
      warning(sprintf("GATE: failed for group '%s': %s", gv,
                      conditionMessage(attr(res, "condition"))))
      rows[[length(rows) + 1L]] <- data.frame(
        group = gv, ate = NaN, se = NaN, ci_lower = NaN, ci_upper = NaN,
        n = nrow(gdf), stringsAsFactors = FALSE)
    } else {
      rows[[length(rows) + 1L]] <- data.frame(
        group = gv, ate = res$ate, se = res$se, ci_lower = res$ci_lower,
        ci_upper = res$ci_upper, n = res$n, stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0L)
    return(data.frame(group = character(0), ate = numeric(0),
                      se = numeric(0), ci_lower = numeric(0),
                      ci_upper = numeric(0), n = numeric(0),
                      stringsAsFactors = FALSE))
  do.call(rbind, rows)
}
