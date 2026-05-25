# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie survey -- design-based estimation under complex survey designs.
#
# Thin R port of src/morie/survey.py. Wraps `survey::svydesign` / `svymean` /
# `svyglm` (Lumley) and hand-rolls Horvitz-Thompson + Hajek + ratio +
# post-stratification fall-backs in base R so users without `survey`
# installed still get correct point estimates with Taylor SEs.

.req_survey <- function() {
  morie_ensure_extras("survey")
}

#' Construct a survey design object.
#'
#' Returns a `survey::svydesign` object when `survey` is available; otherwise
#' returns a lightweight list with the same fields the morie helpers consume.
#'
#' @param data data.frame.
#' @param weights_col Column name of analytic/probability weights.
#' @param strata_col Optional strata column.
#' @param cluster_col Optional PSU/cluster column.
#' @param fpc_col Optional finite-population-correction column.
#' @param nest If TRUE, treat cluster IDs as nested within strata.
#' @export
morie_survey_design <- function(data, weights_col, strata_col = NULL,
                                cluster_col = NULL, fpc_col = NULL,
                                nest = FALSE) {
  if (!weights_col %in% names(data))
    stop(sprintf("weights_col '%s' not in data.", weights_col), call. = FALSE)
  if (requireNamespace("survey", quietly = TRUE)) {
    ids_fml <- if (is.null(cluster_col)) ~1
               else stats::as.formula(paste("~", cluster_col))
    strata_fml <- if (is.null(strata_col)) NULL
                  else stats::as.formula(paste("~", strata_col))
    fpc_fml <- if (is.null(fpc_col)) NULL
               else stats::as.formula(paste("~", fpc_col))
    w_fml <- stats::as.formula(paste("~", weights_col))
    return(survey::svydesign(ids = ids_fml, strata = strata_fml,
                             weights = w_fml, fpc = fpc_fml,
                             data = data, nest = nest))
  }
  structure(list(data = data, weights = data[[weights_col]],
                 strata = if (!is.null(strata_col)) data[[strata_col]] else NULL,
                 cluster = if (!is.null(cluster_col)) data[[cluster_col]] else NULL),
            class = "morie_survey_design_fallback")
}

#' Horvitz-Thompson estimator of a population total.
#' @return list with `total`, `se`, `ci_lower`, `ci_upper`.
#' @export
morie_survey_ht_total <- function(y, inclusion_probs) {
  y <- as.numeric(y)
  pi <- as.numeric(inclusion_probs)
  if (length(y) != length(pi))
    stop("y and inclusion_probs must have the same length.", call. = FALSE)
  if (any(pi <= 0) || any(pi > 1))
    stop("inclusion_probs must lie in (0, 1].", call. = FALSE)
  total <- sum(y / pi)
  z <- y / pi
  var_ht <- sum(z^2 * (1 - pi))
  se <- sqrt(max(0, var_ht))
  zc <- qnorm(0.975)
  list(total = total, se = se,
       ci_lower = total - zc * se, ci_upper = total + zc * se)
}

#' Hajek (ratio) estimator of a population mean.
#' @export
morie_survey_hajek_mean <- function(y, weights) {
  y <- as.numeric(y)
  w <- as.numeric(weights)
  if (length(y) != length(w))
    stop("y and weights must have the same length.", call. = FALSE)
  if (any(w <= 0)) stop("weights must be > 0.", call. = FALSE)
  if (length(y) < 2) stop("Need >= 2 observations.", call. = FALSE)
  sw <- sum(w)
  m <- sum(w * y) / sw
  res <- y - m
  var_h <- sum(w^2 * res^2) / sw^2
  se <- sqrt(max(0, var_h))
  zc <- qnorm(0.975)
  list(mean = m, se = se,
       ci_lower = m - zc * se, ci_upper = m + zc * se)
}

#' Survey-weighted mean (delegates to `survey::svymean` when available).
#' @export
morie_survey_mean <- function(design, variable) {
  if (inherits(design, "survey.design") ||
      inherits(design, "survey.design2")) {
    .req_survey()
    fml <- stats::as.formula(paste("~", variable))
    sm <- survey::svymean(fml, design)
    return(list(mean = as.numeric(sm),
                se = sqrt(diag(stats::vcov(sm)))[[1]]))
  }
  # fallback
  morie_survey_hajek_mean(design$data[[variable]], design$weights)
}

#' Ratio estimator of a population total using known X_pop.
#' @export
morie_survey_ratio <- function(y, x, weights, X_population_total) {
  y <- as.numeric(y)
  x <- as.numeric(x)
  w <- as.numeric(weights)
  if (length(unique(c(length(y), length(x), length(w)))) != 1)
    stop("y, x, weights must be same length.", call. = FALSE)
  if (any(w <= 0)) stop("weights must be > 0.", call. = FALSE)
  if (X_population_total <= 0)
    stop("X_population_total must be > 0.", call. = FALSE)
  y_ht <- sum(w * y)
  x_ht <- sum(w * x)
  if (x_ht == 0) stop("Weighted sum of x is zero.", call. = FALSE)
  r <- y_ht / x_ht
  total <- r * X_population_total
  res <- y - r * x
  var_est <- sum(w^2 * res^2) / x_ht^2 * X_population_total^2
  se <- sqrt(max(0, var_est))
  zc <- qnorm(0.975)
  list(ratio = r, total_estimate = total, se = se,
       ci_lower = total - zc * se, ci_upper = total + zc * se)
}

#' Post-stratification weights (sample-to-population alignment).
#'
#' Delegates to `survey::postStratify()` when given a design; otherwise
#' computes raw post-stratification factors in base R.
#' @export
morie_survey_poststratify <- function(df, strata_col, population_counts) {
  if (!strata_col %in% names(df))
    stop(sprintf("strata_col '%s' not in df.", strata_col), call. = FALSE)
  s <- as.character(df[[strata_col]])
  pop_names <- as.character(names(population_counts))
  missing <- setdiff(unique(s), pop_names)
  if (length(missing) > 0)
    stop(sprintf("Strata %s present in sample but not population_counts.",
                 paste(missing, collapse = ", ")), call. = FALSE)
  N_tot <- sum(unlist(population_counts))
  n_tot <- nrow(df)
  w <- rep(1, n_tot)
  for (h in names(population_counts)) {
    mask <- s == as.character(h)
    n_h <- sum(mask)
    if (n_h == 0) {
      warning(sprintf("Stratum '%s' has 0 sample units.", h))
      next
    }
    w[mask] <- (population_counts[[h]] / N_tot) / (n_h / n_tot)
  }
  w
}

#' Raking calibration to known marginal totals (iterative proportional fitting).
#'
#' For multi-variable marginals use `morie_weights_rake()`; this helper is the
#' single-variable convenience.
#' @export
morie_survey_calibrate <- function(df, aux_vars, population_totals,
                                   max_iter = 50, tol = 1e-6) {
  for (v in aux_vars) {
    if (!v %in% names(df))
      stop(sprintf("aux_var '%s' not in df.", v), call. = FALSE)
    if (!v %in% names(population_totals))
      stop(sprintf("population_totals missing '%s'.", v), call. = FALSE)
  }
  n <- nrow(df)
  w <- rep(1, n)
  converged <- FALSE
  for (i in seq_len(max_iter)) {
    max_dev <- 0
    for (v in aux_vars) {
      x <- as.numeric(df[[v]])
      # `target` was originally written `T` (legacy TRUE alias).
      # 3MMM.32's T_and_F_symbol_linter auto-fix rewrote it to
      # literal TRUE, which is illegal as an LHS; renamed to a
      # descriptive identifier on 2026-05-25.
      target <- as.numeric(population_totals[[v]])
      cur <- sum(w * x)
      if (cur == 0) {
        warning(sprintf("Weighted total of '%s' is zero at iter %d.", v, i))
        next
      }
      f <- target / cur
      w <- w * f
      max_dev <- max(max_dev, abs(f - 1))
    }
    if (max_dev < tol) { converged <- TRUE
    break }
  }
  if (!converged)
    warning(sprintf("Raking did not converge in %d iters (max_dev=%.2e).",
                    max_iter, max_dev))
  w
}

#' Subpopulation (domain) mean with Woodruff linearised SE.
#' @export
morie_survey_subpop <- function(df, domain_col, domain_value,
                                outcome_col, weight_col) {
  needed <- c(domain_col, outcome_col, weight_col)
  for (c in needed)
    if (!c %in% names(df))
      stop(sprintf("Column '%s' not in df.", c), call. = FALSE)
  dom <- as.numeric(df[[domain_col]] == domain_value)
  y <- as.numeric(df[[outcome_col]])
  w <- as.numeric(df[[weight_col]])
  if (any(w <= 0)) stop("weights must be > 0.", call. = FALSE)
  n_dom <- sum(dom)
  if (n_dom == 0) stop("No observations match the domain.", call. = FALSE)
  N_d <- sum(w * dom)
  y_bar <- sum(w * dom * y) / N_d
  z <- dom * (y - y_bar)
  var_d <- sum(w^2 * z^2) / N_d^2
  se <- sqrt(max(0, var_d))
  zc <- qnorm(0.975)
  list(mean = y_bar, se = se,
       ci_lower = y_bar - zc * se, ci_upper = y_bar + zc * se,
       n_domain = n_dom)
}

#' Survey-weighted GLM with design-based SEs.
#'
#' Wraps `survey::svyglm()`. Family argument accepts the same strings as the
#' Python module ("gaussian", "binomial", "poisson", "gamma", "negativebinomial")
#' or any R `family` object.
#' @export
morie_survey_glm <- function(design, formula,
                             family = c("gaussian", "binomial", "poisson",
                                        "gamma", "negativebinomial")) {
  .req_survey()
  if (is.character(family)) {
    family <- match.arg(family)
    fam <- switch(family,
                  gaussian = stats::gaussian(),
                  binomial = stats::binomial(),
                  poisson  = stats::poisson(),
                  gamma    = stats::Gamma(),
                  negativebinomial = {
                    if (!requireNamespace("MASS", quietly = TRUE))
                      stop("Package 'MASS' is required for negative binomial.",
                           call. = FALSE)
                    MASS::negative.binomial(1)
                  })
  } else {
    fam <- family
  }
  fml <- if (inherits(formula, "formula")) formula
         else stats::as.formula(formula)
  survey::svyglm(fml, design = design, family = fam)
}

#' Complex-survey GLM constructor (single-shot wrapper that builds a design
#' and fits a `svyglm` in one call). Cluster-robust SEs via the design.
#' @export
morie_survey_complex_glm <- function(df, formula, weight_col,
                                     family = "gaussian",
                                     cluster_col = NULL, strata_col = NULL) {
  if (!weight_col %in% names(df))
    stop(sprintf("weight_col '%s' not in df.", weight_col), call. = FALSE)
  if (any(df[[weight_col]] <= 0))
    stop("All survey weights must be > 0.", call. = FALSE)
  des <- morie_survey_design(df, weights_col = weight_col,
                             strata_col = strata_col,
                             cluster_col = cluster_col)
  morie_survey_glm(des, formula = formula, family = family)
}
