# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie survey -- design-based estimation under complex survey designs.
#
# R port of src/morie/survey.py. Designs, means and GLMs are native: the one-stage
# stratified cluster design with a finite-population correction, and the Taylor-linearisation
# variance of survey::svyrecvar (Lumley 2004), cross-validated against the survey package in
# tests/cross. Horvitz-Thompson, Hajek, ratio, post-stratification and raking are base R.

#' Shared parameters for morie_survey_* design-based estimators
#'
#' Roxygen-only stub holding the @param entries shared across the
#' survey family (HT, Hajek, ratio, calibration, post-stratification,
#' subpopulation, complex-design GLM, mean). Functions reference these
#' via `@inheritParams morie_survey_params`.
#'
#' @param df A `data.frame` holding the (unit-level) sample data plus
#'   any weight/strata/cluster/domain columns referenced by name.
#' @param design A design from [morie_survey_design()]. A `survey::svydesign`
#'   object is accepted too and read into the same native form (one-stage designs).
#' @param y Numeric vector of outcome values aligned with the sample.
#' @param x Numeric vector of auxiliary values aligned with `y` (used
#'   in ratio estimation).
#' @param weights Numeric vector of design weights aligned with `y`.
#' @param inclusion_probs Numeric vector of inclusion probabilities
#'   (`pi_i`) for the Horvitz-Thompson estimator.
#' @param X_population_total Known population total for the auxiliary
#'   variable `x` (ratio estimator).
#' @param aux_vars Character vector of column names of auxiliary
#'   variables used for calibration (raking, GREG, etc.).
#' @param population_totals Named numeric vector of population totals
#'   to calibrate to (one entry per `aux_vars` element).
#' @param population_counts Named numeric vector of population counts
#'   by stratum (post-stratification target).
#' @param strata_col Character; column name of the stratum identifier
#'   in `df`.
#' @param cluster_col Character; column name of the cluster identifier
#'   in `df`.
#' @param weight_col Character; column name of the design weight
#'   variable in `df`.
#' @param domain_col Character; column name of the subpopulation /
#'   domain indicator in `df`.
#' @param domain_value Value (matching `df[[domain_col]]`) defining
#'   the subpopulation to estimate.
#' @param outcome_col Character; column name of the outcome variable
#'   in `df`.
#' @param variable Character; column name of the outcome variable in
#'   the `design` object (`morie_survey_mean`).
#' @param formula A `formula` (e.g. `y ~ x1 + x2`) for survey-weighted
#'   regression / GLM.
#' @param family A `family` object (e.g. `stats::binomial()`) passed
#'   to the survey-weighted GLM.
#' @param nest If TRUE, cluster IDs are only unique within a stratum (see [morie_survey_design()]).
#' @param max_iter Iteration cap for the iterative calibration loop.
#' @param tol Convergence tolerance for calibration.
#' @keywords internal
#' @name morie_survey_params
NULL



#' Construct a survey design object
#'
#' A one-stage design: sampling weights, optional strata, optional primary sampling units
#' (clusters) and an optional finite-population correction. Everything here is native; the
#' variance of the estimators built on it is the Taylor linearisation of `survey::svyrecvar`.
#'
#' @param data data.frame.
#' @param weights_col Column name of the sampling weights (inverse inclusion probabilities).
#' @param strata_col Optional strata column.
#' @param cluster_col Optional PSU/cluster column.
#' @param fpc_col Optional finite-population-correction column: the population size of the
#'   stratum (number of PSUs when clustered), or the sampling fraction when every value is <= 1.
#' @param nest If TRUE, cluster IDs are only unique within a stratum; if FALSE (the default)
#'   a cluster ID that appears in two strata is an error, as in `survey::svydesign`.
#' @return An object of class \code{"morie_survey_design"}: a list with the data, the
#'   weights, and per-row stratum, PSU, PSU count of the stratum and population size.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(40), w = runif(40, 0.5, 2),
#'                  s = rep(c("a", "b"), 20), psu = rep(1:10, each = 4))
#' d <- morie_survey_design(df, "w", strata_col = "s", cluster_col = "psu", nest = TRUE)
#' class(d)
#' morie_survey_mean(d, "y")
#' @export
morie_survey_design <- function(data, weights_col, strata_col = NULL,
                                cluster_col = NULL, fpc_col = NULL,
                                nest = FALSE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (col in c(weights_col, strata_col, cluster_col, fpc_col)) {
    if (!is.character(col) || length(col) != 1L || !col %in% names(data))
      stop(sprintf("column '%s' not in data.", paste(col, collapse = ",")), call. = FALSE)
  }
  w <- data[[weights_col]]
  if (!is.numeric(w) || anyNA(w) || any(w < 0))
    stop("the sampling weights must be numeric, >= 0 and not missing.", call. = FALSE)
  n <- nrow(data)
  strata <- if (is.null(strata_col)) rep("1", n) else data[[strata_col]]
  cluster <- if (is.null(cluster_col)) seq_len(n) else data[[cluster_col]]
  if (anyNA(strata)) stop("missing values in the strata column.", call. = FALSE)
  if (anyNA(cluster)) stop("missing values in the cluster column.", call. = FALSE)
  strata <- as.character(strata)
  if (isTRUE(nest)) {
    cluster <- paste(strata, cluster, sep = "\r")
  } else if (!is.null(strata_col) && !is.null(cluster_col)) {
    if (any(tapply(strata, as.character(cluster), function(v) length(unique(v))) > 1L))
      stop("Clusters not nested in strata at top level; you may want nest=TRUE.", call. = FALSE)
  }
  cluster <- as.character(cluster)
  n_psu <- stats::ave(seq_len(n), strata, FUN = function(i) length(unique(cluster[i])))
  popsize <- NULL
  if (!is.null(fpc_col)) {
    fpc <- data[[fpc_col]]
    if (!is.numeric(fpc) || anyNA(fpc) || any(fpc <= 0))
      stop("the fpc column must be positive numbers.", call. = FALSE)
    if (any(tapply(fpc, strata, function(v) length(unique(v))) > 1L))
      stop("the fpc must be the same for every unit of a stratum.", call. = FALSE)
    # every value <= 1 means sampling fractions, as survey::svydesign reads them
    popsize <- if (all(fpc <= 1)) n_psu / fpc else as.numeric(fpc)
    if (any(popsize < n_psu))
      stop("the fpc gives a population smaller than the sample in some stratum.", call. = FALSE)
  }
  structure(list(data = data, weights = as.numeric(w), strata = strata, cluster = cluster,
                 n_psu = as.numeric(n_psu), popsize = popsize, nest = isTRUE(nest)),
            class = c("morie_survey_design", "morie_survey_design_fallback"))
}

#' Internal: a design from morie_survey_design() or a survey::svydesign object, in native form
#' @noRd
.morie_svy_as_design <- function(design) {
  if (inherits(design, "morie_survey_design")) return(design)
  if (inherits(design, "morie_survey_design_fallback")) {
    # an older fallback list: weights only
    n <- NROW(design$data)
    return(structure(list(data = design$data, weights = as.numeric(design$weights),
                          strata = rep("1", n), cluster = as.character(seq_len(n)),
                          n_psu = rep(n, n), popsize = NULL, nest = FALSE),
                     class = c("morie_survey_design", "morie_survey_design_fallback")))
  }
  if (inherits(design, c("survey.design2", "survey.design"))) {
    # read the object's fields; no call into the survey package
    if (NCOL(design$cluster) > 1L && !is.null(design$fpc$popsize) && NCOL(design$fpc$popsize) > 1L &&
        any(is.finite(design$fpc$popsize[, -1L])))
      stop("this design has a finite-population correction below the first stage; ",
           "the native estimators use the first-stage (ultimate cluster) variance only.", call. = FALSE)
    pop <- design$fpc$popsize
    pop <- if (is.null(pop)) NULL else as.numeric(pop[, 1L])
    if (!is.null(pop) && all(!is.finite(pop))) pop <- NULL
    return(structure(list(data = design$variables, weights = 1 / as.numeric(design$prob),
                          strata = as.character(design$strata[[1L]]),
                          cluster = as.character(design$cluster[[1L]]),
                          n_psu = as.numeric(design$fpc$sampsize[, 1L]), popsize = pop,
                          nest = TRUE),
                     class = c("morie_survey_design", "morie_survey_design_fallback")))
  }
  stop("`design` must come from morie_survey_design() (or be a survey::svydesign object).",
       call. = FALSE)
}

#' Internal: Taylor-linearisation variance of estimated totals, one-stage design
#'
#' `U` holds one row per sampled unit (zero rows for units outside the fitted subset). Per
#' stratum the PSU totals are centred and scaled by `f n_h / (n_h - 1)`, `f = (N_h - n_h) / N_h`
#' with a population size and 1 without: survey::svyrecvar's estimator at stage 1 with
#' lonely.psu = "fail".
#' @noRd
.morie_svy_recvar <- function(U, design) {
  U <- as.matrix(U)
  p <- ncol(U)
  V <- matrix(0, p, p)
  for (h in unique(design$strata)) {
    i <- which(design$strata == h)
    nh <- design$n_psu[i[1L]]
    f <- if (is.null(design$popsize) || !is.finite(design$popsize[i[1L]])) 1 else
      (design$popsize[i[1L]] - nh) / design$popsize[i[1L]]
    if (f < 1e-7) next
    if (nh < 2) stop("Stratum (", h, ") has only one PSU at stage 1", call. = FALSE)
    tot <- rowsum(U[i, , drop = FALSE], design$cluster[i], reorder = FALSE)
    if (nrow(tot) < nh) tot <- rbind(tot, matrix(0, nh - nrow(tot), p))
    tot <- sweep(tot, 2L, colMeans(tot))
    V <- V + crossprod(tot) * (f * nh / (nh - 1))
  }
  V
}

#' Horvitz-Thompson estimator of a population total
#' @return list with `total`, `se`, `ci_lower`, `ci_upper`.
#' @inheritParams morie_survey_params
#' @examples
#' set.seed(1)
#' str(morie_survey_ht_total(rnorm(30, 5), runif(30, 0.05, 0.2)), max.level = 1)
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

#' Hajek (ratio) estimator of a population mean
#' @inheritParams morie_survey_params
#' @return A named list with elements \code{mean}, \code{se}, \code{ci_lower}, \code{ci_upper}.
#' @examples
#' set.seed(1)
#' morie_survey_hajek_mean(rnorm(30, 5), runif(30, 0.5, 2))
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
  # with the n/(n - 1) of the with-replacement variance, as survey::svymean
  var_h <- length(y) / (length(y) - 1) * sum(w^2 * res^2) / sw^2
  se <- sqrt(max(0, var_h))
  zc <- qnorm(0.975)
  list(mean = m, se = se,
       ci_lower = m - zc * se, ci_upper = m + zc * se)
}

#' Survey-weighted mean with its design-based standard error
#'
#' The Hajek mean \eqn{\sum w y / \sum w} and the Taylor-linearisation SE under the design
#' (strata, clusters, finite-population correction), as `survey::svymean` computes them. A
#' missing value in the variable gives `NA`, as there.
#' @inheritParams morie_survey_params
#' @return A named \code{list}: \code{mean}, \code{se}.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(40), w = runif(40, 0.5, 2))
#' d <- morie_survey_design(df, "w")
#' str(morie_survey_mean(d, "y"), max.level = 1)
#' @export
morie_survey_mean <- function(design, variable) {
  d <- .morie_svy_as_design(design)
  if (!is.character(variable) || length(variable) != 1L || !variable %in% names(d$data))
    stop("`variable` must name one column of the design's data.", call. = FALSE)
  y <- as.numeric(d$data[[variable]])
  if (anyNA(y)) return(list(mean = NA_real_, se = NA_real_))
  w <- d$weights
  m <- sum(w * y) / sum(w)
  z <- w * (y - m) / sum(w)
  list(mean = m, se = sqrt(.morie_svy_recvar(cbind(z), d)[1L, 1L]))
}

#' Ratio estimator of a population total using known X_pop
#' @inheritParams morie_survey_params
#' @return A named list with elements \code{ratio}, \code{total_estimate}, \code{se},
#' \code{ci_lower}, \code{ci_upper}.
#' @examples
#' set.seed(1)
#' x <- runif(30, 1, 3); y <- 2 * x + rnorm(30, 0, 0.2)
#' str(morie_survey_ratio(y, x, rep(1, 30), X_population_total = 60),
#'     max.level = 1)
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
  # with the n/(n - 1) of the with-replacement variance, as survey::svyratio
  var_est <- length(y) / (length(y) - 1) * sum(w^2 * res^2) / x_ht^2 * X_population_total^2
  se <- sqrt(max(0, var_est))
  zc <- qnorm(0.975)
  list(ratio = r, total_estimate = total, se = se,
       ci_lower = total - zc * se, ci_upper = total + zc * se)
}

#' Post-stratification weights (sample-to-population alignment)
#'
#' Delegates to `survey::postStratify()` when given a design; otherwise
#' computes raw post-stratification factors in base R.
#' @inheritParams morie_survey_params
#' @return A numeric vector of post-stratification weights.
#' @examples
#' set.seed(1)
#' df <- data.frame(g = rep(c("a", "b"), 15))
#' str(morie_survey_poststratify(df, "g", c(a = 60, b = 40)), max.level = 1)
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

#' Raking calibration to known marginal totals (iterative proportional fitting)
#'
#' For multi-variable marginals use `morie_weights_rake()`; this helper is the
#' single-variable convenience.
#' @inheritParams morie_survey_params
#' @return A numeric vector of calibration weights.
#' @examples
#' set.seed(1)
#' df <- data.frame(x1 = runif(30, 1, 3))
#' res <- morie_survey_calibrate(df, "x1", c(x1 = 60))
#' str(res, max.level = 1)
#' @export
morie_survey_calibrate <- function(df, aux_vars, population_totals,
                                   max_iter = 50, tol = 1e-6) {
  for (v in aux_vars) {
    if (!v %in% names(df))
      stop(sprintf("aux_var '%s' not in df.", v), call. = FALSE)
    if (!v %in% names(population_totals))
      stop(sprintf("population_totals missing '%s'.", v), call. = FALSE)
  }
  # Raking (Deville and Sarndal 1992): w_i = exp(x_i' lambda) from unit
  # starting weights, lambda solving sum_i w_i x_i = T by Newton, as
  # survey::calibrate(calfun = "raking") with no intercept
  X <- as.matrix(as.data.frame(lapply(aux_vars, function(v) as.numeric(df[[v]]))))
  target <- vapply(aux_vars, function(v) as.numeric(population_totals[[v]]), numeric(1))
  lam <- numeric(length(aux_vars))
  converged <- FALSE
  for (i in seq_len(max_iter)) {
    w <- as.numeric(exp(X %*% lam))
    Fv <- as.numeric(crossprod(X, w)) - target
    if (max(abs(Fv) / pmax(abs(target), 1)) < tol) {
      converged <- TRUE
      break
    }
    lam <- lam - solve(crossprod(X, X * w), Fv)
  }
  if (!converged)
    warning(sprintf("Raking did not converge in %d iterations.", max_iter))
  w
}

#' Subpopulation (domain) mean with Woodruff linearised SE
#' @inheritParams morie_survey_params
#' @return A named list with elements \code{mean}, \code{se}, \code{ci_lower},
#' \code{ci_upper}, \code{n_domain}.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(40), g = rep(c("a", "b"), 20),
#'                  w = runif(40, 0.5, 2))
#' str(morie_survey_subpop(df, "g", "a", "y", "w"), max.level = 1)
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
  # n/(n - 1) over the FULL sample, as survey::svymean on subset(design, .)
  var_d <- length(y) / (length(y) - 1) * sum(w^2 * z^2) / N_d^2
  se <- sqrt(max(0, var_d))
  zc <- qnorm(0.975)
  list(mean = y_bar, se = se,
       ci_lower = y_bar - zc * se, ci_upper = y_bar + zc * se,
       n_domain = n_dom)
}

#' Survey-weighted GLM with design-based SEs
#'
#' The design-weighted GLM with the linearisation (sandwich) variance of
#' `survey::svyglm()` (Binder 1983; Lumley 2004), computed natively for weights, strata,
#' clusters and a finite-population correction. Rows with a missing model variable leave the
#' fit but stay in the design with a zero score, as in survey. Family accepts the same strings
#' as the Python module ("gaussian", "binomial", "poisson", "gamma", "negativebinomial") or any
#' R `family` object; binomial and Poisson fits use the quasi family (same estimates, no
#' non-integer warning).
#' @references Binder, D. A. (1983). On the variances of asymptotically normal estimators from
#'   complex surveys. International Statistical Review 51, 279-292.
#'
#'   Lumley, T. (2004). Analysis of complex survey samples. Journal of Statistical Software
#'   9(1), 1-19.
#' @inheritParams morie_survey_params
#' @return A list: \code{coefficients} (a matrix of estimates, design-based standard errors,
#'   t values and p-values on the design degrees of freedom: PSUs minus strata minus the
#'   coefficients beyond the intercept), \code{confint}, \code{vcov} (the sandwich
#'   \eqn{A^{-1} B A^{-1}} with \eqn{B} the Taylor-linearisation variance of the score totals)
#'   and \code{fit} (the weighted \code{glm} giving the estimates).
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(40), x = rnorm(40), w = runif(40, 0.5, 2),
#'                  s = rep(c("a", "b"), 20), psu = rep(1:10, each = 4))
#' d <- morie_survey_design(df, "w", strata_col = "s", cluster_col = "psu", nest = TRUE)
#' morie_survey_glm(d, y ~ x)$coefficients
#' @export
morie_survey_glm <- function(design, formula,
                             family = c("gaussian", "binomial", "poisson",
                                        "gamma", "negativebinomial")) {
  d <- .morie_svy_as_design(design)
  if (is.character(family)) {
    fam <- switch(match.arg(family),
                  gaussian = stats::gaussian(),
                  binomial = stats::binomial(),
                  poisson  = stats::poisson(),
                  gamma    = stats::Gamma(link = "log"),
                  negativebinomial = .morie_negbin_family(1))
  } else {
    fam <- family
  }
  fml <- if (inherits(formula, "formula")) formula else stats::as.formula(formula)
  .morie_svyglm_native(fml, data = d$data, weights = d$weights, family = fam, design = d)
}

#' Complex-survey GLM in one call
#'
#' Builds the design with [morie_survey_design()] and fits [morie_survey_glm()]:
#' cluster-robust, stratified design-based standard errors.
#' @inheritParams morie_survey_params
#' @return A list: \code{coefficients} (a matrix of estimates, design-based standard errors,
#'   t values and p-values on the design degrees of freedom: PSUs minus strata minus the
#'   coefficients beyond the intercept), \code{confint}, \code{vcov} (the sandwich
#'   \eqn{A^{-1} B A^{-1}} with \eqn{B} the Taylor-linearisation variance of the score totals)
#'   and \code{fit} (the weighted \code{glm} giving the estimates).
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(40), x = rnorm(40), w = runif(40, 0.5, 2))
#' str(morie_survey_complex_glm(df, y ~ x, "w"), max.level = 1)
#' @export
morie_survey_complex_glm <- function(df, formula, weight_col,
                                     family = "gaussian",
                                     cluster_col = NULL, strata_col = NULL,
                                     nest = FALSE) {
  if (!weight_col %in% names(df))
    stop(sprintf("weight_col '%s' not in df.", weight_col), call. = FALSE)
  if (any(df[[weight_col]] <= 0))
    stop("All survey weights must be > 0.", call. = FALSE)
  des <- morie_survey_design(df, weights_col = weight_col,
                             strata_col = strata_col,
                             cluster_col = cluster_col, nest = nest)
  morie_survey_glm(des, formula = formula, family = family)
}
