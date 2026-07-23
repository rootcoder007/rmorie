# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Matching methods for causal inference in observational studies.
#
# Phase 1.b refactor (2026-05-25): the hand-written base-R fallbacks
# have been removed. Every method-style entry point now delegates
# directly to the canonical CRAN package:
#
#   * MatchIt      -- nearest / exact / cem / mahalanobis / optimal /
#                     full / subclass / genetic / variable-ratio
#                     (the full standard suite).
#   * cobalt       -- covariate-balance diagnostics (bal.tab, love.plot).
#   * WeightIt     -- entropy balancing (method = "ebal").
#   * Matching     -- genetic matching back end consumed by MatchIt.
#   * designmatch  -- cardinality / mixed-integer-programming matching
#                     (documented as a recommended alternative; the
#                     `morie_matching_cardinality()` wrapper keeps its
#                     iterative-caliper heuristic over MatchIt).
#
# Carceral-domain helpers (treatment-effect estimators on matched
# samples, Abadie-Imbens SE, Rosenbaum bounds, doubly-robust ATT,
# multi-treatment / longitudinal orchestrators, quality + overlap
# diagnostics) are kept because they encode rmorie-specific output
# shapes (`morie_match_result`, `morie_te_result`,
# `morie_balance_result`) that downstream MRM code depends on.

#' @importFrom stats glm binomial predict quantile sd var cov lm complete.cases as.formula model.matrix qnorm pnorm ks.test weighted.mean setNames
#' @importFrom utils head tail
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Internal helper: Morie Matching Have
#' @noRd
.morie_matching_have <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

#' Internal helper: Morie Matching Require
#' @noRd
.morie_matching_require <- function(pkg, fn) {
  morie_ensure_extras(pkg)
  invisible(TRUE)
}

#' @keywords internal
.morie_matching_drop_na <- function(data, cols) {
  data[stats::complete.cases(data[, cols, drop = FALSE]), , drop = FALSE]
}

#' @keywords internal
.morie_matching_logit <- function(p, eps = 1e-6) {
  p <- pmin(pmax(p, eps), 1 - eps)
  log(p / (1 - p))
}

#' @keywords internal
.morie_matching_empty_pairs <- function() {
  data.frame(
    treated_idx = integer(0),
    control_idx = integer(0),
    distance    = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.morie_matching_result <- function(matched_data, n_treated, n_matched_control,
                                   match_pairs, method,
                                   details = list()) {
  out <- list(
    matched_data      = matched_data,
    n_treated         = as.integer(n_treated),
    n_matched_control = as.integer(n_matched_control),
    match_pairs       = match_pairs,
    method            = method,
    details           = details
  )
  class(out) <- c("morie_match_result", "list")
  out
}

#' @noRd
.morie_matching_have_cpp <- function(name) {
  exists(name, envir = asNamespace("rmorie"), inherits = FALSE)
}

#' @keywords internal
.morie_matching_need_matchit <- function(fn) {
  if (!.morie_matching_have("MatchIt")) {
    stop(sprintf(
      "`%s()` requires the 'MatchIt' package. Install it with %s",
      fn, "install.packages(\"MatchIt\")"), call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
.morie_matching_matchit_to_result <- function(mi, df, treatment, method_label,
                                              details = list()) {
  md <- MatchIt::match.data(mi)
  pairs_df <- .morie_matching_empty_pairs()
  mm <- mi$match.matrix
  if (!is.null(mm) && nrow(mm) > 0L) {
    treated_rn <- rownames(mm)
    recs <- list()
    for (i in seq_len(nrow(mm))) {
      t_id <- treated_rn[i]
      for (j in seq_len(ncol(mm))) {
        c_id <- mm[i, j]
        if (is.na(c_id) || identical(as.character(c_id), "")) next
        recs[[length(recs) + 1L]] <- data.frame(
          treated_idx = t_id,
          control_idx = as.character(c_id),
          distance    = NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(recs) > 0L) pairs_df <- do.call(rbind, recs)
  }
  n_treated <- sum(md[[treatment]] == 1)
  n_control <- sum(md[[treatment]] == 0)
  .morie_matching_result(
    matched_data       = md,
    n_treated          = n_treated,
    n_matched_control  = n_control,
    match_pairs        = pairs_df,
    method             = method_label,
    details            = c(list(matchit = mi), details)
  )
}


# ---------------------------------------------------------------------------
# Propensity score estimation
# ---------------------------------------------------------------------------

#' Estimate propensity scores
#'
#' Estimates the probability of treatment via logistic regression or
#' gradient boosting on a set of covariates.
#'
#' @param data Data frame.
#' @param treatment Name of the binary treatment column (0/1).
#' @param covariates Character vector of covariate names.
#' @param model One of \code{"logistic"} (default) or \code{"gbm"}.
#'   \code{"gbm"} requires the \pkg{gbm} package.
#' @param max_iter Maximum iterations for logistic regression.
#' @return A numeric vector of propensity scores aligned to the rows of
#'   \code{data} (after dropping NAs in \code{treatment} or
#'   \code{covariates}); the \code{names} of the vector are the row names
#'   of the retained rows.
#' @references Rosenbaum, P. R., & Rubin, D. B. (1983). The central role of
#'   the propensity score in observational studies for causal effects.
#'   \emph{Biometrika}, 70(1), 41--55.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' ps <- morie_matching_estimate_propensity(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_estimate_propensity <- function(data, treatment, covariates,
                                               model = "logistic",
                                               max_iter = 1000) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  f <- stats::as.formula(paste(treatment, "~",
                               paste(covariates, collapse = " + ")))
  if (model == "gbm") {
    if (!.morie_matching_have("gbm")) {
      stop("Package 'gbm' is required for model = \"gbm\".  ",
           "Install it with install.packages(\"gbm\").",
           call. = FALSE)
    }
    fit <- gbm::gbm(f, data = df, distribution = "bernoulli",
                    n.trees = 100, interaction.depth = 3,
                    shrinkage = 0.1, verbose = FALSE)
    ps <- gbm::predict.gbm(fit, newdata = df, n.trees = 100,
                           type = "response")
  } else {
    fit <- stats::glm(f, data = df, family = stats::binomial(),
                      control = list(maxit = max_iter))
    ps <- stats::predict(fit, newdata = df, type = "response")
  }
  ps <- as.numeric(ps)
  names(ps) <- rownames(df)
  ps
}


# ---------------------------------------------------------------------------
# Propensity score trimming and common support
# ---------------------------------------------------------------------------

#' Trim propensity scores to a fixed range
#'
#' Clips propensity scores to \code{[lower, upper]}.
#'
#' @param ps Numeric vector of propensity scores.
#' @param lower,upper Numeric clip bounds (defaults 0.01, 0.99).
#' @return A numeric vector of the same length as \code{ps}.
#' @examples
#' morie_matching_trim_propensity(c(0.001, 0.5, 0.999))
#' @export
morie_matching_trim_propensity <- function(ps, lower = 0.01, upper = 0.99) {
  pmin(pmax(as.numeric(ps), lower), upper)
}

#' Restrict a sample to the region of common support
#'
#' Drops units whose propensity score falls outside the overlap region
#' of treated and control units.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param ps_col Propensity-score column name (default
#'   \code{"propensity_score"}).
#' @param method One of \code{"minmax"} (overlap of ranges) or
#'   \code{"trim"} (drop the extreme 5 percent of each tail).
#' @return A subset of \code{data} on common support.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' df$propensity_score <- morie_matching_estimate_propensity(df, "d",
#'                                                           c("x1", "x2"))
#' morie_matching_common_support(df, "d")
#' }
#' @export
morie_matching_common_support <- function(data, treatment,
                                          ps_col = "propensity_score",
                                          method = "minmax") {
  df <- data
  ps_t <- df[[ps_col]][df[[treatment]] == 1]
  ps_c <- df[[ps_col]][df[[treatment]] == 0]
  if (method == "minmax") {
    lower <- max(min(ps_t, na.rm = TRUE), min(ps_c, na.rm = TRUE))
    upper <- min(max(ps_t, na.rm = TRUE), max(ps_c, na.rm = TRUE))
  } else {
    lower <- max(stats::quantile(ps_t, 0.05, na.rm = TRUE),
                 stats::quantile(ps_c, 0.05, na.rm = TRUE))
    upper <- min(stats::quantile(ps_t, 0.95, na.rm = TRUE),
                 stats::quantile(ps_c, 0.95, na.rm = TRUE))
  }
  mask <- df[[ps_col]] >= lower & df[[ps_col]] <= upper
  mask[is.na(mask)] <- FALSE
  df[mask, , drop = FALSE]
}


# ---------------------------------------------------------------------------
# Method-style entry points -- thin MatchIt / WeightIt / Matching wrappers
# ---------------------------------------------------------------------------

#' Nearest-neighbour propensity-score matching
#'
#' Native greedy nearest-neighbour matching on the logit propensity
#' score (Rosenbaum & Rubin 1985; caliper per Cochran & Rubin 1973).
#' The propensity model is base \code{stats::glm}; no MatchIt at
#' runtime. Returns the same \code{morie_match_result} shape as
#' always; cross-validated against MatchIt in \code{tests/cross/}.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column (0/1).
#' @param covariates Character vector of covariates for the propensity model.
#' @param n_neighbors Number of matches per treated unit
#'   (forwarded as \code{ratio}).
#' @param caliper Maximum logit-propensity distance for a valid match,
#'   expressed in SD units of the logit (or \code{NULL} for no caliper).
#' @param replace If \code{TRUE}, controls may be re-used.
#' @param ps Optional pre-computed propensity scores
#'   (ignored; retained for back-compat).
#' @param alpha Significance level (carried through to \code{details}).
#' @return A list of class \code{morie_match_result}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"),
#'                                        caliper = 0.2)
#' }
#' @export
morie_matching_nearest_neighbor <- function(data, treatment, covariates,
                                            n_neighbors = 1L,
                                            caliper = NULL,
                                            replace = FALSE,
                                            ps = NULL,
                                            alpha = 0.05) {
  .morie_match_nearest_native(
    data, treatment, covariates,
    n_neighbors = n_neighbors,
    caliper = caliper,
    replace = replace,
    alpha = alpha
  )
}

#' Exact matching on discrete covariates
#'
#' Native rmorie implementation: units are stratified on the exact
#' combination of \code{exact_vars}; strata containing both arms are
#' retained and controls receive CEM-style stratum weights
#' (\code{weights} and \code{subclass} columns on the matched data).
#' No MatchIt at runtime.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param exact_vars Character vector of discrete variables for exact matching.
#' @return A list of class \code{morie_match_result}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(d = rbinom(200, 1, 0.4),
#'                  region = sample(c("N", "S", "E", "W"), 200, TRUE),
#'                  year = sample(2019:2021, 200, TRUE))
#' morie_matching_exact(df, "d", c("region", "year"))
#' }
#' @export
morie_matching_exact <- function(data, treatment, exact_vars) {
  .morie_match_exact_native(data, treatment, exact_vars)
}

#' Coarsened Exact Matching (CEM)
#'
#' Native rmorie implementation of Coarsened Exact Matching (Iacus, King
#' & Porro 2012): numeric covariates are coarsened into bins (quantile
#' cutpoints; Sturges' rule when \code{n_bins} is \code{NA} for a
#' variable), units are exact-matched on the coarsened strata, and
#' controls receive CEM stratum weights (\code{weights} and
#' \code{subclass} columns on the matched data). The multivariate L1
#' imbalance of the stratification is reported in
#' \code{details$l1_before}. No MatchIt/cem at runtime.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param n_bins Either a single integer (applied to every covariate) or a
#'   named list mapping covariate name to the number of bins; list
#'   entries left \code{NULL}/\code{NA} fall back to Sturges' rule.
#' @return A list of class \code{morie_match_result}.
#' @references Iacus, S. M., King, G., & Porro, G. (2012). Causal inference
#'   without balance checking: Coarsened exact matching.
#'   \emph{Political Analysis}, 20(1), 1--24.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_cem(df, "d", c("x1", "x2"), n_bins = 5)
#' }
#' @export
morie_matching_cem <- function(data, treatment, covariates, n_bins = 5L) {
  .morie_match_cem_native(data, treatment, covariates, n_bins = n_bins)
}

#' Mahalanobis distance matching
#'
#' Thin wrapper around \code{MatchIt::matchit(distance = "mahalanobis")}.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of continuous covariates.
#' @param n_neighbors Number of matches per treated unit.
#' @param caliper Maximum Mahalanobis distance for a valid match.
#' @param replace If \code{TRUE}, controls may be re-used.
#' @param exact Optional character vector of variables to match exactly
#'   prior to distance matching.
#' @return A list of class \code{morie_match_result}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_mahalanobis(df, "d", c("x1", "x2"), n_neighbors = 1)
#' }
#' @export
morie_matching_mahalanobis <- function(data, treatment, covariates,
                                       n_neighbors = 1L,
                                       caliper = NULL,
                                       replace = FALSE,
                                       exact = NULL) {
  .morie_match_mahalanobis_native(
    data, treatment, covariates,
    n_neighbors = n_neighbors,
    caliper = caliper,
    replace = replace,
    exact = exact
  )
}

#' Optimal pair matching
#'
#' Native rmorie implementation of optimal 1:1 pair matching: globally
#' minimizes the total matched distance (Rosenbaum 1989), unlike greedy
#' nearest-neighbour. Propensity distance uses an exact non-crossing
#' dynamic program on the logit score; Mahalanobis distance uses an
#' exact shortest-augmenting-path assignment on whitened covariates.
#' No MatchIt/optmatch at runtime.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param distance One of \code{"propensity"} or \code{"mahalanobis"}.
#' @param ps Optional pre-computed propensity scores (ignored; retained
#'   for back-compat).
#' @return A list of class \code{morie_match_result}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_optimal_pair(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_optimal_pair <- function(data, treatment, covariates,
                                        distance = "propensity",
                                        ps = NULL) {
  .morie_match_optimal_native(data, treatment, covariates,
                              distance = distance, ps = ps)
}

#' Full matching via subclassification
#'
#' Thin wrapper around \code{MatchIt::matchit(method = "full")}, which
#' calls \pkg{optmatch}.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param ps Optional pre-computed propensity scores (ignored; retained
#'   for back-compat).
#' @param n_subclasses Carried for back-compat (ignored under MatchIt).
#' @return A list of class \code{morie_match_result}.
#' @references Hansen, B. B. (2004). Full matching in an observational
#'   study of coaching for the SAT. \emph{JASA}, 99(467), 609--618.
#' @examplesIf requireNamespace("MatchIt", quietly = TRUE) && requireNamespace("optmatch", quietly = TRUE)
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_full(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_full <- function(data, treatment, covariates,
                                ps = NULL, n_subclasses = 10L) {
  .morie_matching_need_matchit("morie_matching_full")
  if (!.morie_matching_have("optmatch")) {
    stop("`morie_matching_full()` requires the 'optmatch' package. ",
         "Install it with install.packages(\"optmatch\").",
         call. = FALSE)
  }
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  f <- stats::as.formula(paste(treatment, "~",
                               paste(covariates, collapse = " + ")))
  mi <- MatchIt::matchit(f, data = df, method = "full", distance = "glm")
  .morie_matching_matchit_to_result(
    mi, df, treatment,
    method_label = "full_matching (MatchIt + optmatch)",
    details = list(n_subclasses = n_subclasses)
  )
}

#' Subclassification (stratification) on the propensity score
#'
#' Thin wrapper around \code{MatchIt::matchit(method = "subclass")} that
#' reports within-stratum sample sizes and PS ranges, preserving the
#' rmorie return shape (\code{data_with_strata} + \code{stratum_effects}).
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param ps Optional pre-computed propensity scores (ignored; retained
#'   for back-compat).
#' @param n_strata Number of quantile-based strata (default 5).
#' @return A list with components \code{data_with_strata} (the matched
#'   data augmented with \code{._stratum} and \code{._ps} columns) and
#'   \code{stratum_effects} (per-stratum sample sizes and PS ranges).
#' @examplesIf requireNamespace("MatchIt", quietly = TRUE)
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_subclassify(df, "d", c("x1", "x2"), n_strata = 5)
#' }
#' @export
morie_matching_subclassify <- function(data, treatment, covariates,
                                       ps = NULL, n_strata = 5L) {
  .morie_matching_need_matchit("morie_matching_subclassify")
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  f <- stats::as.formula(paste(treatment, "~",
                               paste(covariates, collapse = " + ")))
  mi <- MatchIt::matchit(f, data = df, method = "subclass",
                         distance = "glm", subclass = as.integer(n_strata))
  md <- MatchIt::match.data(mi)
  if (!is.null(md$distance)) md[["._ps"]] <- as.numeric(md$distance)
  md[["._stratum"]] <- as.integer(md$subclass)
  recs <- list()
  for (s in sort(unique(md[["._stratum"]]))) {
    if (is.na(s)) next
    grp <- md[md[["._stratum"]] == s, , drop = FALSE]
    n_t <- sum(grp[[treatment]] == 1)
    n_c <- sum(grp[[treatment]] == 0)
    if (!n_t || !n_c) next
    recs[[length(recs) + 1L]] <- data.frame(
      stratum       = s,
      n_treated     = n_t,
      n_control     = n_c,
      ps_range_low  = min(grp[["._ps"]], na.rm = TRUE),
      ps_range_high = max(grp[["._ps"]], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
  stratum_effects <- if (length(recs)) do.call(rbind, recs) else
    data.frame(stratum = integer(0), n_treated = integer(0),
               n_control = integer(0), ps_range_low = numeric(0),
               ps_range_high = numeric(0))
  list(
    data_with_strata = md,
    stratum_effects  = stratum_effects
  )
}

#' Entropy balancing weights (Hainmueller, 2012)
#'
#' Thin wrapper around \code{WeightIt::weightit(method = "ebal")} (or
#' \code{ebal::ebalance} if \pkg{WeightIt} is unavailable).  Computes
#' weights for the control group so that the weighted moments of the
#' covariates match those of the treated group.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param max_moment Highest moment to balance (1 = means, 2 = means + var,
#'   3 = + skewness).
#' @param max_iter Maximum Newton iterations (forwarded to \pkg{ebal}).
#' @param tol Convergence tolerance (forwarded to \pkg{ebal}).
#' @return A numeric vector of weights aligned to the rows of \code{data}
#'   after dropping NAs.  Treated units receive weight 1.
#' @references Hainmueller, J. (2012). Entropy balancing for causal effects.
#'   \emph{Political Analysis}, 20(1), 25--46.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' w <- morie_matching_entropy_balance(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_entropy_balance <- function(data, treatment, covariates,
                                           max_moment = 1L,
                                           max_iter = 500L,
                                           tol = 1e-6) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  t_mask <- df[[treatment]] == 1
  X <- as.matrix(df[, covariates, drop = FALSE])
  if (max_moment >= 2L) {
    X <- cbind(X, X^2)
  }
  # Native Hainmueller (2012) entropy balancing, ATT: reweight controls
  # so their covariate moments match the treated moments. Weight scale
  # follows ebal::ebalance (control weights sum to n_control);
  # cross-validated against ebal + WeightIt in tests.
  fit <- .morie_entropy_balance(t_mask, X, max_iter = max_iter)
  if (!fit$converged) {
    warning("entropy balancing did not fully converge; ",
            "max moment imbalance = ",
            format(fit$max_imbalance, digits = 3), call. = FALSE)
  }
  w <- rep(1.0, nrow(df))
  w[!t_mask] <- as.numeric(fit$w)
  names(w) <- rownames(df)
  w
}

#' Genetic matching (Diamond & Sekhon, 2013)
#'
#' Native rmorie implementation of genetic matching (Diamond & Sekhon
#' 2013): a real-coded genetic algorithm searches the diagonal
#' Mahalanobis weight matrix maximizing worst-case covariate balance
#' (paired-t p-value of the worst covariate). Deterministic given
#' \code{seed}. No Matching/rgenoud at runtime; returns a
#' \code{morie_match_result} with the selected weights in
#' \code{details$best_weights}.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param n_neighbors Number of matches per treated unit
#'   (\code{M} in \pkg{Matching}).
#' @param pop_size Genetic-algorithm population size (default 50).
#' @param n_generations Number of GA generations.
#' @param seed Random seed.
#' @return A list of class \code{morie_match_result}.
#' @references Diamond, A., & Sekhon, J. S. (2013). Genetic matching for
#'   estimating causal effects.  \emph{Review of Economics and
#'   Statistics}, 95(3), 932--945.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_genetic(df, "d", c("x1", "x2"),
#'                        pop_size = 50, n_generations = 20)
#' }
#' @export
morie_matching_genetic <- function(data, treatment, covariates,
                                   n_neighbors = 1L,
                                   pop_size = 50L,
                                   n_generations = 20L,
                                   seed = 42L) {
  .morie_match_genetic_native(data, treatment, covariates,
                              n_neighbors = n_neighbors,
                              pop_size = pop_size,
                              n_generations = n_generations,
                              seed = seed)
}

#' Variable-ratio matching on propensity score
#'
#' Thin wrapper around \code{MatchIt::matchit(method = "nearest",
#' ratio = max_ratio, min.controls = min_ratio)} which supports
#' variable-ratio nearest-neighbour matching natively.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param min_ratio,max_ratio Match-count bounds per treated unit.
#' @param caliper Caliper on the propensity score (in SD units).
#' @param ps Optional pre-computed propensity scores (ignored; retained
#'   for back-compat).
#' @return A list of class \code{morie_match_result}.
#' @examplesIf requireNamespace("MatchIt", quietly = TRUE)
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_variable_ratio(df, "d", c("x1", "x2"),
#'                               min_ratio = 1, max_ratio = 3)
#' }
#' @export
morie_matching_variable_ratio <- function(data, treatment, covariates,
                                          min_ratio = 1L,
                                          max_ratio = 5L,
                                          caliper = 0.2,
                                          ps = NULL) {
  .morie_matching_need_matchit("morie_matching_variable_ratio")
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  f <- stats::as.formula(paste(treatment, "~",
                               paste(covariates, collapse = " + ")))
  min_ratio <- as.integer(min_ratio)
  max_ratio <- as.integer(max_ratio)
  if (max_ratio > min_ratio) {
    # MatchIt requires min.controls <= ratio < max.controls for
    # variable-ratio matching: `ratio` is the target AVERAGE number of
    # controls, not the maximum.
    target <- min(max_ratio - 1L,
                  as.integer(ceiling((min_ratio + max_ratio) / 2)))
    target <- max(target, min_ratio)
    mi <- MatchIt::matchit(
      f, data = df,
      method       = "nearest",
      distance     = "glm",
      ratio        = target,
      min.controls = min_ratio,
      max.controls = max_ratio,
      caliper      = caliper,
      replace      = FALSE
    )
  } else {
    # Degenerate bounds (min == max): plain fixed-ratio matching.
    mi <- MatchIt::matchit(
      f, data = df,
      method   = "nearest",
      distance = "glm",
      ratio    = max_ratio,
      caliper  = caliper,
      replace  = FALSE
    )
  }
  .morie_matching_matchit_to_result(
    mi, df, treatment,
    method_label = "variable_ratio (MatchIt)",
    details = list(min_ratio = min_ratio,
                   max_ratio = max_ratio,
                   caliper   = caliper)
  )
}


# ---------------------------------------------------------------------------
# Cardinality matching (rmorie-specific iterative-caliper heuristic)
# ---------------------------------------------------------------------------

#' Cardinality matching
#'
#' Finds the largest matched sample with maximum absolute SMD below
#' \code{balance_threshold}.  Uses an iterative caliper-tightening
#' heuristic over \code{morie_matching_nearest_neighbor}; for an
#' exact mixed-integer-programming alternative see
#' \code{\link[designmatch:cardmatch]{designmatch::cardmatch}}.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param balance_threshold Maximum absolute SMD tolerated (default 0.1).
#' @param ps Optional pre-computed propensity scores.
#' @return A list of class \code{morie_match_result}.
#' @references Zubizarreta, J. R. (2012). Using mixed integer programming for
#'   matching in an observational study of kidney failure after surgery.
#'   \emph{JASA}, 107(500), 1360--1371.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_cardinality(df, "d", c("x1", "x2"),
#'                            balance_threshold = 0.1)
#' }
#' @export
morie_matching_cardinality <- function(data, treatment, covariates,
                                       balance_threshold = 0.1,
                                       ps = NULL) {
  best_result <- NULL
  calipers <- list(NULL, 0.5, 0.3, 0.2, 0.15, 0.1, 0.05)
  # Track repeated MatchIt warnings across calipers; collapse to a
  # single summary at the end so we don't emit one per caliper.
  n_few_ctrl_warn <- 0L
  ctrl_warn_pattern <- "Fewer control units than treated"
  call_nn <- function(...) {
    withCallingHandlers(
      morie_matching_nearest_neighbor(...),
      warning = function(w) {
        if (grepl(ctrl_warn_pattern, conditionMessage(w))) {
          n_few_ctrl_warn <<- n_few_ctrl_warn + 1L
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  for (cal in calipers) {
    res <- call_nn(data, treatment, covariates,
                   caliper = cal, replace = FALSE, ps = ps)
    if (!nrow(res$matched_data)) next
    bal <- morie_matching_balance(res$matched_data, treatment, covariates)
    if (bal$max_smd <= balance_threshold) {
      if (is.null(best_result) ||
          (res$n_treated + res$n_matched_control >
             best_result$n_treated + best_result$n_matched_control)) {
        res$method <- "cardinality"
        res$details$balance_threshold <- balance_threshold
        best_result <- res
        break
      }
    }
  }
  if (is.null(best_result)) {
    best_result <- call_nn(data, treatment, covariates, ps = ps)
    best_result$method <- "cardinality"
    best_result$details$warning <- "Balance threshold not achieved."
  }
  if (n_few_ctrl_warn > 0L) {
    warning(sprintf(
      "%d of %d caliper passes had fewer control units than treated; not all treated units in those passes got a match.",
      n_few_ctrl_warn, length(calipers)), call. = FALSE)
  }
  best_result
}


# ---------------------------------------------------------------------------
# Balance diagnostics
# ---------------------------------------------------------------------------

#' Balance diagnostics for matched / weighted samples
#'
#' Reports standardised mean differences (SMD), variance ratios, and
#' Kolmogorov-Smirnov statistics for each covariate.  For a richer
#' covariate-balance report (including continuous + categorical handling
#' and Love-plot rendering), see
#' \code{\link[cobalt:bal.tab]{cobalt::bal.tab}} /
#' \code{\link[cobalt:love.plot]{cobalt::love.plot}}.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param weights Optional column name of matching / weighting weights.
#' @param threshold Absolute-SMD threshold for the \code{balanced} flag.
#' @return A list of class \code{morie_balance_result} with
#'   \code{balance_table} (a data frame) and scalar summaries
#'   \code{overall_balance}, \code{max_smd}, \code{balanced}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_balance(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_balance <- function(data, treatment, covariates,
                                   weights = NULL, threshold = 0.1) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  t_mask <- df[[treatment]] == 1
  c_mask <- df[[treatment]] == 0
  recs <- list()
  for (cov in covariates) {
    t_vals <- as.numeric(df[t_mask, cov])
    c_vals <- as.numeric(df[c_mask, cov])
    if (!is.null(weights) && weights %in% colnames(df)) {
      w_t <- as.numeric(df[t_mask, weights])
      w_c <- as.numeric(df[c_mask, weights])
      mean_t <- stats::weighted.mean(t_vals, w_t)
      mean_c <- stats::weighted.mean(c_vals, w_c)
      var_t <- stats::weighted.mean((t_vals - mean_t)^2, w_t)
      var_c <- stats::weighted.mean((c_vals - mean_c)^2, w_c)
    } else {
      mean_t <- mean(t_vals)
      mean_c <- mean(c_vals)
      var_t <- if (length(t_vals) > 1L) stats::var(t_vals) else 0
      var_c <- if (length(c_vals) > 1L) stats::var(c_vals) else 0
    }
    pooled_sd <- sqrt((var_t + var_c) / 2)
    smd <- if (pooled_sd > 0) (mean_t - mean_c) / pooled_sd else 0
    var_ratio <- if (var_c > 0) var_t / var_c else NA_real_
    ks <- tryCatch(stats::ks.test(t_vals, c_vals),
                   error = function(e) list(statistic = NA_real_,
                                            p.value = NA_real_),
                   warning = function(w)
                     suppressWarnings(stats::ks.test(t_vals, c_vals)))
    recs[[length(recs) + 1L]] <- data.frame(
      covariate       = cov,
      mean_treated    = mean_t,
      mean_control    = mean_c,
      smd             = smd,
      abs_smd         = abs(smd),
      variance_ratio  = var_ratio,
      ks_stat         = as.numeric(ks$statistic),
      ks_p_value      = as.numeric(ks$p.value),
      stringsAsFactors = FALSE
    )
  }
  bal_df <- if (length(recs)) do.call(rbind, recs) else
    data.frame(covariate = character(0), mean_treated = numeric(0),
               mean_control = numeric(0), smd = numeric(0),
               abs_smd = numeric(0), variance_ratio = numeric(0),
               ks_stat = numeric(0), ks_p_value = numeric(0))
  overall <- if (nrow(bal_df)) mean(bal_df$abs_smd, na.rm = TRUE) else 0
  max_smd <- if (nrow(bal_df)) max(bal_df$abs_smd, na.rm = TRUE) else 0

  out <- list(
    balance_table   = bal_df,
    overall_balance = as.numeric(overall),
    max_smd         = as.numeric(max_smd),
    balanced        = isTRUE(max_smd <= threshold)
  )
  class(out) <- c("morie_balance_result", "list")
  out
}

#' Love-plot data: pre- vs post-matching balance
#'
#' Returns a data frame suitable for plotting absolute SMDs before and
#' after matching.  For a publication-ready plot, pass the same
#' \code{matchit} object to \code{\link[cobalt:love.plot]{cobalt::love.plot}}.
#'
#' @param unmatched_data,matched_data Data frames.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param weights_col Optional column of matching weights in
#'   \code{matched_data}.
#' @return A data frame with columns \code{covariate}, \code{smd_before},
#'   \code{smd_after}, \code{abs_smd_before}, \code{abs_smd_after}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
#' morie_matching_love_plot_data(df, res$matched_data,
#'                               "d", c("x1", "x2"))
#' }
#' @export
morie_matching_love_plot_data <- function(unmatched_data, matched_data,
                                          treatment, covariates,
                                          weights_col = NULL) {
  before <- morie_matching_balance(unmatched_data, treatment, covariates)
  after  <- morie_matching_balance(matched_data, treatment, covariates,
                                   weights = weights_col)
  b_smd <- setNames(before$balance_table$smd, before$balance_table$covariate)
  a_smd <- setNames(after$balance_table$smd,  after$balance_table$covariate)
  res <- data.frame(
    covariate  = covariates,
    smd_before = as.numeric(b_smd[covariates]),
    smd_after  = as.numeric(a_smd[covariates]),
    stringsAsFactors = FALSE
  )
  res$abs_smd_before <- abs(res$smd_before)
  res$abs_smd_after  <- abs(res$smd_after)
  res
}

#' Publication-ready balance table
#'
#' Thin wrapper around \code{morie_matching_balance} returning only the
#' data-frame component.  See \code{\link[cobalt:bal.tab]{cobalt::bal.tab}}
#' for an alternative with categorical-variable support.
#'
#' @inheritParams morie_matching_balance
#' @return A data frame.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_balance_table(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_balance_table <- function(data, treatment, covariates,
                                         weights = NULL) {
  morie_matching_balance(data, treatment, covariates,
                         weights = weights)$balance_table
}


# ---------------------------------------------------------------------------
# Treatment effect estimation from matched samples
# ---------------------------------------------------------------------------

#' @keywords internal
.morie_matching_te_empty <- function(estimand) {
  out <- list(
    estimand   = estimand,
    estimate   = NA_real_,
    std_error  = NA_real_,
    ci_lower   = NA_real_,
    ci_upper   = NA_real_,
    p_value    = NA_real_,
    n_obs      = 0L,
    details    = list()
  )
  class(out) <- c("morie_te_result", "list")
  out
}

#' @keywords internal
.morie_matching_te_result <- function(estimand, estimate, se, n_obs,
                                      alpha = 0.05, details = list()) {
  z <- if (se > 0) estimate / se else 0
  p_val <- 2 * stats::pnorm(-abs(z))
  cv <- stats::qnorm(1 - alpha / 2)
  out <- list(
    estimand   = estimand,
    estimate   = as.numeric(estimate),
    std_error  = as.numeric(se),
    ci_lower   = as.numeric(estimate - cv * se),
    ci_upper   = as.numeric(estimate + cv * se),
    p_value    = as.numeric(p_val),
    n_obs      = as.integer(n_obs),
    details    = details
  )
  class(out) <- c("morie_te_result", "list")
  out
}

#' ATT from a matched sample
#'
#' Estimates the Average Treatment effect on the Treated using paired
#' differences from a matched sample.  Uses the explicit \code{_matched}
#' suffix to distinguish it from the IPW estimator
#' \code{morie_estimate_att} in \code{causal.R}.
#'
#' @param data Data frame.
#' @param outcome Outcome column name.
#' @param treatment Binary treatment column name.
#' @param match_pairs Data frame with columns \code{treated_idx} and
#'   \code{control_idx}.
#' @param weights Optional column of matching weights.
#' @param alpha Significance level for confidence intervals.
#' @return A list of class \code{morie_te_result}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
#' morie_matching_att_matched(df, "y", "d", res$match_pairs)
#' }
#' @export
morie_matching_att_matched <- function(data, outcome, treatment,
                                       match_pairs, weights = NULL,
                                       alpha = 0.05) {
  if (!nrow(match_pairs)) return(.morie_matching_te_empty("ATT"))
  diffs <- numeric(0)
  for (k in seq_len(nrow(match_pairs))) {
    t_id <- match_pairs$treated_idx[k]
    c_id <- match_pairs$control_idx[k]
    if (t_id %in% rownames(data) && c_id %in% rownames(data)) {
      diffs <- c(diffs, as.numeric(data[t_id, outcome]) -
                   as.numeric(data[c_id, outcome]))
    }
  }
  if (!length(diffs)) return(.morie_matching_te_empty("ATT"))
  att <- mean(diffs)
  se  <- stats::sd(diffs) / sqrt(length(diffs))
  .morie_matching_te_result("ATT", att, se, length(diffs), alpha)
}

#' ATE from a matched / weighted sample
#'
#' Estimates the Average Treatment Effect via a (weighted) mean difference
#' between treated and control outcomes.  Uses the explicit
#' \code{_matched} suffix to distinguish it from the IPW estimator
#' \code{morie_estimate_ate} in \code{causal.R}.
#'
#' @param data Data frame.
#' @param outcome,treatment Column names.
#' @param covariates Character vector of covariates (carried for parity
#'   with the Python signature).
#' @param weights Optional column of matching / weighting weights.
#' @param alpha Significance level for confidence intervals.
#' @return A list of class \code{morie_te_result}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' m <- morie_matching_cem(df, "d", c("x1", "x2"), n_bins = 5)
#' morie_matching_ate_matched(m$matched_data, "y", "d", c("x1", "x2"),
#'                            weights = "weights")
#' }
#' @export
morie_matching_ate_matched <- function(data, outcome, treatment, covariates,
                                       weights = NULL, alpha = 0.05) {
  df <- .morie_matching_drop_na(data, c(outcome, treatment))
  t_mask <- df[[treatment]] == 1
  c_mask <- df[[treatment]] == 0
  y_t <- as.numeric(df[t_mask, outcome])
  y_c <- as.numeric(df[c_mask, outcome])
  if (!is.null(weights) && weights %in% colnames(df)) {
    w_t <- as.numeric(df[t_mask, weights])
    w_c <- as.numeric(df[c_mask, weights])
    mean_t <- stats::weighted.mean(y_t, w_t)
    mean_c <- stats::weighted.mean(y_c, w_c)
    var_t <- stats::weighted.mean((y_t - mean_t)^2, w_t)
    var_c <- stats::weighted.mean((y_c - mean_c)^2, w_c)
    n_eff_t <- sum(w_t)^2 / sum(w_t^2)
    n_eff_c <- sum(w_c)^2 / sum(w_c^2)
    se <- sqrt(var_t / n_eff_t + var_c / n_eff_c)
  } else {
    mean_t <- mean(y_t)
    mean_c <- mean(y_c)
    se <- sqrt(stats::var(y_t) / length(y_t) +
                 stats::var(y_c) / length(y_c))
  }
  ate <- mean_t - mean_c
  .morie_matching_te_result("ATE", ate, se, nrow(df), alpha)
}

#' ATC from a matched sample
#'
#' Estimates the Average Treatment Effect on the Controls.  Uses the
#' explicit \code{_matched} suffix to distinguish it from the IPW estimator
#' \code{morie_estimate_atc} in \code{causal.R}.
#'
#' @inheritParams morie_matching_att_matched
#' @return A list of class \code{morie_te_result}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
#' morie_matching_atc_matched(df, "y", "d", res$match_pairs)
#' }
#' @export
morie_matching_atc_matched <- function(data, outcome, treatment,
                                       match_pairs, alpha = 0.05) {
  if (!nrow(match_pairs)) return(.morie_matching_te_empty("ATC"))
  diffs <- numeric(0)
  for (k in seq_len(nrow(match_pairs))) {
    t_id <- match_pairs$treated_idx[k]
    c_id <- match_pairs$control_idx[k]
    if (t_id %in% rownames(data) && c_id %in% rownames(data)) {
      diffs <- c(diffs, as.numeric(data[t_id, outcome]) -
                   as.numeric(data[c_id, outcome]))
    }
  }
  if (!length(diffs)) return(.morie_matching_te_empty("ATC"))
  atc <- mean(diffs)
  se  <- stats::sd(diffs) / sqrt(length(diffs))
  .morie_matching_te_result("ATC", atc, se, length(diffs), alpha)
}


# ---------------------------------------------------------------------------
# Abadie-Imbens standard error
# ---------------------------------------------------------------------------

#' Abadie-Imbens standard error for matching estimators
#'
#' Computes the conditional-variance Abadie-Imbens SE accounting for the
#' fact that matching introduces correlation across matched observations.
#'
#' @param data Data frame.
#' @param outcome,treatment Column names.
#' @param match_pairs Data frame of matched indices.
#' @param n_matches Number of matches per treated unit (carried for parity).
#' @return Scalar numeric Abadie-Imbens SE.
#' @references Abadie, A., & Imbens, G. W. (2006). Large sample properties
#'   of matching estimators for average treatment effects.
#'   \emph{Econometrica}, 74(1), 235--267.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
#' morie_matching_abadie_imbens_se(df, "y", "d", res$match_pairs)
#' }
#' @export
morie_matching_abadie_imbens_se <- function(data, outcome, treatment,
                                            match_pairs, n_matches = 1L) {
  df <- .morie_matching_drop_na(data, c(outcome, treatment))
  n <- nrow(df)
  y <- as.numeric(df[[outcome]])
  idx_to_pos <- setNames(seq_len(n), rownames(df))
  K <- numeric(n)
  for (k in seq_len(nrow(match_pairs))) {
    c_id <- match_pairs$control_idx[k]
    if (c_id %in% names(idx_to_pos)) {
      pos <- idx_to_pos[[c_id]]
      K[pos] <- K[pos] + 1
    }
  }
  sigma2 <- numeric(n)
  for (k in seq_len(nrow(match_pairs))) {
    t_id <- match_pairs$treated_idx[k]
    c_id <- match_pairs$control_idx[k]
    if (t_id %in% names(idx_to_pos) && c_id %in% names(idx_to_pos)) {
      tp <- idx_to_pos[[t_id]]
      cp <- idx_to_pos[[c_id]]
      diff2 <- (y[tp] - y[cp])^2 / 2
      sigma2[tp] <- diff2
      sigma2[cp] <- diff2
    }
  }
  # Abadie-Imbens (2006) eq 14 for ATT:
  #   V_ATT = (1/N_t^2) * [ sum_{i: D=1} sigma^2(X_i, 1)
  #                       + sum_{j: D=0} K_j^2 * sigma^2(X_j, 0) ]
  t_vec <- as.integer(df[[treatment]])
  if (.morie_matching_have_cpp("morie_matching_abadie_imbens_kernel_cpp")) {
    V <- morie_matching_abadie_imbens_kernel_cpp(
      y, t_vec,
      as.integer(seq_len(n)[match(match_pairs$treated_idx,
                                   rownames(df))]),
      as.integer(seq_len(n)[match(match_pairs$control_idx,
                                   rownames(df))]))
    return(sqrt(max(V, 0)))
  }
  is_t <- t_vec == 1L
  n_treated <- max(sum(is_t), 1L)
  V <- (sum(sigma2[is_t]) + sum((K[!is_t]^2) * sigma2[!is_t])) /
    n_treated^2
  sqrt(max(V, 0))
}


# ---------------------------------------------------------------------------
# Rosenbaum bounds (sensitivity analysis)
# ---------------------------------------------------------------------------

#' Rosenbaum bounds for hidden bias
#'
#' Computes bounds on the p-value for the treatment effect over a grid of
#' values of \code{gamma} (the maximum odds ratio of differential treatment
#' assignment due to an unobserved confounder).  Uses the Wilcoxon
#' signed-rank approach.  For exact bounds, see
#' \code{\link[sensitivitymv:senmv]{sensitivitymv::senmv}} or the
#' \pkg{rbounds} package.
#'
#' @param data Data frame.
#' @param outcome,treatment Column names.
#' @param match_pairs Data frame of matched indices.
#' @param gamma_range Optional numeric vector of \eqn{\Gamma}{Gamma} values.
#' @return A data frame with columns \code{gamma}, \code{p_lower},
#'   \code{p_upper}, \code{significant_lower}, \code{significant_upper}.
#' @references Rosenbaum, P. R. (2002). \emph{Observational Studies}
#'   (2nd ed.).  Springer.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
#' morie_matching_rosenbaum_bounds(df, "y", "d", res$match_pairs)
#' }
#' @export
morie_matching_rosenbaum_bounds <- function(data, outcome, treatment,
                                            match_pairs,
                                            gamma_range = NULL) {
  if (is.null(gamma_range)) {
    gamma_range <- c(1.0, 1.1, 1.2, 1.3, 1.5, 1.75, 2.0, 2.5, 3.0)
  }
  diffs <- numeric(0)
  for (k in seq_len(nrow(match_pairs))) {
    t_id <- match_pairs$treated_idx[k]
    c_id <- match_pairs$control_idx[k]
    if (t_id %in% rownames(data) && c_id %in% rownames(data)) {
      diffs <- c(diffs, as.numeric(data[t_id, outcome]) -
                   as.numeric(data[c_id, outcome]))
    }
  }
  if (!length(diffs)) {
    return(data.frame(gamma = numeric(0), p_lower = numeric(0),
                      p_upper = numeric(0),
                      significant_lower = logical(0),
                      significant_upper = logical(0)))
  }
  n_pairs <- length(diffs)
  abs_diffs <- abs(diffs)
  ranks <- rank(abs_diffs)
  signs <- sign(diffs)
  T_plus <- sum(ranks[signs > 0])

  rows <- list()
  for (gamma in gamma_range) {
    if (gamma == 1.0) {
      E_T <- n_pairs * (n_pairs + 1) / 4
      V_T <- n_pairs * (n_pairs + 1) * (2 * n_pairs + 1) / 24
      z <- (T_plus - E_T) / sqrt(max(V_T, 1e-10))
      p_val <- 2 * stats::pnorm(-abs(z))
      rows[[length(rows) + 1L]] <- data.frame(
        gamma = gamma, p_lower = p_val, p_upper = p_val,
        significant_lower = p_val < 0.05,
        significant_upper = p_val < 0.05,
        stringsAsFactors = FALSE
      )
    } else {
      p_plus_upper <- gamma / (1 + gamma)
      p_plus_lower <- 1 / (1 + gamma)
      E_u <- sum(ranks * p_plus_upper)
      V_u <- sum(ranks^2 * p_plus_upper * (1 - p_plus_upper))
      z_u <- (T_plus - E_u) / sqrt(max(V_u, 1e-10))
      p_u <- stats::pnorm(z_u, lower.tail = FALSE)
      E_l <- sum(ranks * p_plus_lower)
      V_l <- sum(ranks^2 * p_plus_lower * (1 - p_plus_lower))
      z_l <- (T_plus - E_l) / sqrt(max(V_l, 1e-10))
      p_l <- stats::pnorm(z_l, lower.tail = FALSE)
      rows[[length(rows) + 1L]] <- data.frame(
        gamma = gamma,
        p_lower = min(p_l, p_u),
        p_upper = max(p_l, p_u),
        significant_lower = min(p_l, p_u) < 0.05,
        significant_upper = max(p_l, p_u) < 0.05,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# Doubly-robust estimation with matching
# ---------------------------------------------------------------------------

#' Doubly-robust ATT combining matching and regression
#'
#' Matches on the propensity score, then applies bias-corrected linear
#' regression adjustment within the matched sample.  Standard errors come
#' from a non-parametric bootstrap.
#'
#' @param data Data frame.
#' @param outcome,treatment Column names.
#' @param covariates Character vector of covariates.
#' @param ps Optional pre-computed propensity scores.
#' @param n_bootstrap Number of bootstrap replications.
#' @param seed Random seed.
#' @param alpha Significance level.
#' @return A list of class \code{morie_te_result} with estimand
#'   \code{"ATT_DR"}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_doubly_robust(df, "y", "d", c("x1", "x2"),
#'                              n_bootstrap = 50)  # 50 keeps the example fast
#' }
#' @export
morie_matching_doubly_robust <- function(data, outcome, treatment, covariates,
                                         ps = NULL, n_bootstrap = 200L,
                                         seed = 42L, alpha = 0.05) {
  set.seed(seed)
  df <- .morie_matching_drop_na(data, c(outcome, treatment, covariates))
  mr <- morie_matching_nearest_neighbor(df, treatment, covariates,
                                        n_neighbors = 1L, ps = ps)
  matched <- mr$matched_data
  c_mask <- matched[[treatment]] == 0
  t_mask <- matched[[treatment]] == 1
  X_c <- as.data.frame(matched[c_mask, covariates, drop = FALSE])
  y_c <- as.numeric(matched[c_mask, outcome])
  X_t <- as.data.frame(matched[t_mask, covariates, drop = FALSE])
  y_t <- as.numeric(matched[t_mask, outcome])
  fit <- stats::lm(y_c ~ ., data = cbind(y_c = y_c, X_c))
  y0_hat_t <- stats::predict(fit, newdata = X_t)
  att_dr <- mean(y_t - y0_hat_t)

  n <- nrow(df)
  boot_ests <- numeric(0)
  # Track the "Fewer control units than treated" warning that MatchIt
  # emits per-resample inside the bootstrap loop; we collapse N
  # individual warnings into a single summary at the end so the user
  # still gets the signal without 20+ duplicate messages.
  n_few_ctrl_warn <- 0L
  ctrl_warn_pattern <- "Fewer control units than treated"
  for (b in seq_len(n_bootstrap)) {
    idx <- sample.int(n, n, replace = TRUE)
    df_b <- df[idx, , drop = FALSE]
    rownames(df_b) <- as.character(seq_len(n))
    out_b <- tryCatch(
      withCallingHandlers({
        mr_b <- morie_matching_nearest_neighbor(df_b, treatment, covariates,
                                                n_neighbors = 1L)
        md_b <- mr_b$matched_data
        cm <- md_b[[treatment]] == 0
        tm <- md_b[[treatment]] == 1
        if (sum(cm) < 2 || sum(tm) < 2) return(NA_real_)
        Xc_b <- as.data.frame(md_b[cm, covariates, drop = FALSE])
        yc_b <- as.numeric(md_b[cm, outcome])
        lr <- stats::lm(yc_b ~ ., data = cbind(yc_b = yc_b, Xc_b))
        Xt_b <- as.data.frame(md_b[tm, covariates, drop = FALSE])
        y0h <- stats::predict(lr, newdata = Xt_b)
        mean(as.numeric(md_b[tm, outcome]) - y0h)
      }, warning = function(w) {
        if (grepl(ctrl_warn_pattern, conditionMessage(w))) {
          n_few_ctrl_warn <<- n_few_ctrl_warn + 1L
          invokeRestart("muffleWarning")
        }
      }),
      error = function(e) NA_real_)
    if (!is.na(out_b)) boot_ests <- c(boot_ests, out_b)
  }
  if (n_few_ctrl_warn > 0L) {
    warning(sprintf(
      "%d of %d bootstrap resamples had fewer control units than treated; not all treated units in those resamples got a match.",
      n_few_ctrl_warn, n_bootstrap), call. = FALSE)
  }
  se <- if (length(boot_ests) > 1L) stats::sd(boot_ests) else NA_real_

  .morie_matching_te_result(
    "ATT_DR", att_dr,
    ifelse(is.na(se), 0, se),
    nrow(matched), alpha,
    details = list(n_bootstrap = n_bootstrap,
                   n_successful_boots = length(boot_ests)))
}


# ---------------------------------------------------------------------------
# Matching with multiple treatments
# ---------------------------------------------------------------------------

#' Matching with multiple (> 2) treatment groups
#'
#' For each non-reference treatment level, matches treated units to the
#' reference group via the chosen binary matching method.
#'
#' @param data Data frame.
#' @param treatment Treatment column (may take more than two levels).
#' @param covariates Character vector of covariates.
#' @param reference_group Optional reference level (defaults to the
#'   modal level).
#' @param method One of \code{"nearest_neighbor"} or \code{"mahalanobis"}.
#' @return A named list whose keys are treatment levels and whose values
#'   are \code{morie_match_result} objects.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(treat3 = sample(0:2, 200, TRUE),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_multi_treatment(df, "treat3", c("x1", "x2"))
#' }
#' @export
morie_matching_multi_treatment <- function(data, treatment, covariates,
                                           reference_group = NULL,
                                           method = "nearest_neighbor") {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  levels <- sort(unique(df[[treatment]]))
  if (is.null(reference_group)) {
    tab <- table(df[[treatment]])
    reference_group <- names(tab)[which.max(tab)]
    # restore type if numeric
    if (is.numeric(df[[treatment]])) {
      reference_group <- as.numeric(reference_group)
    }
  }
  results <- list()
  for (lvl in levels) {
    if (identical(lvl, reference_group)) next
    df_b <- df[df[[treatment]] %in% c(lvl, reference_group), , drop = FALSE]
    df_b[["._treat_binary"]] <- as.integer(df_b[[treatment]] == lvl)
    mr <- if (method == "mahalanobis") {
      morie_matching_mahalanobis(df_b, "._treat_binary", covariates)
    } else {
      morie_matching_nearest_neighbor(df_b, "._treat_binary", covariates)
    }
    mr$details$treatment_level <- lvl
    mr$details$reference_group <- reference_group
    results[[as.character(lvl)]] <- mr
  }
  results
}


# ---------------------------------------------------------------------------
# Longitudinal / panel matching
# ---------------------------------------------------------------------------

#' Longitudinal matching for panel data
#'
#' Matches treated and control units on the basis of their pre-treatment
#' covariate values.
#'
#' @param data Panel data frame.
#' @param treatment Binary treatment indicator column.
#' @param covariates Character vector of covariates.
#' @param unit Column name identifying units.
#' @param time Column name identifying time.
#' @param treatment_time Column giving the (per-unit) start of treatment;
#'   non-finite values indicate never-treated.
#' @param n_pre_periods Number of pre-treatment periods to summarise.
#' @param method One of \code{"nearest_neighbor"} or \code{"mahalanobis"}.
#' @return A list of class \code{morie_match_result}.
#' @examples
#' \donttest{
#' set.seed(1)
#' panel <- data.frame(id = rep(1:40, each = 3), t = rep(1:3, 40),
#'                     x1 = rnorm(120),
#'                     d = rep(rbinom(40, 1, 0.5), each = 3))
#' panel$t0 <- ifelse(panel$d == 1, 3, NA)
#' morie_matching_longitudinal(panel, "d", "x1", unit = "id",
#'                             time = "t", treatment_time = "t0")
#' }
#' @export
morie_matching_longitudinal <- function(data, treatment, covariates, unit,
                                        time, treatment_time,
                                        n_pre_periods = 1L,
                                        method = "nearest_neighbor") {
  df <- data
  df[["._treat_time"]] <- as.numeric(df[[treatment_time]])
  unit_features <- list()
  for (u in unique(df[[unit]])) {
    u_data <- df[df[[unit]] == u, , drop = FALSE]
    u_data <- u_data[order(u_data[[time]]), , drop = FALSE]
    treat_t <- u_data[["._treat_time"]][1]
    if (is.finite(treat_t)) {
      pre_data <- u_data[u_data[[time]] < treat_t, , drop = FALSE]
      pre_data <- utils::tail(pre_data, n_pre_periods)
      is_treated <- 1L
    } else {
      pre_data <- utils::tail(u_data, n_pre_periods)
      is_treated <- 0L
    }
    if (!nrow(pre_data)) next
    feat <- list(`._unit` = u, `._treated` = is_treated)
    for (c in covariates) {
      feat[[c]] <- mean(as.numeric(pre_data[[c]]), na.rm = TRUE)
    }
    unit_features[[length(unit_features) + 1L]] <- as.data.frame(
      feat, stringsAsFactors = FALSE
    )
  }
  unit_df <- do.call(rbind, unit_features)
  rownames(unit_df) <- as.character(unit_df[["._unit"]])
  unit_df[["._unit"]] <- NULL
  mr <- if (method == "mahalanobis") {
    morie_matching_mahalanobis(unit_df, "._treated", covariates)
  } else {
    morie_matching_nearest_neighbor(unit_df, "._treated", covariates)
  }
  mr$method <- paste0("longitudinal_", method)
  mr
}


# ---------------------------------------------------------------------------
# Matching quality assessment
# ---------------------------------------------------------------------------

#' Comprehensive matching-quality assessment
#'
#' Compares balance before and after matching and reports percent bias
#' reduction, count of balanced covariates, and overlap statistics.
#'
#' @param unmatched_data,matched_data Data frames.
#' @param treatment Binary treatment column.
#' @param covariates Character vector of covariates.
#' @param weights Optional column of matching weights in \code{matched_data}.
#' @return A list with \code{balance_before}, \code{balance_after},
#'   \code{bias_reduction}, \code{mean_bias_reduction},
#'   \code{pct_balanced_before}, \code{pct_balanced_after},
#'   \code{n_obs_before}, \code{n_obs_after}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' res <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
#' morie_matching_quality(df, res$matched_data, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_quality <- function(unmatched_data, matched_data,
                                   treatment, covariates,
                                   weights = NULL) {
  bal_before <- morie_matching_balance(unmatched_data, treatment, covariates)
  bal_after  <- morie_matching_balance(matched_data, treatment, covariates,
                                       weights = weights)
  smd_before <- setNames(bal_before$balance_table$abs_smd,
                         bal_before$balance_table$covariate)
  smd_after  <- setNames(bal_after$balance_table$abs_smd,
                         bal_after$balance_table$covariate)
  bias_reduction <- setNames(rep(NA_real_, length(covariates)), covariates)
  for (c in covariates) {
    b <- as.numeric(smd_before[c])
    a <- as.numeric(smd_after[c])
    if (!is.na(b) && b > 0 && !is.na(a)) {
      bias_reduction[c] <- (1 - a / b) * 100
    }
  }
  n_bal_before <- sum(smd_before <= 0.1, na.rm = TRUE)
  n_bal_after  <- sum(smd_after  <= 0.1, na.rm = TRUE)
  list(
    balance_before      = bal_before,
    balance_after       = bal_after,
    bias_reduction      = as.list(bias_reduction),
    mean_bias_reduction = mean(bias_reduction, na.rm = TRUE),
    pct_balanced_before = if (length(covariates))
      n_bal_before / length(covariates) * 100 else 0,
    pct_balanced_after  = if (length(covariates))
      n_bal_after  / length(covariates) * 100 else 0,
    n_obs_before        = nrow(unmatched_data),
    n_obs_after         = nrow(matched_data)
  )
}


# ---------------------------------------------------------------------------
# Overlap diagnostics
# ---------------------------------------------------------------------------

#' Propensity-score overlap diagnostics
#'
#' Reports the propensity-score range overlap between treated and control,
#' the number / percentage of units off support, and the IPW effective
#' sample size.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column.
#' @param covariates Character vector of covariates.
#' @param ps Optional pre-computed propensity scores.
#' @return A list with \code{ps_summary} (per-group quantiles),
#'   \code{overlap_region}, \code{n_off_support}, \code{pct_off_support},
#'   and \code{effective_sample_size}.
#' @examples
#' \donttest{
#' set.seed(1)
#' df <- data.frame(y = rnorm(200), d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' morie_matching_overlap(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_overlap <- function(data, treatment, covariates,
                                   ps = NULL) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  if (is.null(ps)) {
    ps <- morie_matching_estimate_propensity(df, treatment, covariates)
  }
  df[["._ps"]] <- ps[rownames(df)]
  t_ps <- df[["._ps"]][df[[treatment]] == 1]
  c_ps <- df[["._ps"]][df[[treatment]] == 0]
  overlap_lower <- max(min(t_ps), min(c_ps))
  overlap_upper <- min(max(t_ps), max(c_ps))
  on_support <- df[["._ps"]] >= overlap_lower & df[["._ps"]] <= overlap_upper
  ps_clip <- pmin(pmax(df[["._ps"]], 0.01), 0.99)
  d <- as.numeric(df[[treatment]])
  ipw_w <- d + (1 - d) * ps_clip / (1 - ps_clip)
  ess <- sum(ipw_w)^2 / sum(ipw_w^2)
  ps_summary <- list(
    treated = stats::quantile(t_ps, probs = c(0, 0.25, 0.5, 0.75, 1),
                              na.rm = TRUE),
    control = stats::quantile(c_ps, probs = c(0, 0.25, 0.5, 0.75, 1),
                              na.rm = TRUE)
  )
  list(
    ps_summary            = ps_summary,
    overlap_region        = c(lower = overlap_lower, upper = overlap_upper),
    n_off_support         = sum(!on_support),
    pct_off_support       = mean(!on_support) * 100,
    effective_sample_size = as.numeric(ess)
  )
}
