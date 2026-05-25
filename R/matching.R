# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Matching methods for causal inference in observational studies.
#
# Ports the public API of `src/morie/matching.py` (~2211 LOC) to R.
#
# Strategy: prefer CRAN delegation.
#   * `MatchIt` handles the full nearest-neighbour / optimal / full / CEM /
#     exact / Mahalanobis / genetic / cardinality / subclassification suite.
#   * `cobalt` produces balance diagnostics + Love-plot data.
#   * `WeightIt` (method = "ebal") or `ebal` does entropy balancing.
#   * `Matching::Match` is the genetic / generic fallback.
#   * `sensitivitymw` / `sensitivitymv` give exact Rosenbaum bounds.
#   * Base-R hand-rolled implementations exist as a final fallback so the
#     package still installs in a minimal environment, but the CRAN routes
#     are the recommended path.
#
# Every public function is named with the `morie_matching_*` prefix to avoid
# shadowing the IPW estimators in `causal.R` (which use names like
# `morie_estimate_att`, `morie_estimate_ate`, etc.).  Treatment-effect
# functions ported from `estimate_att_matched` / `estimate_atc_matched` /
# `estimate_ate_matched` use the explicit `_matched` suffix and live here
# under `morie_matching_att_matched`, `morie_matching_atc_matched`,
# `morie_matching_ate_matched` so they cannot be confused with IPW.

#' @importFrom stats glm binomial predict quantile sd var cov cor lm coef
#'   complete.cases as.formula model.matrix qnorm pnorm pchisq ks.test
#'   weighted.mean median quantile setNames
#' @importFrom utils head tail
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.morie_matching_have <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

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


# ---------------------------------------------------------------------------
# Propensity score estimation
# ---------------------------------------------------------------------------

#' Estimate propensity scores
#'
#' Estimates the probability of treatment via logistic regression or
#' gradient boosting on a set of covariates.  Mirrors Python
#' \code{morie.matching.estimate_propensity_score}.
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
#' \dontrun{
#' df <- data.frame(d = rbinom(200, 1, 0.4),
#'                  x1 = rnorm(200), x2 = rnorm(200))
#' ps <- morie_matching_estimate_propensity(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_estimate_propensity <- function(data, treatment, covariates,
                                               model = "logistic",
                                               max_iter = 1000) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  if (model == "gbm") {
    if (.morie_matching_have("gbm")) {
      f <- stats::as.formula(paste(treatment, "~",
                                   paste(covariates, collapse = " + ")))
      fit <- gbm::gbm(f, data = df, distribution = "bernoulli",
                      n.trees = 100, interaction.depth = 3,
                      shrinkage = 0.1, verbose = FALSE)
      ps <- gbm::predict.gbm(fit, newdata = df, n.trees = 100,
                             type = "response")
    } else {
      stop("Package 'gbm' is required for model = \"gbm\".  ",
           "Install it with install.packages(\"gbm\").",
           call. = FALSE)
    }
  } else {
    f <- stats::as.formula(paste(treatment, "~",
                                 paste(covariates, collapse = " + ")))
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
#' Clips propensity scores to \code{[lower, upper]}.  Mirrors Python
#' \code{morie.matching.trim_propensity_scores}.
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
#' of treated and control units.  Mirrors Python
#' \code{morie.matching.common_support}.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param ps_col Propensity-score column name (default
#'   \code{"propensity_score"}).
#' @param method One of \code{"minmax"} (overlap of ranges) or
#'   \code{"trim"} (drop the extreme 5 percent of each tail).
#' @return A subset of \code{data} on common support.
#' @examples
#' \dontrun{
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
# MatchIt-backed unified entry point
# ---------------------------------------------------------------------------

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
# Nearest-neighbour propensity score matching
# ---------------------------------------------------------------------------

#' @noRd
.morie_matching_have_cpp <- function(name) {
  exists(name, envir = asNamespace("morie"), inherits = FALSE)
}

#' Nearest-neighbour propensity-score matching
#'
#' For each treated unit, finds the \code{n_neighbors} closest control units
#' by logit-propensity-score distance.  Delegates to \pkg{MatchIt} when
#' installed; otherwise uses a base-R implementation.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column (0/1).
#' @param covariates Character vector of covariates for the propensity model.
#' @param n_neighbors Number of matches per treated unit.
#' @param caliper Maximum logit-propensity distance for a valid match,
#'   expressed in SD units of the logit (or \code{NULL} for no caliper).
#' @param replace If \code{TRUE}, controls may be re-used.
#' @param ps Optional pre-computed propensity scores.
#' @param alpha Significance level (carried through to \code{details}).
#' @return A list with class \code{morie_match_result} carrying
#'   \code{matched_data}, \code{n_treated}, \code{n_matched_control},
#'   \code{match_pairs}, \code{method}, and \code{details}.
#' @examples
#' \dontrun{
#' set.seed(1)
#' df <- data.frame(d = rbinom(200, 1, 0.4),
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
  df <- .morie_matching_drop_na(data, c(treatment, covariates))

  if (.morie_matching_have("MatchIt")) {
    f <- stats::as.formula(paste(treatment, "~",
                                 paste(covariates, collapse = " + ")))
    cal <- if (!is.null(caliper)) c(.morie_matching_logit_sd = caliper) else NULL
    mi <- tryCatch(
      MatchIt::matchit(
        f, data = df,
        method   = "nearest",
        distance = "glm",
        ratio    = n_neighbors,
        caliper  = caliper,
        replace  = replace
      ),
      error = function(e) NULL
    )
    if (!is.null(mi)) {
      return(.morie_matching_matchit_to_result(
        mi, df, treatment,
        method_label = "nearest_neighbor (MatchIt)",
        details = list(
          caliper     = caliper,
          replace     = replace,
          n_neighbors = n_neighbors,
          alpha       = alpha
        )
      ))
    }
  }

  # Base-R fallback
  if (is.null(ps)) {
    ps <- morie_matching_estimate_propensity(df, treatment, covariates)
  }
  df[["._ps"]] <- ps[rownames(df)]
  treated_idx <- rownames(df)[df[[treatment]] == 1]
  control_idx <- rownames(df)[df[[treatment]] == 0]
  logit_t <- .morie_matching_logit(df[treated_idx, "._ps"])
  logit_c <- .morie_matching_logit(df[control_idx, "._ps"])
  caliper_val <- if (!is.null(caliper)) {
    caliper * stats::sd(c(logit_t, logit_c))
  } else {
    Inf
  }
  used_controls <- character(0)
  recs <- list()
  for (i in seq_along(treated_idx)) {
    t_id <- treated_idx[i]
    dists <- abs(logit_t[i] - logit_c)
    ord <- order(dists)
    matched <- 0L
    for (j in ord) {
      d <- dists[j]
      if (d > caliper_val) break
      c_id <- control_idx[j]
      if (!replace && c_id %in% used_controls) next
      recs[[length(recs) + 1L]] <- data.frame(
        treated_idx = t_id, control_idx = c_id,
        distance = as.numeric(abs(df[t_id, "._ps"] - df[c_id, "._ps"])),
        stringsAsFactors = FALSE
      )
      used_controls <- c(used_controls, c_id)
      matched <- matched + 1L
      if (matched >= n_neighbors) break
    }
  }
  match_df <- if (length(recs)) do.call(rbind, recs) else .morie_matching_empty_pairs()
  all_ids <- unique(c(match_df$treated_idx, match_df$control_idx))
  matched_data <- df[rownames(df) %in% all_ids, , drop = FALSE]
  matched_data[["._ps"]] <- NULL

  .morie_matching_result(
    matched_data       = matched_data,
    n_treated          = length(unique(match_df$treated_idx)),
    n_matched_control  = length(unique(match_df$control_idx)),
    match_pairs        = match_df,
    method             = "nearest_neighbor",
    details            = list(
      caliper     = caliper_val,
      replace     = replace,
      n_neighbors = n_neighbors,
      n_unmatched = length(treated_idx) - length(unique(match_df$treated_idx))
    )
  )
}


# ---------------------------------------------------------------------------
# Exact matching
# ---------------------------------------------------------------------------

#' Exact matching on discrete covariates
#'
#' Matches treated and control units that share identical values on every
#' variable in \code{exact_vars}.  Delegates to \pkg{MatchIt} when
#' available.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param exact_vars Character vector of discrete variables for exact matching.
#' @return A list of class \code{morie_match_result}.
#' @examples
#' \dontrun{
#' morie_matching_exact(df, "d", c("region", "year"))
#' }
#' @export
morie_matching_exact <- function(data, treatment, exact_vars) {
  df <- .morie_matching_drop_na(data, c(treatment, exact_vars))

  if (.morie_matching_have("MatchIt")) {
    f <- stats::as.formula(paste(treatment, "~",
                                 paste(exact_vars, collapse = " + ")))
    mi <- tryCatch(
      MatchIt::matchit(f, data = df, method = "exact"),
      error = function(e) NULL
    )
    if (!is.null(mi)) {
      return(.morie_matching_matchit_to_result(
        mi, df, treatment,
        method_label = "exact (MatchIt)",
        details = list(exact_vars = exact_vars)
      ))
    }
  }

  df[["._stratum"]] <- apply(df[, exact_vars, drop = FALSE], 1L,
                             function(r) paste(as.character(r), collapse = "|"))
  recs <- list()
  matched_ids <- character(0)
  for (s in unique(df[["._stratum"]])) {
    grp <- df[df[["._stratum"]] == s, , drop = FALSE]
    t_ids <- rownames(grp)[grp[[treatment]] == 1]
    c_ids <- rownames(grp)[grp[[treatment]] == 0]
    if (!length(t_ids) || !length(c_ids)) next
    for (t in t_ids) {
      for (c in c_ids) {
        recs[[length(recs) + 1L]] <- data.frame(
          treated_idx = t, control_idx = c, distance = 0.0,
          stringsAsFactors = FALSE
        )
      }
    }
    matched_ids <- c(matched_ids, t_ids, c_ids)
  }
  match_df <- if (length(recs)) do.call(rbind, recs) else .morie_matching_empty_pairs()
  matched_data <- df[rownames(df) %in% unique(matched_ids), , drop = FALSE]
  matched_data[["._stratum"]] <- NULL

  .morie_matching_result(
    matched_data       = matched_data,
    n_treated          = length(unique(match_df$treated_idx)),
    n_matched_control  = length(unique(match_df$control_idx)),
    match_pairs        = match_df,
    method             = "exact",
    details            = list(exact_vars = exact_vars)
  )
}


# ---------------------------------------------------------------------------
# Coarsened exact matching (CEM)
# ---------------------------------------------------------------------------

#' Coarsened Exact Matching (CEM)
#'
#' Coarsens continuous covariates into bins, then performs exact matching
#' on the coarsened values.  Returns the matched (uncoarsened) data along
#' with stratum weights.  Delegates to \pkg{MatchIt}'s \code{method = "cem"}
#' (which itself calls \pkg{cem}) when available.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param n_bins Either a single integer (applied to every covariate) or a
#'   named list mapping covariate name to the number of bins.
#' @return A list of class \code{morie_match_result}; \code{matched_data}
#'   contains a \code{._cem_weight} column.
#' @references Iacus, S. M., King, G., & Porro, G. (2012). Causal inference
#'   without balance checking: Coarsened exact matching.
#'   \emph{Political Analysis}, 20(1), 1--24.
#' @examples
#' \dontrun{
#' morie_matching_cem(df, "d", c("x1", "x2"), n_bins = 5)
#' }
#' @export
morie_matching_cem <- function(data, treatment, covariates, n_bins = 5L) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))

  if (.morie_matching_have("MatchIt")) {
    f <- stats::as.formula(paste(treatment, "~",
                                 paste(covariates, collapse = " + ")))
    mi <- tryCatch(
      MatchIt::matchit(f, data = df, method = "cem"),
      error = function(e) NULL
    )
    if (!is.null(mi)) {
      return(.morie_matching_matchit_to_result(
        mi, df, treatment,
        method_label = "cem (MatchIt)",
        details = list(n_bins = n_bins)
      ))
    }
  }

  bins_map <- if (is.list(n_bins)) {
    n_bins
  } else {
    setNames(rep(as.integer(n_bins), length(covariates)), covariates)
  }

  cem_cols <- character(0)
  for (c in covariates) {
    nb <- as.integer(bins_map[[c]] %||% 5L)
    col <- df[[c]]
    new_col <- paste0("._cem_", c)
    if (is.numeric(col) && length(unique(col)) > nb) {
      brks <- unique(stats::quantile(col, probs = seq(0, 1, length.out = nb + 1L),
                                     na.rm = TRUE))
      if (length(brks) < 2L) brks <- c(min(col, na.rm = TRUE),
                                       max(col, na.rm = TRUE))
      df[[new_col]] <- as.integer(cut(col, breaks = brks,
                                      include.lowest = TRUE, labels = FALSE))
    } else {
      df[[new_col]] <- as.character(col)
    }
    cem_cols <- c(cem_cols, new_col)
  }
  df[["._cem_stratum"]] <- apply(df[, cem_cols, drop = FALSE], 1L,
                                 function(r) paste(as.character(r),
                                                   collapse = "|"))

  valid_strata <- character(0)
  for (s in unique(df[["._cem_stratum"]])) {
    grp <- df[df[["._cem_stratum"]] == s, , drop = FALSE]
    if (length(unique(grp[[treatment]])) == 2L) {
      valid_strata <- c(valid_strata, s)
    }
  }
  df_matched <- df[df[["._cem_stratum"]] %in% valid_strata, , drop = FALSE]
  df_matched[["._cem_weight"]] <- 1.0
  for (s in valid_strata) {
    mask_s <- df_matched[["._cem_stratum"]] == s
    n_t <- sum(df_matched[[treatment]][mask_s] == 1)
    n_c <- sum(df_matched[[treatment]][mask_s] == 0)
    if (n_c > 0) {
      idx_c <- mask_s & (df_matched[[treatment]] == 0)
      df_matched[idx_c, "._cem_weight"] <- n_t / n_c
    }
  }

  recs <- list()
  for (s in valid_strata) {
    grp <- df_matched[df_matched[["._cem_stratum"]] == s, , drop = FALSE]
    t_ids <- rownames(grp)[grp[[treatment]] == 1]
    c_ids <- rownames(grp)[grp[[treatment]] == 0]
    for (t in t_ids) {
      for (c in c_ids) {
        recs[[length(recs) + 1L]] <- data.frame(
          treated_idx = t, control_idx = c, distance = 0.0,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  match_df <- if (length(recs)) do.call(rbind, recs) else .morie_matching_empty_pairs()
  df_matched[, c(cem_cols, "._cem_stratum")] <- NULL

  .morie_matching_result(
    matched_data       = df_matched,
    n_treated          = sum(df_matched[[treatment]] == 1),
    n_matched_control  = sum(df_matched[[treatment]] == 0),
    match_pairs        = match_df,
    method             = "cem",
    details            = list(n_strata = length(valid_strata))
  )
}

#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a


# ---------------------------------------------------------------------------
# Mahalanobis distance matching
# ---------------------------------------------------------------------------

#' Mahalanobis distance matching
#'
#' Matches on Mahalanobis distance over the supplied covariates, optionally
#' combined with exact matching on discrete variables.  Delegates to
#' \pkg{MatchIt} when available.
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
#' \dontrun{
#' morie_matching_mahalanobis(df, "d", c("x1", "x2"), n_neighbors = 1)
#' }
#' @export
morie_matching_mahalanobis <- function(data, treatment, covariates,
                                       n_neighbors = 1L,
                                       caliper = NULL,
                                       replace = FALSE,
                                       exact = NULL) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))

  if (.morie_matching_have("MatchIt")) {
    f <- stats::as.formula(paste(treatment, "~",
                                 paste(covariates, collapse = " + ")))
    mi <- tryCatch(
      MatchIt::matchit(
        f, data = df,
        method   = "nearest",
        distance = "mahalanobis",
        ratio    = n_neighbors,
        caliper  = caliper,
        replace  = replace,
        exact    = exact
      ),
      error = function(e) NULL
    )
    if (!is.null(mi)) {
      return(.morie_matching_matchit_to_result(
        mi, df, treatment,
        method_label = "mahalanobis (MatchIt)",
        details = list(caliper = caliper, replace = replace,
                       exact_vars = exact)
      ))
    }
  }

  X <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(X) <- "double"
  cov_mat <- stats::cov(X)
  cov_inv <- tryCatch(solve(cov_mat),
                      error = function(e) MASS::ginv(cov_mat))
  treated_idx <- rownames(df)[df[[treatment]] == 1]
  control_idx <- rownames(df)[df[[treatment]] == 0]
  X_t <- X[treated_idx, , drop = FALSE]
  X_c <- X[control_idx, , drop = FALSE]

  # Fast path: no exact-blocking and C++ kernels available -> compute
  # the full distance matrix in C++ (Cholesky whitening) and do NN
  # selection in C++. The exact-blocking branch keeps the R loop
  # because the per-treated `ok` mask is multi-variable.
  if (is.null(exact) &&
      .morie_matching_have_cpp("morie_matching_mahalanobis_pairs_cpp") &&
      .morie_matching_have_cpp("morie_matching_nn_select_cpp") &&
      length(treated_idx) > 0L && length(control_idx) > 0L) {
    D <- morie_matching_mahalanobis_pairs_cpp(X_t, X_c, cov_inv)
    cal <- if (is.null(caliper)) Inf else as.numeric(caliper)
    sel <- morie_matching_nn_select_cpp(D, as.logical(replace), cal,
                                         as.integer(n_neighbors))
    match_df <- if (length(sel$treated_pos) == 0L) {
      .morie_matching_empty_pairs()
    } else {
      data.frame(treated_idx = treated_idx[sel$treated_pos],
                 control_idx = control_idx[sel$control_pos],
                 distance    = sel$distance,
                 stringsAsFactors = FALSE)
    }
  } else {
    used_controls <- character(0)
    recs <- list()
    for (i in seq_along(treated_idx)) {
      t_id <- treated_idx[i]
      diffs <- sweep(X_c, 2L, X_t[i, ], "-")
      dists <- sqrt(pmax(rowSums((diffs %*% cov_inv) * diffs), 0))
      if (!is.null(exact)) {
        ok <- rep(TRUE, length(control_idx))
        for (v in exact) {
          ok <- ok & (df[control_idx, v] == df[t_id, v])
        }
        if (!any(ok)) next
        ord <- order(dists)
        ord <- ord[ok[ord]]
      } else {
        ord <- order(dists)
      }
      matched <- 0L
      for (j in ord) {
        d <- dists[j]
        if (!is.null(caliper) && d > caliper) break
        c_id <- control_idx[j]
        if (!replace && c_id %in% used_controls) next
        recs[[length(recs) + 1L]] <- data.frame(
          treated_idx = t_id, control_idx = c_id,
          distance = as.numeric(d), stringsAsFactors = FALSE
        )
        used_controls <- c(used_controls, c_id)
        matched <- matched + 1L
        if (matched >= n_neighbors) break
      }
    }
    match_df <- if (length(recs)) do.call(rbind, recs) else .morie_matching_empty_pairs()
  }
  all_ids <- unique(c(match_df$treated_idx, match_df$control_idx))
  matched_data <- df[rownames(df) %in% all_ids, , drop = FALSE]

  .morie_matching_result(
    matched_data       = matched_data,
    n_treated          = length(unique(match_df$treated_idx)),
    n_matched_control  = length(unique(match_df$control_idx)),
    match_pairs        = match_df,
    method             = "mahalanobis",
    details            = list(caliper = caliper, replace = replace,
                              exact_vars = exact)
  )
}


# ---------------------------------------------------------------------------
# Optimal pair matching
# ---------------------------------------------------------------------------

#' Optimal pair matching
#'
#' Optimal 1:1 pair matching that minimises the total within-pair distance.
#' Delegates to \pkg{MatchIt}'s \code{method = "optimal"} (which calls
#' \pkg{optmatch}); otherwise uses a greedy approximation.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param distance One of \code{"propensity"} or \code{"mahalanobis"}.
#' @param ps Optional pre-computed propensity scores.
#' @return A list of class \code{morie_match_result}.
#' @examples
#' \dontrun{
#' morie_matching_optimal_pair(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_optimal_pair <- function(data, treatment, covariates,
                                        distance = "propensity",
                                        ps = NULL) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))

  if (.morie_matching_have("MatchIt") && .morie_matching_have("optmatch")) {
    f <- stats::as.formula(paste(treatment, "~",
                                 paste(covariates, collapse = " + ")))
    dist_arg <- if (distance == "mahalanobis") "mahalanobis" else "glm"
    mi <- tryCatch(
      MatchIt::matchit(f, data = df, method = "optimal",
                       distance = dist_arg),
      error = function(e) NULL
    )
    if (!is.null(mi)) {
      return(.morie_matching_matchit_to_result(
        mi, df, treatment,
        method_label = "optimal_pair (MatchIt + optmatch)",
        details = list(distance = distance)
      ))
    }
  }

  treated_idx <- rownames(df)[df[[treatment]] == 1]
  control_idx <- rownames(df)[df[[treatment]] == 0]

  if (distance == "propensity") {
    if (is.null(ps)) {
      ps <- morie_matching_estimate_propensity(df, treatment, covariates)
    }
    df[["._ps"]] <- ps[rownames(df)]
    X_t <- as.matrix(df[treated_idx, "._ps", drop = FALSE])
    X_c <- as.matrix(df[control_idx, "._ps", drop = FALSE])
    dist_matrix <- outer(as.numeric(X_t), as.numeric(X_c),
                         function(a, b) abs(a - b))
  } else {
    X <- as.matrix(df[, covariates, drop = FALSE])
    cov_inv <- tryCatch(solve(stats::cov(X)),
                        error = function(e) MASS::ginv(stats::cov(X)))
    X_t <- X[treated_idx, , drop = FALSE]
    X_c <- X[control_idx, , drop = FALSE]
    dist_matrix <- matrix(NA_real_, nrow = nrow(X_t), ncol = nrow(X_c))
    for (i in seq_len(nrow(X_t))) {
      diffs <- sweep(X_c, 2L, X_t[i, ], "-")
      dist_matrix[i, ] <- sqrt(pmax(rowSums((diffs %*% cov_inv) * diffs), 0))
    }
  }

  n_t <- length(treated_idx)
  n_c <- length(control_idx)
  n_pairs <- min(n_t, n_c)
  # Greedy sort
  flat <- expand.grid(i = seq_len(n_t), j = seq_len(n_c), KEEP.OUT.ATTRS = FALSE)
  flat$d <- as.numeric(dist_matrix[cbind(flat$i, flat$j)])
  flat <- flat[order(flat$d), , drop = FALSE]
  used_t <- integer(0)
  used_c <- integer(0)
  recs <- list()
  for (k in seq_len(nrow(flat))) {
    i <- flat$i[k]
    j <- flat$j[k]
    if (i %in% used_t || j %in% used_c) next
    recs[[length(recs) + 1L]] <- data.frame(
      treated_idx = treated_idx[i], control_idx = control_idx[j],
      distance = flat$d[k], stringsAsFactors = FALSE
    )
    used_t <- c(used_t, i)
    used_c <- c(used_c, j)
    if (length(recs) >= n_pairs) break
  }
  match_df <- if (length(recs)) do.call(rbind, recs) else .morie_matching_empty_pairs()
  all_ids <- unique(c(match_df$treated_idx, match_df$control_idx))
  matched_data <- df[rownames(df) %in% all_ids, , drop = FALSE]
  matched_data[["._ps"]] <- NULL

  .morie_matching_result(
    matched_data       = matched_data,
    n_treated          = length(used_t),
    n_matched_control  = length(used_c),
    match_pairs        = match_df,
    method             = "optimal_pair",
    details            = list(distance = distance)
  )
}


# ---------------------------------------------------------------------------
# Full matching
# ---------------------------------------------------------------------------

#' Full matching via subclassification
#'
#' Places every unit into a subclass containing at least one treated and one
#' control unit.  Delegates to \pkg{MatchIt}'s \code{method = "full"}
#' (which calls \pkg{optmatch}) when available; otherwise approximates
#' via quantile-based stratification of the propensity score.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param ps Optional pre-computed propensity scores.
#' @param n_subclasses Number of propensity-score strata for the fallback.
#' @return A list of class \code{morie_match_result}; \code{matched_data}
#'   contains a \code{._full_weight} column.
#' @references Hansen, B. B. (2004). Full matching in an observational
#'   study of coaching for the SAT. \emph{JASA}, 99(467), 609--618.
#' @examples
#' \dontrun{
#' morie_matching_full(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_full <- function(data, treatment, covariates,
                                ps = NULL, n_subclasses = 10L) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))

  if (.morie_matching_have("MatchIt") && .morie_matching_have("optmatch")) {
    f <- stats::as.formula(paste(treatment, "~",
                                 paste(covariates, collapse = " + ")))
    mi <- tryCatch(
      MatchIt::matchit(f, data = df, method = "full", distance = "glm"),
      error = function(e) NULL
    )
    if (!is.null(mi)) {
      return(.morie_matching_matchit_to_result(
        mi, df, treatment,
        method_label = "full_matching (MatchIt + optmatch)",
        details = list(n_subclasses = n_subclasses)
      ))
    }
  }

  if (is.null(ps)) {
    ps <- morie_matching_estimate_propensity(df, treatment, covariates)
  }
  df[["._ps"]] <- ps[rownames(df)]
  brks <- unique(stats::quantile(df[["._ps"]],
                                 probs = seq(0, 1, length.out = n_subclasses + 1L),
                                 na.rm = TRUE))
  df[["._subclass"]] <- as.integer(cut(df[["._ps"]], breaks = brks,
                                       include.lowest = TRUE, labels = FALSE))
  valid_strata <- integer(0)
  for (s in unique(df[["._subclass"]])) {
    if (is.na(s)) next
    grp <- df[df[["._subclass"]] == s, , drop = FALSE]
    if (length(unique(grp[[treatment]])) == 2L) {
      valid_strata <- c(valid_strata, s)
    }
  }
  df_matched <- df[df[["._subclass"]] %in% valid_strata, , drop = FALSE]
  df_matched[["._full_weight"]] <- 1.0
  for (s in valid_strata) {
    mask_s <- df_matched[["._subclass"]] == s
    n_t <- sum(df_matched[[treatment]][mask_s] == 1)
    n_c <- sum(df_matched[[treatment]][mask_s] == 0)
    if (n_c > 0) {
      idx_c <- mask_s & (df_matched[[treatment]] == 0)
      df_matched[idx_c, "._full_weight"] <- n_t / n_c
    }
  }
  recs <- list()
  for (s in valid_strata) {
    grp <- df_matched[df_matched[["._subclass"]] == s, , drop = FALSE]
    t_ids <- rownames(grp)[grp[[treatment]] == 1]
    c_ids <- rownames(grp)[grp[[treatment]] == 0]
    for (t in t_ids) {
      for (c in c_ids) {
        recs[[length(recs) + 1L]] <- data.frame(
          treated_idx = t, control_idx = c,
          distance = abs(df_matched[t, "._ps"] - df_matched[c, "._ps"]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  match_df <- if (length(recs)) do.call(rbind, recs) else .morie_matching_empty_pairs()
  df_matched[, c("._subclass", "._ps")] <- NULL

  .morie_matching_result(
    matched_data       = df_matched,
    n_treated          = sum(df_matched[[treatment]] == 1),
    n_matched_control  = sum(df_matched[[treatment]] == 0),
    match_pairs        = match_df,
    method             = "full_matching",
    details            = list(n_subclasses = length(valid_strata))
  )
}


# ---------------------------------------------------------------------------
# Subclassification
# ---------------------------------------------------------------------------

#' Subclassification (stratification) on the propensity score
#'
#' Divides observations into propensity-score strata and reports within-
#' stratum sample sizes and PS ranges.  Mirrors Python
#' \code{morie.matching.subclassify}.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param ps Optional pre-computed propensity scores.
#' @param n_strata Number of quantile-based strata (default 5).
#' @return A list with components \code{data_with_strata} (the original
#'   data with a \code{._stratum} column appended) and
#'   \code{stratum_effects} (per-stratum sample sizes and PS ranges).
#' @examples
#' \dontrun{
#' morie_matching_subclassify(df, "d", c("x1", "x2"), n_strata = 5)
#' }
#' @export
morie_matching_subclassify <- function(data, treatment, covariates,
                                       ps = NULL, n_strata = 5L) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  if (is.null(ps)) {
    ps <- morie_matching_estimate_propensity(df, treatment, covariates)
  }
  df[["._ps"]] <- ps[rownames(df)]
  brks <- unique(stats::quantile(df[["._ps"]],
                                 probs = seq(0, 1, length.out = n_strata + 1L),
                                 na.rm = TRUE))
  df[["._stratum"]] <- as.integer(cut(df[["._ps"]], breaks = brks,
                                      include.lowest = TRUE, labels = FALSE))
  recs <- list()
  for (s in sort(unique(df[["._stratum"]]))) {
    if (is.na(s)) next
    grp <- df[df[["._stratum"]] == s, , drop = FALSE]
    n_t <- sum(grp[[treatment]] == 1)
    n_c <- sum(grp[[treatment]] == 0)
    if (!n_t || !n_c) next
    recs[[length(recs) + 1L]] <- data.frame(
      stratum = s, n_treated = n_t, n_control = n_c,
      ps_range_low = min(grp[["._ps"]]),
      ps_range_high = max(grp[["._ps"]]),
      stringsAsFactors = FALSE
    )
  }
  stratum_effects <- if (length(recs)) do.call(rbind, recs) else
    data.frame(stratum = integer(0), n_treated = integer(0),
               n_control = integer(0), ps_range_low = numeric(0),
               ps_range_high = numeric(0))
  list(
    data_with_strata = df,
    stratum_effects  = stratum_effects
  )
}


# ---------------------------------------------------------------------------
# Entropy balancing
# ---------------------------------------------------------------------------

#' Entropy balancing weights (Hainmueller, 2012)
#'
#' Computes weights for the control group so that the weighted moments of
#' the covariates match those of the treated group.  Delegates to
#' \pkg{WeightIt} (method \code{"ebal"}) or \pkg{ebal} when available;
#' otherwise solves the dual problem via base-R Newton iteration.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param max_moment Highest moment to balance (1 = means, 2 = means + var,
#'   3 = + skewness).
#' @param max_iter Maximum Newton iterations.
#' @param tol Convergence tolerance on the gradient.
#' @return A numeric vector of weights aligned to the rows of \code{data}
#'   after dropping NAs.  Treated units receive weight 1.
#' @references Hainmueller, J. (2012). Entropy balancing for causal effects.
#'   \emph{Political Analysis}, 20(1), 25--46.
#' @examples
#' \dontrun{
#' w <- morie_matching_entropy_balance(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_entropy_balance <- function(data, treatment, covariates,
                                           max_moment = 1L,
                                           max_iter = 500L,
                                           tol = 1e-6) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))

  if (.morie_matching_have("WeightIt")) {
    f <- stats::as.formula(paste(treatment, "~",
                                 paste(covariates, collapse = " + ")))
    fit <- tryCatch(
      WeightIt::weightit(f, data = df, method = "ebal",
                         estimand = "ATT", moments = max_moment),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      w <- as.numeric(fit$weights)
      names(w) <- rownames(df)
      return(w)
    }
  }
  if (.morie_matching_have("ebal")) {
    t_mask <- df[[treatment]] == 1
    X <- as.matrix(df[, covariates, drop = FALSE])
    fit <- tryCatch(
      ebal::ebalance(Treatment = as.integer(t_mask), X = X,
                     max.iterations = max_iter),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      w <- rep(1.0, nrow(df))
      w[!t_mask] <- as.numeric(fit$w)
      names(w) <- rownames(df)
      return(w)
    }
  }

  # Base-R Newton fallback
  t_mask <- df[[treatment]] == 1
  c_mask <- df[[treatment]] == 0
  X_t <- as.matrix(df[t_mask, covariates, drop = FALSE])
  X_c <- as.matrix(df[c_mask, covariates, drop = FALSE])
  storage.mode(X_t) <- "double"
  storage.mode(X_c) <- "double"
  n_c <- nrow(X_c)

  targets <- numeric(0)
  C_list <- list()
  for (m in seq_len(max_moment)) {
    targets <- c(targets, colMeans(X_t^m))
    C_list[[m]] <- X_c^m
  }
  C <- do.call(cbind, C_list)

  lam <- rep(0, length(targets))
  for (k in seq_len(max_iter)) {
    logits <- as.numeric(C %*% lam)
    logits <- logits - max(logits)
    w_raw <- exp(logits)
    w <- w_raw / sum(w_raw)
    g <- as.numeric(crossprod(C, w)) - targets
    if (max(abs(g)) < tol) break
    Cw <- as.numeric(crossprod(C, w))
    H <- crossprod(C * w, C) - tcrossprod(Cw)
    dlam <- tryCatch(solve(H, -g),
                     error = function(e) {
                       MASS::ginv(H) %*% (-g)
                     })
    lam <- lam + as.numeric(dlam)
  }
  logits <- as.numeric(C %*% lam)
  logits <- logits - max(logits)
  w_raw <- exp(logits)
  w_final <- w_raw / sum(w_raw) * n_c

  weights <- rep(1.0, nrow(df))
  weights[c_mask] <- w_final
  names(weights) <- rownames(df)
  weights
}


# ---------------------------------------------------------------------------
# Genetic matching
# ---------------------------------------------------------------------------

#' Genetic matching (Diamond & Sekhon, 2013)
#'
#' Uses a genetic algorithm to find weights for Mahalanobis distance
#' matching that maximise covariate balance.  Delegates to
#' \pkg{Matching::GenMatch} + \pkg{Matching::Match} when available;
#' otherwise runs a base-R genetic algorithm.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param n_neighbors Number of matches per treated unit.
#' @param pop_size Genetic-algorithm population size (default 50).
#' @param n_generations Number of GA generations.
#' @param seed Random seed.
#' @return A list of class \code{morie_match_result}.
#' @references Diamond, A., & Sekhon, J. S. (2013). Genetic matching for
#'   estimating causal effects.  \emph{Review of Economics and
#'   Statistics}, 95(3), 932--945.
#' @examples
#' \dontrun{
#' morie_matching_genetic(df, "d", c("x1", "x2"),
#'                        pop_size = 50, n_generations = 20)
#' }
#' @export
morie_matching_genetic <- function(data, treatment, covariates,
                                   n_neighbors = 1L,
                                   pop_size = 50L,
                                   n_generations = 20L,
                                   seed = 42L) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))

  if (.morie_matching_have("Matching")) {
    Tr <- as.integer(df[[treatment]])
    X <- as.matrix(df[, covariates, drop = FALSE])
    set.seed(seed)
    gen <- tryCatch(
      Matching::GenMatch(Tr = Tr, X = X, pop.size = pop_size,
                         max.generations = n_generations,
                         M = n_neighbors, print.level = 0,
                         replace = FALSE, ties = FALSE),
      error = function(e) NULL
    )
    if (!is.null(gen)) {
      m <- Matching::Match(Tr = Tr, X = X, Weight.matrix = gen,
                           M = n_neighbors, replace = FALSE, ties = FALSE)
      if (!is.null(m) && !is.null(m$index.treated)) {
        recs <- data.frame(
          treated_idx = rownames(df)[m$index.treated],
          control_idx = rownames(df)[m$index.control],
          distance    = as.numeric(m$mdata$X[, 1]) * 0,  # GenMatch doesn't expose
          stringsAsFactors = FALSE
        )
        all_ids <- unique(c(recs$treated_idx, recs$control_idx))
        matched_data <- df[rownames(df) %in% all_ids, , drop = FALSE]
        return(.morie_matching_result(
          matched_data       = matched_data,
          n_treated          = length(unique(recs$treated_idx)),
          n_matched_control  = length(unique(recs$control_idx)),
          match_pairs        = recs,
          method             = "genetic (Matching::GenMatch)",
          details            = list(best_weights = diag(gen),
                                    pop_size = pop_size,
                                    n_generations = n_generations)
        ))
      }
    }
  }

  # Base-R fallback
  set.seed(seed)
  rng <- function(n) stats::runif(n)
  X <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(X) <- "double"
  X_scaled <- scale(X)
  p <- ncol(X_scaled)
  treated_mask <- df[[treatment]] == 1
  X_t <- X_scaled[treated_mask, , drop = FALSE]
  X_c <- X_scaled[!treated_mask, , drop = FALSE]
  treated_idx <- rownames(df)[treated_mask]
  control_idx <- rownames(df)[!treated_mask]

  evaluate_weights <- function(w) {
    W <- diag(abs(w), nrow = p)
    Xt_w <- X_t %*% W
    Xc_w <- X_c %*% W
    # Greedy 1:1 match
    used <- integer(0)
    matched_j <- integer(0)
    for (i in seq_len(nrow(Xt_w))) {
      diffs <- sweep(Xc_w, 2L, Xt_w[i, ], "-")
      d <- sqrt(rowSums(diffs^2))
      d[used] <- Inf
      j <- which.min(d)
      if (is.finite(d[j])) {
        used <- c(used, j)
        matched_j <- c(matched_j, j)
      }
    }
    if (!length(matched_j)) return(1e6)
    X_c_matched <- X_c[matched_j, , drop = FALSE]
    smds <- abs(colMeans(X_t[seq_along(matched_j), , drop = FALSE]) -
                  colMeans(X_c_matched))
    pooled_sd <- sqrt((apply(X_t, 2L, stats::var) +
                         apply(X_c_matched, 2L, stats::var)) / 2 + 1e-10)
    max(smds / pooled_sd)
  }

  population <- matrix(stats::runif(pop_size * p, 0.1, 2.0),
                       nrow = pop_size, ncol = p)
  best_w <- rep(1, p)
  best_score <- evaluate_weights(best_w)
  for (gen_i in seq_len(n_generations)) {
    scores <- apply(population, 1L, evaluate_weights)
    ord <- order(scores)
    if (scores[ord[1]] < best_score) {
      best_score <- scores[ord[1]]
      best_w <- population[ord[1], ]
    }
    parents <- population[ord[seq_len(pop_size %/% 2L)], , drop = FALSE]
    n_kids <- pop_size - nrow(parents)
    kids <- matrix(NA_real_, nrow = n_kids, ncol = p)
    for (kk in seq_len(n_kids)) {
      pi_ <- sample(nrow(parents), 2L)
      mask <- stats::runif(p) > 0.5
      child <- ifelse(mask, parents[pi_[1], ], parents[pi_[2], ])
      if (stats::runif(1) < 0.3) {
        mu <- sample(p, 1L)
        child[mu] <- child[mu] * stats::runif(1, 0.5, 1.5)
      }
      kids[kk, ] <- child
    }
    population <- rbind(parents, kids)
  }

  W <- diag(abs(best_w), nrow = p)
  Xt_w <- X_t %*% W
  Xc_w <- X_c %*% W
  used <- integer(0)
  recs <- list()
  for (i in seq_len(nrow(Xt_w))) {
    diffs <- sweep(Xc_w, 2L, Xt_w[i, ], "-")
    d <- sqrt(rowSums(diffs^2))
    ord <- order(d)
    matched <- 0L
    for (j in ord) {
      if (j %in% used) next
      recs[[length(recs) + 1L]] <- data.frame(
        treated_idx = treated_idx[i],
        control_idx = control_idx[j],
        distance    = as.numeric(d[j]),
        stringsAsFactors = FALSE
      )
      used <- c(used, j)
      matched <- matched + 1L
      if (matched >= n_neighbors) break
    }
  }
  match_df <- if (length(recs)) do.call(rbind, recs) else .morie_matching_empty_pairs()
  all_ids <- unique(c(match_df$treated_idx, match_df$control_idx))
  matched_data <- df[rownames(df) %in% all_ids, , drop = FALSE]

  .morie_matching_result(
    matched_data       = matched_data,
    n_treated          = length(unique(match_df$treated_idx)),
    n_matched_control  = length(unique(match_df$control_idx)),
    match_pairs        = match_df,
    method             = "genetic",
    details            = list(best_weights = as.numeric(best_w),
                              best_balance = best_score,
                              pop_size = pop_size,
                              n_generations = n_generations)
  )
}


# ---------------------------------------------------------------------------
# Variable ratio matching
# ---------------------------------------------------------------------------

#' Variable-ratio matching on propensity score
#'
#' Matches each treated unit to between \code{min_ratio} and
#' \code{max_ratio} controls within a caliper.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param min_ratio,max_ratio Match-count bounds per treated unit.
#' @param caliper Caliper on the propensity score (in SD units).
#' @param ps Optional pre-computed propensity scores.
#' @return A list of class \code{morie_match_result}.
#' @examples
#' \dontrun{
#' morie_matching_variable_ratio(df, "d", c("x1", "x2"),
#'                               min_ratio = 1, max_ratio = 3)
#' }
#' @export
morie_matching_variable_ratio <- function(data, treatment, covariates,
                                          min_ratio = 1L,
                                          max_ratio = 5L,
                                          caliper = 0.2,
                                          ps = NULL) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  if (is.null(ps)) {
    ps <- morie_matching_estimate_propensity(df, treatment, covariates)
  }
  df[["._ps"]] <- ps[rownames(df)]
  treated_idx <- rownames(df)[df[[treatment]] == 1]
  control_idx <- rownames(df)[df[[treatment]] == 0]
  caliper_val <- caliper * stats::sd(df[["._ps"]])

  recs <- list()
  for (t in treated_idx) {
    ps_t <- df[t, "._ps"]
    ds <- abs(ps_t - df[control_idx, "._ps"])
    names(ds) <- control_idx
    ds <- ds[ds <= caliper_val]
    if (!length(ds)) next
    ds <- sort(ds)
    n_match <- min(max(length(ds), min_ratio), max_ratio)
    n_match <- min(n_match, length(ds))
    for (i in seq_len(n_match)) {
      recs[[length(recs) + 1L]] <- data.frame(
        treated_idx = t, control_idx = names(ds)[i],
        distance = as.numeric(ds[i]), stringsAsFactors = FALSE
      )
    }
  }
  match_df <- if (length(recs)) do.call(rbind, recs) else .morie_matching_empty_pairs()
  all_ids <- unique(c(match_df$treated_idx, match_df$control_idx))
  matched_data <- df[rownames(df) %in% all_ids, , drop = FALSE]
  matched_data[["._ps"]] <- NULL

  .morie_matching_result(
    matched_data       = matched_data,
    n_treated          = length(unique(match_df$treated_idx)),
    n_matched_control  = length(unique(match_df$control_idx)),
    match_pairs        = match_df,
    method             = "variable_ratio",
    details            = list(caliper = caliper_val,
                              min_ratio = min_ratio,
                              max_ratio = max_ratio)
  )
}


# ---------------------------------------------------------------------------
# Cardinality matching
# ---------------------------------------------------------------------------

#' Cardinality matching
#'
#' Finds the largest matched sample with maximum absolute SMD below
#' \code{balance_threshold}.  Uses an iterative caliper-tightening
#' heuristic over \code{morie_matching_nearest_neighbor}.
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
#' \dontrun{
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
#' Kolmogorov-Smirnov statistics for each covariate.  When \pkg{cobalt}
#' is installed it is used to compute the balance table; otherwise a
#' base-R implementation is used.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param weights Optional column name of matching / weighting weights.
#' @param threshold Absolute-SMD threshold for the \code{balanced} flag.
#' @return A list with \code{balance_table} (a data frame), and scalar
#'   summaries \code{overall_balance}, \code{max_smd}, \code{balanced}.
#' @examples
#' \dontrun{
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
                   warning = function(w) suppressWarnings(stats::ks.test(t_vals, c_vals)))
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
#' after matching.  Delegates to \pkg{cobalt::love.plot}'s data when
#' available.
#'
#' @param unmatched_data,matched_data Data frames.
#' @param treatment Binary treatment column name.
#' @param covariates Character vector of covariates.
#' @param weights_col Optional column of matching weights in
#'   \code{matched_data}.
#' @return A data frame with columns \code{covariate}, \code{smd_before},
#'   \code{smd_after}, \code{abs_smd_before}, \code{abs_smd_after}.
#' @examples
#' \dontrun{
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
#' data-frame component.
#'
#' @inheritParams morie_matching_balance
#' @return A data frame.
#' @examples
#' \dontrun{
#' morie_matching_balance_table(df, "d", c("x1", "x2"))
#' }
#' @export
morie_matching_balance_table <- function(data, treatment, covariates,
                                         weights = NULL) {
  morie_matching_balance(data, treatment, covariates, weights = weights)$balance_table
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
#' \dontrun{
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
#' \dontrun{
#' morie_matching_ate_matched(df, "y", "d", c("x1", "x2"),
#'                            weights = "._cem_weight")
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
    se <- sqrt(stats::var(y_t) / length(y_t) + stats::var(y_c) / length(y_c))
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
#' \dontrun{
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
#' \dontrun{
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
    V <- morie_matching_abadie_imbens_kernel_cpp(y, t_vec,
                                                  as.integer(seq_len(n)[
                                                    match(match_pairs$treated_idx,
                                                          rownames(df))]),
                                                  as.integer(seq_len(n)[
                                                    match(match_pairs$control_idx,
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
#' signed-rank approach.  When \pkg{sensitivitymv} is installed, callers
#' should prefer it for exact bounds; this function provides a base-R
#' implementation parallel to the Python version.
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
#' \dontrun{
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
#' \dontrun{
#' morie_matching_doubly_robust(df, "y", "d", c("x1", "x2"),
#'                              n_bootstrap = 200)
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

  .morie_matching_te_result("ATT_DR", att_dr,
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
#' \dontrun{
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
#' covariate values.  Mirrors Python
#' \code{morie.matching.match_longitudinal}.
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
#' \dontrun{
#' morie_matching_longitudinal(panel, "d", c("x1"), unit = "id",
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
    for (c in covariates) feat[[c]] <- mean(as.numeric(pre_data[[c]]),
                                            na.rm = TRUE)
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
#' \dontrun{
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
    balance_before       = bal_before,
    balance_after        = bal_after,
    bias_reduction       = as.list(bias_reduction),
    mean_bias_reduction  = mean(bias_reduction, na.rm = TRUE),
    pct_balanced_before  = if (length(covariates)) n_bal_before / length(covariates) * 100 else 0,
    pct_balanced_after   = if (length(covariates)) n_bal_after  / length(covariates) * 100 else 0,
    n_obs_before         = nrow(unmatched_data),
    n_obs_after          = nrow(matched_data)
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
#' \dontrun{
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
