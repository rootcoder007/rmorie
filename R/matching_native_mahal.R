# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native Mahalanobis matching engine (feat/native-specializations,
# module 2). Rubin (1980): covariance estimated on the CONTROL pool,
# distance computed after Cholesky whitening — Mahalanobis distance in
# the original space equals Euclidean distance in whitened space, so
# the greedy scan kernel needs no matrix solves per pair. Optional
# exact-matching strata and a caliper in Mahalanobis-distance units.

# Whiten a covariate matrix by the control-pool covariance:
# X %*% t(chol(solve(S))) via a triangular backsolve on chol(S).
#' Internal helper: control-pool whitening transform
#' @noRd
.morie_match_whiten <- function(X, X_control) {
  S <- stats::cov(X_control)
  # ridge for near-singular pools (constant or collinear covariates)
  d <- diag(S)
  diag(S) <- d + max(d, 1e-8) * 1e-8
  R <- chol(S)
  t(backsolve(R, t(X), transpose = TRUE))
}

#' Internal helper: native Mahalanobis matching engine
#' @srrstats {G1.0} Primary reference: Rubin (1980, Biometrics 36(2)),
#'   Mahalanobis-metric matching with covariance estimated on the
#'   control pool.
#' @srrstats {G3.1} The covariance estimator (control-pool `stats::cov`
#'   with a documented proportional diagonal ridge for near-singular
#'   pools) is stated in the docs and BRANCH_PLAN entry; whitening via
#'   Cholesky makes the metric explicit.
#' @noRd
.morie_match_mahalanobis_native <- function(data, treatment, covariates,
                                            n_neighbors = 1L,
                                            caliper = NULL,
                                            replace = FALSE,
                                            exact = NULL) {
  df <- .morie_matching_drop_na(data,
                                c(treatment, covariates, exact))
  X <- as.matrix(df[, covariates, drop = FALSE])
  storage.mode(X) <- "double"
  tr <- df[[treatment]] == 1
  idx_t <- which(tr)
  idx_c <- which(!tr)
  W <- .morie_match_whiten(X, X[idx_c, , drop = FALSE])
  cal <- if (is.null(caliper)) Inf else as.numeric(caliper)
  rn <- rownames(df)
  if (is.null(rn)) rn <- as.character(seq_len(nrow(df)))

  # exact strata: match only within identical combinations of `exact`
  strata <- if (is.null(exact)) rep("", nrow(df)) else
    do.call(paste, c(df[, exact, drop = FALSE], sep = "\r"))

  ti_all <- integer(0); ci_all <- integer(0); d_all <- numeric(0)
  for (s in unique(strata[idx_t])) {
    st <- idx_t[strata[idx_t] == s]
    sc <- idx_c[strata[idx_c] == s]
    if (length(st) == 0L || length(sc) == 0L) next
    mm <- .morie_match_greedy_kd_cpp(
      W[st, , drop = FALSE], W[sc, , drop = FALSE],
      as.integer(n_neighbors), cal, isTRUE(replace)
    )
    ti_rep <- rep(seq_len(nrow(mm)), times = ncol(mm))
    hits <- as.vector(mm)
    ok <- !is.na(hits)
    if (!any(ok)) next
    t_glob <- st[ti_rep[ok]]
    c_glob <- sc[hits[ok]]
    ti_all <- c(ti_all, t_glob)
    ci_all <- c(ci_all, c_glob)
    d_all <- c(d_all, sqrt(rowSums(
      (W[t_glob, , drop = FALSE] - W[c_glob, , drop = FALSE])^2
    )))
  }
  pairs_df <- if (length(ti_all)) data.frame(
    treated_idx = rn[ti_all],
    control_idx = rn[ci_all],
    distance    = d_all,
    stringsAsFactors = FALSE
  ) else .morie_matching_empty_pairs()
  keep <- sort(unique(c(ti_all, ci_all)))
  md <- df[keep, , drop = FALSE]
  .morie_matching_result(
    matched_data      = md,
    n_treated         = length(unique(ti_all)),
    n_matched_control = length(unique(ci_all)),
    match_pairs       = pairs_df,
    method            = "mahalanobis (rmorie native)",
    details           = list(
      engine      = "native-greedy-kd-whitened",
      caliper     = caliper,
      replace     = replace,
      exact_vars  = exact,
      n_neighbors = n_neighbors
    )
  )
}
