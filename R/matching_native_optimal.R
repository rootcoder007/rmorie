# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native optimal pair matching (feat/native-specializations, module 5).
# distance = "propensity": exact optimal 1:1 matching on the logit
# propensity via the non-crossing dynamic program (1-D optimal matching
# is monotone in the sorted scores) -- globally minimal total distance,
# unlike greedy nearest-neighbour. distance = "mahalanobis": exact
# optimal assignment on Cholesky-whitened covariates via shortest
# augmenting paths. No MatchIt/optmatch at runtime.

#' Internal helper: native optimal pair matching engine
#' @srrstats {G1.0} Primary references: Rosenbaum (1989, JASA 84(408))
#'   for optimal matching as a network/assignment problem; Hansen &
#'   Klopfer (2006) for the reference implementation (optmatch) this
#'   engine is cross-validated against.
#' @srrstats {G3.0} All distance comparisons use explicit totals and
#'   duals; no exact floating-point equality tests.
#' @noRd
.morie_match_optimal_native <- function(data, treatment, covariates,
                                        distance = "propensity",
                                        ps = NULL) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  tr <- df[[treatment]] == 1
  idx_t <- which(tr)
  idx_c <- which(!tr)
  if (distance == "mahalanobis") {
    X <- as.matrix(df[, covariates, drop = FALSE])
    W <- .morie_match_whiten(X, X[idx_c, , drop = FALSE])
    mm <- .morie_match_optimal_assign_cpp(W[idx_t, , drop = FALSE],
                                          W[idx_c, , drop = FALSE])
    dvec <- sqrt(rowSums((W[idx_t, , drop = FALSE] -
                            W[idx_c[mm], , drop = FALSE])^2))
  } else {
    lp <- .morie_match_ps_logit(df, treatment, covariates)
    mm <- .morie_match_optimal_1d_cpp(lp[idx_t], lp[idx_c])
    dvec <- abs(lp[idx_t] - lp[idx_c[mm]])
  }
  rn <- rownames(df)
  if (is.null(rn)) rn <- as.character(seq_len(nrow(df)))
  ok <- !is.na(mm)
  pairs_df <- if (any(ok)) data.frame(
    treated_idx = rn[idx_t[ok]],
    control_idx = rn[idx_c[mm[ok]]],
    distance    = dvec[ok],
    stringsAsFactors = FALSE
  ) else .morie_matching_empty_pairs()
  keep <- sort(unique(c(idx_t[ok], idx_c[mm[ok]])))
  .morie_matching_result(
    matched_data      = df[keep, , drop = FALSE],
    n_treated         = sum(ok),
    n_matched_control = length(unique(idx_c[mm[ok]])),
    match_pairs       = pairs_df,
    method            = "optimal_pair (rmorie native)",
    details           = list(
      engine         = if (distance == "mahalanobis")
        "native-optimal-assignment" else "native-optimal-1d-dp",
      distance       = distance,
      total_distance = sum(dvec[ok])
    )
  )
}
