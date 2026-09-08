# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native exact and coarsened-exact matching (feat/native-specializations,
# modules 3-4). Iacus, King & Porro (2012): coarsen covariates, exact-
# match on the coarsened strata, keep strata containing both arms, and
# weight controls by the stratum-wise treated/control ratio scaled by
# the global ratio. Pure base R -- no MatchIt/cem at runtime.

# Stratum keys from a set of (already discrete) columns.
#' Internal helper: stratum key
#' @noRd
.morie_match_stratum_key <- function(df, vars) {
  do.call(paste, c(lapply(df[, vars, drop = FALSE], as.character),
                   sep = "\r"))
}

# Exact matching core: retain strata with both arms; CEM weights.
# Returns list(md, l1) where md carries weights + subclass columns.
#' Internal helper: exact-match core with CEM weights
#' @noRd
.morie_match_exact_core <- function(df, treatment, key) {
  tr <- df[[treatment]] == 1
  tab_t <- table(key[tr])
  tab_c <- table(key[!tr])
  common <- intersect(names(tab_t), names(tab_c))
  keep <- key %in% common
  md <- df[keep, , drop = FALSE]
  k <- key[keep]
  trk <- md[[treatment]] == 1
  n_t_tot <- sum(trk)
  n_c_tot <- sum(!trk)
  w <- numeric(nrow(md))
  w[trk] <- 1
  m_t <- tab_t[k[!trk]]
  m_c <- tab_c[k[!trk]]
  w[!trk] <- as.numeric(m_t) / as.numeric(m_c) * (n_c_tot / n_t_tot)
  md$weights <- w
  md$subclass <- factor(match(k, common))
  md
}

# Multivariate L1 imbalance (Iacus-King-Porro) on a stratification.
#' Internal helper: L1 imbalance statistic
#' @noRd
.morie_match_l1 <- function(df, treatment, key) {
  tr <- df[[treatment]] == 1
  f_t <- table(key[tr]) / sum(tr)
  f_c <- table(key[!tr]) / sum(!tr)
  cells <- union(names(f_t), names(f_c))
  p_t <- ifelse(cells %in% names(f_t), as.numeric(f_t[cells]), 0)
  p_c <- ifelse(cells %in% names(f_c), as.numeric(f_c[cells]), 0)
  sum(abs(p_t - p_c)) / 2
}

#' Internal helper: native exact matching engine
#' @noRd
.morie_match_exact_native <- function(data, treatment, exact_vars) {
  df <- .morie_matching_drop_na(data, c(treatment, exact_vars))
  key <- .morie_match_stratum_key(df, exact_vars)
  md <- .morie_match_exact_core(df, treatment, key)
  .morie_matching_result(
    matched_data      = md,
    n_treated         = sum(md[[treatment]] == 1),
    n_matched_control = sum(md[[treatment]] == 0),
    match_pairs       = .morie_matching_empty_pairs(),
    method            = "exact (rmorie native)",
    details           = list(
      engine     = "native-exact",
      exact_vars = exact_vars,
      n_strata   = nlevels(md$subclass),
      l1_before  = .morie_match_l1(df, treatment, key)
    )
  )
}

# Coarsen one covariate: numeric -> cut() with n_bins (Sturges when
# n_bins is NA), everything else kept as-is (already discrete).
#' Internal helper: CEM coarsening
#' @noRd
.morie_match_coarsen <- function(x, bins) {
  if (!is.numeric(x)) return(as.character(x))
  if (is.na(bins)) bins <- max(1L, grDevices::nclass.Sturges(x))
  # low-cardinality numerics (binary/ordinal codes) are already
  # discrete; quantile cutpoints would collapse them into one bin
  if (length(unique(x)) <= bins) return(as.character(x))
  brks <- unique(stats::quantile(x, probs = seq(0, 1, length.out = bins + 1),
                                 na.rm = TRUE, names = FALSE))
  if (length(brks) < 2L) return(rep("bin1", length(x)))
  as.character(cut(x, breaks = brks, include.lowest = TRUE))
}

#' Internal helper: native CEM engine
#' @srrstats {G1.0} Primary reference: Iacus, King & Porro (2012,
#'   Political Analysis 20(1)); stratum weights and the multivariate
#'   L1 imbalance statistic follow that paper.
#' @noRd
.morie_match_cem_native <- function(data, treatment, covariates,
                                    n_bins = 5L) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  coarse <- df[, covariates, drop = FALSE]
  for (v in covariates) {
    b <- if (is.list(n_bins)) {
      if (!is.null(n_bins[[v]])) as.integer(n_bins[[v]]) else NA_integer_
    } else as.integer(n_bins)
    coarse[[v]] <- .morie_match_coarsen(df[[v]], b)
  }
  key <- .morie_match_stratum_key(coarse, covariates)
  l1_before <- .morie_match_l1(df, treatment, key)
  md <- .morie_match_exact_core(df, treatment, key)
  .morie_matching_result(
    matched_data      = md,
    n_treated         = sum(md[[treatment]] == 1),
    n_matched_control = sum(md[[treatment]] == 0),
    match_pairs       = .morie_matching_empty_pairs(),
    method            = "cem (rmorie native)",
    details           = list(
      engine    = "native-cem",
      n_bins    = n_bins,
      n_strata  = nlevels(md$subclass),
      l1_before = l1_before
    )
  )
}
