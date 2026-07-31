# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native matching engine (feat/native-specializations, module 1).
# Greedy nearest-neighbour matching on the logit propensity score
# (Rosenbaum & Rubin 1985; caliper per Cochran & Rubin 1973), with the
# propensity model fit by base stats::glm — no MatchIt at runtime.
#
# Greedy PS matching is a ONE-DIMENSIONAL nearest-neighbour problem, so
# the engine is a sorted logit vector + findInterval() binary search +
# outward expansion over an availability mask: O(n log n), no distance
# matrix. Cross-validated against MatchIt in tests/cross/.

# Fit the propensity model and return logit scores (glm distance).
#' Internal helper: native propensity logits
#' @noRd
.morie_match_ps_logit <- function(df, treatment, covariates) {
  f <- stats::as.formula(paste(
    treatment, "~", paste(covariates, collapse = " + ")
  ))
  fit <- stats::glm(f, data = df, family = stats::binomial())
  p <- stats::fitted(fit)
  eps <- 1e-12
  p <- pmin(pmax(p, eps), 1 - eps)
  stats::qlogis(p)
}

# Greedy 1-D nearest-neighbour assignment.
# treated_val / control_val: logit scores. Returns an integer matrix
# n_treated x ratio of CONTROL indices (into control_val), NA where no
# admissible match. Treated processed in decreasing propensity order
# (MatchIt's m.order = "largest").
#' Internal helper: greedy sorted-vector matcher
#' @noRd
.morie_match_greedy_1d <- function(treated_val, control_val,
                                   ratio = 1L, caliper_width = Inf,
                                   replace = FALSE) {
  # C++ kernel (src/morie_matching_native.cpp); returns 1-based control
  # indices into control_val, NA where no admissible match.
  .morie_match_greedy_1d_cpp(as.numeric(treated_val),
                             as.numeric(control_val),
                             as.integer(ratio),
                             as.numeric(caliper_width),
                             isTRUE(replace))
}

# Native replacement for the MatchIt-backed nearest-neighbour wrapper.
# Same arguments, same morie_match_result shape.
#' Internal helper: native nearest-neighbour matching engine
#' @srrstats {G1.0} Primary references: Rosenbaum & Rubin (1985, Am.
#'   Stat. 39(1)) for greedy propensity matching; Cochran & Rubin
#'   (1973) for the caliper in SD-of-logit units.
#' @srrstats {G3.0} No exact floating-point equality: fitted
#'   propensities are clamped to \eqn{[\epsilon, 1-\epsilon]} before qlogis, and
#'   caliper admissibility uses explicit widths, never ==.
#' @noRd
.morie_match_nearest_native <- function(data, treatment, covariates,
                                        n_neighbors = 1L,
                                        caliper = NULL,
                                        replace = FALSE,
                                        alpha = 0.05) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  lp <- .morie_match_ps_logit(df, treatment, covariates)
  tr <- df[[treatment]] == 1
  idx_t <- which(tr)
  idx_c <- which(!tr)
  if (length(idx_c) < length(idx_t)) {
    # same wording the MatchIt-backed path used: the cardinality sweep
    # and the doubly-robust bootstrap collapse these into one summary
    warning("Fewer control units than treated units; not all treated ",
            "units will get a match.", call. = FALSE)
  }
  caliper_width <- if (is.null(caliper)) Inf else caliper * stats::sd(lp)
  mm <- .morie_match_greedy_1d(
    lp[idx_t], lp[idx_c],
    ratio = as.integer(n_neighbors),
    caliper_width = caliper_width,
    replace = replace
  )
  rn <- rownames(df)
  if (is.null(rn)) rn <- as.character(seq_len(nrow(df)))
  # vectorized pair assembly: expand the nt x ratio match matrix
  ti_rep <- rep(seq_len(nrow(mm)), times = ncol(mm))
  ci_all <- as.vector(mm)
  ok <- !is.na(ci_all)
  ti_ok <- ti_rep[ok]
  ci_ok <- ci_all[ok]
  matched_t <- idx_t[unique(ti_ok)]
  matched_c <- idx_c[ci_ok]
  pairs_df <- if (length(ti_ok)) data.frame(
    treated_idx = rn[idx_t[ti_ok]],
    control_idx = rn[idx_c[ci_ok]],
    distance    = abs(lp[idx_t[ti_ok]] - lp[idx_c[ci_ok]]),
    stringsAsFactors = FALSE
  ) else .morie_matching_empty_pairs()
  keep <- sort(unique(c(matched_t, matched_c)))
  md <- df[keep, , drop = FALSE]
  .morie_matching_result(
    matched_data      = md,
    n_treated         = length(unique(matched_t)),
    n_matched_control = length(unique(matched_c)),
    match_pairs       = pairs_df,
    method            = "nearest_neighbor (rmorie native)",
    details           = list(
      engine      = "native-greedy-1d",
      caliper     = caliper,
      replace     = replace,
      n_neighbors = n_neighbors,
      alpha       = alpha,
      propensity_logit_sd = stats::sd(lp)
    )
  )
}
