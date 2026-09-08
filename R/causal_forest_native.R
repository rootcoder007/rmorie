# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native causal forest (feat/native-specializations, module 11).
# R-learner decomposition (Nie & Wager 2021): cross-fit m(x) = E[Y|X]
# and e(x) = E[W|X] with the module-10 nuisance engines, then fit a
# weighted subsampled regression forest (C++ kernel) on the pseudo-
# outcome (Y - m)/(W - e) with weights (W - e)^2. ATE via the AIPW
# orthogonal score with tau(x) as the heterogeneous nuisance --
# the same estimand grf::average_treatment_effect(method = "AIPW")
# targets. No grf at runtime.

#' Internal helper: native R-learner causal forest
#' @srrstats {G1.0} Primary references: Nie & Wager (2021, Biometrika
#'   108(2)) for the R-learner; Athey, Tibshirani & Wager (2019, Ann.
#'   Statist. 47(2)) for the causal-forest estimand; grf is the
#'   reference implementation cross-validated against in tests/cross/.
#' @srrstats {G3.1} Nuisance construction (cross-fit ridge/logistic,
#'   propensity clipping) and forest hyperparameters are documented
#'   here and surfaced in details.
#' @noRd
.morie_causal_forest_native <- function(X, y, w, n_trees = 500L,
                                        max_depth = 8L, min_node = 10L,
                                        subsample = 0.5,
                                        n_folds = 5L,
                                        random_state = 42L) {
  if (length(unique(w)) < 2L || nrow(X) < 2L * n_folds) {
    stop("morie_estimate_dr_forest: needs both treatment arms and at ",
      "least ", 2L * n_folds, " rows",
      call. = FALSE
    )
  }
  ml_y <- .morie_dml_xfit_ridge_gcv(X, y, n_folds, random_state)
  ps <- .morie_dml_xfit_logit(X, w, n_folds, random_state + 1L)
  u <- y - ml_y
  v <- w - ps
  vv <- pmax(v^2, 1e-8)
  pseudo <- u / ifelse(abs(v) < 1e-4, sign(v + 1e-12) * 1e-4, v)
  fw <- vv
  fit <- .morie_rlearner_forest_cpp(
    X, pseudo, fw, X,
    as.integer(n_trees),
    as.integer(max_depth),
    as.integer(min_node),
    subsample, as.integer(random_state)
  )
  tau <- fit[, 1L]
  mu1 <- ml_y + (1 - ps) * tau
  mu0 <- ml_y - ps * tau
  list(tau = tau, mu1 = mu1, mu0 = mu0, ps = ps, ml_y = ml_y)
}

#' Internal helper: AIPW ATE over a target sample from forest nuisances
#' @noRd
.morie_causal_forest_ate <- function(nf, y, w, target_sample = "all") {
  psi <- (nf$mu1 - nf$mu0) +
    w * (y - nf$mu1) / nf$ps -
    (1 - w) * (y - nf$mu0) / (1 - nf$ps)
  sel <- switch(target_sample,
    all      = rep(TRUE, length(y)),
    treated  = w == 1,
    control  = w == 0,
    overlap  = rep(TRUE, length(y))
  )
  wt <- if (target_sample == "overlap") {
    nf$ps * (1 - nf$ps)
  } else {
    as.numeric(sel)
  }
  ate <- sum(wt * psi) / sum(wt)
  se <- sqrt(sum(wt^2 * (psi - ate)^2)) / sum(wt)
  list(ate = ate, se = se)
}
