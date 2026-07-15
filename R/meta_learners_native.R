# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native X- and DR-learners (feat/native-specializations, module 12).
# They join the T/S-learners already native in morie_estimate_cate(),
# reusing the module-10 cross-fit nuisance engines and the module-11
# forest kernel — no EconML-style dependency anywhere.

#' Internal helper: X-learner CATE
#' @srrstats {G1.0} Kuenzel, Sekhon, Bickel & Yu (2019, PNAS 116(10)):
#'   two-stage X-learner with propensity-weighted combination.
#' @noRd
.morie_cate_x_learner <- function(X, y, d, n_folds = 5L,
                                  random_state = 42L) {
  i1 <- which(d == 1); i0 <- which(d == 0)
  if (length(i1) < 2L || length(i0) < 2L)
    stop("x_learner needs both treatment arms", call. = FALSE)
  # stage 1: arm-wise outcome models (GCV ridge, train on arm,
  # predict everywhere)
  mu1 <- .morie_dml_ridge_predict(X[i1, , drop = FALSE], y[i1], X)
  mu0 <- .morie_dml_ridge_predict(X[i0, , drop = FALSE], y[i0], X)
  # stage 2: imputed individual effects, regressed within each arm
  d1 <- y[i1] - mu0[i1]
  d0 <- mu1[i0] - y[i0]
  tau1 <- .morie_dml_ridge_predict(X[i1, , drop = FALSE], d1, X)
  tau0 <- .morie_dml_ridge_predict(X[i0, , drop = FALSE], d0, X)
  # combine with the propensity as the weight (Kuenzel et al. eq. 9)
  g <- .morie_dml_xfit_logit(X, d, n_folds, random_state)
  g * tau0 + (1 - g) * tau1
}

#' Internal helper: DR-learner CATE
#' @srrstats {G1.0} Kennedy (2023, EJS 17(2), "Towards optimal doubly
#'   robust estimation of heterogeneous causal effects"): regress the
#'   cross-fit AIPW pseudo-outcome on covariates.
#' @noRd
.morie_cate_dr_learner <- function(X, y, d, n_folds = 5L,
                                   n_trees = 300L,
                                   random_state = 42L) {
  if (length(unique(d)) < 2L)
    stop("dr_learner needs both treatment arms", call. = FALSE)
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = nrow(X)))
  mu1 <- numeric(nrow(X)); mu0 <- numeric(nrow(X))
  ps <- .morie_dml_xfit_logit(X, d, n_folds, random_state)
  for (k in seq_len(n_folds)) {
    te <- which(folds == k); tr <- setdiff(seq_len(nrow(X)), te)
    tr1 <- tr[d[tr] == 1]; tr0 <- tr[d[tr] == 0]
    mu1[te] <- if (length(tr1) >= ncol(X) + 2L)
      .morie_dml_ridge_predict(X[tr1, , drop = FALSE], y[tr1],
                               X[te, , drop = FALSE]) else mean(y[tr1])
    mu0[te] <- if (length(tr0) >= ncol(X) + 2L)
      .morie_dml_ridge_predict(X[tr0, , drop = FALSE], y[tr0],
                               X[te, , drop = FALSE]) else mean(y[tr0])
  }
  psi <- (mu1 - mu0) + d * (y - mu1) / ps - (1 - d) * (y - mu0) / (1 - ps)
  # second-stage regression of the pseudo-outcome: the module-11
  # forest (weights 1) captures non-linear heterogeneity
  fit <- .morie_rlearner_forest_cpp(X, psi, rep(1, nrow(X)), X,
                                    as.integer(n_trees), 8L, 10L, 0.5,
                                    as.integer(random_state))
  fit[, 1L]
}
