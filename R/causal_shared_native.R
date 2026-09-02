# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Internal helpers shared by the causal-forest and TMLE tiers
# (R/causal_forest_native.R and R/tmle_native.R). Kept in their own
# file so neither tier depends on the other's load order.

# Moore-Penrose pseudo-inverse via SVD, for singular Hessians.

# Newton-Raphson logistic regression; returns fitted probabilities.
#' Newton-Raphson logistic regression; returns fitted probabilities
#'
#' A step of the causal_shared_native implementation. Called by \code{morie_dr_learner}, \code{morie_regime_value}, \code{morie_tmle_ate} and 3 others in the module.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param X Passed to \code{cbind}.
#' @param y Numeric; combined arithmetically in the body.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-09}.
#' @return The value of \code{as.vector}.
#' @export
.morie_logit_fit <- function(X, y, max_iter = 100L, tol = 1e-9) {
  D <- cbind(1, X)
  beta <- rep(0, ncol(D))
  converged <- FALSE
  step <- rep(Inf, ncol(D))
  for (i in seq_len(max_iter)) {
    eta <- pmin(pmax(D %*% beta, -35), 35)
    p <- as.vector(1 / (1 + exp(-eta)))
    W <- pmax(p * (1 - p), 1e-10)
    grad <- crossprod(D, y - p)
    H <- crossprod(D * W, D)
    step <- tryCatch(solve(H, grad),
      error = function(e) .morie_ginv(H) %*% grad
    )
    beta <- beta + step
    if (max(abs(step)) < tol) {
      converged <- TRUE
      break
    }
  }
  # Without this the loop simply runs out of iterations and returns the
  # last iterate as though it had converged, so a separating or
  # near-collinear design yields propensity scores that downstream IPW /
  # matching / DML weight as if they were fitted (RE3.0).
  if (!converged) {
    warning(sprintf(
      paste0(
        "logistic fit did not converge in %d iterations ",
        "(last step %.3g > tol %.3g); estimates that use ",
        "these fitted values are unreliable."
      ),
      max_iter, max(abs(step)), tol
    ), call. = FALSE)
  }
  as.vector(1 / (1 + exp(-pmin(pmax(D %*% beta, -35), 35))))
}

# Ridge coefficients with an unpenalised intercept.
#' Ridge coefficients with an unpenalised intercept
#'
#' A step of the causal_shared_native implementation. Called by \code{morie_dr_learner}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param X Passed to \code{cbind}.
#' @param y A matrix; passed to \code{crossprod}.
#' @param lam Numeric; combined arithmetically in the body. Defaults to \code{0.001}.
#' @return A matrix, from \code{solve}.
#' @export
.morie_ridge_fit <- function(X, y, lam = 1e-3) {
  D <- cbind(1, X)
  A <- crossprod(D) + lam * diag(ncol(D))
  A[1, 1] <- A[1, 1] - lam
  solve(A, crossprod(D, y))
}

# Inverse-probability-of-censoring-weighted RMST pseudo outcome, whose
# expectation is E[min(T, horizon)] under independent censoring.
#' Inverse-probability-of-censoring-weighted RMST pseudo outcome, whose
#'
#' expectation is E\[min(T, horizon)\] under independent censoring.
#'
#' @param time A vector; its length is taken and its elements indexed.
#' @param event A vector; indexed elementwise.
#' @param horizon Numeric; combined arithmetically in the body.
#' @return The value of \code{ifelse}.
#' @export
.morie_cf_rmst_pseudo <- function(time, event, horizon) {
  n <- length(time)
  o <- order(time)
  ts <- time[o]
  cs <- 1 - event[o]
  G <- 1
  at_risk <- n
  grid <- 0
  vals <- 1
  for (i in seq_len(n)) {
    if (cs[i] == 1) {
      G <- G * (1 - 1 / max(at_risk, 1))
      grid <- c(grid, ts[i])
      vals <- c(vals, G)
    }
    at_risk <- at_risk - 1
  }
  vals <- pmax(vals, 1e-3)
  Ghat <- function(t) vals[findInterval(t, grid)]
  ifelse(
    event == 1 & time <= horizon, time / Ghat(pmax(time - 1e-12, 0)),
    ifelse(time > horizon, horizon / Ghat(horizon), 0)
  )
}
