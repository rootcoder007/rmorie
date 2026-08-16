# SPDX-License-Identifier: AGPL-3.0-or-later
#' SPDX-License-Identifier: AGPL-3.0-or-later
#'
#' A step of the tmlphd implementation. Called by \code{.tmlphd_lasso}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @param t Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.tmlphd_soft <- function(z, t) if (z > t) z - t else if (z < -t) z + t else 0

## Cyclic coordinate descent for a weighted lasso; column 1 unpenalised.
## Fixed sweep count and fixed cyclic order, so both language arms take
## the same path even where the objective has a flat direction.
#' # Cyclic coordinate descent for a weighted lasso; column 1
#' unpenalised
#'
#' # Fixed sweep count and fixed cyclic order, so both language arms
#' take # the same path even where the objective has a flat direction.
#'
#' @param X A matrix; indexed by row and column.
#' @param y Numeric; combined arithmetically in the body.
#' @param lam Passed to \code{.tmlphd_soft}.
#' @param w Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param sweeps A count; the body uses it as \code{seq_len(...)}. Defaults to \code{400L}.
#' @return The value of \code{beta}, as built in the body.
#' @export
.tmlphd_lasso <- function(X, y, lam, w = NULL, sweeps = 400L) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  if (is.null(w)) w <- rep(1, n)
  beta <- numeric(p); fit <- numeric(n)
  denom <- as.numeric(colSums(w * X * X)) / n
  for (s in seq_len(sweeps)) {
    for (j in seq_len(p)) {
      if (denom[j] <= 0) next
      r <- sum(w * X[, j] * (y - fit + X[, j] * beta[j])) / n
      nb <- if (j == 1L) r / denom[j] else .tmlphd_soft(r, lam) / denom[j]
      if (nb != beta[j]) {
        fit <- fit + (nb - beta[j]) * X[, j]
        beta[j] <- nb
      }
    }
  }
  beta
}

## Proximal-Newton L1 logistic: IRLS outside, weighted lasso inside.
#' # Proximal-Newton L1 logistic: IRLS outside, weighted lasso inside
#'
#' A step of the tmlphd implementation. Called by \code{Tmlphd}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X A matrix; passed to \code{ncol}.
#' @param y Numeric; combined arithmetically in the body.
#' @param lam Passed to \code{.tmlphd_lasso}.
#' @param outer A count; the body uses it as \code{seq_len(...)}. Defaults to \code{15L}.
#' @param sweeps Passed to \code{.tmlphd_lasso}. Defaults to \code{60L}.
#' @return The value of \code{beta}, as built in the body.
#' @export
.tmlphd_lasso_logit <- function(X, y, lam, outer = 15L, sweeps = 60L) {
  X <- as.matrix(X); y <- as.numeric(y)
  beta <- numeric(ncol(X))
  for (it in seq_len(outer)) {
    eta <- as.numeric(X %*% beta)
    mu <- .s4_expit(eta)
    w <- .s4_clip(mu * (1 - mu), 1e-6, 0.25)
    z <- eta + (y - mu) / w
    beta <- .tmlphd_lasso(X, z, lam, w = w, sweeps = sweeps)
  }
  beta
}

#' High-dimensional TMLE with L1-penalised nuisance models
#'
#' When \code{p} is comparable with \code{n} both nuisance models must be
#' regularised, and regularisation is exactly what breaks a plain
#' plug-in: the penalty biases \code{Q} toward zero and that bias does
#' not vanish at root-n.  The targeting step removes it.  Selection is by
#' L1 on both models -- coordinate descent for \code{Q}, proximal-Newton
#' for \code{g} -- with the intercept unpenalised, followed by
#' \code{H = D/g - (1 - D)/(1 - g)},
#' \code{eps = sum H (y - Q)/sum H^2}.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates; \code{p} may exceed \code{n}.
#' @param lam Non-negative L1 penalty applied to both nuisance models;
#'   \code{lam = 0} reproduces the unpenalised TMLE.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{nz_q}, \code{nz_g}, \code{n}.
#' @references Belloni, A., Chernozhukov, V. & Hansen, C. (2014). Review
#'   of Economic Studies 81(2):608-650; van der Laan, M. J. & Rubin, D.
#'   (2006). IJB 2(1):11.
#' @export
Tmlphd <- function(y, D, X, lam) {
  yv <- as.numeric(y); Dv <- as.numeric(D); n <- length(yv); lam <- as.numeric(lam)
  if (n == 0L || length(Dv) != n)
    stop("Tmlphd: y and D must share one length")
  if (lam < 0) stop("Tmlphd: lam must be non-negative")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlphd: X must have one row per subject")
  W <- cbind(1, Xm)
  gb <- .tmlphd_lasso_logit(W, Dv, lam)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  des <- cbind(1, Dv, Xm)
  qb <- .tmlphd_lasso(des, yv, lam)
  Q1 <- as.numeric(cbind(1, 1, Xm) %*% qb)
  Q0 <- as.numeric(cbind(1, 0, Xm) %*% qb)
  Qobs <- as.numeric(des %*% qb)
  H <- Dv / g - (1 - Dv) / (1 - g)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Qobs)) / den else 0
  Q1s <- Q1 + eps / g
  Q0s <- Q0 - eps / (1 - g)
  psi <- sum(Q1s - Q0s) / n
  ic <- H * (yv - Qobs - eps * H) + Q1s - Q0s - psi
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps,
             nz_q = sum(qb[-(1:2)] != 0), nz_g = sum(gb[-1] != 0), n = n,
             method = "High-dimensional TMLE with L1-penalised nuisance models")
}
