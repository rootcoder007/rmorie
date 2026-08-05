# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for a vector-valued (multivariate) binary treatment
#'
#' With a treatment vector the propensity is the sequential
#' factorisation \code{g(a|X) = prod_j P(A_j = a_j | X, A_{<j} = a_{<j})},
#' fitted by one logistic per component with the preceding components as
#' regressors.  The counterfactual probability evaluates those same
#' fitted coefficients at the counterfactual history, not a refit --
#' refitting would condition on the observed history and target a
#' different parameter.
#'
#' Given \code{g} the machinery is the point-treatment one with the
#' indicator on the whole vector:
#' \code{H = I(A = 1)/g(1|X) - I(A = 0)/g(0|X)},
#' \code{eps = sum H (y - Q)/sum H^2},
#' \code{psi = mean[Q*(1, X) - Q*(0, X)]}.  With one component this is
#' exactly the binary point-treatment TMLE.
#'
#' @param y Outcome.
#' @param A Binary treatment vector, one column per component.
#' @param X Covariates.
#' @return List with \code{estimate}, \code{se}, \code{eps}, \code{q},
#'   \code{n}.
#' @references Lendle, S. D. et al. (2017). Journal of Statistical
#'   Software 81(1); Robins, J. M. (1986). Mathematical Modelling
#'   7:1393-1512.
#' @export
Tmlmct <- function(y, A, X) {
  yv <- as.numeric(y); n <- length(yv)
  Am <- as.matrix(A); Xm <- as.matrix(X)
  if (n == 0L || nrow(Am) != n || nrow(Xm) != n)
    stop("Tmlmct: y, A and X must share n rows")
  q <- ncol(Am)
  Wd <- cbind(1, Xm)
  betas <- vector("list", q)
  for (j in seq_len(q)) {
    des <- if (j > 1L) cbind(Wd, Am[, seq_len(j - 1L), drop = FALSE]) else Wd
    betas[[j]] <- .s4_glmbin(des, Am[, j])
  }
  gprob <- function(a) {
    p <- rep(1, n)
    for (j in seq_len(q)) {
      row <- if (j > 1L) cbind(Wd, matrix(a, n, j - 1L)) else Wd
      pj <- .s4_clip(.s4_expit(as.numeric(row %*% betas[[j]])), 1e-6, 1 - 1e-6)
      p <- p * (if (a > 0.5) pj else 1 - pj)
    }
    .s4_clip(p, 0.025, 0.975)
  }
  g1 <- gprob(1); g0 <- gprob(0)
  des <- cbind(Am, Wd)
  qb <- .s4_ols(des, yv)$beta
  Q1 <- as.numeric(cbind(matrix(1, n, q), Wd) %*% qb)
  Q0 <- as.numeric(cbind(matrix(0, n, q), Wd) %*% qb)
  Qobs <- as.numeric(des %*% qb)
  all1 <- ifelse(apply(Am > 0.5, 1L, all), 1, 0)
  all0 <- ifelse(apply(Am < 0.5, 1L, all), 1, 0)
  H <- all1 / g1 - all0 / g0
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Qobs)) / den else 0
  Q1s <- Q1 + eps / g1
  Q0s <- Q0 - eps / g0
  psi <- sum(Q1s - Q0s) / n
  ic <- H * (yv - Qobs - eps * H) + Q1s - Q0s - psi
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps, q = q, n = n,
             method = "TMLE for a vector-valued binary treatment")
}
