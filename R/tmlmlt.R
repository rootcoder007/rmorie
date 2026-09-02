# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for a multi-arm treatment: arm means and pairwise contrasts
#'
#' With more than two arms a single clever covariate cannot solve every
#' arm's score, so the fluctuation is saturated in the arm: one
#' \code{eps_a} per arm with \code{H_a = I(A = a)/g_a(X)}.  That is what
#' makes every arm mean, not just one contrast, a valid plug-in.
#'
#' The multinomial is built from one-versus-rest logistics normalised to
#' sum to one across the arm set -- a working model, chosen because it is
#' deterministic and mirrors exactly across the language arms; the
#' targeting step repairs its bias as long as it or the outcome model is
#' right.  \code{psi_a = mean_i \[Q(a, X_i) + eps_a/g_a(X_i)\]}.
#'
#' @param y Outcome.
#' @param D Arm label of each subject.
#' @param X Covariates.
#' @param arm_set The distinct arm labels; the first is the reference.
#' @return List with \code{estimate} (last arm minus first), \code{se},
#'   \code{psi}, \code{contrasts}, \code{n_arms}, \code{n}.
#' @references Lendle, S. D. et al. (2017). Journal of Statistical
#'   Software 81(1).
#' @export
#' @examples
#' Tmlmlt(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), arm_set = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlmlt <- function(y, D, X, arm_set) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  arms <- as.numeric(arm_set)
  n <- length(yv)
  k <- length(arms)
  if (n == 0L || length(Dv) != n)
    stop("Tmlmlt: y and D must share one length")
  if (k < 2L) stop("Tmlmlt: need at least two arms")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlmlt: X must have one row per subject")
  for (i in seq_len(n))
    if (!any(abs(Dv[i] - arms) < 1e-9))
      stop("Tmlmlt: an arm label is not in arm_set")
  W <- cbind(1, Xm)
  ind <- matrix(0, n, k)
  for (j in seq_len(k)) ind[, j] <- ifelse(abs(Dv - arms[j]) < 1e-9, 1, 0)
  raw <- matrix(0, n, k)
  for (j in seq_len(k)) {
    b <- .s4_glmbin(W, ind[, j])
    raw[, j] <- .s4_clip(.s4_expit(as.numeric(W %*% b)), 0.01, 0.99)
  }
  g <- matrix(0, n, k)
  for (i in seq_len(n)) g[i, ] <- .s4_clip(raw[i, ] / sum(raw[i, ]), 0.01, 0.99)
  des <- cbind(W, ind[, -1L, drop = FALSE])
  qb <- .s4_ols(des, yv)$beta
  Qobs <- as.numeric(des %*% qb)
  psi <- numeric(k)
  ics <- matrix(0, n, k)
  for (j in seq_len(k)) {
    dj <- matrix(0, n, k - 1L)
    if (j > 1L) dj[, j - 1L] <- 1
    Qj <- as.numeric(cbind(W, dj) %*% qb)
    H <- ind[, j] / g[, j]
    den <- sum(H * H)
    e <- if (den != 0) sum(H * (yv - Qobs)) / den else 0
    Qs <- Qj + e / g[, j]
    psi[j] <- sum(Qs) / n
    ics[, j] <- H * (yv - Qobs - e * H) + Qs - psi[j]
  }
  contrasts <- psi - psi[1L]
  ic <- ics[, k] - ics[, 1L]
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = contrasts[k], se = se, psi = psi,
             contrasts = contrasts, n_arms = k, n = n,
             method = "TMLE for a multi-arm treatment with pairwise contrasts")
}
