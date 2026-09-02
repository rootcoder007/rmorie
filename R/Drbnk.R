# SPDX-License-Identifier: AGPL-3.0-or-later
#' Adaptively-weighted AIPW for data collected by a bandit
#'
#' When the assignment probabilities are chosen adaptively the ordinary
#' AIPW average need not be asymptotically normal.  The paper fixes the
#' denominator by construction with the stick-breaking recursion
#' \code{h_t^2 / e_t = (1 - sum_{s < t} h_s^2 / e_s) lambda_t}, so that
#' \code{sum_t h_t^2 / e_t = 1} exactly at every sample size.  The
#' allocation rate used here is \code{lambda_t = 1/(T - t + 1)}, the
#' smallest rate the paper's Theorem 3 permits.  The score is the
#' standard AIPW contrast and the estimator is
#' \code{sum h_t psi_t / sum h_t}; a constant assignment probability
#' makes every \code{h_t} equal and the estimator collapses to the
#' unweighted AIPW mean.
#'
#' @param y Outcome, one entry per time step, in assignment order.
#' @param D_t Binary assignment actually made at each step.
#' @param X Optional covariates for the per-arm outcome regressions.
#' @param pi_t Known assignment probability at each step, strictly
#'   inside (0, 1); \code{NULL} uses the realised treated share.
#' @return List with \code{estimate}, \code{se}, \code{aipw_unweighted},
#'   \code{h}, \code{sum_h2_over_e}, \code{n_treat}, \code{n}.
#' @references Hadad, V., Hirshberg, D. A., Zhan, R., Wager, S. and
#'   Athey, S. (2021). Confidence intervals for policy evaluation in
#'   adaptive experiments. PNAS 118(15), e2014602118; arXiv:1911.02768
#'   equation (12).
#' @export
#' @examples
#' set.seed(1)
#' r <- Drbnk(y = rnorm(10), D_t = rnorm(10)); TRUE
Drbnk <- function(y, D_t, X = NULL, pi_t = NULL) {
  yv <- .s03vec(y); dv <- .s03vec(D_t); n <- length(yv)
  if (n == 0L) stop("Drbnk: empty input, y has no observations")
  if (length(dv) != n) stop("Drbnk: y and D_t must have the same length")
  s <- sum(dv)
  if (s <= 0 || s >= n) stop("Drbnk: D_t must contain both arms")
  e <- if (is.null(pi_t)) rep(s / n, n) else .s03vec(pi_t)
  if (length(e) != n) stop("Drbnk: pi_t must have the same length as y")
  if (any(e <= 0 | e >= 1))
    stop("Drbnk: pi_t must lie strictly inside (0, 1)")
  Z <- .s03design(X, n)
  i1 <- which(dv >= 0.5); i0 <- which(dv < 0.5)
  b1 <- .s03lstsq(Z[i1, , drop = FALSE], yv[i1])
  b0 <- .s03lstsq(Z[i0, , drop = FALSE], yv[i0])
  m1 <- .s03matvec(Z, b1); m0 <- .s03matvec(Z, b0)
  psi <- m1 - m0 + dv * (yv - m1) / e - (1 - dv) * (yv - m0) / (1 - e)
  h <- numeric(n); acc <- 0
  for (i in seq_len(n)) {
    lam <- 1 / (n - i + 1)
    q <- (1 - acc) * lam
    if (q < 0) q <- 0
    acc <- acc + q
    h[i] <- sqrt(q * e[i])
  }
  sh <- sum(h)
  if (sh <= 0) stop("Drbnk: degenerate weights, every h_t is zero")
  est <- sum(h * psi) / sh
  v <- sum((h * (psi - est))^2)
  .t1_result(estimate = est, se = sqrt(v) / sh,
             aipw_unweighted = .s03mean(psi), h = h, sum_h2_over_e = acc,
             n_treat = s, n = n, method = "DR for adaptive bandit-DiD")
}
