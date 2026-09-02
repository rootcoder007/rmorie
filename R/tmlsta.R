# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stabilize a targeted estimate by truncating the propensity score
#'
#' Truncation caps the clever-covariate weight at the price of bias; the
#' whole trade-off curve is returned rather than one point.
#' \code{max_weight} far above sqrt(n) means the estimate rests on a
#' handful of observations.
#'
#' Formula: g_trunc = min(max(g, delta), 1 - delta); re-target at each
#'   delta and report psi, se and max weight
#'
#' @param Y,A Outcome in \[0, 1\] and binary treatment.
#' @param QAW,Q1W,Q0W Initial outcome predictions.
#' @param g1W Initial propensity.
#' @param gbounds Truncation levels to try.
#' @return List with \code{gbounds}, \code{estimate}, \code{se},
#'   \code{max_weight}, \code{n_truncated}, \code{spread}, \code{n}.
#' @references Verified against the CRAN package tmle 2.1.1 (Gruber & van
#'   der Laan), which bounds the propensity before forming the clever
#'   covariates. The stabilizing role of that bound is the subject of
#'   Gruber & van der Laan (2010), International Journal of Biostatistics
#'   6(1), Article 26, which this row cites.
#' @export
#' @examples
#' Tmlestab(Y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), QAW = c(1, 2, 3, 4, 5, 6, 7, 8), Q1W = c(1, 2, 3, 4, 5, 6, 7, 8), Q0W = c(1, 2, 3, 4, 5, 6, 7, 8), g1W = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlestab <- function(Y, A, QAW, Q1W, Q0W, g1W, gbounds = NULL) {
  Y <- .t1_vec(Y)
  A <- .t1_vec(A)
  n <- length(Y)
  QAW <- .t1_vec(QAW)
  Q1W <- .t1_vec(Q1W)
  Q0W <- .t1_vec(Q0W)
  g1W <- .t1_vec(g1W)
  if (any(c(length(A), length(QAW), length(Q1W), length(Q0W),
            length(g1W)) != n))
    stop("every argument must have one entry per observation")
  if (n < 2L) stop("at least two observations are required")
  gb <- if (is.null(gbounds)) c(0.001, 0.01, 0.025, 0.05, 0.10) else
        .t1_vec(gbounds)
  if (any(gb <= 0 | gb >= 0.5))
    stop("each bound must lie strictly between 0 and 0.5")
  K <- length(gb)
  est <- ses <- mw <- nt <- numeric(K)
  for (t in seq_len(K)) {
    fit <- .b1_target(Y, A, QAW, Q1W, Q0W, g1W, gb[t])
    cv <- .b1_curves(Y, A, fit)
    ic <- cv$ic1 - cv$ic0
    est[t] <- cv$mu1 - cv$mu0
    ses[t] <- sqrt(stats::var(ic) / n)
    mw[t] <- max(max(fit$H1), max(fit$H0))
    nt[t] <- sum(g1W < gb[t] | g1W > 1 - gb[t])
  }
  .t1_result(gbounds = gb, estimate = est, se = ses, max_weight = mw,
             n_truncated = nt, spread = max(est) - min(est),
             n = as.numeric(n),
             method = "Propensity truncation trade-off for a targeted estimate")
}
