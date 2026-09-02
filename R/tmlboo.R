# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nonparametric bootstrap interval for the targeted risk difference.
#'
#' The INITIAL FITS are carried along as fixed columns, so this
#' bootstraps the targeting step and the empirical means, not the machine
#' learning that produced Q and g; it therefore understates uncertainty
#' when those fits are themselves adaptive. The influence-curve interval
#' is returned beside it.
#'
#' Formula: resample indices with replacement, re-target each replicate,
#'   take the empirical percentiles of psi*
#'
#' @param Y,A Outcome in \[0, 1\] and binary treatment.
#' @param QAW,Q1W,Q0W Initial outcome predictions.
#' @param g1W Initial propensity.
#' @param B Fixed number of bootstrap replicates.
#' @param seed Seed for the pinned generator.
#' @param gbound Propensity truncation level.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{boot_se}, \code{ci_lower},
#'   \code{ci_upper}, \code{ic_se}, \code{ic_lower}, \code{ic_upper},
#'   \code{boot_mean}, \code{B}, \code{n}.
#' @references Verified against the CRAN package tmle 2.1.1 (Gruber & van
#'   der Laan) for the targeting step and the influence-curve interval.
#'   The bootstrap is Efron (1979), Annals of Statistics 7(1), 1-26; van
#'   der Laan & Rose (2011), Targeted Learning, recommend the
#'   influence-curve interval as the default and the bootstrap as a check.
#' @export
#' @examples
#' Tmleboot(Y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), QAW = c(1, 2, 3, 4, 5, 6, 7, 8), Q1W = c(1, 2, 3, 4, 5, 6, 7, 8), Q0W = c(1, 2, 3, 4, 5, 6, 7, 8), g1W = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmleboot <- function(Y, A, QAW, Q1W, Q0W, g1W, B = 200, seed = 1,
                     gbound = 0.025, level = 0.95) {
  Y <- .t1_vec(Y); A <- .t1_vec(A); n <- length(Y)
  QAW <- .t1_vec(QAW); Q1W <- .t1_vec(Q1W); Q0W <- .t1_vec(Q0W)
  g1W <- .t1_vec(g1W)
  if (any(c(length(A), length(QAW), length(Q1W), length(Q0W),
            length(g1W)) != n))
    stop("every argument must have one entry per observation")
  B <- as.integer(B)
  if (B < 2L) stop("B must be at least 2")
  if (n < 2L) stop("at least two observations are required")
  fit <- .b1_target(Y, A, QAW, Q1W, Q0W, g1W, gbound)
  cv <- .b1_curves(Y, A, fit)
  psi <- cv$mu1 - cv$mu0
  ic <- cv$ic1 - cv$ic0
  icse <- sqrt(stats::var(ic) / n)
  z <- stats::qnorm((1 + level) / 2)
  g <- .t1_lcg(seed)
  reps <- numeric(0)
  for (b in seq_len(B)) {
    idx <- integer(n)
    for (i in seq_len(n)) {
      j <- as.integer(g$unif() * n)
      if (j >= n) j <- n - 1L
      idx[i] <- j + 1L
    }
    if (length(unique(A[idx])) < 2L) next
    f <- .b1_target(Y[idx], A[idx], QAW[idx], Q1W[idx], Q0W[idx],
                    g1W[idx], gbound)
    reps <- c(reps, mean(f$Q1star) - mean(f$Q0star))
  }
  if (length(reps) < 2L) stop("too few usable bootstrap replicates")
  q <- sort(reps); m <- length(q)
  a <- (1 - level) / 2
  lo <- q[max(1L, floor(a * (m - 1)) + 1L)]
  hi <- q[min(m, ceiling((1 - a) * (m - 1)) + 1L)]
  .t1_result(estimate = psi, boot_se = stats::sd(reps), ci_lower = lo,
             ci_upper = hi, ic_se = icse, ic_lower = psi - z * icse,
             ic_upper = psi + z * icse, boot_mean = mean(reps),
             B = as.numeric(m), n = as.numeric(n),
             method = "Bootstrap and influence-curve intervals for a TMLE")
}
