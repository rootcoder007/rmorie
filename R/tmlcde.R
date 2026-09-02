# SPDX-License-Identifier: AGPL-3.0-or-later
#' Controlled direct effect: the treatment effect with the mediator fixed.
#'
#' The clever covariate carries BOTH nuisance densities in its
#' denominator, so positivity is far more demanding than for a total
#' effect; \code{min_denominator} exists for that reason.
#'
#' Formula: H_a = 1\{A = a, M = m\} / (g_a(W) h_m(A, W));
#'   psi = E\[Q*(1, m, W)\] - E\[Q*(0, m, W)\]
#'
#' @param Y Outcome in \[0, 1\].
#' @param A Binary treatment.
#' @param M Mediator level of each observation.
#' @param QAM Initial E\[Y | A, M, W\] at the observed (A, M).
#' @param Q1m,Q0m Initial E\[Y | A = 1/0, M = m, W\].
#' @param g1W Initial P(A = 1 | W).
#' @param hmW Initial P(M = m | A, W).
#' @param m The level the mediator is set to.
#' @param gbound Truncation on both nuisance probabilities.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{mu1}, \code{mu0}, \code{epsilon},
#'   \code{min_denominator}, \code{max_weight}, \code{n}.
#' @references van der Laan & Petersen (2008), International Journal of
#'   Biostatistics 4(1), Article 23 -- the row's own citation. That paper
#'   was NOT obtainable, so the estimator is assembled from the components
#'   verified in the CRAN package tmle 2.1.1 (Gruber & van der Laan), with
#'   the clever covariate's denominator extended from g_a(W) to
#'   g_a(W) h_m(A, W) as the CDE identification requires.
#' @export
Tmlecde <- function(Y, A, M, QAM, Q1m, Q0m, g1W, hmW, m = 1,
                    gbound = 0.025, level = 0.95) {
  Y <- .t1_vec(Y); n <- length(Y)
  A <- .t1_vec(A); M <- .t1_vec(M); QAM <- .t1_vec(QAM)
  Q1m <- .t1_vec(Q1m); Q0m <- .t1_vec(Q0m)
  g1W <- .t1_vec(g1W); hmW <- .t1_vec(hmW)
  if (any(c(length(A), length(M), length(QAM), length(Q1m), length(Q0m),
            length(g1W), length(hmW)) != n))
    stop("every argument must have one entry per observation")
  if (any(!(A %in% c(0, 1)))) stop("A must be binary 0/1")
  if (any(Y < 0 | Y > 1)) stop("Y must lie in [0, 1]")
  if (n < 2L) stop("at least two observations are required")
  m <- as.numeric(m)
  g1 <- .b1_bound(g1W, gbound, 1 - gbound); g0 <- 1 - g1
  h <- .b1_bound(hmW, gbound, 1)
  at <- as.numeric(M == m)
  H1 <- at * A / (g1 * h)
  H0 <- at * (1 - A) / (g0 * h)
  off <- .b1_logit(QAM)
  e <- c(0, 0)
  for (t in seq_len(100L)) {
    mu <- .b1_expit(off + e[1] * H0 + e[2] * H1)
    r <- Y - mu; w <- mu * (1 - mu)
    gr <- c(sum(H0 * r), sum(H1 * r))
    Hm <- matrix(c(sum(H0^2 * w) + 1e-10, sum(H0 * H1 * w),
                   sum(H0 * H1 * w), sum(H1^2 * w) + 1e-10), 2, 2)
    st <- as.numeric(solve(Hm, gr))
    e <- e + st
    if (max(abs(st)) < 1e-12) break
  }
  QAs <- .b1_expit(off + e[1] * H0 + e[2] * H1)
  Q1s <- .b1_expit(.b1_logit(Q1m) + e[2] / (g1 * h))
  Q0s <- .b1_expit(.b1_logit(Q0m) + e[1] / (g0 * h))
  mu1 <- mean(Q1s); mu0 <- mean(Q0s)
  ic <- H1 * (Y - QAs) + Q1s - mu1 - (H0 * (Y - QAs) + Q0s - mu0)
  psi <- mu1 - mu0
  se <- sqrt(stats::var(ic) / n)
  z <- stats::qnorm((1 + level) / 2)
  .t1_result(estimate = psi, se = se, ci_lower = psi - z * se,
             ci_upper = psi + z * se, mu1 = mu1, mu0 = mu0, epsilon = e,
             min_denominator = min(pmin(g1, g0) * h),
             max_weight = max(max(H1), max(H0)), n = as.numeric(n),
             method = "TMLE controlled direct effect at a fixed mediator level")
}
