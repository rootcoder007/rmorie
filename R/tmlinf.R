# SPDX-License-Identifier: AGPL-3.0-or-later
#' Standard error, interval and score check from an influence curve
#'
#' An efficient influence curve must have empirical mean ZERO at the
#' targeted estimate; \code{score_solved} reports whether it does, because
#' if it does not the standard error is not trustworthy.
#'
#' Formula: se = sqrt(var(IC)/n); CI = psi -+ z_\{alpha/2\} se;
#'   the estimating equation is (1/n) sum IC_i = 0
#'
#' @param psi The targeted point estimate.
#' @param ic Influence-curve values, one per observation.
#' @param level Confidence level.
#' @param null_value Value tested against.
#' @return List with \code{estimate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{statistic}, \code{p_value}, \code{ic_mean},
#'   \code{score_solved}, \code{n}.
#' @references Verified against the CRAN package tmle 2.1.1 (Gruber & van
#'   der Laan), whose calcParameters computes var.psi <- var(IC)/n, the
#'   qnorm-based interval and pvalue <- 2*pnorm(-abs(psi/sqrt(var.psi))).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Tmleinf(V, V)
Tmleinf <- function(psi, ic, level = 0.95, null_value = 0) {
  ic <- .t1_vec(ic)
  n <- length(ic)
  if (n < 2L) stop("at least two influence-curve values are required")
  if (level <= 0 || level >= 1)
    stop("level must lie strictly between 0 and 1")
  psi <- as.numeric(psi)
  m <- mean(ic)
  sd <- stats::sd(ic)
  se <- sqrt(stats::var(ic) / n)
  z <- stats::qnorm((1 + level) / 2)
  st <- if (se > 0) (psi - null_value) / se else Inf
  .t1_result(estimate = psi, se = se, ci_lower = psi - z * se,
             ci_upper = psi + z * se, statistic = st,
             p_value = if (se > 0) 2 * stats::pnorm(abs(st), lower.tail = FALSE) else 0,
             ic_mean = m,
             score_solved = as.numeric(abs(m) < 1e-8 * ifelse(sd > 0, sd, 1)),
             n = as.numeric(n),
             method = "Influence-curve inference for a targeted estimate")
}
