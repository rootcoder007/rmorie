# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bounds on the causal odds ratio for a binary outcome
#'
#' The odds ratio is strictly increasing in the treated risk and strictly
#' decreasing in the control risk, so the extreme odds ratios are attained
#' at the corners of the two risk intervals: no search is needed and the
#' bound is exact rather than conservative. The risks are the worst-case
#' bounds on \code{E[y(t)]}, computed within stratum and averaged.
#'
#' Formula: \code{OR = odds(p_1) / odds(p_0)}, so
#' \code{OR_low = odds(p_1^L) / odds(p_0^U)} and
#' \code{OR_high = odds(p_1^U) / odds(p_0^L)}.
#'
#' @param y Binary outcome, coded 0/1.
#' @param D Binary treatment indicator, coded 0/1.
#' @param X Discrete stratum label, one per unit.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{p1_lower}, \code{p1_upper}, \code{p0_lower},
#'   \code{p0_upper}, \code{n_strata}, \code{n}.
#' @references Robins, J. M. (2002) is the stub's attribution. The risk
#'   bounds used are Manski's worst case, equation (2.11) of Molinari, F.
#'   (2021), Handbook of Econometrics 7A (arXiv:2004.11751 p. 17); the
#'   corner argument is written out here rather than copied, because the
#'   attributed source could not be obtained.
#' @export
Bndlgt <- function(y, D, X) {
  z <- .bnd_yd(y, D, "Bndlgt")
  if (any(!(z$y %in% c(0, 1)))) stop("Bndlgt: y must be coded 0/1")
  xv <- unlist(X)
  n <- length(z$y)
  if (length(xv) != n) stop("Bndlgt: X must have one value per unit")
  p1lo <- 0
  p1hi <- 0
  p0lo <- 0
  p0hi <- 0
  grp <- unique(xv)
  for (g in grp) {
    sel <- xv == g
    cm <- .bnd_cellmeans(z$y[sel], z$d[sel])
    a1 <- .bnd_wc_arm(cm$m1, cm$p1, 0, 1)
    a0 <- .bnd_wc_arm(cm$m0, cm$p0, 0, 1)
    w <- sum(sel) / n
    p1lo <- p1lo + w * a1[1]
    p1hi <- p1hi + w * a1[2]
    p0lo <- p0lo + w * a0[1]
    p0hi <- p0hi + w * a0[2]
  }
  odds <- function(p) if (p <= 0) 0 else if (p >= 1) Inf else p / (1 - p)
  lo <- if (odds(p0hi) > 0) odds(p1lo) / odds(p0hi) else 0
  hi <- if (odds(p0lo) > 0) odds(p1hi) / odds(p0lo) else Inf
  est <- if (lo > 0 && is.finite(hi)) sqrt(lo * hi) else NaN
  .t1_result(lower = lo, upper = hi, width = hi - lo, estimate = est,
             p1_lower = p1lo, p1_upper = p1hi,
             p0_lower = p0lo, p0_upper = p0hi,
             n_strata = length(grp), n = n,
             method = "Logistic odds-ratio bound")
}
