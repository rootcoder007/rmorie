# SPDX-License-Identifier: AGPL-3.0-or-later
#' Manski no-assumption ATE bounds computed within covariate strata
#'
#' Inside a stratum \code{X = x}:
#' \code{E\[Y(1)|x\] in \[m1 p1 + ymin (1-p1), m1 p1 + ymax (1-p1)\]} and
#' \code{E\[Y(0)|x\] in \[m0 p0 + ymin (1-p0), m0 p0 + ymax (1-p0)\]}, with
#' \code{p1 = P(D=1|x)}, \code{p0 = 1 - p1}, \code{m1 = E\[Y|D=1,x\]},
#' \code{m0 = E\[Y|D=0,x\]}. Differencing the opposite ends gives the
#' stratum ATE bound, whose width is exactly \code{ymax - ymin} however
#' the data fall -- the signature of the no-assumption bound, and the
#' reason it always contains zero.
#'
#' Two population summaries follow, answering different questions.
#' \code{lower}/\code{upper} average the stratum bounds with weights
#' \code{P(x)}: the bound on the population ATE, weakly tighter than
#' pooling first. \code{inter_lower}/\code{inter_upper} INTERSECT the
#' stratum bounds, which is valid only under the extra assumption of a
#' common effect across strata; it is reported separately and flagged
#' empty when the strata disagree.
#'
#' @param y Observed outcomes, all within \code{\[y_min, y_max\]}.
#' @param D Binary treatment indicator.
#' @param X Stratum label per unit.
#' @param y_min,y_max A priori outcome support.
#' @return List with lower, upper, width, inter_lower, inter_upper,
#'   n_strata, n.
#' @references Manski (1990), AER P&P 80(2), 319-323; Manski (2003),
#'   Partial Identification of Probability Distributions, Springer.
#'   Standard published form; neither source was available locally, so the
#'   bound is stated in full above for checking.
#' @export
Mskbnd2 <- function(y, D, X, y_min, y_max) {
  yv <- .t1_vec(y)
  d <- .t1_vec(D)
  n <- length(yv)
  if (n == 0L) stop("y is empty")
  xs <- unlist(X)
  if (length(d) != n || length(xs) != n) {
    stop("y, D and X must have the same length")
  }
  lo <- as.numeric(y_min)
  hi <- as.numeric(y_max)
  if (lo > hi) stop("y_min must not exceed y_max")
  if (any(yv < lo | yv > hi)) stop("observed outcomes must lie in [y_min, y_max]")
  if (any(d != 0 & d != 1)) stop("D must be binary 0/1")
  levels <- unique(xs)
  tot_lo <- 0
  tot_hi <- 0
  ilo <- -Inf
  ihi <- Inf
  for (lev in levels) {
    idx <- which(xs == lev)
    nk <- length(idx)
    t <- idx[d[idx] == 1]
    cc <- idx[d[idx] == 0]
    p1 <- length(t) / nk
    p0 <- length(cc) / nk
    m1 <- if (length(t)) sum(yv[t]) / length(t) else 0
    m0 <- if (length(cc)) sum(yv[cc]) / length(cc) else 0
    e1lo <- m1 * p1 + lo * p0
    e1hi <- m1 * p1 + hi * p0
    e0lo <- m0 * p0 + lo * p1
    e0hi <- m0 * p0 + hi * p1
    klo <- e1lo - e0hi
    khi <- e1hi - e0lo
    w <- nk / n
    tot_lo <- tot_lo + w * klo
    tot_hi <- tot_hi + w * khi
    if (klo > ilo) ilo <- klo
    if (khi < ihi) ihi <- khi
  }
  .t1_result(lower = tot_lo, upper = tot_hi, width = tot_hi - tot_lo,
             inter_lower = ilo, inter_upper = ihi, n_strata = length(levels), n = n,
             method = "Manski no-assumption ATE bounds within covariate strata")
}
