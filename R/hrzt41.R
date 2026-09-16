# SPDX-License-Identifier: AGPL-3.0-or-later

#' Binary-response identification under median independence
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 4.2, Theorem 4.1 (page 99).  For
#' Y = I(X'beta + U >= 0) with median(U | X = x) = 0 and
#' abs(beta_1) = 1, beta is identified if (a) the support of X is not
#' contained in any proper linear subspace of R^d and (b) for almost
#' every xtilde the distribution of X_1 given Xtilde = xtilde has an
#' everywhere positive density.  Mean independence is not enough
#' (Example 4.1, page 98).
#'
#' Condition (b) is not decidable from a finite sample; what is
#' reported is how completely the support of X_1 is covered inside
#' fixed cells of the remaining covariates.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param beta Numeric vector, scale normalised so abs(beta\[1\]) == 1.
#' @param ncell Integer; conditioning cells per remaining covariate,
#'   cut at fixed quantiles.
#' @param nbin Integer; bins of the range of X_1 used to measure
#'   coverage inside a cell.
#' @return Named list with identified, conda, condb, condscale, rank,
#'   dim, minsv, coverage, ncells, n, method.
#' @keywords internal
#' @examples
#' g <- seq(-3, 3, length.out = 40)
#' x <- cbind(rep(g, 40), rep(seq(-1, 1, length.out = 40), each = 40))
#' Binidmed(x, c(1, 0.5))$coverage
#' @export
Binidmed <- function(x, beta, ncell = 4L, nbin = 10L) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  b <- as.numeric(beta)
  if (ncol(X) != length(b) && nrow(X) == length(b)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)

  condscale <- abs(abs(b[1L]) - 1) <= 1e-12

  sv <- svd(X)$d
  rank <- if (length(sv) && sv[1L] > 0) sum(sv > sv[1L] * 1e-12) else 0L
  minsv <- if (length(sv)) sv[length(sv)] else 0
  conda <- rank == d

  x1 <- X[, 1L]
  lo <- min(x1)
  hi <- max(x1)
  cellid <- numeric(n)
  if (d > 1L) {
    mult <- 1
    for (j in 2L:d) {
      cuts <- as.numeric(stats::quantile(X[, j],
                                         seq(0, 1, length.out = ncell + 1L)))
      idx <- numeric(n)
      for (k in seq_len(ncell - 1L)) idx <- idx + (X[, j] >= cuts[k + 1L])
      cellid <- cellid + mult * idx
      mult <- mult * ncell
    }
  }
  cells <- sort(unique(cellid))
  edges <- seq(lo, hi, length.out = nbin + 1L)
  filled <- numeric(0)
  for (cc in cells) {
    sel <- cellid == cc
    if (!any(sel)) next
    v <- x1[sel]
    hit <- 0L
    for (k in seq_len(nbin)) {
      a <- edges[k]
      bb <- edges[k + 1L]
      inb <- if (k == nbin) v >= a & v <= bb else v >= a & v < bb
      if (any(inb)) hit <- hit + 1L
    }
    filled <- c(filled, hit / nbin)
  }
  coverage <- if (length(filled)) min(filled) else 0
  condb <- coverage >= 1

  list(identified = conda && condb && condscale, conda = conda,
       condb = condb, condscale = condscale, rank = as.integer(rank),
       dim = as.integer(d), minsv = minsv, coverage = coverage,
       ncells = length(cells), n = n,
       method = "Horowitz (2009) Theorem 4.1, median independence")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Binidmed
#' @keywords internal
#' @export
morie_horowitz_thm4_1_id_median <- Binidmed
