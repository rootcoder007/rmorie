# SPDX-License-Identifier: AGPL-3.0-or-later
#' Minimum volume ellipsoid
#'
#' Rousseeuw, P. J. (1985), "Multivariate estimation with high breakdown
#' point", in Mathematical Statistics and Applications, Vol. B, Reidel,
#' 283-297.  The MVE is the ellipsoid of smallest volume covering at least h of
#' the n observations; its centre estimates location and its shape, suitably
#' scaled, estimates scatter.  Like the MCD it attains the maximal breakdown
#' point at h = \[(n + p + 1)/2\].
#'
#' Computation follows the standard resampling scheme: for each (p+1)-subset J
#' take the mean m_J and covariance C_J of J, and inflate the ellipsoid until
#' it covers h points, which means scaling by the h-th smallest squared
#' Mahalanobis distance m2_J.  The volume of \{x : (x-m)' C^-1 (x-m) <= m2\} is
#' proportional to m2^(p/2) sqrt(det C) = sqrt(det(m2 * C)), so minimising the
#' volume is minimising det(m2_J * C_J) over J.  That determinant is the
#' objective reported here.
#'
#' DETERMINISM.  Subsets are enumerated deterministically rather than drawn at
#' random, so both language arms examine the same candidates in the same order.
#' They are taken by an even STRIDE through the lexicographic enumeration, not
#' from its prefix: the prefix is drawn almost entirely from the lowest
#' indices, which biases the search towards whatever sits at the start of the
#' data.  See the note on .rscombosstride, where a confusion matrix caught
#' exactly that failure in the sibling FastMCD module.
#'
#' The univariate case is a closed form and is this module's anchor: for p = 1
#' an ellipsoid is an interval, so the MVE is the SHORTEST interval containing
#' h points and its centre is that interval's midpoint.  That is exactly the
#' shortest-half construction of Rousseeuw (1984) Theorem 2, p. 873.
#'
#' @param X n-by-p data matrix.
#' @param h coverage; defaults to \[(n + p + 1)/2\].
#' @param n_starts cap on the number of (p+1)-subsets enumerated.
#' @return list: estimate, center, cov, cov_raw, m2, subset, covered, h, n, p,
#'   method.
#' @keywords internal
#' @examples
#' Mvedet(matrix(c(1, 2, 3, 4, 30), 5, 1), 3)$center
#' @export
Mvedet <- function(X, h = NULL, n_starts = 100000L) {
  Xm <- .s03mat(X)
  n <- nrow(Xm)
  if (n == 0L) stop("mve: X is empty")
  p <- ncol(Xm)
  if (p == 0L) stop("mve: X has no columns")
  hh <- if (is.null(h)) .rsmcdh(n, p) else as.integer(h)
  if (hh <= p) stop("mve: h must exceed p")
  if (hh > n) stop("mve: h cannot exceed the number of observations")
  if (n < p + 1L) stop("mve: need at least p + 1 observations")
  bobj <- NULL
  bm2 <- NULL
  bmu <- NULL
  bC <- NULL
  bJ <- NULL
  bcov <- NULL
  for (J in .rscombosstride(n, p + 1L, as.integer(n_starts))) {
    mc <- .rsmeancov(Xm, J)
    dd <- .rsmahal2(Xm, mc$mu, mc$S)
    if (is.null(dd)) next
    ord <- .rsosort(dd)
    m2 <- dd[ord[hh]]
    dC <- .rscovdet(mc$S)
    obj <- (m2 ^ p) * dC
    if (is.null(bobj) || obj < bobj) {
      bobj <- obj
      bm2 <- m2
      bmu <- mc$mu
      bC <- mc$S
      bJ <- J
      bcov <- sort(ord[seq_len(hh)])
    }
  }
  if (is.null(bobj)) stop("mve: every subset was degenerate")
  list(estimate = bobj, center = bmu, cov = bC * bm2, cov_raw = bC, m2 = bm2,
       subset = as.numeric(bJ - 1L), covered = as.numeric(bcov - 1L),
       h = hh, n = n, p = p,
       method = "Rousseeuw (1985) MVE, lexicographic (p+1)-subsets inflated to cover h points, objective det(m2 * C)")
}
