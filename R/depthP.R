# SPDX-License-Identifier: AGPL-3.0-or-later

#' Stahel-Donoho projection depth
#'
#' Formula: d = 1 / (1 + sup_u |u'x - Med(u'X)| / MAD(u'X))
#'
#' The outlyingness of a point is the worst standardised deviation over
#' all one-dimensional projections, and the depth is its reciprocal
#' transform, so depth lies in (0, 1] and equals exactly 1 at a point
#' whose projection is the median in every direction.  In one dimension
#' the supremum is attained at u = +/-1, so the depth is available in
#' closed form.
#'
#' @param x The point, length d.
#' @param X An n x d data matrix.
#' @param n_dir Number of equally spaced directions when d = 2.
#' @return List with \code{estimate}, \code{depth},
#'   \code{outlyingness}, \code{med}, \code{mad}, \code{worst_dir},
#'   \code{n}, \code{d}, \code{method}.
#' @references Stahel (1981), PhD thesis, ETH Zurich; Donoho (1982),
#'   qualifying paper, Harvard; Zuo & Serfling (2000), Ann. Statist.
#'   28(2):461-482.
#' @export
#' @examples
#' DepthP(x = 5L, X = c(1, 2, 3, 4, 5, 6, 7, 8))
DepthP <- function(x, X, n_dir = 180) {
  p <- .s03vec(x)
  M <- .s03mat(X)
  n <- nrow(M)
  if (n < 2L) stop("need at least two data points")
  d <- ncol(M)
  if (length(p) != d) stop("x and X must have the same dimension")
  if (d == 1L) {
    dirs <- list(1)
  } else if (d == 2L) {
    nd <- as.integer(n_dir)
    if (nd < 2L) stop("n_dir must be at least 2")
    dirs <- lapply(0:(nd - 1L), function(t)
      c(cos(pi * t / nd), sin(pi * t / nd)))
  } else stop("projection depth here supports d = 1 or 2")
  worst <- 0
  wmed <- NaN
  wmad <- NaN
  wdir <- 0L
  for (q in seq_along(dirs)) {
    u <- dirs[[q]]
    proj <- numeric(n)
    for (i in seq_len(n)) {
      s <- 0
      for (t in seq_len(d)) s <- s + M[i, t] * u[t]
      proj[i] <- s
    }
    pu <- 0
    for (t in seq_len(d)) pu <- pu + p[t] * u[t]
    med <- .s03median(proj)
    mad <- .s03mad(proj)
    if (mad <= 0) next
    o <- abs(pu - med) / mad
    if (o > worst) { worst <- o
    wmed <- med
    wmad <- mad
    wdir <- q - 1L }
  }
  .t1_result(estimate = 1 / (1 + worst), depth = 1 / (1 + worst),
             outlyingness = worst, med = wmed, mad = wmad,
             worst_dir = wdir, n = n, d = d,
             method = "Stahel-Donoho projection depth")
}
