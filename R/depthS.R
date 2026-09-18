# SPDX-License-Identifier: AGPL-3.0-or-later

#' Liu simplicial depth
#'
#' Formula: P(theta in the simplex of d+1 random points)
#'
#' The sample version counts the simplices spanned by d+1 data points
#' that contain theta, over all C(n, d+1) of them.  In one dimension a
#' simplex is an interval, so the depth is the fraction of pairs
#' straddling theta and its population value is exactly
#' 2 F(theta) (1 - F(theta)) -- maximised at the median.
#'
#' @param X An n x d data matrix, d = 1 or 2.
#' @param theta The point, length d.
#' @return List with \code{estimate}, \code{depth},
#'   \code{n_containing}, \code{n_simplices}, \code{ecdf},
#'   \code{closed_form_1d}, \code{n}, \code{d}, \code{method}.
#' @references Liu (1990), Ann. Statist. 18(1):405-414.
#' @export
#' @examples
#' DepthS(X = c(1, 2, 3, 4, 5, 6, 7, 8), theta = 0.5)
DepthS <- function(X, theta) {
  M <- .s03mat(X)
  n <- nrow(M)
  if (n < 2L) stop("need at least two data points")
  d <- ncol(M)
  th <- .s03vec(theta)
  if (length(th) != d) stop("X and theta must have the same dimension")
  if (!(d %in% c(1L, 2L)))
    stop("simplicial depth here supports d = 1 or 2")
  cnt <- 0L
  tot <- 0L
  if (d == 1L) {
    xs <- M[, 1]
    for (i in seq_len(n - 1L)) for (j in seq(i + 1L, n)) {
      tot <- tot + 1L
      lo <- min(xs[i], xs[j])
      hi <- max(xs[i], xs[j])
      if (lo <= th[1] && th[1] <= hi) cnt <- cnt + 1L
    }
    F <- sum(xs <= th[1]) / n
    cf <- 2 * F * (1 - F)
  } else {
    if (n < 3L) stop("need at least three points in two dimensions")
    side <- function(a, b, c)
      (b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1])
    for (i in seq_len(n - 2L)) for (j in seq(i + 1L, n - 1L))
      for (k in seq(j + 1L, n)) {
        tot <- tot + 1L
        s1 <- side(M[i, ], M[j, ], th)
        s2 <- side(M[j, ], M[k, ], th)
        s3 <- side(M[k, ], M[i, ], th)
        if ((s1 >= 0 && s2 >= 0 && s3 >= 0) ||
            (s1 <= 0 && s2 <= 0 && s3 <= 0)) cnt <- cnt + 1L
      }
    F <- NaN
    cf <- NaN
  }
  .t1_result(estimate = if (tot > 0L) cnt / tot else NaN,
             depth = if (tot > 0L) cnt / tot else NaN,
             n_containing = cnt, n_simplices = tot, ecdf = F,
             closed_form_1d = cf, n = n, d = d,
             method = "Liu simplicial depth")
}
