# SPDX-License-Identifier: AGPL-3.0-or-later

#' Runs declustering of threshold exceedances
#'
#' Formula: cluster = consecutive exceedances within gap r
#'
#' A cluster ends once r consecutive observations fall below the
#' threshold.  The cluster maxima are approximately independent, which
#' is what makes a GPD fit to them legitimate; the extremal index is
#' estimated by the runs estimator, number of clusters over number of
#' exceedances.
#'
#' @param x Time-ordered series.
#' @param u Threshold.
#' @param r Run length separating two clusters.
#' @return List with \code{cluster_max}, \code{cluster_id},
#'   \code{n_clusters}, \code{theta}, \code{n_exceed},
#'   \code{estimate}, \code{n}, \code{method}.
#' @references Smith (1989), Statistical Science 4(4):367-377.
#' @export
Evdec <- function(x, u, r) {
  x <- .s03vec(x)
  n <- length(x)
  if (n == 0L) stop("empty input: x has no observations")
  u <- as.numeric(u); r <- as.integer(r)
  if (r < 1L) stop("r must be at least 1")
  cid <- integer(n)
  cur <- 0L
  gap <- r + 1L
  for (i in seq_len(n)) {
    if (x[i] > u) {
      if (gap > r) cur <- cur + 1L
      gap <- 0L
      cid[i] <- cur
    } else gap <- gap + 1L
  }
  cmax <- numeric(cur)
  if (cur > 0L) for (c in seq_len(cur)) cmax[c] <- max(x[cid == c])
  nex <- sum(cid > 0L)
  theta <- if (nex > 0L) cur / nex else NaN
  .t1_result(cluster_max = cmax, cluster_id = cid, n_clusters = cur,
             theta = theta, n_exceed = nex, estimate = theta, n = n,
             method = "runs declustering of threshold exceedances")
}
