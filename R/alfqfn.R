# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaZero action value Q(s, a)
#'
#' Schrittwieser et al. (2020), arXiv:1911.08265 (FETCHED), appendix B,
#' writes Q(s,a) as the mean of the values backed up through the edge,
#' W(s,a) / N(s,a) in the AlphaGo Zero notation (Silver et al., Nature
#' 550, 354-359).  With counts N(s,a,z) of each distinct backed-up return
#' v(z) this is Q(s,a) = sum_z N(s,a,z) v(z) / N(s,a).  An unvisited edge
#' gets `unvisited`, which AlphaGo Zero sets to zero.
#'
#' @param N visit counts: a vector paired with W, or a matrix of
#'   N(s,a,z) with actions in rows.
#' @param v the totals W(s,a) (vector case) or the distinct returns
#'   v(z) (matrix case).
#' @param unvisited Q for an edge with N = 0.
#' @return list: estimate, q, w, n, method.
#' @keywords internal
#' @examples
#' Mctsq(c(3, 0), c(1.5, 0))$q
#' @export
Mctsq <- function(N, v, unvisited = 0) {
  if (is.matrix(N)) {
    rows <- N
    vz <- .s03vec(v)
    nr <- nrow(rows)
    q <- numeric(nr)
    w <- numeric(nr)
    nn <- numeric(nr)
    for (i in seq_len(nr)) {
      tot <- 0
      wt <- 0
      for (j in seq_len(ncol(rows))) {
        tot <- tot + rows[i, j]
        wt <- wt + rows[i, j] * vz[j]
      }
      nn[i] <- tot
      w[i] <- wt
      q[i] <- if (tot > 0) wt / tot else as.numeric(unvisited)
    }
  } else {
    nn <- .s03vec(N)
    w <- .s03vec(v)
    q <- numeric(length(nn))
    for (a in seq_along(nn)) q[a] <- if (nn[a] > 0) w[a] / nn[a] else as.numeric(unvisited)
  }
  list(
    estimate = if (length(q)) q[1] else NaN, q = q, w = w, n = nn,
    method = "AlphaZero action value Q(s,a) = W(s,a) / N(s,a)"
  )
}
