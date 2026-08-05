# SPDX-License-Identifier: AGPL-3.0-or-later
#' Degree assortativity: Pearson correlation over edge endpoints
#'
#' Newman (2002), "Assortative mixing in networks", Physical Review Letters
#' 89(20), 208701, doi:10.1103/PhysRevLett.89.208701, and Newman (2003),
#' "Mixing patterns in networks", Physical Review E 67(2), 026126,
#' doi:10.1103/PhysRevE.67.026126, fetched from arXiv (cond-mat/0209450) and
#' read.  Equation (26) of the 2003 paper is the computational form,
#' r = (sum_i j_i k_i - M^-1 sum_i j_i sum_i k_i) /
#' sqrt((sum_i j_i^2 - M^-1 (sum_i j_i)^2)(sum_i k_i^2 - M^-1 (sum_i k_i)^2)),
#' "where j_i and k_i are the excess in-degree and out-degree of the vertices
#' that the ith edge leads into and out of respectively, and M is again the
#' number of edges. For an undirected network we can use the same formula."
#'
#' Two things decide whether this is right.  Excess degree versus plain degree:
#' the paper says excess, degree minus one, and for an undirected graph it
#' makes no difference, since r is a correlation and subtracting the same
#' constant from every j_i and k_i leaves it unchanged -- that invariance is
#' checked as an anchor rather than asserted.  And symmetrisation: an
#' undirected edge has no direction, so each edge must enter the sums as both
#' (j, k) and (k, j); skip that and r depends on how the edge list happened to
#' be written down, the same graph giving different answers.
#'
#' This is the scalar/degree coefficient, a different quantity from the
#' enumerative assortativity in asorxx.R; Newman derives both and they do not
#' agree numerically.  Neither is a duplicate.
#'
#' @param y ignored; accepted for interface compatibility with the shelf.
#' @param A n-by-n adjacency matrix, treated as undirected and unweighted.
#' @param excess use excess degree (degree - 1) as the paper does; the result
#'   is identical either way and the switch exists to exercise that.
#' @return list: r, estimate, M, degree, sum_jk, sum_j, sum_j2, excess, n,
#'   method.
#' @keywords internal
#' @examples
#' A <- rbind(c(0,1,1,0), c(1,0,1,0), c(1,1,0,1), c(0,0,1,0))
#' Assort(NULL, A)$r
#' @export
Assort <- function(y = NULL, A = NULL, excess = TRUE) {
  if (is.null(A)) stop("degree_assortativity: an adjacency matrix is required")
  Am <- as.matrix(A); storage.mode(Am) <- "double"
  n <- nrow(Am)
  if (n == 0L || length(Am) == 0L) stop("degree_assortativity: the graph is empty")
  if (ncol(Am) != n) stop("degree_assortativity: the adjacency matrix is not square")
  deg <- numeric(n)
  eu <- integer(0); ev <- integer(0)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (j <= i) next
      if (Am[i, j] != 0 || Am[j, i] != 0) {
        eu <- c(eu, i); ev <- c(ev, j)
        deg[i] <- deg[i] + 1
        deg[j] <- deg[j] + 1
      }
    }
  }
  M <- length(eu)
  if (M == 0L) stop("degree_assortativity: the graph has no edges")
  off <- if (isTRUE(excess)) 1 else 0
  sjk <- 0; sj <- 0; sk <- 0; sj2 <- 0; sk2 <- 0
  for (t in seq_len(M)) {
    for (o in 1:2) {
      p <- if (o == 1L) eu[t] else ev[t]
      q <- if (o == 1L) ev[t] else eu[t]
      jj <- deg[p] - off
      kk <- deg[q] - off
      sjk <- sjk + jj * kk
      sj <- sj + jj; sk <- sk + kk
      sj2 <- sj2 + jj * jj; sk2 <- sk2 + kk * kk
    }
  }
  m2 <- 2 * M
  num <- sjk - sj * sk / m2
  d1 <- sj2 - sj * sj / m2
  d2 <- sk2 - sk * sk / m2
  den <- sqrt(d1 * d2)
  if (den == 0) {
    stop("degree_assortativity: every edge endpoint has the same degree, r is undefined")
  }
  r <- num / den
  list(r = r, estimate = r, M = M, degree = deg, sum_jk = sjk, sum_j = sj,
       sum_j2 = sj2, excess = isTRUE(excess), n = n,
       method = "Newman (2003) eq. (26), degree assortativity over symmetrised edge endpoints")
}
