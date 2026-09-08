# SPDX-License-Identifier: AGPL-3.0-or-later
#' Assortativity coefficient for an enumerative (categorical) attribute
#'
#' Newman (2003), "Mixing patterns in networks", Physical Review E 67(2),
#' 026126, doi:10.1103/PhysRevE.67.026126, fetched from arXiv
#' (cond-mat/0209450) and read.  Equation (2) on p.2:
#' r = (sum_i e_ii - sum_i a_i b_i) / (1 - sum_i a_i b_i), where e_ij is the
#' fraction of all edges joining a vertex of type i to one of type j,
#' a_i = sum_j e_ij and b_i = sum_j e_ji.  Equation (3) gives the lower bound
#' r_min = -sum_i a_i b_i / (1 - sum_i a_i b_i), returned as r_min: r is not a
#' correlation on \[-1, 1\].  It reaches 1 for perfect assortative mixing but its
#' most negative attainable value depends on the type distribution, so calling
#' -0.3 "weak disassortativity" without comparing it to r_min is a mistake the
#' output makes avoidable; r_normalised = r/|r_min| is supplied for r < 0.
#'
#' This is the enumerative coefficient, for unordered categories.  It is a
#' different quantity from the degree assortativity in assort.R, which is a
#' Pearson correlation over edge endpoints; Newman derives both in this one
#' paper and they do not agree numerically.  Neither is a duplicate.
#'
#' The mixing matrix is symmetrised: an undirected edge between types i and j
#' contributes half to e_ij and half to e_ji, so e is symmetric and sums to one
#' regardless of how the edge list happened to be oriented.  Without that, r
#' depends on arbitrary orientation.
#'
#' @param G n-by-n adjacency matrix, treated as undirected; the diagonal is
#'   ignored and weights are honoured.
#' @param attribute categorical vertex label, one per vertex.
#' @return list: r, estimate, r_min, r_normalised, e, a, b, trace_e, sum_ab,
#'   n_types, n, method.
#' @keywords internal
#' @examples
#' A <- rbind(c(0,1,0,0), c(1,0,1,0), c(0,1,0,1), c(0,0,1,0))
#' Asorxx(A, c("x", "x", "y", "y"))$r
#' @export
Asorxx <- function(G, attribute) {
  A <- as.matrix(G)
  storage.mode(A) <- "double"
  n <- nrow(A)
  if (n == 0L || length(A) == 0L) stop("assortativity: the graph is empty")
  if (ncol(A) != n) stop("assortativity: the adjacency matrix is not square")
  att <- as.character(attribute)
  if (length(att) != n) stop("assortativity: attribute has one entry per vertex")
  types <- sort(unique(att))
  T <- length(types)
  if (T == 0L) stop("assortativity: no attribute values")
  e <- matrix(0, nrow = T, ncol = T)
  tot <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      w <- A[i, j]
      if (w == 0) next
      if (w < 0) stop("assortativity: negative edge weight")
      ti <- match(att[i], types)
      tj <- match(att[j], types)
      e[ti, tj] <- e[ti, tj] + 0.5 * w
      e[tj, ti] <- e[tj, ti] + 0.5 * w
      tot <- tot + w
    }
  }
  if (tot <= 0) stop("assortativity: the graph has no edges")
  e <- e / tot
  a <- numeric(T)
  b <- numeric(T)
  for (i in seq_len(T)) {
    sa <- 0
    sb <- 0
    for (j in seq_len(T)) { sa <- sa + e[i, j]
    sb <- sb + e[j, i] }
    a[i] <- sa
    b[i] <- sb
  }
  tr <- 0
  for (i in seq_len(T)) tr <- tr + e[i, i]
  ab <- 0
  for (i in seq_len(T)) ab <- ab + a[i] * b[i]
  den <- 1 - ab
  if (den == 0) stop("assortativity: every edge is within one type, r is undefined")
  r <- (tr - ab) / den
  rmin <- -ab / den
  list(r = r, estimate = r, r_min = rmin,
       r_normalised = if (r < 0 && rmin != 0) r / abs(rmin) else NA_real_,
       e = e, a = a, b = b, trace_e = tr, sum_ab = ab, n_types = T, n = n,
       method = "Newman (2003) eq. (2), enumerative assortativity")
}
