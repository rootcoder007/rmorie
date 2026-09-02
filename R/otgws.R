# SPDX-License-Identifier: AGPL-3.0-or-later
# Couple two spaces that have no common ground metric
#
# Ordinary transport needs a cost between a point of one space and a
# point of the other. When the two live in different spaces -- a graph
# and a point cloud, say -- no such cost exists, and the only thing that
# can be compared is the pattern of within-space distances. The resulting
# objective is quadratic in the plan, so it is minimised by repeatedly
# linearising it and running Sinkhorn on the linearisation.
#
# The Gromov objective is not convex, so what the iteration returns is a
# stationary point. When the two spaces share a symmetry and the plan is
# started at \eqn{ b prime}, which is itself symmetric, the iteration cannot
# prime break the tie and settles on the symmetric average of the two optima
# prime rather than on either of them.
# prime
# prime Formula: \code{min_T sum_ijkl |Cx_ik - Cy_jl|^2 T_ij T_kl - eps H(T)},
# prime solved by \code{T^{l+1} = argmin <T, -Cx T^l Cy> - eps H(T)} -- Peyre
#' and Cuturi (2019) eq. (10.27)-(10.28), p. 176; Peyre, Cuturi and
#' Solomon (2016).
#'
#' @param Cx Within-space distances of the first space, n by n, symmetric.
#' @param Cy Within-space distances of the second space, m by m.
#' @param a,b Marginals.
#' @param epsilon Entropic strength, positive.
#' @param max_iter Outer linearisations.
#' @param inner_iter Sinkhorn sweeps per linearisation.
#' @return List with \code{T}, \code{cost}, \code{GW}, \code{n},
#'   \code{m}, \code{iters}.
#' @references Peyre, G., Cuturi, M. and Solomon, J. (2016). Proceedings
#'   of Machine Learning Research 48:2664-2672 (ICML).
#' @export
Otgws <- function(Cx, Cy, a, b, epsilon, max_iter = 20, inner_iter = 200) {
  A <- as.matrix(Cx); B <- as.matrix(Cy)
  aa <- .ot_hist(a); bb <- .ot_hist(b)
  n <- length(aa); m <- length(bb)
  if (nrow(A) != n || ncol(A) != n) stop("Cx must be n by n")
  if (nrow(B) != m || ncol(B) != m) stop("Cy must be m by m")
  eps <- as.numeric(epsilon)
  gwc <- function(T) {
    t1 <- sum(outer(aa, aa) * A^2)
    t3 <- sum(outer(bb, bb) * B^2)
    CT <- A %*% T %*% B
    list(cost = t1 + t3 - 2 * sum(CT * T), CT = CT)
  }
  T <- outer(aa, bb)
  it <- as.integer(max_iter)
  for (t in seq_len(it)) {
    L <- -gwc(T)$CT
    T <- .ot_sinkhorn(aa, bb, L, eps, inner_iter)$T
  }
  cost <- gwc(T)$cost
  if (cost < 0) cost <- 0
  .t1_result(T = T, cost = cost, GW = sqrt(cost), n = n, m = m, iters = it,
             method = "Entropic Gromov-Wasserstein coupling")
}
