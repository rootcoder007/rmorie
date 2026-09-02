# SPDX-License-Identifier: AGPL-3.0-or-later
#' DeltaCon structural distance between two graphs
#'
#' DeltaCon-0 similarity from fast-belief-propagation affinities: for
#' each graph \eqn{S = (I + \epsilon^2 D - \epsilon A)^{-1}} (eq. 2.2),
#' with \eqn{\epsilon = 1/(1 + \max_i d_{ii})} taken over both graphs
#' so one constant is shared; the distance is the root Euclidean
#' (Matusita) distance
#' \eqn{d = \sqrt{\sum_{ij} (\sqrt{s_{1,ij}} - \sqrt{s_{2,ij}})^2}}
#' (eq. 3.3) and the similarity is \eqn{1/(1+d)} (Algorithm 1).
#'
#' @param G1,G2 Adjacency matrices on the same node set.
#' @param eps Optional override of the influence constant.
#' @return List with distance, similarity, estimate, eps, n.
#' @references Koutra, D., Vogelstein, J. T. and Faloutsos, C. (2013).
#'   DeltaCon: a principled massive-graph similarity function. SIAM SDM
#'   2013, arXiv:1304.4657, eqs. (2.2), (3.3), Table 1, Algorithm 1.
#'   Archived: fetched-wave3/koutra-2013-deltacon.pdf.
#' @examples
#' A <- matrix(0, 3, 3); A[1, 2] <- A[2, 1] <- 1; A[2, 3] <- A[3, 2] <- 1
#' B <- A; B[1, 3] <- B[3, 1] <- 1
#' Strdis(A, B)
#' @export
Strdis <- function(G1, G2, eps = NULL) {
  A1 <- as.matrix(G1)
  A2 <- as.matrix(G2)
  n <- nrow(A1)
  if (ncol(A1) != n || nrow(A2) != n || ncol(A2) != n) {
    stop("G1 and G2 must be square matrices of equal size")
  }
  d1 <- rowSums(A1)
  d2 <- rowSums(A2)
  if (is.null(eps)) {
    dmax <- max(max(d1), max(d2))
    eps <- 1 / (1 + dmax)
  }
  eps <- as.numeric(eps)
  affinity <- function(A, d) solve(diag(n) + eps^2 * diag(d, n) - eps * A)
  S1 <- affinity(A1, d1)
  S2 <- affinity(A2, d2)
  dist <- sqrt(sum((sqrt(abs(S1)) - sqrt(abs(S2)))^2))
  sim <- 1 / (1 + dist)
  list(distance = dist, similarity = sim, estimate = sim, eps = eps, n = n,
       method = "DeltaCon-0 (FaBP affinities, RootED distance)")
}

#' @rdname Strdis
#' @export
structural_distance <- Strdis
