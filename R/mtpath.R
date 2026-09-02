# SPDX-License-Identifier: AGPL-3.0-or-later
#' Meta-path counts on a heterogeneous information network
#'
#' Sun, Han, Yan, Yu and Wu (2011), PathSim: meta path-based top-k
#' similarity search in heterogeneous information networks, PVLDB 4(11),
#' 992-1003.  A meta path is a sequence of object types, and the number of
#' path instances following it is an entry of the product of the
#' type-restricted adjacency matrices, M_P = W_(A1,A2) ... W_(Al,A(l+1)).
#' PathSim normalises this to s(x, y) = 2 M_P(x, y) / (M_P(x, x) + M_P(y,
#' y)) for a symmetric meta path, which is returned too.  The PVLDB paper
#' is open access but was not retrievable here; both expressions are
#' quoted in their standard published form.
#'
#' @param G adjacency matrix of the whole network.
#' @param node_types type label per node.
#' @param metapath the sequence of types.
#' @return list: estimate, M, pathsim, counts, symmetric, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 1, 1, 0), 2, 2)
#' Metapath(A, c("A", "P"), c("A", "P"))$estimate
#' @export
Metapath <- function(G, node_types = NULL, metapath = NULL) {
  W <- .s03mat(G)
  n <- nrow(W)
  ty <- if (!is.null(node_types)) as.character(node_types) else rep("0", n)
  mp <- if (!is.null(metapath)) as.character(metapath) else rep(ty[1], 2L)
  M <- diag(1, n)
  if (length(mp) > 1L) for (step in seq_len(length(mp) - 1L)) {
    a <- mp[step]
    b <- mp[step + 1L]
    S <- matrix(0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n)) {
      if (ty[i] == a && ty[j] == b) S[i, j] <- W[i, j]
    }
    M <- .s03matmul(M, S)
  }
  ends <- which(ty == mp[length(mp)])
  starts <- which(ty == mp[1])
  tot <- 0
  for (i in starts) for (j in ends) tot <- tot + M[i, j]
  sym <- mp[1] == mp[length(mp)]
  ps <- matrix(NaN, n, n)
  if (sym) for (i in starts) for (j in starts) {
    den <- M[i, i] + M[j, j]
    ps[i, j] <- if (den > 0) 2 * M[i, j] / den else 0
  }
  counts <- numeric(n)
  for (i in seq_len(n)) { s <- 0
  for (j in seq_len(n)) s <- s + M[i, j]
  counts[i] <- s }
  list(estimate = tot, M = M, pathsim = ps, counts = counts, symmetric = sym,
       method = "Meta-path instance counts and PathSim (Sun et al. 2011)")
}
