# SPDX-License-Identifier: AGPL-3.0-or-later
#' One Leiden refinement pass over an existing partition
#'
#' Louvain can leave a community internally disconnected. Leiden inserts
#' a refinement phase between local moving and aggregation: each
#' community is broken back into singletons and rebuilt from the inside,
#' and a node only joins a sub-community that is itself well connected
#' to the rest of its original community.
#'
#' Determinism: nodes are visited in index order and communities in
#' order of first appearance; no random restarts.
#'
#' Formula: CPM quality \code{H = sum_c \[e_c - gamma * C(n_c, 2)\]}; a
#' subset C of community S is well connected when
#' \code{E(C, S \ C) >= gamma * |C| * (|S| - |C|)}.
#'
#' @param A Non-negative weighted adjacency matrix, symmetrised on entry.
#' @param labels Current community index of each node.
#' @param gamma CPM resolution, default 1.
#' @return List with \code{labels_new}, \code{Q_new}, \code{n_communities}, \code{n}.
#' @references Traag, V. A., Waltman, L. & van Eck, N. J. (2019). From
#'   Louvain to Leiden: guaranteeing well-connected communities.
#'   Scientific Reports 9:5233. Open access.
#' @export
Sgtleid <- function(A, labels, gamma = 1) {
  A <- as.matrix(A)
  n <- nrow(A)
  W <- (A + t(A)) / 2
  diag(W) <- 0
  lab <- as.integer(round(as.numeric(labels)))
  seen <- unique(lab)
  comm <- seq_len(n) - 1L
  for (s in seen) {
    S <- which(lab == s) - 1L
    members <- vector("list", n)
    for (i in S) members[[i + 1L]] <- i
    for (v in S) {
      cur <- members[[comm[v + 1L] + 1L]]
      if (length(cur) != 1L) next
      ev <- sum(W[v + 1L, S[S != v] + 1L])
      if (ev < gamma * (length(S) - 1)) next
      best <- NA_integer_
      bestd <- 0
      for (cc in S) {
        Cm <- members[[cc + 1L]]
        if (length(Cm) == 0L || cc == comm[v + 1L]) next
        rest <- S[!(S %in% Cm)]
        out <- if (length(rest) == 0L) 0 else sum(W[Cm + 1L, rest + 1L])
        if (out < gamma * length(Cm) * (length(S) - length(Cm))) next
        d <- sum(W[v + 1L, Cm + 1L]) - gamma * length(Cm)
        if (d > bestd) { bestd <- d
        best <- cc }
      }
      if (!is.na(best)) {
        members[[comm[v + 1L] + 1L]] <- integer(0)
        members[[best + 1L]] <- c(members[[best + 1L]], v)
        comm[v + 1L] <- best
      }
    }
  }
  ord <- unique(comm)
  newlab <- match(comm, ord) - 1L
  k <- length(ord)
  q <- 0
  for (cc in seq_len(k) - 1L) {
    mem <- which(newlab == cc)
    e <- if (length(mem) > 1L) sum(W[mem, mem][upper.tri(matrix(0, length(mem), length(mem)))]) else 0
    q <- q + e - gamma * length(mem) * (length(mem) - 1) / 2
  }
  .t1_result(labels_new = newlab, Q_new = q, n_communities = k, n = n,
             method = "Leiden refinement phase, CPM quality")
}
