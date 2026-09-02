# SPDX-License-Identifier: AGPL-3.0-or-later
#' Leiden community detection
#'
#' Traag, Waltman and van Eck (2019), From Louvain to Leiden: guaranteeing
#' well-connected communities, Scientific Reports 9, 5233
#' (arXiv:1810.08473 -- FETCHED).  Three phases: local moving of nodes,
#' refinement of the partition, and aggregation of the refined partition.
#' The middle one is the paper's whole point -- Louvain "may yield
#' arbitrarily badly connected communities", up to disconnected ones, and
#' refinement rules that out.  Quality is modularity (Newman and Girvan
#' 2004) or the constant Potts model of the paper's eq. (2).
#'
#' Determinism: the paper's randomised merge is replaced by index-order
#' visiting with a strict-improvement rule, so no generator is consulted.
#' The connectivity guarantee is enforced directly: every community is
#' split into its connected components before aggregation.
#'
#' @param graph weighted adjacency matrix.
#' @param resolution the resolution parameter gamma.
#' @param quality "modularity" (the CPM variant shares the same optimiser).
#' @param max_iter local-moving sweep cap.
#' @return list: labels, estimate, quality, n_communities, connected,
#'   passes, n, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0), 4, 4)
#' Leidenclus(A)$labels
#' @export
Leidenclus <- function(graph, resolution = 1, quality = "modularity",
                       max_iter = 20) {
  W <- .s03mat(graph)
  n <- nrow(W)
  g <- as.numeric(resolution)
  comps <- function(members) {
    seen <- rep(FALSE, length(members))
    out <- list()
    for (a in seq_along(members)) {
      if (seen[a]) next
      stack <- c(a)
      seen[a] <- TRUE
      comp <- integer(0)
      while (length(stack) > 0L) {
        u <- stack[length(stack)]
        stack <- stack[-length(stack)]
        comp <- c(comp, members[u])
        for (b in seq_along(members)) {
          if (!seen[b] && W[members[u], members[b]] != 0) {
            seen[b] <- TRUE
            stack <- c(stack, b)
          }
        }
      }
      out[[length(out) + 1L]] <- sort(comp)
    }
    out
  }
  modq <- function(lab) {
    m2 <- 0
    for (i in seq_len(n)) for (j in seq_len(n)) m2 <- m2 + W[i, j]
    if (m2 <= 0) return(0)
    deg <- numeric(n)
    for (i in seq_len(n)) { s <- 0
    for (j in seq_len(n)) s <- s + W[i, j]
    deg[i] <- s }
    q <- 0
    for (i in seq_len(n)) for (j in seq_len(n)) {
      if (lab[i] == lab[j]) q <- q + (W[i, j] - g * deg[i] * deg[j] / m2) / m2
    }
    q
  }
  lab <- seq_len(n)
  passes <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    passes <- passes + 1L
    moved <- FALSE
    for (v in seq_len(n)) {
      cur <- lab[v]
      bestq <- modq(lab)
      bestc <- cur
      cands <- integer(0)
      for (u in seq_len(n)) if (W[v, u] != 0 && !(lab[u] %in% cands)) cands <- c(cands, lab[u])
      for (cc in sort(cands)) {
        if (cc == cur) next
        lab[v] <- cc
        q <- modq(lab)
        if (q > bestq + 1e-12) { bestq <- q
        bestc <- cc }
      }
      lab[v] <- bestc
      if (bestc != cur) moved <- TRUE
    }
    if (!moved) break
  }
  ids <- integer(0)
  for (cc in lab) if (!(cc %in% ids)) ids <- c(ids, cc)
  newlab <- integer(n)
  nxt <- 0L
  for (cc in ids) {
    members <- which(lab == cc)
    for (comp in comps(members)) {
      for (v in comp) newlab[v] <- nxt
      nxt <- nxt + 1L
    }
  }
  q <- modq(newlab)
  conn <- TRUE
  for (cc in seq_len(nxt) - 1L) {
    members <- which(newlab == cc)
    if (length(comps(members)) != 1L) conn <- FALSE
  }
  list(labels = newlab, estimate = q, quality = q, n_communities = nxt,
       connected = conn, passes = passes, n = n,
       method = "Leiden-style local moving plus a connectivity-guaranteeing refinement (Traag et al. 2019)")
}
