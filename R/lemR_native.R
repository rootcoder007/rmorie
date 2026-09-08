# SPDX-License-Identifier: AGPL-3.0-or-later
#' Leiden refined community detection
#'
#' Deterministic Leiden-style optimisation mirroring the Python
#' morie.fn.scleid engine exactly: greedy local moving of nodes in index
#' order (a node moves only on a strict quality improvement), then the
#' refinement guarantee applied directly -- every community is split
#' into its connected components before the result is returned, which
#' is the property the Leiden refinement phase exists to provide
#' (Louvain may yield arbitrarily badly connected communities).
#'
#' Quality is generalized modularity with resolution gamma
#' (quality = "modularity"): \eqn{Q = (1/2m)\sum_{ij}(A_{ij} - \gamma k_i k_j/2m)\delta(c_i,c_j)}.
#'
#' @param A Weighted adjacency matrix.
#' @param resolution Resolution parameter gamma.
#' @param quality Quality function; "modularity" is implemented.
#' @param max_iter Local-moving passes.
#' @return List with labels (0-based, matching the Python arm),
#'   estimate, quality, n_communities, connected, passes, n.
#' @references Traag, V. A., Waltman, L. and van Eck, N. J. (2019).
#'   From Louvain to Leiden: guaranteeing well-connected communities.
#'   Scientific Reports, 9, 5233, arXiv:1810.08473. Archived:
#'   fetched-wave3/traag-2019-louvain-to-leiden.pdf.
#' @examples
#' blocks <- matrix(0, 6, 6)
#' blocks[1:3, 1:3] <- 1; blocks[4:6, 4:6] <- 1; diag(blocks) <- 0
#' blocks[3, 4] <- blocks[4, 3] <- 1
#' LemR(blocks)
#' @export
LemR <- function(A, resolution = 1, quality = "modularity", max_iter = 20L) {
  W <- as.matrix(A)
  n <- nrow(W)
  if (ncol(W) != n) stop("A must be square")
  g <- as.numeric(resolution)

  modularity_of <- function(lab) {
    m2 <- sum(W)
    if (m2 <= 0) return(0)
    deg <- rowSums(W)
    q <- 0
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (lab[i] == lab[j]) q <- q + (W[i, j] - g * deg[i] * deg[j] / m2) / m2
      }
    }
    q
  }

  components_of <- function(members) {
    k <- length(members)
    seen <- rep(FALSE, k)
    out <- list()
    for (a0 in seq_len(k)) {
      if (seen[a0]) next
      stack <- c(a0)
      seen[a0] <- TRUE
      comp <- integer(0)
      while (length(stack)) {
        u <- stack[length(stack)]
        stack <- stack[-length(stack)]
        comp <- c(comp, members[u])
        for (b in seq_len(k)) {
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

  lab <- 0:(n - 1L)
  passes <- 0L
  for (iter in seq_len(as.integer(max_iter))) {
    passes <- passes + 1L
    moved <- FALSE
    for (v in seq_len(n)) {
      cur <- lab[v]
      bestq <- modularity_of(lab)
      bestc <- cur
      cands <- integer(0)
      for (u in seq_len(n)) {
        if (W[v, u] != 0 && !(lab[u] %in% cands)) cands <- c(cands, lab[u])
      }
      for (cc in sort(cands)) {
        if (cc == cur) next
        lab[v] <- cc
        q <- modularity_of(lab)
        if (q > bestq + 1e-12) {
          bestq <- q
          bestc <- cc
        }
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
    for (comp in components_of(members)) {
      newlab[comp] <- nxt
      nxt <- nxt + 1L
    }
  }
  q <- modularity_of(newlab)
  conn <- TRUE
  for (cc in 0:(nxt - 1L)) {
    members <- which(newlab == cc)
    if (length(components_of(members)) != 1L) conn <- FALSE
  }
  list(labels = newlab, estimate = q, quality = q, n_communities = nxt,
       connected = conn, passes = passes, n = n,
       method = "Leiden-style local moving plus a connectivity-guaranteeing refinement (Traag et al. 2019)")
}

#' @rdname LemR
#' @export
leiden_grph <- LemR
