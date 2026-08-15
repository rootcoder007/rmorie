# SPDX-License-Identifier: AGPL-3.0-or-later
#' One Louvain local-moving phase
#'
#' Each node starts alone and is repeatedly moved to the neighbouring
#' community with the largest positive modularity gain
#' dQ = k_i_in/m - Sigma_tot k_i / (2 m^2).  Nodes are visited in index order
#' and ties go to the smallest community label, so the pass is deterministic.
#' Source consulted: Blondel, Guillaume, Lambiotte and Lefebvre (2008), JSTAT
#' P10008, section 2.
#'
#' @param A symmetric adjacency (or weight) matrix.
#' @return list: estimate, communities, n_communities, modularity_before,
#'   sweeps, n, method.
#' @keywords internal
#' @examples
#' sgtcoml(matrix(c(0,1,1,0), 2, 2))$n_communities
#' @export
.k02phase1 <- function(a, n, m2) {
  comm <- seq_len(n) - 1L
  kdeg <- rowSums(a)
  tot <- kdeg
  moved <- TRUE; sweeps <- 0L
  while (moved && sweeps < 100L) {
    moved <- FALSE; sweeps <- sweeps + 1L
    for (i in seq_len(n)) {
      ci <- comm[i]
      tot[ci + 1L] <- tot[ci + 1L] - kdeg[i]
      links <- list()
      for (j in seq_len(n)) if (j != i && a[i, j] != 0) {
        key <- as.character(comm[j])
        links[[key]] <- (if (is.null(links[[key]])) 0 else links[[key]]) + a[i, j]
      }
      kci <- as.character(ci)
      if (is.null(links[[kci]])) links[[kci]] <- 0
      best <- ci
      bestgain <- links[[kci]] / m2 - tot[ci + 1L] * kdeg[i] / (m2 * m2)
      cands <- sort(as.integer(names(links)))
      for (cc in cands) {
        g <- links[[as.character(cc)]] / m2 - tot[cc + 1L] * kdeg[i] / (m2 * m2)
        if (g > bestgain + 1e-12) { bestgain <- g; best <- cc }
      }
      tot[best + 1L] <- tot[best + 1L] + kdeg[i]
      if (best != ci) { comm[i] <- best; moved <- TRUE }
    }
  }
  list(comm = comm, sweeps = sweeps)
}

.k02relabel <- function(comm) {
  seen <- integer(0); out <- integer(length(comm))
  for (i in seq_along(comm)) {
    w <- which(seen == comm[i])
    if (length(w) == 0L) { seen <- c(seen, comm[i]); out[i] <- length(seen) - 1L }
    else out[i] <- w[1] - 1L
  }
  out
}

#' @rdname sgtcoml
#' @keywords internal
#' @export
sgtcoml <- function(A) {
  a <- as.matrix(A); dimnames(a) <- NULL
  n <- nrow(a); m2 <- sum(a)
  before <- k02mod(a, seq_len(n) - 1L)
  ph <- if (m2 > 0) .k02phase1(a, n, m2) else list(comm = seq_len(n) - 1L, sweeps = 0L)
  comm <- .k02relabel(ph$comm)
  list(estimate = k02mod(a, comm), communities = comm,
       n_communities = length(unique(comm)), modularity_before = before,
       sweeps = as.integer(ph$sweeps), n = n,
       method = "Louvain local-moving phase (Blondel, Guillaume, Lambiotte & Lefebvre 2008, sec. 2)")
}

# CANONICAL TEST
# A <- matrix(0,6,6); E <- rbind(c(1,2),c(1,3),c(2,3),c(3,4),c(4,5),c(4,6),c(5,6))
# for (i in seq_len(nrow(E))) { A[E[i,1],E[i,2]] <- 1; A[E[i,2],E[i,1]] <- 1 }
# stopifnot(abs(sgtcoml(A)$estimate - 0.357142857142857) < 1e-12)

#' @rdname sgtcoml
#' @keywords internal
#' @export
morie_sgtcoml <- sgtcoml
