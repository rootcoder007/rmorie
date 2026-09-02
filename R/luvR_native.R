# SPDX-License-Identifier: AGPL-3.0-or-later
#' Louvain community detection with a resolution parameter
#'
#' Multi-level Louvain maximizing the generalized modularity
#' \eqn{Q_\gamma = (1/2m)\sum_{ij}\[A_{ij} - \gamma k_i k_j/2m\]\delta(c_i, c_j)}:
#' a local-moving phase (each node joins the neighbouring community
#' with the largest strictly positive gain) alternates with an
#' aggregation phase (each community contracts to one node carrying the
#' internal weight as a self-loop) until a level no longer improves the
#' quality. Deterministic: nodes are visited in index order, candidate
#' communities in ascending label order, and moves need a strict gain,
#' mirroring the Python arm exactly. gamma = 1 is plain Louvain.
#'
#' @param A Symmetric adjacency (or weight) matrix.
#' @param resolution The gamma parameter; larger values give more,
#'   smaller communities.
#' @param max_levels Cap on aggregation levels.
#' @return List with communities (0-based labels, matching the Python
#'   arm), estimate (final quality), n_communities, levels,
#'   modularity_by_level, n, resolution.
#' @references Blondel, V. D., Guillaume, J.-L., Lambiotte, R. and
#'   Lefebvre, E. (2008). Fast unfolding of communities in large
#'   networks. Journal of Statistical Mechanics, P10008, Sec. 2,
#'   arXiv:0803.0476. Archived:
#'   fetched-wave3/blondel-2008-fast-unfolding-louvain.pdf.
#'
#'   Reichardt, J. and Bornholdt, S. (2006). Statistical mechanics of
#'   community detection. Physical Review E, 74, 016110.
#' @examples
#' blocks <- matrix(0, 6, 6)
#' blocks[1:3, 1:3] <- 1; blocks[4:6, 4:6] <- 1; diag(blocks) <- 0
#' blocks[3, 4] <- blocks[4, 3] <- 1
#' LuvR(blocks)
#' @export
LuvR <- function(A, resolution = 1, max_levels = 20L) {
  a <- as.matrix(A)
  n <- nrow(a)
  if (ncol(a) != n) stop("A must be square")
  gamma <- as.numeric(resolution)
  m2 <- sum(a)
  if (m2 <= 0) stop("A must have positive total weight")

  phase1 <- function(w) {
    nn <- nrow(w)
    comm <- 0:(nn - 1L)
    kdeg <- rowSums(w)
    tot <- kdeg
    moved <- TRUE
    sweeps <- 0L
    while (moved && sweeps < 100L) {
      moved <- FALSE
      sweeps <- sweeps + 1L
      for (i in seq_len(nn)) {
        ci <- comm[i]
        tot[ci + 1L] <- tot[ci + 1L] - kdeg[i]
        links <- new.env(parent = emptyenv())
        for (j in seq_len(nn)) {
          if (j != i && w[i, j] != 0) {
            key <- as.character(comm[j])
            prev <- if (is.null(links[[key]])) 0 else links[[key]]
            links[[key]] <- prev + w[i, j]
          }
        }
        if (is.null(links[[as.character(ci)]])) links[[as.character(ci)]] <- 0
        cand <- sort(as.integer(ls(links)))
        best <- ci
        bestgain <- links[[as.character(ci)]] / m2 -
          gamma * tot[ci + 1L] * kdeg[i] / (m2 * m2)
        for (cc in cand) {
          g <- links[[as.character(cc)]] / m2 -
            gamma * tot[cc + 1L] * kdeg[i] / (m2 * m2)
          if (g > bestgain + 1e-12) {
            bestgain <- g
            best <- cc
          }
        }
        tot[best + 1L] <- tot[best + 1L] + kdeg[i]
        if (best != ci) {
          comm[i] <- best
          moved <- TRUE
        }
      }
    }
    comm
  }

  relabel <- function(comm) {
    seen <- integer(0)
    out <- integer(length(comm))
    for (i in seq_along(comm)) {
      hit <- match(comm[i], seen)
      if (is.na(hit)) {
        seen <- c(seen, comm[i])
        hit <- length(seen)
      }
      out[i] <- hit - 1L
    }
    out
  }

  modularity_gamma <- function(lab) {
    deg <- rowSums(a)
    q <- 0
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (lab[i] == lab[j]) {
          q <- q + (a[i, j] - gamma * deg[i] * deg[j] / m2) / m2
        }
      }
    }
    q
  }

  mapping <- 0:(n - 1L)
  cur <- a
  qs <- numeric(0)
  labels <- 0:(n - 1L)
  for (level in seq_len(as.integer(max_levels))) {
    nn <- nrow(cur)
    comm <- relabel(phase1(cur))
    labels <- comm[mapping + 1L]
    q <- modularity_gamma(labels)
    if (length(qs) && q <= qs[length(qs)] + 1e-12) break
    qs <- c(qs, q)
    k <- max(comm) + 1L
    if (k == nn) break
    agg <- matrix(0, k, k)
    for (i in seq_len(nn)) {
      for (j in seq_len(nn)) {
        agg[comm[i] + 1L, comm[j] + 1L] <- agg[comm[i] + 1L, comm[j] + 1L] + cur[i, j]
      }
    }
    cur <- agg
    mapping <- comm[mapping + 1L]
  }
  list(communities = labels,
       estimate = if (length(qs)) qs[length(qs)] else 0,
       n_communities = max(labels) + 1L, levels = length(qs),
       modularity_by_level = qs, n = n, resolution = gamma,
       method = "Louvain with resolution gamma (Blondel 2008 / Reichardt-Bornholdt 2006)")
}

#' @rdname LuvR
#' @export
louvain <- LuvR
