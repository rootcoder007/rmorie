# SPDX-License-Identifier: AGPL-3.0-or-later

# Q = (1/2m) sum_ij (A_ij - gamma k_i k_j / 2m) delta(z_i, z_j)
.lou_modularity <- function(A, z, resolution = 1) {
  n <- nrow(A)
  k <- numeric(n)
  for (i in seq_len(n)) k[i] <- sum(A[i, ])
  m2 <- sum(k)
  if (m2 <= 0) return(0)
  q <- 0
  for (i in seq_len(n)) for (j in seq_len(n))
    if (z[i] == z[j]) q <- q + A[i, j] - resolution * k[i] * k[j] / m2
  q / m2
}

#' Louvain community detection
#'
#' Formula: greedy modularity max via local moves
#'
#' Each node is moved to the neighbouring community that raises
#' modularity most, repeatedly until no single move helps; the
#' communities are then contracted into super-nodes and the sweep is
#' repeated.  Moves are evaluated in a fixed node order with ties broken
#' toward the lowest community index, so the partition is reproducible
#' rather than merely good.
#'
#' @param G An n x n symmetric weighted adjacency matrix.
#' @param resolution Resolution gamma; larger gives smaller communities.
#' @param max_pass Cap on the number of aggregation passes.
#' @return List with \code{estimate} (modularity), \code{z},
#'   \code{counts}, \code{n_communities}, \code{Q}, \code{n},
#'   \code{method}.
#' @references Blondel, Guillaume, Lambiotte & Lefebvre (2008),
#'   J. Stat. Mech. 2008(10):P10008.
#' @export
Comlou <- function(G, resolution = 1, max_pass = 20) {
  A0 <- .s03mat(G)
  n0 <- nrow(A0)
  if (n0 == 0L) stop("empty input: G has no rows")
  if (ncol(A0) != n0) stop("G must be a square adjacency matrix")
  if (!(resolution > 0)) stop("resolution must be strictly positive")
  A <- matrix(as.numeric(A0), n0, n0)
  member <- seq_len(n0)
  for (pass in seq_len(as.integer(max_pass))) {
    n <- nrow(A)
    k <- numeric(n)
    for (i in seq_len(n)) k[i] <- sum(A[i, ])
    m2 <- sum(k)
    if (m2 <= 0) break
    z <- seq_len(n)
    ktot <- k
    moved <- TRUE
    rounds <- 0L
    while (moved && rounds < 50L) {
      moved <- FALSE
      rounds <- rounds + 1L
      for (i in seq_len(n)) {
        ci <- z[i]
        ktot[ci] <- ktot[ci] - k[i]
        lk <- rep(0, n)
        has <- rep(FALSE, n)
        for (j in seq_len(n)) {
          if (j == i || A[i, j] == 0) next
          lk[z[j]] <- lk[z[j]] + A[i, j]
          has[z[j]] <- TRUE
        }
        best_c <- ci
        best_g <- (if (has[ci]) lk[ci] else 0) - resolution * ktot[ci] * k[i] / m2
        for (cc in seq_len(n)) {
          if (!has[cc]) next
          g <- lk[cc] - resolution * ktot[cc] * k[i] / m2
          if (g > best_g + 1e-12) { best_g <- g; best_c <- cc }
        }
        z[i] <- best_c
        ktot[best_c] <- ktot[best_c] + k[i]
        if (best_c != ci) moved <- TRUE
      }
    }
    labs <- sort(unique(z))
    remap <- integer(n)
    remap[labs] <- seq_along(labs)
    z <- remap[z]
    K <- length(labs)
    member <- z[member]
    if (K == n) break
    B <- matrix(0, K, K)
    for (i in seq_len(n)) for (j in seq_len(n))
      B[z[i], z[j]] <- B[z[i], z[j]] + A[i, j]
    A <- B
  }
  labs <- sort(unique(member))
  remap <- integer(max(labs))
  remap[labs] <- seq_along(labs)
  member <- remap[member]
  K <- length(labs)
  counts <- vapply(seq_len(K), function(c) sum(member == c), 0L)
  Q <- .lou_modularity(A0, member, resolution)
  .t1_result(estimate = Q, z = member - 1L, counts = counts,
             n_communities = K, Q = Q, n = n0,
             method = "Louvain greedy modularity communities")
}
