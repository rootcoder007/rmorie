# SPDX-License-Identifier: AGPL-3.0-or-later

#' Erdos-Renyi G(n,p)
#'
#' Formula: each of the C(n,2) possible edges is present independently
#' with probability p.  Realised on a DETERMINISTIC low-discrepancy
#' stream -- edge slot k is compared against p using van der Corput base
#' \code{PRIMES[k mod 12]}, so successive dyads are not correlated the
#' way one shared stream would make them, and both language arms build
#' the identical graph.
#'
#' PROVENANCE: the binomial model G(n,p) is Gilbert (1959).  Erdos &
#' Renyi (1959) defined G(n,M), the uniform model on graphs with exactly
#' M edges; the two are asymptotically equivalent but not the same
#' construction, and the usual name for G(n,p) is a misattribution.
#'
#' Alongside the realised graph the exact analytic quantities are
#' returned: E[edges] = C(n,2) p, E[degree] = (n-1) p, the connectivity
#' threshold log(n)/n and the giant-component threshold 1/n.
#'
#' @param n Number of vertices (>= 1).
#' @param p Edge probability in [0, 1].
#' @return List with \code{estimate}, \code{edges}, \code{density},
#'   \code{expected_edges}, \code{mean_degree}, \code{expected_degree},
#'   \code{n_components}, \code{largest_component},
#'   \code{giant_threshold}, \code{connectivity_threshold}, \code{n},
#'   \code{method}.
#' @references Gilbert (1959), Ann. Math. Statist. 30(4):1141-1144,
#'   doi:10.1214/aoms/1177706098; Erdos & Renyi (1959), Publ. Math.
#'   Debrecen 6:290-297 (the G(n,M) model).
#' @export
Erdosg <- function(n, p) {
  PR <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
  .uu <- function(k) .s03vdc(k %/% 12L + 1L, PR[(k %% 12L) + 1L])
  n <- as.integer(n); p <- as.numeric(p)
  if (n < 1L) stop("n must be positive")
  if (!(p >= 0 && p <= 1)) stop("p must lie in [0, 1]")
  m <- (n * (n - 1L)) %/% 2L
  adj <- matrix(0L, n, n)
  k <- 0L; edges <- 0L
  if (n > 1L) for (i in seq_len(n)) {
    if (i < n) for (j in (i + 1L):n) {
      if (.uu(k) < p) {
        adj[i, j] <- 1L; adj[j, i] <- 1L; edges <- edges + 1L
      }
      k <- k + 1L
    }
  }
  deg <- rowSums(adj)
  seen <- rep(FALSE, n); comps <- integer(0)
  for (s in seq_len(n)) {
    if (seen[s]) next
    seen[s] <- TRUE
    q <- s; size <- 0L
    while (length(q)) {
      v <- q[1]; q <- q[-1]
      size <- size + 1L
      for (w in seq_len(n)) if (adj[v, w] == 1L && !seen[w]) {
        seen[w] <- TRUE; q <- c(q, w)
      }
    }
    comps <- c(comps, size)
  }
  dens <- if (m > 0L) edges / m else 0
  .t1_result(estimate = dens, edges = edges, density = dens,
             expected_edges = m * p, mean_degree = sum(deg) / n,
             expected_degree = (n - 1) * p, n_components = length(comps),
             largest_component = max(comps), giant_threshold = 1 / n,
             connectivity_threshold = if (n > 1L) log(n) / n else 0,
             n = n, method = "Erdos-Renyi G(n,p)")
}
