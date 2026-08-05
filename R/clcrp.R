# SPDX-License-Identifier: AGPL-3.0-or-later

#' Distance-dependent CRP
#'
#' Formula: P(c_i = j) proportional to f(d_ij) for j != i, alpha for j = i
#'
#' Customers link to other CUSTOMERS rather than to tables, and the
#' clusters are the connected components of the link graph.  With the
#' exponential decay f(d) = exp(-d/decay), a decay near zero makes every
#' customer link to itself and gives n singleton clusters, while a very
#' large decay makes every link equally likely and collapses the
#' partition.  Unlike the ordinary CRP the induced partition is not
#' exchangeable, which is the point: distance carries information.
#'
#' @param y Observations, used only for the reported cluster means.
#' @param distances An n x n matrix of pairwise distances.
#' @param alpha Self-link weight, strictly positive.
#' @param decay Scale of the exponential decay, strictly positive.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate} (number of clusters), \code{z},
#'   \code{links}, \code{counts}, \code{cluster_mean},
#'   \code{n_clusters}, \code{n}, \code{method}.
#' @references Blei & Frazier (2011), J. Machine Learning Research
#'   12:2461-2488.
#' @export
Clcrp <- function(y, distances, alpha = 1, decay = 1, seed = 42) {
  y <- .s03vec(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  D <- .s03mat(distances)
  if (nrow(D) != n || ncol(D) != n)
    stop("distances must be an n x n matrix")
  if (!(alpha > 0)) stop("alpha must be strictly positive")
  if (!(decay > 0)) stop("decay must be strictly positive")
  e <- .ghc_rng(seed)
  links <- integer(n)
  for (i in seq_len(n)) {
    w <- numeric(n)
    for (j in seq_len(n)) w[j] <- if (j == i) alpha else exp(-D[i, j] / decay)
    tot <- 0
    for (v in w) tot <- tot + v
    u <- .ghc_unif(e, 1L) * tot
    acc <- 0
    pick <- n
    for (j in seq_len(n)) {
      acc <- acc + w[j]
      if (u <= acc) { pick <- j; break }
    }
    links[i] <- pick
  }
  parent <- seq_len(n)
  find <- function(a) {
    while (parent[a] != a) {
      parent[a] <<- parent[parent[a]]
      a <- parent[a]
    }
    a
  }
  for (i in seq_len(n)) {
    ra <- find(i); rb <- find(links[i])
    if (ra != rb) parent[max(ra, rb)] <- min(ra, rb)
  }
  roots <- c(); z <- integer(n)
  for (i in seq_len(n)) {
    r <- find(i)
    if (!(r %in% roots)) roots <- c(roots, r)
    z[i] <- which(roots == r)[1]
  }
  K <- length(roots)
  counts <- vapply(seq_len(K), function(c) sum(z == c), 0L)
  means <- vapply(seq_len(K), function(c) sum(y[z == c]) / counts[c], 0)
  .t1_result(estimate = K, z = z - 1L, links = links - 1L, counts = counts,
             cluster_mean = means, n_clusters = K, n = n,
             method = "distance-dependent Chinese restaurant process")
}
