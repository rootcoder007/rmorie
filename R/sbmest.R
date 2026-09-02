# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stochastic blockmodel probabilities and log-likelihood
#'
#' Given a partition, edges are independent Bernoulli draws with a probability
#' depending only on the pair of blocks, so the MLE is the observed density of
#' each block pair and the log-likelihood sums the Bernoulli terms over
#' unordered node pairs.  Source consulted: Holland, Laskey and Leinhardt
#' (1983), Social Networks 5(2), 109-137.
#'
#' @param A symmetric binary adjacency matrix.
#' @param blocks block label per node.
#' @return list: estimate, probabilities, edge_counts, pair_counts,
#'   block_sizes, n_blocks, n, method.
#' @keywords internal
#' @examples
#' sbmest(matrix(c(0,1,1,0), 2, 2), c(1, 1))$block_sizes
#' @export
sbmest <- function(A, blocks) {
  a <- as.matrix(A)
  dimnames(a) <- NULL
  n <- nrow(a)
  lab <- as.character(blocks)
  keys <- unique(lab)
  b <- length(keys)
  idx <- match(lab, keys)
  e <- matrix(0, b, b)
  np <- matrix(0, b, b)
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    r <- idx[i]
    s <- idx[j]
    e[r, s] <- e[r, s] + a[i, j]
    np[r, s] <- np[r, s] + 1
    if (r != s) { e[s, r] <- e[s, r] + a[i, j]
    np[s, r] <- np[s, r] + 1 }
  }
  p <- ifelse(np > 0, e / np, 0)
  ll <- 0
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    pr <- p[idx[i], idx[j]]
    y <- a[i, j]
    if (pr > 0 && pr < 1) ll <- ll + y * log(pr) + (1 - y) * log(1 - pr)
  }
  sizes <- as.integer(tabulate(idx, nbins = b))
  list(estimate = ll, probabilities = p, edge_counts = e, pair_counts = np,
       block_sizes = sizes, n_blocks = as.integer(b), n = n,
       method = "Stochastic blockmodel MLE (Holland, Laskey & Leinhardt 1983)")
}

# CANONICAL TEST
# A <- matrix(0,6,6); E <- rbind(c(1,2),c(1,3),c(2,3),c(3,4),c(4,5),c(4,6),c(5,6))
# for (i in seq_len(nrow(E))) { A[E[i,1],E[i,2]] <- 1; A[E[i,2],E[i,1]] <- 1 }
# r <- sbmest(A, c(0,0,0,1,1,1)); stopifnot(abs(r$probabilities[1,1] - 1) < 1e-15)

#' @rdname sbmest
#' @keywords internal
#' @export
morie_sbmest <- sbmest
