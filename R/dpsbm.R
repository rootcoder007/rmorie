# SPDX-License-Identifier: AGPL-3.0-or-later

#' .sbm_log_beta
#'
#' A step of the dpsbm implementation. Called by \code{Dpsbm}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.sbm_log_beta <- function(a, b) lgamma(a) + lgamma(b) - lgamma(a + b)

#' Dirichlet-process stochastic block model
#'
#' Formula: DP prior on community labels; Bernoulli edges
#'
#' The block-to-block edge probabilities carry independent Beta(1,1)
#' priors and are integrated out, so a block pair (r, s) with e edges out
#' of t possible contributes B(1 + e, 1 + t - e) to the collapsed
#' likelihood.  A node is then reassigned by the CRP prior times the
#' change in that likelihood, with the number of blocks learned rather
#' than fixed.
#'
#' @param adjacency An n x n symmetric 0/1 adjacency matrix; the
#'   diagonal is ignored.
#' @param alpha Concentration, strictly positive.
#' @param n_iter Number of sweeps.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate} (number of blocks), \code{z},
#'   \code{counts}, \code{n_blocks}, \code{log_likelihood}, \code{n},
#'   \code{method}.
#' @references Kemp, Tenenbaum, Griffiths, Yamada & Ueda (2006),
#'   AAAI-06:381-388.
#' @export
#' @examples
#' Dpsbm(adjacency = 5L)
Dpsbm <- function(adjacency, alpha = 1, n_iter = 30, seed = 42) {
  A <- .s03mat(adjacency)
  n <- nrow(A)
  if (n == 0L) stop("empty input: adjacency has no rows")
  if (ncol(A) != n) stop("adjacency must be square")
  if (!(alpha > 0)) stop("alpha must be strictly positive")
  block_ll <- function(z, K) {
    e <- matrix(0, K, K)
    tt <- matrix(0, K, K)
    for (i in seq_len(n)) for (j in seq_len(n)) {
      if (i == j) next
      a <- z[i]
      b <- z[j]
      tt[a, b] <- tt[a, b] + 1
      if (A[i, j] > 0.5) e[a, b] <- e[a, b] + 1
    }
    s <- 0
    for (r in seq_len(K)) for (c in seq_len(K))
      if (tt[r, c] > 0)
        s <- s + .sbm_log_beta(1 + e[r, c], 1 + tt[r, c] - e[r, c])
    s
  }
  ee <- .ghc_rng(seed)
  z <- rep(1L, n)
  K <- 1L
  for (it in seq_len(as.integer(n_iter))) {
    for (i in seq_len(n)) {
      counts <- vapply(seq_len(K), function(c) sum(z[-i] == c), 0L)
      logw <- c()
      cand <- c()
      for (c in seq_len(K)) {
        if (counts[c] == 0L) next
        zz <- z
        zz[i] <- c
        logw <- c(logw, log(counts[c]) + block_ll(zz, K))
        cand <- c(cand, c)
      }
      zz <- z
      zz[i] <- K + 1L
      logw <- c(logw, log(alpha) + block_ll(zz, K + 1L))
      cand <- c(cand, K + 1L)
      mx <- max(logw)
      w <- exp(logw - mx)
      tot <- 0
      for (v in w) tot <- tot + v
      u <- .ghc_unif(ee, 1L) * tot
      acc <- 0
      pick <- cand[length(cand)]
      for (q in seq_along(w)) {
        acc <- acc + w[q]
        if (u <= acc) { pick <- cand[q]
        break }
      }
      z[i] <- pick
      if (pick == K + 1L) K <- K + 1L
      used <- sort(unique(z))
      remap <- integer(max(used))
      remap[used] <- seq_along(used)
      z <- remap[z]
      K <- length(used)
    }
  }
  counts <- vapply(seq_len(K), function(c) sum(z == c), 0L)
  .t1_result(estimate = K, z = z - 1L, counts = counts, n_blocks = K,
             log_likelihood = block_ll(z, K), n = n,
             method = "Dirichlet-process stochastic block model")
}
