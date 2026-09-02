# SPDX-License-Identifier: AGPL-3.0-or-later

#' Pitman-Yor two-parameter process
#'
#' Formula: P(new) = (alpha + sigma K) / (n + alpha)
#'
#' An occupied table takes (n_k - sigma)/(n + alpha), so the discount
#' sigma moves mass from large tables to new ones and the number of
#' blocks grows like n^sigma instead of log n.  Setting sigma = 0
#' recovers the Dirichlet process exactly.
#'
#' @param n Number of customers to seat.
#' @param alpha Concentration; must exceed -sigma.
#' @param sigma Discount in [0, 1).
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate} (number of blocks), \code{K},
#'   \code{counts}, \code{p_new}, \code{n}, \code{method}.
#' @references Pitman & Yor (1997), Ann. Probab. 25(2):855-900.
#' @export
#' @examples
#' Dpparit()
Dpparit <- function(n = 100, alpha = 1, sigma = 0.5, seed = 42) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1")
  if (!(sigma >= 0 && sigma < 1)) stop("sigma must lie in [0, 1)")
  if (!(alpha > -sigma)) stop("alpha must exceed -sigma")
  e <- .ghc_rng(seed)
  counts <- c(1L)
  if (n > 1L) for (i in seq_len(n - 1L)) {
    K <- length(counts)
    w <- c((counts - sigma) / (i + alpha), (alpha + sigma * K) / (i + alpha))
    u <- .ghc_unif(e, 1L)
    acc <- 0
    pick <- K + 1L
    for (c in seq_len(K + 1L)) {
      acc <- acc + w[c]
      if (u <= acc) { pick <- c; break }
    }
    if (pick == K + 1L) counts <- c(counts, 0L)
    counts[pick] <- counts[pick] + 1L
  }
  K <- length(counts)
  .t1_result(estimate = K, K = K, counts = counts,
             p_new = (alpha + sigma * K) / (n + alpha), n = n,
             method = "Pitman-Yor two-parameter seating process")
}
