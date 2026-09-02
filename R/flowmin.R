# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stoer-Wagner global minimum cut
#'
#' Stoer and Wagner (1997), A simple min-cut algorithm, Journal of the ACM
#' 44(4), 585-591.  The algorithm repeats a minimum cut phase: grow a set
#' A from a start vertex, always adding the vertex most tightly connected
#' to A; the last two vertices added, s and t, give a cut-of-the-phase
#' whose weight is that of the cut separating t from the rest, and s and t
#' are then merged.  After n - 1 phases the lightest cut-of-the-phase is
#' the global minimum cut.  The JACM paper is paywalled; the phase
#' construction and the merge are quoted in their standard published form.
#'
#' Determinism: the paper's "arbitrary" start vertex and its tie-breaking
#' are fixed -- the phase starts at the lowest surviving index and ties
#' break to the lowest index -- so the cut and the partition reproduce
#' exactly in both arms.
#'
#' @param A symmetric weight matrix.
#' @return list: estimate, weight, partition, phases, n, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 3, 0, 3, 0, 1, 0, 1, 0), 3, 3)
#' Mincutsw(A)$weight
#' @export
Mincutsw <- function(A) {
  W <- .s03mat(A)
  n <- nrow(W)
  w <- W
  groups <- vector("list", n)
  for (i in seq_len(n)) groups[[i]] <- i
  alive <- seq_len(n)
  best <- Inf
  bestset <- integer(0)
  phases <- numeric(0)
  while (length(alive) > 1L) {
    m <- length(alive)
    inA <- rep(FALSE, m)
    wsum <- numeric(m)
    ord <- integer(0)
    for (step in seq_len(m)) {
      sel <- -1L
      for (t in seq_len(m)) if (!inA[t] && (sel < 0L || wsum[t] > wsum[sel])) sel <- t
      inA[sel] <- TRUE
      ord <- c(ord, sel)
      for (t in seq_len(m)) if (!inA[t]) wsum[t] <- wsum[t] + w[alive[sel], alive[t]]
    }
    t_i <- ord[m]
    s_i <- ord[m - 1L]
    cut <- 0
    for (t in seq_len(m)) if (t != t_i) cut <- cut + w[alive[t_i], alive[t]]
    phases <- c(phases, cut)
    if (cut < best) { best <- cut
    bestset <- groups[[alive[t_i]]] }
    s <- alive[s_i]
    tt <- alive[t_i]
    for (t in seq_len(n)) {
      w[s, t] <- w[s, t] + w[tt, t]
      w[t, s] <- w[s, t]
    }
    w[s, s] <- 0
    groups[[s]] <- c(groups[[s]], groups[[tt]])
    alive <- alive[alive != tt]
  }
  part <- as.integer(seq_len(n) %in% bestset)
  list(estimate = best, weight = best, partition = part, phases = phases,
       n = n,
       method = "Stoer-Wagner global minimum cut (1997), deterministic start and tie-breaking")
}
