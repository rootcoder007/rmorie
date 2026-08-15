# SPDX-License-Identifier: AGPL-3.0-or-later
#' Degree-corrected stochastic blockmodel objective
#'
#' L = sum_rs m_rs log(m_rs / (kappa_r kappa_s)) with m_rs the edges between
#' blocks (m_rr counted twice) and kappa_r the degree sum of block r.  Because
#' degrees are held fixed the objective rewards blocks denser than their own
#' degree sequence predicts; the uncorrected objective is returned alongside so
#' the two can be compared.  Source consulted: Karrer and Newman (2011),
#' Physical Review E 83, 016107, equation (16).
#'
#' @param A symmetric adjacency matrix.
#' @param blocks block label per node.
#' @return list: estimate, uncorrected, m_rs, kappa, block_sizes, n_blocks,
#'   n_edges, n, method.
#' @keywords internal
#' @examples
#' sbmdg2(matrix(c(0,1,1,0), 2, 2), c(1, 1))$n_edges
#' @export
sbmdg2 <- function(A, blocks) {
  a <- as.matrix(A); dimnames(a) <- NULL
  n <- nrow(a)
  lab <- as.character(blocks)
  keys <- unique(lab); b <- length(keys); idx <- match(lab, keys)
  m <- matrix(0, b, b)
  for (i in seq_len(n)) for (j in seq_len(n)) m[idx[i], idx[j]] <- m[idx[i], idx[j]] + a[i, j]
  deg <- rowSums(a)
  kappa <- numeric(b)
  for (i in seq_len(n)) kappa[idx[i]] <- kappa[idx[i]] + deg[i]
  sizes <- as.integer(tabulate(idx, nbins = b))
  ll <- 0
  for (r in seq_len(b)) for (s in seq_len(b))
    if (m[r, s] > 0 && kappa[r] > 0 && kappa[s] > 0)
      ll <- ll + m[r, s] * log(m[r, s] / (kappa[r] * kappa[s]))
  unc <- 0
  for (r in seq_len(b)) for (s in seq_len(b)) {
    nn <- sizes[r] * sizes[s]
    if (m[r, s] > 0 && nn > 0) unc <- unc + m[r, s] * log(m[r, s] / nn)
  }
  list(estimate = ll, uncorrected = unc, m_rs = m, kappa = kappa,
       block_sizes = sizes, n_blocks = as.integer(b), n_edges = sum(a) / 2,
       n = n,
       method = "Degree-corrected stochastic blockmodel objective (Karrer & Newman 2011, eq. 16)")
}

# CANONICAL TEST
# A <- matrix(0,6,6); E <- rbind(c(1,2),c(1,3),c(2,3),c(3,4),c(4,5),c(4,6),c(5,6))
# for (i in seq_len(nrow(E))) { A[E[i,1],E[i,2]] <- 1; A[E[i,2],E[i,1]] <- 1 }
# stopifnot(sbmdg2(A, c(0,0,0,1,1,1))$estimate > sbmdg2(A, c(0,1,0,1,0,1))$estimate)

#' @rdname sbmdg2
#' @keywords internal
#' @export
morie_sbmdg2 <- sbmdg2
