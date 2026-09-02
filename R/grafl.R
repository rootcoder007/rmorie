# SPDX-License-Identifier: AGPL-3.0-or-later
#' Graphlet kernel
#'
#' Shervashidze, Vishwanathan, Petri, Mehlhorn and Borgwardt (2009),
#' Efficient graphlet kernels for large graph comparison, AISTATS 5,
#' 488-495, define the graphlet kernel as the inner product of the
#' normalised counts of all size-k induced subgraphs, k(G, G') = <f_G,
#' f_G'> with f_G the type frequencies over C(n, k) subsets.  Shervashidze
#' et al. (2011), JMLR 12, 2539-2561 (FETCHED), restates the construction.
#' The 2009 AISTATS volume was not retrievable here; the kernel is quoted
#' in its standard published form.  Graphlet types are identified by a
#' canonical signature -- edge count plus sorted degree sequence -- which
#' separates every isomorphism class for k = 3 and k = 4, so no
#' isomorphism test is needed.
#'
#' @param G1,G2 adjacency matrices.
#' @param k_size the graphlet size.
#' @param normalize divide the counts by C(n, k).
#' @return list: estimate, types, f1, f2, n_types, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' Graphlet(A, A, 3)$estimate
#' @export
Graphlet <- function(G1, G2, k_size = 3, normalize = TRUE) {
  types <- character(0)
  sig <- function(A, idx) {
    m <- 0L
    deg <- integer(length(idx))
    for (a in seq_along(idx)) {
      if (a < length(idx)) for (b in seq(a + 1L, length(idx))) {
        if (A[idx[a], idx[b]] != 0) {
          m <- m + 1L
          deg[a] <- deg[a] + 1L
          deg[b] <- deg[b] + 1L
        }
      }
    }
    paste0(m, ":", paste(sort(deg), collapse = ","))
  }
  counts <- function(G, kk) {
    A <- .s03mat(G)
    n <- nrow(A)
    cn <- character(0)
    cv <- numeric(0)
    idx <- seq_len(kk)
    repeat {
      s <- sig(A, idx)
      if (is.na(match(s, types))) types[[length(types) + 1L]] <<- s
      i <- match(s, cn)
      if (is.na(i)) { cn <- c(cn, s)
      cv <- c(cv, 1) } else cv[i] <- cv[i] + 1
      i <- kk
      while (i >= 1L && idx[i] == n - kk + i) i <- i - 1L
      if (i < 1L) break
      idx[i] <- idx[i] + 1L
      if (i < kk) for (j in seq(i + 1L, kk)) idx[j] <- idx[j - 1L] + 1L
    }
    tot <- if (n >= kk) choose(n, kk) else 0
    list(cn = cn, cv = cv, tot = as.numeric(tot))
  }
  kk <- as.integer(k_size)
  a1 <- counts(G1, kk)
  a2 <- counts(G2, kk)
  getc <- function(a, s) { i <- match(s, a$cn)
  if (is.na(i)) 0 else a$cv[i] }
  f1 <- numeric(length(types))
  f2 <- numeric(length(types))
  for (t in seq_along(types)) {
    f1[t] <- if (normalize && a1$tot > 0) getc(a1, types[t]) / a1$tot else getc(a1, types[t])
    f2[t] <- if (normalize && a2$tot > 0) getc(a2, types[t]) / a2$tot else getc(a2, types[t])
  }
  dot <- 0
  for (t in seq_along(types)) dot <- dot + f1[t] * f2[t]
  list(estimate = dot, types = types, f1 = f1, f2 = f2,
       n_types = length(types),
       method = "Graphlet kernel on size-k induced subgraphs (Shervashidze et al. 2009)")
}
