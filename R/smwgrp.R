# SPDX-License-Identifier: AGPL-3.0-or-later
#' Humphries-Gurney small-world coefficient
#'
#' S = (C/C_rand)/(L/L_rand) with C the mean local clustering coefficient, L
#' the characteristic path length, C_rand = kbar/n and L_rand = log n / log kbar,
#' the paper's analytic Erdos-Renyi reference.  S > 1 is its criterion.  Source
#' consulted: Humphries and Gurney (2008), PLoS ONE 3(4), e0002051, eq. (1)-(4).
#'
#' @param A symmetric binary adjacency matrix.
#' @return list: estimate, clustering, path_length, clustering_random,
#'   path_length_random, mean_degree, n, method.
#' @keywords internal
#' @examples
#' smwgrp(matrix(c(0,1,1,1,0,1,1,1,0), 3, 3))$clustering
#' @export
smwgrp <- function(A) {
  a <- as.matrix(A)
  dimnames(a) <- NULL
  n <- nrow(a)
  deg <- vapply(seq_len(n), function(i) sum(a[i, -i] != 0), numeric(1))
  cl <- numeric(n)
  for (i in seq_len(n)) {
    nb <- which(a[i, ] != 0 & seq_len(n) != i)
    k <- length(nb)
    if (k < 2L) { cl[i] <- 0
    next }
    links <- 0
    for (p in seq_len(k - 1L)) for (q in (p + 1L):k) if (a[nb[p], nb[q]] != 0) links <- links + 1
    cl[i] <- 2 * links / (k * (k - 1))
  }
  cbar <- mean(cl)
  d <- k02bfs(a)
  tot <- 0
  cnt <- 0L
  for (i in seq_len(n)) for (j in seq_len(n))
    if (i != j && d[i, j] >= 0L) { tot <- tot + d[i, j]
    cnt <- cnt + 1L }
  lbar <- if (cnt > 0L) tot / cnt else NA_real_
  kbar <- mean(deg)
  crand <- kbar / n
  lrand <- if (kbar > 1) log(n) / log(kbar) else NA_real_
  list(estimate = (cbar / crand) / (lbar / lrand), clustering = cbar,
       path_length = lbar, clustering_random = crand, path_length_random = lrand,
       mean_degree = kbar, n = n,
       method = "Small-world coefficient S (Humphries & Gurney 2008, eq. 1-4)")
}

# CANONICAL TEST
# A <- matrix(0,6,6); E <- rbind(c(1,2),c(1,3),c(2,3),c(3,4),c(4,5),c(4,6),c(5,6))
# for (i in seq_len(nrow(E))) { A[E[i,1],E[i,2]] <- 1; A[E[i,2],E[i,1]] <- 1 }
# stopifnot(abs(smwgrp(A)$path_length - 1.8) < 1e-12)

#' @rdname smwgrp
#' @keywords internal
#' @export
morie_smwgrp <- smwgrp
