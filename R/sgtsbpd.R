# SPDX-License-Identifier: AGPL-3.0-or-later
#' Adjacency spectral radius
#'
#' Largest eigenvalue in modulus of the symmetrised adjacency matrix; by
#' Perron-Frobenius it is simple, positive and bracketed by the mean and
#' maximum degree, both of which are returned.  K_n has spectral radius n - 1.
#' Source consulted: Horn and Johnson (2013), Matrix Analysis 2nd ed, ch. 8.
#'
#' @param A symmetric adjacency (or weight) matrix.
#' @return list: estimate, eigenvalues, perron_vector, mean_degree,
#'   max_degree, n, method.
#' @keywords internal
#' @examples
#' sgtsbpd(matrix(1, 4, 4) - diag(4))$estimate
#' @export
sgtsbpd <- function(A) {
  m <- as.matrix(A); dimnames(m) <- NULL
  m <- 0.5 * (m + t(m))
  e <- eigen(m, symmetric = TRUE)
  w <- rev(e$values); v <- e$vectors[, rev(seq_len(ncol(e$vectors))), drop = FALSE]
  j <- which.max(abs(w))
  vec <- v[, j]
  if (sum(vec) < 0) vec <- -vec
  deg <- rowSums(m)
  list(estimate = abs(w[j]), eigenvalues = sort(w), perron_vector = vec,
       mean_degree = mean(deg), max_degree = max(deg), n = nrow(m),
       method = "Adjacency spectral radius, Perron-Frobenius (Horn & Johnson 2013, ch. 8)")
}

# CANONICAL TEST
# stopifnot(abs(sgtsbpd(matrix(1,4,4)-diag(4))$estimate - 3) < 1e-12)

#' @rdname sgtsbpd
#' @keywords internal
#' @export
morie_sgtsbpd <- sgtsbpd
