# SPDX-License-Identifier: AGPL-3.0-or-later
#' Graph diffusion (heat) kernel
#'
#' K(beta) = exp(-beta L) with L = D - A, the solution at time beta of
#' dK/dbeta = -L K, K(0) = I.  Taken through the symmetric eigendecomposition
#' of L, which is sign-safe and keeps the kernel positive semi-definite; every
#' row sums to one because L 1 = 0.  Source consulted: Kondor and Lafferty
#' (2002), ICML 2002, 315-322, equation (5).
#'
#' @param A symmetric adjacency (or weight) matrix.
#' @param beta diffusion time.
#' @return list: estimate, kernel, eigenvalues, beta, n, method.
#' @keywords internal
#' @examples
#' rowSums(sgtdiff(matrix(c(0,1,1,0), 2, 2), 0.3)$kernel)
#' @export
sgtdiff <- function(A, beta = 1) {
  m <- as.matrix(A); dimnames(m) <- NULL
  m <- 0.5 * (m + t(m))
  lap <- diag(rowSums(m), nrow = nrow(m)) - m
  e <- eigen(lap, symmetric = TRUE)
  w <- rev(e$values); v <- e$vectors[, rev(seq_len(ncol(e$vectors))), drop = FALSE]
  k <- (v * rep(exp(-beta * w), each = nrow(v))) %*% t(v)
  k <- 0.5 * (k + t(k))
  list(estimate = sum(diag(k)), kernel = k, eigenvalues = w,
       beta = as.numeric(beta), n = nrow(m),
       method = "Graph diffusion (heat) kernel (Kondor & Lafferty 2002, eq. 5)")
}

# CANONICAL TEST
# r <- sgtdiff(matrix(c(0,1,1,0),2,2), 0.3)
# stopifnot(all(abs(rowSums(r$kernel) - 1) < 1e-12),
#           abs(r$kernel[1,1] - (1 + exp(-0.6))/2) < 1e-14)

#' @rdname sgtdiff
#' @keywords internal
#' @export
morie_sgtdiff <- sgtdiff
