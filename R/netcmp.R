# SPDX-License-Identifier: AGPL-3.0-or-later
#' Graph comparison by a choice of kernel
#'
#' Shervashidze et al. (2011), JMLR 12, 2539-2561 (FETCHED), for the WL
#' subtree kernel and its survey of alternatives; Gaertner, Flach and
#' Wrobel (2003), On graph kernels: hardness results and efficient
#' alternatives, COLT/Kernel 2777, 129-143, for the geometric random-walk
#' kernel k_RW = sum_ij \[(I - lambda A_x)^-1\]_ij on the direct product
#' graph, which converges for lambda below the reciprocal of the largest
#' eigenvalue of A_x.  The 2003 COLT paper is paywalled; the closed form
#' is quoted in its standard published form and restated in section 2 of
#' Shervashidze et al.  All three kernels are normalised to a cosine so
#' the value is comparable across graph sizes.
#'
#' @param G1,G2 adjacency matrices.
#' @param kernel "wl", "graphlet" or "rw".
#' @param h WL iterations.
#' @param k_size graphlet size.
#' @param lam random-walk decay.
#' @return list: estimate, raw, k11, k22, kernel, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' Graphcmp(A, A)$estimate
#' @export
Graphcmp <- function(G1, G2, kernel = "wl", h = 3, k_size = 3, lam = 0.1) {
  rw <- function(a, b) {
    A <- .s03mat(a)
    B <- .s03mat(b)
    n <- nrow(A)
    m <- nrow(B)
    N <- n * m
    M <- matrix(0, N, N)
    for (i in seq_len(n)) for (j in seq_len(m)) for (p in seq_len(n)) for (q in seq_len(m)) {
      M[(i - 1L) * m + j, (p - 1L) * m + q] <- -lam * A[i, p] * B[j, q]
    }
    for (i in seq_len(N)) M[i, i] <- M[i, i] + 1
    x <- .s03ridgesolve(M, rep(1, N), 1e-12)
    s <- 0
    for (v in x) s <- s + v
    s
  }
  f <- if (identical(kernel, "graphlet")) {
    function(a, b) Graphlet(a, b, k_size, TRUE)$estimate
  } else if (identical(kernel, "rw")) {
    rw
  } else {
    function(a, b) Wlkernel(a, b, h)$estimate
  }
  raw <- f(G1, G2)
  k11 <- f(G1, G1)
  k22 <- f(G2, G2)
  d <- if (k11 > 0 && k22 > 0) sqrt(k11 * k22) else 0
  list(estimate = if (d > 0) raw / d else NaN, raw = raw, k11 = k11,
       k22 = k22, kernel = kernel,
       method = "Cosine-normalised graph kernel (WL, graphlet, or geometric random walk)")
}
