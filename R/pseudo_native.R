# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Path-specific effects in linear SEMs (Pseudo). Bit-identical mirror
# of src/morie/fn/pseudo.py.

#' Path-specific effect of x on y in a linear SEM
#'
#' Avin, Shpitser and Pearl (2005) define the g-specific effect as the
#' effect transmitted along a chosen subgraph g of edges with all
#' other edges turned off; in linear systems these path-restricted
#' effects are sums of products of path coefficients along the
#' retained paths (Pearl 2001, Section 2). For an acyclic coefficient
#' matrix B with B\[i, j\] the coefficient on the edge from i to j, the
#' unit-change effect along g is entry (x, y) of
#' \eqn{(I - B_g)^{-1} = I + B_g + B_g^2 + \dots}
#' where B_g zeroes every edge outside g. The full graph gives the
#' total effect; the single edge from x to y gives the direct effect;
#' total minus direct is the indirect effect. In linear models natural
#' and controlled path-specific effects coincide.
#'
#' @param B Square acyclic coefficient matrix.
#' @param x Cause index (1-based).
#' @param y Outcome index (1-based).
#' @param edges Optional list of retained edges, each a length-2
#'   vector of 1-based indices c(i, j); default all edges.
#' @return List with \code{estimate}, \code{total}, \code{direct},
#'   \code{indirect}, \code{n_edges_used}, \code{method}.
#' @references Avin, C., Shpitser, I. and Pearl, J. (2005),
#'   Identifiability of path-specific effects, Proc. 19th IJCAI,
#'   357-363, Section 2; local copy
#'   fetched-wave3/avin-shpitser-pearl-2005-path-specific-effects-IJCAI.pdf.
#'   Pearl, J. (2001), Direct and indirect effects, Proc. 17th UAI,
#'   411-420, Section 2; local copy
#'   fetched-wave3/pearl-2001-direct-indirect-effects-UAI.pdf.
#' @export
Pseudo <- function(B, x, y, edges = NULL) {
  Bm <- as.matrix(B)
  storage.mode(Bm) <- "double"
  if (nrow(Bm) != ncol(Bm)) stop("B must be a square matrix", call. = FALSE)
  k <- nrow(Bm)
  x <- as.integer(x)
  y <- as.integer(y)
  if (x < 1L || x > k || y < 1L || y > k || x == y) {
    stop("x and y must be distinct indices into B", call. = FALSE)
  }
  P <- diag(k)
  for (i in seq_len(k)) P <- P %*% Bm
  if (max(abs(P)) > 0) stop("B is not acyclic (B^k != 0)", call. = FALSE)
  path_sum <- function(A) {
    Tm <- diag(k)
    Pm <- diag(k)
    for (i in seq_len(k)) {
      Pm <- Pm %*% A
      Tm <- Tm + Pm
    }
    Tm
  }
  if (is.null(edges)) {
    Bg <- Bm
    used <- sum(Bm != 0)
  } else {
    Bg <- matrix(0, k, k)
    used <- 0L
    for (e in edges) {
      i <- as.integer(e[[1]])
      j <- as.integer(e[[2]])
      if (i < 1L || i > k || j < 1L || j > k) {
        stop(sprintf("edge (%d, %d) out of range", i, j), call. = FALSE)
      }
      Bg[i, j] <- Bm[i, j]
      used <- used + 1L
    }
  }
  T_g <- path_sum(Bg)
  T_full <- path_sum(Bm)
  direct <- Bm[x, y]
  total <- T_full[x, y]
  list(estimate = T_g[x, y], total = total, direct = direct,
       indirect = total - direct, n_edges_used = as.integer(used),
       method = "Avin-Shpitser-Pearl (2005) path-specific effect, linear path rule")
}
