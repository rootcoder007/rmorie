# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sliced distance computed on a shared quantile grid
#'
#' Rabin et al. build their texture-mixing barycenter out of projected
#' quantile functions, which is what lets the two clouds have different
#' sizes: instead of pairing order statistics one for one, both
#' projections are evaluated at a common grid of probabilities. When the
#' clouds do have the same size the grid reproduces the order statistics
#' exactly, so this agrees with the sorted estimator.
#'
#' Formula: \code{SW_p^p = E_theta int_0^1 |Finv_mu(q) - Finv_nu(q)|^p
#' dq}, on the midpoint grid \code{q_k = (k - 1/2)/G} -- Rabin et al.
#' (2012) Section 3.
#'
#' @param X,Y Point clouds; the counts may differ.
#' @param p Exponent, positive.
#' @param n_proj Number of directions.
#' @return List with \code{SW}, \code{SW_p}, \code{per_proj}, \code{n},
#'   \code{m}, \code{d}, \code{n_proj}, \code{grid_size}.
#' @references Rabin, J., Peyre, G., Delon, J. and Bernot, M. (2012).
#'   Lecture Notes in Computer Science 6667:435-446.
#'   \doi{10.1007/978-3-642-24785-9_37}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Otsd(V, V)
Otsd <- function(X, Y, p = 2, n_proj = 32) {
  A <- as.matrix(X)
  B <- as.matrix(Y)
  if (ncol(A) != ncol(B)) stop("point clouds must share a dimension")
  n <- nrow(A)
  m <- nrow(B)
  d <- ncol(A)
  if (n == 0L || m == 0L) stop("empty point cloud")
  pp <- as.numeric(p)
  if (pp <= 0) stop("p must be positive")
  L <- as.integer(n_proj)
  G <- max(n, m)
  grid <- (seq_len(G) - 0.5) / G
  TH <- .ot_directions(d, L)
  per <- vapply(seq_len(L), function(k) {
    qx <- .ot_quantiles(.ot_project(A, TH[k, ]), grid)
    qy <- .ot_quantiles(.ot_project(B, TH[k, ]), grid)
    sum(abs(qx - qy)^pp) / G
  }, 0)
  swp <- sum(per) / L
  .t1_result(SW = swp^(1 / pp), SW_p = swp, per_proj = per, n = n, m = m,
             d = d, n_proj = L, grid_size = G,
             method = "Quantile-based sliced Wasserstein distance")
}
