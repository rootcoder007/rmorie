# SPDX-License-Identifier: AGPL-3.0-or-later
#' Take the worst direction rather than the average one
#'
#' Averaging over directions wastes most of the budget on slices where the
#' two clouds already agree; in high dimension almost every random
#' direction is such a slice, and the sliced distance decays. Keeping only
#' the maximising direction gives a sharper discrepancy and, as a
#' by-product, the direction itself.
#'
#' Formula: \code{max_theta W_p(P_theta mu, P_theta nu)} over unit
#' directions -- Deshpande et al. (2019) eq. (3).
#'
#' @param X,Y Two point clouds with the same number of points.
#' @param p Exponent, positive.
#' @param n_proj Number of candidate directions searched.
#' @return List with \code{MSW}, \code{theta_star}, \code{idx_star},
#'   \code{per_proj}, \code{n}, \code{d}, \code{n_proj}.
#' @references Deshpande, I. et al. (2019). Proceedings of the IEEE
#'   Conference on Computer Vision and Pattern Recognition, 10640-10648.
#'   \doi{10.1109/CVPR.2019.01090}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Otmsw(V, V)
Otmsw <- function(X, Y, p = 2, n_proj = 32) {
  A <- as.matrix(X)
  B <- as.matrix(Y)
  if (nrow(A) != nrow(B))
    stop("max-sliced W_p needs clouds with equal point counts")
  if (ncol(A) != ncol(B)) stop("point clouds must share a dimension")
  d <- ncol(A)
  pp <- as.numeric(p)
  L <- as.integer(n_proj)
  TH <- .ot_directions(d, L)
  per <- vapply(seq_len(L), function(k)
    .ot_wp1d(.ot_project(A, TH[k, ]), .ot_project(B, TH[k, ]), pp), 0)
  best <- which.max(per)
  .t1_result(MSW = per[best], theta_star = TH[best, ], idx_star = best - 1L,
             per_proj = per, n = nrow(A), d = d, n_proj = L,
             method = "Max-sliced Wasserstein distance")
}
