# SPDX-License-Identifier: AGPL-3.0-or-later
#' Average the one-dimensional distance over many directions
#'
#' Projecting to a line makes the transport problem a sort, so a sliced
#' distance costs \code{O(L n log n)} instead of the cubic price of the
#' linear program, and it still metrises weak convergence. The directions
#' are a van der Corput / AS 241 sequence rather than a pseudo-random one,
#' so the value is reproducible across arms and across runs.
#'
#' Formula: \code{SW_p(mu,nu) = (E_theta [W_p(P_theta mu, P_theta
#' nu)^p])^(1/p)} -- Bonneel et al. (2015) eq. (5); Peyre and Cuturi
#' (2019) eq. (10.13), p. 166.
#'
#' @param X,Y Two point clouds with the same number of points.
#' @param p Exponent, positive.
#' @param n_proj Number of directions.
#' @return List with \code{SW}, \code{SW_p}, \code{per_proj}, \code{n},
#'   \code{d}, \code{n_proj}.
#' @references Bonneel, N., Rabin, J., Peyre, G. and Pfister, H. (2015).
#'   Journal of Mathematical Imaging and Vision 51(1):22-45.
#'   \doi{10.1007/s10851-014-0506-3}.
#' @export
Otsw <- function(X, Y, p = 2, n_proj = 32) {
  A <- as.matrix(X); B <- as.matrix(Y)
  if (nrow(A) != nrow(B))
    stop("sliced W_p needs clouds with equal point counts")
  if (ncol(A) != ncol(B)) stop("point clouds must share a dimension")
  d <- ncol(A); pp <- as.numeric(p); L <- as.integer(n_proj)
  TH <- .ot_directions(d, L)
  per <- vapply(seq_len(L), function(k)
    .ot_wp1d(.ot_project(A, TH[k, ]), .ot_project(B, TH[k, ]), pp)^pp, 0)
  swp <- sum(per) / L
  .t1_result(SW = swp^(1 / pp), SW_p = swp, per_proj = per,
             n = nrow(A), d = d, n_proj = L,
             method = "Sliced Wasserstein distance")
}
