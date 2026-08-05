# SPDX-License-Identifier: AGPL-3.0-or-later
#' Distortion of a codebook, computed as transport to its atoms
#'
#' Pollard's formulation of k-means is exactly a transport problem: the
#' empirical measure is pushed onto a finitely supported one, and the
#' distortion is the transport cost. Because nearest-point assignment is
#' itself an optimal plan for the induced weights, the transport cost and
#' the ordinary mean-squared distortion coincide -- so the two are
#' computed separately here and both reported, which turns the identity
#' into a self-check.
#'
#' Formula: \code{Distortion = OT(mu, nu_quant)} with \code{nu_quant}
#' supported on the centroids and weighted by the assignment proportions;
#' equal to \code{(1/n) sum_i min_k ||x_i - c_k||^2} -- Pollard (1982).
#'
#' @param X Data points, n by d, given equal weight.
#' @param centroids Codebook, K by d.
#' @return List with \code{dist}, \code{dist_assign}, \code{gap},
#'   \code{labels}, \code{weights}, \code{n}, \code{K}, \code{d}.
#' @references Pollard, D. (1982). IEEE Transactions on Information Theory
#'   28(2):199-205. \doi{10.1109/TIT.1982.1056481}.
#' @export
Otmqd <- function(X, centroids) {
  A <- as.matrix(X); Cn <- as.matrix(centroids)
  n <- nrow(A); K <- nrow(Cn); d <- ncol(A)
  if (ncol(Cn) != d)
    stop("centroids must live in the same space as the data")
  if (n == 0L || K == 0L) stop("empty data or codebook")
  C <- .ot_costmat(A, Cn, 2)
  labels <- apply(C, 1, which.min) - 1L
  dist_assign <- mean(C[cbind(seq_len(n), labels + 1L)])
  w <- as.numeric(tabulate(labels + 1L, K)) / n
  r <- .ot_emd(rep(1 / n, n), w, C)
  .t1_result(dist = r$cost, dist_assign = dist_assign,
             gap = abs(r$cost - dist_assign), labels = labels,
             weights = w, n = n, K = K, d = d,
             method = "Quantization distortion as transport cost")
}
