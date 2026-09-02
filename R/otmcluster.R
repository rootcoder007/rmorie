# SPDX-License-Identifier: AGPL-3.0-or-later
#' Lloyd's algorithm with transport in place of Euclidean distance
#'
#' Clustering distributions by comparing their moments throws away shape;
#' comparing them bin by bin makes the answer depend on an arbitrary
#' binning. The Wasserstein distance does neither, and the empirical
#' version obeys a central limit theorem, which is what makes the
#' resulting clusters testable rather than merely descriptive. Both the
#' assignment step and the centroid step are transport problems.
#'
#' Formula: alternate \code{label(i) = argmin_c W_2(mu_i, nu_c)} and
#' \code{nu_c = argmin sum W_2^2(mu_i, nu)}; the barycentre step is Cuturi
#' and Doucet's free-support update.
#'
#' @param X_list List of point clouds, all with the same number of points.
#' @param k Number of clusters.
#' @param max_iter Lloyd iterations.
#' @return List with \code{labels}, \code{centers}, \code{inertia},
#'   \code{K}, \code{n_clouds}, \code{n}, \code{d}, \code{iters}.
#' @references del Barrio, E., Cuesta-Albertos, J. A. and Matran, C.
#'   (2019). Annals of Probability 47(2):926-951. \doi{10.1214/18-AOP1275}.
#'   Cuturi, M. and Doucet, A. (2014). Proceedings of Machine Learning
#'   Research 32:685-693 (ICML).
#' @export
#' @examples
#' Otmcluster(X_list = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
Otmcluster <- function(X_list, k, max_iter = 10) {
  clouds <- lapply(X_list, as.matrix)
  N <- length(clouds)
  if (N == 0L) stop("no input clouds")
  n <- nrow(clouds[[1]]); d <- ncol(clouds[[1]])
  for (Xc in clouds) if (nrow(Xc) != n || ncol(Xc) != d)
    stop("all clouds must have the same shape")
  K <- as.integer(k)
  if (K < 1L || K > N)
    stop("k must lie between 1 and the number of clouds")
  u <- rep(1 / n, n)
  centers <- lapply(seq_len(K), function(c) clouds[[c]])
  labels <- integer(N); inertia <- 0
  it <- as.integer(max_iter)
  for (t in seq_len(it)) {
    inertia <- 0
    for (i in seq_len(N)) {
      costs <- vapply(seq_len(K), function(c)
        .ot_emd(u, u, .ot_costmat(centers[[c]], clouds[[i]], 2))$cost, 0)
      bc <- which.min(costs)
      labels[i] <- bc - 1L
      inertia <- inertia + costs[bc]
    }
    for (c in seq_len(K)) {
      mem <- which(labels == (c - 1L))
      if (!length(mem)) next
      w <- rep(1 / length(mem), length(mem))
      Z <- matrix(0, n, d)
      for (tt in seq_along(mem)) {
        i <- mem[tt]
        Tm <- .ot_emd(u, u, .ot_costmat(centers[[c]], clouds[[i]], 2))$T
        Z <- Z + w[tt] * n * (Tm %*% clouds[[i]])
      }
      centers[[c]] <- Z
    }
  }
  .t1_result(labels = labels, centers = centers, inertia = inertia,
             K = K, n_clouds = N, n = n, d = d, iters = it,
             method = "Wasserstein k-means over point clouds")
}
