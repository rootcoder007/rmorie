# SPDX-License-Identifier: AGPL-3.0-or-later
#' Functional clustering by k-means on B-spline coefficients.
#'
#' Formula: c_i = argmin ||y_i - B c||^2 per curve, then Lloyd k-means on the coefficient vectors
#'
#' @param Y One curve per row on a common grid.
#' @param K Number of clusters.
#' @param basis B-spline basis on the grid; raw curves if omitted.
#' @param iters Fixed number of Lloyd iterations.

#' @return List with ``labels``, ``centers``, ``coef``, ``wss``, ``K``, ``n``.
#' @references Abraham, Cornillon, Matzner-Lober and Molinari (2003), Unsupervised curve clustering using B-splines, Scandinavian Journal of Statistics 30(3):581-595. The article itself is behind a paywall and could not be obtained; the two-stage form implemented here -- fit B-spline coefficients per curve, then k-means on the coefficient vectors -- is as the method is described in the functional-clustering review literature (arXiv:1803.00276).
#' @export
Fdaclust <- function(Y, K = 2, basis = NULL, iters = 25) {
  Y <- as.matrix(Y); n <- nrow(Y); K <- as.integer(K)
  if (n < K || K < 1) stop("need at least K curves")
  coef <- if (is.null(basis)) Y else
    t(vapply(seq_len(n), function(i) .t1_lstsq(as.matrix(basis), Y[i, ])$beta,
             numeric(ncol(as.matrix(basis)))))
  coef <- as.matrix(coef); p <- ncol(coef)
  ord <- order(coef[, 1], seq_len(n))
  centers <- coef[ord[floor((seq_len(K) - 1) * n / K) + 1L], , drop = FALSE]
  labels <- integer(n)
  for (it in seq_len(as.integer(iters))) {
    for (i in seq_len(n)) {
      d <- rowSums((centers - matrix(coef[i, ], K, p, byrow = TRUE))^2)
      labels[i] <- which.min(d) - 1L
    }
    for (j in seq_len(K)) {
      m <- coef[labels == (j - 1L), , drop = FALSE]
      if (nrow(m) > 0) centers[j, ] <- colMeans(m)
    }
  }
  wss <- sum((coef - centers[labels + 1L, , drop = FALSE])^2)
  .t1_result(labels = labels, centers = centers, coef = coef, wss = wss,
             K = K, n = n,
             method = "Functional clustering (k-means on B-spline coefficients)")
}
