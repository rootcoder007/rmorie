# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spectral clustering: embed by the bottom eigenvectors, then k-means.
#'
#' The eigenvectors are the SMALLEST ones. Signs are fixed on the
#' largest-magnitude entry and the k-means is initialised at the FIRST k
#' embedded rows with a fixed sweep budget, so both arms agree. Under a
#' repeated eigenvalue the eigenvectors are not unique; that is a property
#' of the problem, not of the code.
#'
#' Formula: L = T - W (or L_sym = T^-1/2 L T^-1/2); U = the k eigenvectors
#'   of smallest eigenvalue; normalized: rows of U scaled to unit norm;
#'   cluster the rows of U by k-means
#'
#' @param W Symmetric non-negative similarity matrix.
#' @param k Number of clusters, 2 <= k <= n.
#' @param normalized Use the symmetric normalized Laplacian and
#'   row-normalize the embedding.
#' @param max_iter k-means sweep budget.
#' @return List with \code{cluster}, \code{embedding}, \code{values},
#'   \code{centers}, \code{tot_withinss}, \code{iterations}, \code{n},
#'   \code{k}.
#' @references von Luxburg (2007), A Tutorial on Spectral Clustering,
#'   Statistics and Computing 17(4), 395-416. Fetched from
#'   arXiv:0711.0189. Lloyd (1982), IEEE Transactions on Information
#'   Theory 28(2), 129-137, for the k-means step.
#' @export
Specclus <- function(W, k = 2, normalized = TRUE, max_iter = 50) {
  W <- as.matrix(W); n <- nrow(W)
  if (ncol(W) != n) stop("W must be square")
  k <- as.integer(k)
  if (k < 2L || k > n) stop("k must satisfy 2 <= k <= n")
  d <- rowSums(W)
  L <- -W; diag(L) <- d - diag(W)
  if (isTRUE(normalized)) {
    s <- ifelse(d == 0, 0, 1 / sqrt(ifelse(d == 0, 1, d)))
    L <- diag(s, n) %*% L %*% diag(s, n)
  }
  e <- .t1_eigsym(L)
  ord <- rev(seq_len(n))
  lam <- e$values[ord]
  U <- e$vectors[, ord[seq_len(k)], drop = FALSE]
  if (isTRUE(normalized)) {
    nr <- sqrt(rowSums(U^2))
    U[nr > 0, ] <- U[nr > 0, , drop = FALSE] / nr[nr > 0]
  }
  cen <- U[seq_len(k), , drop = FALSE]
  lab <- integer(n); it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    moved <- FALSE
    for (i in seq_len(n)) {
      dd <- rowSums((cen - matrix(U[i, ], k, k, byrow = TRUE))^2)
      best <- which.min(dd)
      if (lab[i] != best) { lab[i] <- best; moved <- TRUE }
    }
    for (cc in seq_len(k)) {
      mem <- which(lab == cc)
      if (length(mem)) cen[cc, ] <- colMeans(U[mem, , drop = FALSE])
    }
    if (!moved) break
  }
  wss <- 0
  for (i in seq_len(n)) wss <- wss + sum((U[i, ] - cen[lab[i], ])^2)
  .t1_result(cluster = lab, embedding = U, values = lam[seq_len(k)],
             centers = cen, tot_withinss = wss, iterations = as.numeric(it),
             n = as.numeric(n), k = as.numeric(k),
             method = "Spectral clustering, von Luxburg (2007)")
}
