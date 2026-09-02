# SPDX-License-Identifier: AGPL-3.0-or-later
#' Lloyd k-means on compositions, run in clr coordinates.
#'
#' Deterministic by construction so the two language arms agree exactly:
#' initial centres are the FIRST k rows, assignment ties go to the lowest
#' cluster index, and iteration stops when no label changes.
#'
#' Formula: assign i to argmin_c ||clr(x_i) - m_c||^2; m_c <- mean of the
#' clr coordinates assigned to c; centre_c = clr^-1(m_c)
#'
#' @param X One composition per row; strictly positive.
#' @param k Number of clusters (2 <= k <= n).
#' @param max_iter Maximum Lloyd sweeps.
#' @return List with \code{cluster} (one-based), \code{centers},
#'   \code{clr_centers}, \code{withinss}, \code{tot_withinss},
#'   \code{iterations}, \code{n}, \code{D}, \code{k}.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 8 (the log-ratio distance being clustered on); Lloyd
#'   (1982), Least squares quantization in PCM, IEEE Transactions on
#'   Information Theory 28(2), 129-137 (the algorithm itself).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Compkm(V)
Compkm <- function(X, k = 2, max_iter = 50) {
  X <- as.matrix(X)
  if (any(X <= 0)) stop("compositions must be strictly positive")
  n <- nrow(X); D <- ncol(X); k <- as.integer(k)
  if (k < 2L || k > n) stop("k must satisfy 2 <= k <= n")
  L <- log(X)
  Z <- L - rowSums(L) / D
  cen <- Z[seq_len(k), , drop = FALSE]
  lab <- integer(n)
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    moved <- FALSE
    for (i in seq_len(n)) {
      d <- rowSums((cen - matrix(Z[i, ], k, D, byrow = TRUE))^2)
      best <- which.min(d)
      if (lab[i] != best) { lab[i] <- best; moved <- TRUE }
    }
    for (cc in seq_len(k)) {
      mem <- which(lab == cc)
      if (length(mem)) cen[cc, ] <- colMeans(Z[mem, , drop = FALSE])
    }
    if (!moved) break
  }
  wss <- numeric(k)
  for (i in seq_len(n))
    wss[lab[i]] <- wss[lab[i]] + sum((Z[i, ] - cen[lab[i], ])^2)
  centers <- t(apply(cen, 1, function(z) exp(z) / sum(exp(z))))
  .t1_result(cluster = lab, centers = centers, clr_centers = cen,
             withinss = wss, tot_withinss = sum(wss), iterations = it,
             n = n, D = D, k = k,
             method = "Lloyd k-means in clr coordinates")
}
