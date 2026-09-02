# Best-rotation RMSD between vector sets (Kabsch algorithm).
# Source: Kabsch (1976), Acta Crystallographica A32, 922-923,
# Eqs. 1-9 (fetched-wave3/A solution for the best rotation to relate
# two sets of vectors.pdf).  Mirrors Python morie.fn.rmsdtr exactly
# (same eigen route on R'R, same proper-rotation sign fix).

#' Kabsch optimal-superposition RMSD
#'
#' Finds the proper rotation U minimizing the weighted sum of squared
#' deviations between centered coordinate sets (Kabsch Eq. 1): with
#' R = sum_n w_n y_n x_n' (Eq. 7), diagonalize R'R, form
#' b_k = R a_k / sqrt(mu_k) and U = sum_k b_k a_k', flipping the
#' smallest-eigenvalue direction if needed so det(U) = +1.  Reports
#' the post-superposition RMSD.
#'
#' @param P,Q Matrices (rows = paired points, d = 2 or 3 columns).
#' @param weights Optional non-negative pair weights.
#' @return A list with elements \code{estimate} (RMSD),
#'   \code{rotation}, \code{det}, \code{centroids}, \code{n},
#'   \code{method}.
#' @references Kabsch, W. (1976). A solution for the best rotation to
#'   relate two sets of vectors. Acta Crystallographica, A32,
#'   922-923.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_rmsdtr(V, V)
morie_rmsdtr <- function(P, Q, weights = NULL) {
  P <- as.matrix(P)
  Q <- as.matrix(Q)
  n <- nrow(P)
  d <- ncol(P)
  if (n < 3 || nrow(Q) != n || ncol(Q) != d) {
    stop("need >= 3 paired points of equal dimension")
  }
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(w) != n || any(w < 0) || sum(w) <= 0) {
    stop("weights must be non-negative with positive sum")
  }
  sw <- sum(w)
  cp <- colSums(P * w) / sw
  cq <- colSums(Q * w) / sw
  X <- sweep(P, 2, cp)
  Y <- sweep(Q, 2, cq)
  Rm <- t(Y * w) %*% X
  RtR <- t(Rm) %*% Rm
  ee <- eigen((RtR + t(RtR)) / 2, symmetric = TRUE)
  mu <- pmax(ee$values, 0)
  A <- ee$vectors                       # descending eigenvalues
  B <- matrix(0, d, d)
  for (k in seq_len(d)) {
    if (mu[k] > 1e-24) {
      B[, k] <- as.numeric(Rm %*% A[, k]) / sqrt(mu[k])
    }
  }
  if (all(abs(B[, d]) < 1e-15)) {
    if (d == 3) {
      u <- B[, 1]; v <- B[, 2]
      B[, 3] <- c(u[2]*v[3]-u[3]*v[2], u[3]*v[1]-u[1]*v[3],
                  u[1]*v[2]-u[2]*v[1])
    } else if (d == 2) {
      B[, 2] <- c(-B[2, 1], B[1, 1])
    }
  }
  U <- B %*% t(A)
  if (d %in% c(2, 3) && det(U) < 0) {
    B[, d] <- -B[, d]
    U <- B %*% t(A)
  }
  fitted <- X %*% t(U)
  rmsd <- sqrt(sum(w * rowSums((fitted - Y)^2)) / sw)
  list(estimate = rmsd, rotation = U, det = det(U),
       centroids = list(P = cp, Q = cq), n = n,
       method = "Kabsch (1976) optimal superposition RMSD")
}
