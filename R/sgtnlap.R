# SPDX-License-Identifier: AGPL-3.0-or-later
#' Normalised Laplacian in Chung's sense.
#'
#' Chung's convention T^-1(v, v) = 0 for an isolated vertex is followed
#' exactly, so an isolated vertex contributes a zero row and column.
#'
#' Formula: Lcal = T^-1/2 L T^-1/2, i.e.
#'   Lcal(u, v) = 1 - w(v, v)/d_v if u = v and d_v != 0,
#'              = -w(u, v)/sqrt(d_u d_v) if u ~ v, else 0
#'
#' @param W Symmetric non-negative weight matrix.
#' @return List with \code{Lcal}, \code{degree}, \code{isolated}, \code{n}.
#' @references Chung (1997), Spectral Graph Theory, CBMS 92, Sections 1.2
#'   and 1.4: "L = T^-1/2 L T^-1/2 with the convention T^-1(v, v) = 0 for
#'   d_v = 0". Fetched from the author's own copy of the chapter.
#' @export
Normlap <- function(W) {
  W <- as.matrix(W)
  n <- nrow(W)
  if (ncol(W) != n) stop("W must be square")
  d <- rowSums(W)
  s <- ifelse(d == 0, 0, 1 / sqrt(ifelse(d == 0, 1, d)))
  L <- -W
  diag(L) <- d - diag(W)
  Lc <- diag(s, n) %*% L %*% diag(s, n)
  .t1_result(Lcal = Lc, degree = d, isolated = which(d == 0), n = n,
             method = "Normalised Laplacian T^-1/2 L T^-1/2")
}
