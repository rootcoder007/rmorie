# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rescale a positive matrix to doubly stochastic form
#'
#' Sinkhorn and Knopp proved that any entrywise positive square matrix has
#' a unique doubly stochastic scaling \code{D1 K D2} with positive
#' diagonal factors, and that alternately normalising rows and columns
#' finds it. The diagonal factors are returned as well because they are
#' what the transport modules actually consume.
#'
#' Formula: \code{K <- diag(1/(K 1)) K}, \eqn{K <- K diag(1/(K prime 1))},
#' repeated -- Sinkhorn and Knopp (1967), Theorem 1.
#'
#' @param K Entrywise positive square matrix.
#' @param max_iter Number of row/column sweeps.
#' @return List with \code{M}, \code{iters}, \code{d1}, \code{d2},
#'   \code{row_err}, \code{col_err}, \code{n}.
#' @references Sinkhorn, R. and Knopp, P. (1967). Pacific Journal of
#'   Mathematics 21(2):343-348. \doi{10.2140/pjm.1967.21.343}.
#' @export
#' @examples
#' Otdwd(K = 5L)
Otdwd <- function(K, max_iter = 200) {
  Km <- as.matrix(K)
  n <- nrow(Km)
  if (ncol(Km) != n) stop("Sinkhorn-Knopp scaling needs a square matrix")
  if (any(Km <= 0)) stop("the matrix must be entrywise positive")
  d1 <- rep(1, n); d2 <- rep(1, n)
  it <- as.integer(max_iter)
  for (k in seq_len(it)) {
    d1 <- 1 / as.numeric(Km %*% d2)
    d2 <- 1 / as.numeric(crossprod(Km, d1))
  }
  M <- Km * d1 * rep(d2, each = n)
  .t1_result(M = M, iters = it, d1 = d1, d2 = d2,
             row_err = max(abs(rowSums(M) - 1)),
             col_err = max(abs(colSums(M) - 1)), n = n,
             method = "Sinkhorn-Knopp doubly stochastic scaling")
}
