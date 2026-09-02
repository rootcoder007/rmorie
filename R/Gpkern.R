# SPDX-License-Identifier: AGPL-3.0-or-later
#' Composing Gaussian process kernels
#'
#' Positive semidefiniteness is closed under the operations used here:
#' sums, products and any input warping k(u(x), u(x')).  That closure is
#' the whole reason a grammar over kernels is well posed, so the
#' smallest eigenvalue of every returned Gram matrix is reported and the
#' tests check it is non-negative.
#'
#' Formula: k_sum = sum_i k_i; k_prod = prod_i k_i;
#'   k_warp = k(u(x), u(x')) with u(x) = (sin x, cos x).
#'
#' @param X Input matrix, one row per point.
#' @param Y Optional second input matrix; X by default.
#' @param kernel_spec List with \code{op} ("sum" or "prod") and
#'   \code{parts}, a list of lists with \code{type} ("rbf" or "warp"),
#'   \code{lengthscale} and \code{variance}.
#' @return List with \code{estimate}, \code{K}, \code{diagonal},
#'   \code{min_eigenvalue}, \code{is_psd}, \code{n}, \code{method}.
#' @references Duvenaud, Lloyd, Grosse, Tenenbaum and Ghahramani (2013),
#'   Structure discovery in nonparametric regression through
#'   compositional kernel search, ICML 2013, PMLR 28(3):1166-1174,
#'   arXiv:1302.4922; Rasmussen and Williams (2006), Gaussian Processes
#'   for Machine Learning, MIT Press, sect. 4.2.4.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Gpkern(V)
Gpkern <- function(X, Y = NULL, kernel_spec = NULL) {
  A <- .s03mat(X)
  if (nrow(A) == 0L) stop("gp_kernel_compose: X is empty")
  B <- if (is.null(Y)) A else .s03mat(Y)
  if (ncol(B) != ncol(A)) stop("gp_kernel_compose: X and Y have different dimensions")
  if (is.null(kernel_spec))
    kernel_spec <- list(op = "sum", parts = list(list(type = "rbf", lengthscale = 1, variance = 1)))
  op <- if (is.null(kernel_spec$op)) "sum" else kernel_spec$op
  if (!(op %in% c("sum", "prod"))) stop("gp_kernel_compose: op must be sum or prod")
  parts <- kernel_spec$parts
  if (is.null(parts) || length(parts) == 0L) stop("gp_kernel_compose: kernel_spec has no parts")
  rbf <- function(P, Q, ell, var) {
    out <- matrix(0, nrow(P), nrow(Q))
    for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q)))
      out[i, j] <- var * exp(-0.5 * sum((P[i, ] - Q[j, ])^2) / (ell * ell))
    out
  }
  warp <- function(P) {
    out <- matrix(0, nrow(P), 2 * ncol(P))
    for (i in seq_len(nrow(P))) {
      v <- numeric(0)
      for (c in seq_len(ncol(P))) v <- c(v, sin(P[i, c]), cos(P[i, c]))
      out[i, ] <- v
    }
    out
  }
  K <- NULL
  for (p in parts) {
    typ <- if (is.null(p$type)) "rbf" else p$type
    ell <- if (is.null(p$lengthscale)) 1 else as.numeric(p$lengthscale)
    var <- if (is.null(p$variance)) 1 else as.numeric(p$variance)
    if (ell <= 0 || var <= 0) stop("gp_kernel_compose: lengthscale and variance must be positive")
    Kp <- if (typ == "rbf") rbf(A, B, ell, var) else if (typ == "warp") rbf(warp(A), warp(B), ell, var) else
      stop(paste("gp_kernel_compose: unknown kernel type", typ))
    K <- if (is.null(K)) Kp else if (op == "sum") K + Kp else K * Kp
  }
  lo <- if (is.null(Y)) .s03jacobi(K)$values[1] else NaN
  d <- diag(K[seq_len(min(nrow(A), nrow(B))), seq_len(min(nrow(A), nrow(B))), drop = FALSE])
  .t1_result(estimate = K[1, 1], K = K, diagonal = d, min_eigenvalue = lo,
             is_psd = as.integer(is.nan(lo) || lo > -1e-10), n = nrow(A),
             method = "sum/product/warp composition of RBF kernels, Duvenaud et al. (2013); Rasmussen & Williams (2006) sect. 4.2.4")
}
