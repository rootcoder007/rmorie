# SPDX-License-Identifier: AGPL-3.0-or-later
#' Choose k from the spectrum: the gap after the small eigenvalues
#'
#' The goal is k such that lambda_1..lambda_k are all SMALL and
#' lambda_\{k+1\} is relatively LARGE -- not simply the largest gap
#' anywhere in the spectrum. \code{gaps} is returned in full so a caller
#' can see whether the choice was clear-cut.
#'
#' Formula: gamma_k = |lambda_k - lambda_\{k+1\}|; khat = argmax_k gamma_k
#'
#' @param values Laplacian eigenvalues in INCREASING order.
#' @param kmax Largest k considered.
#' @return List with \code{k}, \code{gap}, \code{gaps}, \code{values},
#'   \code{n_zero}, \code{kmax}.
#' @references von Luxburg (2007), A Tutorial on Spectral Clustering,
#'   Statistics and Computing 17(4), 395-416, Section 8.3. Fetched from
#'   arXiv:0711.0189.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Eigengap(V)
Eigengap <- function(values, kmax = NULL) {
  v <- .t1_vec(values); n <- length(v)
  if (n < 2L) stop("at least two eigenvalues are required")
  if (any(diff(v) < -1e-12))
    stop("eigenvalues must be given in increasing order")
  km <- if (is.null(kmax)) n - 1L else as.integer(kmax)
  if (km < 1L || km > n - 1L)
    stop("kmax must satisfy 1 <= kmax <= length(values) - 1")
  gaps <- abs(v[2:(km + 1L)] - v[1:km])
  best <- which.max(gaps)
  .t1_result(k = as.numeric(best), gap = gaps[best], gaps = gaps,
             values = v, n_zero = sum(v < 1e-10), kmax = as.numeric(km),
             method = "Eigengap heuristic, von Luxburg (2007) Section 8.3")
}
