# SPDX-License-Identifier: AGPL-3.0-or-later
#' Principal component analysis in centred log-ratio coordinates.
#'
#' Formula: clr(x) = log x - mean(log x); eigendecompose the clr covariance; proportion retained = (s_1^2+...+s_r^2)/sum s^2
#'
#' @param X One composition per row; strictly positive.
#' @param k Number of components retained.

#' @return List with ``values``, ``loadings``, ``scores``, ``prop_var``, ``cum_prop``, ``k``, ``n``, ``D``.
#' @references Aitchison, A Concise Guide to Compositional Data Analysis, Chapter 2. Verified against the text: the centre estimate is xi-hat = C(g_1, ..., g_D) with g_i the geometric mean of the ith component, and totvar(x) = trace(Gamma) = (1/D) sum_{i<j} var{log(x_i/x_j)}.
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Clrpca(M)
Clrpca <- function(X, k = 2) {
  X <- as.matrix(X)
  if (any(X <= 0)) stop("compositions must be strictly positive")
  L <- log(X); D <- ncol(X); n <- nrow(X); k <- as.integer(k)
  Z <- L - rowMeans(L)
  Zc <- sweep(Z, 2, colMeans(Z), "-")
  e <- .t1_eigsym(stats::cov(Zc))
  tot <- sum(e$values[e$values > 0])
  load <- e$vectors[, seq_len(k), drop = FALSE]
  prop <- e$values[seq_len(k)] / tot
  .t1_result(values = e$values, loadings = load, scores = Zc %*% load,
             prop_var = prop, cum_prop = cumsum(prop), k = k, n = n, D = D,
             method = "Compositional (clr) principal components")
}
