# SPDX-License-Identifier: AGPL-3.0-or-later
#' Functional principal components analysis (FPCA)
#'
#' NOT IN THE BOOK.  Montesinos Lopez, Montesinos Lopez and Crossa (2022),
#' Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#' Springer, was searched in full -- all seventeen page-range volumes and the
#' index, [Pages 683-691].  "Functional principal" and "eigenfunction" do not
#' occur anywhere; Chapter 14, volume [Pages 579-631], covers functional
#' regression by fixed basis expansion (Fourier and B-spline, Section 14.2)
#' and never introduces a data-driven basis.  Chapter 2, volume [Pages 35-70],
#' Section 2.8, gives multivariate PCA on a rectangular matrix, which is the
#' discrete analogue only.
#'
#' The functional version is therefore taken from the primary source the book
#' itself cites for functional data, Ramsay, J., Hooker, G. and Graves, S.
#' (2009), Functional Data Analysis with R and MATLAB, Springer, whose
#' Karhunen-Loeve expansion is x_i(t) = mu(t) + sum_k score_ik phi_k(t), with
#' phi_k the eigenfunctions of the covariance surface
#' v(s,t) = (1/(n-1)) sum_i (x_i(s)-mu(s))(x_i(t)-mu(t)) and the normalisation
#' integral phi_k^2 = 1 rather than the Euclidean one.  The integral is taken
#' by the trapezoid rule on the observation grid, which is what makes this the
#' functional and not the multivariate problem.
#'
#' @param data_functions n-by-m matrix; row i is curve i sampled on the common
#'   grid.
#' @param n_components number of components to keep, 1 <= k <= min(n-1, m).
#' @param a,b end points of the equally spaced grid.
#' @return list: estimate, scores, eigenfuncs, eigenvalues, mean, weights, n,
#'   method.
#' @keywords internal
#' @examples
#' g <- seq(0, 1, length.out = 21)
#' Fpca(rbind(sin(2 * pi * g), cos(2 * pi * g), sin(4 * pi * g)), 1)$estimate
#' @export
Fpca <- function(data_functions, n_components, a = 0, b = 1) {
  Xr <- .s03mat(data_functions)
  n <- nrow(Xr)
  if (n < 2L) stop("functional_pca: need at least two curves")
  m <- ncol(Xr)
  if (m < 2L) stop("functional_pca: need at least two grid points")
  kk <- as.integer(n_components)
  if (kk < 1L || kk > min(n - 1L, m)) {
    stop("functional_pca: n_components must lie between 1 and min(n-1, m)")
  }
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (!(b > a)) stop("functional_pca: the grid must have positive width")
  h <- (b - a) / (m - 1L)
  w <- rep(h, m)
  w[1L] <- h * 0.5
  w[m] <- h * 0.5
  mu <- colSums(Xr) / n
  C <- matrix(0, n, m)
  for (i in seq_len(n)) C[i, ] <- Xr[i, ] - mu
  sw <- sqrt(w)
  V <- matrix(0, m, m)
  for (s_ in seq_len(m)) {
    for (t_ in seq_len(m)) {
      acc <- 0
      for (i in seq_len(n)) acc <- acc + C[i, s_] * C[i, t_]
      V[s_, t_] <- acc / (n - 1L) * sw[s_] * sw[t_]
    }
  }
  eg <- .s03jacobi(V)
  ord <- rev(seq_len(m))
  ev <- eg$values[ord]
  phi <- matrix(0, kk, m)
  for (j in seq_len(kk)) {
    col <- numeric(m)
    for (r in seq_len(m)) col[r] <- if (sw[r] > 0) eg$vectors[r, ord[j]] / sw[r] else 0
    nrm <- 0
    for (r in seq_len(m)) nrm <- nrm + w[r] * col[r] * col[r]
    nrm <- if (nrm > 0) sqrt(nrm) else 1
    phi[j, ] <- col / nrm
  }
  scores <- matrix(0, n, kk)
  for (i in seq_len(n)) {
    for (j in seq_len(kk)) {
      s <- 0
      for (r in seq_len(m)) s <- s + w[r] * C[i, r] * phi[j, r]
      scores[i, j] <- s
    }
  }
  tot <- sum(pmax(ev, 0))
  top <- sum(pmax(ev[seq_len(kk)], 0))
  list(estimate = if (tot > 0) top / tot else NaN, scores = scores,
       eigenfuncs = phi, eigenvalues = ev[seq_len(kk)], mean = mu, weights = w,
       n = n,
       method = paste0("Karhunen-Loeve x_i(t) = mu(t) + sum_k s_ik phi_k(t) ",
                       "(Ramsay, Hooker and Graves 2009); not in the book"))
}
