# SPDX-License-Identifier: AGPL-3.0-or-later
#' EIGENSTRAT principal components of a genotype matrix
#'
#' The normalization is the whole method. Centring alone leaves each
#' marker weighted by its own allele frequency, so common SNPs dominate
#' the leading components for no genetic reason. Patterson, Price &
#' Reich divide instead by the drift standard deviation the binomial
#' model predicts, \code{p_j = mu_j / 2} and
#' \code{M_ij = (g_ij - mu_j) / sqrt(p_j (1 - p_j))}, which puts every
#' marker on the same footing, and then take the eigenvectors of
#' \code{X = M M' / m} over INDIVIDUALS -- an n by n problem however
#' many markers there are.
#'
#' Significance of the leading eigenvalue is judged against the
#' Tracy-Widom law. With \code{l1' = n lambda_1 / sum(lambda)} and
#' \code{n' = (n + 1) (sum l)^2 / ((n - 1) sum l^2 - (sum l)^2)},
#' \code{mu = (sqrt(n - 1) + sqrt(n'))^2 / n'},
#' \code{sigma = ((sqrt(n-1) + sqrt(n')) / n')
#' (1/sqrt(n-1) + 1/sqrt(n'))^(1/3)}, the statistic
#' \code{(l1' - mu) / sigma} follows TW1 under the null of no
#' structure. \code{n'} is the EFFECTIVE number of markers and is not
#' \code{m}: linkage makes markers less independent than they look, and
#' using \code{m} overstates significance.
#'
#' Monomorphic markers carry no information and would divide by zero;
#' they are dropped and counted rather than left to produce Inf.
#'
#' @param genotypes Genotype counts in \{0, 1, 2\}, individuals by
#'   markers.
#' @param n_components Number of leading components to return.
#' @return List with \code{eigenvalues}, \code{pcs},
#'   \code{variance_explained}, \code{estimate}, \code{tw_statistic},
#'   \code{n_eff}, \code{n_dropped}, \code{n}, \code{m}.
#' @references Patterson, N., Price, A. L. & Reich, D. (2006).
#'   Population structure and eigenanalysis. PLoS Genetics, 2(12),
#'   e190. doi:10.1371/journal.pgen.0020190
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Pcasnps(M)
Pcasnps <- function(genotypes, n_components = 2L) {
  G <- as.matrix(genotypes)
  storage.mode(G) <- "double"
  n <- nrow(G)
  if (n < 2L) stop("Pcasnps: need at least two individuals")
  m0 <- ncol(G)
  if (m0 == 0L) stop("Pcasnps: no markers")
  k <- as.integer(n_components)
  if (k < 1L || k > n) stop("Pcasnps: n_components must satisfy 1 <= k <= n")
  mu <- colMeans(G)
  pj <- mu / 2
  v <- pj * (1 - pj)
  keep <- which(v > 0)
  dropped <- m0 - length(keep)
  if (!length(keep)) stop("Pcasnps: every marker is monomorphic")
  Mn <- (G[, keep, drop = FALSE] - rep(mu[keep], each = n)) /
    rep(sqrt(v[keep]), each = n)
  m <- length(keep)
  Xc <- tcrossprod(Mn) / m
  eg <- .s03jacobi(Xc)
  ord <- rev(seq_len(n))
  ev <- eg$values[ord]
  pcs <- eg$vectors[, ord[seq_len(k)], drop = FALSE]
  tot <- sum(ev)
  vexp <- if (tot > 0) ev / tot else rep(NaN, n)
  s1 <- sum(ev)
  s2 <- sum(ev * ev)
  den <- (n - 1) * s2 - s1 * s1
  neff <- if (den > 0) (n + 1) * s1 * s1 / den else NaN
  if (!is.na(neff) && neff > 0) {
    a <- sqrt(n - 1) + sqrt(neff)
    mu_tw <- a * a / neff
    sg <- (a / neff) * (1 / sqrt(n - 1) + 1 / sqrt(neff))^(1 / 3)
    l1 <- if (s1 > 0) n * ev[1] / s1 else NaN
    tw <- (l1 - mu_tw) / sg
  } else {
    tw <- NaN
  }
  .t1_result(eigenvalues = ev, pcs = pcs, variance_explained = vexp,
             estimate = ev[1], tw_statistic = tw, n_eff = neff,
             n_dropped = dropped, n = n, m = m,
             method = "EIGENSTRAT genotype PCA (Patterson-Price-Reich 2006)")
}
