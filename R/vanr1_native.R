# VanRaden Method 1 genomic relationship matrix.
# Source: VanRaden, P. M. (2008), Efficient methods to compute genomic
# predictions, Journal of Dairy Science 91(11), 4414-4423, his
# "method 1": centre the marker matrix by twice the allele frequency,
# Z = M - 2P, and scale the cross-product by 2 sum p_j (1 - p_j) so
# that G is analogous to the numerator relationship matrix A,
#     G = Z Z' / [2 sum_j p_j (1 - p_j)].
# The denominator is what puts G on the same scale as A: it is the
# total variance of the marker genotypes under Hardy-Weinberg.
#
# Native implementation mirroring Python morie.fn.vanr1 exactly,
# including the default allele frequencies estimated from the marker
# matrix itself as column mean / 2.

#' VanRaden Method 1 genomic relationship matrix
#'
#' Computes \eqn{G = ZZ'/[2\sum_j p_j(1-p_j)]} with \eqn{Z = M - 2P}
#' (VanRaden 2008, method 1).  The mean diagonal of \eqn{G} is
#' reported as \code{estimate}; it exceeds 1 when the sample is more
#' inbred than the base population implied by \code{freq}.
#'
#' @param marker_matrix Genotype matrix, lines by markers, coded as
#'   allele dosages 0/1/2.
#' @param freq Optional base allele frequencies, one per marker;
#'   default estimates them from \code{marker_matrix}.  Supplying
#'   base-population frequencies is the route VanRaden recommends when
#'   they are known, so both are available.
#' @return A list with \code{estimate} (mean diagonal), \code{G},
#'   \code{freq}, \code{denominator}, \code{n_lines},
#'   \code{n_markers}, \code{method}.
#' @references VanRaden, P. M. (2008). Efficient methods to compute
#'   genomic predictions. Journal of Dairy Science, 91(11),
#'   4414-4423.
#' @export
morie_vanr1 <- function(marker_matrix, freq = NULL) {
  M <- as.matrix(marker_matrix)
  storage.mode(M) <- "double"
  J <- nrow(M); p <- ncol(M)
  pj <- if (is.null(freq)) colSums(M) / (2 * J) else as.numeric(freq)
  Z <- M - rep(2 * pj, each = J)
  den <- 2 * sum(pj * (1 - pj))
  G <- (Z %*% t(Z)) / den
  list(estimate = mean(diag(G)), G = G, freq = pj, denominator = den,
       n_lines = J, n_markers = p,
       method = "VanRaden (2008) method 1 genomic relationship matrix")
}
