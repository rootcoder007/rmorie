# Yang et al. realized (genomic) relationship matrix.
# Source: Yang, J., Benyamin, B., McEvoy, B. P., Gordon, S., Henders,
# A. K., Nyholt, D. R., Madden, P. A., Heath, A. C., Martin, N. G.,
# Montgomery, G. W., Goddard, M. E. and Visscher, P. M. (2010),
# Common SNPs explain a large proportion of the heritability for human
# height, Nature Genetics 42, 565-569, and the GCTA implementation
# (Yang et al. 2011, AJHG 88, 76-82).
#
# Off-diagonal entries average the per-marker standardised products
#   (x_ik - 2 p_k)(x_jk - 2 p_k) / [2 p_k (1 - p_k)]
# over markers.  The DIAGONAL has two routes and both are kept:
#   yang_diagonal = FALSE  -- the same formula with i = j, the plain
#     standardised sum of squares;
#   yang_diagonal = TRUE   -- their unbiased diagonal
#     1 + (1/p) sum_k [x^2 - (1 + 2 p_k) x + 2 p_k^2] / [2 p_k(1-p_k)],
#     which corrects the upward bias the naive diagonal carries from
#     using sample allele frequencies.
#
# Native implementation mirroring Python morie.fn.yangr exactly,
# including the skip of markers with zero variance.

#' Yang et al. realized relationship matrix
#'
#' Genomic relationship matrix in which each marker is standardised by
#' its own Hardy-Weinberg variance before averaging (Yang et al.
#' 2010), so that rare variants are weighted up relative to
#' \code{\link{morie_vanr1}}, which standardises once for all markers.
#'
#' @param marker_matrix Genotype matrix, lines by markers, coded 0/1/2.
#' @param freq Optional allele frequencies, one per marker.
#' @param yang_diagonal \code{FALSE} (default) uses the plain
#'   standardised diagonal; \code{TRUE} uses the unbiased diagonal of
#'   Yang et al.  Both routes the source gives are available.
#' @return A list with \code{estimate} (mean diagonal), \code{A},
#'   \code{freq}, \code{n_lines}, \code{n_markers},
#'   \code{yang_diagonal}, \code{method}.
#' @references Yang, J. et al. (2010). Common SNPs explain a large
#'   proportion of the heritability for human height. Nature
#'   Genetics, 42, 565-569.
#' @export
morie_yangr <- function(marker_matrix, freq = NULL, yang_diagonal = FALSE) {
  M <- as.matrix(marker_matrix)
  storage.mode(M) <- "double"
  J <- nrow(M); p <- ncol(M)
  pi_ <- if (is.null(freq)) colSums(M) / (2 * J) else as.numeric(freq)
  var_ <- 2 * pi_ * (1 - pi_)
  keep <- var_ > 0
  A <- matrix(0, J, J)
  Zs <- (M - rep(2 * pi_, each = J))
  for (i in seq_len(J)) {
    for (j in seq_len(J)) {
      if (i == j && isTRUE(yang_diagonal)) {
        x <- M[i, keep]
        s <- sum((x * x - (1 + 2 * pi_[keep]) * x + 2 * pi_[keep]^2) /
                   var_[keep])
        A[i, j] <- 1 + s / p
      } else {
        A[i, j] <- sum(Zs[i, keep] * Zs[j, keep] / var_[keep]) / p
      }
    }
  }
  list(estimate = mean(diag(A)), A = A, freq = pi_, n_lines = J,
       n_markers = p, yang_diagonal = isTRUE(yang_diagonal),
       method = "Yang et al. (2010) realized relationship matrix")
}
